#!/usr/bin/env python3
"""Convert the GitHub wiki git repo into an MkDocs source tree.

The wiki stays the single source of truth: contributors keep editing pages in
GitHub's wiki UI, and the `gollum` event rebuilds this site. Everything here is
a pure transform -- nothing is written back to the wiki.

Three things need fixing up on the way through:

  * Page names carry an ordering prefix and a difficulty emoji
    (`[01a]-<green>-Joining-the-Beta.md`). The prefix drives nav order, the
    emoji stays in the nav label, and neither belongs in the URL.
  * Headings are linked to with GitHub's anchor slugs (`#1---general-loader-name`),
    which are not the slugs Python-Markdown generates. We emit an explicit
    anchor per heading so the wiki's own TOC links keep working.
  * A handful of cross-page links are absolute github.com/wiki URLs, some of
    them pointing at pages that were since renamed.
"""

from __future__ import annotations

import html
import posixpath
import re
import shutil
import sys
import unicodedata
from pathlib import Path
from urllib.parse import unquote

# Pages that links still point at but which no longer exist under that name.
# Keys and values are wiki page names without the .md suffix.
RENAMED_PAGES = {
    "[01]-💚-Getting-Started": "[01b]-💚-Getting-Started",
    "[02]-💚-Getting-Started": "[01b]-💚-Getting-Started",
    "[01]-💚-Joining-the-Beta": "[01a]-💚-Joining-the-Beta",
    "[03]-Table-Manager": "[03]-💚-Table-Manager-‐-Wizard",
    "[04]-Table-Manager-‐-Other-Features": "[05]-🧡-TM-‐-Other-Features",
    "[04]-🧡-TM-‐-Other-Features": "[05]-🧡-TM-‐-Other-Features",
}

# Links to a section that has since moved to a different page, so the page
# rename above is not enough. Keyed by "<page>#<anchor>" exactly as written in
# the wiki; the value is the page and anchor it should land on now.
RENAMED_SECTIONS = {
    "[03]-Table-Manager#extract-vbs-file":
        ("[05]-🧡-TM-‐-Other-Features", "extract-vbs-file"),
    "[02]-💚-Getting-Started#table-manager":
        ("[01b]-💚-Getting-Started", "table-manager-install"),
}

# Page names may contain parentheses, e.g. `[06]-...-(Table-Settings)`, so a
# single balanced pair is allowed inside the URL.
WIKI_URL_RE = re.compile(
    r"https?://github\.com/[^/\s]+/[^/\s]+/wiki/"
    r"((?:[^()\s\"'<>]|\([^()\s]*\))*)"
)
ORDER_PREFIX_RE = re.compile(r"^\[(\d+)([a-z]?)\]-")
ATX_HEADING_RE = re.compile(r"^(#{1,6})\s+(.*?)\s*#*\s*$")
FENCE_RE = re.compile(r"^\s*(```|~~~)")

# github-slugger drops anything that is not a letter, digit, underscore,
# hyphen or space -- which is what removes punctuation, the U+2010 hyphen used
# in some page names, and emoji. \w is unicode-aware, so accented letters stay.
_SLUG_STRIP_RE = re.compile(r"[^\w\s-]", re.UNICODE)

# Inline markdown that is not part of the rendered heading text.
_INLINE_CODE_RE = re.compile(r"`([^`]*)`")
_MD_LINK_RE = re.compile(r"\[([^\]]*)\]\([^)]*\)")
_HTML_TAG_RE = re.compile(r"<[^>]+>")
_STAR_EMPHASIS_RE = re.compile(r"(\*{1,3})(?=\S)(.*?)(?<=\S)\1", re.DOTALL)
# An underscore inside a word is literal per CommonMark, so emphasis with `_`
# only counts at a word boundary -- otherwise `_use_these_files_` closes early.
_UNDER_EMPHASIS_RE = re.compile(
    r"(?<!\w)(_{1,3})(?=\S)(.*?)(?<=\S)\1(?!\w)", re.DOTALL
)


def heading_text(raw: str) -> str:
    """Reduce a raw markdown heading to the text GitHub would slugify."""
    text = _MD_LINK_RE.sub(r"\1", raw)
    text = _INLINE_CODE_RE.sub(r"\1", text)
    text = _HTML_TAG_RE.sub("", text)
    for _ in range(3):  # nested emphasis, e.g. ***bold italic***
        text = _STAR_EMPHASIS_RE.sub(r"\2", text)
        text = _UNDER_EMPHASIS_RE.sub(r"\2", text)
    return html.unescape(text).strip()


def github_slug(text: str) -> str:
    """Approximate github-slugger, which is what wiki anchor links assume."""
    slug = unicodedata.normalize("NFC", text).strip().lower()
    slug = _SLUG_STRIP_RE.sub("", slug)
    return slug.replace(" ", "-")


def page_slug(name: str) -> str:
    """`[01a]-<emoji>-Joining-the-Beta` -> `01a-joining-the-beta`."""
    match = ORDER_PREFIX_RE.match(name)
    prefix = f"{match.group(1)}{match.group(2)}-" if match else ""
    rest = name[match.end():] if match else name
    rest = _SLUG_STRIP_RE.sub("", rest.replace("-", " ")).strip().lower()
    rest = re.sub(r"\s+", "-", rest)
    return f"{prefix}{rest}".strip("-")


def nav_title(name: str) -> str:
    """Nav label: drop the ordering prefix, keep the difficulty emoji."""
    title = ORDER_PREFIX_RE.sub("", name).replace("-", " ").strip()
    return re.sub(r"\s+", " ", title)


def sort_key(name: str) -> tuple:
    match = ORDER_PREFIX_RE.match(name)
    if name == "Home":
        return (-1, 0, "", name)  # Home is the site index, so it leads the nav
    if not match:
        return (1, 0, "", name)
    return (0, int(match.group(1)), match.group(2), name)


def collect_anchors(lines: list[str]) -> dict[int, str]:
    """Map line index -> GitHub anchor slug, for every ATX heading."""
    anchors: dict[int, str] = {}
    seen: dict[str, int] = {}
    in_fence = False
    fence_marker = ""
    for index, line in enumerate(lines):
        fence = FENCE_RE.match(line)
        if fence:
            marker = fence.group(1)
            if not in_fence:
                in_fence, fence_marker = True, marker
            elif marker == fence_marker:
                in_fence = False
            continue
        if in_fence:
            continue
        match = ATX_HEADING_RE.match(line)
        if not match:
            continue
        slug = github_slug(heading_text(match.group(2)))
        if not slug:
            continue
        # github-slugger disambiguates repeats with -1, -2, ...
        count = seen.get(slug, 0)
        seen[slug] = count + 1
        anchors[index] = slug if count == 0 else f"{slug}-{count}"
    return anchors


def case_aliases(text: str, slugs: set[str]) -> dict[str, list[str]]:
    """Extra ids for links that only differ from a real heading by case.

    Several pages link to `#Brightness` while the heading generates
    `brightness`. Those links are dead on the wiki itself; emitting the
    original-case id as well makes them work here.
    """
    by_lower = {slug.lower(): slug for slug in slugs}
    aliases: dict[str, list[str]] = {}
    for target in re.findall(r"\]\(#([^)\s]+)\)", text):
        slug = unquote(target)
        if slug in slugs:
            continue
        canonical = by_lower.get(slug.lower())
        if canonical and slug not in aliases.get(canonical, []):
            aliases.setdefault(canonical, []).append(slug)
    return aliases


def inject_anchors(lines: list[str], anchors: dict[int, str],
                   aliases: dict[str, list[str]]) -> list[str]:
    """Emit an explicit <a id> above each heading, using the GitHub slug.

    MkDocs generates its own ids for its table of contents; these extra
    anchors are what the wiki's existing `#some-heading` links resolve to.
    """
    out: list[str] = []
    for index, line in enumerate(lines):
        slug = anchors.get(index)
        if slug:
            for alias in aliases.get(slug, []):
                out.append(f'<a id="{alias}"></a>')
            out.append(f'<a id="{slug}"></a>')
        out.append(line)
    return out


def rewrite_links(text: str, page_paths: dict[str, str], source: str,
                  source_dir: str, problems: list[str]) -> str:
    """Point absolute github.com/wiki links at the local page instead.

    Links are emitted relative to the current page so they resolve both at the
    custom domain root and under the /<repo>/ project-pages path.
    """

    def replace(match: re.Match) -> str:
        target = match.group(1)
        anchor = ""
        if "#" in target:
            target, anchor = target.split("#", 1)
            anchor = f"#{anchor}"
        name = unquote(target)
        if not name:
            return "/"
        moved = RENAMED_SECTIONS.get(f"{name}{anchor}")
        if moved:
            name, anchor = moved[0], f"#{moved[1]}"
        else:
            name = RENAMED_PAGES.get(name, name)
        target_dir = page_paths.get(name)
        if target_dir is None:
            problems.append(f"{source}: unresolved wiki link -> {name!r}")
            return match.group(0)  # leave pointing at GitHub rather than 404
        # Link to the source file: MkDocs rewrites it to the right URL for
        # whatever base path the site is served under, and validates the
        # anchor while it is at it.
        target = posixpath.join(target_dir, "index.md")
        rel = posixpath.relpath(target, source_dir or ".")
        return f"{rel}{anchor}"

    return WIKI_URL_RE.sub(replace, text)


def check_anchors(text: str, anchors: set[str], source: str,
                  problems: list[str]) -> None:
    """Report same-page `#anchor` links with no matching heading."""
    explicit = set(re.findall(r'<a\s+id="([^"]+)"', text))
    explicit |= set(re.findall(r'\sid="([^"]+)"', text))
    for target in re.findall(r"\]\(#([^)\s]+)\)", text):
        slug = unquote(target)
        if slug not in anchors and slug not in explicit:
            problems.append(f"{source}: dead anchor -> #{slug}")


def main() -> int:
    if len(sys.argv) != 3:
        print("usage: build.py <wiki-clone-dir> <output-dir>", file=sys.stderr)
        return 2
    wiki_dir, out_dir = Path(sys.argv[1]), Path(sys.argv[2])
    docs_dir = out_dir / "docs"
    base_config = Path(__file__).parent / "mkdocs.base.yml"

    pages = sorted(
        (p for p in wiki_dir.glob("*.md") if not p.name.startswith("_")),
        key=lambda p: sort_key(p.stem),
    )
    if not pages:
        print(f"error: no wiki pages found in {wiki_dir}", file=sys.stderr)
        return 1

    # Home becomes the site index; everything else gets a slugged directory.
    page_paths = {
        p.stem: ("" if p.stem == "Home" else page_slug(p.stem))
        for p in pages
    }

    if docs_dir.exists():
        shutil.rmtree(docs_dir)
    docs_dir.mkdir(parents=True)

    problems: list[str] = []
    nav: list[tuple[str, str]] = []

    for page in pages:
        raw = page.read_text(encoding="utf-8")
        lines = raw.splitlines()
        anchors = collect_anchors(lines)
        aliases = case_aliases(raw, set(anchors.values()))
        body = "\n".join(inject_anchors(lines, anchors, aliases)) + "\n"
        body = rewrite_links(body, page_paths, page.name,
                             page_paths[page.stem], problems)
        check_anchors(body, set(anchors.values()), page.name, problems)

        if page.stem == "Home":
            out_path, title = docs_dir / "index.md", "Home"
        else:
            slug = page_slug(page.stem)
            out_path = docs_dir / slug / "index.md"
            out_path.parent.mkdir(parents=True, exist_ok=True)
            title = nav_title(page.stem)
        out_path.write_text(body, encoding="utf-8")
        nav.append((title, str(out_path.relative_to(docs_dir))))

    # The nav has to live in mkdocs.yml, so emit the whole config: the static
    # part is kept editable in mkdocs.base.yml, the nav is derived from the
    # wiki page names and their ordering prefixes.
    config = base_config.read_text(encoding="utf-8").rstrip("\n")
    nav_yaml = "nav:\n" + "".join(
        f'  - "{title}": {path}\n' for title, path in nav
    )
    (out_dir / "mkdocs.yml").write_text(
        f"# Generated by .github/wiki-site/build.py -- do not edit by hand.\n"
        f"{config}\n\n{nav_yaml}",
        encoding="utf-8",
    )

    print(f"built {len(pages)} pages into {docs_dir}")
    for problem in problems:
        print(f"  warning: {problem}")
    if problems:
        print(f"{len(problems)} link warning(s); site still builds.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
