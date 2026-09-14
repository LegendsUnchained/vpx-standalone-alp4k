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

# The difficulty emoji in a page name is the page's track. It already drives
# the nav label; here it also groups the nav and colors the eyebrow. Keys are
# the exact emoji used in wiki page names.
TRACKS = {
    "💚": ("beginner", "START HERE"),
    "🧡": ("intermediate", "GOING FURTHER"),
    "❤️": ("advanced", "DEEP END"),
    "❤": ("advanced", "DEEP END"),  # some pages use the unqualified glyph
}
TRACK_ORDER = ["beginner", "intermediate", "advanced"]

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

# `> [!WARNING]` is GitHub-flavoured and renders as a plain blockquote in
# Python-Markdown. Mapped onto the admonition types the theme styles.
ALERT_TYPES = {
    "NOTE": "note",
    "TIP": "tip",
    "IMPORTANT": "important",
    "WARNING": "warning",
    "CAUTION": "danger",
}
ALERT_RE = re.compile(r"^(\s*)>\s*\[!(" + "|".join(ALERT_TYPES) + r")\]\s*$")

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
    """Nav label: drop the ordering prefix and the difficulty emoji.

    The emoji is the page's track, which the rail already shows as a colored
    dot and the page as its eyebrow, so in the label it is a third copy of the
    same fact. Tracks are read from the file name, not the label, so removing
    it here changes nothing but the text.
    """
    title = ORDER_PREFIX_RE.sub("", name)
    for emoji in TRACKS:  # the qualified "\u2764\ufe0f" precedes the bare glyph
        title = title.replace(emoji, "")
    title = title.replace("\ufe0f", "")  # a variation selector left behind
    title = title.replace("-", " ").strip()
    return re.sub(r"\s+", " ", title)


def page_track(name: str) -> str | None:
    """Difficulty track for a page, from the emoji in its name."""
    for emoji, (track, _section) in TRACKS.items():
        if emoji in name:
            return track
    return None


def page_order(name: str) -> str:
    """`[01b]-...` -> `01b`. Empty when a page carries no prefix."""
    match = ORDER_PREFIX_RE.match(name)
    return f"{match.group(1)}{match.group(2)}" if match else ""


def front_matter(name: str) -> str:
    """Meta block read by MkDocs, consumed by overrides/main.html."""
    track = page_track(name)
    if not track:
        return ""
    lines = ["---", f"track: {track}"]
    order = page_order(name)
    if order:
        # Quoted: a bare `06` is octal under YAML 1.1 and would reach the
        # template as the integer 6, printing "PAGE 6" instead of "PAGE 06".
        lines.append(f'order: "{order}"')
    lines += ["---", ""]
    return "\n".join(lines)


def sort_key(name: str) -> tuple:
    match = ORDER_PREFIX_RE.match(name)
    if name == "Home":
        return (-1, 0, "", name)  # Home is the site index, so it leads the nav
    if not match:
        return (1, 0, "", name)
    return (0, int(match.group(1)), match.group(2), name)


def rewrite_alerts(lines: list[str]) -> list[str]:
    """`> [!WARNING]` blockquotes -> `!!! warning` admonitions.

    Runs before anchors are collected so heading line indexes stay valid: the
    rewrite is line-for-line except for the marker line, which is replaced
    rather than removed. Fenced blocks are skipped.
    """
    out: list[str] = []
    in_fence = False
    fence_marker = ""
    in_alert = False
    for line in lines:
        fence = FENCE_RE.match(line)
        if fence:
            marker = fence.group(1)
            if not in_fence:
                in_fence, fence_marker = True, marker
            elif marker == fence_marker:
                in_fence = False
            in_alert = False
            out.append(line)
            continue
        if in_fence:
            out.append(line)
            continue
        match = ALERT_RE.match(line)
        if match:
            indent, kind = match.group(1), ALERT_TYPES[match.group(2)]
            out.append(f'{indent}!!! {kind} "{match.group(2)}"')
            in_alert = True
            continue
        if in_alert:
            body = re.match(r"^(\s*)>\s?(.*)$", line)
            if body:
                # Admonition bodies are indented four spaces, not quoted.
                out.append(f"{body.group(1)}    {body.group(2)}")
                continue
            in_alert = False
        out.append(line)
    return out


def demote_headings(lines: list[str]) -> list[str]:
    """Shift every heading down a level on pages that use `#` for sections.

    The skin styles H1 as the page title and H2 as a section rule, but a lot of
    pages open with banner art and then use `#` for each section. Those pages
    also lose their "On this page" rail: Material builds it from the children of
    the first heading, so a page whose sections are all H1 gets an empty one.

    Demoting leaves no H1 in the content at all, which is the case Material
    fills with the page title from the nav -- so the page gains a real title
    where it had none. Pages with a single H1 already match the design and are
    left alone.

    Line-for-line: only the leading `#` run changes, so the heading line indexes
    anchors are keyed off stay valid.
    """
    in_fence = False
    fence_marker = ""
    headings: list[tuple[int, int]] = []  # (line index, level)
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
        if match:
            headings.append((index, len(match.group(1))))

    if sum(1 for _index, level in headings if level == 1) < 2:
        return lines

    out = list(lines)
    for index, level in headings:
        # Six is as deep as ATX goes, so the bottom level absorbs itself.
        out[index] = "#" + out[index] if level < 6 else out[index]
    return out


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

    # mkdocs resolves `custom_dir` relative to the config file and `extra_css`
    # relative to docs_dir, so the theme has to land in both places.
    overrides_src = Path(__file__).parent / "overrides"
    overrides_dst = out_dir / "overrides"
    if overrides_dst.exists():
        shutil.rmtree(overrides_dst)
    shutil.copytree(overrides_src, overrides_dst)
    shutil.copytree(overrides_src / "assets", docs_dir / "assets")

    problems: list[str] = []
    nav: list[tuple[str | None, str, str]] = []

    for page in pages:
        raw = page.read_text(encoding="utf-8")
        lines = demote_headings(rewrite_alerts(raw.splitlines()))
        anchors = collect_anchors(lines)
        aliases = case_aliases(raw, set(anchors.values()))
        body = "\n".join(inject_anchors(lines, anchors, aliases)) + "\n"
        body = rewrite_links(body, page_paths, page.name,
                             page_paths[page.stem], problems)
        check_anchors(body, set(anchors.values()), page.name, problems)
        # Front matter has to be the first thing in the file, so it is
        # prepended after every line-indexed pass has run.
        body = front_matter(page.stem) + body

        if page.stem == "Home":
            out_path, title = docs_dir / "index.md", "Home"
        else:
            slug = page_slug(page.stem)
            out_path = docs_dir / slug / "index.md"
            out_path.parent.mkdir(parents=True, exist_ok=True)
            title = nav_title(page.stem)
        out_path.write_text(body, encoding="utf-8")
        nav.append((page_track(page.stem), title,
                    str(out_path.relative_to(docs_dir))))

    # The nav has to live in mkdocs.yml, so emit the whole config: the static
    # part is kept editable in mkdocs.base.yml, the nav is derived from the
    # wiki page names and their ordering prefixes.
    config = base_config.read_text(encoding="utf-8").rstrip("\n")
    nav_lines = ["nav:"]
    # Pages with no difficulty emoji -- Home -- sit above the groups, which is
    # already where sort_key puts them.
    nav_lines += [f'  - "{title}": {path}'
                  for track, title, path in nav if track is None]
    for track in TRACK_ORDER:
        section = next(
            (label for _emoji, (t, label) in TRACKS.items() if t == track),
            track.upper(),
        )
        entries = [(title, path) for t, title, path in nav if t == track]
        if not entries:
            continue
        nav_lines.append(f'  - "{section}":')
        nav_lines += [f'      - "{title}": {path}' for title, path in entries]
    nav_yaml = "\n".join(nav_lines) + "\n"
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
