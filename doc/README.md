# Using AsciiDoctor for Documentation

This document describes the process of using Asciidoc provide documentation for the VPX ALP4k project.
The goal is to provide a way to maintain and generate the documentation in a variety of formats and to make maintaing
the documentation long term easier and easier to track.

## Why AsciiDoctor?

AsciiDoctor provides a markup style syntax for writing general documentation, articles, books, etc.  It follows in
the footsteps of formats like DocBook.

* A mature, plain-text writing format for authoring notes, articles, documentation, books, ebooks, web pages, slide decks, blog posts, man pages and more.
* A text processor and toolchain for translating AsciiDoc documents into various formats (called backends), including HTML, DocBook, PDF and ePub

## That is Great why do we need it?

One of the advantages that AsciiDoctor has over say Markdown, is that it is designed for the book publishing industry.
You can write and publish books with it, and it has a powerful Attribute system.  An attribute is like
a custom macro that can be included and referenced within other asciidoc documents.  It allows you to 
create reusable references to items like URLs, other asciidoc documents, images, complex asciidoc markup, etc.

If you reference a common url a lot, you just include the attribute you create, and it will automatically
be replaced with the appropriate markup when publishing the site.

### Commit History

While GitHub wiki provides a revision history it is technically not included in the project repository, and
doesn't allow for pull request and standard PR best practices when somebody wants to update the page.  The pages
are editable without necessarily having a peer review process to it.   

## Can publishing be Automated?

Yes, there are GitHub Actions that can be used to automatically publish the pages to Jekyll format that
GitHub supports.  These can be published to legendsunchained.github.io/vpx-standalone-alp4k.

A custom domain can be used instead when the project goes live.

### Enable Pages publish

Pages for a project need to be enabled in the Project settings, under the Pages entry.  For asciidoc the Custom Github Pages action
would need to be enabled, and follow the instructions on the asciidoctor GitHub pages action page:

https://github.com/marketplace/actions/asciidoctor-ghpages

## Where can I found more information on AsciiDoctor?

https://asciidoctor.org/

## Can this be published locally?

Yes! There are various tools for documented at AsciiDoctor web site that describe how to publish on Windows, Mac, and
Linux based systems.

## How do I edit AsciiDoctor files?

Use any text editor you like.  Many IDE's support AsciiDoctor syntax with content assistance.  
Find your favorite IDE or Text Editor and it probably has a plugin that works with AsciiDoctor files.

## How can I preveiew the files as generated HTML?

You will need to install the AsciiDoctor program for your particular OS platform.  Supported platforms include:

* Linux/Ubuntu
* Mac
* Windows

After installation, you can follow the instructions here for converting the files:

https://docs.asciidoctor.org/asciidoctor/latest/get-started/

Run the asciidoctor program against all the adoc files except the attributes.adoc file.  This is just
used to hold some macros to make cross-referencing other pages easier.


