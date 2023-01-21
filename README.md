# Djota

A [Djot](https://djot.net/) implementation in Prolog. Djot is a markup language inspired by Markdown but different design goals. See [Beyond Markdown](https://johnmacfarlane.net/beyond-markdown.html) and [Design goals of Djot](https://github.com/jgm/djot#rationale) for more information. [Learn more about the syntax of Djot here](https://htmlpreview.github.io/?https://github.com/jgm/djot/blob/master/doc/syntax.html) 

This is a ISO Prolog implementation of Djot, useful to generate HTML fragments.

## Usage

```
:- use_module(djota).

?- djota(+InputStr, -OutputHtml).
```

where InputStr is a string in Djot, and OutputHTML unifies with the output HTML corresponding to that file.

### Example

Let's render this README with Djota!

```
:- use_module(djota).
:- use_module(library(dcgs)).
:- use_module(library(pio)).

? - phrase_from_file(seq(Input), "README.md"), djot(Input, HTML), phrase_to_file(seq(HTML), "README.html").
```

Runs on:

* [Scryer Prolog](https://github.com/mthom/scryer-prolog)
* [Trealla Prolog](https://github.com/trealla-prolog/trealla)

Supported stuff:

* Paragraphs
* Inline links
* Inline images
* Autolinks
* Verbatim
* Emphasis
* Strong
* Highlighted
* Super/subscript
* Insert/delete
* Inline attributes
* Headings
* Blockquotes
* Lists
* Code blocks
* Thematic breaks
* Raw blocks
* Divs
* Tables
