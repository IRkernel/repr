repr&emsp;[![Build Status](https://travis-ci.org/IRkernel/repr.svg?branch=master)](https://travis-ci.org/IRkernel/repr)
====

String and byte representations for all kinds of R objects.

This package has several generic functions to create all mimetypes that [Jupyter][] knows about, but is totally independent of it.

It is intended to be the basis of several packages that need to create rich text and graphics from R objects, such as the Jupyter [IRkernel][], [knitr][], and others, such as a future more powerful replacement for `R CMD Rd2pdf`.

[Jupyter]: http://jupyter.org/
[IRkernel]: https://github.com/IRkernel/IRkernel
[knitr]: http://yihui.name/knitr/

Exports
-------

`repr` is a function delegating to the individual `repr_*` functions.

`repr_*`, e.g. `repr_text`, `repr_html`, and `repr_png` emit single-element character vectors or raw vectors. They have parameters also configurable via global `options`.

`mime2repr` is a list mapping all known mimetypes to `repr_*` functions, e.g. `mime2repr[['application/pdf']]` is `repr_pdf`.

`format2repr` does the same for simple format names. So `format2repr$markdown` is `repr_markdown`.

Imports
-------

Per default, `repr` will not use any packages not part of the R distribution.

Only if you want to use `repr_html.function` and `repr_latex.function`, and specify the parameter `highlight` or option `repr.function.highlight` to be `TRUE`, then be sure to have [highr][] installed.

[highr]: https://github.com/yihui/highr

Formats
-------

Currently, the actually emitted formats are:

* Plain text, for everything, using `capture.output(print(thing))`. This will fail if `print(thing)` plots it instead. Please report classes which do that and aren’t handled yet (such as `recordedplot`).

* HTML, Markdown, and LaTeX, which are emitted for everything non-graphical

* PNG, JPG, SVG, and PDF for everything graphical (ATM just `recordedplot`)

Why not Pander?
---------------

[Pander][] Is very configurable and does the same as this one, only just for Markdown. Why don’t we use it and use [Pandoc][] to convert to other formats like it?

Because it just emits [Markdown][], which is the least semantic format available. A roundtrip through Markdown will undoubtedly create sub-par HTML and LaTeX.

Also Pander supports only text. Plots and images are also important to represent.

Pander is however awesome for high-quality Markdown so this project might want to depend on it.

[Pander]: http://rapporter.github.io/pander/
[Pandoc]: http://pandoc.org/
[Markdown]: http://whatismarkdown.com/
