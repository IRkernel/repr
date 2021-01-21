repr&emsp;[![b-Travis]][Travis] [![b-CRAN]][CRAN]
====

[b-Travis]: https://travis-ci.com/IRkernel/repr.svg?branch=master "Build status"
[Travis]: https://travis-ci.com/IRkernel/repr
[b-CRAN]: https://www.r-pkg.org/badges/version/repr "Comprehensive R Archive Network"
[CRAN]: https://cran.r-project.org/package=repr

String and byte representations for all kinds of R objects.

This package exists to reliably create readable text (and viewable image) representations of data without the side effects [`print()`][print] can cause, such as invoking a [pager][file_show] and plotting to a plot device. In other words, all repr functions and methods are pure.

It is intended to be the basis of several packages that need to create rich text and graphics from R objects, such as [Jupyter][]’s [IRkernel][], [knitr][], and others, such as a future more powerful replacement for `R CMD Rd2pdf`.

[print]: https://stat.ethz.ch/R-manual/R-devel/library/base/html/print.html
[file_show]: https://stat.ethz.ch/R-manual/R-devel/library/base/html/file.show.html
[Jupyter]: https://jupyter.org/
[IRkernel]: https://github.com/IRkernel/IRkernel
[knitr]: https://yihui.org/knitr/

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

Because it emits just [Markdown][], which is the least semantic format available. A roundtrip through Markdown will undoubtedly create sub-par HTML and LaTeX.

Also Pander supports only text. Plots and images are also important to represent.

Pander is however awesome for high-quality Markdown so this project might want to depend on it.

[Pander]: http://rapporter.github.io/pander/
[Pandoc]: https://pandoc.org/
[Markdown]: https://commonmark.org/
