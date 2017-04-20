#' Representations of factors
#' 
#' @param obj  The factor to create a representation for
#' @param ...  ignored
#' 
#' @aliases repr_markdown.factor repr_latex.factor repr_html.factor
#' @name repr_*.factor
#' @include repr_vector.r
#' @include utils.r
NULL

repr_factor_generic <- function(template, repr_vec, repr_lvls = repr_vec)
	function(obj, ...) sprintf(template, repr_vec(obj, ...), repr_lvls(levels(obj)))

# repr_text is defined via print

#' @name repr_*.factor
#' @export
repr_html.factor <- repr_factor_generic(
# "display: list-item" because of https://github.com/jupyter/notebook/issues/2223
'%s
<details>
	<summary style=display:list-item;cursor:pointer>
		<strong>Levels</strong>:
	</summary>
%s
</details>', repr_html.character, function(lvls) strindent(repr_html.character(lvls)))

#' @name repr_*.factor
#' @export
repr_markdown.factor <- repr_factor_generic('%s\n**Levels**: %s', repr_markdown.character)

#' @name repr_*.factor
#' @export
repr_latex.factor <- repr_factor_generic('%s\n\\emph{Levels}: %s', repr_latex.character)
