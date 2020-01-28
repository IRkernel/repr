context('reprs of vectors')

test_that('empty vectors display correctly', {
	expect_id_text(repr_html(logical(0L)), '')
})

test_that('1 element vectors display correctly', {
	expect_id_text(repr_html(1), '1')
})

test_that('plain vectors display correctly', {
	expect_id_text(
		repr_html(c(1, 2)),
		paste0(list_style,'<ol class=list-inline><li>1</li><li>2</li></ol>\n')
	)
})

test_that('character vectors add quotes to non-NA', {
	expect_id_text(
		repr_html(c('a', NA, 'c')),
		paste0(list_style, "<ol class=list-inline><li>'a'</li><li>NA</li><li>'c'</li></ol>\n")
	)
})

test_that('named vectors display correctly', {
	expect_id_text(
		repr_html(c(a = 1, b = 2)),
		paste0(
			def_style,
			'<dl class=dl-inline>',
			'<dt>a</dt>',
			'<dd>1</dd>',
			'<dt>b</dt>',
			'<dd>2</dd>',
			'</dl>'
		)
	)
})

test_that('factors display correctly', {
	expect_id_text(
		repr_html(factor(c('a', 'b'))),
		sprintf(
"%s<ol class=list-inline><li>a</li><li>b</li></ol>

<details>
	<summary style=display:list-item;cursor:pointer>
		<strong>Levels</strong>:
	</summary>
%s
	<ol class=list-inline><li>'a'</li><li>'b'</li></ol>
</details>",
			list_style, strindent(list_style)
		)
	)
})

test_that('Dates display correctly', {
	expect_id_text(repr_html(as.Date('1111-11-11')), '<time datetime=\"1111-11-11\">1111-11-11</time>')
})

test_that('Date vectors display correctly', {
	expect_id_text(
		repr_html(c(as.Date('1111-11-11'), as.Date('1212-12-12'))),
		paste0(
			list_style,
			'<ol class=list-inline>',
			'<li><time datetime="1111-11-11">1111-11-11</time></li>',
			'<li><time datetime="1212-12-12">1212-12-12</time></li>',
			'</ol>\n'
		)
	)
})

test_that('Vectors get limited', {
	max_orig <- getOption('repr.vector.max.items')
	on.exit(options(repr.vector.max.items = max_orig))
	options(repr.vector.max.items = 3L)
	expect_id_text(
		repr_html(5:1),
		sprintf(
			'%s<ol class=list-inline><li>5</li><li>4</li><li>%s</li><li>1</li></ol>\n',
			list_style, ellip_h
		)
	)
})
