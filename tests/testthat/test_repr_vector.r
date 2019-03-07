context('reprs of vectors')

test_that('empty vectors display correctly', {
	expect_id_text(repr_html(logical(0L)), '')
})

test_that('1 element vectors display correctly', {
	expect_id_text(repr_html(1), '1')
})

test_that('plain vectors display correctly', {
	expect_id_text(repr_html(c(1, 2)),
'<ol class=list-inline>
\t<li>1</li>
\t<li>2</li>
</ol>
')
})

test_that('character vectors add quotes to non-NA', {
	expect_id_text(repr_html(c('a', NA, 'c')),
"<ol class=list-inline>
\t<li>'a'</li>
\t<li>NA</li>
\t<li>'c'</li>
</ol>
")
})

test_that('named vectors display correctly', {
	expect_id_text(repr_html(c(a = 1, b = 2)),
'<dl class=dl-horizontal>
\t<dt>a</dt>
\t\t<dd>1</dd>
\t<dt>b</dt>
\t\t<dd>2</dd>
</dl>
')
})

test_that('factors display correctly', {
	expect_id_text(repr_html(factor(c('a', 'b'))),
"<ol class=list-inline>
\t<li>a</li>
\t<li>b</li>
</ol>

<details>
	<summary style=display:list-item;cursor:pointer>
		<strong>Levels</strong>:
	</summary>
	<ol class=list-inline>
		<li>'a'</li>
		<li>'b'</li>
	</ol>
</details>")
})

test_that('Dates display correctly', {
	expect_id_text(repr_html(as.Date('1111-11-11')), '<time datetime=\"1111-11-11\">1111-11-11</time>')
})

test_that('Date vectors display correctly', {
	expect_id_text(repr_html(c(as.Date('1111-11-11'), as.Date('1212-12-12'))),
'<ol class=list-inline>
\t<li><time datetime="1111-11-11">1111-11-11</time></li>
\t<li><time datetime="1212-12-12">1212-12-12</time></li>
</ol>
')
})
