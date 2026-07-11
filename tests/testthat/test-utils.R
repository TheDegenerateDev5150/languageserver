parse_xdoc <- function(code) {
    parsed <- parse(text = code, keep.source = TRUE)
    xml2::read_xml(xmlparsedata::xml_parse_data(parsed))
}

test_that("xdoc_find_token prefers strings at adjacent token boundaries", {
    xdoc <- parse_xdoc('setClass("BaseEntity",x+y)')

    token <- xdoc_find_token(xdoc, line = 1, col = 10)
    expect_equal(xml2::xml_name(token), "STR_CONST")
    expect_equal(xml2::xml_text(token), '"BaseEntity"')

    xdoc <- parse_xdoc('list(x,"Class")')
    token <- xdoc_find_token(xdoc, line = 1, col = 8)
    expect_equal(xml2::xml_name(token), "STR_CONST")
    expect_equal(xml2::xml_text(token), '"Class"')
})

test_that("xdoc_find_token retains document order for other boundaries", {
    xdoc <- parse_xdoc("f(x)")

    token <- xdoc_find_token(xdoc, line = 1, col = 3)
    expect_equal(xml2::xml_name(token), "OP-LEFT-PAREN")
})
