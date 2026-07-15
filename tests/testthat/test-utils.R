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

test_that("indexed enclosing scope lookup preserves results", {
    xdoc <- parse_xdoc(c(
        "first <- 1",
        "fun <- function(argument) {",
        "  nested <- argument",
        "  nested",
        "}"
    ))
    expected <- xdoc_find_enclosing_scopes(xdoc, line = 4L, col = 5L,
        top = TRUE)

    attr(xdoc, "top_level_index") <- xdoc_top_level_index(xdoc)
    actual <- xdoc_find_enclosing_scopes(xdoc, line = 4L, col = 5L,
        top = TRUE)

    expect_equal(xml2::xml_path(actual), xml2::xml_path(expected))
})
