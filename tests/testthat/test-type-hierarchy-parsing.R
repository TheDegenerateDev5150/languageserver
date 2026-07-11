parse_type_hierarchy_xdoc <- function(code) {
    parsed <- parse(text = code, keep.source = TRUE)
    xml2::read_xml(xmlparsedata::xml_parse_data(parsed))
}

test_that("S4 hierarchy parsing handles named arguments", {
    code <- c(
        'setClass("BaseEntity")',
        'setClass("User", contains = "BaseEntity", slots = c(name = "character"))',
        'setClass("AdminUser", contains = "User")',
        'setMethod("show", "User", function(object) object)'
    )
    document <- Document$new("file:///s4.R", content = code)
    xdoc <- parse_type_hierarchy_xdoc(code)

    definition <- detect_s4class(xdoc, "User", document, document$uri)
    expect_equal(definition$range, range(position(1, 10), position(1, 14)))

    supertypes <- find_s4_supertypes(document, xdoc, "User", document$uri)
    expect_equal(map_chr(supertypes, "name"), "BaseEntity")
    expect_equal(supertypes[[1]]$range, range(position(1, 29), position(1, 39)))

    subtypes <- find_s4_subtypes(document, xdoc, "User", document$uri)
    expect_equal(map_chr(subtypes, "name"), "AdminUser")
    expect_equal(subtypes[[1]]$range, range(position(2, 10), position(2, 19)))

    members <- extract_s4_members(
        document, xdoc, list(name = "User", type = "S4")
    )
    expect_setequal(map_chr(members, "name"), c("name", "show"))
})

test_that("RefClass hierarchy parsing handles named arguments", {
    code <- c(
        'setRefClass("BaseReference")',
        paste0(
            'setRefClass("UserReference", contains = "BaseReference", ',
            'fields = list(name = "character", metadata = list(source = "character")), ',
            'methods = list(greet = function() paste("hi", sep = "-")))'
        ),
        'setRefClass("AdminReference", contains = "UserReference")'
    )
    document <- Document$new("file:///refclass.R", content = code)
    xdoc <- parse_type_hierarchy_xdoc(code)

    definition <- detect_refclass(xdoc, "UserReference", document, document$uri)
    expect_equal(definition$range, range(position(1, 13), position(1, 26)))

    supertypes <- find_refclass_supertypes(
        document, xdoc, "UserReference", document$uri
    )
    expect_equal(map_chr(supertypes, "name"), "BaseReference")
    expect_equal(supertypes[[1]]$range, range(position(1, 41), position(1, 54)))

    subtypes <- find_refclass_subtypes(
        document, xdoc, "UserReference", document$uri
    )
    expect_equal(map_chr(subtypes, "name"), "AdminReference")
    expect_equal(subtypes[[1]]$range, range(position(2, 13), position(2, 27)))

    members <- extract_refclass_members(
        document, xdoc, list(name = "UserReference", type = "RefClass")
    )
    expect_setequal(map_chr(members, "name"), c("name", "metadata", "greet"))
})
