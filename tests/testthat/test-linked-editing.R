test_that("linked editing connects roxygen parameters and R formals", {
    expect_true(ServerCapabilities$linkedEditingRangeProvider)
    fixture <- provider_fixture(c(
        "#' Add one",
        "#' @param x A value.",
        "foo <- function(x) x + 1"
    ))
    reply <- linked_editing_range_reply(
        1L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(line = 1L, character = 10L)
    )

    expect_length(reply$result$ranges, 2L)
    expect_equal(reply$result$ranges[[1L]]$start$line, 2L)
    expect_equal(reply$result$ranges[[2L]]$start$line, 1L)
})

test_that("linked editing works through the language server", {
    skip_on_cran()
    client <- language_client()
    path <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "#' Add one",
        "#' @param x A value.",
        "foo <- function(x) x + 1"
    ), path)
    client %>% did_open(path)

    result <- respond(
        client,
        "textDocument/linkedEditingRange",
        list(
            textDocument = list(uri = path_to_uri(path)),
            position = list(line = 1L, character = 10L)
        )
    )
    expect_length(result$ranges, 2L)
})
