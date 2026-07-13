test_that("code lenses lazily resolve R function call counts", {
    expect_true(ServerCapabilities$codeLensProvider$resolveProvider)
    fixture <- provider_fixture(c(
        "foo <- function(x) x + 1",
        "foo(1)",
        "foo(2)",
        "other::foo(3)"
    ))
    capabilities <- list(textDocument = list(codeLens = list(
        resolveSupport = list(properties = list("command"))
    )))

    reply <- code_lens_reply(
        1L, fixture$uri, fixture$workspace, fixture$document, capabilities)
    expect_length(reply$result, 1L)
    expect_null(reply$result[[1L]]$command)

    resolved <- code_lens_resolve_reply(
        2L, fixture$workspace, reply$result[[1L]])$result
    expect_equal(resolved$command$title, "2 calls")
    expect_equal(
        resolved$command$command,
        "editor.showCallHierarchy"
    )
    expect_null(resolved$command$arguments)
})

test_that("code lenses work through the language server after incremental edits", {
    skip_on_cran()
    client <- language_client(capabilities = list(textDocument = list(
        codeLens = list(resolveSupport = list(properties = list("command")))
    )))
    path <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "foo <- function(x) x + 1",
        "foo(1)"
    ), path)
    client %>% did_open(path)
    uri <- path_to_uri(path)

    lenses <- respond(
        client,
        "textDocument/codeLens",
        list(textDocument = list(uri = uri))
    )
    expect_length(lenses, 1L)
    resolved <- respond(client, "codeLens/resolve", lenses[[1L]])
    expect_equal(resolved$command$title, "1 call")
    expect_equal(resolved$command$command, "editor.showCallHierarchy")

    notify(client, "textDocument/didChange", list(
        textDocument = list(uri = uri, version = 2L),
        contentChanges = list(list(
            range = list(
                start = list(line = 0L, character = 0L),
                end = list(line = 0L, character = 3L)
            ),
            rangeLength = 3L,
            text = "bar"
        ))
    ))
    changed_lenses <- respond(
        client,
        "textDocument/codeLens",
        list(textDocument = list(uri = uri))
    )
    expect_equal(changed_lenses[[1L]]$data$symbol, "bar")
})
