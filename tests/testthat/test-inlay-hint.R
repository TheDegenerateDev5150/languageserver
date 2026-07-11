test_that("inlay hints name non-obvious positional R arguments", {
    expect_true(ServerCapabilities$inlayHintProvider$resolveProvider)
    fixture <- provider_fixture(
        "mean(values, TRUE, na.rm = FALSE)",
        formals_resolver = function(...) alist(x =, trim = 0, na.rm = FALSE)
    )
    reply <- inlay_hint_reply(
        1L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(
            start = list(line = 0L, character = 0L),
            end = list(line = 0L, character = 35L)
        )
    )

    expect_equal(
        vapply(reply$result, `[[`, character(1L), "label"),
        "trim ="
    )
    expect_true(all(vapply(reply$result, `[[`, integer(1L), "kind") == 2L))

    resolved <- inlay_hint_resolve_reply(2L, reply$result[[1L]])$result
    expect_match(resolved$tooltip$value, "`trim`")
    expect_match(resolved$tooltip$value, "`mean\\(\\)`")
})

test_that("inlay hints skip syntax and simple calls", {
    fixture <- provider_fixture(
        c(
            "if (argument) value",
            "fun <- function(argument, second, third) other(argument)",
            "other(argument)",
            "other(argument, second)"
        ),
        formals_resolver = function(...) {
            alist(argument =, second =, third =)
        }
    )
    reply <- inlay_hint_reply(
        1L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(
            start = list(line = 0L, character = 0L),
            end = list(line = 3L, character = 30L)
        )
    )

    expect_length(reply$result, 0L)
})

test_that("inlay hints are shown for two supplied arguments", {
    fixture <- provider_fixture(
        "target(one, two)",
        formals_resolver = function(...) alist(first =, second =)
    )
    reply <- inlay_hint_reply(
        1L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(
            start = list(line = 0L, character = 0L),
            end = list(line = 0L, character = 16L)
        )
    )

    expect_equal(
        vapply(reply$result, `[[`, character(1L), "label"),
        c("first =", "second =")
    )
})

test_that("inlay hint argument length excludes an initial dot", {
    old_minimum <- lsp_settings$get("inlay_hints_minimum_argument_length")
    withr::defer(lsp_settings$set(
        "inlay_hints_minimum_argument_length",
        old_minimum
    ))
    lsp_settings$set("inlay_hints_minimum_argument_length", 3L)

    fixture <- provider_fixture(
        "target(one, two)",
        formals_resolver = function(...) alist(.ab =, .abc =)
    )
    reply <- inlay_hint_reply(
        1L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(
            start = list(line = 0L, character = 0L),
            end = list(line = 0L, character = 16L)
        )
    )

    expect_equal(
        vapply(reply$result, `[[`, character(1L), "label"),
        ".abc ="
    )
})

test_that("inlay hints do not use global formals for member calls", {
    fixture <- provider_fixture(
        c(
            "ResponseErrorMessage$new(",
            "    id,",
            "    errortype = \"RequestCancelled\",",
            "    message = \"Cannot rename the symbol\"",
            ")"
        ),
        formals_resolver = function(...) alist(Class =, ...)
    )
    reply <- inlay_hint_reply(
        1L,
        fixture$uri,
        fixture$workspace,
        fixture$document,
        list(
            start = list(line = 0L, character = 0L),
            end = list(line = 4L, character = 1L)
        )
    )

    expect_length(reply$result, 0L)
})

test_that("inlay hints work through the language server", {
    skip_on_cran()
    client <- language_client(capabilities = list(textDocument = list(
        inlayHint = list(resolveSupport = list(properties = list("tooltip")))
    )))
    path <- withr::local_tempfile(fileext = ".R")
    writeLines("stats::rnorm(10, 1, 2)", path)
    client %>% did_open(path)

    hints <- respond(
        client,
        "textDocument/inlayHint",
        list(
            textDocument = list(uri = path_to_uri(path)),
            range = list(
                start = list(line = 0L, character = 0L),
                end = list(line = 0L, character = 24L)
            )
        )
    )
    expect_equal(
        vapply(hints, `[[`, character(1L), "label"),
        c("mean =", "sd =")
    )
})
