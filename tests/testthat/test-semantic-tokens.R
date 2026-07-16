test_that("Semantic tokens full works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test <- function(x, y) {",
            "  x + y",
            "}"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_semantic_tokens_full(temp_file)
    expect_true(!is.null(result$data))
    expect_true(length(result$data) > 0)
    # data should be multiples of 5 (line delta, start delta, length, type, modifiers)
    expect_equal(length(result$data) %% 5, 0)
})

test_that("Semantic tokens range works", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "test_var <- 42",
            "another_var <- test_var + 1"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    # Request tokens for the first line only
    result <- client %>% respond_semantic_tokens_range(
        temp_file,
        start_pos = c(0, 0),
        end_pos = c(1, 0)
    )
    expect_true(!is.null(result$data))
    # data should be multiples of 5
    expect_equal(length(result$data) %% 5, 0)
    decoded_lines <- cumsum(matrix(result$data, ncol = 5, byrow = TRUE)[, 1])
    expect_true(all(decoded_lines == 0L))
})

test_that("Semantic token range honors both range boundaries", {
    content <- sprintf("value_%02d <- %d", 1:10, 1:10)
    fixture <- provider_fixture(content)
    response <- semantic_tokens_range_reply(
        1L, fixture$uri, fixture$workspace, fixture$document,
        list(
            start = list(line = 5L, character = 0L),
            end = list(line = 7L, character = 0L)
        )
    )

    encoded <- matrix(response$result$data, ncol = 5, byrow = TRUE)
    decoded_lines <- cumsum(encoded[, 1])
    expect_true(length(decoded_lines) > 0L)
    expect_true(all(decoded_lines >= 5L & decoded_lines < 7L))
})

test_that("Semantic token deltas reconstruct the current result", {
    previous <- as.integer(c(0, 0, 1, 8, 0, 1, 0, 1, 8, 0))
    current <- as.integer(c(0, 0, 1, 8, 0, 1, 2, 1, 8, 0))
    edits <- semantic_token_delta(previous, current)

    expect_length(edits, 1L)
    edit <- edits[[1L]]
    before <- if (edit$start) previous[seq_len(edit$start)] else integer()
    after_start <- edit$start + edit$deleteCount + 1L
    after <- if (after_start <= length(previous)) {
        previous[seq.int(after_start, length(previous))]
    } else {
        integer()
    }
    expect_identical(c(before, edit$data, after), current)
})

test_that("Incomplete documents produce current empty parse data", {
    parsed <- parse_document("file:///incomplete.R", "x <- function(")
    expect_true(parsed$parse_error)
    expect_length(parsed$semantic_data$encoded, 0L)
    expect_false(is.null(parsed$xml_data))
})

test_that("Semantic token delta requests work through the language server", {
    skip_on_cran()
    client <- language_client()
    path <- withr::local_tempfile(fileext = ".R")
    writeLines("value <- 1", path)
    client %>% did_open(path)
    uri <- path_to_uri(path)

    previous <- respond_semantic_tokens_full(client, path)
    expect_true(nzchar(previous$resultId))
    notify(client, "textDocument/didChange", list(
        textDocument = list(uri = uri, version = 2L),
        contentChanges = list(list(
            range = list(
                start = list(line = 0L, character = 9L),
                end = list(line = 0L, character = 10L)
            ),
            text = "2"
        ))
    ))

    delta <- respond_semantic_tokens_delta(
        client, path, previous$resultId,
        retry_when = function(result) is.null(result$resultId)
    )
    expect_true(nzchar(delta$resultId))
    expect_false(identical(delta$resultId, previous$resultId))
    expect_false(is.null(delta$edits) && is.null(delta$data))
})

test_that("Semantic tokens contain expected types", {
    skip_on_cran()
    client <- language_client()

    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            "my_func <- function(param1, param2) {",
            "  result <- param1 + param2",
            "  result",
            "}"
        ),
        temp_file
    )

    client %>% did_open(temp_file)

    result <- client %>% respond_semantic_tokens_full(temp_file)
    expect_true(!is.null(result$data))
    expect_true(length(result$data) > 0)

    # Check that we have some tokens (data array with valid entries)
    # Each token is 5 elements: [line_delta, start_delta, length, type, modifiers]
    token_count <- length(result$data) %/% 5
    expect_true(token_count > 0)
})
