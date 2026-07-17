test_that("ByteLruCache is byte bounded and refreshes recency", {
    cache <- ByteLruCache$new(max_bytes = 10000, max_entries = 2L)
    cache$set("first", 1L)
    cache$set("second", 2L)
    expect_equal(cache$get("first"), 1L)

    cache$set("third", 3L)
    expect_true(cache$has("first"))
    expect_false(cache$has("second"))
    expect_true(cache$has("third"))
    expect_lte(cache$bytes(), 10000)
})

test_that("ByteLruCache does not retain an oversized value", {
    cache <- ByteLruCache$new(max_bytes = 100, max_entries = 10L)
    cache$set("large", raw(1000))
    expect_false(cache$has("large"))
    expect_equal(cache$bytes(), 0)
})
