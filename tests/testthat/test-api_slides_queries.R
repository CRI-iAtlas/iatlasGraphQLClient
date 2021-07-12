test_that("query_slides", {
  expected_names <- c("name", "description")
  result1 <- query_slides(query_dir = query_dir)
  result2 <- query_slides("not_a_slide", query_dir = query_dir)
  expect_named(result1, expected_names)
  expect_named(result2, expected_names)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})
