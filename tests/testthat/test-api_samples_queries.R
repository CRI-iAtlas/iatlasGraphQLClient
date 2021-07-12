
test_that("query_samples", {
  expected_columns <- c("sample")
  result1 <- query_samples("TCGA-05-4244", query_dir = query_dir)
  result2 <- query_samples("not_a_sample", query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

test_that("query_sample_patients", {
  expected_columns <- c(
    "sample_name",
    "patient_name",
    "patient_age_at_diagnosis",
    "patient_ethnicity",
    "patient_gender",
    "patient_height",
    "patient_race",
    "patient_weight"
  )
  result1 <- query_sample_patients("TCGA-05-4244", query_dir = query_dir)
  result2 <- query_sample_patients("not_a_sample", query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})
