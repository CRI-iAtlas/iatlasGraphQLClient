test_that("query_patients", {
  expected_columns <- c(
    "age_at_diagnosis",
    "patient",
    "ethnicity",
    "gender",
    "height",
    "race",
    "weight"
  )
  result1 <- query_patients("TCGA-05-4244", query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_equal(nrow(result1), 1)

  result2 <- query_patients("not_a_patient", query_dir = query_dir)
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)

  result3 <- query_patients(datasets = "PCAWG", query_dir = query_dir)
  expect_named(result3, expected_columns)
  expect_equal(nrow(result3), 455)
})

test_that("query_patient_slides", {
  expected_columns <- c(
    "slide_name",
    "slide_description",
    "patient_age_at_diagnosis",
    "patient_name",
    "patient_ethnicity",
    "patient_gender",
    "patient_height",
    "patient_race",
    "patient_weight"
  )
  result1 <- query_patient_slides("TCGA-05-4244", query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)

  result2 <- query_patient_slides("not_a_patient", query_dir = query_dir)
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)
})
