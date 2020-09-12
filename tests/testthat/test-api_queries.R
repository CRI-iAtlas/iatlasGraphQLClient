query_dir  <- system.file("queries", package = "iatlas.api.client")

# datasets ------------------------------------------------------------------

test_that("query_datasets", {
  result1 <- query_datasets(query_dir = query_dir)
  expect_named(result1, c("display", "name"))
  expect_true(nrow(result1) > 0)
  result2 <- query_datasets("non_dataset", query_dir = query_dir)
  expect_named(result2, c("display", "name"))
  expect_equal(nrow(result2), 0)
})

test_that("query_dataset_samples", {
  result1 <- query_dataset_samples("PCAWG", query_dir = query_dir)
  expect_named(result1, c("name"))
  expect_true(nrow(result1) > 0)
  result2 <- query_dataset_samples("non_dataset", query_dir = query_dir)
  expect_named(result2, c("name"))
  expect_equal(nrow(result2), 0)
})

# features ------------------------------------------------------------------

test_that("query_features", {
  expected_columns <- c(
    "name",
    "display",
    "class",
    "order",
    "unit",
    "method_tag"
  )

  result1 <- query_features(
    datasets = "PCAWG",
    parent_tags = "Immune_Subtype",
    features = "Lymphocytes_Aggregate1",
    query_dir = query_dir
  )
  result2 <- query_features(
    datasets = "PCAWG",
    parent_tags ="Immune_Subtype",
    features = "not_a_feature",
    query_dir = query_dir
  )

  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)

  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

test_that("query_feature_values", {
  expected_columns <- c(
    "sample",
    "feature_name",
    "feature_display",
    "feature_value",
    "feature_order"
  )

  result1 <- query_feature_values(
    datasets = "PCAWG",
    parent_tags = "Immune_Subtype",
    features = "Lymphocytes_Aggregate1",
    query_dir = query_dir
  )
  result2 <- query_feature_values(
    datasets = "PCAWG",
    parent_tags ="Immune_Subtype",
    features = "not_a_feature",
    query_dir = query_dir
  )

  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)

  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

test_that("query_features_range", {
  expected_columns <- c("name", "display", "value_min", "value_max")
  result1 <- query_features_range(
    "PCAWG",
    features = "Lymphocytes_Aggregate1",
    query_dir = query_dir
  )
  result2 <- query_features_range(
    "PCAWG",
    features = "not_a_feature",
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)

  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})



# features_by_tag -----------------------------------------------------------

test_that("query_feature_values_by_tag", {
  expected_columns <- c(
    "tag_name",
    "tag_display",
    "tag_color",
    "tag_characteristics",
    "sample",
    "value"
  )

  result1 <- query_feature_values_by_tag(
    "Lymphocytes_Aggregate1",
    datasets = "PCAWG",
    parent_tags = "Immune_Subtype",
    query_dir = query_dir
  )
  result2 <- query_feature_values_by_tag(
    "not_a_feature",
    datasets = "PCAWG",
    parent_tags = "Immune_Subtype",
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)

  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

test_that("query_features_values_by_tag", {
  expected_columns <- c(
    "tag_name",
    "tag_display",
    "tag_color",
    "tag_characteristics",
    "sample",
    "feature_name",
    "feature_display",
    "feature_value",
    "feature_order"
  )

  result1 <- query_features_values_by_tag(
    datasets = "PCAWG",
    parent_tags = "Immune_Subtype",
    feature_classes = "EPIC",
    query_dir = query_dir
  )
  result2 <- query_features_values_by_tag(
    datasets = "PCAWG",
    parent_tags = "Immune_Subtype",
    feature_classes = "not_a_class",
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)

  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

# features_by_class ---------------------------------------------------------

test_that("query_features_by_class", {
  expected_columns <- c(
    "class",
    "display",
    "name",
    "order",
    "unit",
    "method_tag"
  )
  result1 <- query_features_by_class(query_dir = query_dir)
  result2 <- query_features_by_class(
    feature_classes = "not_a_class", query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)

  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

# genes ---------------------------------------------------------------------

test_that("query_genes", {
  expected_columns <- c(
    "hgnc",
    "entrez",
    "description",
    "friendly_name",
    "io_landscape_name",
    "gene_family",
    "gene_function",
    "immune_checkpoint",
    "pathway",
    "super_category"
  )
  result1 <- query_genes(entrez = 1, query_dir = query_dir)
  result2 <- query_genes(entrez = -1, query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

test_that("query_immunomodulators", {
  expected_columns <-  c(
    "entrez",
    "hgnc",
    "friendly_name",
    "description",
    "gene_family",
    "gene_function",
    "immune_checkpoint",
    "super_category",
    "publications"
  )
  result1 <- query_immunomodulators(query_dir = query_dir)
  result2 <- query_immunomodulators(entrez = -1, query_dir = query_dir)

  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)

  ARG1_publications <- result1 %>%
    dplyr::filter(.data$entrez == 383L) %>%
    tidyr::unnest(cols = "publications") %>%
    dplyr::pull("pubmedId") %>%
    sort()

  expect_equal(ARG1_publications, c(19764983L, 23890059L))
})

test_that("query_io_targets", {
  result1 <- query_io_targets(query_dir = query_dir)
  expect_named(
    result1,
    c(
      "entrez",
      "hgnc",
      "description",
      "io_landscape_name" ,
      "pathway",
      "therapy_type"
    )
  )
})

test_that("query_expression_by_genes", {
  expected_columns <- c(
    "sample",
    "entrez",
    "hgnc",
    "rna_seq_expr"
  )
  result1 <- query_expression_by_genes(
    "entrez" = 135L, samples = "TCGA-XF-A9T8", query_dir = query_dir
  )
  result2 <- query_expression_by_genes(
    "entrez" = 0L, samples = "TCGA-XF-A9T8", query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_equal(nrow(result1), 1L)
  expect_equal(nrow(result2), 0L)
})

# gene types ----------------------------------------------------------------

test_that("query_gene_types", {
  expected_columns <- c("display", "name")

  result1 <- query_gene_types(query_dir = query_dir)
  result2 <- query_gene_types("not_a_type", query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

test_that("query_genes_by_gene_types", {
  expected_columns <- c("entrez", "hgnc", "gene_type_name", "gene_type_display")

  result1 <- query_genes_by_gene_types(query_dir = query_dir)
  result2 <- query_genes_by_gene_types("not_a_type", query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

# mutations -----------------------------------------------------------------

test_that("mutations", {
  expected_columns <-  c("id", "entrez", "hgnc", "code")
  result1 <- query_mutations(ids = 1, query_dir = query_dir)
  result2 <- query_mutations(entrez = -1, query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

# related -------------------------------------------------------------------

test_that("dataset_tags", {
  expected_columns <- c("name", "display")
  result1 <- query_dataset_tags("PCAWG", query_dir = query_dir)
  result2 <- query_dataset_tags("not_a_dataset", query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})


# samples by mutation status ------------------------------------------------

test_that("query_samples_by_mutation_status", {
  expected_names <- c("sample", "status")
  result1 <- query_samples_by_mutation_status(
    samples = "TCGA-2Z-A9J8", query_dir = query_dir)
  result2 <- query_samples_by_mutation_status(
    777, "Mut", "TCGA-2Z-A9J8", query_dir = query_dir
  )
  result3 <- query_samples_by_mutation_status(
    777, "Mut", "none", query_dir = query_dir
  )
  expect_named(result1, expected_names)
  expect_named(result2, expected_names)
  expect_named(result3, expected_names)
  expect_true(nrow(result1) > 0)
  expect_true(nrow(result2) > 0)
  expect_equal(nrow(result3),  0)
})

# samples by tag ------------------------------------------------------------

test_that("query_samples_by_tag", {
  expected_names <- c(
    "tag_name",
    "tag_display",
    "tag_characteristics",
    "tag_color",
    "sample"
  )
  result1 <- query_samples_by_tag(
    datasets = "PCAWG", parent_tags = "Immune_Subtype", query_dir = query_dir
  )
  result2 <- query_samples_by_tag(
    datasets = "PCAWG", parent_tags = "not_a_tag", query_dir = query_dir
  )
  expect_named(result1, expected_names)
  expect_named(result2, expected_names)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

test_that("query_tag_samples", {
  expected_names <- c("sample")
  result1 <- query_tag_samples(
    datasets = "PCAWG", tags = "C1", query_dir = query_dir
  )
  result2 <- query_tag_samples(
    datasets = "PCAWG", tags = "not_a_tag", query_dir = query_dir
  )
  expect_named(result1, expected_names)
  expect_named(result2, expected_names)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

# tags ----------------------------------------------------------------------

test_that("tags", {
  expected_columns <- c(
    "name",
    "display",
    "characteristics",
    "color",
    "sample_count"
  )

  result1 <- query_tags("PCAWG", "Immune_Subtype", query_dir = query_dir)
  result2 <- query_tags("PCAWG", "not_a_tag", query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

test_that("query_cohort_selector", {
  expected_columns <- c(
    "name",
    "display",
    "characteristics",
    "color",
    "size",
    "samples"
  )

  result1 <- query_cohort_selector(
    "PCAWG", "Immune_Subtype", query_dir = query_dir
  )
  result2 <- query_cohort_selector("PCAWG", "not_a_tag", query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})
