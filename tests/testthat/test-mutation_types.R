test_that("mutation_types_mapping_so_to_maf works", {

  # Runs without error
  expect_error(
    mutation_types_mapping_so_to_maf(),
    NA
  )

  # Returns data.frame
  expect_s3_class(
    mutation_types_mapping_so_to_maf(),
    "data.frame"
  )

  # Columns named appropriately
  expect_named(
    mutation_types_mapping_so_to_maf(),
    c("SO", "MAF")
  )

  # Expect at least some reasonable number of rows
  expect_true(
    nrow(mutation_types_mapping_so_to_maf()) > 10
  )

})
