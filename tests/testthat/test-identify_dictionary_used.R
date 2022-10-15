test_that("mutation_types_identify works", {

  # Runs without error
  expect_error(
    mutation_types_identify(
      c(
        "INTRAGENIC",
        "INTRAGENIC",
        "intergenic_region",
        "stop_lost",
        "missense_variant",
        "missense_variant"
      )
    ) |> suppressMessages(),
    NA
  )

  #SO classification works correctly
  expect_equal(
    mutation_types_identify(
      c(
        "INTRAGENIC",
        "INTRAGENIC",
        "intergenic_region",
        "stop_lost",
        "missense_variant",
        "missense_variant"
      )
    ) |> suppressMessages(),
    "SO"
  )

  # Addition of a non SO term leads 'UNKNOWN''
  expect_equal(
    mutation_types_identify(
      c(
        "RANDOMTERM",
        "INTRAGENIC",
        "intergenic_region",
        "stop_lost",
        "missense_variant",
        "missense_variant"
      )
    ) |> suppressMessages(),
    "UNKNOWN"
  )

  # MAF term classification works correctly
  expect_equal(
    mutation_types_identify(c("Nonstop_Mutation", "Splice_Region", "Nonstop_Mutation")) |> suppressMessages(),
    "MAF"
  )

  # Mix of MAF + SO terms returns 'UNKNOWN'
  expect_equal(
    mutation_types_identify(c("Nonstop_Mutation", "Splice_Region", "Nonstop_Mutation", "missense_variant")) |> suppressMessages(),
    "UNKNOWN"
  )
})
