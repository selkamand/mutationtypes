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

  # PAVE term classification works correctly
  expect_equal(
    mutation_types_identify(c("upstream_gene_variant", "inframe_insertion", "inframe_deletion", "phased_inframe_insertion",
      "phased_inframe_deletion", "phased_missense", "stop_gained", "splice_acceptor_variant")) |> suppressMessages(),
    "PAVE"
  )

  # Dictionaries that could be explained by PAVE / SO default to SO (more common)
  expect_equal(
    mutation_types_identify(c("upstream_gene_variant", "intron_variant")) |> suppressMessages(),
    "SO")

  # Mix of MAF + SO terms returns 'UNKNOWN'
  expect_equal(
    mutation_types_identify(c("Nonstop_Mutation", "Splice_Region", "Nonstop_Mutation", "missense_variant")) |> suppressMessages(),
    "UNKNOWN"
  )

  # Mix of PAVE + MAF terms returns 'UNKNOWN'
  expect_equal(
    mutation_types_identify(c("Nonstop_Mutation", "Splice_Region", "Nonstop_Mutation", "inframe_insertion")) |> suppressMessages(),
    "UNKNOWN"
  )
})

test_that("mutation_types_identify handles empty and missing data appropriately", {

  expect_equal(
    mutation_types_identify(c('missense_variant'), verbose = FALSE),
    "SO"
  )

  # Not ignoring missing / empty
  expect_equal(
    mutation_types_identify(c(NA_character_), verbose = FALSE),
    "UNKNOWN"
  )

  expect_equal(
    mutation_types_identify(c(''), verbose = FALSE),
    "UNKNOWN"
  )

  expect_equal(
    mutation_types_identify(c('missense_variant', NA), verbose = FALSE),
    "UNKNOWN"
  )

  expect_equal(
    mutation_types_identify(c('missense_variant', ''), verbose = FALSE),
    "UNKNOWN"
  )

  # Ignoring missing / empty
  expect_equal(
    mutation_types_identify(c('missense_variant', NA),ignore_missing = TRUE, verbose = FALSE),
    "SO"
  )
  expect_equal(
    mutation_types_identify(c('missense_variant', ''), ignore_missing = TRUE, verbose = FALSE),
    "SO"
  )




})
