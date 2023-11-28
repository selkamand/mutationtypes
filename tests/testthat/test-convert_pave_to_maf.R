test_that("mutation_types_convert_pave_to_maf works", {

  # Define all pave terms EXCEPT `frameshift` which requires additional info to map to MAF types
  pave_terms <- c("upstream_gene_variant", "intron_variant", "5_prime_UTR_variant",
                "3_prime_UTR_variant", "non_coding_transcript_exon_variant",
                "synonymous_variant", "phased_synonymous", "missense_variant",
                "inframe_insertion", "inframe_deletion", "phased_inframe_insertion",
                "phased_inframe_deletion", "phased_missense", "stop_gained", "start_lost", "stop_lost", "splice_donor_variant",
                "splice_acceptor_variant")


  expect_error(
    mutation_types_convert_pave_to_maf(pave_terms[1], verbose = TRUE) |> suppressMessages(),
    NA
  )

  expect_error(
    mutation_types_convert_pave_to_maf(pave_terms, verbose = TRUE) |> suppressMessages(),
    NA
  )

  expect_type(
    mutation_types_convert_pave_to_maf(pave_terms, verbose = TRUE) |> suppressMessages(),
    "character"
  )

  expect_length(
    mutation_types_convert_pave_to_maf(pave_terms, verbose = TRUE) |> suppressMessages(),
    n=length(pave_terms)
  )

  expect_equal(
    mutation_types_convert_pave_to_maf("missense_variant", verbose = TRUE) |> suppressMessages(),
    "Missense_Mutation"
  )

  expect_error(
    mutation_types_convert_pave_to_maf("frameshift", verbose = TRUE) |> suppressMessages(),
    'To convert PAVE term.*variant classification, you must supply argument .*variant_type'
  )


  # Test conversions of frameshift and inframe_deletion
  expect_equal(
    mutation_types_convert_pave_to_maf(
      c("frameshift", "frameshift"),
      variant_type = c("DEL", "INS"),
      verbose = FALSE) |> suppressMessages(),
    c("Frame_Shift_Del", "Frame_Shift_Ins")
  )

  # Test that errors are thrown when inframe_deletion and inframe_insertion disagree with variant type
  expect_error(
    mutation_types_convert_pave_to_maf(
      c("inframe_insertion"),
      variant_type = c("DEL", "INS"),
      verbose = FALSE) |> suppressMessages(),
    "Variant Type must be INS when pave_mutation_type is 'inframe_insertion'"
  )
  expect_error(
    mutation_types_convert_pave_to_maf(
      c("inframe_deletion"),
      variant_type = c("INS"),
      verbose = FALSE) |> suppressMessages(),
    "Variant Type must be DEL when pave_mutation_type is 'inframe_deletion'"
  )

  # Test & seperated consequences are resolved correctly
  expect_equal(
    mutation_types_convert_pave_to_maf("missense_variant&splice_acceptor_variant", verbose = TRUE) |> suppressMessages(),
    "Splice_Site"
  )

  # Test & seperated consequences are resolved correctly
  expect_equal(
    mutation_types_convert_pave_to_maf(c("stop_lost&missense_variant", "stop_gained&upstream_gene_variant&phased_synonymous"), verbose = TRUE) |> suppressMessages(),
    c("Nonstop_Mutation","Nonsense_Mutation")
  )


  # Test
  expect_error(
    mutation_types_convert_pave_to_maf(pave_mutation_types = "frameshift", variant_type = "SNP"),
    regexp = "Variant Type must be INS or DEL when pave_mutation_type is frameshift"
  )

})

test_that("mutation_types_convert_pave_to_maf() throws appropriate errors", {
  expect_error(assert_all_mutations_are_valid_pave("bob"), regexp =  "Found 1 mutation type which was NOT a valid PAVE terms: [bob]", fixed = TRUE)
})
