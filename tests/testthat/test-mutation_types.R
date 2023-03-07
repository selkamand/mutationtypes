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

test_that("mutation_types_so and mutation_types_maf work",{

  # Run without error
  expect_error(
    mutation_types_so(),
    NA
  )
  expect_error(
    mutation_types_maf(),
    NA
  )

  # Expect character type
  expect_type(
    mutation_types_so(),
    "character"
  )
  expect_type(
    mutation_types_maf(),
    "character"
  )

  # Length > 0
  expect_true(
    length(mutation_types_so()) > 0
  )
  expect_true(
    length(mutation_types_maf()) > 0
  )

  # No NA's returned
  expect_true(
    !any(is.na(mutation_types_so()))
  )
  expect_true(
    !any(is.na(mutation_types_maf()))
  )

  # All mutation types have names > 1 character
  expect_true(
    all(nchar(mutation_types_so()) > 0)
  )
  expect_true(
    all(nchar(mutation_types_maf()) > 0)
  )

})

test_that("uniform set of SO and MAF terms included in all files", {

  df_mapping <- mutation_types_mapping_so_to_maf()
  vec_so <- mutation_types_so()
  vec_maf <- mutation_types_maf()

  # All terms in SO-MAF mapping should be in the individual SO/MAF valid terms list, and vice versa.
  # If this fails, go to the tsv's containing terms lists and make sure SO/MAF columns across different files
  # cover all the valid terms



  # Exclude Frame_Shift_Del and Frame_Shift_Ins since
  # they can't be mapped from SO terms without additional info
  vec_maf <- vec_maf[!vec_maf %in% c('Frame_Shift_Del', 'Frame_Shift_Ins')]


  expect_equal(
    sort(unique(na.omit(df_mapping[['SO']]))),
    sort(vec_so)
  )

  expect_equal(
    sort(unique(na.omit(df_mapping[['MAF']]))),
    sort(vec_maf)
  )

})


test_that("palette functions work", {

  # Run without error
  expect_error(
    mutation_types_so_palette(),
    NA
  )
  expect_error(
    mutation_types_maf_palette(),
    NA
  )

  # Expect character type
  expect_type(
    mutation_types_so_palette(),
    "character"
  )
  expect_type(
    mutation_types_maf_palette(),
    "character"
  )

  # Length > 0
  expect_true(
    length(mutation_types_so_palette()) > 0
  )
  expect_true(
    length(mutation_types_maf_palette()) > 0
  )

  # No NA's returned
  expect_true(
    !any(is.na(mutation_types_so_palette()))
  )
  expect_true(
    !any(is.na(mutation_types_maf_palette()))
  )

  # All colours have names > 1 character
  expect_true(
    all(nchar(mutation_types_so_palette()) > 0)
  )
  expect_true(
    all(nchar(mutation_types_maf_palette()) > 0)
  )

  # Named appropriated
  expect_named(
    mutation_types_so_palette(),
    mutation_types_so()
  )
  expect_named(
    mutation_types_maf_palette(),
    mutation_types_maf()
  )
})

test_that("mutation_types_so_with_priority produces a dataframe with EffectPriority sorted in ascending order", {
  # This is VERY important for identifying the most severe SO term
  expect_true(!is.unsorted(mutation_types_so_with_priority()[["EffectPriority"]]))
})

