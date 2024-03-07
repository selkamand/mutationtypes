test_that("multiplication works", {
  expect_equal(
    select_most_severe_consequence_so(character(0)),
    character(0)
  )

  expect_equal(
    select_most_severe_consequence_pave(character(0)),
    character(0)
  )
})
