dat <- structure(
  list(
    AFRAID = c(0, 0, 1, 0, 0, 0),
    AMUSED = c(2, 1,0, 3, 2, 3),
    ANGRY = c(2, 3, 1, 0, 2, 1),
    ANNOYED = c(1, 0, 1,
                0, 0, 1),
    DONT_CARE = c(1, 1, 1, 0, 0, 0),
    HAPPY = c(0, 0, 0,0, 0, 0),
    INSPIRED = c(4, 5, 6, 2, 6, 6),
    SAD = c(0, 2, 1, 0,2, 1)
  ),
  row.names = c(1L, 2L, 3L, 4L, 5L,
                6L),
  class = c("data.frame", "emoxicon", "emotions")
)

test_that("groups are the same length as observations", {
  expect_error(rasch(
    scores = dat,
    groups = "test"
  ),"The length of groups does not match the number of rows of data")
})
