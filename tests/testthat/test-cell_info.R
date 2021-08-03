context("test cell_info()")

sdc <- testprob_a(with_supps = TRUE)
expect_is(sdc, "sdcProblem")
expect_equal(sum(sdc@problemInstance@sdcStatus == "u"), 1)

# check correct input
expect_error(cell_info(p))
expect_error(cell_info(p, 1))

# vector input
specs_vec <- c(region = "D", gender = "male")
res <- cell_info(sdc, specs = specs_vec)
expect_identical(nrow(res), 1L)
expect_identical(res$id, 14L)
expect_identical(res$strID, "0401")
expect_identical(res$region, "D")
expect_identical(res$gender, "male")
expect_identical(res$freq, 11)
expect_identical(res$val, 366)
expect_identical(res$sdcStatus, "s")

# data.frame input
specs_df <- data.frame(
  region = c("A", "D", "A"),
  gender = c("male", "female", "female")
)
res <- cell_info(sdc, specs = specs_df)
expect_identical(nrow(res), 3L)
expect_identical(res$id, as.integer(c(5, 15, 6)))
expect_identical(res$sdcStatus, c("s", "s", "u"))

# protect the table
sdc_safe <- protectTable(sdc, method = "SIMPLEHEURISTIC")
res <- cell_info(sdc_safe, specs = specs_df)
expect_identical(nrow(res), 3L)
expect_identical(res$id, as.integer(c(5, 15, 6)))
expect_identical(res$sdcStatus, c("x", "s", "u"))
