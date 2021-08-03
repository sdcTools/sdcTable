## code to prepare `microdata2` dataset goes here

microdata2 <- structure(list(
  region = c(rep("R1", 51), rep("R2", 49)),
  gender = c(rep("f", 26), rep("m", 25), rep("f", 18), rep("m", 31)),
  ecoOld = c(
    "Aa", "Bb", "Ab", "Aa", "Bb", "Ab", "Ba", "Ba", "Bb", "Aa", "Ab", "Ba",
    "Ba", "Ab", "Ab", "Ba", "Bb", "Bb", "Ba", "Ab", "Bb", "Aa", "Ba", "Aa",
    "Ab", "Bb", "Ab", "Bb", "Ab", "Ab", "Aa", "Aa", "Ab", "Ba", "Bb", "Ba",
    "Ba", "Bb", "Bb", "Aa", "Ba", "Aa", "Aa", "Ab", "Ba", "Aa", "Ab", "Bb",
    "Aa", "Ab", "Ab", "Aa", "Ba", "Bb", "Ba", "Ab", "Aa", "Bb", "Ba", "Aa",
    "Ba", "Ab", "Ba", "Ab", "Ab", "Aa", "Ba", "Bb", "Aa", "Ab", "Ab", "Ba",
    "Bb", "Ab", "Bb", "Bb", "Ab", "Ba", "Bb", "Ab", "Ab", "Ba", "Ba", "Ab",
    "Aa", "Aa", "Ba", "Aa", "Bb", "Bb", "Aa", "Aa", "Ba", "Aa", "Ba", "Ab",
    "Aa", "Ba", "Ab", "Bb"),
  ecoNew = c(
    "Cc", "Dc", "Cc", "Cb", "Dc", "Cb", "Db", "Db", "Db", "Ca", "Ca", "Da",
    "Dc", "Cb", "Cb", "Da", "Dc", "Dc", "Db", "Cc", "Dc", "Cb", "Dc", "Ca",
    "Cc", "Db", "Ca", "Db", "Ca", "Cb", "Cb", "Cc", "Cc", "Dc", "Db", "Db",
    "Db", "Db", "Da", "Ca", "Db", "Cb", "Ca", "Cc", "Da", "Ca", "Cc", "Da",
    "Cb", "Cc", "Cb", "Cc", "Db", "Db", "Da", "Ca", "Cb", "Db", "Da", "Cc",
    "Da", "Cc", "Da", "Cb", "Ca", "Cb", "Dc", "Db", "Ca", "Cb", "Ca", "Dc",
    "Dc", "Cc", "Da", "Dc", "Cc", "Da", "Db", "Cc", "Ca", "Dc", "Db", "Cc",
    "Ca", "Cb", "Db", "Ca", "Dc", "Dc", "Ca", "Cc", "Da", "Ca", "Db", "Ca",
    "Cc", "Dc", "Cb", "Da"),
  numVal = as.integer(c(
    17, 15, 5, 11, 20, 10, 20, 8, 15, 20, 8, 3, 9, 15, 11, 1, 10, 7, 9, 9,
    13, 20, 10, 9, 16, 18, 17, 18, 11, 6, 10, 3, 13, 9, 6, 1, 13, 5, 3, 2,
    16, 3, 4, 20, 19, 8, 19, 11, 9, 10, 10, 4, 20, 3, 2, 8, 7, 19, 1, 5, 5,
    20, 8, 4, 18, 11, 4, 10, 11, 3, 9, 10, 8, 10, 17, 2, 13, 14, 19, 5, 4,
    9, 7, 14, 6, 19, 2, 6, 15, 16, 18, 12, 18, 8, 1, 1, 10, 4, 20, 2))
  ),
  row.names = c(NA, 100L),
  class = "data.frame"
)

usethis::use_data(microdata2, overwrite = TRUE)
