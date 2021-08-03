## code to prepare `microdata1` dataset goes here

microdata1 <- structure(
  list(
    region = c(
      "C", "C", "A", "A", "C", "D", "D",
      "A", "A", "C", "A", "B", "B", "B", "C", "D", "A", "B", "C", "A",
      "B", "D", "A", "C", "D", "D", "A", "B", "D", "C", "C", "B", "B",
      "B", "B", "D", "C", "D", "B", "C", "B", "A", "D", "B", "B", "D",
      "B", "C", "C", "B", "A", "C", "D", "B", "C", "B", "B", "A", "C",
      "D", "B", "A", "D", "D", "A", "A", "D", "D", "D", "A", "C", "B",
      "B", "D", "D", "B", "D", "D", "A", "B", "B", "C", "A", "B", "B",
      "B", "C", "A", "C", "C", "C", "D", "B", "D", "B", "B", "D", "B",
      "A", "B"),
  gender = c(
      "male", "male", "male", "male", "male",
      "female", "female", "female", "male", "male", "female", "female",
      "male", "male", "female", "female", "male", "male", "male", "male",
      "male", "male", "male", "female", "female", "male", "male", "female",
      "male", "male", "female", "male", "female", "female", "female",
      "male", "female", "male", "female", "male", "male", "male", "female",
      "female", "female", "male", "female", "male", "female", "female",
      "male", "female", "female", "female", "male", "male", "female",
      "male", "female", "male", "male", "male", "female", "female",
      "male", "male", "female", "female", "male", "male", "female",
      "male", "female", "female", "female", "female", "female", "male",
      "male", "female", "female", "female", "male", "male", "male",
      "male", "male", "male", "male", "female", "male", "male", "female",
      "female", "female", "male", "male", "female", "male", "male"),
   val = c(
     9, 11, 10, 11, 18, 10, 6, 9, 5, 12, 11, 10, 11, 5,
     10, 11, 7, 7, 10, 13, 12, 280, 15, 12, 16, 9, 13, 16, 9,
     6, 11, 14, 12, 8, 18, 8, 7, 7, 11, 11, 15, 6, 14, 8, 12,
     9, 14, 6, 12, 13, 9, 9, 9, 7, 6, 9, 9, 7, 9, 8, 10, 9, 7,
     6, 8, 9, 14, 10, 7, 10, 19, 10, 14, 11, 9, 5, 11, 8, 10,
     13, 8, 9, 18, 7, 10, 8, 11, 6, 6, 8, 12, 11, 10, 18, 9, 8,
     10, 7, 12, 14)
    ),
  row.names = c(NA, -100L),
  class = "data.frame"
)

usethis::use_data(microdata1, overwrite = TRUE)
