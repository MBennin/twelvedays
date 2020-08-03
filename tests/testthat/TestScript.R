context("Testing Script for Lab 5")

test_that("Running pluralize_gift",{
  names <- c("Kimberly", "Trini", "Jason", "Billy", "Zach", "Tommy")

  names <- purrr::map_chr(names, pluralize_gift)

  expect_equal(names, c("Kimberlies", "Trinis", "Jasons", "Billies", "Zachs", "Tommies"))
})

test_that("Running make_phrase", {

  output <- make_phrase(num = 10,
              num_word = "ten",
              item = "lords",
              verb = "a-leaping",
              adjective = "",
              location = "")

  expect_equal(output, "ten lords a-leaping")

})

test_that("Running sing_day", {

  xmas2 <- read.csv("https://www.dropbox.com/s/e584pryn8evm1gz/xmas.csv?dl=1")

  xmas2$Gift.Item[2:12] <- purrr::map_chr(xmas2$Gift.Item[2:12], pluralize_gift)

  xmas2 <- xmas2 %>% mutate(Full.Phrase = pmap(list(xmas2$Day, xmas2$Day.in.Words, xmas2$Gift.Item, xmas2$Verb, xmas2$Adjective, xmas2$Location), make_phrase))

  output <- sing_day(xmas2, 2, Day.in.Words, Full.Phrase)

  expect_equal(output, "On the second day of Christmas, my true love sent to me,
second turtle doves
and first partridge in a pear tree")

})
