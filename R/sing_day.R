#' Takes a noun and makes it plural
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
sing_day <- function(dataset, line, day_col, phrase_col){

  phrases <- dataset %>% pull({{phrase_col}})

  days <- dataset %>% pull({{day_col}})

  output <- "On the"

  output <- paste(output, days[line])

  output <- paste(output, "day of Christmas, my true love sent to me,")

  if(line == 1){
    output <- paste(output, phrases[1], sep = '\n')
  }
  else{

    lines <- paste(phrases[line:2], collapse='\n')

    output <- paste(output, lines, sep = "\n")

    output <- paste(output, "and", sep = '\n')

    output <- paste(output, phrases[1], sep = ' ')
  }

  return(output)
}
