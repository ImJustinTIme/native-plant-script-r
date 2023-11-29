# Load the required libraries
library(readxl)
library(rvest)
# Define the URL of the website

grabExcelPlantNames <- function(fileName) {
  xl <- read_excel(fileName)
  xl
  return(xl)
}

scrap_ncsu_website <- function(url) {
  webpage <- read_html(url)
  main_content <- webpage %>%
    html_elements("main") # %>%

  brick_list <- main_content %>%
    html_elements(".brick") %>%
    html_elements("dl")

  # Initialize empty lists to store titles and descriptions
  titles_list <- list()
  descriptions_list <- list()
  current_title <- character()
  # Iterate through each dl element

  child_list <- html_children(brick_list)
  for (i in seq_along(child_list)) {
    inner_element <- child_list[i]
    inner_element_class <- html_name(inner_element)

    if (inner_element_class == "dt") {
      inner_element_text <- inner_element %>%
        html_text(trim = TRUE)
      if (length(descriptions_list) > 0) {
        titles_list[[current_title]] <- unlist(descriptions_list)
        descriptions_list <- list()
      }
      current_title <- gsub(":", "", inner_element_text)
    } else if (inner_element_class == "dd") {
      inner_element_text <- inner_element %>%
        html_element("span") %>%
        html_text2()
      descriptions_list <- c(descriptions_list, inner_element_text)
    }
  }
  # data <- data.frame(t(titles_list))
  return(titles_list)
}

args <- commandArgs(trailingOnly = TRUE)

length(args) > 0
if (length(args) > 0) {
  "went through"
  # process_file <- grabExcelPlantNames(args[1])
  # process_file
  excel_sheets(args[1])
  xl <- read_excel(args[1])
  xl
  url <- "https://plants.ces.ncsu.edu/plants/silene-virginica/"
  # df_save <- scrap_ncsu_website(url)
  # df_save
} else {
  "Please supply an xlsx file to go through"
}