# Load the required libraries
library(readxl)
library(rvest)
library(tibble)
# Define the URL of the website
base_url <- "https://plants.ces.ncsu.edu/plants/"

# function to scrape the website page and return a list attributes
# url: string of the url to be scraped
scrap_ncsu_website <- function(url) {
  webpage <- read_html(url) # using rvest we read the url given

  # we then dive further into the page getting to the main section where the
  # plant facts that we want are.
  main_content <- webpage %>%
    html_elements("main")

  # this grabs all elements with .brick
  # which is a css class where all the tables start
  brick_list <- main_content %>%
    html_elements(".brick") %>%
    html_elements("dl") # this just digs deeper into the tables

  # Initialize empty lists to store titles and descriptions
  titles_list <- list()
  descriptions_list <- list()
  current_title <- character() # initialize a place to store the current plant fact label

  child_list <- html_children(brick_list) # grabs all childrent inside the dl html element


  # Iterate through each dl element
  # seq_along just makes a list counting up to the length of child_list
  for (i in seq_along(child_list)) {
    inner_element <- child_list[i] # grab the value for entry i
    inner_element_class <- html_name(inner_element) # this grabs the html tag of the child

    # here we check if it is dt or dd
    # 'dt' is the description title
    # 'dd' is the value of that plant fact
    if (inner_element_class == "dt") {
      # we grab the description title and trim any white space
      inner_element_text <- inner_element %>%
        html_text(trim = TRUE)

      # if this is a new 'dt' and we have some 'dd' values
      # we should start a new entry and record the previou
      if (length(descriptions_list) > 0) {
        titles_list[[current_title]] <- unlist(descriptions_list)
        descriptions_list <- list()
      }
      # removes the extra ":" that a lot of the titles have
      # and stores for later user in list
      current_title <- gsub(":", "", inner_element_text)
    } else if (inner_element_class == "dd") {
      # here we will grab the plant fact value.
      # we dig in more to get past the span that a lot are using
      inner_element_text <- inner_element %>%
        html_element("span") %>%
        html_text2()

      # we then append that value to our description_list just incase there is more than one entry for this plant fact
      descriptions_list <- c(descriptions_list, inner_element_text)
    }
  }

  # when this is all done we return the list with all the entries in it for that plant
  return(titles_list)
}

# starts the main code
# commandArgs with trailingOnly set to TRUE
# gets all aguments that you set after --args when running the program
args <- commandArgs(trailingOnly = TRUE)

# process_file <- grabExcelPlantNames(args[1])
# process_file

# we grab the first args provided and read the excel sheet that is in htere
xl <- read_excel(args[1])

# we grab the second row. which has the plant name
plant_name_list <- as.list(xl[, 2])[[1]]


# make a working tibble for later use
working_tribble <- tibble(
  "Common Name" = NULL,
  "Scientific Name" = NULL,
  "Deciduous/Evergreen" = NULL,
  "Type" = NULL,
  "Size" = NULL,
  "Flower" = NULL,
  "Bloom" = NULL,
  "Sun/Shade" = NULL,
  "Soil" = NULL,
  "Type" = NULL,
  "Moist/Dry" = NULL,
  "Picture" = NULL
)

# iterate over the plant_name_list
for (i in seq_along(plant_name_list)) {
  # grab the ith entry
  pn <- plant_name_list[[i]]

  # create a url to use for web scrapping replacing spaces with -
  # expl: "https://plants.ces.ncsu.edu/plants/Yucca-filamentosa"
  url <- paste(base_url, gsub(" ", "-", pn), sep = "")

  # call function to scrap website
  df_save <- scrap_ncsu_website(url)

  # TODO grab only the values we want and export it to an excel sheet
  df_save
}
