## code to prepare `DATASET` dataset goes here
MYDATA <- read.csv("data-raw/MYDATA.csv") # Read in csv file
names(MYDATA) <- c("name", "length_km", "county", "municipality", "coordinates") # clean up column names
usethis::use_data(MYDATA, overwrite = TRUE) # save as package dataset!
