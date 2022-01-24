## code to prepare `data_CRS` dataset goes here
data_CRS <- read.csv(file = "data-raw/Binned_data_HC_RA_SLE_CRS_n.csv")
usethis::use_data(data_CRS, overwrite = TRUE)
