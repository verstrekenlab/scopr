library(data.table)
library(fslscopr)
library(fslggetho)
library(usethis)

source("data-raw/set_attrs.R")
if(!interactive()) {
  use_data(results_folders, internal = TRUE, overwrite = TRUE)
}

