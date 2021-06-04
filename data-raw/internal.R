library(data.table)
library(fslscopr)
library(fslggetho)
library(usethis)

source("data-raw/set_attrs.R")
var_map <- read.table("data-raw/var_map.csv", header=TRUE,sep=",")

if(!interactive()) {
  use_data(results_folders, var_map, internal = TRUE, overwrite = TRUE)
}

