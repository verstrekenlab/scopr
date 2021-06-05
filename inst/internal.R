library(data.table)
library(scopr)
library(ggetho)
library(usethis)

var_map <- read.table("inst/extdata/var_map.csv", header=TRUE,sep=",")
use_data(var_map, internal = TRUE, overwrite = TRUE)
