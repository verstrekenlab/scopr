library(data.table)
library(scopr)
library(ggetho)
library(usethis)

var_map <- read.table("inst/extdata/var_map.csv", header=TRUE,sep=",")

metadata <- data.table(region_id = 1:20,
                       machine_name = c("E_014"),
                       date = c("2016-01-25"),
                       test=c(1)
)

use_data(var_map, metadata, internal = TRUE, overwrite = TRUE)
