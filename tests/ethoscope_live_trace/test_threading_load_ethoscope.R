library(fslbehavr)
library(fslscopr)
library(fslsleepr)
library(data.table)
library(fslggetho)
library(rethogui)
library(ggplot2)

results_dir <- '/media/antortjim/data/antortjim/ethoscope_results'
metadata <- data.table(machine_name = 'ETHOSCOPE_013', date = '2020-02-03', region_id = 1:20)
metadata <- link_ethoscope_metadata(x = metadata, result_dir = results_dir)

metadata[, datetime := NULL]
metadata[, machine_name := NULL]
metadata


system.time(
dt_raw <- load_ethoscope(metadata = metadata, verbose = T, rds_interface = F)
)
system.time(
  dt_raw <- load_ethoscope(metadata = metadata, verbose = T, rds_interface = T)
)

as.character(unique(dt_raw$id))

dt_raw[xmv(region_id) == 5]

df <- readRDS("/media/antortjim/data/antortjim/ethoscope_results/013aad42625f433eb4bd2b44f811738e/ETHOSCOPE_013/2020-02-03_15-01-15/2020-02-03_15-01-15_013aad42625f433eb4bd2b44f811738e_ROI_5.rds"); nrow(df)
