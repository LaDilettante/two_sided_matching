rm(list = ls())
library(data.table)


d <- read.table("~/Desktop/IO_VN/IO_VNM_2012_BasicPrice.tab", sep = "")

d <- as.matrix(fread("~/Desktop/IO_VN/IO_VNM_2012_BasicPrice.tab"))
