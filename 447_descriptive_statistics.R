degreesdata = read.csv("degrees-that-pay-back.csv")
collegedata = read.csv("salaries-by-college-type.csv")
regiondata = read.csv("salaries-by-region.csv")

head(degreesdata)
head(collegedata)
head(regiondata)

library(tidyverse)
library(ggpubr)
library(rstatix)

salarydata = read.csv()