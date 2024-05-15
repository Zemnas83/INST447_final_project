library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)

collegedata = read.csv("salaries-by-college-type.csv")

# descriptive statistics
head(collegedata)

collegedata$Starting.Median.Salary <- as.numeric(gsub("\\$|,", "", collegedata$Starting.Median.Salary))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Filter data for "Ivy" school type and calculate the mean, median, and mode
ivy_stats <- collegedata %>%
  filter(School.Type == "Ivy") %>%
  summarise(
    Mean = mean(Starting.Median.Salary, na.rm = TRUE),
    Median = median(Starting.Median.Salary, na.rm = TRUE),
    Mode = getmode(Starting.Median.Salary)
  )
print(ivy_stats)

