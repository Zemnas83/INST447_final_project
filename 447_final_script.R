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
  filter(School.Type == "Ivy League") %>%
  summarise(
    Mean = mean(Starting.Median.Salary, na.rm = TRUE),
    Median = median(Starting.Median.Salary, na.rm = TRUE),
    Mode = getmode(Starting.Median.Salary)
  )

non_ivy_stats <- collegedata %>%
  filter(School.Type != "Ivy League") %>%
  summarise(
    Mean = mean(Starting.Median.Salary, na.rm = TRUE),
    Median = median(Starting.Median.Salary, na.rm = TRUE),
    Mode = getmode(Starting.Median.Salary)
  )

print(ivy_stats)
print(non_ivy_stats)

# box plot of ivy league data vs non ivy league data
collegedata$Category <- if_else(collegedata$School.Type == "Ivy League", "Ivy League", "Non-Ivy League")

ggplot(collegedata, aes(x = Category, y = Starting.Median.Salary, fill = Category)) +
  geom_boxplot() +
  labs(title = "Box Plot of Starting Median Salaries: Ivy League vs. Non-Ivy League",
       x = "Category",
       y = "Starting Median Salary",
       fill = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust text angle for better legibility
