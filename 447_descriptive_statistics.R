degreesdata = read.csv("degrees-that-pay-back.csv")
collegedata = read.csv("salaries-by-college-type.csv")
regiondata = read.csv("salaries-by-region.csv")

head(degreesdata)
head(collegedata)
head(regiondata)

library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)


# factor the column data as numeric
regiondata$Starting.Median.Salary <- as.numeric(gsub("[\\$,]", "", regiondata$Starting.Median.Salary))
collegedata$Starting.Median.Salary <- as.numeric(gsub("[\\$,]", "", collegedata$Starting.Median.Salary))
degreesdata$Starting.Median.Salary <- as.numeric(gsub("[\\$,]", "", degreesdata$Starting.Median.Salary))
degreesdata$Mid.Career.Median.Salary <- as.numeric(gsub("[\\$,]", "", degreesdata$Mid.Career.Median.Salary))

(v <- regiondata$Starting.Median.Salary)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

region_stats <- regiondata %>%
  group_by(Region) %>%
  summarise(
    Mean = mean(Starting.Median.Salary, na.rm = TRUE),
    Median = median(Starting.Median.Salary, na.rm = TRUE),
    Mode = getmode(Starting.Median.Salary)
  )

region_stats

schooltype_stats <- collegedata %>%
  group_by(School.Type) %>%
  summarise(
    Mean = mean(Starting.Median.Salary, na.rm = TRUE),
    Median = median(Starting.Median.Salary, na.rm = TRUE),
    Mode = getmode(Starting.Median.Salary)
  )

schooltype_stats

# create a bar chart to visualize median starting salary per region
ggplot(region_stats, aes(x = Region, y = Median, fill = Region)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  labs(title = "Median Starting Salary by Region",
       x = "Region",
       y = "Median Starting Salary ($)",
       fill = "Region") +
  scale_fill_brewer(palette = "Paired") + # optional: adds color palette
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# create a bar chart to visualize median starting salary per school type
ggplot(schooltype_stats, aes(x = School.Type, y = Median, fill = School.Type)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  labs(title = "Median Starting Salary by School Type",
       x = "School Type",
       y = "Median Starting Salary ($)",
       fill = "School Type") +
  scale_fill_brewer(palette = "Paired") + # optional: adds color palette
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
# Filter the top 5 undergraduate majors based on starting median salary
top5_majors <- degreesdata %>%
  arrange(desc(Starting.Median.Salary)) %>%  
  head(5)  

# Create the bar plot for the top 5 majors with rotated x-axis labels
ggplot(top5_majors, aes(x = Undergraduate.Major, y = Mid.Career.Median.Salary - Starting.Median.Salary)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    x = "Undergraduate Major",
    y = "Difference in Median Salary (Mid-Career - Starting)",
    title = "Difference in Median Salary between Mid-Career and Starting for Top 5 Undergraduate Majors"
  ) +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
