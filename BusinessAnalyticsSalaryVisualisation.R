library (ggplot2) 
library (readxl)

library (tidyverse) 
library(dplyr) 
library(ggally) 
library(ggcorrplot) 
#install.packages("ggcorrplot") 
library(usmap) 
library(sf) 
library(ggalt) 
library(ggrepel)

#Error in library(ggally) : there is no package called ‘ggally’

View(data1)
str(data1)
sapply(data1, class)
any(is.na(data1))

## Box Plot: 

data1 <- data1 %>% arrange(Sector, Ownership) %>% group_by(Job_Title) %>% mutate(Avg_Salary) %>% ungroup()
ggplot(data1, aes(x = Job_Title, y = Avg_Salary, fill = Job_Title)) +
  geom_boxplot() +
  labs(
    title = "Salary Insights by Job Title: A Comparative Boxplot",
    x = "Job Title",
    y = "Average Salary",
    fill = "Job Title"
  ) +
  theme_light() +
  scale_x_discrete(labels = c(
    "Data Scientist" = "DS",
    "Business Analyst" = "BA",
    "Data Engineer" = "DE",
    "Marketing Manager" = "MM",
    "Data Analyst" = "DA",
    "Data Analytics Project Manager" = "DAPM",
    "Data Architect" = "DArc",
    "Machine Learning Engineer" = "ML"
  ))

