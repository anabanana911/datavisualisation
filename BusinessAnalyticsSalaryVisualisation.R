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

## Bubble Plot: 

data1$Company_Size_Group <- cut(data1$Company_Size,
breaks = c(0, 50, 200, 500, 1000, 5000, 10000, Inf),
labels = c("1-50", "51-200", "201-500", "501-1000", "1001-5000", "5000-10000", "10000+"))
ggplot(data1, aes(x = Avg_Salary, y = Sector, size = Company_Size_Group, fill = Job_Title)) +
  geom_point(shape = 21, color = "black") +
  # scale_x_log10() +
  scale_size_manual(values = c(2.5, 3.8, 4.5, 6, 7, 8, 10)) +
  scale_fill_manual(values = c(
    "#e6b0aa", "#d98880", "#cd6155", "#229954", "#2ecc71", "#5499c7", "#40b0c1", "#2e86c1", "#801a1a", "#dc7633" )) +
  labs(title = "Mapping Sector-Wise Salaries: Insights by Job Title and Company Size",
       x = "Average Salary",
       y = "Sector",
       size = "Company Size",
       fill = "Job Title") + theme_light() +
  theme(axis.text.x = element_text(hjust = 1) ,
        legend.text = element_text(size = 12), # Adjust size of legend text
        legend.title = element_text(size = 14)) + # Adjust size of legend title
  guides(fill = guide_legend(
    override.aes = list(size = 5) # Set the size of the legend bubbles for Job Title
  ))
  

## Choropleth Map: 

state_salaryx <- data1 %>%
  group_by(Job_Location) %>%
  summarise(
    Mean_Salary = mean(Avg_Salary, na.rm = TRUE),
    Salary_Count = sum(!is.na(Avg_Salary))
  )
map_data <- us_map(regions = "states")
map_data <- map_data %>%
  left_join(state_salaryx, by = c("abbr" = "Job_Location"))
summary(map_data$Mean_Salary)
map_data <- map_data %>%
  filter(!is.na(Mean_Salary))
view(map_data)
top_3_states <- map_data %>%
  arrange(desc(Salary_Count)) %>%
  slice_head(n = 3) %>%
  pull(abbr) # Get the top 3 state abbreviations
map_data <- map_data %>%
  mutate(highlight = ifelse(abbr %in% top_3_states, "Highlight", "Normal"))
state_centroids <- st_centroid(map_data)
ggplot(data1 = map_data) +
  geom_sf(aes(fill = Mean_Salary), color = "white") +
  geom_sf(data1 = map_data %>% filter(highlight == "Highlight"),
          aes(fill = Mean_Salary), color = "gold", size = 40, alpha = 40) +
  scale_fill_gradient(low = "#ffcccc", high = "#800000", na.value = "grey90") +
  geom_sf_text(data1 = state_centroids, aes(label = abbr), size = 4, color = "white") +
  labs(title = "Average Salary by State with Top 3 States Highlighted", fill = "Mean Salary") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )