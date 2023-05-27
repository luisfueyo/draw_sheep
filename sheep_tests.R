# 0. Preparation ---- 
library(tidyverse)

# 1. Importing the data and cleaning ----
## We import the data
df <- read_csv("https://raw.githubusercontent.com/luisfueyo/draw_sheep/main/sheep_data.csv")

## And convert all of the id's into factors
df <- df |> 
  mutate_at(vars(ends_with("_id")), ~as_factor(.))

# 2. Data wrangling ----
## We take the mean for each group for comparing between initial conditions
condition <- df |> 
  group_by(condition_id) |> 
  summarise(tail = mean(tail_presence)) |> 
  ungroup()

# 3. Statistical tests ----
## 3.1 for the difference between the conditions in the transmission chain
condition_1 <- df |> 
  filter(condition_id == 1) |> 
  select(tail_presence)

condition_2 <- df |> 
  filter(condition_id == 2) |> 
  select(tail_presence)

t.test(condition_2, condition_1, alternative = "greater")

# 4. Data visualization ---
initial_condition_graph <- condition |> 
  ggplot(aes(condition_id, tail)) +
  geom_col() +
  theme_minimal() +
  labs(x= "Condition", y="Mean tail presence")

#ggsave("initial_condition_graph.png", initial_condition_graph)
