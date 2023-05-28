# 0. Preparation ---- 
library(tidyverse, stargazer)

# 1. Importing the data and cleaning ----
## We import the data
df <- read_csv("https://raw.githubusercontent.com/luisfueyo/draw_sheep/main/sheep_data.csv")

## And convert all of the id's into factors
df <- df |> 
  mutate_at(vars(ends_with("_id")), ~as_factor(.))

# 2. Data wrangling ----
## We take the mean for each group according to the initial condition
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

## We make the one-sided t-test
t.test(condition_2, condition_1, alternative = "greater")

## 3.2 We run a probit regression to see how demographic factors affect the drawings
sheep_probit <- glm(tail_presence ~ sex + age_group + as_factor(condition_id), 
                  family = binomial(link = "probit"), 
                  data = df)

summary(sheep_probit)

# 4. Data visualization ---
## Graph the difference between groups
initial_condition_graph <- condition |> 
  ggplot(aes(condition_id, tail)) +
  geom_col() +
  theme_minimal() +
  labs(x= "Condition", y="Mean tail presence")

## Save the plot
#ggsave("initial_condition_graph.png", initial_condition_graph)
