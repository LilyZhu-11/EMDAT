library(tidyverse)

data_file <- file.choose()
disaster <- read_csv(data_file)

glimpse(disaster)
problems(disaster)

disaster <- read_csv(data_file, guess_max = Inf)
problems(disaster)

disaster_selected <- disaster %>%
  select(Entity,
         Year,
         deaths_all_disasters,
         injured_all_disasters,
         homeless_all_disasters)

glimpse(disaster_selected)

#top 10 death
top10_death <- disaster_selected %>%
group_by(Entity) %>%
summarise(
  avg_death = mean(deaths_all_disasters, rm.na = TRUE) %>%

  arrange(desc(avg_death)) %>%
slice(1:10)

#top 10 injured
top10_injured <- disaster_selected %>%
group_by(Entity) %>%
summarise(
  avg_injured = mean(injured_all_disasters, rm.na = TRUE)) %>%
arrange(desc(avg_injured)) %>%
slice(1:10)

#top 10 homeless
top10_homeless <- disaster_selected %>%
  group_by(Entity) %>%
summarise(
  avg_homeless = mean(homeless_all_disasters, rm.na = TRUE)) %>%
  arrange(desc(avg_homeless)) %>%
  slice(1:10)

#question 6
disaster <- disaster %>%
mutate(
  high_death = if_else (deaths_all_disasters > 500, 1, 0)
)
  
#question 7
disaster_long <- disaster_selected %>%
  pivot_longer(
    cols = c("deaths_all_disasters",
             "injured_all_disasters",
             "homeless_all_disasters"),
    names_to = "measure",
    values_to = "count"
  )
write_csv(disaster_long, "disaster_long.csv")