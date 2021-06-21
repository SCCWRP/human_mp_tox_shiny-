library(tidyverse) #General everything

## simple plots for endpoints
human <- read_csv("Humans_Clean_Final.csv", guess_max = 10000)

human %>%  
  filter(pass.all.red %in% c("Y", "N")) %>% 
  group_by(pass.all.red, lvl2) %>% 
  summarize(studies = n_unique(doi)) %>% 
  arrange(desc(studies)) %>% 
  view()

