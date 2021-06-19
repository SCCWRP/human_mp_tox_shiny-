## Data download for annotating apical endpoints
library(tidyverse) #General everything
library(reshape2)
library(ggsci)
human <- read_csv("Humans_Clean_Final.csv", guess_max = 10000)

human %>%  
  filter(pass.all.red == "Y",
         invitro.invivo == "invivo") %>%  
  select(c(doi, 
           authors, 
           year,
           invitro.invivo,
           genus,
           species,
           life.stage,
           sex,
           exposure.route,
           exposure.duration.d,
           sample.size,
           replicates,
           dose.mg.kg.day.bw.nominal,
           dose.mg.mL.master,
           effect,
           lvl1,
           lvl2,
           lvl3,
           endpoint.reliability,
           polymer)) %>% 
  view()

# make table with endpoint reliability
reliability.summary <- human %>% 
  filter(pass.all.red == "Y",
         invitro.invivo == "invivo",
         effect == "Y") %>%  
 # mutate("Study" = paste0(authors, year, " (", doi, ")")) %>% 
  group_by(doi, authors, year, lvl1, lvl2, lvl3, human.relevance) %>% 
  summarize(endpoint.reliability = min(endpoint.reliability))

write.csv(reliability.summary, "reliability_summary.csv")


# make table summarize number of reliable studies for each endpoint
human %>% 
  filter(pass.all.red == "Y",
         invitro.invivo == "invivo",
         effect == "Y",
         endpoint.reliability == "2",
         human.relevance == "relevant") %>%  
  group_by(lvl1, lvl2, lvl3) %>% 
  summarize(n_studies = n_distinct(doi)) %>% 
  mutate(endpoint = factor(fct_reorder(lvl3, n_studies))) %>% 
  ggplot(aes(x = n_studies, y = endpoint, fill = lvl2)) +
  geom_col() +
  xlab("Number of Studies Reporting Effect") +
  labs(title = "Summary of Reliable Endpoints with Relevance to Human Health",
       subtitle = "Studies passing red criteria (n = 12); endpoints explicitly deemed 'reliable' by experts",
       caption = "(Only effects with statistical significant shown)") +
  scale_fill_jco(name = "Endpoint Category") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5))
  
  
