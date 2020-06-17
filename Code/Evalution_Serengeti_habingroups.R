require(tidyverse)

path <- "/Users/annak52/Documents/Serengeti/Results/habingroups/A3B3/"
Extension <- ".RData"

habitats <- c("control","R","G","W","K", "missing")
groups <- c(9,11,13)

for (i in 1:length(groups)){
  gr <- groups[i]
  if (gr==9) habitats <- c("control","G","W","missing")
  if (gr==11) habitats <- c("R","G","missing")
  if (gr==13) habitats <- c("G","W","K", "missing")
  
  for (j in 1:length(habitats)){
    # Result approximation method
    hab <- habitats[j]
    load(paste0(path,"HigherGroup_", gr, "hab", hab, Extension))
    persistence <- mutate(persistence, higher=paste(hab,gr))
    if (hab=="control"){
      persistence1 <- persistence
    } else{ 
      persistence1 <- bind_rows(persistence1,persistence)
    }
  }
}

persistence1 %>%
  select(-code, -sp_no) %>%
  filter(group %in% c(1:6)) %>% 
  ggplot() +
  aes(x=factor(group), y=persist, color=factor(group)) +
  geom_boxplot(outlier.size=0.75) +
  facet_wrap(~higher) +
  theme_bw()

# sp_id web_no scenario_web scenario_Pi Persistence