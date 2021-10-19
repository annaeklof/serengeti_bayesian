require(tidyverse)

path <- "/Users/annak52/Documents/Serengeti/Results/groups/A3B3/"
Extension <- ".RData"

per <- data_frame()
load(paste0(path,"control_", Extension))
colnames(persistence)[4] <- "control"
per<-persistence
plants <- c(7:14)

for (i in head(plants,1):tail(plants,1)){
  # Result approximation method
  # Load control nws (pi=0.1)
  load(paste0(path,"HigherGroup_",i, Extension))
  per <- left_join(per, persistence,by=c("code","group","sp_no"))
  colnames(per)[i-2] <- paste("higher pi group ",i)
}
  
# per %>%
#   as_data_frame %>% 
#   gather(group_higher_pi, persistence, -c(code, group, sp_no)) %>% 
#   filter(group %in% c(1:6)) %>% 
#   select(-sp_no) %>%
#   ggplot() +
#   aes(x=persistence, colour=factor(group), fill=factor(group)) +
#   geom_histogram(alpha=0.2, binwidth=0.01, position="identity") +
#   facet_wrap(~group_higher_pi) +
#   theme_bw()


per %>%
  as_data_frame %>% 
  gather(group_higher_pi, persistence, -c(code, group, sp_no)) %>% 
  filter(group %in% c(1:6)) %>% 
  ggplot() +
  aes(x=factor(group), y=persistence, color=factor(group)) +
  geom_boxplot(outlier.size=0.75) +
  facet_wrap(~group_higher_pi) +
  theme_bw()

# sp_id web_no scenario_web scenario_Pi Persistence