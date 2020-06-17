require(tidyverse)

path <- "/Users/annak52/Documents/Serengeti/Results/traits/"
Extension <- ".RData"

per <- data_frame()
load(paste0(path,"Higher_pi_control", Extension))
colnames(persistence)[7] <- "control"
per<-persistence

forms <- c("W","NW","P","A","forb","shrub","tree", "annual", "gram")

for (i in 1:length(forms)){
    # Result approximation method
    # Load control nws (pi=0.1)
    form <- forms[i]
    
    load(paste0(path,"Higher_pi_",form, Extension))
    per <- left_join(per, persistence,by=c("code","Species","type","cycle","growth_form","sp_no"))
    colnames(per)[i+7] <- paste("higher pi",form)
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


persistence1 %>%
    filter(group %in% c(1:6)) %>% 
    ggplot() +
    aes(x=factor(group), y=persist, color=factor(group)) +
    geom_boxplot(outlier.size=0.75) +
    facet_wrap(~habitat_higher_pi) +
    theme_bw()

# sp_id web_no scenario_web scenario_Pi Persistence