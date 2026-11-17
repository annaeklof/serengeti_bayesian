# A version of Shannon diversity index for niche breath over a number of prey groups

library(dplyr)
library(tidyverse)
library(rcartocolor)
library(ggplot2)
library(ggrepel)
library("RColorBrewer")

shannonNorm <- function(p_vec){
  H <-   -sum(p_vec*log(p_vec))
  return(H)
}

# construct file from multiple sources
traits <- tibble(read.csv("Data/All_traits_r.csv", sep=";")) #Sid_g
habitat <- tibble(read.csv("Data/serengeti_info_habs.csv", sep=","))
dist_factors <- left_join(traits, habitat, by = "code")
links  <- tibble(read.csv("Data/serengeti_links_habs.csv", sep=";"))

df <- links %>%
  rename(code = prey_code) %>%
  left_join(dist_factors,links, by = "code") %>%
  #filter(!habitat %in% c("large_carni", "small_carni", "small_herbi", "large_grazer", "hyrax", "mega_grazer")) %>%
  select(-"AccSpeciesName") %>%
  rename(prey = code) %>%
  rename(predator = pred_code) %>%
  mutate(link = 1)

# use this for testing
#dat <- tibble(read.csv("test_herb_nichewidth_0.csv", sep=",")) # test data

# use this for real data
divisions <- c("group", "habitat", "IUCN.growth.form")

for(d in divisions){
dat <-  df %>%
  select(predator, prey, link, paste(d)) %>%
  rename(group = paste(d)) # "group" here is the grouping analyzed 


max_links_per_pred <- length(unique(dat$prey))

 predator_links <- dat %>%
  group_by(predator) %>%
  mutate(links_for_pred = sum(link)) %>%
   select(predator, links_for_pred) %>%
    distinct()

max_links_per_group <- dat %>%
  select(c(prey, group)) %>%
  distinct() %>%
  count(group)

num_groups <- length(unique(dat$group))

# some indexes for diversity of interactions 
data <- dat %>%
  group_by(predator, group) %>%
  summarise(links_per_group = sum(link)) %>%
  merge(max_links_per_group, by="group") %>%
  rename(max_links_for_group = n) %>%
  mutate(norm = (links_per_group/max_links_for_group)) %>%
  group_by(predator) %>%
  mutate(new = sum(norm)) %>%
  mutate(fin = new/num_groups)%>%
  merge(predator_links, by="predator") %>%
  mutate(weighted_div_links = (fin*links_for_pred)/max_links_per_pred)


# "pure" Shannon diversity index
data2 <- dat %>%
  group_by(predator, group) %>%
  summarise(links_per_group = sum(link)) %>%
  mutate(norm_links_per_group = ((links_per_group/max_links_per_pred)))  %>%
  summarise(H_shannon = shannonNorm(norm_links_per_group)) 

all <- data %>%
  merge(data2, by = "predator")

num_groups_for_pred <- dat %>%
  group_by(predator) %>%
  summarize(num_group = length(unique(group)))

results <- left_join(all, num_groups_for_pred, by = "predator")

results2 <- results %>%
            group_by(predator) %>%
            mutate(prop_links_relaized = sum(links_per_group)/sum(max_links_per_group$n))%>%
            mutate(prop_groups_pred_int = num_group/num_groups) %>%
            mutate(weight_by_links_and_groups = prop_links_relaized*prop_groups_pred_int) %>%
            mutate(H = log(weight_by_links_and_groups * fin))

write.csv(results2, file = paste("linkDiversityIndex_",d, ".csv", sep=""))
}


# Description of what the table entities are:
# results$fin <- H
# results$H <- log of H weighted by number of links the consumer has and number of groups the consumer interact with


results_IUNN.growt.form  <- tibble(read.csv("linkDiversityIndex_IUCN.growth.form.csv"))
results_IUNN.growt.form <- results_IUNN.growt.form %>%
  select(-X) %>%
  mutate(dist_scenario = "traits")

results_groups  <- tibble(read.csv("linkDiversityIndex_group.csv")) 
results_groups <- results_groups %>%
  mutate(group = as.character(group)) %>%
  select(-X) %>%
  mutate(dist_scenario = "groups")

results_habitat  <- tibble(read.csv("linkDiversityIndex_habitat.csv")) 
results_habitat  <- results_habitat %>%
  select(-X) %>%
  mutate(dist_scenario = "habitat")

ranking <- tibble(read.csv("ranking_species.csv"))
ranking <- ranking %>%
  rename(predator = code) %>%
  select(c(predator, rank_sum, type))

diversity_all <- results_IUNN.growt.form %>%
  bind_rows(results_groups, results_habitat) %>%
  inner_join(ranking, by= "predator") %>%
  distinct()
  
  # plotting groups and links
    # links_for_pred is the number of links each consumer has
    # num_group is the number of groups repective predation interacts with
  
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  sc <- scale_colour_gradientn(colours = myPalette(10), limits=c(0.6,1), name="Normalized rank")
  
 diversity_all %>%
    filter(dist_scenario == "traits") %>%
    select(predator, rank_sum, links_for_pred, num_group, type) %>%
    distinct() %>%
    ggplot() +
    aes(x=num_group, y=links_for_pred, color= rank_sum/max(rank_sum), shape=factor(type),size=1) + 
    # highest value of rank_sum is the species that survives best. Here normalized so the "best
    geom_point(position = "jitter") +
    geom_text(aes(label=ifelse(rank_sum<1.2,as.character(predator),'')),hjust=0,vjust=0, size=3) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 0, size=12)) +
    theme(axis.text.y = element_text(angle = 0, size=12)) +
    sc +
   #geom_smooth(method='lm', formula= y~x, size = 0.5, colour = "black") +
    xlab("Number of links") + 
   ylab("Number of groups") 
   ggsave("L_G_traits.png")
  

# temp plotting
plot( x= (results$links_for_pred), y= results2$H_shannon)
plot( x= (results$num_group), y= results2$H_shannon)
plot( x= (results$links_for_pred), y= results2$fin)
plot( x= (results$num_group), y= results2$fin)
plot( x= (results$links_for_pred), y= results2$H)
plot( x= (results$num_group), y= results2$H)


plot( x= (results2$fin), y= results2$H_shannon)
plot( x= (results2$H), y= results2$H_shannon)

