########### COMPARISON RANDOM SIMS VS DISTURBANCE SCEN ###############
## HABITATS
# Load distributions from the sim. with random disturbances
# Choose functional response
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"

# Disturbed plant sp based on:
charac <- c("S","D","T","R","missing","G","W","K", "tree", "shrub", "herb", 
            "graminoid", "annual", 7:14)

# RANDOM SIMULATIONS
# Collect all values from all scenarios and reps into single df tab
for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,"random/",funcresp,"/rand_size_", ch, ".RData"))
    if (ch %in% c(7:14)) per <- per %>% mutate(rand_size = as.character(group_size)) 
    per <- per %>% select(code, group, per, rand_size, rep)
    if (ch=="S"){
        tab <- per
    } else{ 
        tab <- bind_rows(tab,per) 
    }
}

# Calculate the mean probability to survive for each species over
# all replicates, result is one value per species
tab_rand <- tab %>%
    filter(group %in% c(1:6)) %>%
    group_by(code, group, rand_size) %>%
    summarise(per_rand = mean(per)) %>%
    ungroup() %>%
    arrange(rand_size)


# DISTURBANCE SIMULATIONS (no randomness)
# Collect all values from all scenarios into a single df tab_d

for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,funcresp,"/Higher_", ch, ".RData"))
    if (ch=="S"){
        tab_d <- persistence
    } else{ 
        tab_d <- bind_rows(tab_d,persistence)
    }
}
# modify tab_d (df of disturbance sims) and add results from
# tab_rand.
df <- tab_d %>%
    filter(group %in% c(1:6)) %>%
    select(code, habitat_higher_pi, group, per) %>%
    arrange(code) %>%
    arrange(habitat_higher_pi) %>%
    rename(per_dist = per) %>%
    rename(dist_scen = habitat_higher_pi) %>%
    mutate(tab_rand) %>%
    mutate(per_diff = (per_rand - per_dist)) %>%
    select(code, dist_scen, group, per_diff) %>%
    filter(per_diff > 0) %>%
    arrange(dist_scen, per_diff) %>%
    group_by(dist_scen) %>%
    mutate(rank=rank(-per_diff)) %>%
    ungroup() 

df_rank <- df %>%
    # filter(dist_scen %in% c("missing","G","W","K", "shrub", "herb", 
    #                         "graminoid", "9", "13")) %>%
    group_by(code) %>%
    mutate(rank_sum = sum(rank), mean_diff = mean(per_diff)) %>%
    ungroup() %>%
    #filter(dist_scen=="12")
    select(code, group, rank_sum, mean_diff) %>%
    distinct() %>%
    arrange(rank_sum) %>%
    mutate(type=if_else(group%in%c(1,2),"carnivores","herbivores")) #%>%
    #mutate(prop_rank=(rank_sum/672))
    
    
df_rank %>%
    mutate(code = fct_reorder(code, as.integer(as_factor(code)))) %>%
    ggplot() +
    aes(x=code, y=rank_sum, color=mean_diff, shape=factor(type)) +
    geom_point() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size=7)) +
    scale_colour_gradientn(name="Mean per. difference", colours = rainbow(3)) +
    scale_shape_manual(name="", values=c(17,19)) +
    xlab("Consumer species") + ylab("Rank value") 
ggsave(paste0("../Results/Figures/", funcresp, "/random/rank_pos.jpg"), width = 8, height = 5)

    

# df %>% ggplot() +
#     aes(x=code, y=per_diff, color=factor(dist_scen)) +
#     #aes(x=code, y=per_diff) +
#     geom_point() +
#     #facet_wrap(~dist_scen) +
#     theme(axis.text.x = element_text(angle = 90, size=5)) +
#     scale_color_manual(values=carto_pal(name="Safe")) 

