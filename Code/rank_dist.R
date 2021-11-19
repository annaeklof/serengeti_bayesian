require(tidyverse)
library(rcartocolor)
#funcresp = "Linear"
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"
charac <- c("control","S","D","T","R","missing","G","W","K", 7:14,
            "tree", "shrub", "herb", "graminoid", "annual")

charac_h <- c("control","S","D","T","R","missing","G","W","K")

charac_g <- c("control", 7:14)

charac_t <- c("control","tree", "shrub", "herb", "graminoid", "annual")

for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,funcresp,"/Higher_", ch, ".RData"))
    if (ch=="control"){
        tab <- persistence
    } else{ 
        tab <- bind_rows(tab,persistence) %>%
            filter(group %in% c(1:6)) %>%
            select(code, per, habitat_higher_pi, group)
    }
}


control <- tab %>%
    filter(habitat_higher_pi=="control") %>%
    rename(control=habitat_higher_pi, per_c = per)

dist <- c("S","D","T","R","missing","G","W","K", 7:14,
          "tree", "shrub", "herb", "graminoid", "annual")
for (i in 1:length(dist)) {
    d <- dist[i]
    df_sep <- tab %>%
        filter(habitat_higher_pi==d) %>%
        left_join(.,control, by=c("code", "group")) %>%
        mutate(per_diff = per - per_c)
    
    if (d=="S") {
        df <- df_sep
    } else {
        df <- bind_rows(df, df_sep)
    }
}

df_rank <- df %>%
    arrange(code) %>%
    arrange(habitat_higher_pi) %>%
    rename(dist_scen = habitat_higher_pi) %>%
    group_by(dist_scen) %>%
    mutate(mean_dist = mean(per_diff)) %>%
    ungroup() %>%
    filter(mean_dist < -0.01) %>%
    group_by(dist_scen) %>%
    mutate(rank=rank(per_diff)) %>% # lowest per rank=1
    ungroup() %>%
    group_by(code) %>%
    mutate(rank_sum = sum(rank), mean_diff = mean(per_diff)) %>%
    ungroup() %>%
    filter(code=="GIRCAM")
    select(code, group, rank_sum, mean_diff, mean_dist) %>%
    distinct() %>%
    arrange(rank_sum) %>%
    mutate(type=if_else(group%in%c(1,2),"carnivores","herbivores"))

ranking_sub <- df_rank %>%
    select(code, rank_sum, type) %>%
    distinct() %>%
    mutate(pred_code=code) %>%
    select(pred_code, rank_sum, type)



df_rank %>%
    mutate(code = fct_reorder(code, as.integer(as_factor(code)))) %>%
    ggplot() +
    aes(x=code, y=rank_sum, color= mean_diff, shape=factor(type)) +
    geom_point() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size=7)) +
    scale_colour_gradientn(name="Mean per. difference", colours = rainbow(3)) +
    scale_shape_manual(name="", values=c(17,19)) +
    xlab("Consumer species") + ylab("Rank value") 
ggsave(paste0("../Results/Figures/", funcresp, "/rank_dist_limit01.jpg"), width = 8, height = 5)

