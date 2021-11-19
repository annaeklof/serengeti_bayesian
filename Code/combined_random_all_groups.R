########### COMPARISON RANDOM SIMS VS DISTURBANCE SCEN ###############

charac <- c("control","S","D","T","R","missing","G","W","K", 7:14,
            "tree", "shrub", "herb", "graminoid", "annual")
dist <- c("S","D","T","R","missing","G","W","K", 7:14,
          "tree", "shrub", "herb", "graminoid", "annual")

habs <- tibble(read.table(paste("Data/serengeti_info_habs.csv",sep=''), sep=",", header=TRUE)) %>%
    select(code, habitat)
habitat <- habs%>%
    mutate(count=1) %>%
    group_by(habitat)%>%
    mutate(tot_group = sum(count)) %>%
    ungroup %>%
    mutate(habitat_higher_pi = habitat) %>%
    select(habitat_higher_pi, tot_group) %>%
    distinct()

traits <- as_tibble(read.table("Data/All_traits_r.txt", header=TRUE, sep="\t")) %>%
    select(code, growth_form) 
pg <- traits %>%
    mutate(count=1) %>%
    group_by(growth_form)%>%
    mutate(tot_group = sum(count)) %>%
    ungroup %>%
    mutate(habitat_higher_pi = growth_form) %>%
    select(habitat_higher_pi, tot_group) %>%
    distinct()

groups <- read_csv(paste("Data/serengeti_groups.csv",sep=''), col_types="ci") 
grupper <-  groups %>%
    mutate(count=1) %>%
    group_by(group)%>%
    mutate(tot_group = sum(count)) %>%
    ungroup %>%
    mutate(habitat_higher_pi = as.character(group)) %>%
    select(habitat_higher_pi, tot_group) %>%
    distinct() %>%
    add_row(., pg) %>%
    add_row(., habitat) %>%
    filter(habitat_higher_pi %in% dist) %>%
    mutate(dist_scen=habitat_higher_pi) %>%
    select(-habitat_higher_pi)
           
           

## HABITATS
# Load distributions from the sim. with random disturbances
# Choose functional response
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"


# Disturbed plant sp based on:
charac <- c("S","D","T","R","missing","G","W","K")
#charac <- c("G","W","K")

# RANDOM SIMULATIONS
# Collect all values from all scenarios and reps into single df tab
for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,"random/",funcresp,"/rand_size_", ch, ".RData"))
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
    rename(dist_rand=rand_size) %>%
    select(code, group, per, dist_rand, rep) %>%
    group_by(code, group, dist_rand) %>%
    summarise(per_rand = mean(per)) %>%
    ungroup() %>%
    arrange(dist_rand) %>%
    mutate(dist_rand=as.character(dist_rand))


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
tab_hab <- tab_d %>%
    filter(group %in% c(1:6)) %>%
    select(code, habitat_higher_pi, group, per) %>%
    arrange(code) %>%
    arrange(habitat_higher_pi) %>%
    rename(per_dist = per) %>%
    rename(dist_scen = habitat_higher_pi) %>%
    mutate(tab_rand) %>%
    mutate(per_diff = (per_rand - per_dist))

## TRAITS
# Load distributions from the sim. with random disturbances
# Choose functional response
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"

# Disturbed plant sp based on:
charac <- c("tree", "shrub", "herb", "graminoid", "annual")
#charac <- c("shrub", "herb", "graminoid")

# RANDOM SIMULATIONS
# Collect all values from all scenarios and reps into single df tab
for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,"random/",funcresp,"/rand_size_", ch, ".RData"))
    if (ch=="tree"){
        tab <- per
    } else{ 
        tab <- bind_rows(tab,per)
    }
}

# Calculate the mean probability to survive for each species over
# all replicates, result is one value per species
tab_rand <- tab %>%
    filter(group %in% c(1:6)) %>%
    rename(dist_rand=rand_size) %>%
    select(code, group, per, dist_rand, rep) %>%
    group_by(code, dist_rand, group) %>%
    summarise(per_rand = mean(per)) %>%
    ungroup() %>%
    arrange(dist_rand) %>%
    mutate(dist_rand=as.character(dist_rand))


# DISTURBANCE SIMULATIONS (no randomness)
# Collect all values from all scenarios into a single df tab_d

for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,funcresp,"/Higher_", ch, ".RData"))
    if (ch=="tree"){
        tab_d <- persistence
    } else{ 
        tab_d <- bind_rows(tab_d,persistence)
    }
}
# modify tab_d (df of disturbance sims) and add results from
# tab_rand.
tab_pg <- tab_d %>%
    filter(group %in% c(1:6)) %>%
    select(code, habitat_higher_pi, group, per) %>%
    arrange(code) %>%
    arrange(habitat_higher_pi) %>%
    rename(per_dist = per) %>%
    rename(dist_scen = habitat_higher_pi) %>%
    mutate(tab_rand) %>%
    mutate(per_diff = (per_rand - per_dist))  # mean rand (all reps) - val



## GROUPS
# Load distributions from the sim. with random disturbances
# Choose functional response
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"

# Disturbed plant sp based on:
charac <- c(7:14) # plant groups 7-14
#charac <- c(9, 11, 13)
# RANDOM SIMULATIONS
# Collect all values from all scenarios and reps into single df tab
for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,"random/",funcresp,"/rand_size_", ch, ".RData"))
    if (ch==7){
        tab <- per
    } else{ 
        tab <- bind_rows(tab,per)
    }
}

# Calculate the mean probability to survive for each species over
# all replicates, result is one value per species
tab_rand <- tab %>%
    filter(group %in% c(1:6)) %>%
    rename(dist_rand=group_size) %>%
    select(code, group, per, dist_rand, rep) %>%
    group_by(code, dist_rand, group) %>%
    summarise(per_rand = mean(per)) %>%
    ungroup() %>%
    mutate(dist_rand=as.character(dist_rand))


# DISTURBANCE SIMULATIONS (no randomness)
# Collect all values from all scenarios into a single df tab_d

for (i in 1:length(charac)){
    # Result approximation method
    ch <- charac[i]
    load(paste0(path,funcresp,"/Higher_", ch, ".RData"))
    if (ch==7){
        tab_d <- persistence
    } else{ 
        tab_d <- bind_rows(tab_d,persistence)
    }
}


# modify tab_d (df of disturbance sims) and add results from
# tab_rand
# tab_tot: difference in persistence (per_diff) between observed value (per_dist)
# and mean value from 5000 random sims (per_rand) for each consumer (code) in each
# disturbance scen (dist_scen) or the corresponding no of randomly dist sp (dist_rand)
tab_tot <- tab_d %>%
    filter(group %in% c(1:6)) %>%
    select(code, habitat_higher_pi, group, per) %>%
    arrange(code) %>%
    rename(per_dist = per) %>%
    rename(dist_scen = habitat_higher_pi) %>%
    mutate(tab_rand) %>%
    mutate(per_diff = (per_rand - per_dist)) %>%
    add_row(., tab_pg) %>%
    add_row(., tab_hab) 
    select(dist_scen, per_diff) %>%
    left_join(., grupper, by="dist_scen")

tab_tot %>% 
    mutate(dist_scen = case_when(
        dist_scen=="shrub" ~ "shrubs",
        dist_scen== "tree" ~ "trees",
        dist_scen=="herb" ~ "herbs",
        dist_scen=="graminoid" ~ "grasses",
        dist_scen=="annual" ~ "annuals",
        dist_scen=="7" ~ "group 7",
        dist_scen=="8" ~ "group 8",
        dist_scen=="9" ~ "group 9",
        dist_scen=="10" ~ "group 10",
        dist_scen=="11" ~ "group 11",
        dist_scen=="12" ~ "group 12",
        dist_scen=="13" ~ "group 13",
        dist_scen=="14" ~ "group 14",
        dist_scen=="D" ~ "disturbed",
        dist_scen=="T" ~ "thicket",
        dist_scen=="S" ~ "shrubland",
        dist_scen=="R" ~ "riverland",
        dist_scen=="missing" ~ "missing",
        dist_scen=="W" ~ "wetland",
        dist_scen=="K" ~ "kopje",
        dist_scen=="G" ~ "grassland",
        TRUE ~ dist_scen
    )) %>%
    mutate(dist_scen = fct_relevel(dist_scen, "disturbed",
                                           "grassland", "kopje", "missing",
                                           "riverland", "shrubland", "thicket", "wetland",
                                           "shrubs",
                                           "group 7", "group 8", "group 9","group 10",
                                           "group 11","group 12","group 13","group 14",
                                           "trees", "grasses","herbs","annuals"
    )) %>%
    ggplot(aes(x=dist_scen, y=per_diff, color=tot_group)) +
    geom_boxplot(width=0.5,lwd=0.7) +
    xlab("Plant group") + ylab("Persistence difference") +
    geom_hline(yintercept = 0, lty= "dashed", color="black", size=0.5) +
    sc +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13)) 
ggsave(paste0("../Results/Figures/", funcresp, "/compl_rand.jpg"), width = 8, height = 5)

