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
    mutate(dist_rand=habitat_higher_pi) %>%
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
    mutate(per_rand=per) %>%
    select(-per)
#group_by(code, group, dist_rand) %>%
#summarise(per_rand = mean(per)) %>%
#ungroup() %>%
#arrange(dist_rand) %>%
#mutate(dist_rand=as.character(dist_rand))


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
    select(-group) %>%
    rename(dist_rand = habitat_higher_pi) 

df_hab <- tab_rand %>% left_join(., tab_hab, by=c("code", "dist_rand"))

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
    mutate(per_rand=per) %>%
    select(-per)

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
    select(-group) %>%
    rename(dist_rand = habitat_higher_pi) 

df_pg <- tab_rand %>% left_join(., tab_pg, by=c("code", "dist_rand"))

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
    mutate(per_rand=per) %>%
    select(-per) %>%
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
tab_gr <- tab_d %>%
    filter(group %in% c(1:6)) %>%
    select(code, habitat_higher_pi, group, per) %>%
    arrange(code) %>%
    rename(per_dist = per) %>%
    select(-group) %>%
    rename(dist_rand = habitat_higher_pi) 


df_group <- tab_rand %>% left_join(., tab_gr, by=c("code", "dist_rand"))

########## JUST SHORT FIG CODE TO DO FIG 1 BUT WITH RANDOM VALUES INSTEAD OF OBSERVED
# box_df <- df_all_rand %>%
#     group_by(code, dist_rand) %>%
#     summarise(rand_mean = mean(per_rand)) %>%
#     ungroup()
# 
# # Load control data set
# load(paste0(path,funcresp,"/HigherHab_control", ".RData"))
# control <- persistence %>%
#     filter(group %in% c(1:6)) %>%
#     mutate(dist_rand=habitat_higher_pi, rand_mean=per) %>%
#     select(code, dist_rand, rand_mean) 
# 
# gruppa <- grupper%>%
#     rename(dist_rand=habitat_higher_pi)
#     
# left_join(box_df, control, by="code") %>%
#     rename(dist_rand=dist_rand.x, rand_mean=rand_mean.x, dist_c=dist_rand.y, per_c=rand_mean.y) %>%
#     mutate(per_diff=(rand_mean-per_c)) %>%
#     left_join(., gruppa, by="dist_rand") %>%
#     mutate(dist_rand = case_when(
#         dist_rand=="shrub" ~ "shrubs",
#         dist_rand== "tree" ~ "trees",
#         dist_rand=="herb" ~ "herbs",
#         dist_rand=="graminoid" ~ "grasses",
#         dist_rand=="annual" ~ "annuals",
#         dist_rand=="7" ~ "group 7",
#         dist_rand=="8" ~ "group 8",
#         dist_rand=="9" ~ "group 9",
#         dist_rand=="10" ~ "group 10",
#         dist_rand=="11" ~ "group 11",
#         dist_rand=="12" ~ "group 12",
#         dist_rand=="13" ~ "group 13",
#         dist_rand=="14" ~ "group 14",
#         dist_rand=="D" ~ "disturbed",
#         dist_rand=="T" ~ "thicket",
#         dist_rand=="S" ~ "shrubland",
#         dist_rand=="R" ~ "riverland",
#         dist_rand=="missing" ~ "missing",
#         dist_rand=="W" ~ "wetland",
#         dist_rand=="K" ~ "kopje",
#         dist_rand=="G" ~ "grassland",
#         TRUE ~ dist_rand
#     )) %>%
#     mutate(dist_rand = fct_relevel(dist_rand, "disturbed",
#                                            "grassland", "kopje", "missing",
#                                            "riverland", "shrubland", "thicket", "wetland",
#                                            "group 7", "group 8", "group 9","group 10",
#                                            "group 11","group 12","group 13","group 14",
#                                            "trees", "grasses","herbs","annuals", "shrubs"
#     )) %>%
#     ggplot(aes(x=dist_rand, y=per_diff, color=tot_group)) +
#     geom_boxplot(width=0.5,lwd=0.6) +
#     xlab("Disturbed plant group") + ylab("Persistence impact") +
#     sc +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5, size = 12),
#           axis.text.y = element_text(size = 12),
#           axis.title.x = element_text(size = 13),
#           axis.title.y = element_text(size = 13)) 
# ggsave(paste0("../Results/Figures/", funcresp, "/random/", "rand_dist_all", ".pdf"), width = 8, height = 6)

##############


df_all_rand <- df_group %>%
    add_row(., df_pg) %>%
    add_row(., df_hab) %>% # persistence from rand sim (per_rand) of all consumers in each rep for each dist_scen 
                           # obs per (per_dist)
    mutate(count = ifelse(per_rand<per_dist,1,0)) %>% #if rand per is smaller than obs per = 1, otherwise 0
    group_by(code, dist_rand) %>% # for each consumer sp and each dist scenario,
    summarise(p=mean(count)) %>%  # get the mean number of counts. If rand always is smaller than obs = 1
    ungroup()

df <- df_all_rand %>%
    #mutate(point_five = ifelse(p>0.95|p<0.05,1,0)) %>% 
    mutate(rnd_more = ifelse(p>0.975,1,0)) %>% 
    mutate(obs_more = ifelse(p<0.025,1,0)) %>% 
    group_by(dist_rand) %>%
    summarise(prop_rnd_more=sum(rnd_more==1)/length(unique(code)), prop_obs_more=sum(obs_more==1)/length(unique(code))) %>%
    ungroup() %>%
    pivot_longer(!dist_rand, names_to = "type", values_to = "value") %>%
    left_join(., grupper, by="dist_rand")

df %>%
    mutate(dist_rand = case_when(
        dist_rand=="shrub" ~ "shrubs",
        dist_rand== "tree" ~ "trees",
        dist_rand=="herb" ~ "herbs",
        dist_rand=="graminoid" ~ "grasses",
        dist_rand=="annual" ~ "annuals",
        dist_rand=="7" ~ "group 7",
        dist_rand=="8" ~ "group 8",
        dist_rand=="9" ~ "group 9",
        dist_rand=="10" ~ "group 10",
        dist_rand=="11" ~ "group 11",
        dist_rand=="12" ~ "group 12",
        dist_rand=="13" ~ "group 13",
        dist_rand=="14" ~ "group 14",
        dist_rand=="D" ~ "disturbed",
        dist_rand=="T" ~ "thicket",
        dist_rand=="S" ~ "shrubland",
        dist_rand=="R" ~ "riverland",
        dist_rand=="missing" ~ "missing",
        dist_rand=="W" ~ "wetland",
        dist_rand=="K" ~ "kopje",
        dist_rand=="G" ~ "grassland",
        TRUE ~ dist_rand
    )) %>%
    mutate(dist_rand = fct_relevel(dist_rand, "disturbed",
                                   "grassland", "kopje", "missing",
                                   "riverland", "shrubland", "thicket", "wetland",
                                   "group 7", "group 8", "group 9","group 10",
                                   "group 11","group 12","group 13","group 14",
                                   "trees", "grasses","herbs","annuals", "shrubs"
    )) %>%
    mutate(dist_rand = fct_reorder(dist_rand, tot_group)) %>%
    ggplot(aes(x=dist_rand, y=value)) +
    #geom_col(aes(fill = type, alpha=tot_group))  +
    geom_col(aes(fill = type))  +
    xlab("Corresponding group label") + ylab("Fraction of extreme p-values") +
    scale_fill_manual(name="Extremes", values=c("cadetblue3", "coral4"), labels=c("p > 0.975", "p < 0.025")) +
    #scale_alpha_continuous(guide=FALSE, range=c(0.4,1)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13)) 
ggsave(paste0("../Results/Figures/", funcresp, "/random/", "p_val_extremes_herbi", ".pdf"), width = 8, height = 6)

