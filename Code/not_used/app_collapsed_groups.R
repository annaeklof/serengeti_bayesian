require(XRJulia) ## package allowing for Julia interfacing
require(tidyverse) ## for efficient data manipulation and plotting
require(igraph)
library(dplyr)
save_path <- "/Users/annak52/Documents/Serengeti/Results/collapsed/groups/"
Extension <- ".RData"
source("BN-DFS.R") ## to get prey average TL

juliaUsing("Random, Distributions")

## Monte Carlo evaluation of Bayesian network, written in Julia
bayes_ntw_MC_jul <- juliaEval("
                              function bayesianNetworkMonteCarlo(A, pBaseline, a, b, reps)
                              sA = sum(A, dims = 2)
                              nRows = length(sA)
                              lastBasal = maximum(findall(sA .== minimum(sA)))[1][1]
                              marginals = zeros(nRows)
                              for rep in 1:1:reps
                              extant = ones(nRows)
                              for i in 1:1:lastBasal
                              extant[i] = rand() < pBaseline[i] ? 0 : 1
                              end
                              for i in (lastBasal + 1):1:size(A, 1)
                              frac = 1 - (extant' * A[i,:])[1] / sum(A[i,:])
                              pExt = pBaseline[i] + (1 - pBaseline[i]) * cdf(Beta(a, b), frac)
                              extant[i] = rand() < pExt ? 0 : 1
                              end
                              marginals = marginals + extant
                              end
                              return(vec(marginals) / reps)
                              end
                              ")

## turn the above Julia code to an R function
bayes_ntw_MC <- JuliaFunction(bayes_ntw_MC_jul)

as_data_frame <- dplyr::as_data_frame # overwritten above, necessary to read in file below as data_frame
links <- as_data_frame(read.table(paste("Data/serengeti_links.csv",sep=''), sep=",", header=TRUE)) %>%
    mutate_if(is.factor, as.character)

names <- c("large_carni", "sm_carni", "sm_herbi", "large_graz", "hyrax", "large_herbi",
           "gr_7", "gr_8", "gr_9", "gr_10", "gr_11", "gr_12", "gr_13", "gr_14")

groups <- read_csv(paste("Data/serengeti_groups.csv",sep=''), col_types="ci") %>%
    mutate(group=names[group])


# Connect predators to their respective group
pred_links <- links %>% 
    select(code=pred_code) %>%
    left_join(., groups, by="code") %>%
    select(pred_code=code, pred_group=group)
# Connect preys to their resp. group, and combine the resulting 
# data frame with the one for predators
links_groups <- links %>% 
    select(code=prey_code) %>%
    left_join(., groups, by="code") %>%
    select(prey_code=code, prey_group=group) %>%
    bind_cols(pred_links,.)

# below summarizes how many links a group of pred got to each group of prey,
# and also the fraction of no of links going to each prey group.
compl_list <- links_groups %>%
    mutate(pred_code = 1, prey_code=1) %>%
    mutate_if(is.double, as.integer) %>%
    group_by(pred_group, prey_group) %>%
    mutate(counts = sum(prey_code)) %>%
    ungroup() %>%
    distinct(.) %>%
    select(pred_group, prey_group, counts) %>%
    slice(., -8) %>% # manually remove cannibalistic lion...
    group_by(pred_group) %>%
    mutate(tot_counts = sum(counts)) %>%
    ungroup() %>%
    arrange(., desc(pred_group)) %>%
    mutate(frac = counts/tot_counts) 

# Create adjacency matrix from df compl_list
nodes <- 14
A <- matrix(0,nodes,nodes)
rownames(A) <- colnames(A) <- names
for (l in names[1:6]){
    prey_vec <- compl_list %>%
        filter(pred_group==l) %>%
        pull(prey_group)
    A[prey_vec,l] <- 1
}

df <- data_frame(
    group=names,
    node=1:dim(A)[1]
)

# topo sort A and make it lower triangular (for Julia)
S <- dim(A)[1]
#image(t(apply(A,2,rev)))
o <- graph_from_adjacency_matrix(A) %>% topo_sort(mode="out")
A <- A[o,o]
A <- t(A) 
#image(t(apply(A,2,rev)))

# Find the character string belonging to the prey nodes of 
# each predator, and replace the current "1" in the A-matrix
# to the fraction (the frac of links going from a pred node to
# their prey nodes)
# for (k in names[1:6]){
# A[k,compl_list %>%
#       filter(pred_group==k) %>%
#       pull(prey_group)] <- compl_list %>%
#     filter(pred_group==k) %>%
#     pull(frac)
# }

plant_groups <- c("control", 7:14)
appMarg <- function(alpha, beta) {
    if (alpha==1) funcresp="Linear"
    else funcresp="A3B3"
    
    for (i in 1:length(plant_groups)){
        dist <- plant_groups[i]
        print(dist)
        Pb <- rep(0.1, S) ## baseline extinction probabilities: same for each species
        if (dist!="control"){
        per_vec <- df %>%
            filter(node==dist) %>%
            pull(group)
        per_vec <- match(per_vec,rownames(A))
        Pb[per_vec] <- 0.5
        }
        
        persistence <- tibble(
            group=rownames(A), ## species numbers
            per = bayes_ntw_MC(A, Pb, alpha, beta, 100000) %>% ## 100k evals of BN
                juliaGet ## convert vector from Julia to R format
        )  
        persistence <- left_join(df,persistence, by="group") %>%
            mutate(group_higher_pi=dist)
        save(persistence, file=paste0(save_path,funcresp,"/HigherNode_",dist,Extension))
    }
}

alpha = 1
beta = 1
appMarg(alpha,beta)
