require(XRJulia) ## package allowing for Julia interfacing
require(tidyverse) ## for efficient data manipulation and plotting
require(igraph)
library(dplyr)
save_path <- "/Users/annak52/Documents/Serengeti/Results/original/random/"
Extension <- ".RData"
source("BN-DFS.R") ## to get prey average TL

juliaUsing("Random, Distributions")

AdjMatrix <- function(links) {
    ## Create adjacency matrix A; links point from resource to consumer, so
    ## the two columns of the edge list data frame are flipped
    A <- graph_from_data_frame(d=links[,2:1]) %>% ## create igraph graph
        as_adjacency_matrix %>% ## convert to igraph adjacency matrix
        as.matrix ## convert to a regular matrix
    ## get producers in the first rows
    A <- A[order(colSums(A)),order(colSums(A))]
    return(A)
}

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

links <- read.table(paste("Data/serengeti_links.csv",sep=''), sep=",", header=TRUE)
A <- AdjMatrix(links) 
S <- dim(A)[1]
#image(t(apply(A,2,rev)))
A <- DFS(A)
o <- graph_from_adjacency_matrix(A) %>% topo_sort(mode="out")
A <- A[o,o]
A <- t(A) # for Julia, lower triangular

# create data frame with species names (code) and give all species a number,
# note, the same number as the row names in the adjacency matrix
df <- tibble(
    code=rownames(A),
    sp_no=1:dim(A)[1]
)
# read in group belonging and traits for each species
# combine with df 
groups <- left_join(df,read_csv(paste("Data/serengeti_groups.csv",sep=''), col_types="ci"), by="code") %>%
    left_join(.,as_tibble(read.table("Data/All_traits_r.txt", header=TRUE, sep="\t")), by="code")%>%
    select(-Species) 

rand_groups <- c("S","D","T","R","missing","G","W","K")

appMarg <- function(alpha, beta, rep) {
    if (alpha==1) funcresp="/Linear"
    else funcresp="/A3B3"
    for (i in 1:length(rand_groups)){
        rand <- rand_groups[i]
        print(rand)
        if (i < 6){
            all <- tibble(read.table(paste("Data/serengeti_info.csv",sep=''), 
                                     sep=",", header=TRUE)) %>%
                select(-group) %>%
                left_join(., groups, by="code")
        } 
        if (i > 5){
            all <- tibble(read.table(paste("Data/serengeti_info_", rand, ".csv",sep=''), 
                                     sep=",", header=TRUE)) %>%
                select(-group) %>%
                left_join(., groups, by="code")
        }   
        # find number of basal species present in a specific group (Baskerville).
        # This gives how many random basal species I'll disturb later on.
        rand_dist <- all %>%
            count(habitat==rand) %>%
            pull(n)
        rand_dist <- rand_dist[2]
        for (j in 1:rep){
            if (j==1000) print(j)
            if (j==2500) print(j)
            if (j==4000) print (j)
            Pb <- rep(0.1, S) ## baseline extinction probabilities: same for each species
            # find all basal species in the network
            basal_sp <- groups %>%
                filter(group %in% c(7:14)) %>%
                pull(sp_no)
            # randomly choose a number of plant species, equal to the no of plant sp 
            # in a specific group from Baskerville 
            per_vec <- sample(basal_sp, size=rand_dist, replace=F)
            # give the randomly chosen species a higher extinction prob
            Pb[per_vec] <- 0.5
            
            persistence <- tibble(
                code=rownames(A), ## species numbers
                per = bayes_ntw_MC(A, Pb, alpha, beta, 100000) %>% ## 100k evals of BN
                    juliaGet ## convert vector from Julia to R format
            )  
            persistence <- left_join(all,persistence, by="code") %>%
                mutate(rand_size=rand, rep=j) %>%
                select(code, sp_no, group, per, rand_size, rep)
            if (j==1) per <- persistence
            else per <- bind_rows(per, persistence)
        }
        save(per, file=paste0(save_path,funcresp,"/rand_size_",rand,Extension))
    }
}

appMarg(alpha=3, beta=3, rep=5000)
