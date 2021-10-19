require(igraph) ## used for sorting webs topologically
require(tidyverse)
save_path <- "/Users/annak52/Documents/Serengeti/Results/"
Extension <- ".RData"
Result_name <- "traits/"

## Generate one single Monte Carlo realization of the Bayesian network
## Input:
## - A: adjacency matrix, sorted to be upper triangular
## - Pb: vector of baseline probabilities of extinction
## - alpha: first parameter of the Beta distribution
## - beta: second parameter of the Beta distribution
## Output:
## - a vector of persistence probabilities. The ordering of the species
##   is assumed to be the same as in the rows of A.
simulate_BN <- function(A, Pb, alpha=1, beta=1) {
    S <- nrow(A) ## Number of species
    ind_basal <- max(which(colSums(A)==0)) ## Species index of first basal species
    extant <- rep(1, S)
    for (i in 1:ind_basal) if (runif(1)<Pb[i]) extant[i] <- 0 ## Basal species 
    for (i in (ind_basal+1):S) { ## Consumer species
        frac <- 1 - as.numeric(A[,i] %*% extant / sum(A[,i])) ## Fraction lost
        Pext <- Pb[i] + (1 - Pb[i]) * pbeta(frac, alpha, beta)
        if (runif(1)<Pext) extant[i] <- 0
    }
    return(extant)
}

## Repeat the Monte Carlo simulation many times to obtain the marginal
## probabilities of persistence numerically
## Input:
## - A: adjacency matrix, sorted to be upper triangular
## - Pb: vector of baseline probabilities of extinction
## - alpha: first parameter of the Beta distribution
## - beta: second parameter of the Beta distribution
## - reps: number of times to repeat the Monte Carlo simulation; the
##   higher this number, the more accurate the final result will be
## Output:
## - a vector of persistence probabilities. The ordering of the species
##   is assumed to be the same as in the rows of A.
get_marginals <- function(A, Pb, alpha=1, beta=1, reps=1) {
    marginals <- rep(0, nrow(A))
    for (i in 1:reps) marginals <- marginals + simulate_BN(A, Pb, alpha, beta)
    return(marginals/reps)
}

AdjMatrix <- function(links) {
    ## Create adjacency matrix A; links point from resource to consumer, so
    ## the two columns of the edge list data frame are flipped
    A <- graph_from_data_frame(d=links[,2:1]) %>% ## create igraph graph
        as_adjacency_matrix %>% ## convert to igraph adjacency matrix
        as.matrix ## convert to a regular matrix
    ## get producers in the first rows
    A <- A[order(rowSums(A)),order(rowSums(A))]
    return(A)
}


plants <- c(7:8)
AppMarg <- function(){
    for (i in head(plants,1):tail(plants,1)){
        FW <-"serengeti"
        links <- read.table(paste("Data/", FW,"_links.csv",sep=''), sep=",", header=TRUE)
        A <- AdjMatrix(links)
        
        sp_A <- rownames(A)
        
        trait_tab <- read.table("Data/All_traits_r.txt", header=TRUE, sep="\t") %>%
            mutate(sp_no=1:dim(A)[1])
        
        per_vec <- trait_tab%>%
            filter(sp_no, growth_form=="graminoid") %>%
            pull(sp_no)
        
        S <- dim(A)[1]
        rownames(A) <- colnames(A) <- 1:S ## name rows/columns of A with species labels
        o <- topo_sort(graph_from_adjacency_matrix(A), mode="out") ## sort topologically
        A <- A[o,o] ## return matrix with that sorting
        per_vec <- match(per_vec,rownames(A))
        ## Assign baseline probabilities and simulate the Bayesian network
        Pb <- rep(0.1, S) ## baseline extinction probabilities: same for each species
        Pb[per_vec] <- 0.5
        
        persistence <- data_frame( ## compile data frame with:
            sp_no=as.integer(rownames(A)), ## species numbers
            persist=get_marginals(A, Pb, alpha=3, beta=3, reps=100000) ## persistence probs.
        ) %>% arrange(sp_no)
        persistence <- left_join(trait_tab,persistence, by="sp_no")  
        save(persistence, file=paste0(save_path,Result_name,"Higher_pi_gram",Extension)) 
        #save(persistence, file=paste0(save_path,Result_name,"control_",Extension))  
    }
}
persistence <- AppMarg()
#plot(persistence$sp_no, persistence$persist, ylim=c(0, 1)) ## plot results
