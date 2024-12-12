# Trout Stability # 

# supplementary information from Sauve et al. 2016. Ecology. How plants connect...

# Functions to calculate stability and to build binary and quantitative jacobian matrices

# Refs:
# Sauve et al. Ecology 
# Neutel et al. 2002 Science, 2007 Nature
# Allesina and Tang 2012 Nature
# Staniczenko et al. 2013 Nature Communications

## stability () ##
# stability() returns the value stab necessary for the system to be stable.
# stab is thus such that the real part of the greatest eigenvalue of J-stab*I = 0

stability <- function(J, s2){
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
  
  test1 <- (dim(J)[1] == dim(J)[2]) # Is J a square matrix?
  test2 <- FALSE
  if (test1 == TRUE){
    S <- dim(J)[1]
    test2 <- which(diag(J) != vector("numeric", S)) # Does J have a null diagonal?
  }
  
  if ((test1 == TRUE) & (length(test2) == 0)){ # if J is a square matrix with a null diagonal
    S <- dim(J)[1] # S is the number of species in the network
    s1 <- 0
    I <- diag(S)
    E1 <- max(Re(eigen(J-s1*I, only.values = T)$values))
    E2 <- max(Re(eigen(J-s2*I, only.values = T)$values))
    if ((E1>=0) & (E2<0)){ # if s2 is well chosen and the system is not already stable
      while ((s2-s1)>=10^-4){
        stab <-(s1+s2)/2
        E1 <-max(Re(eigen(J-stab*I, only.values = T)$values))
        if (E1>=0){
          s1<-stab
        }
        else {
          s2<-stab
        }
      }
      return(stab)
    }
    
    if (E1<0){
      # stop("J corresponds to a stable system.")
      stab <- 0
      return(stab)
    }
    if (E2>=0){
      # stop("s2 is not high enough.")
      stab <- NA
      return(stab)
    }
  }
  else { # if J is not a square matrix with a null diagonal
    if (test1 == FALSE){
      #stop("J is not a square matrix.")
      stab <- "J is not square"
      return(stab)
    }
    if (length(test2) > 0){
      #stop("J does not have a null diagonal.")
      stab <- "diag not null"
      return(stab)
    }
  }
}

jacobian_quant <- function(m, ab){
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
  # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
  # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
  # S being the total number of species
  # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
  
  test1 <- (dim(m)[1] == dim(m)[2]) # Is m a square matrix?
  test2 <- (dim(m)[1] == length(ab)) # Has ab the right length (S)?
  
  if ((test1 == TRUE) & (test2 == TRUE)){ # If m is a square matrix and ab has the right length.
    S <- dim(m)[1]
    J <- m
    J[which(J < t(J))] <- -t(J)[which(J < t(J))]
    
    nonzero_index <- which(J != 0, arr.ind = TRUE)
    c<-0
    for (i in 1:dim(nonzero_index)[1]){
      J[nonzero_index[i, 1], nonzero_index[i, 2]] <-
        J[nonzero_index[i, 1], nonzero_index[i, 2]]/
        ab[nonzero_index[i, 2]]
      c<-c+1
    }
    
    return(J)
  }
  if (test1 == FALSE){
    stop("m is not a square matrix.")
  }
  if (test2 == FALSE){
    stop("ab doesn't have the right length.")
  }
}

jacobian_binary <- function(m){
  # m is the adjacency matrix of the graph, dim(m) = S*S
  # undirected interactions (or bidirectional) are such that m_ij = m_ji = 1
  # directected interactions (such as antagonistic ones) are such that m_ij = 1 and m_ji = 0 if i feeds on j (j -> i)
  # S being the total number of species
  
  if (dim(m)[1] == dim(m)[2]){ # Is m a square matrix?
    J <- t(m)
    # Modified from original function
    # transpose matrix so negative interactions are effects 
    # of predators in colums on prey in rows and 
    # positive interactions are effects of prey in rows
    # on predator in column
    J[which(J < t(J))] <- -t(J)[which(J < t(J))]
    
    L <- sum(abs(J))/2
    strength <- rnorm(L, sd = 0.1) + 1 # L values for interaction strength magnitude drawn from a normal distribution
    upper_tri_str_index <- which((upper.tri(J) == TRUE) & (J != 0), arr.ind = TRUE) # non-zero elements in the upper triangle matrix
    
    lower_tri_str_index <- cbind(upper_tri_str_index[, 2], upper_tri_str_index[, 1]) # so indices match between upper triangle and lower triangle elements
    J[upper_tri_str_index] <- J[upper_tri_str_index]*strength
    
    strength <- rnorm(L, sd = 0.1) + 1
    J[lower_tri_str_index] <- J[lower_tri_str_index]*strength
    return(J)
  }
  else {
    stop("m is not a square matrix.")
  }  
}


# These are useful functions for running the analyses presented in Pomeranz et al. 2020 Ecology

# bernoulli trials
# this function takes a square, probability matrix, and returns a binary adjacency matrix of the same dimensions
b_trial <- function (prob_matr){
  sum.out = 0
  # make sure that all matrices have at least one link
  while(sum.out == 0){
    out <- matrix(rbinom(length(prob_matr),
                         1,
                         prob = prob_matr),
                  ncol = ncol(prob_matr),
                  nrow = nrow(prob_matr))
    sum.out = sum(out[upper.tri(out)])
  }
  out
}

# function to calculate relative abundance matrices
# this function takes a vector of abundance data and a character vector of taxa names. It returns a square matrix with the cross product of the relative abundances of taxa i,j. 
get_rel_ab <- function(vec, taxa){
  # make sure vectors are the same lenght
  stopifnot(length(vec) == length(taxa)) 
  # make a vector of relative abundances
  rel.ab <- vec / sum(vec) 
  # make an empty square matrix
  Nij <- matrix(0,
                length(vec),
                length(vec))
  # nested for loops to populate matrix i,j, with cross products of relative abundances of taxa i, and j, respectively.  
  for (i in 1:length(vec)){
    for (j in 1:length(vec)){
      Nij[i,j] <- rel.ab[i]*rel.ab[j]
    }
  }
  # add row and columns names from taxa vector
  dimnames(Nij) <- list(taxa, taxa)
  Nij
}

# function to scale continuous variable, x, to [min, max]
scalexy <- function(x,
                    min,
                    max){
  ((max - min) / (max(x) - min(x))) *
    (x - min(x)) + min
}

# function to set probabilities of forbidden taxa to 0
# matrix must have taxa for column names 
# f.taxa is a character vector of taxa names
rm_niche <- function(matrix,
                     f.taxa){
  for(name in 
      (colnames(matrix)[colnames(matrix) %in% f.taxa])){
    matrix[,name] <- 0
  }
  matrix
}

# function to get fw_measures and dominant eigenvalue
# requires functions from stability_fxns_sauve.R
# returns the stability metric, as well as standard food web measures
get_measures <- function(matr, # probability matrix
                         s2, # initial value, see stability() function from Sauve
                         trials = 1, # number of bernoulli trials to run
                         scale.Jij = FALSE, # scale interactions based on body size?
                         correlate.Jij = FALSE, # correlate positive and negative interactions?
                         correlate.value = 0.7){
  result <- list()
  for(i in 1:trials){
    A <- rm_cycle(b_trial(matr))
    J <- jacobian_binary(A)
    if(scale.Jij == TRUE){
      J = scale.Jij(J)
    }
    if(correlate.Jij == TRUE){
      J = correlate.Jij(J,
                        correlate.value = correlate.value)
    }
    stab = stability(J, s2 = s2)
    fw_meas <- Get.web.stats(A)
    result[[i]] <- c(stab = stab, fw_meas) 
  }
  return(result)
}

# function to scale interaction strengths by body size
# used internally in get_measures()
scale.Jij <- function(J){
  # J is a Jacobian matrix where elements Jij = interaction strength
  # e.g. object from jacobian_binary()
  element.rank <- rank(1:nrow(J)) / nrow(J)
  scale.rank <- scalexy(element.rank,
                        min = 0.25,
                        max = 1.25)
  scale.grid <- expand.grid(rev(scale.rank),
                            scale.rank)
  scale.vector <- scale.grid$Var1 * scale.grid$Var2
  scale.matrix <- matrix(scale.vector,
                         nrow = nrow(J),
                         ncol = ncol(J))
  scale.J <- scale.matrix * J
  return(scale.J)
}

# function to correlate positive and negative interactions by body size
# used internally in get_measures()
correlate.Jij <- function(J,
                          correlate.value = 0.7){
  # J is a JAcobian matrix where elements Jij = interaction strength
  # e.g. object from jacobian_binary()
  negative.index <- which(J < 0,
                          arr.ind = TRUE)
  positive.index <- cbind(negative.index[,2],
                          negative.index[,1])
  positive.strength <- abs(J[negative.index] * correlate.value)
    #runif(n = nrow(negative.index), min = 0.6, max = 0.8))
  # 0.7 comes from Montoya et al. 2009
  J[positive.index] <- positive.strength
  return(J)
}

# used internally in get_measures()
rm_cycle <- function(A, diag = TRUE){
  # if A eats B and B eats A, randomly remove one link
  # Default removes diagonal
  # to leave diagonal as is set diag = FALSE
  for(i in 1:nrow(A)){
  for(j in 1:ncol(A)){
    if(diag == FALSE){
      if(i == j){
        next
      }
    }
    if(A[i,j] == 1 & A[j,i] == 1){
      if(runif(1) < 0.5){
        A[i,j] <-  0
      }
      else{
        A[j,i] <- 0
      }
    }
  }
}
  return(A)
}

# food web functions modified from Owen Petchey
# original functions were found on thetrophiclink.org
# and are available on github here: https://github.com/opetchey/dumping_ground/blob/master/random_cascade_niche/FoodWebFunctions.r
Get.web.stats <- function(web, which.stats=1:4, real.web=NA){
  if(!is.list(web)){
    result <- c(S = length(web[,1]),
                L = sum(web),
                C = sum(web)/length(web[,1])^2)
    
    if(sum(which.stats==1)==1){
      BITUC <- Bottom.Intermediate.Top(web, proportion=T)$Proportions.of.each
      result <- c(result,
                  B = BITUC[1],
                  I = BITUC[2],
                  T = BITUC[3]
                  )
    }
  }
  
  if(is.list(web)){
    for(i in 1:length(web)){
      
      t.result <- c(S = length(web[[i]][,1]),
                    L = sum(web[[i]]),
                    C = sum(web[[i]])/length(web[[i]][,1])^2)
      
      if(sum(which.stats==1)==1){
        BITUC <- Bottom.Intermediate.Top(web[[i]], proportion=T)$Proportions.of.each
        t.result <- c(t.result,
                      B = BITUC[1],
                      I = BITUC[2],
                      T = BITUC[3]
                      )
      }
      
      if(i==1)
        result <- t.result
      if(i>1)
        result <- rbind(result, t.result)
    }
  }    
  result
}

# Cannibals <- function(web){
#   length(dimnames(web)[[1]][diag(web)==1]) / length(web[1,])
# }
## return the proportion of bottom, intermediate, top,
## unconnected and purely cannibalistic species
Bottom.Intermediate.Top <- function(web, proportion=TRUE, check.web=FALSE){
  
  ## find the names and numbers of BIT, unconnected and pure cannibals
  names.all <- 1:length(web[1,])
  dimnames(web) <- list(names.all, names.all)
  names.Bottom <-  names.all[apply(web, 2, sum)==0 & apply(web, 1, sum)!=0]#pulls out names where column sum ==0, and where row sum == >0. e.g. an alga does not consume anyhting (column sum == 0) but is consumed (row sum = >0)
  number.Bottom <- length(names.Bottom)
  names.Top <-   names.all[apply(web, 2, sum)!=0 & apply(web, 1, sum)==0]
  number.Top <- length(names.Top)
  names.Unconnected <- names.all[apply(web, 2, sum)==0 & apply(web, 1, sum)==0]
  number.Unconnected <- length(names.Unconnected)
  number.Pure.cannibals <- 0
  names.Pure.cannibals <- character(0)
  for(i in 1:length(web[,1]))
    if(web[i,i]!=0 && sum(web[-i,i]+web[i,-i])==0){
      if(number.Pure.cannibals==0){
        names.Pure.cannibals <- dimnames(web)[[1]][i]
        number.Pure.cannibals <- 1
      }
      else{
        names.Pure.cannibals <- c(names.Pure.cannibals, dimnames(web)[[1]][i])
        number.Pure.cannibals <- number.Pure.cannibals + 1
      }
    }
  names.Intermediate <- dimnames(web)[[1]][is.na(match(dimnames(web)[[1]],
                                                       c(names.Bottom, names.Top, names.Unconnected, names.Pure.cannibals)))]
  number.Intermediate <- length(web[1,])-number.Bottom-number.Top-number.Unconnected-number.Pure.cannibals
  
  if(proportion)
    result <- list(Bottom=names.Bottom,
                   Intermediate=names.Intermediate,
                   Top=names.Top,
                   Unconnected=names.Unconnected,
                   Pure.cannibals=names.Pure.cannibals,
                   Proportions.of.each=c(number.Bottom, number.Intermediate, number.Top, number.Unconnected, number.Pure.cannibals)/length(web[1,]))
  if(!proportion)
    result <- list(Bottom=names.Bottom,
                   Intermediate=names.Intermediate,
                   Top=names.Top,
                   Unconnected=names.Unconnected,
                   Pure.cannibals=names.Pure.cannibals,
                   Proportions.of.each=c(number.Bottom, number.Intermediate, number.Top, number.Unconnected, number.Pure.cannibals))
  result
}

# Actual stability metric calculation # ============================
library(dplyr)
library(GenSA)
library(remotes)
library(here)
library(tidyverse)

# # #SDMTools no longer on CRAN - must install archived version as below
# require("devtools")
# # install.packages("remotes")
# install.packages("https://cran.r-project.org/src/contrib/Archive/SDMTools/SDMTools_1.1-221.2.tar.gz", repo=NULL, type="source", dependencies = T)
# remotes::install_version("SDMTools", "1.1-221.2")
library(SDMTools)

# 
# #install "traitmatch" package - https://github.com/ibartomeus/traitmatch - WILL NOT INSTALL UNLESS SDMTools is installed first
remotes::install_github("ibartomeus/traitmatch", dependencies = T)
library(traitmatch)

setwd(here('functions'))
source('predict.niche.prob.R')
source('stability_fxns_Sauve.R')
source('Example_functions.R')

setwd('C:/Users/tjbut/Box Sync/Butts_Scripts/Trout/trout-flux')

trout.fw = read_csv('trout fw.csv')
trout.fw

community_data = trout.fw %>% 
  select(year, spp, bodymass_g, biomass.g_perhec) %>%
  group_by(year) %>% 
  mutate(rel.ab = biomass.g_perhec/sum(biomass.g_perhec)) %>%
  ungroup() %>%
  mutate(log10.dw = log10(bodymass_g)) %>%
  filter(log10.dw != '-Inf')
community_data

# Test out with 2001 first # 
fw01 = community_data %>% 
  filter(year == 2001)
fw01

# Interaction probability - NICHE -----------------------------------

# Make matrix with all possible pairwise combinations of taxa dryweights # 
M = expand.grid(fw01$log10.dw, 
                fw01$log10.dw)
M

# Get traitmatch model paramters # ----------------------
# Need to build dataset so each row represents an interaction 
  # Lake Trout -> cisco 

int1 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'lake.trout' |
           spp == 'cisco') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(lake.trout, cisco) %>% # order predator - prey 
  rename(predator = lake.trout, 
         prey = cisco)
int1

  # Lake Trout -> pmiv 

int2.1 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'lake.trout' |
           spp == 'chaoborus.larvae') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(lake.trout, chaoborus.larvae) %>% # order predator - prey 
  rename(predator = lake.trout, 
         prey = chaoborus.larvae)

int2.2 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'lake.trout' |
           spp == 'leptodora') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(lake.trout,leptodora) %>% # order predator - prey 
  rename(predator = lake.trout, 
         prey = leptodora)

int2.3 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'lake.trout' |
           spp == 'mysis') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(lake.trout,mysis) %>% # order predator - prey 
  rename(predator = lake.trout, 
         prey = mysis)

  # Walleye -> cisco 

int3 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'walleye' |
           spp == 'cisco') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(walleye, cisco) %>% # order predator - prey 
  rename(predator = walleye, 
         prey = cisco)

  # Cisco -> pmiv 
int4.1 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'cisco' |
           spp == 'chaoborus.larvae') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(cisco, chaoborus.larvae) %>% # order predator - prey 
  rename(predator = cisco, 
         prey = chaoborus.larvae)

int4.2 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'cisco' |
           spp == 'leptodora') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(cisco, leptodora) %>% # order predator - prey 
  rename(predator = cisco, 
         prey = leptodora)

int4.3 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'cisco' |
           spp == 'mysis') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(cisco, mysis) %>% # order predator - prey 
  rename(predator = cisco, 
         prey = mysis)

int4.4 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'cisco' |
           spp == 'bythotrephes') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(cisco, bythotrephes) %>% # order predator - prey 
  rename(predator = cisco, 
         prey = bythotrephes)

  # cisco -> zoops 
int5.1 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'cisco' |
           spp == 'cladocera') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(cisco, cladocera) %>% # order predator - prey 
  rename(predator = cisco, 
         prey = cladocera)

int5.2 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'cisco' |
           spp == 'copepoda') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(cisco, copepoda) %>% # order predator - prey 
  rename(predator = cisco, 
         prey = copepoda)

int5.3 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'cisco' |
           spp == 'rotifera') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(cisco, rotifera) %>% # order predator - prey 
  rename(predator = cisco, 
         prey = rotifera)

  # pmiv -> zoops 
int6.1 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'chaoborus.larvae' |
           spp == 'cladocera') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(chaoborus.larvae, cladocera) %>% # order predator - prey 
  rename(predator = chaoborus.larvae, 
         prey = cladocera)

int6.2 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'chaoborus.larvae' |
           spp == 'copepoda') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(chaoborus.larvae, copepoda) %>% # order predator - prey 
  rename(predator = chaoborus.larvae, 
         prey = copepoda)

int6.3 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'leptodora' |
           spp == 'cladocera') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(leptodora, cladocera) %>% # order predator - prey 
  rename(predator = leptodora, 
         prey = cladocera)

int6.4 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'leptodora' |
           spp == 'rotifera') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(leptodora, rotifera) %>% # order predator - prey 
  rename(predator = leptodora, 
         prey = rotifera)

int6.5 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'mysis' |
           spp == 'cladocera') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(mysis, cladocera) %>% # order predator - prey 
  rename(predator = mysis, 
         prey = cladocera)

int6.6 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'mysis' |
           spp == 'copepoda') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(mysis, copepoda) %>% # order predator - prey 
  rename(predator = mysis, 
         prey = copepoda)

int6.7 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'mysis' |
           spp == 'rotifera') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(mysis, rotifera) %>% # order predator - prey 
  rename(predator = mysis, 
         prey = rotifera)

int6.8 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'bythotrephes' |
           spp == 'cladocera') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(bythotrephes, cladocera) %>% # order predator - prey 
  rename(predator = bythotrephes, 
         prey = cladocera)

int6.9 = community_data %>% 
  select(year, spp, bodymass_g) %>%
  filter(spp == 'bythotrephes' |
           spp == 'copepoda') %>% 
    pivot_wider(id_cols = 'year', 
                names_from = 'spp', 
                values_from = 'bodymass_g') %>% 
  drop_na() %>% # Remove where there's not a predator-prey interaction 
  select(bythotrephes, copepoda) %>% # order predator - prey 
  rename(predator = bythotrephes, 
         prey = copepoda)

# Combine into one data table of predator prey with each row being an interaction # 
interactions = rbind(int1, int2.1, int2.2, int2.3, int3, int4.1, int4.2,
                     int4.3, int4.4, int5.1, int5.2, int5.3, int6.1,
                     int6.2, int6.3, int6.4, int6.5, int6.6, int6.7,
                     int6.8, int6.9)
interactions

# multiply by 10 to avoid problems with the integral of the normal distribution # 
  # Define the vectors for the predator and prey size 
MPred = log10(interactions$predator*10)
MPrey = log10(interactions$prey*10)

# Fit integrated model that integrates neutral and nich constraints. Use pairwise prey-predator interaction and assume the 
  # distribution of preys is well characterized by this network
mt <- 60 # Define max.time to 60s to run things fast 
pars_pre = fit_it(integrated_model, 
                  Tlevel1 = MPrey, 
                  Tlevel2 = MPred, 
                  mean_Tlevel1 = mean(MPrey), 
                  sd_Tlevel1 = sd(MPrey), 
                  pars = c(a0 = 0, a1 = 0, b0 = 0, b1 = 0),
                   par_lo = c(a0 = -10, a1 = 0, b0 = -10, b1 = -10),
                   par_hi = c(a0 = 10, a1 = 10, b0 = 10, b1 = 10),
                   max.time = mt)

# look at predicted model # 
plot_pred(pars = pars_pre, Tlevel1 = MPrey, 
          Tlevel2 = MPred, xlab = 'log (Predator body size)',
          ylab = 'log (Prey body size)', pch = '.')

pars_pre_niche <- fit_it(niche_model,
                         Tlevel1 = MPrey, 
                         Tlevel2 = MPred,
                         mean_Tlevel1 = mean(MPrey),
                         sd_Tlevel1 = sd(MPrey),
                         pars = c(a0 = 0, a1 = 0, b0 = 0, b1 = 0),
                         par_lo = c(a0 = -10, a1 = 0, b0 = -10, b1 = -10),
                         par_hi = c(a0 = 10, a1 = 10, b0 = 10, b1 = 10),
                         max.time = mt)

plot_pred(pars = pars_pre_niche, Tlevel1 = MPrey, 
          Tlevel2 = MPred, xlab = "log (Predator body size)", ylab = "log (prey body size)",
           pch = ".")

# Compute likelihoods
lh_model <- -integrated_model(pars_pre, MPrey, MPred, mean(MPrey),sd(MPrey))
lh_niche <- -niche_model(pars_pre_niche, MPrey, MPred, mean(MPrey), sd(MPrey)) # Best model fit # 
lh_neutral <- -neutral_model(pars = NULL, MPrey, MPred, mean(MPrey), sd(MPrey))

# Visualization
barplot(c(lh_model, lh_niche, lh_neutral), names.arg = c("integrated", "niche", "neutral"))

# model parameters # 
params = pars_pre_niche
params

# estimate probability of interaction based on body sizes
# this uses the traitmatch package, see Bartomeus et al. 2016 for more details. 
link.probs = predict.niche.prob(pars = params, 
                                M[[1]], 
                                M[[2]], 
                                replicates = 1)[[1]]
link.probs

prob.matr <- matrix(link.probs, 
                    nrow = sqrt(length(link.probs)), 
                    ncol = sqrt(length(link.probs)))
prob.matr

# Relative abundance - NEUTRAL EFFECTS ------------------------------------

# See the "get_rel_ab()" function in "MS_functions.R" for more details
# this function takes a vector of abundance data and a character vector of taxa names. 
# It returns a square matrix with the cross product of the relative abundances of taxa i,j. 

rel.ab.matr <- get_rel_ab(vec = fw01$rel.ab,
                          taxa = fw01$spp)


# rescale the relative abundances in this matrix to be from 0.5 to 1
# See the "scalexy()" function in "MS_functions.R" for more details
rel.ab.matr <- scalexy(rel.ab.matr, min = 0.5, max = 1)

# Final probability matrices ----------------------------------------------

# 1) prune niche forbidden links e.g. Pomeranz et al. 2019 Methods Eco Evo
# 2) estimate link probability by multiplying NICHE probability (link.probs) and NEUTRAL probability (rel.ab.matr)
   # Put column names on constructed probability matrix # 
    colnames(prob.matr) <- fw01$spp
prob.matr

# 1) prune niche "forbidden" taxa based on morphology
# No predation on anything in the food web here 
taxa.forbid <- c("cladocera", 'copepoda', 'rotifera')

# set column values to 0 for forbidden taxa
# see rm_niche() function in MS_functions.R
prob.matr.pruned <- rm_niche(prob.matr, taxa.forbid)
prob.matr.pruned



# 2) NEUTRAL EFFECTS
# "weighting" interaction probabilities based on relative abundances
# e.g. more abundant pairs are more likely to interact
prob.matr.neutral <- prob.matr.pruned * rel.ab.matr

# Start here with the Flux matrix (reflects relative abundance and interaction probability; scale it to be 0.01 - 0.99) #=========================
mtest = as.matrix(flux01)
mtest

# rescale probabilities to be between 0.01 and 0.99
prob.matr.final <- scalexy(mtest,
                             min = 0.01, 
                             max = 0.99)
prob.matr.final



# Adjacency matrices ------------------------------------------------------

# The b_trial() function in Example_functions.R takes a square, probability matrix, and returns a binary, adjacency matrix based on those probabilities

b_trial(prob.matr.final)

# you can save these iterations as objects, and then get standard food web measures from them. 
A1 <- b_trial(prob.matr.final)
Get.web.stats(A1)


# the function allows you to "scale" interaction strengths based on relative body size
# i.e. argument "scaled.Jij = TRUE"
# It can also correlate positive and negative interaction strengths  
# i.e. "correlate.Jij = TRUE"
# default "correlate.value" = 0.7, i.e. positive interaction = 0.7 * negative interaction, but you can change this setting. 
# You can also set both scaled and correlate to TRUE to examine effects of both

# You can control the number of replications with the "trials" argument. 

random.J <- get_measures(prob.matr.final,
                         s2 = 2,
                         trials = 1,
                         scale.Jij = T,
                         correlate.Jij = FALSE)

random.J

# the output is a list of length = trials
# each element in the list contains the following information:
# stab = stability metric from Sauve et al. 2016 (lower values are "more" stable)
# S = number of species (taxa) in web, these should be identical for each run per site
# L = number of links in that iteration - this will change for each iteration due to the random nature of bernoulli trials
# C = connectance = L / S^2
# B = number of basal taxa (i.e., prey, not predators)
# I = number of intermediate taxa (i.e., both prey and predators)
# T = number of top taxa (i.e., predators, but not prey)
random.J
