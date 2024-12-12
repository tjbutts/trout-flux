# Pomeranz/Suave Stability Method #

# Custom Functions to Load in # ============================
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
                        min = -1.25,
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
    #runif(n = nrow(negative.index), min = -0.01.6, max = 0.8))
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

# Run alternate stability trials # ================ 

# Need to run flux_trout script to get flux data set 
  # flux01 - flux22

set.seed(55)

## 2001 #====================
flux01 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux01 = as.matrix(flux01) # make sure it's in the correct format columns are predators and rows are prey 
m.flux01

  #Scale fluxes to be between 0.01 and 0100 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
  # If whole row is 0 then eigenvalues all are 0 so need to do the scaling here # 
m.flux01.scaled = scalexy(m.flux01,
                             min = -0.01,
                             max = 0.99)

diag(m.flux01.scaled[,1:ncol(m.flux01.scaled)]) <- 0
m.flux01.scaled
  
  # need vector of species biomasses 
biom01

  # calculation quantitative jacobian matrix 
J.01 = jacobian_quant(m.flux01.scaled, biom01)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
      # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.01

J.01

s = (stability(J.01, s2 = 2)) # make this number a bit more manageable 
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s01 = s %>% 
  mutate(year =2001)
s01

## 2002 #====================
flux02 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux02 = as.matrix(flux02) # make sure it's in the correct format columns are predators and rows are prey 
m.flux02

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux02.scaled = scalexy(m.flux02,
                             min = -0.01,
                             max = 0.99)

diag(m.flux02.scaled[,1:ncol(m.flux02.scaled)]) <- 0
  
  # need vector of species biomasses 
biom02

  # calculation quantitative jacobian matrix 
J.02 = jacobian_quant(m.flux02.scaled, biom02)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.02


s = (stability(J.02, s2 = 2)) # make this number a bit more manageable 
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s02 = s %>% 
  mutate(year =2002)
s02

## 2003 #====================
flux03 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux03 = as.matrix(flux03) # make sure it's in the correct format columns are predators and rows are prey 
m.flux03

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux03.scaled = scalexy(m.flux03,
                             min = -0.01,
                             max = 0.99)
diag(m.flux03.scaled[,1:ncol(m.flux03.scaled)]) <- 0
  
  # need vector of species biomasses 
biom03

  # calculation quantitative jacobian matrix 
J.03 = jacobian_quant(m.flux03.scaled, biom03)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.03


s = (stability(J.03, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s03 = s %>% 
  mutate(year =2003)
s03

## 2004 #====================
flux04 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux04 = as.matrix(flux04) # make sure it's in the correct format columns are predators and rows are prey 
m.flux04

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux04.scaled = scalexy(m.flux04,
                             min = -0.01,
                             max = 0.99)
diag(m.flux04.scaled[,1:ncol(m.flux04.scaled)]) <- 0
  
  # need vector of species biomasses 
biom04

  # calculation quantitative jacobian matrix 
J.04 = jacobian_quant(m.flux04.scaled, biom04)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.04


s = (stability(J.04, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s04 = s %>% 
  mutate(year =2004)
s04

## 2005 #====================
flux05 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux05 = as.matrix(flux05) # make sure it's in the correct format columns are predators and rows are prey 
m.flux05

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux05.scaled = scalexy(m.flux05,
                             min = -0.01,
                             max = 0.99)
diag(m.flux05.scaled[,1:ncol(m.flux05.scaled)]) <- 0
  
  # need vector of species biomasses 
biom05

  # calculation quantitative jacobian matrix 
J.05 = jacobian_quant(m.flux05.scaled, biom05)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.05


s5 = (stability(J.05, s2 = 2)) # already stable...woah 
s5
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s5 = tibble(s5)
s05 = s %>% 
  mutate(year =2005)
s05

## 2006 #====================
flux06 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux06 = as.matrix(flux06) # make sure it's in the correct format columns are predators and rows are prey 
m.flux06

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux06.scaled = scalexy(m.flux06,
                             min = -0.01,
                             max = 0.99)
diag(m.flux06.scaled[,1:ncol(m.flux06.scaled)]) <- 0
  
  # need vector of species biomasses 
biom06

  # calculation quantitative jacobian matrix 
J.06 = jacobian_quant(m.flux06.scaled, biom06)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.06


s = (stability(J.06, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s06 = s %>% 
  mutate(year =2006)
s06

## 2007 #====================
flux07 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux07 = as.matrix(flux07) # make sure it's in the correct format columns are predators and rows are prey 
m.flux07

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux07.scaled = scalexy(m.flux07,
                             min = -0.01,
                             max = 0.99)
diag(m.flux07.scaled[,1:ncol(m.flux07.scaled)]) <- 0
  
  # need vector of species biomasses 
biom07

  # calculation quantitative jacobian matrix 
J.07 = jacobian_quant(m.flux07.scaled, biom07)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.07


s = (stability(J.07, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s07 = s %>% 
  mutate(year =2007)
s07

## 2008 #====================
flux08 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux08 = as.matrix(flux08) # make sure it's in the correct format columns are predators and rows are prey 
m.flux08

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux08.scaled = scalexy(m.flux08,
                             min = -0.01,
                             max = 0.99)
diag(m.flux08.scaled[,1:ncol(m.flux08.scaled)]) <- 0
  
  # need vector of species biomasses 
biom08

  # calculation quantitative jacobian matrix 
J.08 = jacobian_quant(m.flux08.scaled, biom08)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.08


s = (stability(J.08, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s08 = s %>% 
  mutate(year =2008)
s08

## 2009 #====================
flux09 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux09 = as.matrix(flux09) # make sure it's in the correct format columns are predators and rows are prey 
m.flux09

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux09.scaled = scalexy(m.flux09,
                             min = -0.01,
                             max = 0.99)
diag(m.flux09.scaled[,1:ncol(m.flux09.scaled)]) <- 0
  
  # need vector of species biomasses 
biom09

  # calculation quantitative jacobian matrix 
J.09 = jacobian_quant(m.flux09.scaled, biom09)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.09


s = (stability(J.09, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s09 = s %>% 
  mutate(year =2009)
s09

## 2010 #====================
flux10 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux10 = as.matrix(flux10) # make sure it's in the correct format columns are predators and rows are prey 
m.flux10

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux10.scaled = scalexy(m.flux10,
                             min = -0.01,
                             max = 0.99)
diag(m.flux10.scaled[,1:ncol(m.flux10.scaled)]) <- 0
  
  # need vector of species biomasses 
biom10

  # calculation quantitative jacobian matrix 
J.10 = jacobian_quant(m.flux10.scaled, biom10)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.10


s = (stability(J.10, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s10 = s %>% 
  mutate(year =2010)
s10


## 2011 #====================
flux11 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux11 = as.matrix(flux11) # make sure it's in the correct format columns are predators and rows are prey 
m.flux11

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux11.scaled = scalexy(m.flux11,
                             min = -0.01,
                             max = 0.99)
diag(m.flux11.scaled[,1:ncol(m.flux11.scaled)]) <- 0
  
  # need vector of species biomasses 
biom11

  # calculation quantitative jacobian matrix 
J.11 = jacobian_quant(m.flux11.scaled, biom11)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.11


s = (stability(J.11, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s11 = s %>% 
  mutate(year =2011)
s11

## 2012 #====================
flux12 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux12 = as.matrix(flux12) # make sure it's in the correct format columns are predators and rows are prey 
m.flux12

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux12.scaled = scalexy(m.flux12,
                             min = -0.01,
                             max = 0.99)
diag(m.flux12.scaled[,1:ncol(m.flux12.scaled)]) <- 0
  
  # need vector of species biomasses 
biom12

  # calculation quantitative jacobian matrix 
J.12 = jacobian_quant(m.flux12.scaled, biom12)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.12


s = (stability(J.12, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s12 = s %>% 
  mutate(year =2012)
s12

## 2013 #====================
flux13 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux13 = as.matrix(flux13) # make sure it's in the correct format columns are predators and rows are prey 
m.flux13

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux13.scaled = scalexy(m.flux13,
                             min = -0.01,
                             max = 0.99)
diag(m.flux13.scaled[,1:ncol(m.flux13.scaled)]) <- 0
  
  # need vector of species biomasses 
biom13

  # calculation quantitative jacobian matrix 
J.13 = jacobian_quant(m.flux13.scaled, biom13)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.13


s = (stability(J.13, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s13 = s %>% 
  mutate(year =2013)
s13

## 2014 #====================
flux14 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux14 = as.matrix(flux14) # make sure it's in the correct format columns are predators and rows are prey 
m.flux14

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux14.scaled = scalexy(m.flux14,
                             min = -0.01,
                             max = 0.99)
diag(m.flux14.scaled[,1:ncol(m.flux14.scaled)]) <- 0
  
  # need vector of species biomasses 
biom14

  # calculation quantitative jacobian matrix 
J.14 = jacobian_quant(m.flux14.scaled, biom14)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.14


s = (stability(J.14, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s14 = s %>% 
  mutate(year =2014)
s14

## 2015 #====================
flux15 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux15 = as.matrix(flux15) # make sure it's in the correct format columns are predators and rows are prey 
m.flux15

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux15.scaled = scalexy(m.flux15,
                             min = -0.01,
                             max = 0.99)
diag(m.flux15.scaled[,1:ncol(m.flux15.scaled)]) <- 0
  
  # need vector of species biomasses 
biom15

  # calculation quantitative jacobian matrix 
J.15 = jacobian_quant(m.flux15.scaled, biom15)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.15


s = (stability(J.15, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s15 = s %>% 
  mutate(year =2015)
s15

## 2016 #====================
flux16 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux16 = as.matrix(flux16) # make sure it's in the correct format columns are predators and rows are prey 
m.flux16

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux16.scaled = scalexy(m.flux16,
                             min = -0.01,
                             max = 0.99)
diag(m.flux16.scaled[,1:ncol(m.flux16.scaled)]) <- 0
  
  # need vector of species biomasses 
biom16

  # calculation quantitative jacobian matrix 
J.16 = jacobian_quant(m.flux16.scaled, biom16)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.16


s = (stability(J.16, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s16 = s %>% 
  mutate(year =2016)
s16

## 2017 #====================
flux17 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux17 = as.matrix(flux17) # make sure it's in the correct format columns are predators and rows are prey 
m.flux17

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux17.scaled = scalexy(m.flux17,
                             min = -0.01,
                             max = 0.99)
diag(m.flux17.scaled[,1:ncol(m.flux17.scaled)]) <- 0
  
  # need vector of species biomasses 
biom17

  # calculation quantitative jacobian matrix 
J.17 = jacobian_quant(m.flux17.scaled, biom17)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.17


s = (stability(J.17, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s17 = s %>% 
  mutate(year =2017)
s17

## 2018 #====================
flux18 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux18 = as.matrix(flux18) # make sure it's in the correct format columns are predators and rows are prey 
m.flux18

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux18.scaled = scalexy(m.flux18,
                             min = -0.01,
                             max = 0.99)
diag(m.flux18.scaled[,1:ncol(m.flux18.scaled)]) <- 0
  
  # need vector of species biomasses 
biom18

  # calculation quantitative jacobian matrix 
J.18 = jacobian_quant(m.flux18.scaled, biom18)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.18


s = (stability(J.18, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s18 = s %>% 
  mutate(year =2018)
s18

## 2019 #====================
flux19 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux19 = as.matrix(flux19) # make sure it's in the correct format columns are predators and rows are prey 
m.flux19

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux19.scaled = scalexy(m.flux19,
                             min = -0.01,
                             max = 0.99)
diag(m.flux19.scaled[,1:ncol(m.flux19.scaled)]) <- 0
m.flux19.scaled
  
# need vector of species biomasses 
biom19

  # calculation quantitative jacobian matrix 
J.19 = jacobian_quant(m.flux19.scaled, biom19)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.19


s = (stability(J.19, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s19 = s %>% 
  mutate(year =2019)
s19


## 2020 #====================
flux20 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux20 = as.matrix(flux20) # make sure it's in the correct format columns are predators and rows are prey 
m.flux20

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux20.scaled = scalexy(m.flux20,
                             min = -0.01,
                             max = 0.99)
diag(m.flux20.scaled[,1:ncol(m.flux20.scaled)]) <- 0
  
  # need vector of species biomasses 
biom20

  # calculation quantitative jacobian matrix 
J.20 = jacobian_quant(m.flux20.scaled, biom20)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.20


s = (stability(J.20, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s20 = s %>% 
  mutate(year =2020)
s20

## 2021 #====================
flux21 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux21 = as.matrix(flux21) # make sure it's in the correct format columns are predators and rows are prey 
m.flux21

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux21.scaled = scalexy(m.flux21,
                             min = -0.01,
                             max = 0.99)
diag(m.flux21.scaled[,1:ncol(m.flux21.scaled)]) <- 0
  
  # need vector of species biomasses 
biom21

  # calculation quantitative jacobian matrix 
J.21 = jacobian_quant(m.flux21.scaled, biom21)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.21


s = (stability(J.21, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s21 = s %>% 
  mutate(year =2021)
s21

## 2022 #====================
flux22 

# Suave stability function # 
  # Need matrix where off-diagonal elements represent 'interaction frequency' 
m.flux22 = as.matrix(flux22) # make sure it's in the correct format columns are predators and rows are prey 
m.flux22

  # (maybe don't need to scale - look at final values; Scale fluxes to be between 0.01 and 0.99 as an estimate of interaction frequency 
  # If higher energy flux between predator and prey - greater magnitude of interaction
m.flux22.scaled = scalexy(m.flux22,
                             min = -0.01,
                             max = 0.99)
diag(m.flux22.scaled[,1:ncol(m.flux22.scaled)]) <- 0
  
  # need vector of species biomasses 
biom22

  # calculation quantitative jacobian matrix 
J.22 = jacobian_quant(m.flux22.scaled, biom22)
  # m is the adjacency matrix of the graph, dim(m) = S*S whose non-zero elements are interaction frequencies.
      # Instead of 
    # undirected interactions (or bidirectional) are such that m_ij = m_ji > 0
    # directected interactions (such as antagonistic ones) are such that m_ij > 0 and m_ji = 0 if i feeds on j (j -> i)
    # S being the total number of species
    # ab is a vector of species abundance. ab might be inferred with Staniczenko et al.'s method (Nat. Commun. 2013)
J.22


s = (stability(J.22, s2 = 2)) # make this number a bit more manageable 
s
  # J is the community matrix  (i.e. the jacobian matrix, see functions jacobian_binary() or jacobian_quant()) with a null diagonal, dim(J) = S*S
  # s2 is an arbitrary parameter such that the real part of the greatest eigenvalue of J-s2*I is negative
s = tibble(s)
s22 = s %>% 
  mutate(year =2022)
s22


# Put stability values into a data frame #=================================
stab.dat = rbind(s01, s02, s03, s04, s05, s06, s07, s08, s09, 
                 s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, 
                 s20, s21, s22)
stab.dat

windows(height = 5, width = 4)
par(mfrow = c(2,1), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Fluxweb stability calculation (Must run Step3 first) 
plot(stability~year, type = 'o', pch = 19, lwd = 2, 
     data = preference_stability, ylim = c(0, 400), 
     xlim = c(2001,2022), xlab = '', ylab = '', xaxt = 'n')
axis(side = 1, 
     at = c(2000,2001,2002,2003,2004,2005,2006,2007,2008,
            2009,2010,2011,2012,2013,2014,2015,2016,2017,
            2018,2019,2020,2021,2022), 
     labels = c('', '', '', '','','','','','','','','',
                '', '','','','','','','','','',''), cex = 3, las = 3)
# labels = c('', '2001', '', '2003','','2005','','2007','','2009','','2011',
#                 '', '2013','','2015','','2017','','2019','','2021','')
mtext(expression(`Stability, ` ~ italic(s)), side = 2, line = 3, cex = 1.2)
mtext(expression(italic(Fluxweb)), side = 2, line = 1.8, cex = 1)
abline(v = 2006.5, lty = 2)
abline(v = 2014.5, lty = 2)
# mtext('Year', side = 1, line = 2.8, cex = 1.2)

plot(s~year, type = 'o', pch = 19, lwd = 2, 
     data = stab.dat,  
     xlim = c(2001,2022), xlab = '', ylab = '', xaxt = 'n')
axis(side = 1, 
     at = c(2000,2001,2002,2003,2004,2005,2006,2007,2008,
            2009,2010,2011,2012,2013,2014,2015,2016,2017,
            2018,2019,2020,2021,2022), 
     labels = c('', '2001', '', '2003','','2005','','2007','','2009','','2011',
                '', '2013','','2015','','2017','','2019','','2021',''), cex = 3, las = 3)

mtext(expression(`Stability, ` ~ italic(s)), line = 3, cex = 1.2, side = 2)
mtext(expression(italic(Manual)), side = 2, line = 1.8, cex = 1)

abline(v = 2006.5, lty = 2, lwd = 1)
abline(v = 2014.5, lty = 2, lwd = 1)
mtext('Year', side = 1, line = 2.8, cex = 1.2)


# #Fluxweb stability 
# 
# plot(stability~year, type = 'o', pch = 19, lwd = 2, 
#      data = preference_stability, ylim = c(0, 400), 
#      xlim = c(2001,2022), xlab = '', ylab = '', xaxt = 'n')
# axis(side = 1, 
#      at = c(2000,2001,2002,2003,2004,2005,2006,2007,2008,
#             2009,2010,2011,2012,2013,2014,2015,2016,2017,
#             2018,2019,2020,2021,2022), 
#      labels = c('', '2001', '', '2003','','2005','','2007','','2009','','2011',
#                 '', '2013','','2015','','2017','','2019','','2021',''), cex = 3, las = 3)
# mtext(expression(`Stability, ` ~ italic(s)), side = 2, line = 2.8, cex = 1.2)
# mtext('Fluxweb Method', line = 1.8, cex = 1.2, side = 2)
# 
# abline(v = 2006.5, lty = 2)
# abline(v = 2014.5, lty = 2)
# mtext('Year', side = 1, line = 2.8, cex = 1.2)
# 
# 
