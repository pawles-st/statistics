library(goftest)
library(smoothmest)
library(rando)
#library(nimble)

alpha <- 0.05
n <- 100
epsilon <- 0.01
sigma <- 100
M <- 1
theta <- 0.2

cdf_1 <- function(x){
  return(pnorm(x, 0, 1))
}

cdf_2 <- function(x){
  return(pnorm(x, 0, 5))
}

cdf_3 <- function(x){
  return(
    (1-epsilon) * cdf_1(x) + epsilon * pnorm(x, 0, sigma)
  )
}

cdf_4 <- function(x){
  return(0.5 * pexp(abs(x)))
}

data <- r_cdf(Fun=cdf_1, n=n)
MC_simulation <- function(cdf){
  reject_h0_KS <- 0
  reject_h0_CvM <- 0
  reject_h0_AD <- 0
  
  reject_h1_KS <- 0
  reject_h1_CvM <- 0
  reject_h1_AD <- 0
  
  for (i in 1:M){
    data <- r_cdf(Fun=cdf, n=n)
    
    if (ks.test(data, function(x) cdf(x))$p.value < alpha ){
      reject_h0_KS <- reject_h0_KS + 1
    }
    
    if (cvm.test(data, function(x) cdf(x))$p.value < alpha ){
      reject_h0_CvM <- reject_h0_CvM + 1
    }
    
    if (ad.test(data, function(x) cdf(x))$p.value < alpha ){
      reject_h0_AD <- reject_h0_AD + 1
    }
    
    
    
    if (ks.test(data-theta, function(x) cdf(x))$p.value < alpha ){
      reject_h1_KS <- reject_h1_KS + 1
    }
    
    if (cvm.test(data-theta, function(x) cdf(x))$p.value < alpha ){
      reject_h1_CvM <- reject_h1_CvM + 1
    }
    
    if (ad.test(data-theta, function(x) cdf(x))$p.value < alpha ){
      reject_h1_AD <- reject_h1_AD + 1
    }
    
  }

  result <- c(reject_h0_KS, reject_h0_CvM, reject_h0_AD, reject_h1_KS, reject_h1_CvM, reject_h1_AD)/M
  
  return(result)
  
}

result_matrix <- list(
  'N(0, 1)'=c(), 
  'N(0, 5^2)'=c(),
  'N-mieszanka'=c(),
  'podwójnie wykł. (0, 1)'=c()
  )

cdf_list <- list(cdf_1, cdf_2, cdf_3, cdf_4)
for (i in 1:4){
  cdf_i <- cdf_list[[i]]
  result_matrix[[i]] <- c(MC_simulation(cdf_i) )
}
result_matrix








