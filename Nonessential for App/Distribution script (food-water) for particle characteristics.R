#######Partial script from RAMP for Scott Coffin######
### Nur Hazimah Mohamed Nor- hazimah.mohamednor@wur.nl
### Updated: 10/03/21
### Mohamed Nor, N.H., Kooi, M., Diepens, N.J., Koelmans, A.A., Lifetime
### accumulation of microplastic in children and adults, Environmental Science and Technology. 

rm(list=ls())
# install.packages("triangle")
# install.packages("mixtools")
# install.packages("msm")
# install.packages("reader")
# install.packages("vctrs")

library(triangle)
library(ggplot2)
library(mixtools)
library(msm)
library(readr)
library(sn)

####The following codes generate samples based on distributions pre-defined in HEASI model####
####== Number of monte carlo simulations ==####
n = 10000
df <-data.frame(n = c(1:n),
                Alpha = numeric(n),
                Size = numeric(n),
                W_L = numeric(n),
                H_L = numeric(n),
                Dens = numeric(n))

#### ======SIZE======= ####

#Change parameters here
m.alpha.w = 1.60
sd.alpha.w = 0.51
min.alpha.w = 1.01
max.alpha.w = 2.56
size.min = 1    #um
size.max = 5000 #um

# Alpha (-)
df$Alpha <- rtnorm(n = n, mean = m.alpha.w, sd = sd.alpha.w, lower = min.alpha.w, upper = max.alpha.w)

# Size (um)
for(i in 1:n){
  success <- FALSE
  while(!success){
    U = runif(1, 0, 1) 
    size = size.min*(1-U)^(1/(1-df$Alpha[i]))
    success <- size <= 5000}
  df[which(df$n == i),]$Size= size
}


#### ===== Shape =====####
#Need to define w.m1, w.s1, w.m2, w.s2 and w.prob1 from Table S7
### FOOD ###
w.m1=0.271 
w.s1 = 0.113
w.prob1 = 0.728
w.m2=0.690 
w.s2 = 0.140
w.prob2 = 0.272



### Sampling the W:L ratio
for(i in 1:n){
  U = runif(1, 0, 1)
  if(U < w.prob1){
    W_L = rtnorm(1,w.m1,w.s1, lower = 0, upper = 1)
  }else{
    W_L = rtnorm(1,w.m2,w.s2, lower = 0, upper = 1)
  }
  df[which(df$n == i),]$W_L= W_L
}

### Sampling the L:H ratio
##Only allow values which are equal or less than L:W ratio (otherwise, W and H get swapped)
#Need to define h.m1, h.s1, h.m2, h.s2 and h.prob1 from Table S7

# FOOD VALUES BELOW
h.m1=0.255 
h.prob1 = 0.759
h.s1 = 0.116
h.m2 = 0.632
h.prob2 = 0.251
h.s2 = 0.153



for(i in 1:n){
  success <- FALSE
  while(!success){
    U = runif(1, 0, 1) 
    if(U < h.prob1){
      H_L = rtnorm(1,h.m1,h.s1, lower = 0, upper = 1)
    }else{
      H_L = rtnorm(1,h.m2,h.s2, lower = 0, upper = 1)
    }
    success <- H_L <= df$W_L[i]
  }
  df[which(df$n == i),]$H_L= H_L
}


#### ===== Density =====####
df$Dens=rnormmix(n,lambda=df.probs$prob,mu=df.probs$mu,sigma=df.probs$sd)

## may also load density data ##
#read_rds('Density data.R.RData')

#################################
#### MASS #######################
## fitdist pacakge
## sn package 
df$Volume <- (4/3)*(1/8)*pi*df$Size*(df$Size*df$L_W)*(df$Size*df$L_H)*1E-12 #cm3. 
df$Mass <- df$Volume*df$Dens*1000 #mg
df$Mass.log <- log10(df$Mass)

### Fit distribution
fit.normx=normalmixEM(df$Mass.log,k=3)

#Combined gaussian distribution
dmnorm <- function(x, mu, sigma, pmix) {
  pmix[1]*dnorm(x,mu[1],sigma[1]) + pmix[2]*dnorm(x,mu[2],sigma[2])+pmix[3]*dnorm(x,mu[3],sigma[3])
}

#Individual gaussian distribution
dmnorm1<- function(x, mu, sigma, pmix){ 
  pmix*dnorm(x,mu,sigma)
}       

####Plot graph####
barfill <- "#4271AE"
Mass.log=ggplot(data=df, aes(x=df$Mass.log)) + 
  geom_histogram(aes(y=..density..),binwidth=0.1,alpha=0.4,fill=barfill) + 
  scale_x_continuous(name="log10(Mass, mg)")+
  scale_y_continuous(name="Density") +
  theme_bw()+ 
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(family="Times New Roman",size=16),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        panel.border=element_rect(colour="black",fill=NA,size=1.2)) 

Mass.log=Mass.log+stat_function(fun = dmnorm,args=list(mu=fit.normx$mu,
                                                       sigma = fit.normx$sigma,
                                                       pmix = fit.normx$lambda))
for(i in 1:3){
  Mass.log=Mass.log+stat_function(fun=dmnorm1,args=list(mu=fit.normx$mu[i],
                                                        sigma = fit.normx$sigma[i],
                                                        pmix = fit.normx$lambda[i]),linetype="dashed")
}
