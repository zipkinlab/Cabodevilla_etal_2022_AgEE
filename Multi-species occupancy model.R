##############################################################/############
###### Effect of irrigation in the rainfed farmland bird communities ######
###########  Multi-species occupancy model with a BACI design  ############
###########################################################################

rm(list=ls(all=TRUE)) # Clear memory 
setwd() # Select the directory 
getwd() # Check the filepath for the working directory

######## Dataset
## Read bird species occurrence data
bdat <- read.table(file = "birds_occ_data.txt", header = TRUE) 
str(bdat)

# How many times each species was observed
(total.count <- tapply(bdat$occ, bdat$species, sum))

# The number of unique species
(uspecies = as.character(unique(bdat$species)))

# nspec is the number of observed species
(nsp = length(uspecies))

# The number of unique sampling locations
(upoints = as.character(unique(bdat$site)))

# nsites is the number of sampled streams
(nsite = length(upoints))

# Reshape the data using the R package "reshape"
library(reshape)
# The detection/non-detection data is reshaped into a four dimensional 
# array data where the first dimension = nsite (sampling locations); the 
# second dimension = nrep (replicates); the third dimension = nyear (years); 
# and the last dimension = nsp (species). 
dat.melt <- melt(bdat, id.var=c("site", "rep", "year", "species"), measure.var="occ", na.rm=TRUE)
data <- cast(dat.melt, site ~ rep ~ year ~ species) # aggregatin the data in a 4D array.
dimnames(data)=NULL
data

# nrep is the number of replicates
(nrep <- dim (data)[2])


### Read detection covariates

## Hour
bdat_hour <- read.table(file = "Hour.txt", header = TRUE) 
#Standardize the hour
bdat_hour$Hour <- (bdat_hour$Hour-mean(bdat_hour$Hour))/sd(bdat_hour$Hour) 
str(bdat_hour)
# Reshaped into a four dimensional array
dat_hour.melt <- melt(bdat_hour, id = 1:4, na.rm=TRUE)
yH <- cast(dat_hour.melt, site ~ rep ~ year ~ specie)
dimnames(yH)=NULL
yH

## Date 
# Sampling dates were converted to Julien dates 
# We assumed the first day as April 1st (04/01/xxxx))
bdat_date <- read.table(file = "Date.txt", header = TRUE)
#Standardize the date
bdat_date$Date <- (bdat_date$Date-mean(bdat_date$Date))/sd(bdat_date$Date) #Standardize date
str(bdat_date)
# Reshaped into a four dimensional array
dat_date.melt <- melt(bdat_date, id = 1:4, na.rm=TRUE)
yD <- cast(dat_date.melt, site ~ rep ~ year ~ specie)
dimnames(yD)=NULL
yD

### Read habitat covariates

## Arable surface
yA <- read.table(file = "Arable_surface.txt", header = TRUE) 
#Standardize arable surface
yA$x <- (yA$x-mean(yA$x))/sd(yA$x) 
yA

## Irrigation presence/absence
yI <- read.table(file = "Irrigation.txt", header = FALSE) 
yI <-data.matrix(yI, rownames.force = NA) 
yI

# Write the model code to a text file 
library(jagsUI)
###
sink("Model_Multi_sp_BACI1.txt") 
cat("
model {

# Subscripts:
# i = Species
# j = Site
# k = Visit
# t = Year

# Specify priors
  mu.a0.before ~ dnorm(0, 0.37)
  mu.a0.after ~ dnorm(0, 0.37)
  mu.a1 ~ dnorm(0, 0.1)
  mu.b0 ~ dnorm(0, 0.1)
  mu.b1 ~ dnorm(0, 0.1)
  mu.b2 ~ dnorm(0, 0.1)
  mu.b3 ~ dnorm(0, 0.1)


  tau.a0.spp ~ dgamma(0.1,0.1)
  tau.a1 ~ dgamma(0.1,0.1)
  tau.b0 ~ dgamma(0.1,0.1)
  tau.b1 ~ dgamma(0.1,0.1)
  tau.b2 ~ dgamma(0.1,0.1)
  tau.b3 ~ dgamma(0.1,0.1)
  tau.psi.site ~ dgamma(0.1,0.1)
  tau.psi.year ~ dgamma(0.1,0.1)

  for (j in 1:nsite){
    psi.site[j] ~ dnorm(0, tau.psi.site)
    }

  for (i in 1:nsp){
    a1[i] ~ dnorm(mu.a1, tau.a1)
    b0[i] ~ dnorm(mu.b0, tau.b0)
    b1[i] ~ dnorm(mu.b1, tau.b1)
    b2[i] ~ dnorm(mu.b2, tau.b2)
    b3[i] ~ dnorm(mu.b3, tau.b3)
    a0[1,i] ~ dnorm(mu.a0.before, tau.a0.spp) 
    a0[2,i] ~ dnorm(mu.a0.after, tau.a0.spp)
    for (t in 1:nyear){
      psi.year[t,i] ~ dnorm(0, tau.psi.year)
      }
  
# Ecological submodel: Define state conditional on parameters
    for (j in 1:nsite){
      for (t in 1:nyear){
        logit(psi[j,t,i]) <- a0[irrigation[j,t]+1,i] + a1[i]*arable[j] + psi.year[t,i] + psi.site[j]
        z[j,t,i] ~ dbern(psi[j,t,i])

# Observation model
          for (k in 1:nrep){
            muy[j,k,t,i] <- z[j,t,i]*p[j,k,t,i]
            logit(p[j,k,t,i]) <- b0[i] + b1[i]*hour[j,k,t,i] + b2[i]*date[j,k,t,i] + b3[i]*(date[j,k,t,i]^2)
            y[j,k,t,i] ~ dbern(muy[j,k,t,i]) 
            } #k
          } #t
        } #j 
      } #i

# Effect of irrigation per specie
   for(i in 1:nsp){ 
      effect.a0.sp[i] <- a0[2,i] - a0[1,i]
   }
   for (j in 1:nsite){
      for (t in 1:nyear){
         Nsite[j,t] <- sum(z[j,t,])
      }
   }
}
",fill = TRUE) 
sink()

# Load all the data
jags.data <- list(y = yM, nsite = dim(yM)[1], 
                  nrep = dim(yM)[2], nyear = dim (yM)[3],
                  nsp = dim(yM)[4], hour = yH, date = yD, 
                  arable = (yA)[,1], irrigation = yI)

# Specify the parameters to be monitored
jags.params <- c("mu.a0.before", "mu.a0.after", "mu.a1", "mu.b0", "mu.b1", 
                 "mu.b2", "mu.b3", "tau.a0.spp", "tau.a1", "tau.b0", "tau.b1", 
                 "tau.b2", "tau.b3", "tau.psi.year", "tau.psi.site", "psi", 
                 "a0", "a1", "b0", "b1", "b2","b3", "effect.a0.sp", "p", "z", 
                 "Nsite")

# Specify the initial values
zInit <- apply(yM,c(1,3,4),max,na.rm=TRUE)  
jags.inits <- function(){ list(z = zInit)}

# MCMC settings
ni <- 70000 # number of total iterations per chain
nt <- 5 # thinning rate 
nb <- 55000 # number of iterations to discard at the beginning
nc <- 3 # number of Markov chains

# Load the jagsUI library
library(jagsUI)

# Call JAGS from R
Output.model <-  jags(data = jags.data, inits = jags.inits, jags.params, "Model_Multi_sp_BACI1.txt", 
                      n.chains = nc, n.adapt= NULL, n.iter = ni, n.burnin = nb,
                      n.thin = nt, parallel = T, store.data=T)

################################
################################