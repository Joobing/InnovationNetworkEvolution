##############################################
# SIENA practical: modeling selection and influence processes
# Budapest 2016
#
# This is a short script for analysing the co-evolution of the friendship 
# network and smoking behaviour. The data used are from the Glasgow Teenage
# Friends and Lifestyle Study. More information and copyright information at
# http://www.stats.ox.ac.uk/~snijders/siena/Glasgow.htm
#
# The script is based a script by Tom Snijders from the SIENA website: 
# http://www.stats.ox.ac.uk/~snijders/siena/ with modifications by 
# Christoph Stadtfeld and Zsofia Boda
# ETH Zurich, Chair of Social Networks
#
# Do not forget the STEPS:
# 1. read data from files
# 1.5 calculate network descriptives
# 2. create SIENA objects
# 3. specify SIENA model
# 4. create estimation algorithm
# 5. estimate SIENA model
#
######### Some preparation steps #########
#
# you can clean you workspace
rm(list=ls())
#
# and set working directory
#setwd("c:\\Users\\bodaz\\Google Drive\\Teaching\\EUSN_2019")
getwd()
#
# load the RSiena library
library("RSiena")
library("igraph")

####################################################
# Step 1: load and recode the data
####################################################

# load the data (you can find it among the course materials,
# or you can download it from the Siena website:
# http://www.stats.ox.ac.uk/~snijders/siena/Glasgow.zip)

# here are the friendship networks at the three time points
load('Glasgow/Glasgow-friendship.RData')
# here is the data for alcohol, cannabis and tobacco use of the pupils
load('Glasgow/Glasgow-substances.RData')

# friendships:
table(friendship.1) # needs recoding
friendship.1[friendship.1 %in% c(1,2)] <- 1
friendship.2[friendship.2 %in% c(1,2)] <- 1
friendship.3[friendship.3 %in% c(1,2)] <- 1

#alcohol
alcohol # three observations, three coloumns. 
table(alcohol)
table(alcohol, useNA="always")
table(alcohol[,1], useNA="always")
table(alcohol[,2], useNA="always")
table(alcohol[,3], useNA="always")
# values are between 1 and 5, and a higher number means a higher frequency
# of alcohol consumption. there are missings, that's ok.
# we can find more information about the data and the coding on the website:
# www.stats.ox.ac.uk/~snijders/siena/ (data sets -> Glasgow data sescription)
tobacco
table(tobacco, useNA="always")
table(tobacco[,1], useNA="always")
table(tobacco[,2], useNA="always")
table(tobacco[,3], useNA="always")
# tobacco use is measured between 1 and 3.

# introduce a function to "clean" 10s out of matrices when needed 
# (e.g. for plotting), and delete self-nominations
cleanMatrix <- function(m){
  m[m ==10] <- 0
  diag(m) <- 0
  m
}

# create networks to plot
g1 <- graph.adjacency(cleanMatrix(friendship.1))
g2 <- graph.adjacency(cleanMatrix(friendship.2))
g3 <- graph.adjacency(cleanMatrix(friendship.3))
g123 <- graph.adjacency((cleanMatrix(friendship.1) + cleanMatrix(friendship.2) 
                         + cleanMatrix(friendship.3))>0) # we used our function!
layout <- layout.auto(g123) # we set up where nodes will be

# remember when i said missings were ok?
# they are ok for siena, but igraph does not allow them.
alcohol_v <- alcohol
alcohol_v[is.na(alcohol_v)==T]<-0  
tobacco_v <- tobacco
tobacco_v[is.na(tobacco_v)==T]<-0

# function for plotting networks
myplot <- function(g, wave_a, wave_t) plot(g,
       vertex.size = tobacco_v[,wave_t]+4,
       vertex.label = "",
       vertex.color = rgb(alcohol_v[,wave_a], 5-alcohol_v[,wave_a], 
                          0, max = 5),
       vertex.shape = "circle",
       edge.arrow.size = 0.1,
       edge.width = 1.0,
       edge.color = "grey",
       layout = layout,
       edge.curved = 0.1)
# size of nodes is based on cigarette smoking, color is based on 
# alcohol consumption.

# let us plot! first we set up the basics: how many plots in one figure?
par(mfrow = c(1, 3))

# The three networks:
myplot(g1, wave_a=1, wave_t=1)
myplot(g2, wave_a=2, wave_t=2)
myplot(g3, wave_a=3, wave_t=3)

# Now we only look at the first and the last observation.
# First we only plot tie changes, then only the behavior changes.
par(mfrow = c(1, 2))
# social ties in wave 1 and wave 3, attributes kept constant
myplot(g1, wave_a=1, wave_t=1)
myplot(g3, wave_a=1, wave_t=1)
# attrivbutes at wave 1 and wave 3, network kept constant
myplot(g1, wave_a=1, wave_t=1)
myplot(g1, wave_a=3, wave_t=3)

# change of smoking/drinking over time, now using numbers!
apply(tobacco, 2, mean, na.rm=T)
apply(tobacco, 2, sd, na.rm=T)
apply(alcohol, 2, mean, na.rm=T)
apply(alcohol, 2, sd, na.rm=T)

# # change in networks
# # 10 means a "structural 0" - tie that was not possible.
# # siena can take this into account, but now we just recode them to 0.
friendship.1[friendship.1 %in% c(10)] <- 0
friendship.2[friendship.2 %in% c(10)] <- 0
friendship.3[friendship.3 %in% c(10)] <- 0
# diag(friendship.1) <- diag(friendship.2) <- diag(friendship.3) <- 0 # no self-friendships
# N1 <- sum(friendship.1, na.rm = T)
# N2 <- sum(friendship.2, na.rm = T)
# N3 <- sum(friendship.3, na.rm = T)
# Npotential <- 160*159
# N1/Npotential
# N2/Npotential
# N3/Npotential
# # how much does the network change? N of ties being there both times compared to ones
# # being there at least once.
# # both times:
# both <- sum(friendship.1*friendship.3, na.rm = T)
# # at least one:
# once <- sum(friendship.1 + friendship.3 -  friendship.1*friendship.3, na.rm = T)
# # their ratio:
# jaccard <- both/once
# # for siena models, we usually want some change, but not too much change.
# # this is fine.

####################################################
# Step 2: create SIENA objects    
####################################################

# The friendship network is the first dependent variable
friendship <- sienaDependent(array(c(friendship.1, friendship.2,
                      friendship.3), dim = c(160, 160, 3)))

# Also smoking is modeled as dependent (behavioral) variable
drinkingbeh <- sienaDependent(alcohol, type = "behavior")

# Alcohol drinking is defined as a changing covariate
smoke <- varCovar(tobacco)

# Define the data set
myCoEvolutionData <- sienaDataCreate(friendship, drinkingbeh, smoke)

####################################################
# Step 3: specify SIENA model
####################################################

myCoEvolutionEff <- getEffects(myCoEvolutionData)
myCoEvolutionEff

# see the siena manual for effects!

# get reports to check that data and get some basic descriptives

print01Report(myCoEvolutionData)


#BEHAVIOR####

# Define the effects to include in the coevolution model -
# Start with some structural effects.

myCoEvolutionEff <- includeEffects(myCoEvolutionEff, transTrip, transRecTrip, inPop, 
                                   outAct, outPop)

# Include a homophily effect for the constant covariate smoking

myCoEvolutionEff <- includeEffects(myCoEvolutionEff, simX, interaction1 = "smoke")

# If we want to parse out whether there is a selection or influence (or both)
# effect for drnking behaviour,  we need to also include sender, receiver 
# and homophily effects of drinking for friendship formation:

myCoEvolutionEff <- includeEffects(myCoEvolutionEff, egoX, altX, simX,
                                  interaction1 = "drinkingbeh" )

#BEHAVIOR####

# For the influence part, i.e. the effect of the network on behaviour,
# we specify the following effects:
# indegree, outdegree and assimilation effects for smoking

myCoEvolutionEff <- includeEffects(myCoEvolutionEff, name = "drinkingbeh",
                                   indeg, outdeg, avSim, interaction1 = "friendship")

# The effect of smoking on drinking:

myCoEvolutionEff <- includeEffects(myCoEvolutionEff, name = "drinkingbeh",
                                   effFrom, interaction1 = "smoke" )

# The effect of sex on drinking:

myCoEvolutionEff <- includeEffects(myCoEvolutionEff, name = "drinkingbeh",
                                   effFrom, interaction1 = "sex" )

# Check what effects you have decided to include:

myCoEvolutionEff

####################################################
#####  Step 4: create estimation algorithm     #####
####################################################

myCoEvAlgorithm <- sienaAlgorithmCreate(projname = 'coef_algo')

####################################################
######      Step 5: estimate SIENA model       #####
####################################################

ans <- siena07(myCoEvAlgorithm, data=myCoEvolutionData, effects=myCoEvolutionEff, 
               useCluster = T, nbrNodes = 3)

# if necessary:

# ans2 <- siena07(myCoEvAlgorithm, data=myCoEvolutionData, effects=myCoEvolutionEff,
#               useCluster = T, nbrNodes = 3, prevAns=ans)

# THE RESULTS

# To look at the results, type
ans

# or, somewhat more extensive,
summary(ans)

# Note (again) that the "convergence t-ratio" is the t-ratio for convergence checking,
# not the t statistic for testing the significance of this effect.

# We can print the table now!
siena.table(ans, type="html", tstatPrint=T, sig=T)
