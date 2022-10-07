##############################################
# SIENA lab exercise 2
# SAOM workshop at EUSN 2019
#
# This analysis runs a basic model to illustrate the functionality
# of RSiena
#
# The data used are from the Glasgow Teenage Friends and Lifestyle Study
# More information and copyright information at
# http://www.stats.ox.ac.uk/~snijders/siena/Glasgow.htm
#
# Testing structural network effects, dyadic covariate effects, and
# homophily on 'gender' and 'pocket money' in the older cohort of the study
#
# STEPS:
# 1. read data from files
# 1.5 calculate network descriptives
# 2. create SIENA objects
# 3. specify SIENA model
# 4. create estimation algorithm
# 5. estimate SIENA model
#
# The script mainly follows the one for the Leuven course
# on Social Networks taught by Chrisoph Stadtfeld. That script
# was a modified version of a script used in the SIENA course in
# Capri by Christoph Stadtfeld and Per Block. Their script, however,
# is already based on a script by Tom Snijders from the SIENA website:
# http://www.stats.ox.ac.uk/~snijders/siena/
# (check the website for a number of helpful scripts!)
# Some changes applied by Per Block
# Some changes applied by Christoph Stadtfeld
# Some changes applied by Andras Voros
##############################################




######### Step 0: Preparations #########

# in case you want to clean your workspace
rm(list=ls())

# set working directory

#setwd("YOUR-DIRECTORY-HERE")
getwd()

# load the RSiena library

library(RSiena)


######### Step 1: load and recode the data #########

# load the data (you can find it among the course materials,
# or you can download it from the Siena website:
# http://www.stats.ox.ac.uk/~snijders/siena/Glasgow.zip)

# here are the friendship networks at the three time points
load("Glasgow/Glasgow-friendship.RData")

# in here we can find the sex of the pupils
load("Glasgow/Glasgow-demographic.RData")

# this includes the pocket money the pupils receive from their parents
load("Glasgow/Glasgow-various.RData")

# distance shows how far pupils live from each other at the three time points
load("Glasgow/Glasgow-geographic.RData")

# the data can be viewed by clicking on the name of the data object

# recode the network variables into 1s
# allowed network values are 0, 1, 10, and 11
# 10 and 11 stand for structural zeros or structural ones
# for details see the manual section 4.3.1

friendship.1[friendship.1 %in% c(1,2)] <- 1
friendship.2[friendship.2 %in% c(1,2)] <- 1
friendship.3[friendship.3 %in% c(1,2)] <- 1

# recode sex from 1/2 into 0/1

sex.F[sex.F == 1] <- 0
sex.F[sex.F == 2] <- 1

# recode missing values from pocket money (coded as -1) to NA

money[money == -1] <- NA


######### Step 1.5: Let us take a look at the network #########

#detach(package:sna) # if you have the sna package loaded, it's a good idea to
                     # detach it - overlapping function names with igraph
library(igraph)

nActors <- dim(friendship.1)[1]
mat1 <- matrix(0, nActors, nActors)
mat2 <- matrix(0, nActors, nActors)
mat1[friendship.1 %in% c(1,11)] <- 1
mat2[friendship.2 %in% c(1,11)] <- 1

graph1 <- graph.adjacency(mat1)
graph2 <- graph.adjacency(mat2)
graph12 <- graph.adjacency(mat1 + mat2)
myLayout <- layout.fruchterman.reingold(graph12)

par(mfrow = c(1, 2))
plot(graph1,
     vertex.color = ifelse(sex.F == 1, "red", "darkblue"),
     vertex.shape = ifelse(sex.F == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 3,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 1")
plot(graph2,
     vertex.color = ifelse(sex.F == 1, "red", "darkblue"),
     vertex.shape = ifelse(sex.F == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 3,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 2")

# You can think about other descriptive figures, e.g. how to
# visualize the "pocket money" covariate on the network? It's
# not in our main focus right now, but it is always good practice to
# first get a description of the networks and covariates you analyze


######### Step 2: Create the RSiena objects #########

# now that all data is loaded into R, we can 
# construct the type of data that RSiena understands

# create the longitudinal network object

?sienaDependent
myNetwork <- sienaDependent( array( c(friendship.1, friendship.2, friendship.3 ), 
                                    dim = c(nActors, nActors, 3 ) ) )
myNetwork

# create the individual covariates

?coCovar
sex <- coCovar(sex.F)
sex

?varCovar
pocketMoney <- varCovar(money)
pocketMoney

# create the varying dyadic covariate
# PAY ATTNETION TO ONLY USING THE FIRST TWO DISTANCE OBJECTS

?varDyadCovar
distance <- varDyadCovar(array (c(distance.1, distance.2), dim=c(160, 160, 2)))
distance

# now create an object that includes all the information necessary for a Siena analysis

?sienaDataCreate
myData <- sienaDataCreate(myNetwork, sex, pocketMoney, distance) 
myData

# Including the third observation of the dyadic covariate (distance) would have
# returned an error when calling sienaDataCreate:
#                 Error in dyvCovars[[i]][, 1:(observations - 1)] : 
#                   incorrect number of dimensions
# It is because sienaDataCreate expects 2 observations for the varying covariate:
# covariates are not modeled, and their state in t3 is irrelevant for estimation.

# print report to check
print01Report(myData, modelname="Glasgow_160students")


######### Step 3: Specify a model #########

# create an effects object for the created data

?getEffects
myEff <- getEffects(myData)

#look at your effects object

myEff
names(myEff)

# see all the short names of available effects
myEff$effectName
myEff$shortName

# a more civilized list
effectsDocumentation(myEff)

# include the structural effects
?includeEffects
myEff <- includeEffects(myEff, transTrip)
myEff <- includeEffects(myEff, cycle3, transRecTrip, inPop, outPop)

myEff

# include covariate related effects

myEff <- includeEffects(myEff, sameX, interaction1="sex")
myEff <- includeEffects(myEff, egoX, interaction1="sex")
myEff <- includeEffects(myEff, altX, interaction1="sex")

myEff <- includeEffects(myEff, egoX, altX, simX, interaction1="pocketMoney")

# include the dyadic covariate effect

myEff <- includeEffects(myEff, X, interaction1="distance")


# how about an interaction???
# remember that not all interactions are allowed - 
# refer to the manual and search for interaction
# section 5.8

?includeInteraction

myEff <- includeInteraction(myEff, sameX, simX, interaction1=c("sex","pocketMoney"))
myEff

# take care that you might have to specify the interaction1 either way

myEff <- includeInteraction(myEff, recip, sameX, interaction1=c("","sex"))

# ok, we actually don't want these interaction effects here, because
# they make interpretations very complicated
myEff <- includeInteraction(myEff, sameX, simX, interaction1=c("sex","pocketMoney"), include=F)
myEff <- includeInteraction(myEff, recip, sameX, interaction1=c("","sex"), include=F)
myEff

# now we have a model that is reasonably specified (as past research has shown...)


######### Step 4: Create Algorithm #########

# Create a model algorithm (here you can specify lots of parameters, but we won't for now)

?sienaAlgorithmCreate
myModel <- sienaAlgorithmCreate(useStdInits=FALSE, projname="My Model")

# Now, finally, we can actually run the model


######### Step 5: Estimate #########

?siena07
myAnswer <- siena07(myModel, data = myData, effects = myEff, returnDeps = T)
myAnswer

# if convergence is not satisfying or we have a model that takes long to
# get to the proper values, we can run again with new starting values

myAnswer <- siena07(myModel, data = myData, effects = myEff, returnDeps = T,
                    prevAns=myAnswer, # start with previous results
                    useCluster=TRUE, nbrNodes=4) # speed up computation by using multiple processes
                                                 # (if you have multiple processor cores)
myAnswer

siena.table(myAnswer,
            type="html",
            file="Glasgow_result.html",
            tstatPrint=TRUE,
            sig=TRUE,
            d=2)

# All results are also saved in the file "My Model.out" in the working directory
# check the t-ratios for convergence and the overall maximum convergence t-ratio!!!
# only then proceed to looking at the results

# Let's interpret the results together!