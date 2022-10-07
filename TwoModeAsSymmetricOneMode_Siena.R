###############################################################################
## File TwoModeAsSymmetricOneMode_Siena.R                                             ##
## January 13, 2017                                                          ##
## By Tom A.B. Snijders                                                      ##
###############################################################################

# This scripts shows how to analyze a two-mode network as a one-mode network.
# This sometimes is useful because it opens up some further possibilities
# to use RSiena options.

library(RSienaTest)
library(RSiena)

# Download the data from
# https://www.stats.ox.ac.uk/~snijders/siena/Glasgow_data.zip
# Information is at
# https://www.stats.ox.ac.uk/~snijders/siena/Glasgow_data.htm

# Read data sets:
load("Glasgow-demographic.RData") # for gender data
load("Glasgow-various.RData") # for covariate data
load("Glasgow-lifestyle.RData") # for lifestyle data
load("Glasgow-substances.RData") # for substance data
# what do we have:
ls()
# The numbers 1-2-3 in the names refer to waves.
dim(leisure1)
# What is this for an object?
str(leisure1)
dimnames(leisure1)[[2]]

# Item 15 is not an activity, and will not be used.
# Get an impression of the file:
head(leisure1)
# leisure2 and leisure3 are similar.

# Some data exploration

table(leisure1)
table(leisure2)
table(leisure3)

# Dichotomize:
leis1 <- leisure1
leis1[leis1 <= 2 ] <- 1
leis1[leis1 >= 3 ] <- 0
which(is.na(leisure1))
colSums(leis1, na.rm=TRUE)

# Dichotomize differently:
leis1a <- leisure1
leis1a[leis1a <= 1 ] <- 1
leis1a[leis1a >= 2 ] <- 0
colSums(leis1a, na.rm=TRUE)

# This shows that almost all listen daily to tapes and CDs.
# This seems not to differentiate.
# The activities differ naturally in whether one would do them
# daily or almost so, or less than that.
# Daily or almost: 1, 2, 3, 5, 6, 7, 8, 14



#########################################################


tl1 <- apply(leisure1,2, tabulate)
tl2 <- apply(leisure2,2, tabulate)
tl3 <- apply(leisure3,2, tabulate)

colSums(tl1 + tl2 + tl3)
# Note: these are almost constant, so missings are not a problem

# A descriptive table - use your R knowledge to interpret what this is
xtable(t(tl1 + tl2 + tl3))

# new thresholds, depending on frequencies:
threshold <- c(1,1,1,2,1,1,1,1,2,2,3,3,1,3)

# now dichotomize the way we want it:
leis <- array(0, dim=c(160,14,3))
for (i in 1:160){for (j in 1:14) {leis[i,j,1] <- ifelse(leisure1[i,j] <= threshold[j],1,0)}}
for (i in 1:160){for (j in 1:14) {leis[i,j,2] <- ifelse(leisure2[i,j] <= threshold[j],1,0)}}
for (i in 1:160){for (j in 1:14) {leis[i,j,3] <- ifelse(leisure3[i,j] <= threshold[j],1,0)}}
# check
dim(leis)
# Some gender-related descriptives:
(meanF <- apply(leis[sex.F==2,,], c(2,3), mean, na.rm=TRUE))
(meanM <- apply(leis[sex.F==1,,], c(2,3), mean, na.rm=TRUE))
cbind(dimnames(leisure1)[[2]][1:14], round(meanF,2), round(meanM,2))

# Construct artificial variable to use as dependent variable later on (just for demonstration)
(F.act <- round(3*meanF)[2:14,] + 1)
# Definition of the two-mode network

# Note that 15, "I do nothing much (am bored)" is dropped;
# also, the first is dropped, because it is too common.
leisure <- leis[,2:14,]


# Time trend:
timetrend <- varCovar(matrix(c(0,1),160,2,byrow=TRUE), nodeSet="students")
# Composition change:
comp.ch <- sienaCompositionChangeFromFile("cc.txt", nodeSet ="students",
							option=2)

# Instead, this data is used here to illustrate how to represent the
# two-mode network as a non-directed one-mode network, with structural zeros.

# What are the dimensions again:
dim(leisure)
n <- dim(leisure)[1]
m <- dim(leisure)[2]
w <- dim(leisure)[3]
# Start with a matrix filled with structural zeros
leis.b <- array(10, dim=c(m+n,m+n,w))
# check
dim(leis.b)
mean(leis.b)
# now put the two-mode network in the off-diagonal block.
# For this we use the object leisure, the dependent variable itself;
# we could equivalently use the 3-dimensional array of data used to create it.
leis.b[(m+1):(m+n),1:m,] <- leisure
leis.b[1:m,(m+1):(m+n),1] <- t(leisure[,,1])
leis.b[1:m,(m+1):(m+n),2] <- t(leisure[,,2])
leis.b[1:m,(m+1):(m+n),3] <- t(leisure[,,3])
# check
table(leisure)
table(leis.b)   # everything * 2
# Here we go:
leisure.b <- sienaDependent(leis.b)
# And what did we create now:
leisure.b
# Note the Type.

# Calculate the dummy variables indicating the two types of node.
activity <- coCovar(c(rep(1,m), rep(0,n)), centered=FALSE)
student <- coCovar(c(rep(0,m), rep(1,n)), centered=FALSE)
# We have to adapt the composition change object.
# Calculate the longer composition change list
comp.ch.b <- c(lapply(1:m,function(x){c(1,3)}), comp.ch)
comp.ch.b <- sienaCompositionChange(comp.ch.b, option=2)

# Similarly adapt the covariates:
# Define non-centered gender variables, values 0-1:
girls.b <- coCovar(c(rep(NA,m), sex.F-1), centered=FALSE)
boys.b <- coCovar(c(rep(NA,m), 2-sex.F), centered=FALSE)
ages.b <- coCovar(c(rep(NA,m), age))


# Define dependent variables; note that g.act is fake
smoke <- sienaDependent(rbind(cbind(rep( NA,m),rep( NA,m),rep( NA,m)), tobacco), type="behavior",
					allowOnly=FALSE)
drink <- sienaDependent(rbind(cbind(rep( NA,m),rep( NA,m),rep( NA,m)), alcohol), type="behavior",
					allowOnly=FALSE)
g.act <- sienaDependent(rbind(F.act, cbind(rep( NA,n),rep( NA,n),rep( NA,n))), type="behavior",
					allowOnly=FALSE)

# Construct data set:
G160_leisb_data <- sienaDataCreate(leisure.b, student, activity,
					girls.b, boys.b, ages.b, drink, g.act, comp.ch.b)
# This has the two-mode network as two symmetric off-diagonal blocks;
# g.act is a dependent variable for the activities;
# drink is a dependent variable for the students.
# Get some basic description:
G160_leisb_data
print01Report(G160_leisb_data, modelname = "G160_leisure_symm")

# To estimate, we first define the algorithm object
algo.b <- sienaAlgorithmCreate(projname='G160_leisure_symm', seed=54321, cond=FALSE,
						modelType=2)
# modelType = 2 is the forcing model, which puts the action in the first node set only.

# Define the effects object
(G160_leisb_eff <- getEffects(G160_leisb_data))
# We have to exclude the transitivity effect because it is meaningless here:
G160_leisb_eff <- includeEffects(G160_leisb_eff, transTriads, include=FALSE)
# A major thing that we have to do is to define
# the activities as non-active nodes for the leisure network and for drink
G160_leisb_eff <- setEffect(G160_leisb_eff,RateX, type='rate', name='leisure.b',
					interaction1='activity', initialValue=-100, fix=TRUE)
G160_leisb_eff <- setEffect(G160_leisb_eff,RateX, type='rate', name='drink',
					interaction1='activity', initialValue=-100, fix=TRUE)
# Likewise we define the students as non-active nodes for g.act
G160_leisb_eff <- setEffect(G160_leisb_eff,RateX, type='rate', name='g.act',
					interaction1='student', initialValue=-100, fix=TRUE)
# This means that the activities never take a ministep for leisure or smoke,
# and the students never for g.act,
# as -100 is close enough to minus infinity.
G160_leisb_eff

# Make a basic estimation:
(G160_leisure.b0 <- siena07(algo.b, data=G160_leisb_data,
				effects=G160_leisb_eff, useCluster=TRUE))
# add useCluster=TRUE, nbrNodes= something if you have enough processors; e.g., 2 or 5.


# Now add some meaningful effects
# All network effects must be interacted with egoX student.
# I think the similar thing is not necessary for the behavior effects
# because of the use of NA;
# the difference is that network ties are duplicated,
# but behavior values are not.

# hack (This will be corrected...)
G160_leisb_eff$interactionType[G160_leisb_eff$shortName=='egoX'] <- 'ego'

G160_leisb_eff1 <- includeInteraction(G160_leisb_eff, outAct, egoX, interaction1=c('','student'))
#G160_leisb_eff1 <- setEffect(G160_leisb_eff1,outInAss, parameter=1)
#G160_leisb_eff1 <- setEffect(G160_leisb_eff1,cycle4ND, parameter=1)
# and use them for estimating some model.

(G160_leisure.b1 <- siena07(algo.b, data=G160_leisb_data,
				effects=G160_leisb_eff1, prevAns=G160_leisure.b0))
# Generally, if you would like to start without a previous results object,
# it will be better to start with prevAns=G160_leisure.b0,
# which can be regarded as having better basic starting values than the default ones.

G160_leisb_eff2 <- includeInteraction(G160_leisb_eff1, inPop, egoX, interaction1=c('','student'))

# Now you can pursue this to your heart's desire.

#G160_leisb_eff1 <- setEffect(G160_leisb_eff1,outInAss, parameter=1)
#G160_leisb_eff1 <- setEffect(G160_leisb_eff1,cycle4ND, parameter=1)
# and use them for estimating some model.

(G160_leisure.b2 <- siena07(algo.b, data=G160_leisb_data,
				effects=G160_leisb_eff2, prevAns=G160_leisure.b0))