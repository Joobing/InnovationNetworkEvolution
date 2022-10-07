#####################################################################
# R script for illustrating the analysis of bipartite data          #
# Authors: Christian Steglich and Tom Snijders                      #
# Version: September 24, 2014                                       #
#####################################################################

# load RSiena:
library(RSiena)

# The data set is in the file Glasgow_bip_data.zip, available as
# http://www.stats.ox.ac.uk/~snijders/siena/Glasgow_bip_data.zip.
# First extract this zip file in a directory.
# set working directory to where the data are:
setwd("D:/data")
list.files()

# read the friendship data:
friendship1 <- as.matrix(read.table("glasgow/Glasgow-1-net.dat"))
friendship2 <- as.matrix(read.table("glasgow/Glasgow-2-net.dat"))

# now demographic info:
demographics <- as.matrix(read.table("glasgow/Glasgow-sex-birthyear.dat"))

# now the bipartite data:
leisure1 <- as.matrix(read.table("glasgow/GL-1-lsr.dat"))
leisure2 <- as.matrix(read.table("glasgow/GL-2-lsr.dat"))
music1 <- as.matrix(read.table("glasgow/GL-1-mus.dat"))
music2 <- as.matrix(read.table("glasgow/GL-2-mus.dat"))
drugs1 <- as.matrix(read.table("glasgow/GL-1-drg.dat"))
drugs2 <- as.matrix(read.table("glasgow/GL-2-drg.dat"))

# recode into meaningful affiliation info:
leisure1[leisure1 %in% c(2,3,4)] <- 0
# leisure1[leisure1 == 2] <- 1
leisure2[leisure2 %in% c(2,3,4)] <- 0
# leisure2[leisure2 == 2] <- 1
drugs1[drugs1 %in% c(1)] <- 0
drugs1[drugs1 %in% c(2,3,4)] <- 1
drugs2[drugs2 %in% c(1)] <- 0
drugs2[drugs2 %in% c(2,3,4)] <- 1

# also recode valued friendship data & identify missings:
friendship1[friendship1 == 2] <- 1
friendship1[friendship1 == 9] <- NA
friendship2[friendship2 == 2] <- 1
friendship2[friendship2 == 9] <- NA

# Note that demographic info contains just one missing birth year.
# Because we don't use this variable (but sex only), no recoding here.

# find out number of nodes in nodesets,
# and number of items in scales:
(nrpupils <- dim(leisure1)[1])
(nrleisureItems <- dim(leisure1)[2])
(nrmusicItems <- dim(music1)[2])
(nrdrugsItems <- dim(drugs1)[2])

# Some descriptives: affiliation proportions
colMeans(leisure1)
colMeans(leisure2)
colMeans(music1)
colMeans(music2)
colMeans(drugs1)
colMeans(drugs2)

# define different node sets:
pupils <- sienaNodeSet(nrpupils, nodeSetName="pupils")
leisureItems <- sienaNodeSet(nrleisureItems, nodeSetName="leisureItems")
musicItems <- sienaNodeSet(nrmusicItems, nodeSetName="musicItems")
drugsItems <- sienaNodeSet(nrdrugsItems, nodeSetName="drugsItems")

# identify dependent variables for the analysis;
# start with bipartite network:
leisure <- sienaDependent(array(c(leisure1, leisure2),
				dim=c(nrpupils, nrleisureItems,2)),
				"bipartite", nodeSet=c("pupils", "leisureItems"))
music <- sienaDependent(array(c(music1, music2),
				dim=c(nrpupils, nrmusicItems,2)),
				"bipartite", nodeSet=c("pupils", "musicItems"))
drugs <- sienaDependent(array(c(drugs1, drugs2),
				dim=c(nrpupils, nrdrugsItems,2)),
				"bipartite", nodeSet=c("pupils", "drugsItems"))
# now add the one-mode network:
friendship <- sienaDependent(array(c(friendship1, friendship2),
				dim=c(nrpupils, nrpupils,2)), nodeSet="pupils")

# include exogenous predictor variables:
sex.F <- coCovar(demographics[, 1], nodeSet="pupils")

# combine data for the analysis:
bipData <- sienaDataCreate(friendship, leisure, music, drugs, sex.F,
 nodeSets=list(pupils, leisureItems, musicItems, drugsItems))

# The analysis of all this considerable data set can be time-consuming.
# Of course there is also the possibility to use less than 3 (e.g., 1)
# of the 2-mode networks.

# get effects table for model specification:
bipEffects <- getEffects(bipData)

# generate initial descriptive outputfile:
print01Report(bipData, modelname="Bipartite-illustration")

# Take a look at the generated output file "Bipartite-illustration.out"
# to see how RSiena interpreted the directives provided
# and to get an impression of the data (eyeball the degrees,
# look at occurrence of missings, look at Jaccard indices).

# Specify the model:
bipEffects <- includeEffects(bipEffects, transTrip, name="friendship")
bipEffects <- includeEffects(bipEffects, from, name="friendship",
							interaction1="leisure")
bipEffects <- includeEffects(bipEffects, from, name="friendship",
							interaction1="music")
bipEffects <- includeEffects(bipEffects, from, name="friendship",
							interaction1="drugs")
bipEffects <- includeEffects(bipEffects, cycle4, name="leisure")
bipEffects <- includeEffects(bipEffects, cycle4, name="music")
bipEffects <- includeEffects(bipEffects, cycle4, name="drugs")
bipEffects <- includeEffects(bipEffects, to, name="leisure",
							 interaction1="friendship")
bipEffects <- includeEffects(bipEffects, to, name="music",
							 interaction1="friendship")
bipEffects <- includeEffects(bipEffects, to, name="drugs",
							 interaction1="friendship")
bipEffects


# create algorithm object:
bipModel <- sienaAlgorithmCreate(projname='bipartite-Glasgow-results')

# estimate model:
bipResults <- siena07(bipModel, data=bipData, effects=bipEffects)
bipResults

# Perhaps convergence is not satisfactory yet, so the model might be
# estimated in a further step:
bipResults <- siena07(bipModel, data=bipData, effects=bipEffects,
							prevAns=bipResults, returnDeps=TRUE)
bipResults

# store results for possible later use (because computations take so long)
save.image("bipResults1.RData")

bipEffects <- includeEffects(bipEffects, simX, name="friendship",
							interaction1="sex.F")

bipResults2 <- siena07(bipModel, data=bipData, effects=bipEffects,
							prevAns=bipResults, returnDeps=TRUE)
bipResults2

bipEffects <- includeEffects(bipEffects, outActSqrt, name="leisure")
bipEffects <- includeEffects(bipEffects, outActSqrt, name="music")
bipEffects <- includeEffects(bipEffects, outActSqrt, name="drugs")
bipEffects <- includeEffects(bipEffects, inPopSqrt, name="leisure")
bipEffects <- includeEffects(bipEffects, inPopSqrt, name="music")
bipEffects <- includeEffects(bipEffects, inPopSqrt, name="drugs")


bipEffects

bipResults3 <- siena07(bipModel, data=bipData,
         effects=bipEffects, prevAns=bipResults2, returnDeps=TRUE)
bipResults3

save.image("bipResults3.RData")


bipEffects <- includeInteraction(bipEffects, outActIntn, egoX,
  name="friendship", interaction1=c("leisure", "sex.F"))

bipEffects <- includeEffects(bipEffects, egoX,
  name="friendship", interaction1="sex.F")

bipEffects <- includeEffects(bipEffects, outActIntn,
  name="friendship", interaction1="leisure")
bipEffects

bipResults4 <- siena07(bipModel, data=bipData,
         effects=bipEffects, prevAns=bipResults3, returnDeps=TRUE)
bipResults4

# Save results because it was time-consuming to get them:
# save.image("bipResults4.RData")

# Now let us assess goodness of fit for the three estimated models.
# This is done separately for the various dependent variables
# as indicated by the varName parameter in sienaGOF.

gofo1f <- sienaGOF(bipResults, OutdegreeDistribution, join=TRUE,
                    varName="friendship")
gofo2f <- sienaGOF(bipResults2, OutdegreeDistribution, join=TRUE,
                    varName="friendship")
gofo3f <- sienaGOF(bipResults3, OutdegreeDistribution, join=TRUE,
                    varName="friendship")
gofo4f <- sienaGOF(bipResults4, OutdegreeDistribution, join=TRUE,
                    varName="friendship")
# no good!

gofi1f <- sienaGOF(bipResults, IndegreeDistribution, join=TRUE,
                    varName="friendship")
gofi2f <- sienaGOF(bipResults2, IndegreeDistribution, join=TRUE,
                    varName="friendship")
gofi3f <- sienaGOF(bipResults3, IndegreeDistribution, join=TRUE,
                    varName="friendship")
gofi4f <- sienaGOF(bipResults4, IndegreeDistribution, join=TRUE,
                    varName="friendship")
# good from the start

gofo1l <- sienaGOF(bipResults, OutdegreeDistribution, join=TRUE,
                    varName="leisure")
gofo2l <- sienaGOF(bipResults2, OutdegreeDistribution, join=TRUE,
                    varName="leisure")
gofo3l <- sienaGOF(bipResults3, OutdegreeDistribution, join=TRUE,
                    varName="leisure")
gofo4l <- sienaGOF(bipResults4, OutdegreeDistribution, join=TRUE,
                    varName="leisure")
# all of them about 0.001

gofi1l <- sienaGOF(bipResults, IndegreeDistribution, join=TRUE,
                    varName="leisure")
gofi2l <- sienaGOF(bipResults2, IndegreeDistribution, join=TRUE,
                    varName="leisure")
gofi3l <- sienaGOF(bipResults3, IndegreeDistribution, join=TRUE,
                    varName="leisure")
gofi4l <- sienaGOF(bipResults4, IndegreeDistribution, join=TRUE,
                    varName="leisure")
# good for models 3 and especially 4

gofo1m <- sienaGOF(bipResults, OutdegreeDistribution, join=TRUE,
                    varName="music")
gofo2m <- sienaGOF(bipResults2, OutdegreeDistribution, join=TRUE,
                    varName="music")
gofo3m <- sienaGOF(bipResults3, OutdegreeDistribution, join=TRUE,
                    varName="music")
gofo4m <- sienaGOF(bipResults4, OutdegreeDistribution, join=TRUE,
                    varName="music")
# models 3 and 4 about 0.03

gofi1m <- sienaGOF(bipResults, IndegreeDistribution, join=TRUE,
                    varName="music")
gofi2m <- sienaGOF(bipResults2, IndegreeDistribution, join=TRUE,
                    varName="music")
gofi3m <- sienaGOF(bipResults3, IndegreeDistribution, join=TRUE,
                    varName="music")
gofi4m <- sienaGOF(bipResults4, IndegreeDistribution, join=TRUE,
                    varName="music")
# all good

gofo1d <- sienaGOF(bipResults, OutdegreeDistribution, join=TRUE,
                    varName="drugs")
gofo2d <- sienaGOF(bipResults2, OutdegreeDistribution, join=TRUE,
                    varName="drugs")
gofo3d <- sienaGOF(bipResults3, OutdegreeDistribution, join=TRUE,
                    varName="drugs")
gofo4d <- sienaGOF(bipResults4, OutdegreeDistribution, join=TRUE,
                    varName="drugs")
# all good

gofi1d <- sienaGOF(bipResults, IndegreeDistribution, join=TRUE,
                    varName="drugs")
gofi2d <- sienaGOF(bipResults2, IndegreeDistribution, join=TRUE,
                    varName="drugs")
gofi3d <- sienaGOF(bipResults3, IndegreeDistribution, join=TRUE,
                    varName="drugs")
gofi4d <- sienaGOF(bipResults4, IndegreeDistribution, join=TRUE,
                    varName="drugs")
# all very good.

# To conclude: models 3 and 4 are the best,
# but need improvement for the fit of the outdegree distribution 
# for friendship and leisure.

save.image("bipResults4.RData")