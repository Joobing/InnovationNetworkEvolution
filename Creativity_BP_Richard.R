#LOADING data and libraries####
ddir<-'d:/#CREATIVITY/'#IMPORTANT! specify your data folder here####
getwd()
setwd(ddir)
load(file.path(ddir, "bip.RData"))

library(RSiena)
library(igraph)
sienain<-function(lst, const, lbl)  #lst=crtv  #const="nconst"  #lbl="nominees"
{require(tidyverse)
  output<-setNames(reduce(lst, full_join, by = const) %>% replace(., is.na(.), 0) , c(const, paste(lbl,"1", sep = ""), paste(lbl,"2", sep = ""),  paste(lbl,"3", sep = "")))
  mss<-nms[!(nms %in% output[[const]])]
  output<-rbind(output,setNames(data.frame(mss, integer(length(mss)), integer(length(mss)), integer(length(mss)))  , c(const, paste(lbl,"1", sep = ""), paste(lbl,"2", sep = ""),  paste(lbl,"3", sep = ""))))
  output<-output[order(output[[const]]),]  
  row.names(output) <- NULL
  #output<-as(as.matrix(output[,2:4]), "dgTMatrix")
  output<-as.matrix(output[,2:4])
  return(output)
}


#WAVES####
#1- edgelists (bimodal)####
film1<-rbind(link1995,link1996,link1997)
film2<-rbind(link1996,link1997,link1998)
film3<-rbind(link1997,link1998,link1999)

nms<-Reduce(intersect, list(unique(link1995$nconst),unique(link1997$nconst),unique(link1999$nconst)))
#nms<-sample(nms, 200, replace = FALSE, prob = NULL)

film1<-film1[film1$nconst%in% nms, ]
film2<-film2[film2$nconst%in% nms, ]
film3<-film3[film3$nconst%in% nms, ]
film1<- apply(apply(film1, 1, rev), 1, rev)
film2<- apply(apply(film2, 1, rev), 1, rev)
film3<- apply(apply(film3, 1, rev), 1, rev)
#tt01<-tts[!(tts %in% film1[,2])]
#tt02<-tts[!(tts %in% film2[,2])]
#tt03<-tts[!(tts %in% film3[,2])]

# Getting Node Sets####
film1<-na.omit(film1)
g1<-          graph_from_data_frame(film1, directed = FALSE)

film2<-na.omit(film2)
g2<-          graph_from_data_frame(film2, directed = FALSE)

film3<-na.omit(film3)
g3<-          graph_from_data_frame(film3, directed = FALSE)


xv1<-unique(c(V(g2)[!(V(g2)$name %in% V(g1)$name)]$name, V(g3)[!(V(g3)$name %in% V(g1)$name)]$name))
g1 <- add.vertices(g1,nv=length(xv1),attr=list(name=xv1))
xv2<-unique(c(V(g1)[!(V(g1)$name %in% V(g2)$name)]$name, V(g3)[!(V(g3)$name %in% V(g2)$name)]$name))
g2 <- add.vertices(g2,nv=length(xv2),attr=list(name=xv2))
xv3<-unique(c(V(g1)[!(V(g1)$name %in% V(g3)$name)]$name, V(g2)[!(V(g2)$name %in% V(g3)$name)]$name))
g3 <- add.vertices(g3,nv=length(xv3),attr=list(name=xv3))



V(g2)$type<-  V(g2)$name %in% nms
is.bipartite(g2)
V(g1)$type<-  V(g1)$name %in% nms
is.bipartite(g1)
V(g3)$type<-  V(g3)$name %in% nms
is.bipartite(g3)

length(unique(V(g1)))
length(unique(V(g2)))
length(unique(V(g3)))
length(unique(E(g1)))
length(unique(E(g2)))
length(unique(E(g3)))


nms <-V(g1)[(V(g1)$type==TRUE)]$name
tts <-V(g1)[(V(g1)$type==FALSE)]$name



#merge(indv1995,aggregate(sequel ~ nconst, merge(link1995[,c("nconst","tconst")],rtng1995[,c("tconst","sequel")], by="tconst", all.x=T), sum),by="nconst" , all.x=T) #_sequel####

aff1<-as_adj(g1, type = c("both"), attr = NULL, edges = FALSE, names = TRUE, sparse = FALSE)
aff2<-as_adj(g2, type = c("both"), attr = NULL, edges = FALSE, names = TRUE, sparse = FALSE)
aff3<-as_adj(g3, type = c("both"), attr = NULL, edges = FALSE, names = TRUE, sparse = FALSE)

aff1<-t(aff1[ , -which(colnames(aff1) %in% tts)])
aff1<-aff1[order(row.names(aff1)) , -which(colnames(aff1) %in% nms)]
aff1<-aff1[,order(colnames(aff1))]

aff2<-t(aff2[ , -which(colnames(aff2) %in% tts)])
aff2<-aff2[order(row.names(aff2)) , -which(colnames(aff2) %in% nms)]
aff2<-aff2[,order(colnames(aff2))]

aff3<-t(aff3[ , -which(colnames(aff3) %in% tts)])
aff3<-aff3[order(row.names(aff3)) , -which(colnames(aff3) %in% nms)]
aff3<-aff3[,order(colnames(aff3))]

colnames(aff1)
colnames(aff2)
colnames(aff3)
rownames(aff1)
rownames(aff2)
rownames(aff3)
sum(aff1)
sum(aff2)
sum(aff3)

#aff1<-as(aff1, "dgTMatrix")
#aff2<-as(aff2, "dgTMatrix")
#aff3<-as(aff3, "dgTMatrix")
aff1[aff1 > 0] <- 1
aff2[aff2 > 0] <- 1
aff3[aff3 > 0] <- 1


clb1<-as_adj(bipartite.projection(g1)[[2]], type = c("both"), #one-mode of nodes from the first dimensions
             attr = NULL, edges = FALSE, names = TRUE, sparse = FALSE)
clb2<-as_adj(bipartite.projection(g2)[[2]], type = c("both"), attr = NULL, edges = FALSE, names = TRUE, sparse = FALSE)
clb3<-as_adj(bipartite.projection(g3)[[2]], type = c("both"), attr = NULL, edges = FALSE, names = TRUE, sparse = FALSE)
#g<-          set.edge.attribute(g, "weight", E(g), 1)

#clb1<-as(clb1, "dgTMatrix")
#clb2<-as(clb2, "dgTMatrix")
#clb3<-as(clb3, "dgTMatrix")
clb1[clb1 > 0] <- 1
clb2[clb2 > 0] <- 1
clb3[clb3 > 0] <- 1


nrnm <- length(nms) #should be correct for all waves
nrtt <- length(tts)
crew <- sienaNodeSet(nrnm, nodeSetName="crew")
film <- sienaNodeSet(nrtt, nodeSetName="film")
#as_adjacency_matrix(graph, type = c("both", "upper", "lower"),
#                    attr = NULL, edges = FALSE, names = TRUE,
#                    sparse = igraph_opt("sparsematrices"))

gc()
#memory.size(max = TRUE)
#memory.limit(size = 2500000000000)

# Dependent Variables####
# a-bipartite network ####
bip <- sienaDependent(array(c(aff1, aff2, aff3), #order of dimensions in the raw data?
                            dim=c(nrnm, nrtt,3)),
                            "bipartite", nodeSet=c("crew", "film"), sparse=FALSE)

# b-one-mode network####
uni <- sienaDependent(array(c(clb1, clb2, clb3), dim=c(nrnm, nrnm, 3)))#, nodeSet="crew")


# behavioral variables####


indv<-rbind(indv1995, indv1996, indv1997, indv1997, indv1999)
indv<-indv[indv$nconst %in% nms,]
indv1<-indv[indv$year %in% c(1995,1996,1997),]
indv2<-indv[indv$year %in% c(1998,1996,1997),]
indv3<-indv[indv$year %in% c(1998,1999,1997),]


crtv<-list(aggregate(awards ~ nconst, indv1, sum), aggregate(awards ~ nconst, indv2, sum), aggregate(awards ~ nconst, indv3, sum))            
fcgn<-list(aggregate(focus_genres ~ nconst, indv1, mean), aggregate(focus_genres ~ nconst, indv2, mean), aggregate(focus_genres ~ nconst, indv3, mean))            
fcrl<-list(aggregate(focus_roles ~ nconst, indv1, mean), aggregate(focus_roles ~ nconst, indv2, mean), aggregate(focus_roles ~ nconst, indv3, mean))            
gcnf<-list(aggregate(average_confusion ~ nconst, indv1, mean), aggregate(average_confusion ~ nconst, indv2, mean), aggregate(average_confusion ~ nconst, indv3, mean))            
rcnf<-list(aggregate(role_consolidation ~ nconst, indv1, mean), aggregate(role_consolidation ~ nconst, indv2, mean), aggregate(role_consolidation ~ nconst, indv3, mean))            
novc<-list(aggregate(newcomer ~ nconst, indv1, max), aggregate(newcomer ~ nconst, indv2, max), aggregate(newcomer ~ nconst, indv3, max))            
xprc<-list(aggregate(experience ~ nconst, indv1, sum), aggregate(experience ~ nconst, indv2, sum), aggregate(experience ~ nconst, indv3, sum))
bdgt<-list(aggregate(budget ~ nconst, indv1, sum), aggregate(budget ~ nconst, indv2, sum), aggregate(budget ~ nconst, indv3, sum))            
rcpt<-list(aggregate(metascore ~ nconst, indv1, mean), aggregate(metascore ~ nconst, indv2, mean), aggregate(metascore ~ nconst, indv3, mean))            
Comd<-list(aggregate(Comedy ~ nconst, indv1, max), aggregate(Comedy ~ nconst, indv2, max), aggregate(Comedy ~ nconst, indv3, max))            

#birthdate

crtv<-sienain(crtv, "nconst", "nominations")
fcgn<-sienain(fcgn, "nconst", "focus_type")
fcrl<-sienain(fcrl, "nconst", "focus_roles")
gcnf<-sienain(gcnf, "nconst", "genre_confusion")
rcnf<-sienain(rcnf, "nconst", "role_confusion")
novc<-sienain(novc, "nconst", "novice")
xprc<-sienain(xprc, "nconst", "experience")
bdgt<-sienain(bdgt, "nconst", "budget")
rcpt<-sienain(rcpt, "nconst", "rating")
Comd<-sienain(Comd, "nconst", "Comd")
Comedy= Comd[,"Comd1"]+Comd[,"Comd2"]+Comd[,"Comd3"]
Comedy[Comedy > 0] <- 1

crtv[crtv > 1] <- 3


# require(tidyverse)
# crtv<-setNames(reduce(lst, full_join, by = "nconst") %>% replace(., is.na(.), 0) , c("nconst", "awards.1", "awards.2",  "awards.3"))
# mss<-nms[!(nms %in% crtv$nconst)]
# crtv<-rbind(crtv,setNames(data.frame(mss, integer(length(mss)), integer(length(mss)), integer(length(mss)))  , c("nconst", "awards.1", "awards.2",  "awards.3")))
# crtv<-crtv[order(crtv$nconst),]  
# row.names(crtv) <- NULL#crtv<-as(as.matrix(crtv[,2:4]), "dgTMatrix")
# crtv<-as.matrix(crtv[,2:4])

#colnames(crtv)
#c("awards", "focus_roles", "focus_genres", "average_confusion", "quality", "metascore")]

# Also creativity is modeled as dependent (behavioral) variable
creativity <- sienaDependent(crtv, type = "behavior")#, nodeSet="crew")


# combine data for the analysis####
# include exogenous predictor variables:



#link1995<-link1995[, c("tconst", "nconst")]#link1996<-link1996[, c("tconst", "nconst")]#link1997<-link1997[, c("tconst", "nconst")]#link1998<-link1998[, c("tconst", "nconst")]#link1999<-link1999[, c("tconst", "nconst")]#save(uni, bip, creativity,crew, film, link1995, link1996, link1997, link1998, link1999, indv1995, indv1996, indv1997, indv1998, indv1999, file = "bip.RData")

require(RSiena)



# Define the data set
#myCoEvolutionData <- sienaDataCreate(friendship, drinkingbeh, smoke)

focus_type <- varCovar(fcgn)#, nodeSet="crew")
focus_role <- varCovar(fcrl)#, nodeSet="crew")
conf_type <- varCovar(gcnf)#, nodeSet="crew")
conf_role <- varCovar(rcnf)#, nodeSet="crew")
#crtv_cov <- varCovar(crtv, nodeSet="crew")
comedy <- coCovar(Comedy)#, nodeSet="crew")

bipData <- sienaDataCreate(uni, 
                           bip, creativity, focus_type, focus_role, conf_role, conf_type, #crtv_cov, comedy, 
                           nodeSets=list(crew, film)
                           )

uniData <- sienaDataCreate(uni, creativity, focus_type, focus_role, conf_role, conf_type, comedy)#, nodeSets=list(crew))

#BD <- coCovar(Birthyear_data, nodeSet="crew")

# Effects####
bipEffects <- getEffects(bipData)

print01Report(bipData, modelname="Bipartite-illustration1")


#SELECTION####

#a-bipartite####

bipEffects <- includeEffects(bipEffects, cycle4, name="bip")
#bipEffects <- includeEffects(bipEffects, outActSqrt, name="bip")
#bipEffects <- includeEffects(bipEffects, inPopSqrt, name="bip")


#You need the unipartite network to define the following:####

bipEffects <- includeEffects(bipEffects, simX, name="bip",
                             interaction1="comedy", nodeSet="crew")


bipEffects <- includeEffects(bipEffects, simX, name="bip",
                             interaction1="focus_type", nodeSet="crew")

bipEffects <- includeEffects(bipEffects, simX, name="bip",
                             interaction1="focus_type")

bipEffects <- includeEffects(bipEffects, simX, name="bip",
                             interaction1="creativity")

bipEffects <- includeEffects(bipEffects, name = "bip",
                                   indeg, outdeg, interaction1 = "conf_type")


#b-crossmode####
bipEffects <- includeEffects(bipEffects, from, name="uni", interaction1="bip")
bipEffects <- includeEffects(bipEffects, to, name="bip", interaction1="uni")



#a-unipartite####


bipEffects <- includeEffects(bipEffects, simX, name="uni",
                             interaction1="focus_type")

bipEffects <- includeEffects(bipEffects, name = "uni",
                             egoX,interaction1 = "conf_type")# altX, simX, 

bipEffects <- includeEffects(bipEffects, name = "uni",
                             egoX, interaction1 = "conf_role")#altX, simX, 

bipEffects <- includeEffects(bipEffects, simX, name="uni",#egoX, altX, 
                             interaction1="comedy", nodeSet="crew")




uniEffects <- getEffects(uniData)
uniEffects <- includeEffects(uniEffects, transTrip)#, name="uni")

uniEffects <- includeEffects(uniEffects, cycle4)#, name="uni")

uniEffects <- includeEffects(uniEffects, simX, name="uni",
                             interaction1="focus_type")

uniEffects <- includeEffects(uniEffects, name = "uni",
                             egoX,interaction1 = "conf_type")# altX, simX, 

uniEffects <- includeEffects(uniEffects, name = "uni",
                             egoX, interaction1 = "conf_role")#altX, simX, 

uniEffects <- includeEffects(uniEffects, simX, name="uni",#egoX, altX, 
                             interaction1="comedy", nodeSet="crew")



uniEffects <- includeEffects(uniEffects, altX, simX, name="uni", #altX, 
                             interaction1 = "creativity" )

uniEffects <- includeEffects(uniEffects, name = "creativity", indeg, outdeg, avSim, 
                             interaction1 = "uni")
#2- INFLUENCE####
# If we want to parse out whether there is a selection or influence (or both)
# effect for creative behaviour,  we need to also include sender, receiver 
# and homophily effects of creativity for collaboration formation:

bipEffects <- includeEffects(bipEffects, egoX, altX, simX, name="uni", 
                             interaction1 = "creativity" )



#bipEffects <- includeEffects(bipEffects, altX, simX, name="uni", 
 #                            interaction1 = "creativity" )

# For the influence part, i.e. the effect of the network on creativity,
# we specify the following effects:
# indegree, outdegree and assimilation effects for focus

bipEffects <- includeEffects(bipEffects, name = "creativity", indeg, outdeg, avSim, 
                             interaction1 = "uni")


#!!!!####
# There is no effect with short name indeg, 
# and with interaction1 = <uni>, interaction2 = <>, and type = <eval>, 
# for dependent variable creativity .
# See effectsDocumentation() for this effects object.

# Behavioral effects####

# bipEffects <- includeEffects(bipEffects, name = "creativity", effFrom, interaction1 = "focus" )

# ESTIMATE####
gc()

# create algorithm object:
bipModel <- sienaAlgorithmCreate(projname='Project-level Creativity')
uniModel <- sienaAlgorithmCreate(projname='Project-level Creativity')


bipResults2 <- siena07(bipModel, data=bipData, effects=bipEffects)#,useCluster = T, nbrNodes = 3)
uniResults1 <- siena07(uniModel, data=uniData, effects=uniEffects)#,useCluster = T, nbrNodes = 3)


# To look at the results, type
bipResults2

# or, somewhat more extensive,
summary(uniResults1)

# Note (again) that the "convergence t-ratio" is the t-ratio for convergence checking,
# not the t statistic for testing the significance of this effect.

# We can print the table now!
siena.table(uniResults1, type="html", tstatPrint=T, sig=T)

  save.image("uniResults1.RData")
  
  # [???17-???8-???2020 4:25]  Benton, Richard:  
    bipEff_1a <- includeInteraction(bipEff_1a, name = "interlocks", egoX, altInDist2, interaction1 = c("GovernanceBeh", "GovernanceBeh"), type="creation") 
  GovernanceBeh <- sienaDependent(as.matrix(G_index), type= "behavior",  nodeSet="firms") 
  
  # [???17-???8-???2020 4:29]  Benton, Richard:  
    bipEff_1a <- includeEffects(bipEff_1a, name = "GovernanceBeh", popAlt, outdeg, avInAltDist2, interaction1 = "interlocks") 
    
    

