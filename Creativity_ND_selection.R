#LOADINGS####

#IMPORTANT! specify your data folder here#
ddir<-'d:/#CREATIVITY/'
setwd('D:/#SIENA')
getwd()

load(file.path("bip.RData"))

library(RSiena)
library(igraph)

#????????????????????????????????????????????????????????????????????????????????????????####
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


#NETWORK DATA####

#>waves####
# edgelists (bimodal)#
film1<-rbind(link1995,link1996)
film2<-rbind(link1996,link1997)
film3<-rbind(link1997,link1998)

#>active individuals ####
#nms<-Reduce(intersect, list(unique(c(indv1995$nconst, indv1996$nconst)),unique(c( indv1996$nconst, indv1997$nconst)),unique(c(indv1997$nconst,indv1998$nconst)))) 
nms<-Reduce(intersect, list(unique(link1995$nconst),unique(link1997$nconst),unique(link1999$nconst))) #nms<-unique(indv1997$nconst) 


#!!!RANDOM PICK ####
#nms<-sample(nms, 200, replace = FALSE, prob = NULL)

#!!!keep only those who are active in all waves (listed in nms above)#####
film1<-film1[film1$nconst%in% nms, ]
film2<-film2[film2$nconst%in% nms, ]
film3<-film3[film3$nconst%in% nms, ]

#swap column order into ["nms" "tts"] for ease of use in next parts
film1<- apply(apply(film1, 1, rev), 1, rev)
film2<- apply(apply(film2, 1, rev), 1, rev)
film3<- apply(apply(film3, 1, rev), 1, rev)#tt01<-tts[!(tts %in% film1[,2])]#tt02<-tts[!(tts %in% film2[,2])]#tt03<-tts[!(tts %in% film3[,2])]

# Getting Node Sets
film1<-na.omit(film1)
film2<-na.omit(film2)
film3<-na.omit(film3)

g1<-          graph_from_data_frame(film1, directed = FALSE)
g2<-          graph_from_data_frame(film2, directed = FALSE)
g3<-          graph_from_data_frame(film3, directed = FALSE)

# Add inactive vertices: for each network (i) identify and add vertices that are not listed (in network i) but appear in other networks (xvi)
xv1<-unique(c(V(g2)[!(V(g2)$name %in% V(g1)$name)]$name, V(g3)[!(V(g3)$name %in% V(g1)$name)]$name))
g1 <- add.vertices(g1,nv=length(xv1),attr=list(name=xv1))
xv2<-unique(c(V(g1)[!(V(g1)$name %in% V(g2)$name)]$name, V(g3)[!(V(g3)$name %in% V(g2)$name)]$name))
g2 <- add.vertices(g2,nv=length(xv2),attr=list(name=xv2))
xv3<-unique(c(V(g1)[!(V(g1)$name %in% V(g3)$name)]$name, V(g2)[!(V(g2)$name %in% V(g3)$name)]$name))
g3 <- add.vertices(g3,nv=length(xv3),attr=list(name=xv3))

#assign modes
V(g1)$type<-  V(g1)$name %in% nms
V(g2)$type<-  V(g2)$name %in% nms
V(g3)$type<-  V(g3)$name %in% nms
is.bipartite(g1)
is.bipartite(g2)
is.bipartite(g3)

g1<-bipartite.projection(g1)
g2<-bipartite.projection(g2)
g3<-bipartite.projection(g3)

g1<-g1[[2]]
g2<-g2[[2]]
g3<-g3[[2]]


# >net summary####
#same number of vertices, varying nummber of edges
length(unique(V(g1)))
length(unique(V(g2)))
length(unique(V(g3)))
length(unique(E(g1)))
length(unique(E(g2)))
length(unique(E(g3)))

#to avoid error in #ordering (below) replace text list of names with (identical) vertice lables list
nms<-sort(nms)
nms <-V(g1)$name

#merge(indv1995,aggregate(sequel ~ nconst, merge(link1995[,c("nconst","tconst")],rtng1995[,c("tconst","sequel")], by="tconst", all.x=T), sum),by="nconst" , all.x=T) #_sequel#

#extract affiliation/adjacancy matrixes
aff1<-as_adj(g1, type = c("both"), attr = NULL, edges = FALSE, names = TRUE, sparse = FALSE)
isSymmetric(aff1, check.attributes = FALSE)

aff2<-as_adj(g2, type = c("both"), attr = NULL, edges = FALSE, names = TRUE, sparse = FALSE)
isSymmetric(aff2, check.attributes = FALSE)

aff3<-as_adj(g3, type = c("both"), attr = NULL, edges = FALSE, names = TRUE, sparse = FALSE)
isSymmetric(aff3, check.attributes = FALSE)

#ordering

aff1<-aff1[,order(colnames(aff1))]
aff1<-aff1[order(rownames(aff1)),]
aff2<-aff2[,order(colnames(aff2))]
aff2<-aff2[order(rownames(aff2)),]
aff3<-aff3[,order(colnames(aff3))]
aff3<-aff3[order(rownames(aff3)),]


colnames(aff1)[1:10]
colnames(aff2)[1:10]
colnames(aff3)[1:10]
rownames(aff1)[1:10]
rownames(aff2)[1:10]
rownames(aff3)[1:10]
sum(aff1)
sum(aff2)
sum(aff3)

#recoding
aff1[aff1 > 0] <- 1
aff2[aff2 > 0] <- 1
aff3[aff3 > 0] <- 1 #aff1<-as(aff1, "dgTMatrix") #aff2<-as(aff2, "dgTMatrix") #aff3<-as(aff3, "dgTMatrix")

#CREATE NODESETS####
crew <- sienaNodeSet(length(nms), nodeSetName="crew", names= rownames(aff1)) #number of nodes should be correct for all waves


gc()
memory.size(max = TRUE)
memory.limit(size = 2500000000000)

# >DV####
nd <- sienaDependent(array(c(aff1, aff2, aff3), 
                            dim=c(length(nms), length(nms),3)),
                      "oneMode", nodeSet=c("crew"), sparse=FALSE)

#????????????????????????????????????????????????????????????????????????????????????????####
#order of dimensions in the raw data?
#


# BEHAVIORAL DATA####

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#####
#see comment A in your notes from meeting with Joris
#codebook
#add birthdate
##Did you sort indv before using sienain


#Subsetting behavioral data based on the sample and waves
indv<-rbind(indv1995, indv1996, indv1997, indv1998)
indv<-indv[indv$nconst %in% nms,]
indv<-setorder(indv,"nconst")


indv1<-indv[indv$year %in% c(1995,1996),]
indv1<-setorder(indv1,"nconst")
indv2<-indv[indv$year %in% c(1996,1997),]
indv2<-setorder(indv2,"nconst")
indv3<-indv[indv$year %in% c(1997,1998),]
indv3<-setorder(indv3,"nconst")

# , aggregate
#, by="nconst", all.x = TRUE), merge(data.frame(nconst=unlist(nms)), aggregate

crtv<-list(aggregate(awards ~ nconst, indv1, sum),aggregate(awards ~ nconst, indv2, sum),aggregate(awards ~ nconst, indv3, sum)) # crtv<-list(merge(data.frame(nconst=unlist(nms)), aggregate(awards ~ nconst, indv1, sum), by="nconst", all.x = TRUE),             merge(data.frame(nconst=unlist(nms)), aggregate(awards ~ nconst, indv2, sum), by="nconst", all.x = TRUE),             merge(data.frame(nconst=unlist(nms)), aggregate(awards ~ nconst, indv3, sum), by="nconst", all.x = TRUE)            )    # merged with nms to keep all nodes and have them sorted similarly across all variables    
styr<-indv[match(unique(indv$nconst), indv$nconst),"startYear"]

#gcnf<-list(merge(data.frame(nconst=unlist(nms)), aggregate(average_confusion ~ nconst, indv1, mean), by="nconst", all.x = TRUE), merge(data.frame(nconst=unlist(nms)), aggregate(average_confusion ~ nconst, indv2, mean), by="nconst", all.x = TRUE), merge(data.frame(nconst=unlist(nms)), aggregate(average_confusion ~ nconst, indv3, mean), by="nconst", all.x = TRUE))            
#rlcn<-list(merge(data.frame(nconst=unlist(nms)), aggregate(role_consolidation ~ nconst, indv1[indv1$year == 1995,], mean), by="nconst", all.x = TRUE), merge(data.frame(nconst=unlist(nms)), aggregate(role_consolidation ~ nconst, indv2[indv1$year == 1996,], mean), by="nconst", all.x = TRUE), merge(data.frame(nconst=unlist(nms)), aggregate(role_consolidation ~ nconst, indv3[indv1$year == 1997,], mean), by="nconst", all.x = TRUE))            
#novc<-list(merge(data.frame(nconst=unlist(nms)), aggregate(newcomer ~ nconst, indv1, max), by="nconst", all.x = TRUE), merge(data.frame(nconst=unlist(nms)), aggregate(newcomer ~ nconst, indv2, max), by="nconst", all.x = TRUE), merge(data.frame(nconst=unlist(nms)), aggregate(newcomer ~ nconst, indv3, max), by="nconst", all.x = TRUE))            
#xprc<-list(merge(data.frame(nconst=unlist(nms)), aggregate(experience ~ nconst, indv1[indv1$year == 1995,], mean), by="nconst", all.x = TRUE), merge(data.frame(nconst=unlist(nms)), aggregate(experience ~ nconst, indv2[indv1$year == 1996,], mean), by="nconst", all.x = TRUE), merge(data.frame(nconst=unlist(nms)), aggregate(experience ~ nconst, indv3[indv1$year == 1997,], mean), by="nconst", all.x = TRUE))                  
#bdgt<-list(merge(data.frame(nconst=unlist(nms)), aggregate(budget ~ nconst, indv1, sum), by="nconst", all.x = TRUE), merge(data.frame(nconst=unlist(nms)), aggregate(budget ~ nconst, indv2, sum), by="nconst", all.x = TRUE), merge(data.frame(nconst=unlist(nms)), aggregate(budget ~ nconst, indv3, sum), by="nconst", all.x = TRUE))                        
#rcpt<-list(merge(data.frame(nconst=unlist(nms)), aggregate(metascore ~ nconst, indv1, mean), by="nconst", all.x = TRUE), merge(data.frame(nconst=unlist(nms)), aggregate(metascore ~ nconst, indv2, mean), by="nconst", all.x = TRUE), merge(data.frame(nconst=unlist(nms)), aggregate(metascore ~ nconst, indv3, mean), by="nconst", all.x = TRUE))                      
#Comd<-list(merge(data.frame(nconst=unlist(nms)), aggregate(Comedy ~ nconst, indv1, max), by="nconst", all.x = TRUE), merge(data.frame(nconst=unlist(nms)), aggregate(Comedy ~ nconst, indv2, max), by="nconst", all.x = TRUE), merge(data.frame(nconst=unlist(nms)), aggregate(Comedy ~ nconst, indv3, max), by="nconst", all.x = TRUE))                       


crtv<-sienain(crtv, "nconst", "nominations")

#gcnf<-sienain(gcnf, "nconst", "genre_confusion")
#rlcn<-sienain(rlcn, "nconst", "role_consolidation")
#xprc<-sienain(xprc, "nconst", "experience")
#bdgt<-sienain(bdgt, "nconst", "budget")
#rcpt<-sienain(rcpt, "nconst", "rating")
#Comd<-sienain(Comd, "nconst", "Comd")
#Comedy= Comd[,"Comd1"]+Comd[,"Comd2"]+Comd[,"Comd3"]
#Comedy[Comedy > 0] <- 1

#crtv[crtv > 3] <- 3 # require(tidyverse)# crtv<-setNames(reduce(lst, full_join, by = "nconst") %>% replace(., is.na(.), 0) , c("nconst", "awards.1", "awards.2",  "awards.3"))# mss<-nms[!(nms %in% crtv$nconst)]# crtv<-rbind(crtv,setNames(data.frame(mss, integer(length(mss)), integer(length(mss)), integer(length(mss)))  , c("nconst", "awards.1", "awards.2",  "awards.3")))# crtv<-crtv[order(crtv$nconst),]  # row.names(crtv) <- NULL#crtv<-as(as.matrix(crtv[,2:4]), "dgTMatrix")# crtv<-as.matrix(crtv[,2:4])#colnames(crtv)#c("awards", "focus_roles", "focus_genres", "average_confusion", "quality", "metascore")]

# >DV####
creativity <- sienaDependent(crtv, type = "behavior", nodeSet="crew")

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#####
# include exogenous predictor variables

require(RSiena)#link1995<-link1995[, c("tconst", "nconst")]#link1996<-link1996[, c("tconst", "nconst")]#link1997<-link1997[, c("tconst", "nconst")]#link1998<-link1998[, c("tconst", "nconst")]#link1999<-link1999[, c("tconst", "nconst")]#save(nd, nd, creativity,crew, film, link1995, link1996, link1997, link1998, link1999, indv1995, indv1996, indv1997, indv1998, indv1999, file = "nd.RData") #myCoEvolutionData <- sienaDataCreate(friendship, drinkingbeh, smoke)

#>CoVars####
# genre_confusion     <- varCovar(gcnf, nodeSet="crew")
start_year          <- coCovar(styr,  nodeSet="crew")
# role_consolidation  <- varCovar(rlcn, nodeSet="crew")
# reception           <- varCovar(rcpt, nodeSet="crew")
# experience          <- varCovar(xprc, nodeSet="crew")
# budget              <- varCovar(bdgt, nodeSet="crew")
# comedy              <- coCovar(Comedy, nodeSet="crew") #BD <- coCovar(Birthyear_data, nodeSet="crew") #crtv_cov <- varCovar(crtv, nodeSet="crew")

# PREPARE SIENA####

#network composition: leaving and joining
comp <- rep(list(c(1,3)), length(crew))
compcrew <- sienaCompositionChange(comp, nodeSet = "crew")


ndData <- sienaDataCreate(nd, creativity,   #nd,#focus_type, focus_role, conf_role, #crtv_cov, comedy, 
                           nodeSets=list(crew), compcrew)


#print01Report(ndData, modelname="ndartite-illustration1")

# EFFECTS ####

ndEffects <- getEffects(ndData)

#ndEffects <- includeEffects(ndEffects, cycle4ND, degPlus, name="nd")#transRecTrip, inPop, outAct, outPop)


#>selection####

ndEffects <- 
  includeEffects(ndEffects, egoPlusAltX,  interaction1 = "creativity", name="nd" )

# ndEffects <- includeEffects(ndEffects, egoX, interaction1="start_year", name="nd")
# 
# ndEffects <- includeEffects(ndEffects, simX, interaction1="focus_genres", name="nd")


# ESTIMATE####
gc()

# create algorithm object:
ndModel <- sienaAlgorithmCreate(projname='Project-level Creativity', modelType=c(nd=3))


ndResults <- siena07(ndModel, data=ndData, effects=ndEffects)#,useCluster = T, nbrNodes = 3)
# To look at the results, type

ndResults

# ndResults <- siena07(ndModel, data=ndData, effects=ndEffects, prevAns=ndResults2, returnDeps=TRUE)
#summary(ndResults)
#siena.table(ndResults, type="html", tstatPrint=T, sig=T)



# NOTE####
# the "convergence t-ratio" is the t-ratio for convergence checking,
# not the t statistic for testing the significance of this effect.

























