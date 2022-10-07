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
