spp <- as.data.frame(read.csv('data//JSDM_4species_without_forest_plots.xlsx'))
nk <- 5
nkl <- 5
kfold_validate <- list()
kfold_test <- list()

for(i in 1:nk){
  pres <-as.data.frame(apply(spp[,c(2,3,4,5)],2,function(x) rbinom(x,x,1/nkl)))
  print(colSums(pres))
  pres_id <- as.numeric(rownames(pres[rowSums(sapply(pres[,1:4],`!=`,e2=0))==1,]))
  abs_id <- sample(as.numeric(rownames(pres[rowSums(sapply(pres[,1:4],`!=`,e2=0))==0,])),nrow(pres)/nkl,replace = TRUE)
  print(nrow(spp[unique(c(pres_id,abs_id)),]))
  kfold_test[[i]]<- spp[unique(c(pres_id,abs_id)),]
  kfold_validate[[i]]<- spp[-unique(c(pres_id,abs_id)),]
#   spp <- spp[-unique(c(pres_id,abs_id)),]
#   nkl <- nkl - 1
}  

names(kfold_test) <- paste0('kfold_test',1:nk)
names(kfold_validate) <- paste0('kfold_validate',1:nk)
sapply(names(kfold_test), 
       function (x) write.csv(kfold_test[[x]], file=paste0(x, ".csv")))
sapply(names(kfold_validate), 
       function (x) write.csv(kfold_validate[[x]], file=paste0(x, ".csv")))
lapply(kfold_validate, function(x) write.csv(x, file = paste0("kfold_validate",x,".csv")))

