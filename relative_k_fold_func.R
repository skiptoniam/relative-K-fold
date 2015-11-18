spp <- as.data.frame(read.csv('data//JSDM_4species_without_forest_plots.csv'))
nk <- 5
nkl <- 5
kfold_validate <- list()
kfold_test <- list()

#sample without replacement
require(plyr)
createFolds <- function(x,k){
  n <- nrow(x)
  x$folds <- rep(1:k,length.out = n)[sample(n,n)]
  x
}

folds <- ddply(spp,.(EMBHOR, EMBCIT, ANTTRI, ANTSPI),createFolds,k = nk)
for(ii in 1:nk){
  kfold_test[[ii]]<-folds[folds$folds==ii,]
  kfold_validate[[ii]]<-folds[folds$folds!=ii,]
  print(colSums(folds[folds$folds==ii,c(2,3,4,5)],na.rm = T))
}

names(kfold_test) <- paste0('kfold_test_w_out_replacement',1:nk)
names(kfold_validate) <- paste0('kfold_validate_w_out_replacement',1:nk)
sapply(names(kfold_test), 
       function (x) write.csv(kfold_test[[x]], file=paste0(x, ".csv")))
sapply(names(kfold_validate), 
       function (x) write.csv(kfold_validate[[x]], file=paste0(x, ".csv")))
# 
# #sample without replacement
# d <- transform(spp, id = as.numeric(interaction(EMBHOR, EMBCIT, ANTTRI, ANTSPI, drop=TRUE)))
# spp <- transform(spp, id = as.numeric(interaction(EMBHOR, EMBCIT, ANTTRI, ANTSPI, drop=TRUE)))
# 
# freq_tab <- as.data.frame(table(d$id))
# sample_number <- freq_tab$Freq*(1/nk)
# for(ii in 1:nk){
#     id_list <- list()
#     for(i in 1:length(unique(d$id))){
#         id_list[[i]] <- sample(as.numeric(rownames(d[d$id==i,c(2,3,4,5)])),sample_number[i],replace = FALSE)
#     }
#     ids <-do.call(c,id_list)
#     print(colSums(d[ids,c(2,3,4,5)],na.rm = T))
#     kfold_test[[ii]]<-d[ids,]
#     kfold_validate[[ii]]<-d[-ids,]
#     d <- d[-ids,]
# }
# 
# names(kfold_test) <- paste0('kfold_test_w_out_replacement',1:nk)
# names(kfold_validate) <- paste0('kfold_validate_w_out_replacement',1:nk)
# sapply(names(kfold_test), 
#        function (x) write.csv(kfold_test[[x]], file=paste0(x, ".csv")))
# sapply(names(kfold_validate), 
#        function (x) write.csv(kfold_validate[[x]], file=paste0(x, ".csv")))
# 

#sample with replacment
for(i in 1:nk){
  pres <-as.data.frame(apply(spp[,c(2,3,4,5)],2,function(x) rbinom(x,x,1/nkl)))
  print(colSums(pres))
  pres_id <- as.numeric(rownames(pres[rowSums(sapply(pres[,1:4],`!=`,e2=0))==1,]))
  abs_id <- sample(as.numeric(rownames(pres[rowSums(sapply(pres[,1:4],`!=`,e2=0))==0,])),nrow(pres)/nkl,replace = TRUE)
  print(nrow(spp[unique(c(pres_id,abs_id)),]))
  kfold_test[[i]]<- spp[unique(c(pres_id,abs_id)),]
  kfold_validate[[i]]<- spp[-unique(c(pres_id,abs_id)),]
}  

names(kfold_test) <- paste0('kfold_test_w_replacement',1:nk)
names(kfold_validate) <- paste0('kfold_validate_w_replacement',1:nk)
sapply(names(kfold_test), 
       function (x) write.csv(kfold_test[[x]], file=paste0(x, ".csv")))
sapply(names(kfold_validate), 
       function (x) write.csv(kfold_validate[[x]], file=paste0(x, ".csv")))

