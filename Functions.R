
select_surv <- function(surv,index){
  live.surv2 <- list()
  for ( n in 1:length(surv)) {
    tmp <- surv[[n]]
    tmp.idx <- index[[n]]
    live.surv2[[n]] <- tmp[tmp.idx,] 
  }
  names(live.surv2) <- names(surv)
  live.surv2
}

filter_gp <- function(gp,percentage = .1){
  idx <- colSums(gp)/nrow(gp) > percentage & colSums(gp)/nrow(gp) < (1-percentage)
}

test_function <- function(test.progV2,test.surv,model.imm,cutoff){
  risk.imm <- predict(model.imm,test.progV2,type = "response")
  surv <- test.surv
  surv$obj <- Surv(surv$OS,surv$OS_event)
  
  surv$imm <- cut(risk.imm,breaks = c(min(risk.imm)-1,cutoff,max(risk.imm)+1),labels = c("Imm->Good","Imm->Bad"))
  
  fit <- survfit(obj~imm,data = surv)
  fit
}

get_gp <- function(train1.exp){
  library(foreach)
  library(doParallel)
  cl<-makeCluster(4)
  registerDoParallel(cl)
  n <- dim(train1.exp)[1]
  dat <- t(train1.exp)
  
  tmp <- foreach(i = c(1:(n - 1)),.combine = cbind) %dopar% {
    yy <- dat[,i] < dat[,-c(1:i)]
  }
  gp <- c()
  for(i in c(1:(n - 1))){
    gp <- c(gp,paste(colnames(dat)[i],colnames(dat)[-c(1:i)],sep = " < "))
  }
  colnames(tmp) <- gp
  stopCluster(cl)
  mode(tmp) <- "numeric"
  tmp
}

make_stage <- function(surv.tr){
  surv2 <- surv.tr
  surv2$stage <- gsub(" ","",surv2$stage)
  surv2$stage <- gsub("a","A",surv2$stage)
  surv2$stage <- gsub("b","B",surv2$stage)
  surv2$stage2 <- surv2$stage
  
  surv2$stage2[surv2$stage == "1A"] <- 1
  surv2$stage2[surv2$stage == "1"] <- 1.5
  surv2$stage2[surv2$stage == "1B"] <- 2
  surv2$stage2[surv2$stage == "2A"] <- 3
  surv2$stage2[surv2$stage == "2"] <- 3.5
  surv2$stage2[surv2$stage == "2B"] <- 4
  surv2$stage2[surv2$stage == "3A"] <- 5
  surv2$stage2[surv2$stage == "3"] <- 5.5
  surv2$stage2[surv2$stage == "3B"] <- 6
  surv2$stage2[surv2$stage == "4"] <- 7
  
  surv2$stage2 <- as.numeric(surv2$stage2)
  surv2
}

stat_surv <- function(x){
  idx <- complete.cases(x$OS) & complete.cases(x$OS_event)
  x[idx,]
}
