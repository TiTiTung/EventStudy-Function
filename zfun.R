library(tidyverse)
library(zoo)
# ?p???Y?ƥ??϶?n1??n2???ֿn???S?v (???????????S)
CAAR <- function(mydat, nowdate, startdate, n1, n2){
  edate <- which(mydat$event$when<nowdate-n2 & 
                   mydat$event$when>=startdate)
  
  event <- mydat$event[edate,]
  es.w <- mydat$stock[,edate]
  mk.w <- mydat$market[,edate]
  
  s0 <- matrix(1,n2-n1)%*%coredata(es.w[as.character(n1)])
  m0 <- matrix(1,n2-n1)%*%coredata(mk.w[as.character(n1)])
  
  sn <- coredata(es.w[as.character((n1+1):n2)])
  mn <- coredata(mk.w[as.character((n1+1):n2)])
  
  sret <- (sn-s0)/s0
  mret <- (mn-m0)/m0
  aret <- sret-mret
  
  # adj_cret: ?U?��ֿn???`???S
  adj_cret <- rowMeans(aret, na.rm = TRUE)
  # raw_cret: ?U?��??վ??ֿn???S
  raw_cret <- rowMeans(sret, na.rm = TRUE)
  
  # adj_winprob: ?U?��ֿn???`???S?Ӳv?W?V????
  winmatrix <- aret
  winmatrix[winmatrix>=0] <- 1
  winmatrix[winmatrix<0] <- 0
  adj_winprob <- rowSums(winmatrix)/ncol(winmatrix)
  
  # raw_winprob: ?U?��ֿn???S?Ӳv?j??0
  winmatrix <- sret
  winmatrix[winmatrix>=0] <- 1
  winmatrix[winmatrix<0] <- 0
  raw_winprob <- rowSums(winmatrix)/ncol(winmatrix)
  
  CAAR <- list(adj_cret=adj_cret,
               raw_cret=raw_cret,
               adj_winprob=adj_winprob,
               raw_winprob=raw_winprob,
               event=event)
}

# Ef_enum() ?ƥ??o?ͼ?
Ef_enum <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60){
  edate <- which(mydat$event$when<nowdate-n2 & 
                   mydat$event$when>=startdate)
  event <- mydat$event[edate,]
  Ef_enum <- nrow(event)
}

# Ef_car() ?��?(n??)?ֿn???`???S?v
Ef_car <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60){
  edate <- which(mydat$event$when<nowdate-n2 & 
                   mydat$event$when>=startdate)
  event <- mydat$event[edate,]
  es.w <- mydat$stock[,edate]
  mk.w <- mydat$market[,edate]
  
  s0 <- coredata(es.w[as.character(n1)])
  m0 <- coredata(mk.w[as.character(n1)])
  
  sn <- coredata(es.w[as.character(n2)])
  mn <- coredata(mk.w[as.character(n2)])
  
  sret <- (sn-s0)/s0
  mret <- (mn-m0)/m0
  aret <- sret-mret
  
  # adj_cret: ?U?��ֿn???`???S
  Ef_car <- mean(aret, na.rm = TRUE)
}

# Ef_scr() ?��?(n??)?ֿn???S?v
Ef_scr <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60){
  edate <- which(mydat$event$when<nowdate-n2 & 
                   mydat$event$when>=startdate)
  event <- mydat$event[edate,]
  es.w <- mydat$stock[,edate]
  s0 <- coredata(es.w[as.character(n1)])
  sn <- coredata(es.w[as.character(n2)])
  sret <- (sn-s0)/s0

  # adj_cret: ?U?��ֿn???`???S
  Ef_scr <- mean(sret, na.rm = TRUE)
}

# Ef_mcr() ?��?(n??)?????ֿn???S?v
Ef_mcr <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60){
  edate <- which(mydat$event$when<nowdate-n2 & 
                   mydat$event$when>=startdate)
  mk.w <- mydat$market[,edate]
  m0 <- coredata(mk.w[as.character(n1)])
  mn <- coredata(mk.w[as.character(n2)])
  mret <- (mn-m0)/m0
  Ef_mcr <- mean(mret, na.rm = TRUE)
}

# Ef_winprb1() ?��?(n??)?ֿn???S?v?j???s???v
Ef_winprb1 <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60){
  edate <- which(mydat$event$when<nowdate-n2 & 
                   mydat$event$when>=startdate)
  es.w <- mydat$stock[,edate]
  s0 <- coredata(es.w[as.character(n1)])
  sn <- coredata(es.w[as.character(n2)])
  sret <- (sn-s0)/s0
  
  winmatrix <- sret
  winmatrix[sret>=0] <- 1
  winmatrix[sret<0] <- 0

  Ef_winprb1 <- sum(winmatrix, na.rm = TRUE)/length(winmatrix)
}

# Ef_winprb2() ?��?(n??)?ֿn???S?v?W?V???????v
Ef_winprb2 <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60){
  edate <- which(mydat$event$when<nowdate-n2 & 
                   mydat$event$when>=startdate)
  es.w <- mydat$stock[,edate]
  mk.w <- mydat$market[,edate]
  s0 <- coredata(es.w[as.character(n1)])
  m0 <- coredata(mk.w[as.character(n1)])
  
  sn <- coredata(es.w[as.character(n2)])
  mn <- coredata(mk.w[as.character(n2)])
  
  sret <- (sn-s0)/s0
  mret <- (mn-m0)/m0
  aret <- sret-mret
  
  winmatrix <- aret
  winmatrix[aret>=0] <- 1
  winmatrix[aret<0] <- 0
  
  Ef_winprb2 <- sum(winmatrix, na.rm = TRUE)/length(winmatrix)
}

# Ef_carquat() ?��?(n??)?ֿn???`???S?vquantile
Ef_carquat <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60, quat=0.5){
  edate <- which(mydat$event$when<nowdate-n2 & 
                   mydat$event$when>=startdate)
  event <- mydat$event[edate,]
  es.w <- mydat$stock[,edate]
  mk.w <- mydat$market[,edate]
  
  s0 <- coredata(es.w[as.character(n1)])
  m0 <- coredata(mk.w[as.character(n1)])
  
  sn <- coredata(es.w[as.character(n2)])
  mn <- coredata(mk.w[as.character(n2)])
  
  sret <- (sn-s0)/s0
  mret <- (mn-m0)/m0
  aret <- sret-mret
  
  # adj_cret: ?U?��ֿn???`???S
  Ef_carquat <- as.numeric(quantile(aret,probs=quat, na.rm = TRUE))
}

# Ef_scrquat() ?��?(n??)?ֿn???`???S?vquantile
Ef_scrquat <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60, quat=0.5){
  edate <- which(mydat$event$when<nowdate-n2 & 
                   mydat$event$when>=startdate)
  event <- mydat$event[edate,]
  es.w <- mydat$stock[,edate]
  s0 <- coredata(es.w[as.character(n1)])
  sn <- coredata(es.w[as.character(n2)])
  sret <- (sn-s0)/s0
  
  # adj_cret: ?U?��ֿn???`???S
  Ef_scrquat <- as.numeric(quantile(sret,probs=quat, na.rm = TRUE))
}

# Ef_mcrquat() ?��?(n??)?ֿn???`???S?vquantile
Ef_mcrquat <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"),n1=0,n2=60, quat=0.5){
  edate <- which(mydat$event$when<nowdate-n2 & 
                   mydat$event$when>=startdate)
  mk.w <- mydat$market[,edate]
  m0 <- coredata(mk.w[as.character(n1)])
  mn <- coredata(mk.w[as.character(n2)])
  mret <- (mn-m0)/m0
  Ef_mcrquat <- as.numeric(quantile(mret,probs=quat, na.rm = TRUE))
}

# Ef_getallcret() ?^??n1-n2???ֿn???S?v  scr, mcr, event
Ef_getallcret <- function(mydat, nowdate=Sys.Date(), startdate=as.Date("2005-01-01"), n1=0, n2=60){
  edate <- which(mydat$event$when<nowdate-n2 & 
                   mydat$event$when>=startdate)
  
  event <- mydat$event[edate,]
  es.w <- mydat$stock[,edate]
  mk.w <- mydat$market[,edate]
  
  s0 <- matrix(1,n2-n1)%*%coredata(es.w[as.character(n1)])# ??row?ĥΦr???ӿ?
  m0 <- matrix(1,n2-n1)%*%coredata(mk.w[as.character(n1)])
  
  sn <- coredata(es.w[as.character((n1+1):n2)])
  mn <- coredata(mk.w[as.character((n1+1):n2)])
  
  sret <- (sn-s0)/s0
  mret <- (mn-m0)/m0
  # aret <- sret-mret
  
  # adj_cret: ?U?��ֿn???`???S
  #adj_cret <- rowMeans(aret, na.rm = TRUE)
  # raw_cret: ?U?��??վ??ֿn???S
  #raw_cret <- rowMeans(sret, na.rm = TRUE)
  
  Ef_getallcret <- list(scr=sret, mcr=mret, events=event, epar=c(n1,n2))
}

