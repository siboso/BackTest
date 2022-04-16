# ======= "perform.R" GUIDE ======= #20161223
# fix bug: nvindstat > winpct
# =============================


initDate <- function(dates){
  return(sort(unique(dates)))
}

initAcct <- function(dates, initTransfer=1000000){
  Acct <- data.frame(DATETIME=dates, Transfer=NA, Trade=NA, Balance=NA, 
                     MarketValue=NA, TotalAsset=NA)
  Acct[1, 2:6] <- c(initTransfer, 0, initTransfer, 0, initTransfer)
  return(Acct)
}

initPort <- function(dates){
  Port <- list()
  for(dat in dates){
    Port[[dat]] <- list()
  }
  return(Port)
}

# 事件处理
#event_adj <- function(eventlist){
#  eventlist_adj=data.frame()
#  for (i in 1:nrow(eventlist)){
#    ret_tmp=stock[[eventlist$code[i]]] %>% filter(DATETIME>eventlist$DATETIME[i]) %>% 
#      head(1) %>% select(DATETIME,OPEN,HALT,LimitUp) %>% rename(DATETIME_1=DATETIME) %>% # 保留开盘价，以开盘价买入
#      mutate(DATETIME=eventlist$DATETIME[i],code=eventlist$code[i],handle=eventlist$handle[i],id=eventlist$id[i])
#    eventlist_adj=rbind(eventlist_adj,ret_tmp)
#  }
#  eventlist_adj=eventlist_adj %>% filter(HALT==0 & LimitUp==0)
#  return(eventlist_adj)
#}

# initial setup for sub portfolio
initSubPort <- function(dates, N){ # the dates must be sorted
  Port <- list()
  for(dat in dates){
    Port[[dat]] <- list()
    
  }
  for(i in 1:N){
    Port[[dates[1]]][[paste("p",i,sep="")]] <- list(code="na", 
                                                    price=0, 
                                                    num=0, 
                                                    expire="na",
                                                    price_high=0,
                                                    hold=0)
  }
  return(Port)
}

atrade <- function(code, price, num, expire){
  return(list(code=code, price=price, num=num, expire=expire))
}

getStockValue <- function(code, date, colname){
  stock[[code]] %>% {.[.$DATETIME==date, colname]} %>% as.numeric()
}


sharpe <- function (x, r = 0, scale = sqrt(250)){
  if (NCOL(x) > 1) 
    stop("x is not a vector or univariate time series")
  if (any(is.na(x))) 
    stop("NAs in x")
  if (NROW(x) == 1) 
    return(NA)
  else {
    y <- (x-lag(x))/lag(x)
    return(round(scale * (mean(y[-1], na.rm = T) - r)/sd(y[-1]), 2))
  }
}
yearret <- function(x, scale=250){
  if (NCOL(x) > 1) 
    stop("x is not a vector or univariate time series")
  if (any(is.na(x))) 
    stop("NAs in x")
  if (NROW(x) == 1) 
    return(NA)
  else {
    y <- (x-lag(x))/lag(x)
    return(round(scale * mean(y[-1], na.rm = T), 3))
  }
}
yearsd <- function(x, scale=sqrt(250)){
  if (NCOL(x) > 1) 
    stop("x is not a vector or univariate time series")
  if (any(is.na(x))) 
    stop("NAs in x")
  if (NROW(x) == 1) 
    return(NA)
  else {
    y <- (x-lag(x))/lag(x)
    return(round(scale * sd(y[-1], na.rm = T), 3))
  }
}
calmar <- function (x, scale=250){
  if (NCOL(x) > 1) 
    stop("x is not a vector or univariate time series")
  if (any(is.na(x))) 
    stop("NAs in x")
  if (NROW(x) == 1) 
    return(NA)
  else {
    y <- (x-lag(x))/lag(x)
    yr <- scale * mean(y[-1], na.rm = T)
    dw <- min((x-cummax(x))/cummax(x))
    return(round(-yr/dw, 2))
  }
}
maxdrawdown <- function(x){
  if (NCOL(x) > 1) 
    stop("x is not a vector or univariate time series")
  if (any(is.na(x))) 
    stop("NAs in x")
  if (NROW(x) == 1) 
    return(NA)
  else {
    dw <- min((x-cummax(x))/cummax(x))
    return(round(dw,3))
  }
}

nvstat <- function(df.nv){
  if(is.null(dim(df.nv))) df.nv <- data.frame(NV=df.nv)
  if(sum(colnames(df.nv)=='DATETIME')>0) df.nv <- df.nv %>% select(-DATETIME)
  NAMES <- colnames(df.nv)
  for(i in seq_along(NAMES)){
    x <- df.nv[,i]
    ans <- list(lastnv=round(x[length(x)],2), yearret=yearret(x), yearsd=yearsd(x), sharpe=sharpe(x), 
              maxdrawdown=maxdrawdown(x), calmar=calmar(x))
    if(i==1){
      df.stat <- unlist(ans)
    }else{
      df.stat <- rbind(df.stat, unlist(ans))
    }
  }
  if(length(NAMES)>1) rownames(df.stat) <- NAMES
  return(df.stat)
}

IND.tradeindex <- function(v, start_index=0){
  cumsum(1*(v!=lag(v)) %>% na.fill('A', 0, quiet=T))
}

nvindstat <- function(nv, ind, type='IND'){
  if(length(nv)!=length(ind)) stop('nv and ind have different length!')
  if(sum(is.na(nv))) stop('nv has NA!')
  if(sum(is.na(ind))) ind <- ind %>% na.fill('A')
  if(type=='ACT') {
    tid <- rep(NA, length(ind))
    tid[which(ind!=0)] <- 1:length(which(ind!=0))
    ind <- tid %>% na.fill('N') %>% na.fill('A')
  }
  
  x <- nv
  tdata <- data.frame(NV=nv, LIND=lag(ind)) %>% mutate(tid = IND.tradeindex(LIND)) %>% group_by(tid) %>% 
    summarise(IND=first(LIND), NV=last(NV)) %>% mutate(RET=(NV/lag(NV)-1) %>% na.fill('F')) %>% 
    filter(IND!=0)
  ans <- list(lastnv=round(x[length(x)],2), 
              yearret=yearret(x), 
              yearsd=yearsd(x), 
              sharpe=sharpe(x), 
              maxdrawdown=maxdrawdown(x), 
              calmar=calmar(x),
              winpct=round(sum(tdata$RET>0)/nrow(tdata),2),
              `prof/loss`=round(mean(tdata$RET[tdata$RET>0])/-mean(tdata$RET[tdata$RET<0]),2),
              times=round(nrow(tdata)/length(nv)*250,1)
              )
  return(unlist(ans))
}

NV.theory <- function(ZS, IND){
  ret <- lag(IND)  * (ZS/lag(ZS)-1)
  ret[is.na(ret)] <- 0
  cumprod(1+ret)
}

NV.theory2 <- function(ZS, IND, cost=1/10000){
  ret <- lag(IND)  * (ZS/lag(ZS)-1)
  ret[is.na(ret)] <- 0
  ret <- ret - cost * abs(lag(IND)-lag(IND,2)) %>% na.fill('A')
  cumprod(1+ret)
}