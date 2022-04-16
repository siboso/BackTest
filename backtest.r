##### Code: A Minimalist Program for Event-driven Stategies
##### Input parameters: L,M,N,C,SL,startdate,enddate,event_list
##### Date: 2017-4-10
##### Author: Sibo Liu

### 1. Initial setup
rm(list=ls())
setwd("D:/Trading/Backtest") # set working dictionary
source('func/env3.r')
source('func/perform2.r')
source('func/nvplot.r', encoding = 'utf8')
load('table/stock.rdata') # stock returns, please download a sample file here: https://drive.google.com/file/d/19254EsL_GwxzN3Y57aAw0bJ0LLNP-8jQ/view?usp=sharing

### 2. Import event list
# I illustrate using a list of insider purchases.
eventlist <- read_excel("table/Insider_purchase.xlsx",sheet="eventlist")
eventlist <- eventlist %>%
  mutate(DATETIME=as.character(eventlist$anndate), code=sprintf("%06d", stkcd)) %>%
  select(DATETIME, code) %>% arrange(DATETIME) %>% mutate(handle=0)
eventlist$id <- 1:nrow(eventlist)

### 3. Input parameters
## We assume buy in the first trading day after the announcement date. 公告日之后第一个交易日开盘价买入
L <- 5      # The event window 事件有效期（自然日）
M <- 10     # number of days holding（交易日）
N <- 25     # number of stocks in the portfolio（组合分割数目）
C <- 0.002  # trading cost（交易成本）
SL <- 0.01  # stop loss（回撤止损）
startdate <- 2013
enddate <- 2018

# buy cost and sell cost
BC= 1+C*0.25 # cost when buy  买入时：0.05%手续费
SC= 1-C*0.75 # cost when sell 卖出时：0.05%手续费+0.1%印花税

### 4. Portfolio setup
csi300 <- csi300 %>% filter(DATETIME>startdate, DATETIME<enddate) # restrict sample
Acct <- initAcct(csi300$DATETIME)
Port <- initPort(csi300$DATETIME)
TradeList <- data.frame(DATETIME=csi300$DATETIME[1], Code=NA, Type=NA, Price=NA) # 交易清单
eventlist$handle <- 0 # handle 0=no action未处理 1=buy买入
p1 <- Sys.time() # clock starts

### 5. Loop
for(i in 2:max(nrow(Acct),5)){
  
  date <- Acct$DATETIME[i]
  lastdate <- Acct$DATETIME[i-1]
  c <- Acct$DATETIME[i-1]
  Acct[i,"Trade"] <- 0
  
  ### A. Balance 仓位结算
  pt <- Port[[lastdate]]
  amt <- 0
  if(length(pt)>0){
    for(j in length(pt):1){
      code <- pt[[j]]$code
      pt[[j]]$expire <- pt[[j]]$expire - 1
      thisclose <- getStockValue(code, date, 'CLOSE')
      thisopen <- getStockValue(code, date, 'OPEN')
      thislow <- getStockValue(code, date, 'Low')
      thisLimDn <- getStockValue(code, date, 'LimitDown')
      thisLimUp <- getStockValue(code, date, 'LimitUp')
      thisHalt <- getStockValue(code, date, 'HIGH')==getStockValue(code, date, 'Low')
      
      if(thisclose<pt[[j]]$price_high*(1-SL) && thisLimDn==0 && !thisHalt){  # 回撤止损卖出
        price <- thisclose ## sell at the closing price收盘价卖出
        # price <- (pt[[j]]$price*(1-SL))*SC ## or sell at this price 止损价卖出
        # price <- min(thisopen, pt[[j]]$price*(1-SL))*SC  # or sell at this price 止损价卖出，若开盘即止损，按开盘价卖出
        
        num <- pt[[j]]$num
        amt <- amt + price*num
        pt[[j]] <- NULL
        
        TradeList <- rbind(TradeList, c(date, code, "Stop", price))
      }else if(pt[[j]]$expire<=0 && thisLimDn==0 && !thisHalt){    ## holding until the event window we set 持有到期卖出
        price <- getStockValue(code, date, 'CLOSE')*SC  ## holding until the event window we set and sell 持有到期收盘卖出
        
        num <- pt[[j]]$num
        amt <- amt + price*num
        pt[[j]] <- NULL
        
        TradeList <- rbind(TradeList, c(date, code, "Sell", price))
      }else {
        pt[[j]]$price_high <- max(thisclose,pt[[j]]$price) # 更新股票进入组合之后的最高价
      }
      
    }
    Acct[i,"Trade"] <- Acct[i,"Trade"] + round(amt,2)
    Port[[Acct$DATETIME[i]]] <- pt
  }
  
  ### B. Adjust and buy 调仓买入
  pt <- Port[[date]]
  thisevent <- eventlist %>% 
    filter(DATETIME<date, DATETIME>as.character(as.Date(date)-L), handle==0) %>% 
    tail(N-length(pt))   # T-L+1日到T-1日事件的最新事件
  money <- Acct[i-1, "Balance"]/(N-length(pt))
  if(nrow(thisevent)>0){
    amt <- 0
    for(j in 1:nrow(thisevent)){
      code <- thisevent$code[j]
      thisopen <- getStockValue(code, date, 'OPEN')
      thisHalt <- getStockValue(code, date, 'HIGH')==getStockValue(code, date, 'Low')
      thisLimUp <- getStockValue(code, date, 'LimitUp')
      if(length(pt[[code]])==0 && thisLimUp==0 && !thisHalt){
        price <- thisopen*BC  # buy at the opening price 开盘价买入
        num <- trunc(money/price/100)*100
        amt <- amt + price*num
        pt[[code]] <- list(code=code, 
                           price=price, 
                           num=num, 
                           expire=M,
                           price_high=price) # the high price when entering the portfolio 股票进入组合之后的最高价
        eventlist$handle[eventlist$id==thisevent$id[j]] <- 1
        TradeList <- rbind(TradeList, c(date, code, "Buy", price))
      }
    }
    Acct[i,"Trade"] <- Acct[i,"Trade"] - round(amt,2)
  }
  Port[[Acct$DATETIME[i]]] <- pt
  
  ### C. Account Settlement 账户结算
  Acct[i,"Balance"] <- Acct[i-1,"Balance"] + Acct[i,"Trade"]
  amt <- 0
  for(j in seq_along(pt)){
    amt <- amt + pt[[j]]$num * getStockValue(pt[[j]]$code, date, 'CLOSE')
  }
  Acct[i,"MarketValue"] <- amt
  Acct[i,"TotalAsset"] <- Acct[i,"Balance"] + Acct[i,"MarketValue"]
}

time_consumed=Sys.time() - p1
print(time_consumed) # time consumed
# xlsx::write.xlsx(TradeList[-1,], 'table/TradeList.xlsx', row.names = F)  # 打印交易清单,time consuming

### 6. final record of the portfolio
Acct$NV <- Acct$TotalAsset/Acct$TotalAsset[1] # Net Value of the portfolio 组合净值
Acct$RET1 <- (Acct$NV/lag(Acct$NV)-1) %>% na.fill("F",0)  # Daily return of the potfolio 组合日收益率
Acct$CSI300 <- csi300$CLOSE/csi300$CLOSE[1] # Net market value 市场净值
Acct$RET2 <- (Acct$CSI300/lag(Acct$CSI300)-1) %>% na.fill("F",0)  # daily market return 市场日收益率
Acct$difRET <- Acct$RET1 - Acct$RET2 # Abnormal return of the portfolio 组合超额收益率
Acct$difNV <- cumprod(Acct$difRET+1) # Abnormal net value of the portfolio 组合超额净值

### 7. plot figures
nvplot(Acct %>% select(DATETIME, NV, CSI300))  # Absolute return 绝对收益
nvplot(Acct %>% select(DATETIME, difNV))       # Relative return 相对收益
nvstat=nvstat(Acct %>% select(NV, CSI300, difNV))     # Performance statistics 表现统计
nvstat

# TradeList is the transaction list

### 8. Save results
save(Acct,eventlist,TradeList,L,M,N,C,SL,startdate,enddate,time_consumed,file='table/results.rdata')


