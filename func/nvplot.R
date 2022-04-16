#nvplot3  #20161102
#nvplot
#lineplot
#nv_ind_strip
library(dplyr)
library(reshape2)
library(ggplot2)
library(grid)
library(scales)
library(gridExtra)

nvplot <- function(df_only_nv, LABELS=NULL, BASE=NULL, LEGENDS=NULL, TITLE=NULL, XLAB=NULL, NTICKS=8){
  if(sum(colnames(df_only_nv)=='DATETIME')>0){
    LABELS <- df_only_nv$DATETIME
    df_only_nv <- df_only_nv %>% select(-DATETIME)
  }
  m1 <- df_only_nv %>% mutate(id=1:n()) %>% melt(id='id')
  m2 <- m1
  wherel <- numeric(0)
  lastnv <- numeric(0)
  maxdwb <- numeric(0)
  wherem <- numeric(0)
  for(lvl in levels(m2$variable)){
    j <- m2$variable==lvl
    nv <- m2$value[j]
    lastnv <- c(lastnv, tail(m2$value[j],1))
    wherel <- c(wherel, length(m2$value[j]))
    m2$value[j] <- (nv-cummax(nv))/cummax(nv)
    maxdwb <- c(maxdwb, min(m2$value[j]))
    wherem <- c(wherem, which.min(m2$value[j]))
  }
  if(length(LABELS)==0){
    LABELS <- 1:max(wherel)
  }
  if(length(LEGENDS)>0){
    levels(m1$variable) <- LEGENDS
  }
  
  xat <- which(LABELS != lag(LABELS))
  if(length(xat)>NTICKS) xat <- xat[floor(seq(1, length(xat), length(xat) %/% NTICKS))]
  AXIS.X <- scale_x_continuous(breaks=xat, labels=LABELS[xat])
  MARGIN <- theme(plot.margin=unit(c(0,0.1,0,0.1), 'cm'))
  T_LEG1 <- theme(legend.justification=c(0,1), legend.position=c(0,1), 
                  legend.title=element_blank(), legend.background=element_rect(NA), 
                  legend.key=element_rect(NA,NA),legend.text=element_text(size=12))
  T_LEG2 <- theme(legend.position='none')
  PANEL <- theme(panel.background=element_rect(fill = 'white', colour='gray'), panel.grid.major=element_line(colour = 'gray90'))
  
  a <- ggplot() + geom_line(data=m1, aes(x=id, y=value, colour=variable), size=1) +
    geom_text(aes(x=wherel, y=lastnv, label=round(lastnv,4)), size=3) +
    AXIS.X + T_LEG1 + PANEL + MARGIN + labs(y='Net value', title=TITLE) +
    theme(axis.ticks.x=element_blank(),
          axis.text.x=element_blank(), axis.title.x=element_blank())
  if(length(BASE)>0){
    a <- a + geom_line(aes(x=1:length(BASE), 
                           y=rescale(BASE, to = c(min(m1$value,na.rm = T),max(m1$value,na.rm = T)))),
                       alpha=0.2)
  }  
  b <- ggplot() + geom_line(data=m2, aes(x=id, y=value, colour=variable), size=1) + 
    geom_text(aes(x=wherem, y=maxdwb, label=paste(round(maxdwb*100,1),'%')), size=3) +
    AXIS.X + T_LEG2 + PANEL + MARGIN + labs(x=XLAB, y='Max drawndown') 
  
  g1 <- ggplot_gtable(ggplot_build(a))
  g2 <- ggplot_gtable(ggplot_build(b))
  maxWidth <- unit.pmax(g1$widths[2:3], g2$widths[2:3])
  g1$widths[2:3] <- maxWidth
  g2$widths[2:3] <- maxWidth
  grid.arrange(g1,g2, heights=c(3,2))
}
#nvplot(nv %>% select(3:7), nv$DATETIME %>% substr(3,7), NTICKS = 12)
# nvplot(ac %>% select(nv), LABELS = ac$DATETIME %>% substr(3,7), BASE=ac$CLOSE, 
#        XLAB = paste('Sp',round(ps$sharpe, 3),'Yr',round(ps$yearret, 3)) )

lineplot <- function(innerdf, norm=F){
  innerdf <- innerdf %>% as.data.frame()
  if(norm){
    innerdf <- innerdf %>% mutate_all(rescale)
    NORM <- theme(axis.text.y=element_text(colour='gray90'))
  }else{
    NORM <- theme()
  }
  innerdf$id <- 1:nrow(innerdf)
  T_LEG1 <- theme(legend.justification=c(0,1), legend.position=c(0,1), legend.direction='horizontal',
                  legend.title=element_blank(), legend.background=element_rect(NA), 
                  legend.key=element_rect(NA),legend.text=element_text(size=12))
  PANEL <- theme(panel.background=element_rect(fill = 'white', colour='gray'), panel.grid.major=element_line(colour = 'gray90'))
  NOTICK <- theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())
  
  m1 <- innerdf %>% melt(id='id')
  ggplot(m1)+geom_line(aes(x=id, y=value, colour=variable), alpha=0.5,size=1)+T_LEG1+PANEL+NOTICK+labs(x=NULL,y=NULL)+NORM
}


nvstrip <- function(str.data, LABELS=NULL, NTICKS=8, LogY=T){
  if(!all(c('DATETIME','CLOSE','NV','IND') %in% colnames(str.data))){
    stop('DATETIME, CLOSE, NV, IND')
  }
  N <- nrow(str.data);  x <- 1:N
  nvmin <- min(str.data$NV);  nvmax <- max(str.data$NV)
  #ind.data
  j <- rep(T, N); j[which(str.data$IND==lag(str.data$IND))] <- F
  ind.data <- data.frame(IND=str.data$IND) %>% mutate(from=1:n()) %>% filter(j) %>% mutate(to=lead(from)-1)
  ind.data$to[nrow(ind.data)] <- N
  #labels
  if(is.null(LABELS)) LABELS <- str.data$DATETIME
  xat <- which(LABELS != lag(LABELS))
  if(length(xat)>NTICKS) xat <- xat[floor(seq(1, length(xat), length(xat) %/% NTICKS))]
  AXIS.X <- scale_x_continuous(breaks=xat, labels=LABELS[xat])
  MARGIN <- theme(plot.margin=unit(c(0,0.1,0,0.1), 'cm'))
  #plot
  p <- ggplot()+theme(panel.background=element_rect(fill = 'white', colour='gray'), panel.grid.major=element_line(colour = 'gray90'))
  if(sum(ind.data$IND > 0, na.rm=T)>0){
    p <- p + geom_rect(data=ind.data %>% filter(IND > 0), aes(xmin=from-0.5, xmax=to+0.5, ymin=nvmin, ymax=nvmin+(nvmax-nvmin)*abs(IND)), alpha=0.1, fill='red')
  }
  # if(sum(ind.data$IND==0, na.rm=T)>0){
  #   p <- p + geom_rect(data=ind.data %>% filter(IND==0), aes(xmin=from-0.5, xmax=to+0.5, ymin=nvmin, ymax=nvmax), alpha=0.1, fill='blue')
  # }
  if(sum(ind.data$IND < 0, na.rm=T)>0){
    p <- p + geom_rect(data=ind.data %>% filter(IND < 0), aes(xmin=from-0.5, xmax=to+0.5, ymin=nvmin, ymax=nvmin+(nvmax-nvmin)*abs(IND)), alpha=0.1, fill='green')
  }
  p + geom_line(aes(x=x, y=rescale(str.data$CLOSE, to=c(nvmin,  nvmax))), col='gray') +
    geom_line(aes(x=x, y=str.data$NV), size=1) + MARGIN +
    scale_y_continuous(trans=ifelse(LogY, 'log1p', 'identity')) + AXIS.X + labs(y='Net Value', x=NULL)
}
