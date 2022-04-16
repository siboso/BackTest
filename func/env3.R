# ======= "env2.R" GUIDE =======   20161117
# source('env.r')                 #--init a Custom Environment(CE) with base function
# env.source('missing.R')         #--load a source code to CE
# env$value <- 123                #--Use CE like this
# env.list()                      #--list CE variables and functions
# env.dump()                      #--dump CE
# =============================

#clear myfunction env
for(env.i in seq_len(sum(search()=='Custom Environment'))) detach('Custom Environment')
rm(env.i)

#init setting
library(dplyr)
library(pipeR)
library(TTR)
library(readr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(readxl)
options(scipen=200)
options(stringsAsFactors=FALSE)
#cat('dplyr,pipeR,TTR,readr,ggplot2,reshape2,girdExtra;scipen=200,stringsAsFactors=F;missing.r,paraset.r,sqlite.r...')
env <- attach(NULL, name = "Custom Environment")

#env functions
env$env.source <- function(file){
  sys.source(file, envir = env)
}
env$env.list <- function(){
  ls.str(env)
}
env$env.ls <- function(){
  ls(env)
}
env$env.dump <- function(){
  rm(env, envir = .GlobalEnv)
  detach('Custom Environment')
}
env$ret <- function(x){
  y <- x/lag(x)-1
  y[which(!is.na(y))[1]-1] <- 0
  return(y)
}



#missing functions
env$na.fill <- function(missarray, way='F:first | L:last | A:all | N:next | I:increase', value=0, quiet=T){
  x <- missarray
  y <- x
  if(length(way)>10)        stop('para way is needed')
  if(way=="first"|way=='F') y[1] <- value
  if(way=='all' |way=='A')   y[is.na(y)] <- value
  if(way=='last'|way=='L')  y[length(y)] <- value
  if(way=='next'|way=='N'){
    ina <- which(is.na(y))
    if(length(ina)>0 && ina[1]==1)  ina <- ina[-1]
    for(i in seq(length.out=length(ina)))  y[ina[i]] <- y[ina[i]-1]
  }
  if(way=='increase'|way=="I"){
    ina <- which(is.na(y))
    if(length(ina)>0 && ina[1]==1)  ina <- ina[-1]
    if(value==0) stop('increase value cannot be 0')
    for(i in seq(length.out=length(ina)))  y[ina[i]] <- y[ina[i]-1]+value
  }
  if(!quiet)  cat('na.fill:', way, sum(is.na(x)), '->', sum(is.na(y)), 'na in',length(y),'obs \n')
  return(y)
}
env$na.case <- function(df){
  df %>% filter(!complete.cases(df))
}
env$na.column <- function(df){
  df %>% summarise_all(funs(sum(is.na(.))))
}

#paraset function
env$ps.save <- function(ps, file="ParaTxt.txt"){
  y1 <- yaml::as.yaml(ps)
  cat(y1, file=file, sep='\n')
}
env$ps.load <- function(file="ParaTxt.txt"){
  if(file.exists(file)) ps <- yaml::yaml.load_file(file)  else  ps <- list()
  ps
}
env$ps.rec <- function(ps, key, file){
  if(file.exists(file))  load(file)  else ParaSet <- list()
  ParaSet[[as.character(key)]] <- ps
  save(ParaSet, file=file)
}


#ggplot2
env$gg.xlabel <- function(LABELS, NTICKS=6){
  xat <- c(1, which(LABELS != lag(LABELS)))
  if(length(xat)>NTICKS) xat <- xat[floor(seq(1, length(xat), length(xat) %/% NTICKS))]
  scale_x_continuous(breaks=xat, labels=LABELS[xat])
}
env$gg.gc2 <- scale_colour_gradient2(high='red',low='green')
env$gg.gf2 <- scale_fill_gradient2(high='red',low='green')
env$gg.legend.in <- theme(legend.justification=c(0,1), legend.position=c(0,1), legend.title=element_blank(),
                          legend.background=element_rect(NA), legend.key=element_rect(NA,NA),legend.text=element_text(size=12))
env$gg.legend.none <- theme(legend.position='none')
env$gg.panel <- theme(panel.background=element_rect(fill = 'white', colour='gray'), panel.grid.major=element_line(colour = 'gray90'))
