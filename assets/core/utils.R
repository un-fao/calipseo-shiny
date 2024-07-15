#General helpers
logger <- function(type, txt, ...){
  log_txt <- sprintf(txt, ...)
  cat(sprintf("[shiny-calipseo][%s] %s \n", type, log_txt), file = stderr())
}
INFO <- function(txt, ...){logger("INFO", txt, ...)}
WARN <- function(txt, ...){logger("WARN", txt, ...)}
ERROR <- function(txt, ...){logger("ERROR", txt, ...)}

#round2
round2 <- function(x, n=0) {scale<-10^n; trunc(x*scale+sign(x)*0.5)/scale}

#NumberOfDays
NumberOfDays <- function(date) {
  return(as.numeric(format(as.Date(paste0(format(date,format="%Y"),formatC(ifelse(format(date,format="%m")=="12",0,as.numeric(format(date,format="%m")))+1,width=2,format="d",flag="0"),"01"),"%Y%m%d")-1,format="%d")))
}

#Convert Quarter to date
qtodate<- function(date,period="start"){
  
  date<-unlist(strsplit(date,"-") )
  
  Y<-date[1]
  Q<-date[2]
  
  Q<-switch (Q,
             Q1=c('01-01','03-31'),
             Q2=c('04-01','06-30'),
             Q3=c('07-01','09-30'),
             Q4=c('10-01','12-31')
  )
  
  date<-paste0(Y,"-",Q)
  
  if(period=="start")return(date[1])
  if(period=="end")return(date[2])
  if(period=="start+end")return(paste0(date[1],"/",date[2]))
  
}

#Convert Year-Month to date
ymtodate<- function(date,period="start"){
  start_date<-paste0(date,"-01")
  end_date<-as.character(ceiling_date(ymd(start_date),unit= "month") -1)
  if(period=="start")return(start_date)
  if(period=="end")return(end_date)
  if(period=="start+end")return(paste0(start_date,"/",end_date))
  
}

#Convert Year to date
ytodate<- function(date,period="start"){
  Y<-unlist(strsplit(date[i],"-"))[1]
  start_date<-paste0(Y,"-01-01")
  end_date<-paste0(Y,"-12-31")
  if(period=="start")return(start_date)
  if(period=="end")return(end_date)
  if(period=="start+end")return(paste0(start_date,"/",end_date))
}

#Manage Date formating
dateformating<- function(date,period="start"){
  dates<-c()
  for(i in c(1:length(date))){
    if(startsWith(unlist(strsplit(date[i],"-"))[2],"Q")){
      x<-qtodate(date[i],period=period)
    }else if(unlist(strsplit(date[i],"-"))[2]=="NA"){
      x<-ytodate(date[i],period=period)
    }else{
      x<-ymtodate(date[i],period=period)
    }
    dates<-c(dates,x)
  }
  return(dates)
}

#intersection
##Extension of intersect() function, allow intersection of more than two dataframe 
intersection <- function(x, y, ...){
  if (missing(...)) intersect(x, y)
  else intersect(x, intersection(y, ...))
}