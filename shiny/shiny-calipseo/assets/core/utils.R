#General helpers

#round2
round2 <- function(x, n=0) {scale<-10^n; trunc(x*scale+sign(x)*0.5)/scale}

#NumberOfDays
NumberOfDays <- function(date) {
  return(as.numeric(format(as.Date(paste0(format(date,format="%Y"),formatC(ifelse(format(date,format="%m")=="12",0,as.numeric(format(date,format="%m")))+1,width=2,format="d",flag="0"),"01"),"%Y%m%d")-1,format="%d")))
}