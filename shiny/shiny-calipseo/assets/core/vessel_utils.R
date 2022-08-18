#vesselFindeR
vesselFindeR <- function(name, flag_iso2){
  
  tryCatch({
    
    html = httr::content(httr::GET(sprintf("https://www.vesselfinder.com/vessels?name=%s&flag=%s&type=5", name, flag_iso2), httr::add_headers("User-Agent" = "vesselFindeR")))
    
    #links
    alinks = xml2::xml_find_all(html, ".//a")
    alinks = alinks[sapply(alinks, function(x){
      if(!xml2::xml_has_attr(x,"class")) return(FALSE)
      xml2::xml_attr(x, "class") == "ship-link"
    })]
    if(length(alinks)>0){
      link <- paste0("https://www.vesselfinder.com", sapply(alinks, function(x){xml2::xml_attr(x,"href")}))
    }
    
    
    html2 = httr::content(httr::GET(link, httr::add_headers("User-Agent" = "vesselFindeR")))
    df<-html2%>%
      rvest::html_table("tparams", header=F)
    df<-df[[1]]
    
    
    imgs = xml2::xml_find_all(html2, ".//img")
    imgs = imgs[sapply(imgs, function(x){
      if(!xml2::xml_has_attr(x, "class")) return(FALSE)
      xml2::xml_attr(x, "class") == "main-photo"
    })]
    if(length(imgs)>0){
      df <- rbind(c('img_href',xml2::xml_attr(imgs[[1]],"src")), df)
      names(df) <- c('Description','Value')
      
      df <- df[c(1,9,11,10,7,8),]
      
      extra_vessel_finder <- data.frame(
        Description = c('Speed', 'Trawling Speed', 'Power'),
        Value = c('-', '-', '-')
      )
      
      df <- rbind(df,extra_vessel_finder)
      df <- rbind(c('link',link), df)
    }else{
      
      df <- data.frame(
        Value = c(NA,"./assets/img/placeholders/vessel.png")
      )
    }
    
    return(df)
    
  },error=function(e){
    
    #if there is failure in internet connection use the placeholder image 
    df <- data.frame()
  })
}


#LicenseValidity
LicenseValidity <- function(ls_data, validity_names = c('valid', 'expired')){
  ls_data$Valid_to_date <- as.Date(ls_data$Valid_to_date)
  
  valid_to_date <- ls_data$Valid_to_date
  
  ls_data$Validity <- NA
  
  for (i in 1:length(valid_to_date)) {
    validity_status <- Sys.Date()-valid_to_date[i]
    
    if(validity_status<0){
      ls_data$Validity[i] <- validity_names[1]
    }else{
      
      ls_data$Validity[i] <- validity_names[2]
    }
  }
  
  return(ls_data)
}



