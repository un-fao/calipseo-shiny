#vesselFindeR
vesselFindeR <- function(name, flag_iso2){
  
  tryCatch({
    
    html = httr::content(httr::GET(sprintf("https://www.vesselfinder.com/vessels?name=%s&flag=%s", name, flag_iso2), httr::add_headers("User-Agent" = "vesselFindeR")))
    
    #links
    alinks = xml2::xml_find_all(html, ".//a")
    alinks = alinks[sapply(alinks, function(x){
      if(!xml2::xml_has_attr(x,"class")) return(FALSE)
      xml2::xml_attr(x, "class") == "ship-link"
    })]
    if(length(alinks)>0){
      link <- paste0("https://www.vesselfinder.com", sapply(alinks, function(x){xml2::xml_attr(x,"href")}))
    }
    
    
    html2<- xml2::read_html(link)
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
    }
    
    return(df)
    
  },error=function(e){
    
    #if there is failure in internet connection use the placeholder image 
    df <- data.frame()
  })
}


#customeinfobox
custome_infoBox <- function(title = NULL, value = NULL, icon = NULL,
                            width=3, color = 'aqua',text_color = 'white') {
  
  tags$div( class= paste0('col-sm-',width),
            tags$div(class=paste0('info-box bg-',color),
                     style=paste0('background-color:',color,';'),
                     tags$span( class='info-box-icon',
                                style=paste0("color:",text_color,';'),
                                
                                icon),
                     
                     tags$div(
                       class='info-box-content',
                       style=paste0("color:",text_color,';'),
                       
                       tags$span( class='info-box-text',
                                  tags$span( style="font-size:10px;",
                                             title)),
                       tags$span(class='info-box-number',
                                 value)
                     )
                     
            )
  )
  
}

