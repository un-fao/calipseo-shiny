#vesselFindeR
vesselFindeR <- function(name, flag_iso2){
  
  tryCatch({
    
    html = httr::content(httr::GET(sprintf("https://www.vesselfinder.com/vessels?name=%s&flag=%s", name, flag_iso2), httr::add_headers("User-Agent" = "vesselFindeR")))
    tbl = xml2::xml_find_all(html, ".//table")
    if(length(tbl)==0) return(NULL)
    tbl = tbl[[1]]
    df = rvest::html_table(tbl)
    
    #links
    alinks = xml2::xml_find_all(html, ".//a")
    alinks = alinks[sapply(alinks, function(x){
      if(!xml2::xml_has_attr(x,"class")) return(FALSE)
      xml2::xml_attr(x, "class") == "ship-link"
    })]
    if(length(alinks)>0){
      df$link <- paste0("https://www.vesselfinder.com", sapply(alinks, function(x){xml2::xml_attr(x,"href")}))
    }
    
    #keep first one
    df = df[1L,]
    df = as.list(df)
    
    #go to pick up details and image
    html2 = httr::content(httr::GET(df$link, httr::add_headers("User-Agent" = "vesselFindeR")))
    imgs = xml2::xml_find_all(html2, ".//img")
    imgs = imgs[sapply(imgs, function(x){
      if(!xml2::xml_has_attr(x, "class")) return(FALSE)
      xml2::xml_attr(x, "class") == "main-photo"
    })]
    if(length(imgs)>0){
      df$img_href = xml2::xml_attr(imgs[[1]],"src")
    }
    return(df)
    
  },error=function(e){
    
    #if there is failure in internet connection use the placeholder image 
    df <- NULL
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
