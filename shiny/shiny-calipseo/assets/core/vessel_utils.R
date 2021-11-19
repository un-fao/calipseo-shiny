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

