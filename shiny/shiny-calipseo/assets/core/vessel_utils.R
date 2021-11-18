#createBase64Image
createBase64Image <- function(url, width = "auto", height = "auto", alt = "image", Rd = FALSE){
  
  
  if(is.null(url)==FALSE)
    
  {
    
    input = tempfile(fileext = ".jpeg")
    download.file(url = url, destfile = input, mode = "wb", quiet = TRUE)
    buf <- readBin(input, raw(), file.info(input)$size)
    base64 <- openssl::base64_encode(buf, linebreaks = FALSE)
    out <- sprintf("%s<img src=\"data:image/png;base64,\n%s\" alt=\"%s\" width=\"%s\" height=\"%s\" />%s", 
                   if (Rd) 
                     "\\out{"
                   else "", base64, alt, width, height, if (Rd) 
                     "}"
                   else "")
    unlink(input)
    
    
  }else{
    
    placeholder = "placeholder_pic.png"
    
    out <- as.character(tags$img(src = placeholder, height='100'))
  }
  return(out)
}

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

