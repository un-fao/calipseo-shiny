
#UI helpers

#shinyInput
shinyInput <- function(FUN, len, indexes = NULL, id, ns, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    idx <- i
    if(!is.null(indexes)) idx <- indexes[i]
    inputs[i] <- as.character(FUN(paste0(ns(id), idx), ...))
  }
  inputs
}

#downloadButtonCustom
downloadButtonCustom <- function (outputId, label = "Download", class = NULL, href = "", icon = icon("download"), ...) {
  aTab <- tags$a(
    id = outputId, 
    class = paste("btn btn-default shiny-download-link", class),
    href = href,
    target = "_blank", 
    download = NA, 
    icon, 
    label, 
    ...
  )
}


#gicon
gicon <- function(x) as.character(icon(x, lib = "glyphicon"))

#userTooltip
userTooltip <- function(text, style = ""){
  tt <- sprintf("<span class=\"glyphicon glyphicon-info-sign user-tooltip\" title=\"%s\" style=\"%s\"></span>", 
                text, style)
  return(tt)
}

#updatePageUrl
updatePageUrl <- function(page, session){
  updateQueryString(
    queryString = sprintf("?page=%s", page), 
    mode = "push", session
  )
}


#createBase64Image
createBase64Image <- function(src, width = "auto", height = "auto", alt = "image", Rd = FALSE){
  
  input <- NULL
  isSrcUrl <- regexpr("(http|https)[^([:blank:]|\\\"|<|&|#\n\r)]+", src) > 0
  if(isSrcUrl){
    input = tempfile(fileext = ".jpeg")
    download.file(url = src, destfile = input, mode = "wb", quiet = TRUE)
  }else{
    input <- normalizePath(src, mustWork = TRUE)
  }
 
  buf <- readBin(input, raw(), file.info(input)$size)
  base64 <- openssl::base64_encode(buf, linebreaks = FALSE)
  out <- sprintf("%s<img src=\"data:image/png;base64,\n%s\" alt=\"%s\" width=\"%s\" height=\"%s\" />%s",
                 if (Rd)
                   "\\out{"
                 else "", base64, alt, width, height, if (Rd)
                   "}"
                 else "")
  if(isSrcUrl) unlink(input)
  
  return(out)
}



#createplaceholderImage
createPlaceholderImage <- function(id){
  createBase64Image(src = sprintf("./assets/img/placeholders/%s.png", id), height = '150px')
}



#customeinfobox
custome_infoBox <- function(title = NULL, value = NULL, icon = NULL, 
                            color = NULL,width=1, width_info_box=NULL) {
  
  if(is.null(color)){
    
    bgcolor <- c(0, 192, 239)
  }else{
    
    bgcolor <- c(0, 192, 239)
  }
  
  bgcolor_b <- paste0(bgcolor, collapse = ',')
  
  
  darker_rgb <-c()
  darker_color<- function(bgcolor) {
    
    for(i in 1:length(bgcolor)) {
      darker_rgb[i] <- bgcolor[i]-(50)
    }
    return(darker_rgb)
  }
  
  bgcolor_icon_box <- paste0(darker_color(bgcolor), collapse = ',')
  
  
  if(is.null(width_info_box)){
    width_info_box <- '115px'
  }
  
  if(!is.null(icon)){
    
    
    style_icon_box <- paste0("width:5%;height:42px;width:50px;text-align:center;padding-top:5px;display:block;float:left;",
                             "background-color:rgb(",bgcolor_icon_box,');')
    
    style_info_box <- paste0("display:inline-block;height:42px;",
                             "background-color:rgb(",bgcolor_b,');width:',width_info_box,';')
    
    tags$span(
      tags$span(class='col-md-1',
                style= style_icon_box,
                icon(icon,class = 'fa-2x')), 
      
      div(class=if(!is.null(width)&&!is.null(width_info_box))paste0('col-md-',width),
          style= style_info_box,
          div(class='row',title),div(class='row',tags$b(value, style='padding-left:5px;'))))
    
  }
  
  
  
}


