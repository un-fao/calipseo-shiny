
#UI helpers

#footer
footer <- function(id, version, date){
  tags$div(
    tags$p(sprintf("%s - v%s (%s)", id, version, date), style = "float:left;color:white;")
  )
}

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
gicon <- function(x, style = NULL) as.character(icon(x, lib = "glyphicon", style = style))

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



#Age computation
Age_comp <- function(data, Prep = TRUE){
  
  if(Prep != TRUE){
    
    if(data$DOB!=""){
      data <- data[(data$Gender=='Male' | data$Gender=='Female'),]
      
      data$DOB <- as.Date(data$DOB)
      
      data$Age = round(as.numeric(difftime(Sys.Date(),data$DOB, units = "weeks"))/52.25, digits = 0)
      
    }else{data$Age <- ""}
    
  }else{
    
    data <- data[(data$Gender=='Male' | data$Gender=='Female') & !is.na(data$DOB),]
    
    data$DOB <- as.Date(data$DOB)
    
    data$Age = round(as.numeric(difftime(Sys.Date(),data$DOB, units = "weeks"))/52.25, digits = 0)
    
  }
  
  return(data)
  
}


#createplaceholderImage
createPlaceholderImage <- function(id){
  createBase64Image(src = sprintf("./assets/img/placeholders/%s.png", id), height = '150px')
}


#customeinfobox
CalipseoInfoBox <- function(title = NULL, value = NULL, icon = NULL,
                            width=4,box_width=NULL,color = 'aqua',text_color = 'white',style_title = 'font-size:14px',style_value='font-size:18px',
                            icon_width = NULL, content_margin_left = NULL,Use_icon = TRUE,
                            infobox_extra_style = NULL, infobox_icon_extra_style = NULL,infobox_content_extra_style = NULL) {
  
  tags$div(class= paste0('col-sm-',width),
           style=paste0('width:',box_width,';'),
           tags$div(class=paste0('info-box bg-',color),
                    style=paste0('background-color:',color,';',infobox_extra_style),
                    if(isTRUE(Use_icon)){
                      tags$span( class='info-box-icon',
                                 style=paste0("color:",text_color,';width:',icon_width,';',infobox_icon_extra_style),
                                 
                                 icon)},
                    
                    tags$div(
                      class='info-box-content',
                      style=paste0("color:",text_color,';margin-left:',content_margin_left,';',infobox_content_extra_style),
                      
                      tags$span( class='info-box-text',
                                 tags$span(style=paste0("display:block;white-space:nowrap;",style_title,";"),
                                           title)),
                      tags$span(class='info-box-number',
                                style=paste0(style_value,";"),
                                value)
                    )
                    
           )
  )
  
}

#initDTContainer
initDTContainer <- function(df){
  tags$table(
    tags$thead(
      tags$tr(
        lapply(names(df), tags$th),
        role = "row"
      ),
      tags$tr(
        lapply(names(df), function(x){tags$td()}),
        role = "row"
      )
    )
  )
}
