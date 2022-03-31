
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

#JSrenderforlicensetable
js_render_for_license_table <- c(
  'function(data, type, row, meta){',
  '  if(type === "display"){',
  '    var color = data === "ok" ? "green" : "red";',
  '    var validity = data === "ok" ? "The&nbsp;license&nbsp;is&nbsp;valid" : "The&nbsp;license&nbsp;is&nbsp;expired" ',
  '    return "<span style=\\\"color:" + color +',
  '           "; font-size:18px\\\" title=\\\ "+ validity +" \\\"><i class=\\\"glyphicon glyphicon-" +', 
  '           data +"\\\"></i></span>";',
  '  } else {',
  '    return data;',
  '  }',
  '}'
)


#customeinfobox
CalipseoInfoBox <- function(title = NULL, value = NULL, icon = NULL,
                            width=4,box_width=NULL,color = 'aqua',text_color = 'white',style_title = 'font-size:14px',style_value='font-size:18px',
                            icon_width = NULL, content_margin_left = NULL) {
  
  tags$div(class= paste0('col-sm-',width),
           style=paste0('width:',box_width,';'),
           tags$div(class=paste0('info-box bg-',color),
                    style=paste0('background-color:',color,';'),
                    tags$span( class='info-box-icon',
                               style=paste0("color:",text_color,';width:',icon_width,';'),
                               
                               icon),
                    
                    tags$div(
                      class='info-box-content',
                      style=paste0("color:",text_color,';margin-left:',content_margin_left,';'),
                      
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