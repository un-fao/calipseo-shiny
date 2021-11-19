
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

#imgToBase64
imgToBase64 <- function (file, Rd = FALSE, alt = "image") {
  input <- normalizePath(file, mustWork = TRUE)
  buf <- readBin(input, raw(), file.info(input)$size)
  base64 <- sprintf("data:image/png;base64,\n%s", openssl::base64_encode(buf, linebreaks = FALSE))
  return(base64)
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


#createplaceholderImage
createPlaceholderImage <- function(url){
  vessel_placeholder_image <- tags$img(src= imgToBase64(url),height='100px')
  return(vessel_placeholder_image)
}


