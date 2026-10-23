
#'@name set_translator
#'@title set_translator
#'@description set i18n translator will load standard translations and store them
#'into a \link[shiny.i18n]{Translator}.
#'@param config a Calipseo shiny config
#'@return the translator
set_translator = function(config){
  i18n_translator = shiny.i18n::Translator$new(
    translation_csvs_path = file.path("i18n", "translations"),
    translation_csv_config = file.path("i18n", "config.yml")
  )
  language = if(!is.null(config$language)) config$language else "en"
  i18n_translator$set_translation_language(language)
  return(i18n_translator)
}

#'@name set_translation_language
#'@title Set translation language
#'@description set_translation_language
#'@param lang lang
#'@return the config with translator having changed the language
#'@note Requires the global 'appConfig' object
set_translation_language <- function(lang){
  appConfig$translator$set_translation_language(lang)
  #add any package internal translation language here
  fdishinyr::set_translation_language(lang)
  artfishr::set_translation_language(lang)
}

#' @name get get i18n reactive translator
#' @param lang a string or reactive
#'@note Requires the global 'appConfig' object
get_reactive_translator <- function(lang){
  reactive({
    if(is.reactive(lang)){
      appConfig$set_translation_language(lang())
    }else{
      if(!is.null(lang)) set_translation_language(lang)
    }
    appConfig$translator
  })
}