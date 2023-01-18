#initAuthSessionVariables
initAuthSessionVariables <- function(session, auth_info){
  session$userData$CALIPSEO_AUTH_USER = auth_info$user
  session$userData$CALIPSEO_AUTH_PWD = auth_info$pwd
  if(!is.na(auth_info$token)) session$userData$CALIPSEO_AUTH_TOKEN = auth_info$token
}

#resetAuthSessionVariables
resetAuthSessionVariables <- function(session){
  session$userData$CALIPSEO_AUTH_USER = NULL
  session$userData$CALIPSEO_AUTH_PWD = NULL
  session$userData$CALIPSEO_AUTH_TOKEN = NULL
}