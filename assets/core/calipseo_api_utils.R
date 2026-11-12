#CALIPSEO API (OIDC)
#-----------------------------------------------------------------------------------------------------
#When the Calipseo API is configured, some data is read from calipseo-app (/api/analysis/**) instead of
#the database: calipseo-app applies the user's rights and data silo server-side.
#
#Configuration:
#  - API base URL: env var CALIPSEO_API (set by ShinyProxy container-env), or openfismis$api$url in the
#    config file (local development). Not set = everything is read from the database, as before.
#  - OIDC tokens and token endpoint, env vars set by ShinyProxy (never in the config file):
#    SHINYPROXY_OIDC_ACCESS_TOKEN (optional), SHINYPROXY_OIDC_REFRESH_TOKEN ("#{oidcUser.refreshToken}"),
#    OIDC_TOKEN_URL, OIDC_CLIENT_ID, OIDC_CLIENT_SECRET
#
#ShinyProxy gives the tokens to the container once, when it starts, and never updates them. The access
#token expires after a few minutes, so the app gets a new one itself with the refresh token. When
#Keycloak refuses the refresh token (Keycloak session expired or logged out), the user is asked to log in
#again: a plain reload is not enough, ShinyProxy would reconnect to the same container and its old tokens.
#With ShinyProxy each user has their own container (R process), so the tokens kept here are the user's.

calipseo_api <- new.env()

#calipseoAPIUrl
calipseoAPIUrl <- function(){
  url <- Sys.getenv("CALIPSEO_API")
  if(!nzchar(url) && !is.null(appConfig$openfismis$api$url)) url <- appConfig$openfismis$api$url
  sub("/+$", "", url)
}

#useCalipseoAPI
useCalipseoAPI <- function(){
  nzchar(calipseoAPIUrl())
}

#jwtExpiry
#@returns the token expiry (seconds since epoch), NA if the token cannot be read
jwtExpiry <- function(token){
  tryCatch({
    payload <- strsplit(token, ".", fixed = TRUE)[[1]][2]
    payload <- chartr("-_", "+/", payload)
    payload <- paste0(payload, strrep("=", (4 - nchar(payload) %% 4) %% 4))
    as.numeric(jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(payload)))$exp)
  }, error = function(e) NA_real_)
}

#showSessionExpiredModal
#The button opens the ShinyProxy logout (<ShinyProxy base path>/logout): ShinyProxy stops this container and
#sends the user to the Keycloak login, then starts a new container with new tokens. The base path is taken
#from the page URL (/app/, /app_i/, /app_direct/, /app_direct_i/ or /app_proxy/ segment), so it works for
#ShinyProxy under a sub-path (e.g. /shinyproxy) and for the app shown in the ShinyProxy page (iframe).
showSessionExpiredModal <- function(session){
  t <- function(key) appConfig$translator$t(key)
  login_again <- paste(
    "var w = window; try { if (window.top.location.pathname) w = window.top; } catch(e) {}",
    "var m = w.location.pathname.match(/^(.*?)\\/(app|app_i|app_direct|app_direct_i|app_proxy)\\//);",
    "if (m) { w.location.href = m[1] + '/logout'; } else { w.location.reload(); }"
  )
  shiny::showModal(shiny::modalDialog(
    title = t("SESSION_EXPIRED_TITLE"),
    t("SESSION_EXPIRED_MESSAGE"),
    footer = shiny::tags$button(
      type = "button", class = "btn btn-primary",
      onclick = login_again,
      t("SESSION_EXPIRED_LOGIN")
    ),
    easyClose = FALSE
  ), session = session)
}

#calipseoSessionExpired
#Shows the "session expired" dialog and stops the current output/observer silently (shiny::req),
#so the Shiny session is not disconnected. Outside a Shiny session, it raises an error.
calipseoSessionExpired <- function(reason){
  WARN("Calipseo API: session expired (%s)", reason)
  session <- shiny::getDefaultReactiveDomain()
  if(!is.null(session)){
    showSessionExpiredModal(session)
    shiny::req(FALSE)
  }
  stop(sprintf("Calipseo API session expired (%s)", reason), call. = FALSE)
}

#refreshCalipseoAPIToken
refreshCalipseoAPIToken <- function(){
  if(!nzchar(calipseo_api$refresh)) calipseoSessionExpired("no refresh token")
  token_url <- Sys.getenv("OIDC_TOKEN_URL")
  if(!nzchar(token_url)) stop("Calipseo API: OIDC_TOKEN_URL is not set (ShinyProxy container-env)", call. = FALSE)
  DEBUG("Calipseo API: refreshing the OIDC access token")
  resp <- httr::POST(token_url, encode = "form", httr::timeout(30), body = list(
    grant_type = "refresh_token",
    refresh_token = calipseo_api$refresh,
    client_id = Sys.getenv("OIDC_CLIENT_ID"),
    client_secret = Sys.getenv("OIDC_CLIENT_SECRET")
  ))
  status <- httr::status_code(resp)
  body <- tryCatch(httr::content(resp, as = "parsed", type = "application/json"), error = function(e) list())
  if(status == 200 && !is.null(body$access_token)){
    calipseo_api$access <- body$access_token
    #Keycloak returns a new refresh token on each refresh
    if(!is.null(body$refresh_token)) calipseo_api$refresh <- body$refresh_token
    return(invisible(TRUE))
  }
  #invalid_grant = the refresh token is no longer valid (session expired, logged out, user disabled)
  if(identical(body$error, "invalid_grant")) calipseoSessionExpired(sprintf("refresh refused: %s", body$error_description))
  #anything else (wrong client secret, Keycloak down...) is a configuration or server problem
  stop(sprintf("Calipseo API: OIDC token refresh failed (HTTP %s %s)", status, if(is.null(body$error)) "" else body$error), call. = FALSE)
}

#calipseoAPIToken
#@returns a valid access token, refreshed when it expires within the next 30 seconds
calipseoAPIToken <- function(force_refresh = FALSE){
  if(is.null(calipseo_api$access)){
    calipseo_api$access <- Sys.getenv("SHINYPROXY_OIDC_ACCESS_TOKEN")
    calipseo_api$refresh <- Sys.getenv("SHINYPROXY_OIDC_REFRESH_TOKEN")
    if(!nzchar(calipseo_api$access) && !nzchar(calipseo_api$refresh)){
      stop("Calipseo API: no OIDC token, the app must run behind ShinyProxy with OpenID authentication", call. = FALSE)
    }
  }
  expiry <- if(nzchar(calipseo_api$access)) jwtExpiry(calipseo_api$access) else NA_real_
  if(force_refresh || is.na(expiry) || expiry - as.numeric(Sys.time()) < 30) refreshCalipseoAPIToken()
  calipseo_api$access
}

#calipseoAPIGet
#GET on the Calipseo API, with the user's token. A 401 is retried once with a new token (e.g. revoked);
#a second 401 means the user must log in again.
#@returns the parsed JSON
calipseoAPIGet <- function(path, query = NULL){
  start <- Sys.time()
  url <- paste0(calipseoAPIUrl(), path)
  get <- function(token){
    httr::GET(url, query = query, httr::timeout(120), httr::add_headers(Authorization = paste("Bearer", token)))
  }
  resp <- get(calipseoAPIToken())
  if(httr::status_code(resp) == 401){
    resp <- get(calipseoAPIToken(force_refresh = TRUE))
    if(httr::status_code(resp) == 401) calipseoSessionExpired(sprintf("%s still answers 401 with a new token", path))
  }
  if(httr::status_code(resp) != 200){
    stop(sprintf("Calipseo API: %s returned HTTP %s", path, httr::status_code(resp)), call. = FALSE)
  }
  out <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
  time <- Sys.time() - start
  DEBUG("⏳ Calipseo API %s processing time: %s %s", path, as(time, "numeric"), attr(time, "units"))
  out
}

#<MODULE:HOME>
#accessCountersFromAPI
#All home counters in one call (/api/analysis/counters), kept 10 seconds so that the info boxes share it.
#A counter the user has no right for is not in the result.
accessCountersFromAPI <- function(){
  if(is.null(calipseo_api$counters) || difftime(Sys.time(), calipseo_api$counters_time, units = "secs") > 10){
    DEBUG("Query home counters (API)")
    calipseo_api$counters <- calipseoAPIGet("/api/analysis/counters")
    calipseo_api$counters_time <- Sys.time()
  }
  calipseo_api$counters
}

#countFromAPI
#@returns the counter value, NA when the user has no right for it
countFromAPI <- function(name){
  value <- accessCountersFromAPI()[[name]]
  if(is.null(value)) NA_integer_ else value
}
