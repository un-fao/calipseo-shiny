#authLoginUI
authLoginUI <- function (id, config, title = i18n("AUTH_TITLE"), user_title = i18n("AUTH_USERNAME"), 
                         pass_title = i18n("AUTH_PASSWORD"), login_title = i18n("AUTH_LOGIN_TITLE"), login_btn_class = "btn-primary", 
                         error_message = i18n("AUTH_LOGIN_ERROR"), additional_ui = NULL, 
                         cookie_expiry = 7) 
{
  
  ns <- shiny::NS(id)
  shinyjs::hidden(
    shiny::div(
      id = ns("panel"), style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;", 
      shiny::wellPanel(
        shinyjs::useShinyjs(), 
        shinyauthr:::jscookie_script(),
        shinyjs::extendShinyjs(
          text = shinyauthr:::js_cookie_to_r_code(ns("jscookie"), expire_days = cookie_expiry), 
          functions = c("getcookie","setcookie", "rmcookie")
        ), 
        shinyjs::extendShinyjs(text = shinyauthr:::js_return_click(ns("password"), ns("auth_button_login")), functions = c()),
        shiny::tags$p(
          shiny::tags$a(
            href=config$country_profile$website,
            HTML(createBase64Image(src = config$country_profile$logo, width = '50px', alt = 'Logo'))
          ),
          class = "text-center", style = "padding-top: 0;"
        ),
        shiny::tags$h1(tags$b(sprintf("R Shiny Calipseo %s", config$country_profile$data$SHORT_NAME)), class = "text-center", style = "padding-top: 0;"),
        shiny::tags$p(title, class = "text-center", style = "padding-top: 0;"),
        shiny::textInput(ns("auth_username"), shiny::tagList(shiny::icon("user"), user_title)),
        shiny::passwordInput(ns("auth_password"), shiny::tagList(shiny::icon("unlock-alt"), pass_title)), 
        shiny::div(style = "text-align: center;", shiny::tags$button(id = ns("auth_button_login"), type = "button", class = paste("btn", login_btn_class, "action-button"), style = "margin:0px;width:100%;", login_title)),
        additional_ui,
        shinyjs::hidden(shiny::div(id = ns("error"), shiny::tags$p(error_message, style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center"))),
        style = "padding:50px;"
      )
    )
  )
}