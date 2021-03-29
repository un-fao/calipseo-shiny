#loadModuleScripts
loadModuleScripts <- function(config){
  
  scripts <- list.files(path = "./modules", pattern = ".json", recursive = TRUE, full.names = TRUE)
  modules <- data.frame(
    name = sapply(scripts, function(x){unlist(strsplit(unlist(strsplit(x, paste0(dirname(x),"/")))[2], ".json"))[1]}),
    dirname = sapply(scripts, dirname),
    stringsAsFactors = FALSE
  )
  for(i in 1:nrow(modules)){
    module <- modules[i,]
    message(sprintf("-> Shiny module '%s'", module$name))
    enabled = TRUE
    module_config = config$modules[[module$name]]
    has_config = !is.null(module_config)
    if(has_config) if(!is.null(module_config$enabled)) enabled = module_config$enabled
    if(enabled){
      message(sprintf("Loading shiny module '%s' scripts...", module$name))
      source(file.path(module$dirname, paste0(module$name, "_server.R")))
      source(file.path(module$dirname, paste0(module$name, "_ui.R")))
    }else{
      message(sprintf("Shiny module '%s' is disabled!", module$name))
    }
  }
}

#loadModuleServers
loadModuleServers <- function(config, pool){
  scripts <- list.files(path = "./modules", pattern = ".json", recursive = TRUE, full.names = TRUE)
  modules <- unique(sapply(scripts, function(x){unlist(strsplit(unlist(strsplit(x, paste0(dirname(x),"/")))[2], ".json"))[1]}))
  for(module in modules){
    message(sprintf("Shiny module '%s'", module))
    enabled = TRUE
    module_config = config$modules[[module]]
    has_config = !is.null(module_config)
    if(has_config) if(!is.null(module_config$enabled)) enabled = module_config$enabled
    if(enabled){
      message(sprintf("Loading shiny module '%s' functions...", module))
      server_fun_name <- paste0(module, "_server")
      server_fun <- try(eval(expr = parse(text = server_fun_name)))
      if(!is(server_fun, "try-error")){
        shiny::callModule(server_fun, module, pool)
      }else{
        message(sprintf("Error while evaluating server function '%s'", server_fun_name))
      }
    }
  }
}

#loadModuleUIs
loadModuleUIs <- function(config){
  scripts <- list.files(path = "./modules", pattern = ".json", recursive = TRUE, full.names = TRUE)
  modules <- unique(sapply(scripts, function(x){unlist(strsplit(unlist(strsplit(x, paste0(dirname(x),"/")))[2], ".json"))[1]}))
  module_uis <- lapply(modules, function(module){
    out <- NULL
    message(sprintf("Shiny module '%s'", module))
    enabled = TRUE
    module_config = config$modules[[module]]
    has_config = !is.null(module_config)
    if(has_config) if(!is.null(module_config$enabled)) enabled = module_config$enabled
    if(enabled){
      message(sprintf("Loading shiny module '%s' functions...", module))
      ui_fun_name <- paste0(module, "_ui")
      ui_fun <- try(eval(expr = parse(text = ui_fun_name)))
      if(!is(ui_fun, "try-error")){
        out <- ui_fun(module)
      }else{
        message(sprintf("Error while evaluating ui function '%s'", ui_fun_name))
      }
    }
    return(out)
  })
  module_uis <- module_uis[!sapply(module_uis, is.null)]
  ui <- do.call("tabItems", module_uis)
  return(ui)
}

#createSidebarfromModules
sidebarMenuFromModules <- function(config){
  
  #default modules
  default_module_profiles = list.files(path = "./modules", pattern = ".json", recursive = TRUE, full.names = TRUE)
  
  #extend default module profiles in case custom configs are availble
  module_profiles = lapply(default_module_profiles, function(module_profile){
    #read module profile
    module_profile_name <- unlist(strsplit(unlist(strsplit(module_profile, paste0(dirname(module_profile),"/")))[2], ".json"))[1]
    outp <- jsonlite::read_json(module_profile)
    outp$source <- module_profile
    outp$module <- module_profile_name
    outp$enabled <- TRUE
    #overwrite with config module definition
    m_config <- config$modules[[outp$module]]
    if(!is.null(m_config)){
      for(m_config_propname in names(m_config)){
        if(m_config_propname %in% names(outp)){
          print(m_config_propname)
          outp[[m_config_propname]] <- m_config[[m_config_propname]]
        }
      }
    }
    return(outp)
  })
  #TODO custom modules?

  #default structure
  default_structure_menu_items <- list(
    home = list(title = "Home"),
    vessels = list(title = "Vessels")
  )
  structure_menu_items <- default_structure_menu_items
  #overwrite with custom structure
  if(!is.null(config$structure)) structure_menu_items <- config$structure
  
  #sidebar UI
  do.call("sidebarMenu", c(id="calipseo-tabs", lapply(names(structure_menu_items), function(menu_item_name){
    #menu item
    menu_item <- structure_menu_items[[menu_item_name]]
    menu_item_enabled <- TRUE
    if(!is.null(menu_item$enabled)) menu_item_enabled <- menu_item$enabled
    if(!menu_item_enabled) return(NULL)
    
    #profiles of modules associated to this menu item
    menu_item_profiles <- module_profiles[sapply(module_profiles, function(x){x$parent == menu_item_name})]
    menu_item_profiles <- menu_item_profiles[order(sapply(menu_item_profiles, function(x){x$rank}))]
    #ui for menu item
    do.call("menuItem", c(
      text = menu_item$title, tabName = menu_item_name,
      lapply(menu_item_profiles, function(profile){
        if(!profile$enabled) return(NULL)
        icon = shiny::icon("angle-double-right")
        if(!is.null(profile$icon)) icon = shiny::icon(profile$icon)
        menuSubItem(profile$title, tabName = profile$module, icon = icon)
      })
    ))
  })))
}

