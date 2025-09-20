#listModuleProfiles
listModuleProfiles <- function(config, core = TRUE, country = TRUE){
  default_module_profiles <- list.files(path = "./modules", pattern = ".json", recursive = TRUE, full.names = TRUE)
  default_module_profiles <- default_module_profiles[regexpr("i18n", default_module_profiles)<0]
  filter <- NULL
  if(core) filter <- regexpr("core", default_module_profiles) > 0
  if(country){
    if(is.null(core)){
      filter <- regexpr(config$country_profile$iso3, default_module_profiles) > 0
    }else{
      filter <- filter | regexpr(config$country_profile$iso3, default_module_profiles) > 0
    }
  }
  if(!is.null(filter)){
    default_module_profiles <- default_module_profiles[filter]
  }
  return(default_module_profiles)
}

#loadModuleProfile
loadModuleProfile <- function(filename){
  profile <- jsonlite::read_json(filename)
  if(is.null(profile$enabled)) profile$enabled <- TRUE
  if(is.null(profile$menu)) profile$menu <- FALSE
  if(is.null(profile$icon)) profile$icon <- "angle-double-right"
  return(profile)
}

#loadModuleScripts
loadModuleScripts <- function(config){
  default_module_profiles <- listModuleProfiles(config)
  modules <- data.frame(
    name = sapply(default_module_profiles, function(x){unlist(strsplit(unlist(strsplit(x, paste0(dirname(x),"/")))[2], ".json"))[1]}),
    dirname = sapply(default_module_profiles, dirname),
    stringsAsFactors = FALSE
  )
  for(i in 1:nrow(modules)){
    module <- modules[i,]
    enabled = TRUE
    module_config = config$modules[[module$name]]
    has_config = !is.null(module_config)
    if(has_config) if(!is.null(module_config$enabled)) enabled = module_config$enabled
    if(enabled){
      INFO("Loading shiny module '%s' scripts...", module$name)
      source(file.path(module$dirname, paste0(module$name, "_server.R")))
      source(file.path(module$dirname, paste0(module$name, "_ui.R")))
    }else{
      WARN("Shiny module '%s' is disabled!", module$name)
    }
  }
}

#getModuleI18nTerms
getModuleI18nTerms <- function(){
  module_i18n_files <- list.files(path = "./modules", pattern = "i18n", recursive = TRUE, full.names = TRUE)
  module_i18n_files <- sapply(module_i18n_files, function(x){unlist(strsplit(x,"\\.json"))[1]})
  names(module_i18n_files) <- NULL
  available_i18n_languages <- unique(sapply(module_i18n_files, function(x){
    file_parts <- unlist(strsplit(x,"_"))
    return(file_parts[[length(file_parts)]])
  }))
  i18n <- lapply(available_i18n_languages,
    function(lang){
     i18n_files <- list.files(path = "./modules", pattern = sprintf("i18n_%s.json", lang), recursive = TRUE, full.names = TRUE)
     i18n_terms <- do.call("c", lapply(i18n_files, jsonlite::read_json))
     return(i18n_terms)
    }
  )
  names(i18n) <- available_i18n_languages
  return(i18n)
}

#i18n
i18n <- function(term){
  i18n_term <- appConfig$i18n[[appConfig$language]][[term]]
  if(is.null(i18n_term)) i18n_term <- paste0("[",term,"_", toupper(appConfig$language),"]")
  return(i18n_term)
}

#listLinkedModules
listLinkedModules <- function(id, config){
  module_file <- NULL
  core_module_file = paste0("./modules/core/",id,".json")
  is_core_module = file.exists(core_module_file)
  is_cnt_module = FALSE
  if(!is_core_module){
    cnt_module_file = sprintf("./modules/country/%s/%s.json", config$country_profile$iso3, id)
    is_cnt_module = file.exists(cnt_module_file)
    if(is_cnt_module) module_file = cnt_module_file
  }else{
    module_file = core_module_file
  }
  if(!is_core_module & !is_cnt_module) return(list());
  
  print(module_file)
  module_profile <- loadModuleProfile(module_file)
  return(module_profile$linked_modules)
}

#loadModuleServer
loadModuleServer <- function(id, session, config, pool, module_state, reloader){
  id_out = id
  load_module <- !id %in% module_state$initialized
  if(load_module){
    INFO("Module server '%s' not yet initialized.", id)
    attr(id_out, "status") = "initialize"
  }else{
    INFO("Module server '%s' already initialized. Check if it needs to be reloaded", id)
    load_module <- id %in% module_state$toreload | #exact matching
                   any(sapply(module_state$toreload, regexpr, id) > 0) | #regexpr matching
                   '*' %in% module_state$toreload | #wildcard matching
                   regexpr("_info", id) > 0 #specific matching for '_info' modules (systematic reload for record handling)
    if(load_module){
      INFO("Module '%s' is listed in modules to be reloaded.", id)
      attr(id_out, "status") = "reload"
    }else{
      INFO("Module '%s' is not reloaded", id)
      attr(id_out, "status") = "display"
    }
  }
  if(load_module){
    server_fun_name <- paste0(id, "_server")
    server_fun <- try(eval(expr = parse(text = server_fun_name)), silent = TRUE)
    if(!is.null(server_fun)){
      if(!is(server_fun, "try-error")){
        called <- try(server_fun(id, session, pool, reloader))
        if(is(called, "try-error")){
          ERROR("Error while calling shiny module '%s'", id)
        }
      }else{
        ERROR("Error while evaluating server function '%s'", server_fun_name)
      }
    }
  }
  return(id_out)
}

#loadModuleServers
#@deprecated
loadModuleServers <- function(config, pool, reloader){
  INFO("=> Loading Module Servers")							   
  default_module_profiles <- listModuleProfiles(config)
  if(!is.null(reloader())){
    core_module_file = paste0("./modules/core/",reloader(),".json")
    is_core_module = file.exists(core_module_file)
    is_cnt_module = FALSE
    if(!is_core_module){
      cnt_module_file = sprintf("./modules/country/%s/%s.json", config$country_profile$iso3, reloader())
      is_cnt_module = file.exists(cnt_module_file)
      if(is_cnt_module) module_file = cnt_module_file
    }else{
      module_file = core_module_file
    }
    if(!is_core_module & !is_cnt_module) return(NULL);
    
    reloader_profile<-loadModuleProfile(module_file)
    modules_to_reload<-reloader_profile$linked_modules
    if("*" %in% modules_to_reload){
      modules_to_reload = list.files(path = "./modules", pattern = ".json", recursive = TRUE, full.names = TRUE)
    }
    if(length(modules_to_reload)>0){
      INFO("Module '%s' has triggered reloading event. The following modules are going to be reload: %s",reloader(),paste0(modules_to_reload,collapse =", "))
      default_module_profiles<-lapply(modules_to_reload, function(x){
        module_out = NULL
        module_path = paste0("./modules/core/",x,".json")
        if(file.exists(module_path)){
          module_out = module_path
        }else{
          module_path = sprintf("./modules/country/%s/%s.json", config$country_profile$iso3, x)
          if(file.exists(module_path)){
            module_out = module_path
          }
        }
        return(module_out)
      })
      default_module_profiles = unlist(default_module_profiles[!sapply(default_module_profiles, is.null)])
    }else{
      return(NULL)
    }
  }  
  for(module_profile in default_module_profiles){
    module <- unlist(strsplit(unlist(strsplit(module_profile, paste0(dirname(module_profile),"/")))[2], ".json"))[1]
    outp <- loadModuleProfile(module_profile)
    if(outp$type != "internal" && !outp$menu){
      enabled = TRUE
      module_config = config$modules[[module]]
      has_config = !is.null(module_config)
      if(has_config) if(!is.null(module_config$enabled)) enabled = module_config$enabled
      if(enabled){
        INFO("%s shiny module '%s' server functions...",ifelse(!is.null(reloader()),"Reloading","Loading"),module)
        server_fun_name <- paste0(module, "_server")
        server_fun <- try(eval(expr = parse(text = server_fun_name)), silent = TRUE)
        if(!is.null(server_fun)){
          if(!is(server_fun, "try-error")){
            called <- try(server_fun(module, pool, reloader))
            if(is(called, "try-error")){
              ERROR("Error while calling shiny module '%s'", module)
            }
          }else{
            ERROR("Error while evaluating server function '%s'", server_fun_name)
          }
        }
      }
    }
  }
}

#loadModuleUIs
loadModuleUIs <- function(config){
  default_module_profiles <- listModuleProfiles(config)
  module_uis <- lapply(default_module_profiles, function(module_profile){
    out <- NULL
    module <- unlist(strsplit(unlist(strsplit(module_profile, paste0(dirname(module_profile),"/")))[2], ".json"))[1]
    outp <- jsonlite::read_json(module_profile)
    if(outp$type != "internal"){
      enabled = TRUE
      module_config = config$modules[[module]]
      has_config = !is.null(module_config)
      if(has_config) if(!is.null(module_config$enabled)) enabled = module_config$enabled
      if(enabled){
        INFO("Loading shiny module '%s' UI functions...", module)
        ui_fun_name <- paste0(module, "_ui")
        ui_fun <- try(eval(expr = parse(text = ui_fun_name)))
        if(!is(ui_fun, "try-error")){
          out <- ui_fun(module)
        }else{
          ERROR("Error while evaluating UI function '%s'", ui_fun_name)
        }
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
  default_module_profiles <- listModuleProfiles(config)
  
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
          outp[[m_config_propname]] <- m_config[[m_config_propname]]
        }
      }
    }
    return(outp)
  })
  #remove internal modules
  module_profiles <- module_profiles[sapply(module_profiles, function(x){x$type != "internal"})]
  
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

