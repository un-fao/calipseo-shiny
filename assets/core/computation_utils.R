#Process I/O accessors
#-----------------------------------------------------------------------------------------------------

#getProcessOutput
getProcessOutputs <- function(config, id, year, quarter = NULL, month = NULL, target = "release"){
  if(target == "release+staging"){
    out <- rbind(
      getProcessOutputs(config, id, year, quarter, month, target = "release"),
      getProcessOutputs(config, id, year, quarter, month, target = "staging")
    )
  }else{
    filepath <- file.path(config$store, target, id, year)
    if(!is.null(quarter)) filepath <- file.path(filepath, paste0("Q",quarter))
    if(!is.null(month)) filepath <- file.path(filepath, paste0("M",month))
    files <- list.files(filepath,recursive = T,full.names = T, pattern = ".csv")
    files <- files[regexpr("archive", files) < 0]
    out <- do.call("rbind", lapply(files, readr::read_csv))
  }
  
  return(out)
  
}

#getStatPeriods
getStatPeriods <- function(config, id, target = c("release", "staging", "release+staging")) {
  
  target <- match.arg(target)
  
  if(target == "release+staging"){
    return(rbind(
      getStatPeriods(config, id, target = "release"),
      getStatPeriods(config, id, target = "staging")
    ))
  }
  
  target_folder <- sprintf("%s/%s/%s", config$store, target, id)
  files <- list.files(target_folder, recursive = TRUE, full.names = TRUE)
  files <- files[regexpr("archive", files) < 0]
  
  if (length(files) == 0)
    return(data.frame())
  
  rel_paths <- sub(paste0("^", target_folder, "/"), "", files)
  parts <- strsplit(rel_paths, "/")
  
  years <- sapply(parts, function(p) p[1])
  
  second <- sapply(parts, function(p) if (length(p) >= 2) p[2] else NA)
  
  by_quarter <- any(!is.na(second) & startsWith(second, "Q"))
  by_month   <- any(!is.na(second) & startsWith(second, "M"))
  
  if (by_quarter) {
    out <- data.frame(
      year = years,
      quarter = ifelse(startsWith(second, "Q"), second, NA),
      file = files
    )
  } else if (by_month) {
    out <- data.frame(
      year = years,
      month = ifelse(startsWith(second, "M"), second, NA),
      file = files
    )
  } else {
    out <- data.frame(
      year = years,
      file = files
    )
  }
  
  return(out)
}

#getAvailablePeriods
getAvailablePeriods <- function(id, config, indicators){
  indicator = indicators[sapply(indicators, function(x){x$id == id})][[1]]
  DEBUG("Get available periods for indicator '%s'", indicator$id)
  available_periods <- unlist(indicator$compute_by$available_periods)
  period<-indicator$compute_by$period
  period <- switch(period,
                   "year" = c("year"),
                   "quarter" = c("year","quarter"),
                   "month" = c("year", "month")
  )

  common_periods<-lapply(available_periods, function (x) {
    available_periods_parts <- unlist(strsplit(x, ":"))
    period_key <- available_periods_parts[1]
    period_value <- available_periods_parts[2]
    
    available_periods_new <- switch(period_key,
      "data" = eval(parse(text=paste0(period_value, "(con = pool)"))),
      "process" = getAvailablePeriods(id = period_value, config = config, indicators = indicators)
    )
    
    if(all(period %in% names(available_periods_new))){
      available_periods_new <- unique(available_periods_new[period])
    }else{
      available_periods_new <- available_periods_new %>%
        mutate("quarter"= case_when(month %in% c(1:3)~"Q1",
                                    month %in% c(4:6)~"Q2",
                                    month %in% c(7:9)~"Q3",
                                    month %in% c(10:12)~"Q4"))
      available_periods_new <- unique(available_periods_new[period])
    }
  })
  
  if(length(common_periods)>1){
    common_periods<-do.call("intersection",common_periods)
  }else{
    common_periods<-as.data.frame(common_periods)
  }
  
  return(common_periods)
}

#formatAvailablePeriods
formatAvailablePeriods <- function(available_periods, indicator){
  available_periods_new <- subset(available_periods, !is.na(year))
  if("month" %in% indicator$compute_by$period){
    available_periods_new <- subset(available_periods_new, !is.na(month))
    available_periods_new$period <- paste0(available_periods_new$year,"-","M", available_periods_new$month)
    available_periods_new<- available_periods_new %>% arrange(desc(year),month)
  }
  if("quarter" %in% indicator$compute_by$period){
    available_periods_new <- subset(available_periods_new,!is.na(quarter))
    available_periods_new$period <- paste0(available_periods_new$year,"-","Q", available_periods_new$quarter)
    available_periods_new <- available_periods_new %>% arrange(desc(year),quarter) %>% as.data.frame()
  }
  if("year" %in% indicator$compute_by$period){
    available_periods_new$period <- available_periods_new$year
    available_periods_new <- available_periods_new %>% arrange(desc(year)) %>% as.data.frame()
  }
  return(available_periods_new)
}

#getFullPeriods
getFullPeriods <- function(available_periods, indicator){
  full_periods <- NULL
  years <- seq(min(available_periods$year), max(available_periods$year))
  if("month" %in% indicator$compute_by$period){
    full_periods <- data.frame(
      year = rep(years, each=12),
      month = rep(1:12, length(years))
    )
    full_periods$Period <- paste0(full_periods$year,"-","M",full_periods$month)
  }
  
  if("quarter" %in% indicator$compute_by$period){
    full_periods <- data.frame(
      year = rep(years, each=4),
      quarter = rep(1:4, length(years))
    )
    full_periods$Period <- paste0(full_periods$year,"-","Q",full_periods$quarter)
  }
  
  if("year" %in% indicator$compute_by$period){
    full_periods <- data.frame(
      year = years,
      Period = years
    )
  }
  return(full_periods)
}

#IsReleasable
isReleasable <- function(id, target_period, config, indicators){
  
  indicator <- indicators[sapply(indicators, function(x){x$id == id})][[1]]
  
  available_periods<-unlist(indicator$compute_by$available_periods)
  
  result<-sapply(available_periods, function (x) {
    available_periods_parts <- unlist(strsplit(x, ":"))
    period_key <- available_periods_parts[1]
    period_value <- available_periods_parts[2]
    
    releasable<-TRUE
    if(period_key=="data"){
      #TODO?
      #for now there check over input data coverage
      #to discuss if we need to be more permissive on isReleasable and leave this to user
      #with a warning in case data coverage is not full for the indicator to be released
      #instead of disabling the 'release' button
      return(releasable)
    }else{
      #check over dependent indicator available periods
      available_periods_new <- getAvailablePeriods(id = period_value, config = config, indicators = indicators)
      target <- unlist(strsplit(target_period, "-"))
      releasable <- if(length(target) == 1){
        if("month"%in% names(available_periods_new)){
          nrow(subset(available_periods_new, year == as.integer(target[1])))/12
        }else if("quarter"%in% names(available_periods_new)){
          nrow(subset(available_periods_new, year == as.integer(target[1])))/4
        }else{
          nrow(subset(available_periods_new, year == as.integer(target[1])))
        }
      }else{
        if(grepl("M",target[2])){
          target[2] <- gsub("M","",target[2])
          nrow(subset(available_periods_new, year == as.integer(target[1]) & month == as.integer(target[2])))/1
        }else if(grepl("Q",target[2])){
          if("month"%in% names(available_periods_new)){
            months<-switch(target[2],
                           "Q1"=c(1:3),
                           "Q2"=c(4:6),
                           "Q3"=c(7:9),
                           "Q4"=c(10:12))
            nrow(subset(available_periods_new, year == as.integer(target[1]) & month %in% months))/3
          }else{
            nrow(subset(available_periods_new,year == as.integer(target[1]) & quarter == target[2]))/1
          }
        }
      }
      releasable <- releasable == 1
      if(!releasable){
        return(releasable)
      }else{
        sub_releasable <- isReleasable(id = period_value, target, config = config, indicators = indicators)
        releasable <- all(releasable, sub_releasable)
      }
    }
    
    return(releasable)
  })
  
  releasable<-all(result)
  INFO("[ISReleasable] '%s' indicator is %s for the period '%s'",id,ifelse(releasable,"releasable","not releasable"),target_period)
  return(releasable)
  
}

#getComputationResults
getComputationResults <- function(indicator, config){
  
  staging <- list.files(path = sprintf("%s/staging/%s", config$store, indicator$id), recursive = TRUE)
  released <- list.files(path = sprintf("%s/release/%s", config$store, indicator$id), recursive = TRUE)
  released = released[regexpr("archive", released) < 0]
  values <- unique(c(unlist(strsplit(staging, ".csv")), unlist(strsplit(released, ".csv"))))
  periods <- as.vector(sapply(values, function(x){ 
    x.splits <- unlist(strsplit(x,"_"))
    period <- x.splits[length(x.splits)]
    return(period)
  }))
  
  df <- data.frame(
    Period = character(0),
    File = character(0),
    Status = character(0),
    Date = character(0),
    stringsAsFactors = FALSE
  )
  if(length(periods)>0){
    status <- sapply(periods, function(x){
      if(any(regexpr(x, released) > 0)){
        return("release")
      }else{
        return("staging")
      }
    })
    
    df <- do.call("rbind", lapply(1:length(periods), function(i){
      filepath <- file.path(config$store, status[i], indicator$id, paste0( values[i], ".csv"))
      tibble::tibble(
        Period = periods[i],
        File = filepath,
        Status = status[i],
        Date = file.info(filepath)$mtime
      )
    }))
    df <- df[order(df$Period),]
  }
  return(df)
}