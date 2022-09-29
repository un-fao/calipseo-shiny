Category_fishery <- function(data, code = NULL, category_name) {
  
  if(!is.null(code)){
    if(code=='FIS'){
      data[!is.na(data$FISHER_ID),1] <- category_name
      data[!is.na(data$PERMIT_NUMBER),1] <- category_name}
    data[data$FSH_CODE== code & !is.na(data$FSH_CODE),1] <- category_name
    data <- data[data$Category==category_name & !is.na(data$Category),]
  }else{
    data <- data[!is.na(data$PERMIT_NUMBER),]
    data[,1] <- category_name
    data[data$Category==category_name,]
  }
  return(data)
}


fisher_nonfisher <- function(data){
  
  data <- unique(data[data$individualNumber,])
  fisher <- data[data$Category=='fisher',]
  non_fisher <- data[data$Category =='nonfisher',]
  
  
  Owner <- Category_fishery(fisher, code = 'OWN', category_name =  i18n("INDIVIDUAL_LABEL_OWNER"))
  Captain <- Category_fishery(fisher, code = 'CAP', category_name = i18n("INDIVIDUAL_LABEL_CAPTAIN"))
  Fisher_ID <- Category_fishery(fisher, code = 'FIS', category_name = i18n("INDIVIDUAL_LABEL_HOLDER_FISHING_ID"))
  Fisher_ID <- Fisher_ID[!is.na(Fisher_ID$FSH_CODE),]
  Fisher_ID <- Fisher_ID[Fisher_ID$FSH_CODE!='CAP' & Fisher_ID$FSH_CODE!='OWN',]
  Fisher_license <- Category_fishery(fisher, category_name = i18n("INDIVIDUAL_LABEL_HOLDER_FISHING_LICENSE"))
  Fisher_license <- Fisher_license[!is.na(Fisher_license$PERMIT_NUMBER),]
  Fisher_license <- Fisher_license[(Fisher_license$FSH_CODE!='OWN' & Fisher_license$FSH_CODE!='CAP' & Fisher_license$FSH_CODE!='FIS'),]
  Fisher_license <- Fisher_license[!is.na(Fisher_license$Category),]
  
  
  data <- rbind(fisher,Owner,Captain,Fisher_ID,Fisher_license,non_fisher)
  
  return(data)
  
}



pyramid_df <- function(data, subset = NULL){
  
  data <- data[,-2]
  
  subset <- data[which(data$Gender == subset),names(data) %in% c("Gender","Age", "Edulevel")]
  
  subset$Age <- as.factor(subset$Age)
  
  Pop_subset <- as.data.frame(table(subset$Age, dnn = c("Age")))
  if(nrow(subset)<1){
    subset <- data.frame(Gender = subset, Age = character(0))
    Pop_subset <-  data.frame(Gender = subset, Age = character(0))
  }
  
  df <- unique(left_join(Pop_subset,subset,  by = "Age"))
  
  return(df)
  
}



plot_df <- function(category, fill){
  
  age <- Age_comp(category[,c('Gender','DOB', 'Edulevel')], Prep = TRUE)
  
  Male <- pyramid_df(age, subset = i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"))
  Female <- pyramid_df(age, subset = i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"))
  
  pyramid_df <- rbind(Female,Male)
  
  pyramid_df$Age <- as.numeric(as.character(pyramid_df$Age))
  
  pyramid_df["Age_group"] = cut(
    pyramid_df$Age,c(0, 4, 9, 14,19,24,29,34,39,44,
                     49,54,59,64,69,74,79,84,89,94,99,Inf),
    c("0-4", "5-9", "10-14", "15-19",
      "20-24","25-29", "30-34", "35-39",
      "40-44", "45-49", "50-54", "55-59",
      "60-64","65-69", "70-74", "75-79",
      "80-84","85-89", "90-94", "95-99",
      '100+'),include.lowest=TRUE)
  
  if(fill==i18n("INDIVIDUAL_OVERVIEW_LABEL_EDULEVEL")){
    
    df <- aggregate(Freq ~ Age_group+Edulevel+Gender,pyramid_df ,sum)
    
    names(df)[-3] <- c(i18n("INDIVIDUAL_OVERVIEW_LABEL_AGEGROUP"), i18n("INDIVIDUAL_OVERVIEW_LABEL_EDULEVEL"), i18n("INDIVIDUAL_OVERVIEW_LABEL_INDIVIDUAL"))
    
    df$`Education level` <- as.factor(df$`Education level`)
    category_edu <- levels(df$`Education level`)
    
    orig_cat <- c(i18n("INDIVIDUAL_OVERVIEW_LABEL_NONE"),i18n("INDIVIDUAL_OVERVIEW_LABEL_PRIMARY"),i18n("INDIVIDUAL_OVERVIEW_LABEL_SECONDARY"),i18n("INDIVIDUAL_OVERVIEW_LABEL_UNIVERSITY"))
    
    
    if(length(category_edu)<4){
      
      for (i in 1:length(orig_cat)) {
        catg<- match(orig_cat[i],category_edu)
        if(is.na(catg)){
          
          x <- data.frame(Agegroup='100+',Educationlevel=orig_cat[i],Gender=i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"),Individual=0)
          
          names(x) <- c(i18n("INDIVIDUAL_OVERVIEW_LABEL_AGEGROUP"),i18n("INDIVIDUAL_OVERVIEW_LABEL_EDULEVEL"),i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"),i18n("INDIVIDUAL_OVERVIEW_LABEL_INDIVIDUAL"))
          df <- rbind(df,x)
          
          
        }
        
      }
      
    }
    
    
    df$`Education level` <- factor(df$`Education level`, levels=c(i18n("INDIVIDUAL_OVERVIEW_LABEL_NONE"),i18n("INDIVIDUAL_OVERVIEW_LABEL_PRIMARY"),i18n("INDIVIDUAL_OVERVIEW_LABEL_SECONDARY"),i18n("INDIVIDUAL_OVERVIEW_LABEL_UNIVERSITY")))
    
    if(nlevels(as.factor(as.character(df[,3])))>1){
      
      p <- ggplot(data = df[order(df$`Education level`, decreasing = T),], aes(x = `Age group`, fill = `Education level`,
                                                                               text = paste0(i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"),': ',Gender)))+
        geom_bar(data = df[df$Gender == i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"),],
                 stat = "identity", aes(y = Individual)) +
        geom_bar(data = df[df$Gender == i18n( "INDIVIDUAL_OVERVIEW_LABEL_FEMALE"),],
                 stat = "identity",aes(y = -Individual)) +
        geom_hline(yintercept=-0, colour="white", lwd=1)+
        coord_flip()+scale_y_reverse()+
        scale_fill_manual(aesthetics = 'fill',values =  c('lightblue','maroon','orange',"seagreen"),drop = FALSE, name=i18n("INDIVIDUAL_OVERVIEW_LABEL_EDULEVEL"))
      
      
      q <- ggplotly(p) %>% add_annotations(yref="paper",legendtitle=FALSE, xref="paper", y=1, x=0, text=i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"), showarrow=FALSE, font=list(size=8)) %>%
        add_annotations(yref="paper", xref="paper",legendtitle=FALSE, y=1, x=1, text=i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"), showarrow=FALSE, font=list(size=8)) %>%
        layout(title=FALSE,legend = list(orientation = 'h', y=-0.2),plot_bgcolor= '#fff',
               yaxis = list(title = i18n("INDIVIDUAL_OVERVIEW_LABEL_AGEGROUP"),titlefont = list(size = 13)),
               xaxis = list(title = i18n("INDIVIDUAL_OVERVIEW_LABEL_INDIVIDUAL"),titlefont = list(size = 13)))
      q$x$data[[7]]$text <- NULL
      
      q
    }else{
      
      if(nrow(df[df$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"),])>0){
        p <- ggplot(data = df[order(df$`Education level`, decreasing = T),], aes(x = `Age group`, fill = `Education level`, 
                                                                                 text = paste0(i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"),': ',Gender)))+
          geom_bar(data = df[df$Gender == i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"),],
                   stat = "identity", aes(y = Individual)) +
          coord_flip()+scale_y_reverse()+
          scale_fill_manual(aesthetics = 'fill',values =  c('lightblue','maroon','orange',"seagreen"),drop = FALSE, name=i18n("INDIVIDUAL_OVERVIEW_LABEL_EDULEVEL"))+
          ggtitle(i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"))
        
        
        q <- ggplotly(p) %>% add_annotations(yref="paper",legendtitle=FALSE, xref="paper", y=1, x=0, text=i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"), showarrow=FALSE, font=list(size=8)) %>%
          layout(title=FALSE,legend = list(orientation = 'h', y=-0.2),plot_bgcolor= '#fff',
                 yaxis = list(title = i18n("INDIVIDUAL_OVERVIEW_LABEL_AGEGROUP"),titlefont = list(size = 13)),
                 xaxis = list(title = i18n("INDIVIDUAL_OVERVIEW_LABEL_INDIVIDUAL"),titlefont = list(size = 13)))
        
        q
        
      }else{
        p <- ggplot(data = df[order(df$`Education level`, decreasing = T),], aes(x = `Age group`, fill = `Education level`, 
                                                                                 text = paste0(i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"),': ',Gender)))+
          geom_bar(data = df[df$Gender == i18n( "INDIVIDUAL_OVERVIEW_LABEL_FEMALE"),],
                   stat = "identity",aes(y = -Individual)) +
          coord_flip()+scale_y_reverse()+
          scale_fill_manual(aesthetics = 'fill',values =  c('lightblue','maroon','orange',"seagreen"),drop = FALSE, name= i18n("INDIVIDUAL_OVERVIEW_LABEL_EDULEVEL"))+
          ggtitle(i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"))
        
        
        q <- ggplotly(p) 
        add_annotations(yref="paper", xref="paper",legendtitle=FALSE, y=1, x=1, text=i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"), showarrow=FALSE, font=list(size=8)) %>%
          layout(title=FALSE,legend = list(orientation = 'h', y=-0.2),plot_bgcolor= '#fff',
                 yaxis = list(title = i18n("INDIVIDUAL_OVERVIEW_LABEL_AGEGROUP"),titlefont = list(size = 13)),
                 xaxis = list(title = i18n("INDIVIDUAL_OVERVIEW_LABEL_INDIVIDUAL"),titlefont = list(size = 13)))
        
        q
        
      }
      
    }
    
    
    
  }else if(fill==i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER")){
    
    df <- aggregate(Freq ~ Age_group+Gender,pyramid_df ,sum)
    
    names(df)[-2] <- c(i18n("INDIVIDUAL_OVERVIEW_LABEL_AGEGROUP"), i18n("INDIVIDUAL_OVERVIEW_LABEL_INDIVIDUAL"))
    
    df$Gender <- factor(df$Gender, levels=c(i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"),i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE")))
    
    if(nlevels(as.factor(as.character(df[,2])))>1){
      
      p <- ggplot(data = df, aes(x = `Age group`, fill = Gender))+
        geom_bar(data = df[df$Gender == i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"),],
                 stat = "identity", aes(y = Individual))+
        geom_bar(data = df[df$Gender == i18n( "INDIVIDUAL_OVERVIEW_LABEL_FEMALE"),],
                 stat = "identity",aes(y = -Individual))+
        geom_hline(yintercept=-0, colour="white", lwd=1)+
        coord_flip()+scale_y_reverse()+
        scale_fill_manual(aesthetics = 'fill',values =  c("seagreen","orange"),drop = FALSE, name=i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"))
      
      
      q <- ggplotly(p) %>% add_annotations(yref="paper",legendtitle=FALSE, xref="paper", y=1, x=0, text=i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"), showarrow=FALSE, font=list(size=10)) %>% 
        add_annotations(yref="paper", xref="paper",legendtitle=FALSE, y=1, x=1, text=i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"), showarrow=FALSE, font=list(size=10)) %>% 
        layout(title=FALSE,legend = list(orientation = 'h', y=-0.3),plot_bgcolor= '#fff',
               yaxis = list(title = i18n("INDIVIDUAL_OVERVIEW_LABEL_AGEGROUP"),titlefont = list(size = 13)),
               xaxis = list(title = i18n("INDIVIDUAL_OVERVIEW_LABEL_INDIVIDUAL"),titlefont = list(size = 13)))
      q$x$data[[3]]$text <- NULL
      
      q
      
    }else{
      
      if(nrow(df[df$Gender==i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"),])>0){
        p <- ggplot(data = df, aes(x = `Age group`, fill = Gender))+
          geom_bar(data = df[df$Gender == i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"),],
                   stat = "identity", aes(y = Individual))+
          coord_flip()+scale_y_reverse()+
          scale_fill_manual(aesthetics = 'fill',values =  c("seagreen","orange"),drop = FALSE, name=i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"))+
          ggtitle(i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"))
        
        q <- ggplotly(p) %>% add_annotations(yref="paper",legendtitle=FALSE, xref="paper", y=1, x=0, text=i18n("INDIVIDUAL_OVERVIEW_LABEL_MALE"), showarrow=FALSE, font=list(size=10)) %>% 
          layout(title=FALSE,legend = list(orientation = 'h', y=-0.3),plot_bgcolor= '#fff',
                 yaxis = list(title = i18n("INDIVIDUAL_OVERVIEW_LABEL_AGEGROUP"),titlefont = list(size = 13)),
                 xaxis = list(title = i18n("INDIVIDUAL_OVERVIEW_LABEL_INDIVIDUAL"),titlefont = list(size = 13)))
        
        q
        
      }else{
        
        p <- ggplot(data = df, aes(x = `Age group`, fill = Gender))+
          geom_bar(data = df[df$Gender == i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"),],
                   stat = "identity", aes(y = Individual))+
          coord_flip()+scale_y_reverse()+
          scale_fill_manual(aesthetics = 'fill',values =  c("seagreen","orange"),drop = FALSE, name=i18n("INDIVIDUAL_OVERVIEW_LABEL_GENDER"))+
          ggtitle(i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"))
        
        
        q <- ggplotly(p) %>% 
          add_annotations(yref="paper", xref="paper",legendtitle=F, y=1, x=1, text=i18n("INDIVIDUAL_OVERVIEW_LABEL_FEMALE"), showarrow=FALSE, font=list(size=10)) %>% 
          layout(title=FALSE,legend = list(orientation = 'h', y=-0.3),plot_bgcolor= '#fff',
                 yaxis = list(title = i18n("INDIVIDUAL_OVERVIEW_LABEL_AGEGROUP"),titlefont = list(size = 13)),
                 xaxis = list(title = i18n("INDIVIDUAL_OVERVIEW_LABEL_INDIVIDUAL"),titlefont = list(size = 13)))
        
        q 
        
        
      }
      
    }
    
  }
  
  
}