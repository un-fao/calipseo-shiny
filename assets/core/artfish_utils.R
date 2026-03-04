#ACCURACY
artfish_accuracy<-function(n,N,method="higher"){
  #Algebraic approach
  NP<-function(n,N){
    W=0.75*(1-1/N)
    a=(2*W*(N^2))/(N-1)^2-(N+1)/(N-1)
    g=a+(1-a)/N
    S=(1-a)*(1/log(N)-1/(N*log(N))-1/N)
    k=(-2/log(N))*log(S/(1-S-g))
    a2=(1-S-g)^2/(2*S+g-1)
    a1=g-a2
    x=log(n)/log(N)
    A=a1+a2*(N^(-k*x))
    
    return(A)
  }
  
  #Probabilistic approach
  P<-function(n,N){
    R=sqrt((2*N-1)/(6*(N-1))-1/4)
    A=1-1.96*(R/sqrt(n))*sqrt(1-n/N)
    
    return(A)
  }
  
  Acc<-switch(method,
              "algebraic"=NP(n=n,N),
              "probabilistic"=P(n,N),
              "higher"=max(NP(n,N),P(n,N)))
  return(Acc)
}

#uniformity index
unif_index<-function(days){
  table<-as.data.frame(table(days))
  mean=mean(table$Freq)
  table$ratio<-ifelse(table$Freq/mean>1,1,table$Freq/mean)
  index=mean(table$ratio)
  return(index)
}

#get_artfish_results_for_ui
#@param files list of computation output files
#@ref_fishing_units reference fishing units accessed through \code{accessRefFishingUnits}
#@ref_species reference species accessed through \code{accessRefSpecies}
#@ref_landing_sites reference landing sites accessed through \code{accessLandingSites}
#@with_status boolean include file status
get_artfish_results_for_ui = function(files, ref_fishing_units = NULL, ref_species = NULL, ref_landing_sites = NULL, with_status=FALSE){
  
  if(with_status){
    estimate <- do.call(
      rbind,
      lapply(1:nrow(files), function(i) {
        readr::read_csv(files$file[i]) %>%
          mutate(status = files$status[i])
      })
    )
  }else{
    estimate <- do.call(rbind,lapply(files$file, readr::read_csv))
  }
  
  
  if(!is.null(ref_fishing_units)){
    estimate <- estimate %>%
      merge(ref_fishing_units %>%
              select(ID,NAME) %>%
              rename(fishing_unit = ID,
                     fishing_unit_label = NAME)
      ) %>%
      ungroup()
  }
  
  if(!is.null(ref_species)){
    estimate <- estimate %>%
      merge(ref_species %>%
              select(ID,NAME,SCIENTIFIC_NAME) %>%
              rename(species = ID,
                     species_label = NAME,
                     species_scientific = SCIENTIFIC_NAME)
      )%>%
      ungroup()
  }
  
  if(!is.null(ref_landing_sites)){
    estimate <- estimate %>%
      merge(ref_landing_sites %>%
              select(ID,NAME) %>%
              rename(landing_site = ID,
                     landing_site_label = NAME)
      ) %>%
      ungroup()
  }
  
  estimate <- estimate %>%
    mutate(date = as.Date(sprintf("%04d-%02d-01",year,month)))
  return(estimate)
}
