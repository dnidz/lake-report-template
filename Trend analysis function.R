# Small lake trend analyses
# Daniel Nidzgorski
# November 15, 2017

lake.trend<-function(data,lake,year,params.list="L2") {
  
  library(NADA)

  if(params.list=="L2") {
    params.list<-c("Temperature",
                   "Secchi",
                   "ChlorophyllA",
                   "TotalPhosphorus",
                   "TotalNitrogen",
                   "NPRatio"
    )
  } else if(params.list=="ST") {
    params.list<-c("Temperature",
                   "Secchi") 
    
  } else if(params.list=="noST") {
    params.list<-c("ChlorophyllA",
                   "TotalPhosphorus",
                   "TotalNitrogen",
                   "NPRatio"
    )
  } else if (params.list=="all") {
    params.list<-unique(data$Parameter)
  } # If not defined, leave it alone -- user-specified
  
  d.means<-data %>%
    filter(Depth==1,
           Parameter %in% params.list) %>%
    mutate(Year=year(Date),
           Month=month(Date)) %>%
    filter(Year>=1994,
           Year<=year,
           Month>=5,
           Month<=10) %>%
    group_by(Parameter,Year,Lake) %>%
    summarize(Value=mean(Value,na.rm=T))
  
  # Only include lake/year combos with at least five sampling dates
  # (don't filter per parameter since this will preclude ever doing mean alkalinity or color)
  trimlist<-data %>%
    select(Lake,Date) %>%
    unique() %>% # since each date will have multiple rows for params
    mutate(Year=year(Date)) %>%
    group_by(Lake,Year) %>%
    summarize(Count=n()) %>%
    filter(Count>=5) %>%
    mutate(Keep=T) %>%
    select(-Count)
  
  d.trends<-d.means %>%
    left_join(trimlist,by=c("Lake","Year")) %>%
    filter(Keep,
           Lake==lake,
           !is.na(Value)) %>%
    select(-Keep)
  
  
  # Trend analysis of annual means for a single parameter
  # Use cenken to estimate both slope and intercept
  trend.means<-function(data,param) {
    d<-data %>%
      filter(Parameter==param,
             !is.na(Value)) %>%
      arrange(Year)
    # If the censored data have simply been replaced, dummy BelowMDL column
    if(!str_detect(str_c(names(d),collapse=""),"BelowMDL")) d<-mutate(d,BelowMDL=F)
    
    
    # cenken needs numeric x and y -- can't use Date class
    # so consider Year instead of specific date
    r<-cenken(d$Value,d$BelowMDL,d$Year)
    
    output<-tibble(
      Parameter=param,
      slope=r$slope,
      intercept=r$intercept,
      tau=r$tau,
      p=r$p
    )
    
    output
  }
  
  # Map through all parameters
  # Only include params with at least five years of data
  p.list<-d.trends %>%
    group_by(Parameter) %>%
    summarize(numYears=length(unique(Year))) %>%
    filter(numYears>=5) %>%
    .$Parameter
  
  output<-map_df(p.list,trend.means,data=d.trends) 
  
  output
  
}
