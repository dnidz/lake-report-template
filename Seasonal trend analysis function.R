# Seasonal Kendall trend test
# Daniel Nidzgorski
# November 21, 2017

library(rkt)

lake.trend.seasonal<-function(data,lake,year,params.list="L2") {
  
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
  } # If not defined, leave it alone -- user-specified
  
  d<-data %>%
    mutate(Year=year(Date),
           Month=month(Date),
           dDate=decimal_date(Date)) %>%
    filter(Lake==lake,
           Year>=1994,
           Year<=year,
           Month>=5,
           Month<=10,
           Parameter %in% params.list)
  
  rkt.to.df<-function(r) {
    output<-tibble(
      p=r$sl,
      slope=r$B,
      tau=r$tau,
      score=r$S,
      var.score=r$varS)
    
    output
  }
  
  rkt.param<-function(data,param) {
    d.param<-data %>%
      filter(Parameter==param)
    
    r.overall<-rkt(date=d.param$dDate,
           y=d.param$Value,
           block=d.param$Month,
           rep="a") %>%
      rkt.to.df() %>%
      mutate(Month="Overall",
             Parameter=param) %>%
      select(Parameter,Month,everything())
    
    r.months<-map_df(5:10, function(x) {
      rkt(date=filter(d.param,Month==x)$dDate,
          y=filter(d.param,Month==x)$Value,
          rep="a") %>%
        rkt.to.df()
    }) %>%
      mutate(Month=as.character(5:10),
             Parameter=param) %>%
      select(Parameter,Month,everything())
           
  
  output<-bind_rows(r.overall,r.months)
  
  output
  }
  
  trends<-map_df(params.list,rkt.param,data=d) %>%
    mutate(Lake=lake) %>%
    select(Lake,everything())
  
  # Add column of overall p-value to aid in filtering significant trends
  overall.p<-trends %>%
    filter(Month=="Overall") %>%
    mutate(overall.p=p) %>%
    select(Lake,Parameter,overall.p)
  
  trends<-left_join(trends,overall.p,by=c("Lake","Parameter"))
  
  trends
}
