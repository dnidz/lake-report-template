# Seasonal Kendall trend test
# Daniel Nidzgorski
# November 21, 2017

library(rkt)

lake.trend.seasonal<-function(data,lake,year,params.list="L2",overall.only=T) {
  
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
  
  sk.to.df<-function(s) {
    output<-tibble(
      p=s$p.value["z (Trend)"],
      slope=s$estimate["slope"],
      intercept=s$estimate["intercept"],
      tau=s$estimate["tau"],
      z=s$statistic["z (Trend)"],
      all=list(s)
    ) 

    output
  }
  
  sk.to.df.indiv<-function(s) {
    output<-tibble(
      p=s$p.value,
      slope=s$estimate["slope"],
      intercept=s$estimate["intercept"],
      tau=s$estimate["tau"],
      z=s$statistic,
      all=list(s)
    ) 
    
    output
  }
  
  sk.param<-function(data,param) {
    d.param<-data %>%
      filter(Parameter==param)
    
    s.overall<-kendallSeasonalTrendTest(data=d.param,
                                Value~Month+Year) %>%
      sk.to.df() %>%
      mutate(Parameter=param,
             Month="Overall") %>%
      select(Parameter,Month,everything())
    
    s.months<-map_df(5:10, function(x) {
      kendallTrendTest(data=filter(d.param,Month==x),
                       Value~Year) %>%
        sk.to.df.indiv()
    }) %>%
      mutate(Month=as.character(5:10),
             Parameter=param) %>%
      select(Parameter,Month,everything())
    
    output<-bind_rows(s.overall,s.months)
    
    output
    }
  
  trends<-map_df(params.list,sk.param,data=d) %>%
    mutate(Lake=lake) %>%
    select(Lake,everything())
  
  # Add column of overall p-value to aid in filtering significant trends
  overall.p<-trends %>%
    filter(Month=="Overall") %>%
    mutate(overall.p=p) %>%
    select(Lake,Parameter,overall.p)

  trends<-left_join(trends,overall.p,by=c("Lake","Parameter"))
  
  if(overall.only) {
    trends<-trends %>%
      filter(Month=="Overall") %>%
      select(-Month,-overall.p)
  }
  
  trends
}
