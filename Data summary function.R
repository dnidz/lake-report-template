# Data summary for L1 or L2 data
# Daniel Nidzgorski
# December 4, 2017

lake.data.summary<-function(data,lake,year,y,wy=F,params.list="L2") {
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
  } # If not defined, leave it alone -- user-specified.
  
  if(!wy) {
    start<-as.Date(sprintf("%s-05-01", year))
    end<-as.Date(sprintf("%s-10-31", year))
  } else {
    start<-as.Date(sprintf("%s-10-01", year-1))
    end<-as.Date(sprintf("%s-09-30", year))
  }
  
  d<-data %>%
    filter(Lake==lake,
           Depth==1,
           Date>=start,
           Date<=end,
           Parameter %in% params.list) %>%
    mutate(Month=month(Date)) %>%
    left_join(y,by="Parameter")
  
  # Mean is the mean of monthly means to help deal with missing or different-frequency data.
  summary<-d %>%
    group_by(label,Month) %>%
    summarize(Min=min(Value,na.rm=T),
              Mean=mean(Value,na.rm=T),
              Max=max(Value,na.rm=T)
    ) %>%
    group_by(label) %>%
    summarize(Min=min(Min,na.rm=T),
              Mean=mean(Mean,na.rm=T),
              Max=max(Max,na.rm=T)
    ) %>%
    mutate_if(is.numeric,round,digits=1)
  
  summary
  
}

summarize.L2<-function(data,lake,year,y) {
  summary<-lake.data.summary(data,lake,year,y,wy=F,params.list="L2")
  
  table<-summary %>%
    kable(col.names=c("Parameter","Minimum","Mean","Maximum"),
          caption=sprintf("%s May-October summary statistics",year)
    ) %>%
    kable_styling(full_width=F)
  
  table
  
}

summarize.L1<-function(data.L1,data.L2,lake,year,y) {
  summary.L1<-lake.data.summary(data.L1,lake,year,y,wy=T,params.list="ST")
  summary.L2<-lake.data.summary(data.L2,lake,year,y,wy=F,params.list="noST")
  
  table<-bind_rows(summary.L1,summary.L2) %>%
    kable(col.names=c("Parameter","Minimum","Mean","Maximum"),
          caption=sprintf("%s summary statistics",year)
    ) %>%
    kable_styling(full_width=F) %>%
    group_rows(index=c("Annual statistics"=nrow(summary.L1),
                       "May-October statistics"=nrow(summary.L2)
    ))
  
  table
  
}
