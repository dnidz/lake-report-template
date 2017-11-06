# Annual lake plotting function
# Daniel Nidzgorski
# October 27, 2017

# For everything except precip/level
# Returns a ggplot object

# Set wy=T to plot for a full water year
# Set means=T to return a plot of annual means
annual.plot.facet<-function(data,lake,year,wy=F,means=F){
 
  y<-tribble(
    ~Parameter, ~label, ~min, ~max, ~breaksize,
    "TotalPhosphorus", "Total Phosphorus (µg/L)", 0, 100, 20,
    "TotalNitrogen", "Total Nitrogen (µg/L)", 0, 1000, 200,
    "Temperature", "Water temperature (°C)", 0, 30, 5,
    'TotalAlk', "mg CaCO3/L", 0, 25, 5,
    "NPRatio", "N:P ratio", 0, 100, 25,
    "ChlorophyllA", "Chlorophyll-a (µg/L)", 0, 40, 10,
    "Secchi", "Secchi depth (m)", 0, 10, 2,
    "UV.Absorbance", "UV254 absorbance", 0, 1, 0.2
  ) 
  
  params.list<-c("Temperature",
                 "Secchi",
                 "ChlorophyllA",
                 "TotalPhosphorus",
                 "TotalNitrogen",
                 "NPRatio"
  )


  # Set data frames and nucleate desired plot
  if(!means) {
    
    if(!wy) {
      start<-as.Date(sprintf("%s-05-01", year))
      end<-as.Date(sprintf("%s-10-31", year))
    } else {
      start<-as.Date(sprintf("%s-10-01", year-1))
      end<-as.Date(sprintf("%s-09-30", year))
    }
    
    d.background<-data %>%
      filter(Depth==1,
             Parameter %in% params.list,
             Date>=start,
             Date<=end) %>%
      left_join(y,by="Parameter")
    
    d<-d.background %>%
      filter(Lake==lake)

    } else {
      # Annual means
      d.background<-data %>%
        filter(Depth==1,
               Parameter %in% params.list) %>%
        mutate(Year=year(Date),
               Month=month(Date)) %>%
        filter(Year>=1994,
               Month>=5,
               Month<=10) %>%
        group_by(Parameter,Year,Lake) %>%
        summarize(Value=mean(Value,na.rm=T)) %>%
        left_join(y,by="Parameter") %>%
        mutate(Date=Year)
      
      d<-d.background %>%
        filter(Lake==lake)
      
      start<-min(d$Year,na.rm=T)
      end<-max(d$Year,na.rm=T)
    }
  
  
  # Filter chlorophyll data to exclude high ones
  # with "high" defined by this lake's data if > set limit
  data.chlor.max<-d %>%
    filter(Parameter=="ChlorophyllA") %>%
    .$Value %>%
    max(na.rm=T)
  chlor.max<-y %>%
    filter(Parameter=="ChlorophyllA") %>%
    .$max %>%
    {ifelse(data.chlor.max> . ,data.chlor.max, . )}
  
  d.background<-d.background %>%
    filter(Parameter!="ChlorophyllA" | Value<chlor.max,
           !is.na(Value))
  
  # High, low, and median lakes
  grandmeans<-d.background %>%
    group_by(Lake,Parameter) %>%
    summarize(Mean=mean(Value,na.rm=T)) %>%
    arrange(Parameter,Mean)
  
  lakes<-grandmeans %>%
    group_by(Parameter) %>%
    summarize(n=n(),
              min=first(Lake),
              median=nth(Lake,n=round(n/2)),
              max=last(Lake)
    )
  
  selection<-lakes %>%
    select(-n) %>%
    gather(-Parameter,key=Type,value=Lake)
  
  highlow<-map2_df(selection$Parameter,selection$Lake,
                   function(x,y) filter(d.background,
                                        Parameter==x,
                                        Lake==y)
  ) %>%
    left_join(selection,by=c("Parameter","Lake"))
  
  # Nucleate plot
  if(!means) {
    
    p<-ggplot(d.background,aes(x=Date,y=Value))+
      scale_x_date(date_breaks="1 month",date_labels="%m/1/%y",limits=c(start,end))
    
    jitterwidth<-2
    
  } else {
    p<-ggplot(d.background,aes(x=Year,y=Value))+
      scale_x_continuous(limits=c(start,end))
    
    jitterwidth<-0.2
  }
  
  # Rest of plot
  p<-p+
    facet_wrap(~Parameter,ncol=1,scales="free_y")+
    # Background data
    geom_jitter(color="black",size=2,alpha=0.25,width=jitterwidth)+
    # High, low, median lines
    geom_line(data=highlow,aes(group=Type),linetype="dashed",na.rm=T)+
    geom_text_repel(
      data = subset(highlow,Date==median(Date,na.rm=T)),
      aes(label=Lake),
      size = 2,
      nudge_x = 5,
      segment.color = NA
    ) +
    # scale_color_manual(values=c("min"="black","median"="black","max"="black"))+
    # Lake-specific data
    geom_line(data=d,color="blue",size=1)+geom_point(data=d,size=3,color="blue")+
    # scale_y_continuous(breaks=seq(y$min,y$max,y$breaksize))+
    # Ensure that plot doesn't get smaller than specified limits
    geom_blank(aes(y=min))+geom_blank(aes(y=max))+
    theme(axis.text.x=element_text(size=11),
          axis.text.y=element_text(size=11),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()
    )+
    guides(color=F)+
    geom_hline(data=filter(d,Parameter=="NPRatio"),aes(yintercept=25),color="grey",size=0.5)
  
  p
}
