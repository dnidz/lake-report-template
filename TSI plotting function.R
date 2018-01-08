# TSI plotting function
# Daniel Nidzgorski
# November 14, 2017

# Note that this calculates TSIs correctly -- takes the mean of the 
# water quality parameters first and then does the log TSI calculations.

# TSI.plot plots a lake's mean TSI values over time
# TSI.map maps all lakes' mean TSI for one year

TSI.plot<-function(data,lake,year) {
  
  params.list<-c("TotalPhosphorus",
                 "ChlorophyllA",
                 "Secchi")
  
  d.means<-data %>%
    filter(Lake==lake,
           Depth==1,
           Parameter %in% params.list) %>%
    mutate(Year=year(Date),
           Month=month(Date)) %>%
    filter(Year>=1994,
           Year<=year,
           Month>=5,
           Month<=10) %>%
    # Average by month first and then by year to help buffer against data gaps 
    group_by(Parameter,Year,Lake,Month) %>%
    summarize(Value=mean(Value,na.rm=T)) %>%
    group_by(Parameter,Year,Lake) %>%
    summarize(Value=mean(Value,na.rm=T))
  
  # Only include lake/year combos with at least five sampling dates
  # (don't filter per parameter since this will preclude ever doing mean alkalinity or color)
  trimlist<-data %>%
    filter(Lake==lake) %>%
    select(Lake,Date) %>%      
    unique() %>% # since each date will have multiple rows for params
    mutate(Year=year(Date)) %>%
    group_by(Lake,Year) %>%
    summarize(Count=n()) %>%
    filter(Count>=5) %>%
    mutate(Keep=T) %>%
    select(-Count)
  
  # Years without data are likely simply skipped -- insert NAs to break the line
  year.list<-tibble(Year=full_seq(d.means$Year,1))
  
  d.TSI<-d.means %>%
    spread(key=Parameter,value=Value) %>%
    mutate(TSI.Sec=10*(6-(log(Secchi/0.693))),
           TSI.Chl=10*(6-((2.04)-(0.68*log(ChlorophyllA)))/log(2)),
           TSI.TP=10*(6-(log(48/TotalPhosphorus)/0.693)) # TP in ug/L
    ) %>%
    left_join(trimlist, by=c("Lake","Year")) %>%
    filter(Keep) %>%
    full_join(year.list,by="Year") %>%
    select(Year,TSI.Sec,TSI.Chl,TSI.TP) %>%
    gather(-Year,key=TSI,value=Value)
  
  start<-min(d.TSI$Year,na.rm=T)
  end<-max(d.TSI$Year,na.rm=T)
  
  p<-ggplot(d.TSI,aes(x=Year,y=Value,shape=TSI,fill=TSI))+
    geom_hline(aes(yintercept=40),color="grey",size=0.5)+
    geom_hline(aes(yintercept=50),color="grey",size=0.5)+
    geom_line()+geom_point(size=3)+
    annotate("label",x=start,y=30,label=" oligotrophic",hjust=0,size=2,label.padding=unit(0.15,"lines"))+
    annotate("label",x=start,y=45,label=" mesotrophic",hjust=0,size=2,label.padding=unit(0.15,"lines"))+
    annotate("label",x=start,y=60,label=" eutrophic",hjust=0,size=2,label.padding=unit(0.15,"lines"))+
    scale_shape_manual(breaks=c("TSI.Sec","TSI.Chl","TSI.TP"),
                       values=c(23,19,22),
                       labels=c("Secchi","Chlorophyll-a","Total P")
    )+
    scale_fill_manual(breaks=c("TSI.Sec","TSI.Chl","TSI.TP"),
                      values=c("white","Black","#999999"),
                      labels=c("Secchi","Chlorophyll-a","Total P")
    )+
    scale_y_continuous(limits=c(20,70),breaks=seq(20,70,10),
                       name="TSI")+
    scale_x_continuous(limits=c(start,end),breaks=seq(start,end,2))+
    theme(axis.text.x=element_text(size=9),
          axis.text.y=element_text(size=9),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size=11),
          legend.position="top"
          
    )
  
  p
    
}

TSI.map<-function(data,lake,year) {
  
  latlong<-read_csv("lakelatlong.csv",
                    col_types=cols(
                      Lake = col_character(),
                      LakeName=col_character(),
                      Latitude = col_double(),
                      Longitude = col_double()
                    ))
  
  params.list<-c("TotalPhosphorus",
                 "ChlorophyllA",
                 "Secchi")
  
  d.means<-data %>%
    filter(Depth==1,
           Parameter %in% params.list) %>%
    mutate(Year=year(Date),
           Month=month(Date)) %>%
    filter(Year==year,
           Month>=5,
           Month<=10) %>%
    group_by(Parameter,Year,Lake) %>%
    summarize(Mean=mean(Value,na.rm=T))
  
  d.TSI<-d.means %>%
    spread(key=Parameter,value=Mean) %>%
    mutate(TSI.Sec=10*(6-(log(Secchi/0.693))),
           TSI.Chl=10*(6-((2.04)-(0.68*log(ChlorophyllA)))/log(2)),
           TSI.TP=10*(6-(log(48/TotalPhosphorus)/0.693)) # TP in ug/L
    ) %>%
    mutate(Focus=ifelse(Lake==lake,T,F)) %>%
    left_join(latlong,by="Lake")
  
  pal <- colorNumeric(
    palette = "BuGn",
    domain = c(20,70))
  
  # Interactive leaflet
  l<-leaflet(d.TSI) %>% 
    fitBounds(-122.432533,47.779165, -122.186827,47.248) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(color= ~pal(TSI.Chl),
                     stroke = F, fillOpacity = 1,
                     radius= ~ifelse(Focus, 10, 6),
                     label= ~Lake,
                     labelOptions=lapply(d.TSI$Focus, function(x) {
                       labelOptions(noHide=T,textOnly= !x)
                       }),
                     popup= ~sprintf("%s Chlorophyll-a TSI = %s",year,
                                     round(TSI.Chl))
    ) %>%
    addLegend("topright", pal = pal, values = ~c(20,70),
              title = "TSI",
              labFormat = labelFormat(digits=0),
              opacity = 1
    )
  
  # Return interactive leaflet for HTML version
  l
  
}

TSI.map.static<-function(data,year) {
  
  latlong<-read_csv("lakelatlong.csv",
                    col_types=cols(
                      Lake = col_character(),
                      LakeName=col_character(),
                      Latitude = col_double(),
                      Longitude = col_double()
                    ))
  
  params.list<-c("TotalPhosphorus",
                 "ChlorophyllA",
                 "Secchi")
  
  d.means<-data %>%
    filter(Depth==1,
           Parameter %in% params.list) %>%
    mutate(Year=year(Date),
           Month=month(Date)) %>%
    filter(Year==year,
           Month>=5,
           Month<=10) %>%
    group_by(Parameter,Year,Lake) %>%
    summarize(Mean=mean(Value,na.rm=T))
  
  d.TSI<-d.means %>%
    spread(key=Parameter,value=Mean) %>%
    mutate(TSI.Sec=10*(6-(log(Secchi/0.693))),
           TSI.Chl=10*(6-((2.04)-(0.68*log(ChlorophyllA)))/log(2)),
           TSI.TP=10*(6-(log(48/TotalPhosphorus)/0.693)) # TP in ug/L
    ) %>%
    mutate(Focus=ifelse(Lake==lake,T,F)) %>%
    left_join(latlong,by="Lake")

  
  # Static ggmap
  pad<-0.025
  box<-d.TSI %>%
    summarize(left=min(Longitude,na.rm=T)-pad,
              right=max(Longitude,na.rm=T)+pad,
              bottom=min(Latitude,na.rm=T)-pad,
              top=max(Latitude,na.rm=T)+pad
    )

  maptile<-get_stamenmap(bbox=c(box$left,box$bottom,box$right,box$top),
                       maptype="toner-background"
                       )
  

  s<-ggmap(maptile,
           maprange=T,
           base_layer=ggplot(data=d.TSI,aes(x=Longitude,y=Latitude,color=TSI.Chl)))+
    geom_point(size=4)+
    geom_label_repel(aes(label=Lake,fill=TSI.Chl),color="white",size=4)+
    scale_color_gradient(low="cornflowerblue",high="darkgreen",name="Chlorophyll-a TSI")+
    scale_fill_gradient(low="cornflowerblue",high="darkgreen",name="Chlorophyll-a TSI")+
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.justification=c(1,0), 
          legend.direction="horizontal",
          legend.position=c(1,0),
          legend.box.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
          legend.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
          
    )
  
  ggsave(s,filename=sprintf("%s/Plots/%s-TSI map.png",year,year),width=5.25,height=8)
  
  
}
