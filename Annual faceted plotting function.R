# Annual lake plotting function
# and wrapper functions
# Daniel Nidzgorski
# November 3, 2017

# Returns a ggplot object

# Set wy=T to plot for a full water year
# Set means=T to return a plot of annual means
# Can take any parameter list. Also has some pre-defined ones:
# L2 = Secchi, Temp, Chlor, TP, TN, NP
# ST = Secchi & Temp
# no ST = Chlor, TP, TN, NP (L2 except Secchi & Temp)
lake.plot.facet<-function(data,lake,year,wy=F,means=F,params.list="L2"){
 
  y<-tribble(
    ~Parameter, ~label, ~min, ~max, ~breaksize,
    "TotalPhosphorus", "Total Phosphorus (ug/L)", 0, 100, 20,
    "TotalNitrogen", "Total Nitrogen (ug/L)", 0, 1100, 200,
    "Temperature", "Water temperature (C)", 0, 30, 5,
    'TotalAlk', "Total Alkalinity (mg CaCO3/L)", 0, 25, 5,
    "NPRatio", "N:P ratio", 0, 100, 25,
    "ChlorophyllA", "Chlorophyll-a (ug/L)", 0, 40, 10,
    "Secchi", "Secchi depth (m)", 0, 10, 2,
    "UV.Absorbance", "UV254 absorbance", 0, 1, 0.2
  ) %>%
    mutate(label=ordered(label,
                             levels=c("Secchi depth (m)",
                                      "Water temperature (C)",
                                      "Chlorophyll-a (ug/L)",
                                      "Total Nitrogen (ug/L)",
                                      "Total Phosphorus (ug/L)",
                                      "N:P ratio",
                                      "UV254 absorbance",
                                      "Total Alkalinity (mg CaCO3/L)")))
    
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


  # Start with individual data. If plotting annual means, this is still needed to set the same
  # chlorophyll max for both the individual and annual-means plots.

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

  # Now that chlorophyll max is set, can override the data frames with annual-means
  if (means) {
    # Annual means.
    d.means<-data %>%
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
    
    # Only include lake/year combos with at least five sampling dates
    # (don't filter per parameter since this will preclude ever doing mean alkalinity or color)
    trimlist<-data %>%
      select(Lake,Date) %>%
      mutate(Year=year(Date)) %>%
      group_by(Lake,Year) %>%
      summarize(Count=n()) %>%
      filter(Count>=5) %>%
      mutate(Keep=T) %>%
      select(-Count)
    
    d.background<-d.means %>%
      left_join(trimlist,by=c("Lake","Year")) %>%
      filter(Keep) %>%
      select(-Keep)
    
    d<-d.background %>%
      filter(Lake==lake)
    
    start<-min(d$Year,na.rm=T)
    end<-max(d$Year,na.rm=T)
  }

  # Filter chlorophyll background data
  d.background<-d.background %>%
    filter(Parameter!="ChlorophyllA" | Value<chlor.max,
           !is.na(Value))
  

  # Nucleate plot
  if(!means) {
    
    # Different date breaks for water year or summer-only
    db<-ifelse(wy,"2 months","1 month")
    
    p<-ggplot(d.background,aes(x=Date,y=Value))+
      scale_x_date(date_breaks=db,date_labels="%m/1/%y",limits=c(start,end))
    
    jitterwidth<-2
    
  } else {
    p<-ggplot(d.background,aes(x=Year,y=Value))+
      scale_x_continuous(limits=c(start,end),breaks=seq(start,end,2))
    
    jitterwidth<-0.2
  }
  
  # Rest of plot
  p<-p+
    facet_wrap(~label,ncol=1,scales="free_y")+
    # Background data
    geom_jitter(color="black",size=2,alpha=0.25,width=jitterwidth)+
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
    guides(color=F)
  
  if("NPRatio" %in% params.list) {
    p<-p+geom_hline(data=filter(d,Parameter=="NPRatio"),aes(yintercept=25),color="grey",size=0.5)
  }
  
  p
}


# Plot a lake that has only L2 data
lake.plot.L2<-function(data,lake,year) {
  
  p<-lake.plot.facet(data,lake,year,wy=F,means=F,params.list="L2")
  p.rev<-p+scale_y_reverse()
  
  m<-lake.plot.facet(data,lake,year,wy=F,means=T,params.list="L2")
  m.rev<-m+scale_y_reverse()
  
  # For tweaking, set width and height once
  w<-6
  h<-10
  
  ggsave(p,filename="tmp-indiv.png",width=w,height=h)
  ggsave(p.rev,filename="tmp-indiv-rev.png",width=w,height=h)
  ggsave(m,filename="tmp-means.png",width=w,height=h)
  ggsave(m.rev,filename="tmp-means-rev.png",width=w,height=h)
  
  # Cropping - 300 dpi
  wpix<-300*w
  hpix<-300*h
  
  # Set height for Secchi l
  hsec<-hpix*1.05/6.2
  
  GpimageCrop("tmp-indiv-rev.png","tmp-secchi.png",1,hpix-hsec,wpix,hpix)
  GpimageCrop("tmp-indiv.png","tmp-indiv2.png",1,1,wpix,hpix-hsec)
  GpimageCrop("tmp-means-rev.png","tmp-secchi-means.png",1,hpix-hsec,wpix,hpix)
  GpimageCrop("tmp-means.png","tmp-means2.png",1,1,wpix,hpix-hsec)
  
  GpimageTile(sprintf("%s-%s.png",lake,year),
              matrix(c("tmp-means2.png","tmp-indiv2.png","tmp-secchi-means.png","tmp-secchi.png"),ncol=2),
              rep.int(wpix,2),
              c(hpix-hsec,hsec)
  )


}

# Plot a lake with standard L1 and L2 data
lake.plot.L1<-function(data.L1,data.L2,lake,year) {
  
  L2<-lake.plot.facet(data.L2,lake,year,wy=F,means=F,params.list="noST")
  temp<-lake.plot.facet(data.L1,lake,year,wy=T,means=F,params.list="ST")
  secchi<-temp+scale_y_reverse()

  m.L2<-lake.plot.facet(data.L2,lake,year,wy=F,means=T,params.list="noST")
  m.temp<-lake.plot.facet(data.L1,lake,year,wy=T,means=T,params.list="ST")
  m.secchi<-m.temp+scale_y_reverse()
  
  # For tweaking, set width and height once
  w<-6
  h<-10
  h.L2<-h*4.2/6.2
  h.st<-h*2.2/6.2
  
  ggsave(L2,filename="tmp-indiv-L2.png",width=w,height=h.L2)
  ggsave(secchi,filename="tmp-indiv-secchi.png",width=w,height=h.st)
  ggsave(temp,filename="tmp-indiv-temp.png",width=w,height=h.st)
  
  ggsave(m.L2,filename="tmp-means-L2.png",width=w,height=h.L2)
  ggsave(m.secchi,filename="tmp-means-secchi.png",width=w,height=h.st)
  ggsave(m.temp,filename="tmp-means-temp.png",width=w,height=h.st)
  

  
  # Cropping - 300 dpi
  wpix<-300*w
  hpix.L2<-300*h.L2
  hpix.st<-300*h.st
  
  # Set crop height for Secchi
  hsec<-hpix.st/2.15
  
  # Crop and combine secchi and temp first
  GpimageCrop("tmp-indiv-secchi.png","tmp-secchi.png",1,hpix.st-hsec,wpix,hpix.st)
  GpimageCrop("tmp-means-secchi.png","tmp-secchi-means.png",1,hpix.st-hsec,wpix,hpix.st)
  GpimageCrop("tmp-indiv-temp.png","tmp-temp.png",1,1,wpix,hpix.st-hsec)
  GpimageCrop("tmp-means-temp.png","tmp-temp-means.png",1,1,wpix,hpix.st-hsec)
  
  GpimageTile("tmp-st.png",
              matrix(c("tmp-temp.png","tmp-secchi.png"),ncol=2),
              wpix,
              c(hpix.st-hsec,hsec)
  )
  GpimageTile("tmp-st-means.png",
              matrix(c("tmp-temp-means.png","tmp-secchi-means.png"),ncol=2),
              wpix,
              c(hpix.st-hsec,hsec)
  )
           

  # Then tile with L2
  GpimageTile(sprintf("%s-%s.png",lake,year),
              matrix(c("tmp-means-L2.png","tmp-indiv-L2.png","tmp-st-means.png","tmp-st.png"),ncol=2),
              rep.int(wpix,2),
              c(hpix.L2,hpix.st)
  )
  # The x-axis is the same for secchi/temp and L2 for the means, but need to repeat it to make the plots line up side-by-side
  
  
}
