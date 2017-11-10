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
    "TotalNitrogen", "Total Nitrogen (ug/L)", 0, 1000, 200,
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
}


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

  # Filter chlorophyll
  d.background<-d.background %>%
    filter(Parameter!="ChlorophyllA" | Value<chlor.max,
           !is.na(Value))
  

  # Nucleate plot
  if(!means) {
    
    p<-ggplot(d.background,aes(x=Date,y=Value))+
      scale_x_date(date_breaks="1 month",date_labels="%m/1/%y",limits=c(start,end))
    
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
  
  tile.files<-matrix(c("tmp-means2.png","tmp-indiv2.png","tmp-secchi-means.png","tmp-secchi.png"),ncol=2)
  tile.widths<-rep.int(wpix,2)
  tile.heights<-c(hpix-hsec,hsec)
  
  GpimageTile(sprintf("%s-%s.png",lake,year),
              tile.files,tile.widths,tile.heights)

}


# Plot a lake with standard L1 and L2 data
lake.plot.L1<-function(data,lake,year) {
  
  L2<-lake.plot.facet(data,lake,year,wy=F,means=F,params.list="noST")
  secchi<-lake.plot.facet(data,lake,year,wy=T,means=F,params.list="Secchi")+
    scale_y_reverse()
  temp<-lake.plot.facet(data,lake,year,wy=T,means=F,params.list="Temperature")
  
  m.L2<-lake.plot.facet(data,lake,year,wy=F,means=T,params.list="noST")
  m.secchi<-lake.plot.facet(data,lake,year,wy=T,means=T,params.list="Secchi")+
    scale_y_reverse()
  m.temp<-lake.plot.facet(data,lake,year,wy=T,means=T,params.list="Temperature")
  
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
  
  tile.files<-matrix(c("tmp-means2.png","tmp-indiv2.png","tmp-secchi-means.png","tmp-secchi.png"),ncol=2)
  tile.widths<-rep.int(wpix,2)
  tile.heights<-c(hpix-hsec,hsec)
  
  GpimageTile(sprintf("%s-%s.png",lake,year),
              tile.files,tile.widths,tile.heights)
  
}
