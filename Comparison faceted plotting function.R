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
lake.plot.facet.comparison<-function(data,lake,year,y,wy=F,means=F,params.list="L2"){
 
  has.trends<-F # only the means calculate this, so set here for individual.
  
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
  
  # Set up lake names and comparisons
  lakenames<-read_csv("lakelatlong.csv",
                    col_types=cols(
                      Lake = col_character(),
                      LakeName=col_character(),
                      Latitude = col_double(),
                      Longitude = col_double()
                    )) %>%
    mutate(LakeName=str_to_title(LakeName)) %>%
    select(Lake,LakeName)
  
  # Can change comparison lakes here and will propagate throughout
  highlake<-"Cottage"
  lowlake<-"Pipe"
  lakelist<-c(lake,highlake,lowlake)
  
  y<-y %>%
    mutate(max=ifelse(Parameter=="ChlorophyllA",40,max))
  
  namelist<-lakenames %>%
    filter(Lake %in% lakelist) %>%
    .$LakeName %>%
    unique()
  
  
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
           Date<=end,
           Lake %in% lakelist) %>%
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
    
    y<-mutate(y,label=meanslabel)
    
    # Annual means.
    d.means<-data %>%
      filter(Depth==1,
             Parameter %in% params.list,
             Lake %in% lakelist) %>%
      mutate(Year=year(Date),
             Month=month(Date)) %>%
      filter(Year>=1994,
             Year<=year,
             Month>=5,
             Month<=10) %>%
      # Average by month first and then by year to help buffer against data gaps or
      # years with partial L1 (and all L2) data
      group_by(Parameter,Year,Lake,Month) %>%
      summarize(Value=mean(Value,na.rm=T)) %>%
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
    
    d.background<-d.means %>%
      left_join(trimlist,by=c("Lake","Year")) %>%
      filter(Keep) %>%
      select(-Keep) %>%
      left_join(y,by="Parameter")
    
    # Years without data are likely simply skipped -- insert NAs to break the line
    year.list<-expand.grid(Year=full_seq(d.means$Year,1),
                           Parameter=params.list) %>%
      mutate(Parameter=as.character(Parameter))
    
    d<-d.means %>%
      left_join(trimlist,by=c("Lake","Year")) %>%
      filter(Keep,Lake==lake) %>%
      select(-Keep) %>%
      full_join(year.list,by=c("Parameter","Year")) %>%
      left_join(y,by="Parameter") 
      
    # Adding the year.list added lots of NA years, so finding start/end requires filtering
    start<-d %>%
      filter(!is.na(Value)) %>%
      {min(.$Year,na.rm=T)}
    end<-d %>%
      filter(!is.na(Value)) %>%
      {max(.$Year,na.rm=T)} 
    
    # Trendlines
    trends<-lake.trend.seasonal(data,lake,year,params.list,overall.only=T) %>%
      filter(p<0.05)

    has.trends<-nrow(trends)>0
  } # end means

  # Filter chlorophyll background data
  # For means, this filters after the means are calculated
  d.background<-d.background %>%
    filter(Parameter!="ChlorophyllA" | Value<chlor.max,
           !is.na(Value)) %>%
  # Rename lakes as focus, high, and low to set plot aesthetics manually
    mutate(Lake=ifelse(Lake==lake,"focus",Lake),
           Lake=ifelse(Lake==highlake,"high",Lake),
           Lake=ifelse(Lake==lowlake,"low",Lake)
    )
  
  
  

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
    # Comparison lakes
    geom_line(aes(size=Lake,color=Lake))+
    scale_size_manual(values=c("focus"=1,"high"=0.75,"low"=0.75),
                      breaks=lakelist,labels=namelist)+
    geom_point(aes(shape=Lake,color=Lake),size=2)+
    scale_shape_manual(values=c("focus"=19,"high"=17,"low"=15),
                       breaks=lakelist,labels=namelist)+
    scale_color_manual(values=c("focus"="blue","high"="grey50","low"="grey50"),
                       breaks=lakelist,labels=namelist)+
    # Lake-specific data - re-plot separately to plot on top
    geom_line(data=d,color="blue",size=1)+
    geom_point(data=d,size=3,color="blue",shape=19)+
    # Ensure that plot doesn't get smaller than specified limits
    geom_blank(aes(y=0))+geom_blank(aes(y=max))+
    theme(axis.text.x=element_text(size=11),
          axis.text.y=element_text(size=11),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()
    )
  
  if("NPRatio" %in% params.list) {
    p<-p+geom_hline(data=filter(d,Parameter=="NPRatio"),aes(yintercept=25),color="grey",size=0.5)
  }
  
  
  if(has.trends) {
    t<-trends %>%
      filter(Parameter %in% params.list) %>%
      left_join(y,by="Parameter")

    p<-p+geom_abline(data=t,aes(slope=slope,intercept=intercept),linetype="dashed",size=1)
  }
      
  p
}


# Plot a lake that has only L2 data
lake.plot.L2<-function(data,lake,year,y) {
  
  p<-lake.plot.facet.comparison(data,lake,year,y,wy=F,means=F,params.list="L2")
  p.rev<-p+scale_y_reverse()
  
  m<-lake.plot.facet.comparison(data,lake,year,y,wy=F,means=T,params.list="L2")
  m.rev<-m+scale_y_reverse()
  
  # For tweaking, set width and height once
  w<-6
  h<-12
  dpi<-200
  
  ggsave(p,filename="Tmp/tmp-indiv.png",width=w,height=h,dpi=dpi)
  ggsave(p.rev,filename="Tmp/tmp-indiv-rev.png",width=w,height=h,dpi=dpi)
  ggsave(m,filename="Tmp/tmp-means.png",width=w,height=h,dpi=dpi)
  ggsave(m.rev,filename="Tmp/tmp-means-rev.png",width=w,height=h,dpi=dpi)
  
  # Cropping
  wpix<-dpi*w
  hpix<-dpi*h
  
  # Set height for Secchi l
  hsec<-hpix*1.05/6.2
  
  GpimageCrop("Tmp/tmp-indiv-rev.png","Tmp/tmp-secchi.png",1,hpix-hsec,wpix,hpix)
  GpimageCrop("Tmp/tmp-indiv.png","Tmp/tmp-indiv2.png",1,1,wpix,hpix-hsec)
  GpimageCrop("Tmp/tmp-means-rev.png","Tmp/tmp-secchi-means.png",1,hpix-hsec,wpix,hpix)
  GpimageCrop("Tmp/tmp-means.png","Tmp/tmp-means2.png",1,1,wpix,hpix-hsec)
  
  GpimageTile(sprintf("%s/Plots/%s-%s-WQ.png",year,year,lake),
              matrix(c("Tmp/tmp-means2.png","Tmp/tmp-indiv2.png","Tmp/tmp-secchi-means.png","Tmp/tmp-secchi.png"),ncol=2),
              rep.int(wpix,2),
              c(hpix-hsec,hsec)
  )


}

# Plot a lake with standard L1 and L2 data
lake.plot.L1<-function(data.L1,data.L2,lake,year,y) {
  
  L2<-lake.plot.facet.comparison(data.L2,lake,year,y,wy=F,means=F,params.list="noST")
  temp<-lake.plot.facet.comparison(data.L1,lake,year,y,wy=T,means=F,params.list="ST")
  secchi<-temp+scale_y_reverse()

  m.L2<-lake.plot.facet.comparison(data.L2,lake,year,y,wy=F,means=T,params.list="noST")
  # For secchi and temp means, use both L1 and L2 data since many lakes have a
  # longer L2 record than they do L1
  m.temp<-bind_rows(data.L1,data.L2) %>%
    lake.plot.facet.comparison(lake,year,y,wy=T,means=T,params.list="ST")
  m.secchi<-m.temp+scale_y_reverse()
  
  # For tweaking, set width and height once
  w<-6
  h<-12
  dpi<-200
  h.L2<-h*4.2/6.2
  h.st<-h*2.2/6.2
  
  ggsave(L2,filename="Tmp/tmp-indiv-L2.png",width=w,height=h.L2,dpi=dpi)
  ggsave(secchi,filename="Tmp/tmp-indiv-secchi.png",width=w,height=h.st,dpi=dpi)
  ggsave(temp,filename="Tmp/tmp-indiv-temp.png",width=w,height=h.st,dpi=dpi)
  
  ggsave(m.L2,filename="Tmp/tmp-means-L2.png",width=w,height=h.L2,dpi=dpi)
  ggsave(m.secchi,filename="Tmp/tmp-means-secchi.png",width=w,height=h.st,dpi=dpi)
  ggsave(m.temp,filename="Tmp/tmp-means-temp.png",width=w,height=h.st,dpi=dpi)
  

  
  # Cropping
  wpix<-dpi*w
  hpix.L2<-dpi*h.L2
  hpix.st<-dpi*h.st
  
  # Set crop height for Secchi
  hsec<-hpix.st/2.15
  
  # Crop and combine secchi and temp first
  GpimageCrop("Tmp/tmp-indiv-secchi.png","Tmp/tmp-secchi.png",1,hpix.st-hsec,wpix,hpix.st)
  GpimageCrop("Tmp/tmp-means-secchi.png","Tmp/tmp-secchi-means.png",1,hpix.st-hsec,wpix,hpix.st)
  GpimageCrop("Tmp/tmp-indiv-temp.png","Tmp/tmp-temp.png",1,1,wpix,hpix.st-hsec)
  GpimageCrop("Tmp/tmp-means-temp.png","Tmp/tmp-temp-means.png",1,1,wpix,hpix.st-hsec)
  
  GpimageTile("Tmp/tmp-st.png",
              matrix(c("Tmp/tmp-temp.png","Tmp/tmp-secchi.png"),ncol=2),
              wpix,
              c(hpix.st-hsec,hsec)
  )
  GpimageTile("Tmp/tmp-st-means.png",
              matrix(c("Tmp/tmp-temp-means.png","Tmp/tmp-secchi-means.png"),ncol=2),
              wpix,
              c(hpix.st-hsec,hsec)
  )
           

  # Then tile with L2
  GpimageTile(sprintf("%s/Plots/%s-%s-WQ.png",year,year,lake),
              matrix(c("Tmp/tmp-means-L2.png","Tmp/tmp-indiv-L2.png","Tmp/tmp-st-means.png","Tmp/tmp-st.png"),ncol=2),
              rep.int(wpix,2),
              c(hpix.L2,hpix.st)
  )
  # The x-axis is the same for secchi/temp and L2 for the means, but need to repeat it to make the plots line up side-by-side
  
  
}
