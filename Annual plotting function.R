# Annual lake plotting function
# Daniel Nidzgorski
# October 27, 2017

# For everything except precip/level
# Returns a ggplot object

# Set wy=T to plot for a full water year
annual.plot<-function(data,lake,year,param,wy=F){
 
  y<-tribble(
    ~Parameter, ~label, ~min, ~max, ~breaksize,
    "TotalPhosphorus", "Total Phosphorus (µg/L)", 0, 100, 20,
    "TotalNitrogen", "Total Nitrogen (µg/L)", 0, 1000, 200,
    "Temperature", "Water temperature (°C)", 0, 30, 5,
    'TotalAlk', "mg CaCO3/L", 0, 25, 5,
    "NPRatio", "N:P ratio", 0, 100, 25,
    "ChlorophyllA", "Chlorophyll-a (µg/L)", 0, 40, 10,
    "Secchi", "Secchi depth (m)", 0, 6, 2,
    "UV.Absorbance", "UV254 absorbance", 0, 1, 0.2
  ) %>%
    filter(Parameter==param)

  if(!wy) {
    start<-as.Date(sprintf("%s-05-01", year))
    end<-as.Date(sprintf("%s-10-31", year))
  } else {
    start<-as.Date(sprintf("%s-10-01", year-1))
    end<-as.Date(sprintf("%s-09-30", year))
  }
  
  d.all<-data %>%
    filter(Parameter==param,
           Depth==1)
  d<-d.all %>%
    filter(Lake==lake)
  
  p<-ggplot(d,aes(x=Date,y=Value))+
    geom_line()+geom_point(size=3)+
    scale_x_date(date_breaks="1 month",date_labels="%m/1/%y",limits=c(start,end))+
    scale_y_continuous(breaks=seq(y$min,y$max,y$breaksize))+
    coord_cartesian(ylim=c(y$min,y$max))+
    theme(axis.text.x=element_text(size=11),
          axis.text.y=element_text(size=11),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size=13)
    )+ 
    labs(y=y$label)
  
  # Parameter-specific add-ons:
  if(param=="NP") {
    p<-p+geom_hline(aes(yintercept=25),color="grey",size=0.5)
  } else if(param=="Secchi") {
    p<-p+scale_y_reverse(breaks=seq(y$min,y$max,y$breaksize))+
      coord_cartesian(ylim=c(y$max,y$min),xlim=c(1996,2016))
  }
  
p
}
