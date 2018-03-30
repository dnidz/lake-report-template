# Hydrology plot (lake level and precip)
# Daniel Nidzgorski
# November 13, 2017

hydro.plot<-function(data,lake,year) {
  
  Oct<-as.Date(sprintf("%s-10-01",year-1))
  Sept<-as.Date(sprintf("%s-09-30",year))
  
  d<-subset(data,Lake==lake)
  
  # Weekly sums/averages
  # First make a "Week" column with the previous Sunday's date
  d$Week<-floor_date(d$Date,"week")
  
  d.summary<-d %>%
    group_by(Week) %>%
    summarize(Precip=sum(Precipitation,na.rm=T),
              Level=mean(Level,na.rm=T)
    )
  

  
    p<-ggplot(d.summary,aes(x=Week,y=Precip))+
    geom_col()+
    geom_line(aes(x=Week,y=Level/.75))+
    scale_y_continuous(name="Weekly Precipitation (mm)",
                       limits=c(0,200),breaks=seq(0,200,25),
                       sec.axis=sec_axis(trans=~.*.75,breaks=seq(0,150,25),
                                         name="Lake Level (cm)"
                       )
    )+
    scale_x_date(date_breaks="2 months",date_labels="%m/1/%y",limits=c(Oct,Sept))+
    theme(axis.text.x=element_text(size=9),
          axis.text.y=element_text(size=9),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size=11)
    )
    
    p

}
