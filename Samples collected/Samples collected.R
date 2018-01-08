data<-L2.data
year<-2017

latlong<-read_csv("lakelatlong.csv",
                  col_types=cols(
                    Lake = col_character(),
                    LakeName=col_character(),
                    Latitude = col_double(),
                    Longitude = col_double()
                  ))

params.list<-c("TotalPhosphorus",
               "TotalNitrogen",
               "ChlorophyllA")

d<-data %>%
  filter(Depth==1,
         Parameter %in% params.list,
         !is.na(Value)) %>%
  mutate(Year=year(Date),
         Month=month(Date)) %>%
  filter(Year==year,
         Month>=5,
         Month<=10) %>%
  group_by(Year,Lake) %>%
  summarize(n=length(unique(Date))) %>%
  left_join(latlong,by="Lake")





# Static ggmap
pad<-0.025
box<-d %>%
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
         base_layer=ggplot(data=d,aes(x=Longitude,y=Latitude,color=n)))+
  geom_point(size=4)+
  geom_label_repel(aes(label=Lake,fill=n),color="white",size=4)+
  scale_color_gradient(low="darkred",high="darkblue",name="# of samples")+
  scale_fill_gradient(low="darkred",high="darkblue",name="# of samples")+
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

ggsave(s,filename="Samples collected.png",width=5.25,height=8)

d %>%
  ungroup() %>%
  select(Lake,n) %>%
  write_csv(sprintf("Samples collected in %s.csv",year))
