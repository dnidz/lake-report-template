library(tidyverse)
library(lubridate)
library(scales)

# Read in the data
# This contains TN data for all lakes.
d.background<-read_csv("Graphing test data.csv",
                       col_types=cols(
                         Lake = col_character(),
                         Date = col_date(format = ""),
                         Depth = col_integer(),
                         Parameter = col_character(),
                         Value = col_integer()
                       )) %>%
  mutate(Date=as_date(Date))

# Create a subset that's Lake Wilderness only
d<-filter(d.background,Lake=="Wilderness")

start<-as_date("2017-05-01")
end<-as_date("2017-09-30")

# Plot
p<-ggplot(d.background,aes(x=Date,y=Value))+
  scale_x_date(date_breaks="1 month",date_labels="%m/1/%y",limits=c(start,end))+
  coord_cartesian(ylim=c(0,1000))+
  
  # Background data
  geom_jitter(color="black",size=2,alpha=0.25,width=2)+

  # Lake-specific data
  geom_line(data=d,color="blue",size=1)+geom_point(data=d,size=3,color="blue")+

  theme(axis.text.x=element_text(size=11),
        axis.text.y=element_text(size=11)
  ) +
  labs(y="Total N",x=element_blank())

# Save the plot as a png file
ggsave(p,file="Graphing test output.png",width=6,height=3)
