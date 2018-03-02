### ABM figure for showing proportion by gear sectors for Alternatives 4 and 5, and maybe 6
## dh 3/1/2018
app_plot<-function(mratio,mrpn) {
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(reshape2)
mratio_l<-melt(mratio)
mratio_l$Year<-rep(as.numeric(row.names(mratio)),6)
names(mratio_l)<-c("Area","Proportion","Year")
mratio_l$Proportion<-round(mratio_l$Proportion*100,0)

fill <- topo.colors(6)
p4 <- ggplot() +
  geom_bar(aes(y = Proportion, x = Year, fill = Area), data = mratio_l, stat="identity") +
 # geom_text(data=mratio_l, aes(x = Year, y = Proportion, label = paste0(Proportion,"%")),
#            colour="black", family="Tahoma", size=3.5) +
#  geom_hline(yintercept = trawl_ratio, colour="dark gray",size=2)+
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) +
  theme(legend.text=element_text(size=12))+
  scale_x_continuous(breaks=seq(2000,as.numeric(row.names(mratio)[nrow(mratio)]),3)) +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  labs(x="Year", y="Percentage",size=14) +
  ggtitle("Allocation by ratio of mature females")+
  scale_fill_manual(values=fill) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.title=element_text(size=16),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12))
p4
ggsave("ratio_allocation.png",width=7,height=5,dpi=300)

mratio_l<-melt(mrpn)
mratio_l$Year<-rep(as.numeric(row.names(mratio)),6)
names(mratio_l)<-c("Area","Proportion","Year")
mratio_l$Proportion<-round(mratio_l$Proportion*100,0)

fill <- topo.colors(6)
p4 <- ggplot() +
  geom_bar(aes(y = Proportion, x = Year, fill = Area), data = mratio_l, stat="identity") +
  # geom_text(data=mratio_l, aes(x = Year, y = Proportion, label = paste0(Proportion,"%")),
  #            colour="black", family="Tahoma", size=3.5) +
  #  geom_hline(yintercept = trawl_ratio, colour="dark gray",size=2)+
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) +
  theme(legend.text=element_text(size=12))+
  scale_x_continuous(breaks=seq(2000,2017,3)) +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  labs(x="Year", y="Percentage",size=14) +
  ggtitle("Allocation by RPN of mature females")+
  scale_fill_manual(values=fill) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.title=element_text(size=16),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12))
p4
ggsave("mature_rpn_allocation.png",width=7,height=5,dpi=300)
}