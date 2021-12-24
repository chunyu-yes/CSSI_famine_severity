library(ggplot2)
library(gridExtra)
library(ggrepel)
library(stringi)
library(reshape2)
library(plotly)
library(dplyr)
library(plyr)
library(ggExtra)
library(tidyverse)
library(grid)
library(ggpubr)
library(measurements)
library(sf)
#######################################################################
#                       Input population data
#######################################################################

# Input the population data 
## Input the national population data
china_pop<-read.csv('/Users/liuchunyu/Documents/Lumey New/CSSI_famine_severity-main/data/China_pop.csv') #source: 2000 census from 'https://international.ipums.org/international/'
## Input the provincial population data
province_pop<-read.csv('/Users/liuchunyu/Documents/Lumey New/CSSI_famine_severity-main/data/Province_pop.csv') #source: 2000 census from 'https://international.ipums.org/international/'


#######################################################################
#                           Figure 2
#######################################################################
# The national figure cohort size by birth year between 1900-2000
fig2.1<-ggplot(china_pop,aes(x=Birthyear,y=China))+geom_bar(stat="identity",fill=ifelse(china_pop$Group==1,'#66cdaa','gray40'))+xlab('Birthyear')+ylab('Cohort size')+scale_y_continuous(limits = c(0,300000),breaks = c(0,100000,200000),labels=c('0','0.1M','0.2M'))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.position="none", axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = 7),axis.title.y = element_text(size = 7),axis.text.x = element_text(size =6),axis.text.y = element_text(size =6))

china_pop2<-china_pop[51:71,]
lm(china_pop2[1:8,2]~china_pop2[1:8,1])#slope: 5623 ; intercept: -10817971
lm(china_pop2[14:21,2]~china_pop2[14:21,1])#slope: -165 ; intercept: 562901

# The national figure of cohort size by birth year between 1950-1970
fig2.2<-ggplot(china_pop2,aes(x=Birthyear,y=China))+geom_bar(stat="identity",fill=ifelse(china_pop2$Group==1,'#66cdaa','gray40'))+xlab('Birthyear')+ylab('Cohort size')+
  geom_segment(aes(x=1949.5,y=-10817971+1949.5*5623,xend=1957.5,yend=-10817971+1957.5*5623),color='red')+geom_segment(aes(x=1957.5,y=-10817971+1957.5*5623,xend=1970.5,yend=-10817971+1970.5*5623),linetype = "dashed",color='red')+
  geom_segment(aes(x=1949.5,y=562901+1949.5*-165,xend=1962.5,yend=562901+1962.5*-165),linetype = "dashed",color='#1E90FF')+geom_segment(aes(x=1962.5,y=562901+1962.5*-165,xend=1970.5,yend=562901+1970.5*-165),color='#1E90FF')+
  scale_x_continuous(breaks = c(1950,1957,1959,1961,1963,1970))+scale_y_continuous(limits = c(0,300000),breaks = c(0,100000,200000),labels=c('0','0.1M','0.2M'))+
  geom_text(aes(x =1949.5, y =300000,label = "pre:y=5623*x-10817971"),col='red',hjust = 0,size=2.67)+geom_text(aes(x =1949.5, y =280000, label = "post:y=-165*x+562901"),col='#1E90FF',hjust = 0,size=2.67)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.position="none", axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = 7),axis.title.y = element_text(size = 7),axis.text.x = element_text(size =6),axis.text.y = element_text(size =6))

# Combine two figures
w_in <- conv_unit(11.4, "cm",'inch')
h_in <- conv_unit(14, "cm",'inch')
pdf("./CSSI/Fig2.pdf", width =w_in, height =h_in)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2,1)))
print(fig2.1,vp = viewport(layout.pos.row = 1, layout.pos.col = 1:1))
print(fig2.2,vp = viewport(layout.pos.row = 2, layout.pos.col = 1:1))
dev.off()

#######################################################################
#                       Input map data
#######################################################################
# Input and the province-level map data
chinamap<-read_sf('/Users/liuchunyu/Documents/Lumey New/Chinamapdata.json')
head(chinamap)
#chinamap<-st_simplify(chinamap, preserveTopology = FALSE, dTolerance = 10)
# Add abbreviation of province names
chinamap$provinces_abbreviation<-c('BJ','TJ','HeB','SX','IM','LN','JL','HLJ','SH','JS','ZJ','AH','FJ','JX','SD','HeN','HuB','HuN','GD','GX','HaiN','CQ','SC','GZ','YN','TB','SaX','GS','QH','NX','XJ','TW','XG','AM','NH','NH')

# Extract CSSI data from Main analyses1.R
View(china_cssi59_61_2000_birth) 

# Keep the abbreviation of province names in two data the same
cssi_1<-china_cssi59_61_2000_birth[,c(1,3,7,9)]
cssi_1[25,]<-cssi_1[1,]
cssi_1[25,4]<-'CQ'
cssi_1[26,]<-cssi_1[18,]
cssi_1[26,4]<-'HaiN'
names(cssi_1)[4]<-'provinces_abbreviation'

# Combine the map data and CSSI data
fig3data<-left_join(chinamap,cssi_1,by='provinces_abbreviation')

#######################################################################
#                           Figure 3.1-3.3
#######################################################################

# Map for province-level CSSI based on data before famine years
cbPalette1 <- c("#FFFFB2","#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#B10026")
map1 <- ggplot() +
  geom_sf(data = fig3data,size=0.5,color='#292421',aes(fill =cut(round.linear.pre,c(-Inf,10,20,30,40,50,60,70,Inf))))+
  geom_sf_text(data = fig3data,aes(label=provinces_abbreviation),size=2.23)+ 
  coord_sf()+
  scale_fill_manual(name='CSSI (%)',values=cbPalette1,guide=guide_legend(reverse=T),na.value ="white")+theme_classic()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.border = element_blank())

# Map for province-level CSSI based on data before and after famine years
cbPalette2 <- c("#FFFFB2","#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#B10026")
map2 <- ggplot() +
  geom_sf(data = fig3data,size=0.5,color='#292421',aes(fill =cut(round.linear.pre.post,c(-Inf,10,20,30,40,50,60,70,Inf))))+
  geom_sf_text(data = fig3data,aes(label=provinces_abbreviation),size=2.23)+ 
  coord_sf()+
  scale_fill_manual(name='CSSI (%)',values=cbPalette2,guide=guide_legend(reverse=T),na.value ="white")+theme_classic()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.border = element_blank())

# Map for province-level difference between CSSI based on data before famine years and before and after famine years
cbPalette3 <- c("#084594","#4292C6","#6BAED6",'#E0E0E0',"#F1B6DA","#DE77AE","#C51B7D","#8E0152")

map3<- ggplot() +
  geom_sf(data = fig3data,size=0.5,color='#292421',aes(fill =cut(round.linear.difference,c(-Inf,-20,-15,-10,-5,5,10,15,20,Inf))))+
  geom_sf_text(data = fig3data,aes(label=provinces_abbreviation),size=2.23)+
  coord_sf()+
  scale_fill_manual(name='CSSI difference (%)',values=cbPalette3,guide=guide_legend(reverse=T),na.value ="white")+theme_classic()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.border = element_blank())

# output
w_in <- conv_unit(35, "cm",'inch')
h_in <- conv_unit(15, "cm",'inch')
pdf("./CSSI/Fig3.1-3.pdf", width =w_in, height =h_in)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3)))
print(map1,vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map2,vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(map3,vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
dev.off()
#######################################################################
#                           Figure 3.4
#######################################################################
# Extract CSSI data from Main analyses1.R

provinces <- china_cssi59_61_2000_birth$X
cssi_1 = china_cssi59_61_2000_birth$round.linear.pre
cssi_2 = china_cssi59_61_2000_birth$round.linear.pre.post

fig4_data <- data.frame(cssi_1, cssi_2,provinces_abbreviation = factor(provinces, levels = provinces[order(-colMeans(rbind(cssi_1,cssi_2)))]))

# Calculate the fitting lines for fig4 
a<-lm(cssi_1~as.numeric(provinces_abbreviation),data=fig4_data)
b1<-predict(a)
a<-lm(cssi_2~as.numeric(provinces_abbreviation),data=fig4_data)
b2<-predict(a)

#color
fig4_data$color<-NA
fig4_data$color[which((fig4_data$cssi_1-fig4_data$cssi_2)<=-15)]<-"#084594"
fig4_data$color[which((fig4_data$cssi_1-fig4_data$cssi_2)>-15 & (fig4_data$cssi_1-fig4_data$cssi_2)<=-10)]<-"#4292C6"
fig4_data$color[which((fig4_data$cssi_1-fig4_data$cssi_2)>-10 & (fig4_data$cssi_1-fig4_data$cssi_2)<=-5)]<-"#6BAED6"
fig4_data$color[which((fig4_data$cssi_1-fig4_data$cssi_2)>-5 & (fig4_data$cssi_1-fig4_data$cssi_2)<=5)]<-'#E0E0E0'
fig4_data$color[which((fig4_data$cssi_1-fig4_data$cssi_2)>5 & (fig4_data$cssi_1-fig4_data$cssi_2)<=10)]<-"#F1B6DA"
fig4_data$color[which((fig4_data$cssi_1-fig4_data$cssi_2)>10 & (fig4_data$cssi_1-fig4_data$cssi_2)<=15)]<-"#DE77AE"
fig4_data$color[which((fig4_data$cssi_1-fig4_data$cssi_2)>15)]<-"#C51B7D"
table(fig4_data$color)

# Figure for the distribution of two CSSIs

p<-plot_ly(fig4_data, color = I("gray80"),
           width=380,
           height=200) %>%
  add_lines(y = ~b1, x = ~provinces_abbreviation, color = I("black"),showlegend = FALSE) %>%
  add_lines(y = ~b2, x = ~provinces_abbreviation,line=list(dash='dash',color='black') ,showlegend = FALSE)
  
for (k in 1:nrow(fig4_data)){
    p<-add_segments(p,data=fig4_data[k,],inherit=FALSE,y = ~cssi_1, yend = ~cssi_2, x = ~provinces_abbreviation, xend = ~provinces_abbreviation, showlegend = FALSE,line=list(width=3.5,color=~color))
}

p <- p %>%
  add_markers(data=fig4_data,y = ~cssi_1, x = ~provinces_abbreviation, name = "Pre", color = I("black"),  marker = list(size =6) ) %>%
  add_markers(data=fig4_data,y = ~cssi_2, x = ~provinces_abbreviation, name = "Pre & post", marker = list(type = 'circle',color='white',size=4.5,line=list(color='black',width=1.5))) %>%
  add_markers(data=fig4_data,y = ~colMeans(rbind(cssi_1,cssi_2)), x = ~provinces_abbreviation, name = "Mean", color = I("black"), marker = list(size =3.5),showlegend = FALSE)
  


p <- p %>%
 layout(
    xaxis = list(tickfont=list(size=6,color='black'),title = FALSE, tickangle=0),
    yaxis = list(tickfont=list(size=6,color='black'),title = "CSSI (%)",titlefont=list(size=7,color='black'),dtick = 10),
    margin = list(l=7,r=7,t=7,b=7),
    legend = list(x=0.75,y=1,font=list(size=6.5,color='black'),bgcolor='transparent'),
    plot_bgcolor='white',
    paper_bgcolor = "white",
    fig_bgcolor   = "white")

p

# Output
export(p, file = "fig3.4.pdf") 

## Manually combine fig3.1-4 in adobe illustrator

