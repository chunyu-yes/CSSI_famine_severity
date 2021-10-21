library(ggplot2)
library(gridExtra)
library(ggrepel)
library(reshape2)
library(plotly)
library(dplyr)
library(plyr)
library(ggExtra)
library(tidyverse)
library(grid)
library(ggpubr)
library(measurements)

#######################################################################
#                       Input population data
#######################################################################

# Input the population data 
## Input the national population data
china_pop<-read.csv('{path/folder containing your files}/code/China_pop.csv') #source: 2000 census from 'https://international.ipums.org/international/'
## Input the provincial population data
province_pop<-read.csv('{path/folder containing your files}/code/Province_pop.csv') #source: 2000 census from 'https://international.ipums.org/international/'


#######################################################################
#                           Figure 2
#######################################################################
# The national figure cohort size by birth year between 1900-2000
fig2.1<-ggplot(china_pop,aes(x=Birthyear,y=China))+geom_bar(stat="identity",fill=ifelse(china_pop$Group==1,'#66cdaa','gray40'))+xlab('Birthyear')+ylab('Cohort size')+scale_y_continuous(limits = c(0,300000),breaks = c(0,100000,200000),labels=c('0','0.1M','0.2M'))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.position="none", axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = 7),axis.title.y = element_text(size = 7),axis.text.x = element_text(size =6),axis.text.y = element_text(size =6))

china_pop2<-china_pop[52:72,]
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
pdf("Fig2hh.pdf", width =w_in, height =h_in)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2,1)))
print(fig2.1,vp = viewport(layout.pos.row = 1, layout.pos.col = 1:1))
print(fig2.2,vp = viewport(layout.pos.row = 2, layout.pos.col = 1:1))
dev.off()

#######################################################################
#                       Input map data
#######################################################################

# Input and the province-level map data
tw <- readRDS("{path/folder containing your files}/code/gadm36_TWN_0_sf.rds") #Taiwan
china_map <- readRDS("{path/folder containing your files}/code/gadm36_CHN_1_sf.rds")#Chinese mainland
tws <- st_sf(GID_0 = "CHN",NAME_0 = "China",GID_1 = "CHN.32_1",
             NAME_1 = "Taiwan",VARNAME_1 = "",NL_NAME_1 = "台湾",
             TYPE_1 = "",ENGTYPE_1 = "",CC_1 = "",HASC_1 = "",geometry = tw$geometry)
china_map <- rbind(china_map,tws) #combine Taiwan and Chinese Mainland

## Combine the map data and CSSI data
### Keep the province names of two data the same
china_map$NAME_1[19]<-c('Inner.Mongolia')
china_map$NAME_1[20]<-c('Ningxia')
china_map$NAME_1[28]<-c('Xinjiang')
china_map$NAME_1[29]<-c('Tibet')

### Extract CSSI data from Main analyses1.R
china_cssi59_61_2<-china_cssi59_61[,c(1,3,9)]
china_cssi59_61_2$provinces_abbreviation<-c('BJ','TJ','HeB','SX','IM','LN','JL','HLJ','SH','JS','ZJ','AH','FJ','JX','SD','HeN','HuB','HuN','GD','GX','SC','GZ','YN','TB','SaX','GS','QH','NX','XJ')#add abbreviations of the province names
names(china_cssi59_61_2)[3]<-'NAME_1'

#### Before 1980s, Hainan was part of Guangdong province, and Chongqing was part of Sichuan province.
#### When calculating provincial CSSI, the population of Hainan and Chongqing is regarded as a part of population of Guangdong and Sichuan respectively, so their CSSIs are also Guangdong's and Sichuan's CSSIs at provincial level.
china_cssi59_61_2[30,]<-china_cssi59_61_2[21,]
china_cssi59_61_2[30,3]<-'Chongqing'
china_cssi59_61_2[30,4]<-'CQ'
china_cssi59_61_2[31,]<-china_cssi59_61_2[19,]
china_cssi59_61_2[31,3]<-'Hainan'
china_cssi59_61_2[31,4]<-'HaiN'

### Combine the map data and CSSI data
china_map_province<-left_join(china_map,china_cssi59_61_2,by="NAME_1")
names(china_map_province)[11:12]<-c('cssi_1','cssi_2')
china_map_province$provinces_abbreviation[32]<-'TW'

#######################################################################
#                           Figure 3
#######################################################################

# Set the color palette for map1-2
cbPalette <- c("#99D594","#FFFFB2","#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#B10026")

# Map for province-level CSSI based on data before famine years
map1 <- ggplot() +
  geom_sf(data = china_map_province,size=0.5,color='#292421',aes(fill =cut(cssi_1,c(-Inf,10,20,30,40,50,60,70,Inf))))+
  geom_sf_text(data = china_map_province,aes(label=provinces_abbreviation),size=2)+ 
  coord_sf(crs = "+proj=aeqd +lat_0=37 +lon_0=104")+
  scale_fill_manual(name='CSSI',values=cbPalette,guide=guide_legend(reverse=T),na.value ="white")+theme_classic()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.border = element_blank())

# Map for province-level CSSI based on data before and after famine years
map2 <- ggplot() +
  geom_sf(data = china_map_province,size=0.5,color='#292421',aes(fill =cut(cssi_2,c(-Inf,10,20,30,40,50,60,70,Inf))))+
  geom_sf_text(data = china_map_province,aes(label=provinces_abbreviation),size=2)+ 
  coord_sf(crs = "+proj=aeqd +lat_0=37 +lon_0=104")+
  scale_fill_manual(name='CSSI',values=cbPalette,guide=guide_legend(reverse=T),na.value ="white")+theme_classic()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.border = element_blank())

# Exclude 5 special provinces in map3
china_map_province2<-china_map_province
china_map_province2$cssi_1[which(china_map_province$provinces_abbreviation=='BJ'|china_map_province$provinces_abbreviation=='TJ'|china_map_province$provinces_abbreviation=='SH'|china_map_province$provinces_abbreviation=='XJ'|china_map_province$provinces_abbreviation=='TB')]<-NA
china_map_province2$cssi_2[which(china_map_province$provinces_abbreviation=='BJ'|china_map_province$provinces_abbreviation=='TJ'|china_map_province$provinces_abbreviation=='SH'|china_map_province$provinces_abbreviation=='XJ'|china_map_province$provinces_abbreviation=='TB')]<-NA

# Set the color palette for map3
cbPalette2 <- c("#084594","#4292C6","#6BAED6",'#E0E0E0',"#F1B6DA","#DE77AE","#C51B7D","#8E0152")
# Map for province-level difference of CSSI based on data before famine years
map3<- ggplot() +
  geom_sf(data = china_map_province2,size=0.5,color='#292421',aes(fill =cut(cssi_1-cssi_2,c(-Inf,-20,-15,-10,-5,5,10,15,20,Inf))))+
  geom_sf_text(data = china_map_province2,aes(label=provinces_abbreviation),size=2)+
  coord_sf(crs = "+proj=aeqd +lat_0=37 +lon_0=104")+
  scale_fill_manual(name='CSSI',values=cbPalette2,guide=guide_legend(reverse=T),na.value ="white")+
  theme_classic()+theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.border = element_blank())

# Output
w_in <- conv_unit(54, "cm",'inch')
h_in <- conv_unit(18, "cm",'inch')
pdf("fig3_look.pdf", width =w_in, height =h_in)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3)))
print(map1,vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map2,vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(map3,vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
dev.off()

#######################################################################
#                           Figure 4
#######################################################################
# Extract CSSI data from Main analyses1.R
provinces <- china_cssi59_61$X
cssi_1 = china_cssi59_61$round.linear.pre
cssi_2 = china_cssi59_61$round.linear.pre.post
provinces <- provinces[c(-29,-24,-1,-2,-9)] #exclude 5 special provinces
cssi_1 = cssi_1[c(-29,-24,-1,-2,-9)] #exclude 5 special provinces
cssi_2 = cssi_2[c(-29,-24,-1,-2,-9)] #exclude 5 special provinces
provinces_abbreviation<-c('HeB','SX','IM','LN','JL','HLJ','JS','ZJ','AH','FJ','JX','SD','HeN','HuB','HuN','GD','GX','SC','GZ','YN','SaX','GS','QH','NX') #add abbreviations of province names
fig4_data <- data.frame(cssi_1, cssi_2, provinces_abbreviation=factor(provinces_abbreviation,levels = provinces_abbreviation[order(-colMeans(rbind(cssi_1,cssi_2)))]),provinces = factor(provinces, levels = provinces[order(-colMeans(rbind(cssi_1,cssi_2)))]))

# Calculate the fitting lines for fig4 
a<-lm(cssi_1~as.numeric(provinces),data=fig4_data)
b1<-predict(a)
a<-lm(cssi_2~as.numeric(provinces),data=fig4_data)
b2<-predict(a)

# Figure for the distribution of two CSSIs
p<-plot_ly(fig4_data, color = I("gray80"),
           width=380,
           height=200) %>%
  add_lines(y = ~b1, x = ~provinces_abbreviation, color = I("red"),showlegend = FALSE) %>%
  add_lines(y = ~b2, x = ~provinces_abbreviation, color = I("#4b0082"),showlegend = FALSE) %>%
  add_segments(y = ~cssi_1, yend = ~cssi_2, x = ~provinces_abbreviation, xend = ~provinces_abbreviation, showlegend = FALSE,color=I("black"),line=list(width=0.5)) %>%
  add_markers(y = ~cssi_1, x = ~provinces_abbreviation, name = "pre", color = I("red"),  marker = list(size = 3.5)) %>%
  add_markers(y = ~cssi_2, x = ~provinces_abbreviation, name = "pre & post", color = I("#4b0082"),  marker = list(size =3.5)) %>%
  add_markers(y = ~colMeans(rbind(cssi_1,cssi_2)), x = ~provinces_abbreviation, name = "Mean", color = I("black"), marker = list(size = 2),showlegend = FALSE)%>%
  layout(
    xaxis = list(tickfont=list(size=6),title = FALSE, tickangle=0),
    yaxis = list(tickfont=list(size=6),title = "CSSI(%)",titlefont=list(size=7)),
    margin = list(l=7,r=7,t=7,b=7),
    legend = list(x=0.75,y=1,font=list(size=6.5),bgcolor='transparent'),
    plot_bgcolor='white',
    paper_bgcolor = "white",
    fig_bgcolor   = "white")

# Output
export(p, file = "fig4.pdf")
