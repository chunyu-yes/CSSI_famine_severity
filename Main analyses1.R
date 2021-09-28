
#######################################################################
#                       Input data
#######################################################################

## Input the population data 
# Input the national population data
china_pop<-read.csv('{path/folder containing your files}/code/China_pop.csv') #source: 2000 census from 'https://international.ipums.org/international/'
# Input the provincial population data
province_pop<-read.csv('{path/folder containing your files}/code/Province_pop.csv') #source: 2000 census from 'https://international.ipums.org/international/'

#######################################################################
#                       Estimate provincial CSSI---Table 1
#######################################################################

# Calculate the slope and intercept of the linear trend
slope_intercept<-matrix(nrow=3)
for(i in 2:30){
  pre<-lm(province_pop[1:8,i]~province_pop[1:8,1])
  post<-lm(province_pop[14:21,i]~province_pop[14:21,1])
  pre_post<-lm(province_pop[c(1:8,14:21),i]~province_pop[c(1:8,14:21),1])
  a<-rbind(pre$coefficients,post$coefficients,pre_post$coefficients)
  names(a)
  slope_intercept<-cbind(slope_intercept,a)
}
slope_intercept<-slope_intercept[,-1]

# Estimate the population
observe<-colSums(province_pop[10:12,])[-1] # Observed population between 1959-1961
linear.pre<-rep(1,29)
for(id in 1:29){linear.pre[id]<-(slope_intercept[1,2*id-1]*3+1959*slope_intercept[1,2*id]+1960*slope_intercept[1,2*id]+1961*slope_intercept[1,2*id])} # Population between 1959-1961 estimated based pre-famine years
linear.pre.post<-rep(1,29)
for(id in 1:29){linear.pre.post[id]<-(slope_intercept[3,2*id-1]*3+1959*slope_intercept[3,2*id]+1960*slope_intercept[3,2*id]+1961*slope_intercept[3,2*id])} # Population between 1959-1961 estimated based on pre- & post- famine years
china_pop59_61<-data.frame(observe,linear.pre,linear.pre.post)
View(china_pop59_61)
china_pop59_61$X<-rownames(china_pop59_61)

# Calculate the CSSI based on the estimated population
linear.pre<-(china_pop59_61$linear.pre-china_pop59_61$observe)*100/china_pop59_61$linear.pre # CSSI based on pre-famine years
linear.pre.post<-(china_pop59_61$linear.pre.post-china_pop59_61$observe)*100/china_pop59_61$linear.pre.post # CSSI based on pre- & post- famine years
linear.mean<-rowMeans(cbind(linear.pre,linear.pre.post)) # Mean of the CSSIs based on different non-famine years
linear.difference<-linear.pre-linear.pre.post # CSSI based on pre-famine years minus CSSI based on pre- & post- famine years
china_cssi59_61<-data.frame(linear.pre,linear.pre.post,linear.mean,linear.difference)
china_cssi59_61<-round(china_cssi59_61,1)
View(china_cssi59_61)

china_cssi59_61<-data.frame(round(linear.pre,1),rank(-linear.pre),round(linear.pre.post,1),rank(-linear.pre.post),round(linear.mean,1),rank(-linear.mean),round(linear.difference,1),rank(-linear.difference)) # Add the ranking
names(china_cssi59_61)<-c('round.linear.pre','rank.linear.pre','round.linear.pre.post','rank.linear.pre.post','round.linear.mean','rank.linear.mean','round.linear.difference','rank.linear.difference')
china_cssi59_61$X<-china_pop59_61$X
china_cssi59_61<-china_cssi59_61[order(china_cssi59_61$rank.linear.mean),] #Table 1

# The prefecture level CSSIs were also calculated in this way.
# The raw population data on prefecture level to calculate the CSSI were not uploaded. The prefecture-level CSSI was provided instead. 
