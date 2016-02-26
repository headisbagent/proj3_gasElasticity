rm(list=ls())
setwd("C:/Users/Public")

library(data.table)
library(dummies)
library(plm)
library(nlme)
library(mgcv)
library(xtable)
library(lfe)
library(pglm)
library(ggplot2)
library(Hmisc)

#############################################################################################################
#Setting Up The Data#
#############################################################################################################

#Note that the inspect.txt files are not available publicly

readin <- c("inspect1.txt","inspect2.txt","inspect3.txt","inspect4.txt","inspect5.txt","inspect6.txt","inspect7.txt","inspect8.txt","inspect9.txt","inspect10.txt","inspect11.txt")
readin2 <- c("inspect12.txt","inspect13.txt","inspect14.txt")

#read in and rbind inspection data (for regular inspections)
inspections <- data.table(NULL)
for(i in 1:length(readin)) {
	hold <- read.table(readin[i],header=FALSE,sep=",",colClasses=c("NULL",NA,NA,"NULL","NULL","NULL","NULL","NULL",NA,NA,"NULL","NULL","NULL","NULL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),col.names=c("id","vin","date","testtype","testorder","overall","emissions","visual","odometer","zip","county","shortvin","linebreak","shortvin2","modelyear","make","model","country","body","bodygeneral","liters","engine","enginetype","drive","mpghwy","mpgcity","msrp","weight"))
	hold$date <- as.Date(hold$date)
	inspections <- rbind(inspections,hold)
}
remove(hold)
gc()

for(i in 1:length(readin2)) {
	hold <- read.table(readin2[i],header=FALSE,sep=",",colClasses=c("NULL",NA,NA,NA,NA,"NULL","NULL","NULL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),col.names=c("id","vin","date","odometer","zip","exemption","shortvin","shortvin2","modelyear","make","model","country","body","bodygeneral","liters","engine","enginetype","drive","mpghwy","mpgcity","msrp","weight"))
	hold$date <- as.Date(hold$date)
	inspections <- rbind(inspections,hold)
}
remove(hold)
gc()

#unique ids from VINs
inspections <- transform(inspections,id=as.numeric(vin))

#reordering columns and rows
setcolorder(inspections,c(19,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))
inspections[,vin:=NULL]
setkey(inspections,id)

#############################################################################################################
#Continuing data prep

#delete singular VINs from dataframe
get.keep.index2 <- function(id) {
	X<-table(id)
	v<-as.vector(X)
	names(v)<-names(X)
	v<-v[v>1]
	keep.index<-is.element(id,names(v))
	keep.list <- which(keep.index==TRUE)
	return(keep.list)
}
inspections <- inspections[get.keep.index2(inspections$id),]

#setting dates of inspection and previous date of inspection for each record
inspections$p.date <- c(inspections$date[1],inspections$date)[-(length(inspections$date)+1)]
inspections$p.datemonth <- as.numeric(format(inspections$p.date,format="%m"))
inspections$p.dateyear <- as.numeric(format(inspections$p.date,format="%Y"))
inspections$datemonth <- as.numeric(format(inspections$date,format="%m"))
inspections$dateyear <- as.numeric(format(inspections$date,format="%Y"))

#calculating vehicle age
inspections$age <- inspections$dateyear-inspections$modelyear

#time dummies
inspections$datemonth <- as.numeric(format(inspections$date,format="%m"))
inspections$dateyear <- as.numeric(format(inspections$date,format="%Y"))
setkey(inspections,datemonth,dateyear)
timedummies <- data.frame('datemonth'=rep(1:12,11),'dateyear'=as.vector(mapply(rep,2000:2010,12)),'timedummy'=1:132)
timedummies <- data.table(timedummies)
setkey(timedummies,datemonth,dateyear)
inspections <- inspections[timedummies,roll=TRUE,allow.cartesian=TRUE]
setkey(inspections,id)

#############################################################################################################
#Adding macroecon variables
#############################################################################################################

econindic <- read.csv('econindic.csv',header=TRUE,col.names=c('date','unemp.adj','unemp.unadj','gasprices','gdp','oilprices','oilshutin'))
econindic$date <- as.Date(econindic$date,'%m/%d/%Y')

#gas prices
pdf('C:/Users/ajenn/Dropbox/CarnegieMellon/Research/Project6_PennRebound/AnalysisWriteup/figures/trends_gas.pdf',height=6,width=7.5)
par(mar=c(6,6,2,2))
plot(econindic$date,econindic$gasprices,type='n',xlab='Time (monthly)',ylab='Average PA Gas Prices ($)',cex.lab=1.5,xaxt='n',yaxt='n')
grid()
axis(1,las=0,tck=.02,at=econindic$date[format(econindic$date,"%m")=='01'],labels=format(econindic$date[format(econindic$date,"%m")=='01'],"%Y"),cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
lines(econindic$date,econindic$gasprices,lwd=2.5)
dev.off()

#combined gdp, unemp
pdf('C:/Users/ajenn/Dropbox/CarnegieMellon/Research/Project6_PennRebound/AnalysisWriteup/figures/trends_gdp_unemp.pdf',height=6,width=9)
par(mar=c(5,6.5,4,6.5))
plot(econindic$date,econindic$gdp/1000,type='l',xlab='Time (monthly)',ylab=NA,cex.lab=1.5,xaxt='n',yaxt='n',lwd=3)
axis(1,las=0,tck=.02,at=econindic$date[format(econindic$date,"%m")=='01'],labels=format(econindic$date[format(econindic$date,"%m")=='01'],"%Y"),cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
mtext('Gross Domestic Product',line=3.5,side=2,cex=1.5)
par(new=TRUE)
plot(econindic$date,econindic$unemp.adj,type='l',xlab=NA,ylab=NA,cex.lab=1.5,xaxt='n',yaxt='n',lwd=3,col='red')
axis(4,at=c(4,5,6,7,8,9),cex.axis=1.5,las=1,tck=.02)
mtext("Unemployment Index",line=3,side=4,cex=1.5)
legend('topleft',c('GDP','Unemployment'),lty=c(1,1),lwd=c(3,3),col=c('black','red'),bg='white')
dev.off()


#prepping data frame of macroeconomic variables
avgecon <- data.frame('avgunemp.adj'=numeric(),'avgunemp.unadj'=numeric(),'avggas'=numeric(),'avggdp'=numeric(),'avgoil'=numeric(),'avgshutin'=numeric(),'p.date'=as.Date(character()),'date'=as.Date(character()))

#generate averages over all period combinations
for(i in 1:nrow(econindic)) {
	for(j in i:nrow(econindic)) {
		avgecon <- rbind(avgecon,c(mean(econindic$unemp.adj[i:j]),mean(econindic$unemp.unadj[i:j]),mean(econindic$gasprices[i:j]),mean(econindic$gdp[i:j]),mean(econindic$oilprices[i:j]),mean(econindic$oilshutin[i:j]),econindic$date[i],econindic$date[j]))
	}
}
names(avgecon) <- c('avgunemp.adj','avgunemp.unadj','avggas','avggdp','avgoil','avgshutin','p.date','date')

#adjusting dates from excel
avgecon$p.date <- as.Date(avgecon$p.date,origin='1970-01-01')
avgecon$date <- as.Date(avgecon$date,origin='1970-01-01')

avgecon$p.datemonth <- as.numeric(format(avgecon$p.date,format="%m"))
avgecon$p.dateyear <- as.numeric(format(avgecon$p.date,format="%Y"))
avgecon$datemonth <- as.numeric(format(avgecon$date,format="%m"))
avgecon$dateyear <- as.numeric(format(avgecon$date,format="%Y"))

#drop dates
avgecon <- avgecon[,c(1,2,3,4,5,6,9,10,11,12)]

#prep for merging tables
setkey(inspections,p.datemonth,p.dateyear,datemonth,dateyear)
avgecon <- data.table(avgecon)
setkey(avgecon,p.datemonth,p.dateyear,datemonth,dateyear)

#merging inspection data and macroeconomic data
inspections <- inspections[avgecon,roll=TRUE,allow.cartesian=TRUE]
setkey(inspections,id,date)

#cleanup
inspections[,p.datemonth:=NULL]
inspections[,p.dateyear:=NULL]
inspections[,datemonth:=NULL]
inspections[,dateyear:=NULL]

inspections <- na.omit(inspections)

#############################################################################################################
#Obtaining VMT from the odometer readings and dates of inspectionss
#############################################################################################################

#get vmt and dates function
get.diffs <- function(id,date,odometer,avggas) {
	do<-c(odometer[1],diff(odometer))
	dd<-c(0,diff(date))
	dg<-c(0,diff(avggas))
	X<-table(id)
	cX<-cumsum(X)+1
	drop.ind<-c(1,cX[1:(length(cX)-1)])
	return(list(deltaodometer=do,deltadate=dd,drop.ind=drop.ind,deltagas=dg))
}
hold <- get.diffs(inspections$id,inspections$date,inspections$odometer,inspections$avggas)

#add vmt and dates, drop 1st
inspections$miles.travel <- hold$deltaodometer
inspections$timediff <- hold$deltadate
inspections$annual.vmt <- inspections$miles.travel/inspections$timediff*365
inspections$gasdiff <- hold$deltagas
inspections$pc.gas <- inspections$gasdiff/inspections$avggas
inspections <- inspections[-hold$drop.ind,]
inspections <- inspections[inspections$annual.vmt >= 0 & is.finite(inspections$annual.vmt)]
rm(hold)
gc()

#############################################################################################################
#Demographics
#############################################################################################################

inspections$year <- as.numeric(format(inspections$date,format="%Y"))

setkey(inspections,id,date)

inspections <- na.omit(inspections)

#adding category levels
inspections[,fecity.cat:=cut(inspections$mpgcity,breaks=c(-Inf,20,30,40,max(inspections$mpgcity)),labels=c(1,2,3,4))]
inspections[,fehighway.cat:=cut(inspections$mpghwy,breaks=c(-Inf,20,30,40,max(inspections$mpghwy)),labels=c(1,2,3,4))]
inspections[,vmt.avg:=mean(annual.vmt),by='id']
inspections[,vmt.quant:=cut(inspections$vmt.avg,breaks=c(0,quantile(inspections$vmt.avg,probs=c(.2,.4,.6,.8)),max(inspections$vmt.avg)),labels=c(1,2,3,4,5))]
inspections[,gas.cat:=cut(inspections$avggas,breaks=c(1,2,3,4,max(inspections$avggas)),labels=c(1,2,3,4))]
inspections[,gas.avg:=mean(avggas),by='id']
inspections[,gas.quant:=cut(inspections$gas.avg,breaks=c(0,quantile(inspections$gas.avg,probs=c(.2,.4,.6,.8)),max(inspections$gas.avg)),labels=c(1,2,3,4,5))]
inspections[,fe.quant:=cut(inspections$mpgcity,breaks=c(0,quantile(inspections$mpgcity,probs=c(.2,.4,.6,.8)),max(inspections$mpgcity)),labels=c(1,2,3,4,5))]


inspections <- inspections[inspections$annual.vmt>0&inspections$annual.vmt<200000]
inspections$age <- inspections$age+1
inspections <- inspections[inspections$age>0,]

#############################################################################################################
#Summary Stats
#############################################################################################################

regCount <- data.frame('year'=c(2000:2010),'regCount'=c(7166668,7537835,7428064,7581661,7653461,7721703,7701845,7774535,7817110,7829113,7839661),'inspCount'=table(inspections$year))
regCount$percent <- regCount$inspCount.Freq/regCount$regCount

pdf('summary_inspcount_byyear.pdf',height=6,width=9)
par(mar=c(4,6,3,6))
plot(x=regCount$year,y=regCount$inspCount.Freq/10^6,xlab='Year',ylab='Number of Inspections (millions)',type='l',xaxt='n',yaxt='n',cex=2,cex.lab=1.5,lwd=2)
axis(1,las=0,tck=.02,cex.axis=1.5)
axis(2,las=2,tck=.02,cex.axis=1.5)
par(new=TRUE)
plot(x=regCount$year,y=regCount$percent,type='l',xlab=NA,ylab=NA,xaxt='n',yaxt='n',lwd=2,col='red',ylim=c(0,1))
axis(4,at=c(0,.2,.4,.6,.8,1),cex.axis=1.5,las=1,tck=.02)
mtext('Percent of Registered Vehicles',line=4,side=4,cex=1.5)
legend('topleft',c('Number of Inspections','% of Registered Vehicles'),lty=c(1,1),lwd=c(2,2),col=c('black','red'))
dev.off()

annualSummaryStats <- function() {
	yearsHold <- 2000:2010
	summaryVar <- c('annual.vmt','odometer','avggas','age','mpgcity','avgunemp.adj','avggdp')
	output <- matrix(,nrow=length(summaryVar)*2,ncol=length(yearsHold))
	rowHold <- 1
	for(column in summaryVar) {
		hold <- as.data.frame(inspections[,list(mean=mean(get(column)),sd=sd(get(column))),by=year])
		columnHold <- numeric(0)
		colPosition <- 1
		for(years in yearsHold) {
			rowPosition <- rowHold
			output[rowPosition,colPosition] <- hold[hold$year==years,2]
			rowPosition <- rowPosition+1
			output[rowPosition,colPosition] <- hold[hold$year==years,3]
			rowPosition <- rowPosition+1
			colPosition <- colPosition+1
		}
		rowHold <- rowHold+2
	}
	return(output)
}

annualStats <- annualSummaryStats()
write.table(annualStats,file='C:/Users/ajenn/Dropbox/CarnegieMellon/Research/Project6_PennRebound/AnalysisWriteup/annualStats.csv',sep=',')

pdf('C:/Users/ajenn/Dropbox/CarnegieMellon/Research/Project6_PennRebound/AnalysisWriteup/figures/boxplot_year_vmt.pdf')
boxplot(annual.vmt~year,data=inspections,outline=FALSE,xlab='Year',ylab='Annual VMT (mi)')
dev.off()

pdf('C:/Users/ajenn/Dropbox/CarnegieMellon/Research/Project6_PennRebound/AnalysisWriteup/figures/boxplot_age_vmt.pdf')
boxplot(annual.vmt~age,data=inspections,outline=FALSE,xlab='Vehicle Age',ylab='Annual VMT (mi)')
dev.off()

summaryByZip <- inspections[,list(avgMPG=mean(mpghwy),avgVMT=mean(annual.vmt)),by=zip]
write.table(summaryByZip,file='C:/Users/ajenn/Dropbox/CarnegieMellon/Research/Project6_PennRebound/AnalysisWriteup/summaryByZip.csv',sep=',',col.names=TRUE)


