##analyses MWT data generated from the Plate_average_sbMme.sh shell script
##load MWT plate average data
##replace the text encompassed by quotes in the read.table function below with the path to your file
plate.data <- read.table("/Users/michelleroux/Documents/Tiffany/MWT_data/TZ_mutants/Locomotion/merged.file")

##split up column V1 into strain name, plate identifier and time
library(stringr)
df_V1 <- plate.data$V1
strain_tag  <- "./[a-zA-Z0-9_-]{1,}"
strain_names  <- str_extract(df_V1, strain_tag)
strain_names  <- sub("./", "", strain_names)
plate_tag  <- "/[0-9_]{1,}"
plate_names  <- str_extract(df_V1, plate_tag)
plate_names  <- sub("/", "", plate_names)
time_tag  <- ":[0-9.]{1,}"
time  <- str_extract(df_V1, time_tag)
time  <- sub(":", "", time)
df.temp <- cbind(strain_names, plate_names, time, plate.data[,c(3:7)])
plate.data <- df.temp
rm(df.temp, df_V1, plate_tag, strain_tag, time_tag, strain_names, plate_names, time)

##rename columns 
colnames(plate.data) <- c("strain","plate","time", "speed", "bias", "morphwidth", "midline", "area")

##make time numeric
plate.data$time <- as.numeric(levels(plate.data$time))[plate.data$time]

##plot speed decay over 10 min
attach(plate.data)
strains <- unique(strain)
##bin into time intervals to make it quicker to plot (average speed over every 30s for 10 min)
cut1 <- cut(time, breaks=seq(0, 600, by = 20))
plate.data.tint <- plate.data
plate.data.tint$time <- cut1
times <- as.character(plate.data.tint$time)
int_tag <- ",[0-9]{1,}"
int <- str_extract(times,int_tag)
int <- sub(",", "", int)
int <- as.numeric(int)
plate.data.tint$time <- int
detach(plate.data)

##plate.data.tint.agg <- aggregate(plate.data.tint, by=list(plate.data.tint$strain,plate.data.tint$time), FUN=mean, na.rm=TRUE)
library(plyr)
plate.data.tint.plate <- ddply(plate.data.tint,.(strain,time,plate),summarise,speed=mean(speed))
plate.data.tint.agg <- ddply(plate.data.tint.plate,.(strain, time),summarise,N=length(speed),mean.speed=mean(speed),sd=sd(speed), se=sd/sqrt(N))

##get rid of data from 0-40s of the experiment (sometimes the tracker doesn't start tracking 
##until 15s into the experiment)
plate.data.tint.agg <- plate.data.tint.agg[which(plate.data.tint.agg$time>40),]

##make plot
##plot the points
plot(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[3])] ,plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[3])], col=3, pch=18, xlab="Time(s)",ylab="Speed(mm/s)", ylim = c(0,1.7))
points(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[2])] ,plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[2])],col=2,pch=18)
points(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[1])] ,plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[1])],col=4,pch=18)
points(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[2])] ,plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[4])],col=1,pch=18)
##points(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[1])] ,plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[5])],col=8,pch=18)
##plot the error bars (standard error)
segments(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[3])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[3])]-plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[3])],plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[3])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[3])]+plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[3])], col=3)
segments(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[2])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[2])]-plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[2])],plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[2])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[2])]+plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[2])], col=2)
segments(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[1])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[1])]-plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[1])],plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[1])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[1])]+plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[1])], col=4)
segments(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[4])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[4])]-plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[4])],plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[4])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[4])]+plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[4])], col=1)
##segments(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[5])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[5])]-plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[5])],plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[5])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[5])]+plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[5])], col=8)
##plot the legend
legend("topright",bty="n",y.intersp=1,c(as.character(strains[1]),as.character(strains[2]),as.character(strains[3]), as.character(strains[4])),col=c(3,2,4,1),pch=(18))


##save plot
pdf(file="/Users/michelleroux/Documents/Tiffany/MWT_data/TZ_mutants/Locomotion/Speed_decay.pdf", width=5, height=5)
plot(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[3])] ,plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[3])], col=3, pch=18, xlab="Time(s)",ylab="Speed(mm/s)", ylim = c(0,1.7))
points(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[2])] ,plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[2])],col=2,pch=18)
points(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[1])] ,plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[1])],col=4,pch=18)
points(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[2])] ,plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[4])],col=1,pch=18)
##points(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[1])] ,plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[5])],col=8,pch=18)
##plot the error bars (standard error)
segments(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[3])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[3])]-plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[3])],plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[3])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[3])]+plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[3])], col=3)
segments(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[2])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[2])]-plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[2])],plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[2])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[2])]+plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[2])], col=2)
segments(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[1])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[1])]-plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[1])],plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[1])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[1])]+plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[1])], col=4)
segments(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[4])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[4])]-plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[4])],plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[4])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[4])]+plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[4])], col=1)
##segments(plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[5])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[5])]-plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[5])],plate.data.tint.agg$time[which(plate.data.tint.agg$strain==strains[5])],plate.data.tint.agg$mean.speed[which(plate.data.tint.agg$strain==strains[5])]+plate.data.tint.agg$se[which(plate.data.tint.agg$strain==strains[5])], col=8)
##plot the legend
legend("topright",bty="n",y.intersp=1,c(as.character(strains[1]),as.character(strains[2]),as.character(strains[3]), as.character(strains[4])),col=c(3,2,4,1),pch=(18))
dev.off()



##summarize body area
df.area <- ddply(plate.data.tint,.(strain,time,plate),summarise,area=mean(area))
df.area <- df.area[which(df.area$time==40),]
df.area.agg <- ddply(df.area,.(strain, time),summarise,N=length(area),mean.area=mean(area),sd=sd(area), se=sd/sqrt(N))
##look at the plot
boxplot(df.area$area~df.area$strain, xlab="Strain",ylab="Body size (mm^2)", ylim = c(4,11))
##save the plot 
pdf("/Users/michelleroux/Documents/Tiffany/MWT_data/TZ_mutants/Locomotion/body_size_area.pdf", width=6, height=6)
boxplot(df.area$area~df.area$strain, xlab="Strain",ylab="Body size (mm^2)", ylim = c(4,11))
dev.off()

##summarize length
df.length <- ddply(plate.data.tint,.(strain,time,plate),summarise,length=mean(midline))
df.length <- df.length[which(df.length$time==40),]
df.length.agg <- ddply(df.length,.(strain, time),summarise,N=length(length),mean.length=mean(length),sd=sd(length), se=sd/sqrt(N))

##plot the data
boxplot(df.length$length~df.length$strain, xlab="Strain",ylab="Body length (mm)", ylim = c(6.5,10))
##save the data
pdf("/Users/michelleroux/Documents/Tiffany/MWT_data/TZ_mutants/Locomotion/body_size_length.pdf", width=6, height=6)
boxplot(df.length$length~df.length$strain, xlab="Strain",ylab="Body length (mm)", ylim = c(6.5,10))
dev.off()

##summarize width
df.width <- ddply(plate.data.tint,.(strain,time,plate),summarise,width=mean(morphwidth))
df.width <- df.width[which(df.width$time==40),]
df.width.agg <- ddply(df.width,.(strain, time),summarise,N=length(width),mean.width=mean(width),sd=sd(width), se=sd/sqrt(N))

##plot the data
boxplot(df.width$width~df.width$strain, xlab="Strain",ylab="Body width (mm)")
##save the data
pdf("/Users/michelleroux/Documents/Tiffany/MWT_data/TZ_mutants/Locomotion/body_size_width.pdf", width=6, height=6)
boxplot(df.width$width~df.width$strain, xlab="Strain",ylab="Body width (mm)")
dev.off()

pdf("/Users/michelleroux/Documents/Tiffany/MWT_data/TZ_mutants/Locomotion/body_size_all_measures.pdf", width=6, height=12)
par(mfrow=c(3,1))
boxplot(df.area$area~df.area$strain, xlab="Strain",ylab="Body size (mm^2)")
boxplot(df.length$length~df.length$strain, xlab="Strain",ylab="Body length (mm)")
boxplot(df.width$width~df.width$strain, xlab="Strain",ylab="Body width (mm)")
dev.off()

