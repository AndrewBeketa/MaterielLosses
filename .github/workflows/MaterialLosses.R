#########Andrew Beketa####
########12/10/22##########
###### Material Losses ####

setwd(" ")   ##Make sure to set this right so everything works###

##first, lets load our data so we can use it



losses <- read.csv("Losses.csv")


##Subset the data so we can work with months and not the day by day format.

losses$Date <- as.Date(losses$Date)
head(losses$Date)
class(losses$Date)

## Subset data from February to December 2022
sub <- subset(losses, Date >= as.Date("2022-02-28") &
                     Date <= as.Date("2022-12-09"))

## Format date so that we can work with months
sub$monthyear <- format(sub$Date, "%Y-%m")
range(sub$monthyear)

## Average Losses for Russia and Ukraine by month
## These are our y inputs

RUlosses <- tapply(sub$Russia_Total, sub$monthyear, mean,
                            na.rm=T)

UAlosses <- tapply(sub$Ukraine_Total, sub$monthyear, mean,
                   
                            na.rm=T)
RAFlosses <- tapply(sub$Russia_Aircraft, sub$monthyear, mean,
                    na.rm=T)

UAFlosses <-tapply(sub$Ukraine_Aircraft, sub$monthyear, mean,
                   na.rm=T)

## Want to plot them each at their own point on the x-axis, from the first month to the last month. 

1:length(RUlosses)


plot(x=1:length(RUlosses),
     y=RUlosses,
     type="l", 
     main="Russian Materiel Losses in Ukraine by Month",
     ylab="Materiel Losses (vehicle)",
     xlab="",
     ylim = c(0, 10000), # y-axis limits
     las=1, # orientation of axis labels
     lwd=2, # line width
     bty="n",
     xaxt="n") # removes border
axis(1, at = 1:length(RUlosses), 
     labels=names(RUlosses), las=2)

## Add line to the plot 
lines(x=1:length(UAlosses),
      y=UAlosses, col="red3", lwd=2)

legend("topleft",  col=c("red3", "black"), 
       c("Ukraine", "Russia"), 
       cex = .7, # size of legend
       lwd=2,
       bty="n")

abline(v=8.1, lty=10, col="dodgerblue", lwd=1.5)
text(x=8.1, y=8980, labels = "Beginning of \n Kharkiv/Kherson Counteroffensive", cex=.6)

abline(v=10.1, lty=10, col="dodgerblue", lwd=1.5)
text(x=10.1, y=7080, labels = "Kherson \n Retaken", cex=.6)



plot(x=1:length(RAFlosses),
     y=RAFlosses,
     type="l", 
     main="Russian Air Losses in Ukraine by Month",
     ylab="Materiel Losses (Aircraft)",
     xlab="",
     ylim = c(0, 400), # y-axis limits
     las=1, # orientation of axis labels
     lwd=2, # line width
     bty="n",
     xaxt="n") # removes border
axis(1, at = 1:length(RAFlosses), 
     labels=names(RAFlosses), las=2)

lines(x=1:length(UAFlosses),
      y=UAFlosses, col="red3", lwd=2)

abline(v=8.1, lty=10, col="dodgerblue", lwd=1.5)
text(x=8.1, y=300, labels = "Beginning of \n Kharkiv/Kherson Counteroffensive", cex=.6)

abline(v=10.1, lty=10, col="dodgerblue", lwd=1.5)
text(x=10.1, y=210, labels = "Kherson \n Retaken", cex=.6)

legend("topleft",  col=c("red3", "black"), 
       c("Ukraine", "Russia"), 
       cex = .7, # size of legend
       lwd=2,
       bty="n")

