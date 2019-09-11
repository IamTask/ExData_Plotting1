# Set Workspace containing txt data 
#setwd("~/Scrivania/Exploratory Data Analysis/Project 1")

# Data Load
raw_data = read.csv("household_power_consumption.txt", header = TRUE, sep = ";") 
head(raw_data)

# Taking only the portion I need
subs = subset(raw_data, as.Date(Date, "%d/%m/%Y") >= as.Date("2007/02/01") & as.Date(Date,  "%d/%m/%Y") <= as.Date("2007/02/02"))
head(subs)
summary(subs)


# Loading Plotting Library
library(ggplot2)

# Plot 1: histogram
png("Plot_1_histogram.png")
qplot(as.numeric(subs$Global_active_power), geom="histogram", col = "black", fill= "red", bins = 12)
dev.off()
#computing datetime field
#as.POSIXct("1/2/2007 00:02:03", format = "%d/%m/%Y %H:%M:%S")
subs$datetime = as.POSIXct(paste(subs$Date," ", subs$Time), format = "%d/%m/%Y %H:%M:%S")
head(subs$datetime)

#loading library
library(lattice)

# Plot 2: line plot
# l = line plot
# at = positions at which we want the labels
global_active_power_datetime <- function(){
   summary(subs$Global_active_power)
   xyplot(Global_active_power ~ datetime , 
          data = subs,
          type = c("l"),
          scales = list(
             x=list(
                at=c(
                  as.POSIXct("2007-02-01 00:00:00 CET", "%Y-%m-%d %H:%M:%S"), 
                  as.POSIXct("2007-02-02 00:00:00 CET", "%Y-%m-%d %H:%M:%S"), 
                  as.POSIXct("2007-02-03 00:00:03 CET", "%Y-%m-%d %H:%M:%S")
               ),
               labels = c("Thu", "Fri", "Sat")
             ),
            y=list(
               at=c(0,100,500, 900,1000,1100),
               limits = c(0, 1100)
            )
         ),
         ylab = "Global Active Power (Kilowatts)"
   )
}
png("Plot_2_global_active_power_datetime.png")
global_active_power_datetime()
dev.off()

# Plot 3: another line plot, energy submetering
energy_submetering <- function(){
summary(subs$Sub_metering_1)
   xyplot(Sub_metering_1 + Sub_metering_2 + Sub_metering_3 ~ datetime , 
          data = subs,
          type = c("l"),
          scales = list(
            x=list(
               at=c(
                 as.POSIXct("2007-02-01 00:00:00 CET", "%Y-%m-%d %H:%M:%S"), 
                 as.POSIXct("2007-02-02 00:00:00 CET", "%Y-%m-%d %H:%M:%S"), 
                 as.POSIXct("2007-02-03 00:00:03 CET", "%Y-%m-%d %H:%M:%S")
               ),
               labels = c("Thu", "Fri", "Sat")
            ),
            y=list(
               at=c(0,10,15,20,25),
               limits = c(0, 24)
            )
          ),
          auto.key = list(
             corner = c(1, 1)
          ),
          par.settings = list(superpose.symbol = list(col = c("black","red", "blue"),
                                                      pch = 19
                                                   ),
                              superpose.line = list(col = c("black","red","blue"),
                                                    lwd = 2)),
          ylab="energy submetering"
   )
}

png("Plot_3_energy_submetering.png")
energy_submetering()
dev.off()

#Plot 4:

# Second Graph of Plot 4
voltage_datetime <- function(){
   summary(subs$Voltage)
   xyplot(Voltage ~ datetime , 
          data = subs,
          type = c("l"),
          scales = list(
             x=list(
                at=c(
                   as.POSIXct("2007-02-01 00:00:00 CET", "%Y-%m-%d %H:%M:%S"), 
                   as.POSIXct("2007-02-02 00:00:00 CET", "%Y-%m-%d %H:%M:%S"), 
                   as.POSIXct("2007-02-03 00:00:03 CET", "%Y-%m-%d %H:%M:%S")
                ),
                labels = c("Thu", "Fri", "Sat")
             ),
             y=list(
                at=c(0,200,400),
                limits = c(0, 1100)
             )
          ),
          ylab = "Voltage",
          xlab="datetime"
   )
}

# 4th Graph of Plot 4
global_reactive_power_datetime <- function(){
   summary(subs$Global_reactive_power)
   xyplot(Global_reactive_power ~ datetime , 
          data = subs,
          type = c("l"),
          scales = list(
             x=list(
                at=c(
                   as.POSIXct("2007-02-01 00:00:00 CET", "%Y-%m-%d %H:%M:%S"), 
                   as.POSIXct("2007-02-02 00:00:00 CET", "%Y-%m-%d %H:%M:%S"), 
                   as.POSIXct("2007-02-03 00:00:03 CET", "%Y-%m-%d %H:%M:%S")
                ),
                labels = c("Thu", "Fri", "Sat")
             ),
             y=list(
                at=c(0,10,50, 100,165),
                limits = c(0, 170)
             )
          ),
          ylab = "Global Reactive Power"
   )
}
voltage_datetime()

# Make sure to gridExtra: install.packages("gridExtra")
require(gridExtra)

png("Plot4.png")
# grid.arrange is a function to combine multiple lattice plots
grid.arrange(global_active_power_datetime(), voltage_datetime(), energy_submetering(), global_reactive_power_datetime(), ncol=2)
dev.off()
       
       
