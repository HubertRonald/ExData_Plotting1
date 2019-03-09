#############################################
#   Helper functions
#############################################

#---------------------------------------------
#   How always we're going to repeat this lines
#   create a helper function
#---------------------------------------------
render <- function(n, w=480, h=480){
    # Render 2^n: 2,4,8,16,64,128,256,512,..
    # It's a good practice when it's web or mobile display
    # but in this case takes 480px ¯\_(ツ)_/¯
    dev.copy(png,paste0("plot",n,".png"), width=w, height=h) 
    dev.off() 
}

#---------------------------------------------
#   How we're going to specified directory
#   where data or image are going to save
#   create a helper function: manages directories
#---------------------------------------------
manageDir <- function(mainDir, subDir){
    dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
    setwd(paste0(mainDir,"/",subDir))
}



#############################################
#   Main Script
#############################################

#---------------------------------------------
#   Download and unzip
#---------------------------------------------

# variables for file download

URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
fileName <- "household_power_consumption.zip" 
mainDir <- getwd()
subDir <- "UCI EP Dataset"

# specified directory where data is going to save
manageDir(mainDir, subDir)

# verification for file dowload
if(!file.exists(fileName)){download.file(URL,fileName, mode="wb")}

# verification for unzip file
if(!file.exists(subDir)){unzip(fileName)}



#---------------------------------------------
#   Read Data
#---------------------------------------------
setwd(mainDir)
dt <- read.delim("UCI EP Dataset/household_power_consumption.txt", header = T,  sep=";", na.strings = "?")



#---------------------------------------------
#   Clean data
#---------------------------------------------

# change class data
dt$Date <- as.Date(dt$Date, "%d/%m/%Y")

# dt will only be using data from the dates 2007-02-01 and 2007-02-02
dt <- subset(dt,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

## Combine Date and Time column and format
dateTime <- as.POSIXct(paste(dt$Date, dt$Time))

## Name the vector
dateTime <- setNames(dateTime, "DateTime")

## Remove Date and Time column from dt
dt <- dt[ ,!(names(dt) %in% c("Date","Time"))]

## Add DateTime column
dt <- cbind(dateTime, dt)



#---------------------------------------------
# in folder figure'll save image
#---------------------------------------------
manageDir(mainDir, "figure")


#---------------------------------------------
#   Reset par to the default values at startup
#   more details here:
#   https://stackoverflow.com/questions/5789982/reset-par-to-the-default-values-at-startup
#---------------------------------------------
dev.off() 



#---------------------------------------------
#   PLOT 1
#---------------------------------------------
hist(dt$Global_active_power,
     main="Global Active Power",
     xlab = "Global Active Power (kilowatss)",
     col="red")
render(1)


#---------------------------------------------
#   PLOT 2
#---------------------------------------------
plot(dt$Global_active_power~dateTime,
     type="l",
     ylab = "Global Active Power (kilowatss)",
     xlab ="")
render(2)


#---------------------------------------------
#   PLOT 3
#---------------------------------------------
with(dt, {
    plot(Sub_metering_1~dateTime, type="l",
         ylab="Global Active Power (kilowatts)", xlab="")
    lines(Sub_metering_2~dateTime,col='Red')
    lines(Sub_metering_3~dateTime,col='Blue')
    legend("topright", col = c("Black","Red","Blue"), lwd=c(1,1,1),
           c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    })
render(3)


#---------------------------------------------
#   PLOT 4
#---------------------------------------------
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(dt, {
    # plot top left
    plot(Global_active_power~dateTime, type="l", 
         ylab="Global Active Power", xlab="")
    
    # plot top right
    plot(Voltage~dateTime, type="l", 
         ylab="Voltage", xlab="")
    
    # plot bottom left
    plot(Sub_metering_1~dateTime, type="l", 
         ylab="Global Active Power", xlab="")
    
    lines(Sub_metering_2~dateTime,col='Red')
    lines(Sub_metering_3~dateTime,col='Blue')
    
    #cex = 0.75 # font size legend
    legend("topright", col=c("black", "red", "blue"), cex=0.75,  lty=1, lwd=2, bty="n",
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    # plot bottom right
    plot(Global_reactive_power~dateTime, type="l", 
         ylab="Global Rective Power",xlab="")
})
render(4)


#---------------------------------------------
#   main directory work
#---------------------------------------------
setwd(mainDir)
