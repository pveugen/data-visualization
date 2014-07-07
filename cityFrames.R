# Limit data to speed up
limit <- -1
# Drawing options
opacity <- 0.10
color <- rgb(1, 1, 1, opacity)
cex <- 0.8
# Number of frames
frames = 60 * 24 / 2
# How long should a point stay visible
decay <- 60
# What hour of day should plot start
starthour <- 4
# Data details
root <- "~/r/"
# Activity modes to plot (biking 2, transport 1, running 3, walking 4)
modes = c(1,2,3,4)

#################################################################
library(sp)
library(rgdal)
library(RgoogleMaps)


# Function to plot one mode
plotCity <- function(city, timezone) {
    # Calculate time frames to use for animation frames
    calculate_frame <- function(x) {
        d <- as.POSIXlt(x + (timezone*60*60) - (starthour * 60 * 60), origin="1970-01-01")
        # Calculate minutes since start of day
        frame <- ((d$h * 60) + (d$min)) / (60*24 / frames)
        frame
    }
    # Directories to write to
    output <- paste(paste(root, city, sep="output/"), "/", sep="")
    dir.create(output, showWarnings=FALSE)

    # Grab data
    csvsource <- paste(paste(root, city, sep=""), ".csv", sep="")
    print(csvsource)
    print(paste0("Timezone: ", timezone))
    data <- read.csv(file=csvsource, header=FALSE, nrows=limit)
    colnames(data) <- c("activity", "date", "speed", "y", "x", "mode")

    # Extend data with a value 'frame' (one frame = one image)
    data["frame"] <- NA
    data$frame <- calculate_frame(data$date)

    # Sort data by frame
    data <- data[order(data$frame),]

    print(summary(data))
    # Promote dataframe to spatial
    coordinates(data) = cbind(data$x, data$y)
    # Set projection
    proj4string(data) = CRS("+proj=longlat +datum=WGS84")
    # Calculate bounding box of points
    bb <- qbbox(lat = data$y, lon = data$x, TYPE = "quantile", margin=list(m=c(125, 125, 125, 125), TYPE=c("perc", "abs")[1]))

    for (mode in modes) {
        selection = data[data$mode == mode, ]
        basename <- paste(paste(output, city, sep=""), mode, sep="-mode")
        print(paste("Mode: ", mode, sep=""))

        # Generate an image for every frame
        min_window <- 0
        max_window <- frames + decay # fade out

        for (i in min_window:max_window) {
            segment = subset(selection, frame < i & frame > i-decay)
            print(paste(paste0("| start: ", i-decay), paste0("end: ", i), sep=" - "))
            points_in_segment <- nrow(segment)
            print(paste(paste("Frame ", i, sep=""), points_in_segment, sep=" has points: "))
            filename <- paste0(paste(basename, sprintf("%04d", i), sep="_"), ".png")
            print(paste0("Writing file: ", filename))
            # Generate images
            png(filename, width=1920, height=1920, units="px", bg="black")
            # Plot
            plot.new()
            par(bg="black")
            plot(segment, pch=19, cex=cex, col=color, xlim=bb$lonR, ylim=bb$latR)
            dev.off()
        }
    }
}

plotCity("amsterdam", 1)


