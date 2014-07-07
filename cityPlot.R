# Limit data
limit <- -1
# Drawing options
opacities <- c(0.025, 0.05, 0.1)
cex <- 0.2
root <- "~/r/"
width <- 1920
height <- 1920
background <- "black"
# Activity modes to plot (biking 2, transport 1, running 3, walking 4)
modes = c(1,2,3,4)

#################################################################

library(sp)
library(RgoogleMaps)

# Function to plot one mode
plotCity <- function(city) {
    # Directories to write to
    output <- paste0(root, "output/")
    dir.create(output, showWarnings=FALSE)
    # Grab data
    csvsource <- paste(paste(root, city, sep="input/"), ".csv", sep="")
    print(csvsource)
    data <- read.csv(file=csvsource, header=FALSE, nrows=limit)
    colnames(data) <- c("activity", "date", "speed", "y", "x", "mode")
    # Promote dataframe to spatial
    coordinates(data) = cbind(data$x, data$y)
    # Set projection
    proj4string(data) = CRS("+proj=longlat +datum=WGS84")
    # Calculate bounding box of points
    bb <- qbbox(lat = data$y, lon = data$x, TYPE = "quantile", margin=list(m=c(125, 125, 125, 125), TYPE=c("perc", "abs")[1]))
    # Loop through different  settings
    for (mode in modes) {
        selection = data[data$mode == mode, ]
        print(paste("Mode: ", mode, sep=""))
        for (opacity in opacities) {
            basename <- paste(paste(paste0(output, city), mode, sep="-mode"), sprintf("%04d", opacity*1000), sep="-opacity")
            color <- rgb(1, 1, 1, opacity)
            filename <- paste0(basename, ".png")
            print(paste0("Writing file: ", filename))
            # PNG output
            png(filename, width=width, height=height, units="px", bg=background)
            # Clear plot
            plot.new()
            par(bg=background)
            plot(selection, pch=19, cex=cex, col=color, xlim=bb$lonR, ylim=bb$latR)
            dev.off()
        }
    }
}

# Plot cities in sequence
plotCity("amsterdam")





