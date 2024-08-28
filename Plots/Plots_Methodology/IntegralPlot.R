###########Libraries

library(deSolve) 
library(readxl) 
library(viridis)
library(dplyr)
library(openxlsx)
library(stringr)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)


### Plot for explaining the calculation of the integral of effective contact reduction


# Set working directory
#setwd("<dir/name.")

#save file

png("interventions_plot.png", width = 10, height = 6, units = "in", res = 300)

# time vector from 0 to 365 days
days <- 0:365

#shielding activation
shielding <- ifelse(days >= 50 & days <= 220, 0.3, 0)

#lockdown activation
lockdown <- ifelse((days >= 80 & days <= 170) | (days >= 300 & days <= 365), 0.7, 0)

# Plot
plot(days, shielding, type="l", col= "#fc8660", ylim=c(0, 1), 
     xlab="Time (days)", ylab="Effective Contact Reduction in Total Population [%] ", 
     lwd=2, xlim=c(0, 365))

lines(days, lockdown, col= "#85b5d7", lwd=2)

# Limit the shading under the shielding curve to end at day 220
polygon(c(days[days <= 220], rev(days[days <= 220])), 
        c(shielding[days <= 220], rep(0, sum(days <= 220))), 
        col= "#fc8660", density=20, angle=45)

# Fill the area under the lockdown curve with diagonal lines
polygon(c(days, rev(days)), c(lockdown, rep(0, length(days))), col= "#85b5d7", density=20, angle=135)


# mark start and end dates

arrows(50, 0.4, 50, 0.1, length=0.1, col= "#fc8660", lwd=2)
text(50, 0.45, "Start", col= "#fc8660", pos=3)

arrows(220, 0.4, 220, 0.1, length=0.1, col= "#fc8660", lwd=2)
text(220, 0.45, "End", col= "#fc8660", pos=3)

arrows(80, 0.8, 80, 0.5, length=0.1, col= "#85b5d7", lwd=2)
text(80, 0.85, "Start 1", col= "#85b5d7", pos=3)

arrows(170, 0.8, 170, 0.5, length=0.1, col= "#85b5d7", lwd=2)
text(170, 0.85, "End 1", col= "#85b5d7", pos=3)

arrows(300, 0.8, 300, 0.5, length=0.1, col= "#85b5d7", lwd=2)
text(300, 0.85, "Start 2", col= "#85b5d7", pos=3)

# show how lockdown is calculated if longer than 365 days
arrows(365, 0.8, 365, 0.5, length=0.1, col= "#85b5d7", lwd=2)
text(365, 0.85, "End 2", col= "#85b5d7", pos=3)

legend("topleft", legend=c("Shielding", "Lockdown"), col=c( "#fc8660",  "#85b5d7"), lwd=2)

dev.off()
