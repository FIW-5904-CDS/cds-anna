# Traditional Plotting
setwd("~/VTech/Teaching/ConservationDataScience/GitHub/cds-ferretti/s")
# plot function 
# let's include data

data(mtcars) # dataset that contains fuel consumption and performance characteristics for 32 automobile models from the 1973–74 Motor Trend US magazine, including variables such as miles per gallon (mpg), number of cylinders (cyl), horsepower (hp), weight (wt), and transmission type (am)
data(iris) # dataset containing measurements of 150 iris flowers from three species (setosa, versicolor, and virginica), with four variables describing their morphology: sepal length, sepal width, petal length, and petal width

require(sharkPulseR)

#------------------------------------------
# plot() function

plot(Sepal.Length~Sepal.Width, data = iris)

?plot()


# plotting is done by triggerina device. Output is directed to a particular output device and that dictates the output format that will be produced

# plot one or two variables

y <- rnorm(20)

plot(y, type="p")
plot(y, type="l")
plot(y, type="b")
plot(y, type="h")

# make a scatter plot

plot(pressure) # supply just the dataset

plot(pressure$temperature, pressure$pressure) # generally accept x and y

plot(pressure ~ temperature, data=pressure) # formula format and supplying the data explicitly

# the plot function can adapt to the specific type of the data supplied. If we supply a categorical variable then plot produces a boxplot

plot(Sepal.Length~Species, data = iris)

# if we supply a model object, it generates diagmostic plots

lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)

plot(lm.SR) # diagnostic plots fopr linear regression

stripchart(pressure$pressure, vertical = T) # for plotting a single variable

#------------------------------------------------------
# controlling plot appearance


plot(mpg~qsec, mtcars) # qsec is number of seconds required for the car to travel a quarter mile (0.25 miles) from a standing start - lower qsec values indicate faster acceleration

# symbols

plot(mpg~qsec, mtcars, pch = 16)
plot(mpg~qsec, mtcars, pch = 3)

plot(1:20, 1:20, pch = 1:20)


## colors

plot(mpg~qsec, mtcars, pch = 16, col = "red")

# colors can be set with textual strings
colors()

# the rgb() function allows a colors to be specified in Red-Green-Blue (RGB) triplet of intensities; rgb(1, 0, 0) - as much red as possible, no blue, and no green

# col2rgb() can be used to see the RGB values for a particular color name

col2rgb("green")


# string of the form "#RRGGBB", where each of the pairs RR, GG, BB consist of two hexadecimal digits giving a value in the range zero (00) to 255 (FF). color red is given as "#FF0000"

plot(mpg~qsec, mtcars, pch = 16, col = "#FF0000")

# useful site https://colorbrewer2.org

# transparency can be included 

plot(mpg~qsec, mtcars, pch = 16, col = rgb(1, 0, 0, 0.5))
plot(mpg~qsec, mtcars, pch = 16, col = "#FF000050")

mod1 = lm(mpg~qsec, mtcars)
abline(mod1, col = "red")


plot(mpg~qsec, mtcars, pch = 16, axes = FALSE, col = "green")
axis(1, fg = "red")
axis(2, fg = "blue")
mtext("miles per gallon vs. quarter mile seconds", side = 3)



#--------------------------------------------------
# barplot()

# VADeaths is a dataset that provides age-specific death rates (per 1,000 population) in Virginia (USA) in 1940, classified by age group and population group. it gives mortality rates for five age categories (50–54, 55–59, 60–64, 65–69, 70–74) across four demographic groups: Rural Male (RM), Rural Female (RF), Urban Male (UM) and Urban Female (UF)

barplot(VADeaths[1:2,], angle = c(45, 135), density = 20, col = "grey", names=c("RM", "RF", "UM", "UF")) # plotting age group 50-54 and 55-59 

barplot(VADeaths[1:2,], angle = c(45, 135), density = 20, col = "grey", names=c("RM", "RF", "UM", "UF"), horiz=TRUE) # plotting horizontal bars


#---------------------------------------------------
# boxplot()

# The OrchardSprays dataset records the results of an agricultural experiment measuring the effectiveness of different chemical sprays on controlling pests in an orchard. It contains 64 observations with three variables:

# decrease – the reduction in pest counts (a measure of spray effectiveness),
# treatment – a factor indicating which of eight sprays (A–H) was used,
# rowpos and colpos – the row and column positions in the orchard where each spray was applied.

boxplot(decrease ~ treatment, data = OrchardSprays, log = "y", col="light grey")

boxplot(decrease ~ treatment, data = OrchardSprays, log = "y", col="light grey", boxwex=0.5) # boxwex controls width of boxes


#----------------------------------------------------
# curve() # is useful for plotting a matematical functions, usefuld to explore funtions

curve(sin, from = -2*pi, to = 2*pi, xname = "t")

curve(x^3 - 3*x, -2, 2) # you can omit "from" and "to" if they are included in order

curve(x^2 - 2, add = TRUE, col = "violet")
mtext("x^2 - 2", side = 3, col = "violet")



#------------------------------------------------------
# adding lines, points and other elements in plots

# let's simulate a linear regression

x = rnorm(100, mean = 5, sd = 3)
y = 2+ 2.3*x + rnorm(100, mean = 0, sd = 2) 
plot(y~x, pch = 16)

newdat = data.frame(x = x)


mod1 = lm(y~x)

# Make a grid (optional but nicer)
newdat <- data.frame(x = seq(min(x), max(x), length.out = 200))

# Mean fit + SEs
pr <- predict(mod1, newdata = newdat, se.fit = TRUE)

newdat$fit    <- pr$fit
newdat$upper  <- pr$fit + 1.96*pr$se.fit
newdat$lower  <- pr$fit - 1.96*pr$se.fit

# Ensure sorted by x (it already is since we used seq)
with(newdat, {
  lines(x, fit)
  lines(x, upper, col = "forestgreen")
  lines(x, lower, col = "forestgreen")
})

# Including a legend
# Generating some data data
x <- 1:10
y1 <- 2*x + rnorm(10)
y2 <- 2*x + 5 + rnorm(10)

# Create the plot for the first dataset
plot(x, y1, type = "b", pch = 16, col = "blue",
     xlab = "X values", ylab = "Y values",
     main = "Example of legend() in Base R")

# Add the second line
lines(x, y2, type = "b", pch = 17, col = "red")

# Add a legend
legend("topleft",                       # position
       legend = c("Series 1", "Series 2"),  # labels
       col = c("blue", "red"),              # colors
       pch = c(16, 17),                     # point symbols
       lty = 1,                             # line type
       title = "Legend",                    # legend title
       bty = "n")                           # no box border




#--------------------------------------------------------------
# plot has a variety of standard arguments which can be accessed with ?plot or also with ?par for a longer list
# par() is the function to set a long list of parameters for the plotting device

y <- rnorm(20)

plot(y, type="l", lwd=3)
plot(y, type="l", col="grey")
plot(y, type="l", lty="dashed")
plot(y, type="l", ylim=c(-4, 4))

?par() # to display the entire list of parameters

# using par to modulate plot settings

# get sharkPulse data
dat <- getSharkPulse(dbuser="spr",dbpass="spr_pass", external = TRUE)

freqs = with(dat, aggregate.data.frame(list(Count = species_name), list(Species = species_name), length))
freqs = freqs[rev(order(freqs$Count)),]
freqs$prop = with(freqs, Count/sum(Count))

par(mar = c(5,12,4,2)) # increases the space on the 
bplot = with(freqs[1:20,],barplot(height = rev(Count), axes = FALSE, horiz = TRUE))
axis(1)
axis(2, at = bplot, labels = rev(freqs[1:20,]$Species), las = 1, font = 3)


# a more complex boxplot

load("../data/measured.RData") # object "measured" 
total = read.csv("../data/estimatedLengthRecords.csv")



dev.off()

par(oma = c(2,4,2,5))
plot(c(0,300,300,550)~c(0,1,2,3),type = 'n',axes = FALSE,xlab = "",ylab = "")


segments(0.2,135,1,135,lty=3) # earliest size at 1 year p.pectinata
segments(0.2,253,1,253,lty=3) # earliest age at maturity
axis(2,pos = 0.2,at = c(0,50,100,135,200,253,300,350,400,450,500,550),labels = c("0","50","100","135 (early juvenile)","200","253 (size at maturity)","300","350","400","450","500","Total length (cm) 550"),las=1)

text(1,560,"Smalltooth Sawfish", cex = 0.7)

pristis = list(pectinata = total$length[total$species.x=="Pristis pectinata"],pristis = total$length[total$species.x=="Pristis pristis"])

boxplot(pristis,add=T,axes=FALSE,outpch = NA)

stripchart(length ~ species.x, data = total, vertical = TRUE, method = "jitter", pch = 21, col = "maroon", bg = "bisque", add = TRUE) 

segments(2.8,105,2,105,lty=3) # earliest size at 1 year p.pectinata
segments(2.8,300,2,300,lty=3) # earliest age at maturity
axis(4,pos=2.8,,at = c(0,50,105,150,200,250,300,350,400,450,500,550),labels = c("0","50","105 (early juvenile)","150","200","250","300 (size at maturity)","350","400","450","500","550 Total length (cm)"),las=1)
text(2,560,"Largetooth Sawfish",cex = 0.7)
# 105 earliest age 1 year
# 300 agemat



#------------------------------------------------------
# plots of multiple variables

plot(iris) # useful to check correlations between variables
# similar results with pairs() - you need to supply a matrix of continuous variables

#--------------------------------------------------------
# interactive plots


#------------------------------------------------------
# multipanel plots
# par()

# 2 by 2 multipanel plot
par(mfrow = c(2,2))

plot(pressure~temperature, pressure)
plot(mpg~cyl, mtcars)
boxplot(mpg~cyl, mtcars)
plot(mpg~qsec, mtcars)
# including lines
mod1 = lm(mpg~qsec, mtcars)
abline(mod1)


dev.off()
par(mfrow = c(3, 2))
for (i in 1:6) {
  plot.new()
  text(0.5, 0.5, paste("Plot", i), cex = 2)
  box()
}

## using layout

# this is equivalent to the above

layout(matrix(c(1, 2, 3, 4, 5, 6), byrow=TRUE, ncol=2))
layout.show(6)
layout(rbind(c(1, 2),c(3, 4),c(5, 6)))

# layout is useful because you can make more complex plot arrangments

layout(matrix(c(1, 1, 2, 3, 4, 5), byrow=TRUE, ncol=2))
layout.show(5)



#------------------------------------------------------
# saving plots to files

pdf("../maps/parMultipanel.pdf")
par(mfrow = c(3, 2))
for (i in 1:6) {
  plot.new()
  text(0.5, 0.5, paste("Plot", i), cex = 2)
  box()
}
dev.off()

pdf("../maps/layoutMultipanel.pdf")
layout(matrix(c(1, 1, 2, 3, 4, 5), byrow=TRUE, ncol=2))
layout.show(5)
dev.off()



pdf("../maps/demographics.pdf") # open the pdf file, specifying te file names and path

par(oma = c(2,4,2,5))
plot(c(0,300,300,550)~c(0,1,2,3),type = 'n',axes = FALSE,xlab = "",ylab = "")


segments(0.2,135,1,135,lty=3) # earliest size at 1 year p.pectinata
segments(0.2,253,1,253,lty=3) # earliest age at maturity
axis(2,pos = 0.2,at = c(0,50,100,135,200,253,300,350,400,450,500,550),labels = c("0","50","100","135 (early juvenile)","200","253 (size at maturity)","300","350","400","450","500","Total length (cm) 550"),las=1)

text(1,560,"Smalltooth Sawfish", cex = 0.7)

pristis = list(pectinata = total$length[total$species.x=="Pristis pectinata"],pristis = total$length[total$species.x=="Pristis pristis"])

boxplot(pristis,add=T,axes=FALSE,outpch = NA)

stripchart(length ~ species.x, data = total, vertical = TRUE, method = "jitter", pch = 21, col = "maroon", bg = "bisque", add = TRUE) 

segments(2.8,105,2,105,lty=3) # earliest size at 1 year p.pectinata
segments(2.8,300,2,300,lty=3) # earliest age at maturity
axis(4,pos=2.8,,at = c(0,50,105,150,200,250,300,350,400,450,500,550),labels = c("0","50","105 (early juvenile)","150","200","250","300 (size at maturity)","350","400","450","500","550 Total length (cm)"),las=1)
text(2,560,"Largetooth Sawfish",cex = 0.7)
# 105 earliest age 1 year
# 300 agemat
dev.off() # closes the file - only after this command it can be opened



#------------------------------------------------------
# mapping with the maps package

require(mapdata) 
pdf("../maps/allRecords.pdf", width = 12, height = 7)
map("worldHires", fill = T)
points(latitude~longitude, data = dat, pch=16, col = "green")
dev.off()


# Exercise

# Please produce a pdf figure with a 4 panel plot where in panel 1 you have a location map (your choice), in panel 2 a scatter plot, in panel 3 a barplot and panel 4 a box plot. Save this fugure in your GitHub folder (exerciseBaseGraph.pdf) in your figure folder and the code in your R folder (exerciseBaseGraph.R). 