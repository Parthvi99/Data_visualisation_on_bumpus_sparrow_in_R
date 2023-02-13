library(readr)
sparrows <- read_csv("~/Desktop/Parthvi/MITA/SEM2/MVA/Bumpus_sparrows.csv")
str(sparrows)

boxplot(sparrows[,2:6])
stars(sparrows,labels = sparrows$Survivorship)

# Dive Deeper into Visualization Techniques
# using the economist theme in plots 
attach(sparrows)

#Plots
sparr.birds <- data.frame(as.numeric(rownames(sparrows)),sparrows[,2:6])

labs.diagonal <- c("Bird","Total length","Alar extent","L. beak & head","L. humerous","L. keel & sternum")

plot(Total_length, Alar_extent,xlab="Total Length (mm)",ylab="Alar extent (nm)",pch=c(16,1))

# Correlations
pairs(sparrows[,2:6])


# Scatter Plot Matrix
#Scatterplot matrix is a collection of scatterplots being organized into a matrix, 
#and each scatterplot shows the relationship between a pair of variables.

#library(car)
scatterplotMatrix(~Total_length+Alar_extent+L_beak_head+L_humerous+L_keel_sternum | Survivorship, data=sparr.birds, var.labels=labs.diagonal,cex.labels=0.7, diagonal="boxplot",smooth=FALSE,reg.line=FALSE,pch=c(1,16),col=rep("black",2), legend.plot=FALSE)
# scatterplot matrix giving a relationship between each variable using survivor and non-survivor groups 



library(lattice)
super.sym <- trellis.par.get("superpose.symbol")
super.sym$superpose.symbol$pch <- c(1,16,rep(1,5))
super.sym$superpose.symbol$col <- rep("#000000",7)
trellis.par.set(super.sym)
splom(~sparr.birds, groups = Survivorship, data = sparr.birds, ps=0.5, varname.cex = .5,panel = panel.superpose,key = list(columns = 2,points = list(pch = super.sym$pch[1:2], col=super.sym$col[1:2]),text = list(c("Non-survivor", "Survivor"))))

library(GGally)
ggscatmat(sparrows, columns=2:6, color="Survivorship")
detach(sparrows)

# load packages
library(lattice)
library(ggplot2)
library(ggridges)
library(ggvis)
library(ggthemes)
library(cowplot)
library(gapminder)
library(gganimate)
library(dplyr)
library(tidyverse)
library(grid)
library(gridExtra)
library(RColorBrewer)

# Using Diamonds Data
data(sparrows)
attach(sparrows)

ggpairs(sparrows)


# base R
plot(Total_length~Alar_extent)

plot(Total_length~Alar_extent)
abline(lm(Total_length~Alar_extent), col="red")

xyplot(Total_length~Alar_extent)
xyplot(Total_length~Alar_extent | Survivorship)
xyplot(Total_length~Alar_extent, groups=Survivorship)

xyplot(Total_length~Alar_extent | Survivorship + L_beak_head)
xyplot(Total_length~Alar_extent  | Survivorship , groups=Survivorship, auto.key=list(space="right"))



# ggplot
ggplot(sparrows, aes(x=Total_length,y=Alar_extent)) + geom_point() + theme_economist()

ggplot(sparrows, aes(x=Total_length,y=Alar_extent)) + facet_wrap(~Survivorship) + geom_point()+ theme_economist()

ggplot(sparrows, aes(x=Total_length, y=Alar_extent)) + geom_point(aes(color=Survivorship))+ theme_economist()

ggplot(sparrows, aes(x=Total_length,y=Alar_extent)) + xlim(0,3) + geom_point(colour="steelblue", pch=3) + 
  labs(x="total length of sparrows", y="tip to tip extended length", title="Sparrows Survival Data") 

# bar chart
ggplot(sparrows, aes(Survivorship)) + geom_bar(position="stack") + theme_economist()
ggplot(sparrows, aes(Survivorship)) + facet_grid(.~Total_length) + geom_bar(position="dodge") + theme_economist()

# histogram
ggplot(sparrows, aes(L_beak_head))+geom_histogram() + theme_economist()
ggplot(sparrows, aes(L_beak_head))+geom_histogram(aes(fill = after_stat(count))) + theme_economist()

# regression
ggplot(sparrows, aes(x=Total_length, y=Alar_extent)) + geom_point() + geom_smooth(method=lm) + theme_economist()
ggplot(sparrows, aes(x=Total_length, y=Alar_extent)) + geom_point() + stat_smooth() + theme_economist()

# violin plot 
#A violin plot is a hybrid of a box plot and a kernel density plot, 
#which shows peaks in the data. It is used to visualize the distribution of 
#numerical data. Unlike a box plot that can only show summary statistics, 
#violin plots depict summary statistics and the density of each variable.

ggplot(sparrows, aes(x=Total_length, y=Alar_extent)) + geom_violin() + theme_economist()
ggplot(sparrows, aes(x=Total_length, y=L_humerous)) + geom_violin() + theme_economist()
ggplot(sparrows, aes(x=Total_length, y=L_keel_sternum)) + geom_violin() + theme_economist()

# box plot
ggplot(sparrows, aes(x=Total_length, y=Alar_extent)) + geom_boxplot() + theme_economist()
ggplot(sparrows, aes(x=Total_length, y=Alar_extent)) + geom_boxplot() + coord_flip() + theme_economist()

# density plot and ggridges
#A density plot can be seen as an extension of the histogram. As opposed 
#to the histogram, the density plot can smooth out the distribution of 
#values and reduce the noise. It visualizes the distribution of data over 
#a given period, and the peaks show where values are concentrated.

ggplot(sparrows, aes(x=Total_length)) + geom_density() + theme_economist()
ggplot(sparrows, aes(x=Total_length, fill=Alar_extent, color=Survivorship)) + geom_density() + theme_economist()
ggplot(sparrows, aes(x=Total_length, fill=Alar_extent, color=Survivorship)) + geom_density(alpha=0.3, aes(y=..scaled..)) + theme_economist()

ggplot(sparrows, aes(x=Total_length, y=Survivorship)) + geom_density_ridges() + theme_economist()
ggplot(sparrows, aes(x=Total_length)) + geom_density() + theme_economist()

# hexbin
#A hexbin plot is useful to represent the relationship of 2 numerical variables 
#when you have a lot of data points. Without overlapping of the points, 
#the plotting window is split into several hexbins. The color of each hexbin
#denotes the number of points in it. Like in our case the Maximum count is of 5 
ggplot(sparrows, aes(x=Total_length, y=Survivorship)) + geom_hex() + theme_economist()

lastplot <- ggplot(sparrows, aes(x=Total_length, y=Alar_extent)) + xlim(0,3) + geom_point(aes(color=Survivorship)) + stat_smooth() + 
  labs(x="total length of sparrows", y="tip to tip extended length", title="Sparrows Survival Data") 

lastplot + theme_bw()
lastplot + theme_cowplot()
lastplot + theme_dark()
lastplot + theme_economist()
lastplot + theme_fivethirtyeight()
lastplot + theme_tufte()
lastplot + theme_wsj()



