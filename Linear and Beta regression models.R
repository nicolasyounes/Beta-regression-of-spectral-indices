# 1) The aim of this code is to create linear and beta models that predict Fractional Vegetation Coverage (FVC) 
# of mangroves, # from NDVI and SAVI values. In other words, for a given NDVI or SAVI, what is the expected range of 
# mangrove coverage fractions in a given pixel.
# To do this, I created a pilot experimet where i layed mangrove leaves over a water background and 
# used a hyperspectral scanner to create an image file of the mangroves. Over that image file, y superimposed 
# four (4) grids of different sizes (5x5, 10x10, 15x15 and 20x20) to determine if the grid size alters
# the spectral index values.
# 2) This code will fit a linear regression and Beta regression for the indices.
# 3) Indices.csv has the information related to NDVI and SAVI in all grid sizes.

######## Part A: READING AND PREPARING THE DATA ########
# read CSV file with ALL data
indices.dat= read.csv(file.choose()) #navigate to: "Indices.csv"

# Alternatively you can use if the file is in your working directory
indices.dat = read.csv(file = "Indices.csv", header = TRUE, dec = ".")

# Gettin the libraries to be used
library(ggplot2) ## enhaced graphics
library(betareg) ## Beta Regression package
library(GGally)  ## superb 'pair-wise' plot

######## Part B: ACCESSING THE DATA ########
# FVC, SAVI and NDVI values have already been calculated from the reflectance values in each pixel
# GRID represents the 
FVC   = indices.dat$FVC
SAVI  = indices.dat$SAVI
NDVI  = indices.dat$NDVI
GRID  = indices.dat$GRID

# Some preliminary data exploration
ggpairs(indices.dat, columns = 1:3)


######## Part D: LINEAR REGRESSION ######## 
# NDVI:
ndvi.20x = lm(FVC ~ NDVI, subset(indices.dat, GRID=='20', weights=GRID^2))
summary(ndvi.20x)

# SAVI:
savi.20x = lm(FVC ~ SAVI, subset(indices.dat, GRID=='20', weights=GRID^2))
summary(savi.20x)


#function to extract RMSE 
rmse <- function(error)
{
    #This function calculates the Root Mean Square Error
    #source: https://heuristically.wordpress.com/2013/07/12/calculate-rmse-and-mae-in-r-and-sas/
    sqrt(mean(error^2))
}

rmse(savi.20x$residuals)
rmse(ndvi.20x$residuals)


######## Part E: BETA REGRESSION ######## 
# Transforms FVC into 0-1 
FVC_B = indices.dat$FVC/100

#ordering the data for Beta graphs
ndvi.sorted = indices.dat[order(indices.dat$NDVI),] 
savi.sorted = indices.dat[order(indices.dat$SAVI),]

# If FVC values == 0 or ==1 add or substract 0.0001 because beta models cannot handle
# values ==0 or ==1
for(i in 1:length(FVC_B)){          
    if(FVC_B[i]==0){FVC_B[i]=FVC_B[i]+0.0001}
    if(FVC_B[i]==1){FVC_B[i]=FVC_B[i]-0.0001}
}

# Beta Regression for  NDVI:
beta.ndvi20x = betareg(FVC_B ~ NDVI, data=indices.dat, link="logit", weights=GRID^2)
summary(beta.ndvi20x)

# Beta Regression for  SAVI:
beta.savi20x = betareg(FVC_B ~ SAVI, data=indices.dat, link="logit", weights=GRID^2)
summary(beta.savi20x)

rmse(beta.savi20x$residuals)
rmse(beta.ndvi20x$residuals)

######## Part G: GRAPHS ########
# Linear models ~ 20x20
par(mfrow=c(1,2))
plot(FVC ~ NDVI, data=subset(indices.dat), xlab="NDVI - lm" , 
     main='E',ylab="FVC (%)", las=1, col="gray")
legend('topleft', legend=c('Observed NDVI', 'Linear Model'),  col=c('gray','blue'),lwd = 3,lty = c(NA, 1), pch=c(19,NA), bty='n')
points(x=subset(indices.dat, GRID=='20')$NDVI, y=ndvi.20x$fitted, lwd = 3,  type = 'l')

plot(FVC ~ SAVI, data=subset(indices.dat, GRID=='20'), main='SAVI - lm', xlab="SAVI" , ylab="FVC (%)", las=1, col="gray")
legend('bottomright', legend=c('Observed SAVI', 'Linear Model'),  col=c('gray','blue'),lwd = 3,lty = c(NA, 1), pch=c(19,NA), bty='n')
points(x=subset(indices.dat, GRID=='20')$SAVI, y=savi.20x$fitted, lwd = 3, type = 'l')

# Beta models ~ 20x20 
par(mfrow=c(1,2))
plot(FVC_B ~ NDVI, data=subset(indices.dat, GRID=='20'), xlab="NDVI", 
     main="NDVI - Beta",ylab="FVC ", las=1, col="gray")
points(x=NDVI, y=beta.ndvi20x$fitted, lwd=1, pch=16)

plot(FVC_B ~ SAVI, data=subset(indices.dat, GRID==20), xlab="SAVI", 
     main="SAVI - beta", ylab="FVC ", las=1, col="gray")
points(x=SAVI, y=beta.savi20x$fitted, lwd=1, pch=16)

# Using ggplot2:

ggndvi  = ggplot(data = indices.dat, aes(NDVI, FVC, color = factor(GRID)))+ geom_point()
ggndvi  = ggndvi+ggtitle('NDVI')+theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0)))
ggndvi

ggsavi  = ggplot(data = indices.dat, aes(SAVI, FVC, color = factor(GRID)))+ geom_point()
ggsavi  = ggsavi+ggtitle('SAVI')+theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0)))
ggsavi

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ end of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##