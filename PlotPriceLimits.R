rm(list=ls())
library(quantmod)



MovingFun <- function(x, func, k){
	temp <- NULL
	for(i in 1:k){	temp <- cbind(temp, lag(x,i)) }
	return(as.double(apply(temp, 1, func)))
}


PlotPriceLimits <- function(prices, k=30, f=1/10, sd=3){
	upper <- rep(NA,nrow(prices))
	lower <- rep(NA,nrow(prices))
	if(nrow(prices) > k) upper <- MovingFun(prices, function(x) median(x) + sd * mad(x), k)
	if(nrow(prices) > k) lower <- MovingFun(prices, function(x) median(x) - sd * mad(x), k)
	start <- which(is.na(upper))
	upper[start] <- median(prices[start],na.rm=T) + sd * mad(prices[start],na.rm=T)
	lower[start] <- median(prices[start],na.rm=T) - sd * mad(prices[start],na.rm=T)
	
	upper <- lowess(upper, f=f)$y
	lower <- lowess(lower, f=f)$y
	
	
	plot(as.double(prices), type='l',ylim=c(min(lower),max(upper)), bty='l')
	lines(upper, col="blue")
	lines(lower, col="blue")
}


data <- getSymbols("^GDAXI",auto.assign=F)
data <- data[,4]
names(data) <- "Close"

PlotPriceLimits(data, 30, 1/10, 3)


