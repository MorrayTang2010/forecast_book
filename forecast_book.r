library(fpp)

## Section 2.1 

plot(melsyd[,"Economy.Class"],
     main="Economy class passengers: Melbourne-Sydney",
     xlab="Year",ylab="Thousands")

plot(a10, ylab="$ million", xlab="Year", main="Antidiabetic drug sales")

seasonplot(a10,ylab="$ million", xlab="Year",
           main="Seasonal plot: antidiabetic drug sales",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)

monthplot(a10,ylab="$ million",xlab="Month",xaxt="n",
          main="Seasonal deviation plot: antidiabetic drug sales")
axis(1,at=1:12,labels=month.abb,cex=0.8)

plot(jitter(fuel[,5]), jitter(fuel[,8]), xlab="City mpg", ylab="Carbon footprint")

pairs(fuel[,-c(1:2,4,7)], pch=19)

#chapter 5
## put histograms on the diagonal
panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE, breaks = "FD")
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
pairs(credit[, -(4:5)], diag.panel = panel.hist)

creditlog <- data.frame(score=credit$score, 
                        log.savings=log(credit$savings+1), 
                        log.income=log(credit$income+1), 
                        log.address=log(credit$time.address+1),
                        log.employed=log(credit$time.employed+1), 
                        fte=credit$fte, single=credit$single)
pairs(creditlog[,1:5],diag.panel=panel.hist)

#5.6
Cityp <- pmax(fuel$City-25,0)
fit2 <- lm(Carbon ~ City + Cityp, data=fuel)
x <- 15:50; z <- pmax(x-25,0)
fcast2 <- forecast(fit2, newdata=data.frame(City=x,Cityp=z))
plot(jitter(Carbon) ~ jitter(City), data=fuel)
lines(x, fcast2$mean,col="red")

#Chapter 6时间序列分解例子
#6.1
fit <- stl(elecequip, s.window=5)
plot(elecequip, col="gray",
     main="Electrical equipment manufacturing",
     ylab="New orders index", xlab="")
lines(fit$time.series[,2],col="red",ylab="Trend")
plot(fit)
#6.5
fit5 <- stl(elecequip, t.window=15, s.window="periodic", robust=TRUE)
plot(fit5)
#6.6
fit <- stl(elecequip, t.window=15, s.window="periodic", robust=TRUE)
eeadj <- seasadj(fit)
plot(naive(eeadj), xlab="New orders index",
     main="Naive forecasts of seasonally adjusted data")
fcast <- forecast(fit, method="naive")
plot(fcast, ylab="New orders index")

#Chapter 7
#7.1
oildata <- window(oil,start=1996,end=2007)
plot(oildata, ylab="Oil (millions of tonnes)",xlab="Year")

fit1 <- ses(oildata, alpha=0.2, initial="simple", h=3)
fit2 <- ses(oildata, alpha=0.6, initial="simple", h=3)
fit3 <- ses(oildata, h=3)
plot(fit1, plot.conf=FALSE, ylab="Oil (millions of tonnes)",
     xlab="Year", main="", fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"), 
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.89)),pch=1)
#7.2
air <- window(ausair,start=1990,end=2004)
plot(air)
fit1 <- holt(air, alpha=0.8, beta=0.2, initial="simple", h=5) 
fit2 <- holt(air, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=5) 
# Results for first model:
fit1$model$state
fitted(fit1)
fit1$mean


fit3 <- holt(air, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=5) 
plot(fit2, type="o", ylab="Air passengers in Australia (millions)", xlab="Year", 
     fcol="white", plot.conf=FALSE)
lines(fitted(fit1), col="blue") 
lines(fitted(fit2), col="red")
lines(fitted(fit3), col="green")
lines(fit1$mean, col="blue", type="o") 
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft", lty=1, col=c("black","blue","red","green"), 
       c("Data","Holt's linear trend","Exponential trend","Additive damped trend"))

#7.3
livestock2 <- window(livestock,start=1970,end=2000)
plot(livestock)
fit1 <- ses(livestock2)
fit2 <- holt(livestock2)
fit3 <- holt(livestock2,exponential=TRUE)
fit4 <- holt(livestock2,damped=TRUE)
fit5 <- holt(livestock2,exponential=TRUE,damped=TRUE)
# Results for first model:
fit1$model
accuracy(fit1) # training set
accuracy(fit1,livestock) # test set
plot(fit2$model$state)
plot(fit4$model$state)
plot(fit3, type="o", ylab="Livestock, sheep in Asia (millions)", 
     flwd=1, plot.conf=FALSE)
lines(window(livestock,start=2001),type="o")
lines(fit1$mean,col=2)
lines(fit2$mean,col=3)
lines(fit4$mean,col=5)
lines(fit5$mean,col=6)
legend("topleft", lty=1, pch=1, col=1:6,
       c("Data","SES","Holt's","Exponential",
         "Additive Damped","Multiplicative Damped"))

#7.5
aust <- window(austourists,start=2005)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")

plot(fit2,ylab="International visitor night in Australia (millions)",
     plot.conf=FALSE, type="o", fcol="white", xlab="Year")
lines(fitted(fit1), col="red", lty=2)
lines(fitted(fit2), col="green", lty=2)
lines(fit1$mean, type="o", col="red")
lines(fit2$mean, type="o", col="green")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))
states <- cbind(fit1$model$states[,1:3],fit2$model$states[,1:3])
colnames(states) <- c("level","slope","seasonal","level","slope","seasonal")
plot(states, xlab="Year")
fit1$model$state[,1:3]
fitted(fit1)
fit1$mean