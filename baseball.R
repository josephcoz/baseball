#read in data
baseball <- read.table(text = "Year	TVaud	Tigers	AtBreak	DNP	Bullpen
2004	235.6	2		0.483	1	10	
2005	436.6	1		0.488	4	10
2006	290.9	3		0.670	7	10
2007	305.1	5		0.605	2	10
2008	223.3	1		0.500	4	11
2009	302.1	5		0.552	19	13
2010	260.8	3		0.558	10	16
2011	201.9	5		0.533	15	13
2012	296.2	3		0.512	6	9
2013	374.7	6		0.553	8	14
2014	279.9	4		0.582	17	12
2015	190.4	4		0.500	8	12
2016	122.1	1		0.517	11	14
2017	111.2	2		0.448	7	11
2018	87.3	1		0.418	9	14", header = TRUE)


str(baseball)

#find means of variables
summary(baseball)

#find standard deviations of variables
sd(baseball$TVaud)
sd(baseball$Tigers)
sd(baseball$AtBreak)
sd(baseball$DNP)
sd(baseball$Bullpen)

#find correlation b/w explanatory and response varieable
cor(baseball$Year, baseball$TVaud)
cor(baseball$Tigers, baseball$TVaud)
cor(baseball$AtBreak, baseball$TVaud)
cor(baseball$DNP, baseball$TVaud)
cor(baseball$Bullpen, baseball$TVaud)

#Model : TVAud = beta0 + beta1 * Tigers + beta2 * AtBreak + beta3 * DNP + beta4 * Bullpen + epsilon, 
#epsilon ~ N(0,signm^2)

baseball_out <- lm(TVaud ~ Tigers + AtBreak + DNP + Bullpen, data = baseball)

baseball_rstud <- rstudent(baseball_out)

#plot r-studentized residuals
plot(density(baseball_rstud), main = "R-studentized residuals for Baseball data")
my_z <- seq(-3, 3, length = 50)
lines(my_z, dnorm(my_z, 0, 1), col = 'purple', lty = 2)

baseball_rstud
#2005 is greater than 3, which violates rule of thumb

#Remove 2005
baseball <- baseball[-2,]

#check if removed
baseball

#re-fit model using new data (will overwrite old data)
baseball_out <- lm(TVaud ~ Tigers + AtBreak + DNP + Bullpen, data = baseball)

#new r-studentized as well
baseball_rstud <- rstudent(baseball_out)

#test if new data is normal using shapiro-wilk test
shapiro.test(baseball_rstud)
#p-value is not significant, data is normal

#Regression Data Diagnostics

#leverage: the weight an obs has in predicting itself
baseball_leverage <- lm.influence(baseball_out)$hat

baseball_leverage
#Leverage *rule of thumb*: 2*(p+1)/n
#n=num rows, p=num parameters
subset(baseball, baseball_leverage > 2 * 4 / 14)
#this gives us 2006 as a potential influential observation

#Cook's distance: change in parameter estimates with and without obs
baseball_cooks <- cooks.distance(baseball_out)

#Cook's distance *rule of thumb*: 4/(n-(p+1))
#n = num rows, p=num parameters
subset(baseball, baseball_cooks > 4 / (14 - (4)))

identify(plot(baseball$TVaud, cex=0.5))

#check for collinearity
plot(~Tigers+AtBreak+DNP+Bullpen, data=baseball)

#show model estimates after determining which observations to use
#(we only removed 2005)
summary(baseball_out)

#show confidence intervals
confint(baseball_out)

#make plot for Tiger response variable
plot(baseball$Tigers, baseball$TVaud, main = "Tigers in All-Star Game vs. TV Viewership",
     xlab = "Tigers", ylab = "Viewership")
abline(lm(baseball$TVaud~baseball$Tigers), col = "blue")

#make plot for Bullpen response variable
plot(baseball$Bullpen, baseball$TVaud, main = "Pitchers in Bullpen vs. TV Viewership",
     xlab = "Pitchers", ylab = "Viewership")
abline(lm(baseball$TVaud~baseball$Bullpen), col = "darkgreen")

#predict values if Tigers successful
successful <- predict(baseball_out, newdata=data.frame(Tigers=5,AtBreak=0.6,DNP=7,Bullpen=11),
                           type="response", se.fit=TRUE)

#calculate confidence interval
successful_L <- successful$fit - 1.96*successful$se.fit
successful_U <- successful$fit + 1.96*successful$se.fit

#predict values if Tigers tank
tanking <- predict(baseball_out, newdata=data.frame(Tigers=1,AtBreak=0.4,DNP=7,Bullpen=11),
                      type="response", se.fit=TRUE)

#calculate confidence interval
tanking_L <- tanking$fit - 1.96*tanking$se.fit
tanking_U <- tanking$fit + 1.96*tanking$se.fit

#show successful predictions
successful
successful_L
successful_U

#show tanking predictions
tanking
tanking_L
tanking_U
