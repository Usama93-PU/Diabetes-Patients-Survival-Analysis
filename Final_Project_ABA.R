library(survival)
library(readxl)
library(ggplot2)
library(survminer)

setwd("E:/Summer Semester/Module 1/Advanced Business Analytics/Data/Project Data")
data <- read_excel("DiabeticData.xlsx")

str(data)

fit.all <- survfit(Surv(time,status==1)~1,data=data)
plot(fit.all, xlab = "Time", ylab = "Survival Probability", main="Visual Loss", 
     ylim = c(0.40,1))

### KM curve shows the survival rate to be between (~0.47-1)

data$laser<- as.factor(data$laser)
data$eye <- as.factor(data$eye)
data$trt<- as.factor(data$trt)

fit.laser<-survfit(Surv(time,status==1)~laser,data=data)
plot(fit.laser,col=2:5, lty=1:4, xlab = "Time", ylim = c(0.40,1), 
     ylab = "Survival Probability", main="Visual Loss by Laser Type")
llabel<-gsub("x=","",names(fit.laser$strata))
legend("bottomleft",legend=llabel,col=2:5,lty=1:4,bty='n')

survdiff(Surv(time,status==1)~laser,data=data)

# Call:
#       survdiff(formula = Surv(time, status == 1) ~ laser, data = data)

#N Observed Expected (O-E)^2/E (O-E)^2/V
#laser=argon 166       68     67.2   0.00988    0.0175
#laser=xenon 228       87     87.8   0.00756    0.0175

#Chisq= 0  on 1 degrees of freedom, p= 0.9

### Survival rate for both types of laser seem to be pretty similar. However, as per
### survdiff analysis laser doesn't seem to be a significant predictor for this problem

data$agecut <- cut(data$age, breaks=c(0, 18, Inf), labels=c("juvenile", "adult"))

fit.age <- survfit(Surv(time,status==1)~agecut,data=data)
plot(fit.age, col=2:5, lty=1:4, xlab = "Time", ylim = c(0.40,1), 
     ylab = "Survival Probability", main="Visual Loss by Age")
llabel <- gsub("x=","",names(fit.age$strata))
legend("bottomleft",legend=llabel,col=2:5,lty=1:4,bty='n')

survdiff(Surv(time,status==1)~agecut,data=data)

# Call:
#        survdiff(formula = Surv(time, status == 1) ~ agecut, data = data)

#N Observed Expected (O-E)^2/E (O-E)^2/V
#agecut=child 222       84     85.2    0.0165    0.0367
#agecut=adult 172       71     69.8    0.0201    0.0367

#Chisq= 0  on 1 degrees of freedom, p= 0.8 

### Again, survival rate for both child & adult seem pretty similar with both becoming
### constant after one point. Also as per survdiff analysis, agecut is an insignificant
### predictor. 

fit.eye<-survfit(Surv(time,status==1)~eye,data=data)
plot(fit.eye,col=2:5, lty=1:4, xlab = "Time", ylim = c(0.40,1), 
     ylab = "Survival Probability", main="Visual Loss by Eye")
llabel <- gsub("x=","",names(fit.eye$strata))
legend("bottomleft",legend=llabel,col=2:5,lty=1:4,bty='n')

survdiff(Surv(time,status==1)~eye,data=data)

# Call:
#        survdiff(formula = Surv(time, status == 1) ~ eye, data = data)

#N Observed Expected (O-E)^2/E (O-E)^2/V
#eye=left  197       69     79.8      1.47      3.03
#eye=right 197       86     75.2      1.56      3.03

#Chisq= 3  on 1 degrees of freedom, p= 0.08 

### As per KM curve for eye, survival rate for left eye seems to be higher but again
### eye is an insignificant predictor for this problem. 

fit.trt<-survfit(Surv(time,status==1)~trt,data=data)
plot(fit.trt,col=2:5, lty=1:4, xlab = "Time", ylim = c(0.40,1), 
     ylab = "Survival Probablity", main="Visual Loss by Treatment")
llabel<-gsub("x=","",names(fit.trt$strata))
legend("bottomleft",legend=llabel,col=2:5,lty=1:4,bty='n')

survdiff(Surv(time,status==1)~trt,data=data)

#Call:
#        survdiff(formula = Surv(time, status == 1) ~ trt, data = data)

#N Observed Expected (O-E)^2/E (O-E)^2/V
#trt=0 197      101     71.8      11.9      22.2
#trt=1 197       54     83.2      10.3      22.2

#Chisq= 22.2  on 1 degrees of freedom, p= 2e-06

### As per KM curve for treatment, survival rate for treatment is drastically higher
### than no treatment. Also, trt is a significant predictor for this problem. 

data$riskcat <- cut(data$risk, breaks=c(5, 9, 12), labels=c("medium", "high"))

fit.risk<-survfit(Surv(time,status==1)~riskcat,data=data)
plot(fit.risk,col=2:5, lty=1:4, xlab = "Time", ylim = c(0.40,1), 
     ylab = "Survival Probability", main="Visual Loss by Risk")
llabel<-gsub("x=","",names(fit.risk$strata))
legend("bottomleft",legend=llabel,col=2:5,lty=1:4,bty='n')

survdiff(Surv(time,status==1)~riskcat,data=data)

#Call:
#        survdiff(formula = Surv(time, status == 1) ~ riskcat, data = data)

#N Observed Expected (O-E)^2/E (O-E)^2/V
#riskcat=medium 196       58     85.1      8.62      19.2
#riskcat=high   198       97     69.9     10.50      19.2

#Chisq= 19.2  on 1 degrees of freedom, p= 1e-05

### As per KM curve for risk, survival rate for high risk group is lower vs. medium 
### risk group. Also, risk is a significant predictor for this problem. 

fit.cox <- coxph(formula = Surv(time, status  ==1) ~ laser + age + eye + trt 
                 + riskcat ,data = data)
summary(fit.cox)

# Call:
#        coxph(formula = Surv(time, status == 1) ~ laser + age + eye + 
#                      trt + riskcat, data = data)

#n= 394, number of events= 155 

#coef exp(coef)  se(coef)      z Pr(>|z|)    
#laserxenon   0.073359  1.076117  0.293240  0.250   0.8025    
#age          0.005193  1.005207  0.009690  0.536   0.5920    
#eyeright     0.306206  1.358262  0.163292  1.875   0.0608 .  
#trt1        -0.837169  0.432935  0.169934 -4.926 8.37e-07 ***
#        riskcathigh  0.720188  2.054819  0.166947  4.314 1.60e-05 ***
#        ---
#        Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#exp(coef) exp(-coef) lower .95 upper .95
#laserxenon     1.0761     0.9293    0.6057     1.912
#age            1.0052     0.9948    0.9863     1.024
#eyeright       1.3583     0.7362    0.9863     1.871
#trt1           0.4329     2.3098    0.3103     0.604
#riskcathigh    2.0548     0.4867    1.4814     2.850

#Concordance= 0.655  (se = 0.022 )
#Likelihood ratio test= 47.1  on 5 df,   p=5e-09
#Wald test            = 44.93  on 5 df,   p=1e-08
#Score (logrank) test = 46.81  on 5 df,   p=6e-09

### The overall model's p-value of 5.946e-09 means that the model is meaningful 
### and would accurately portray the visual loss of patients
### trt1 is significant based on p-value (8.17e-07)
### Interpreting exp(coef) we can conclude, compared to the reference variable 
### trt0, trt1 is 0.57x less likely to lose vision as those who did not receive treatment
### riskcathigh is significant based on p-value (1.44e-05), riskcathigh is ~1x more likely 
### to not lose vision.
### Interpreting exp(coef) we can conclude, compared to the reference of riskcatmedium, 
### riskcathigh is 1.0628x more likely to lose vision as those who are in the medium 
### risk group.

#Residual Plot
plot(predict(fit.cox),residuals(fit.cox,type = 'martingale'),ylab = 'residuals',
     xlab = 'fittedvalues')
abline(h=0)
lines(smooth.spline(predict(fit.cox),residuals(fit.cox,type = 'martingale'))
      ,col='red')

### Shows clear linear residual plot

#Checking for Constant Hazard Ratio 
k=cox.zph(fit.cox)
k

plot(k[5,])
abline(h=0)

plot(k[4,])
abline(h=0)

plot(k[3,])
abline(h=0)

plot(k[2,])
abline(h=0)

plot(k[1,])
abline(h=0)
### All have constant hazard ratio


#Outlier Analysis
ggcoxdiagnostics(fit.cox, type = "dfbeta",linear.predictions = FALSE, 
                 ggtheme = theme_bw())

### No outlier values have significant contribution