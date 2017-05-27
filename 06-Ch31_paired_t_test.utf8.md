# Chapter 31: Comparing Two Paired Groups

Kenzie Pereira, with Nathan Brouwer

## Introduction

### About the data used in this Chapter:
The data are originally from Darwin.  He conducted an experiment to determine the impacts of cross- versus self-fertilization.  These data appear in several R packages (HistData::ZeaMays, marg::darwin, boot::darwin), but always as the difference between paired measurements, not the raw data.  The help files for these packages provide more information on the data set and examples of their use, especially ?HistData::ZeaMays.

The R documentation ?HistData::ZeaMays describes the experiment as 

> "Darwin (1876) studied the growth of pairs of *Zea mays* (aka corn) seedlings, one produced by cross-fertilization and the other produced by self-fertilization, but otherwise grown under identical conditions. His goal was to demonstrate the greater vigour of the cross-fertilized plants. The data recorded are the final height (inches, to the nearest 1/8th) of the plants in each pair."


Owens and Miller (2009) discuss Darwin's work on plant fertilizaiton.


## Recreating Table 31.1 

Motulsky presents Darwin's raw data in Table 31.1 (pg 232).

### Enter data 

```r
cross.fert <- c(23.5,  12,   21,    22,   19.125,21.5,  22.125,
                20.375,18.25,21.625,23.25,21,    22.125,23,
                12)

self.fert <- c(17.375,20.375,20,20,18.375,18.625,18.625,15.25,16.5,18,16.25,18,12.75,15.5,18)
```

 Each data set should be of equal length. Use "==" to check. If of equal lengths, the output should be "TRUE"

```r
length(cross.fert) == length(self.fert)
```

```
## [1] TRUE
```

### Construct the table

```r
dattab <- data.frame(cross.fert = cross.fert,
                     self.fert = self.fert)

names(dattab) <- c("CROSS FERTILIZED", "SELF-FERTILIZED")

dattab$DIFFERENCE <- dattab[,1]-dattab[,2]
```

### Make the data table with pander. 

The output of this script will be the completed data table (object class = dataframe) om Table 31.1


```r
library(pander)
```


```r
# use pander() to create datatable. Caption 
pander(dattab,
       caption = "Sample data for paired t test")
```


-------------------------------------------------
 CROSS FERTILIZED   SELF-FERTILIZED   DIFFERENCE 
------------------ ----------------- ------------
       23.5              17.38          6.125    

        12               20.38          -8.375   

        21                20              1      

        22                20              2      

      19.12              18.38           0.75    

       21.5              18.62          2.875    

      22.12              18.62           3.5     

      20.38              15.25          5.125    

      18.25              16.5            1.75    

      21.62               18            3.625    

      23.25              16.25            7      

        21                18              3      

      22.12              12.75          9.375    

        23               15.5            7.5     

        12                18              -6     
-------------------------------------------------

Table: Sample data for paired t test

## Analyzing Data

Data in the form of Table 31.1 can be fed into "t.test()" for an analysis, but its more common to "stack" the data. Therefore, we are going to make a new data table that will be used for conducting actual analyses.


```r
#determine number of pairs
n <- length(self.fert)

#Assemble the data
dat <- data.frame(height = c(cross.fert,
                             self.fert),
                  cross = c(rep("cross",n),
                          rep("self",n)),
                  ID = c(1:n,1:n))

#Look at new table
head(dat)
```

```
##   height cross ID
## 1 23.500 cross  1
## 2 12.000 cross  2
## 3 21.000 cross  3
## 4 22.000 cross  4
## 5 19.125 cross  5
## 6 21.500 cross  6
```

```r
tail(dat)
```

```
##    height cross ID
## 25  18.00  self 10
## 26  16.25  self 11
## 27  18.00  self 12
## 28  12.75  self 13
## 29  15.50  self 14
## 30  18.00  self 15
```

## Plotting Interactions

We'll recreate Figure 31.1 (pg. 233) representing a "Before-After Graph" using the stacked data table created above. This can be done by using either the interaction.plot() function in Base R or with ggplot2 using "qplot()".


### Figure 31.1: Interaction plot with Base R

* See the [UCLA IDRE](http://stats.idre.ucla.edu/r/faq/how-can-i-make-spaghetti-plots/) website for information on interaction plots (aka "spaghetti plots").
* x.factor = x axis
* response = y axis
* trace.factor = what identifies linked/associated records


```r
interaction.plot(x.factor = dat$cross,
                 response = dat$height,
                 trace.factor = dat$ID,
                 legend = F, 
                 ylab = "Plant Height (inches)", 
                 xlab = "")
```

<img src="06-Ch31_paired_t_test_files/figure-html/unnamed-chunk-7-1.png" width="672" />


### Figure 31.1: Interaction plot with ggplot::qplot

The arguement "group = ID" tells qplot that the cross & self fertilized data values are linked and should be connected by a line.


```r
## upload ggplot2 and cowplot to R
library(ggplot2)
library(cowplot)
```

```
## 
## Attaching package: 'cowplot'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     ggsave
```

```r
## create interaction plot using qplot()
qplot(y = height,
      x = cross,
      data = dat,
      color = factor(ID),
      group = ID,
      geom = c("point",
               "line"), 
      ylab = "Plant Height (inches)", 
      xlab = ""
      )
```

<img src="06-Ch31_paired_t_test_files/figure-html/unnamed-chunk-8-1.png" width="672" />

### Figure 31.2 (pg233): 

An Alternative to plotting interactions using a before-after graph using a bar plot. However, this is an inferior approach because though this plots the same data as Figure 31.1 it only shows the mean and SEM of each group with accounting for pairing.

For practice, we'll carry out all the calcualtions by hand, though there are other ways to do this that require less code.


```r
#load packages "doBy" and "plotrix" to R
library(gplots)
```

```
## 
## Attaching package: 'gplots'
```

```
## The following object is masked from 'package:stats':
## 
##     lowess
```


Calculate the summary statistics

```r
# Calculate mean of each data set using mean()
mean_cross <- mean(cross.fert)
mean_self <- mean(self.fert)

#Calculate standard deviation (SD) of each data set using sd()
sd_cross <- sd(cross.fert)
sd_self <- sd(self.fert)

#Caculate the standard error fo the mean (SEM) using formula sd/sqrt(length (data))
sem_cross <- sd_cross/sqrt(length(cross.fert))
sem_self <- sd_self/sqrt(length(self.fert))
```


Use calculated mean and SEM values and original data to recreate Figure 31.2

```r
barplot2(height = c(mean_cross, mean_self), 
                   names.arg = c("Cross-fertilized", "Self-fertilized"), 
                   ylab = "Plant Height (inches)", 
                   xpd = FALSE, 
                   ylim = c(0,25), 
                   plot.ci = TRUE, 
                   ci.u = c(mean_cross + sem_cross, mean_self + sem_self),
                   ci.l = c(mean_cross - sem_cross, mean_self - sem_self),                    ci.width = 0.15)
                  abline(h=0)
```

<img src="06-Ch31_paired_t_test_files/figure-html/unnamed-chunk-11-1.png" width="672" />
Figure 31.2: And Alternative, but Inferior Approach to a Before-After Graph


## Analyses Using Paired t-test

There are multiple ways to carry out a paired t-test in R. The way that you use depends on 1)how the data are formatted and the 2)function you use.  We'll show several variations on how the data can be entered and analyzed.  As for the R function used to coduct the test, the most common way is with the t.test() function; this is how almost everyone will do it.

However, there are also essentially equivalent ways to run the model with other functions, including lm(), aov(), nlme::lme(), lme4::lmer(), and ez:ezANOVA.  These are unlikley to be used in practice. However, comparing these different methods is a way of seeing how different functions and modeling approaches are all based on the same fundamental question: "Is the difference between a pair of observations great than zero?"  We'll cover these methods to introduce these different functions, which are typically appiled to more complex analyses, and also show how they are all conceptually linked.

### Paired t-test on data in standard "long" format.

For this to work, however, the order has to be correct - the first crossed plant to occur in the data frame and first self plants have to be the same, the 2nd have to be the same, etc .  If the records are jumbled, you'll get the wrong answer.

Note: this uses a formula notation with a "~"


```r
t.test.out <- t.test(height ~ cross,
       data = dat,
       paired = TRUE)

# Look at results
t.test.out
```

```
## 
## 	Paired t-test
## 
## data:  height by cross
## t = 2.148, df = 14, p-value = 0.0497
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.003899165 5.229434169
## sample estimates:
## mean of the differences 
##                2.616667
```

### Paired t-test on data in separate vectors

We'll use the original data vectors we used for data entry.
Note: this approach does NOT use a "~" to seperate the vectors.

```
## 
## 	Paired t-test
## 
## data:  cross.fert and self.fert
## t = 2.148, df = 14, p-value = 0.0497
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.003899165 5.229434169
## sample estimates:
## mean of the differences 
##                2.616667
```


### Paired t-test on Data in "wide" Format

Using our "wide" data set up like Motulsky's table (Table 31.1) we could also run the t.test like this.  Again, this does NOT use "~"

```
## 
## 	Paired t-test
## 
## data:  dattab[, 1] and dattab[, 2]
## t = 2.148, df = 14, p-value = 0.0497
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.003899165 5.229434169
## sample estimates:
## mean of the differences 
##                2.616667
```

### Unpaired 1-Sample Test on Differences

A paired t-test is IDENTICAL to a 1-sample t-test where you are comparing the mean of a sample against 0 ("mu = 0").  A 1-sample t-test is often one of the first things you are taught in stats class but they don't bother to mention that it is the SAME thing as a paired t-test later in the class!


```r
t.test.onesample <- t.test(dattab$DIFFERENCE,
       mu = 0)

# Look at results
t.test.onesample
```

```
## 
## 	One Sample t-test
## 
## data:  dattab$DIFFERENCE
## t = 2.148, df = 14, p-value = 0.0497
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  0.003899165 5.229434169
## sample estimates:
## mean of x 
##  2.616667
```

## Equivalent ways to run a paried t-test 

These methods are unlikely to be used in practice but its useful to see how they all produce similar results as t.test()


### Paired t-test as a Repeated Measures ANOVA with aov()

The p-values are exactly the same as a t-test.

Note: for aov() to work correctly the group variable ID must be a factor, not numeric. 


```r
# Check the class of dat$ID
class(dat$ID)
```

```
## [1] "integer"
```
The output = "integer" which is numeric. We need to change this to a factor.


Convert ID to factor

```r
dat$ID <- factor(dat$ID)
```

Now check the class of dat$ID again. If the output is "factor", you are good to go using aov()!

```r
class(dat$ID)
```

```
## [1] "factor"
```


Conduct Repeated Measures ANOVA using aov()

```r
m.aov <- aov(height ~ cross + Error(ID/height),
       data = dat)

# Look at summary data
summary(m.aov)
```

```
## 
## Error: ID
##           Df Sum Sq Mean Sq F value Pr(>F)
## Residuals 14  86.26   6.162               
## 
## Error: height:ID
##           Df Sum Sq Mean Sq F value Pr(>F)  
## cross      1  51.35   51.35   4.614 0.0497 *
## Residuals 14 155.82   11.13                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


### Paired t-test using ez::ezANOVA

the ezANOVA function is used by some people to simplify specification of the Error() term in aov().  ezANOVA() is a "wrapper" that runs aov().  


```r
library(ez)
```


Conduct paired t-test using ezANOVA()

```r
ezANOVA(data = dat,
        dv = height,
        wid = ID,
        within = cross)
```

```
## $ANOVA
##   Effect DFn DFd       F          p p<.05      ges
## 2  cross   1  14 4.61385 0.04970294     * 0.175003
```

### Paired t-test as Linear Model of differences using lm()

A 1-sample t-test of the differences is the SAME as a linear regression model with only an intercept. This is the only way to get paired t-test results with the lm() function.


```r
summary(lm(DIFFERENCE ~ 1, data = dattab))
```

```
## 
## Call:
## lm(formula = DIFFERENCE ~ 1, data = dattab)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.9917  -1.2417   0.3833   3.0083   6.7583 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    2.617      1.218   2.148   0.0497 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.718 on 14 degrees of freedom
```

### ADVANCED: Paired t-test as a mixed model

*The following section contains advanced content.*

A paired t-test can be run as a mixed effects model with nlme::lme and lme4::lmer.  Since we are focused on the fixed effects, we'll use maximum likelihood (e.g., method = "ML" for lme, REML = FALSE for lmer).  Note: p-values are a little different.

### A Warning on p-values and Mixed Models

There are many ways to get p-values from mixed models. We have shown several ways for reference.  See ?lme4::pvalue for more information.  These two sites provide additional information.  

* http://glmm.wikidot.com/faq
* http://mindingthebrain.blogspot.com/2014/02/three-ways-to-get-parameter-specific-p.html

In general, the approach you use will depend on whether you are looking at fixed or random effects, whether your data are balanced and match traditional experimental designs, and whether you fit the model with REML or ML.  The Kenward-Rogers method (pbkrtest::KRmodcomp) is a good choice if you used lmer. Confidence intervals from "parametric bootstrapping"" are probably the best apporach for reporting results especially when using generalized linear models, but requires some work to implement.

### Mixed Model with nlme::lme

The p-value is similar but not exact because of the use of maximum likelihood (I think)



First, install the "nlme" package to R


```r
library(nlme)
```


```r
# Run Alternative Model
m.lme.full <- lme(height ~ cross,
             random = ~1|ID,
             method = "ML",
       data = dat)

# Run Null Model
m.lme.null <- lme(height ~ 1,
             random = ~1|ID,
             method = "ML",
       data = dat)
```

Conduct a Likelihood Ratio Test using anova()

```r
anova(m.lme.full,
      m.lme.null)
```

```
##            Model df      AIC      BIC    logLik   Test  L.Ratio p-value
## m.lme.full     1  4 155.7789 161.3836 -73.88943                        
## m.lme.null     2  3 159.5501 163.7537 -76.77506 1 vs 2 5.771267  0.0163
```


Conduct a F-test using the "car" Package 

```r
library(car)
```



```r
# Conduct F-test using Anova()
Anova(m.lme.full,
      test.statistic = "F")
```

```
## Analysis of Deviance Table (Type II tests)
## 
## Response: height
##        Chisq Df Pr(>Chisq)  
## cross 6.3638  1    0.01165 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Mixed model w/ lme4::lmer

This method gives similar results as lme, but again, not the same as t.test (or aov or lm)


```r
library(lme4)
```

```
## Loading required package: Matrix
```

```
## Loading required package: methods
```

```
## 
## Attaching package: 'lme4'
```

```
## The following object is masked from 'package:nlme':
## 
##     lmList
```


```r
# Run Alternative Model
m.lmer.full <- lmer(height ~ cross + (1|ID),
       data = dat,
       REML = FALSE)

# Run Null Model
m.lmer.null <- lmer(height ~ 1 + (1|ID),
       data = dat,
       REML = FALSE)

# Again, conduct Likelihood Ratio Test using anova()
anova(m.lmer.full,m.lmer.null)
```

```
## Data: dat
## Models:
## m.lmer.null: height ~ 1 + (1 | ID)
## m.lmer.full: height ~ cross + (1 | ID)
##             Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
## m.lmer.null  3 159.55 163.75 -76.775   153.55                           
## m.lmer.full  4 155.78 161.38 -73.889   147.78 5.7713      1    0.01629 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


#### Kenward-Rogers approximation

The "Kenward-Rogers approximation" is fequently used for p-values for mixed models.

Install and load "pbkrtest" to R

```r
library(pbkrtest)
```

Conduct K-R Approximation using KRmodcomp()

```r
KRmodcomp(m.lmer.full,
          m.lmer.null)
```

```
## F-test with Kenward-Roger approximation; computing time: 0.21 sec.
## large : height ~ cross + (1 | ID)
## small : height ~ 1 + (1 | ID)
##          stat     ndf     ddf F.scaling p.value  
## Ftest  5.9395  1.0000 14.0000         1 0.02875 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## Figure 31.3 (p234): The defference between matched measurements.

Plot the differences.  using par() you can create figures composed of multiple graphs

```r
par(mfrow = c(1,2))
library(beeswarm)

# Figure 31.3.a, use beeswarm()to create beeswarm plot shown on the left panel of Fig. 31.3
beeswarm(dattab$DIFFERENCE,
         pch = 16)

#abline() is used to add a line in the plot.
abline(h = mean(dattab$DIFFERENCE))
abline(h = 0, col = 2,lty =2)
```

<img src="06-Ch31_paired_t_test_files/figure-html/unnamed-chunk-32-1.png" width="672" />

Using the t.test.long data that we got earlier (see lines 177-184), we can get the mean difference and the confidence interval around these data

```r
t.test.out$estimate
```

```
## mean of the differences 
##                2.616667
```

```r
t.test.out$conf.int[1]
```

```
## [1] 0.003899165
```

```r
t.test.out$conf.int[2]
```

```
## [1] 5.229434
```

Recreate second panel of Figure 31.3 located on the right using barplot2(). Essentially, we will be plotting the mean difference and the 95%CI.  

```r
barplot2(height = t.test.out$estimate, 
          ylab = "Difference in Height", 
          xlab = "", 
          width = 4, 
          #xlim = c(0,2),
          plot.ci = TRUE, 
          ci.l = t.test.out$conf.int[1],
          ci.u = c(t.test.out$conf.int[2]), 
          ci.width = 0.75, ylim = c(0,6))
```

<img src="06-Ch31_paired_t_test_files/figure-html/unnamed-chunk-34-1.png" width="672" />

Note: Motulsky in Figure 31.3 plots the SEM of the difference.  In general its better to plot the 95% CI, but he uses the SEM.  See code above for calculating SEM.

## Recreate Table 31.2 (p234) using pander()


## Figure 31.4 (p236): Testing the need for a paired t test. 

In other words, are the paired values correlated?  The reason for doing a paired t-test is because you expect that the paired values are somehow correlated. 

To recreate this graph, ggplot is being used to make the layout look nice.  The low correlation is an indication that most of the variation was between the fertilization treatment, and that there was not much variation between pairs relative to the effect induced by the experiemtnal treatment.


```r
library(ggplot2)
library(cowplot)
qplot(x =cross.fert, 
      y = self.fert, 
      ylab = "Self-fertilized", 
      xlab = "Cross-fertilized")
```

<img src="06-Ch31_paired_t_test_files/figure-html/unnamed-chunk-36-1.png" width="672" />
Figure 31.4: Testing the need for a paired t-test


Here is how to get the correlation coefficient shown within the figure using the cor() function.

```r
cor(cross.fert,
    self.fert)
```

```
## [1] -0.3347553
```

## References

Cross- and self-fertilization of plants - Darwin's experiments and what we know now.  Bot. J. Linn. Soc.  161:357-395.
http://onlinelibrary.wiley.com/doi/10.1111/j.1095-8339.2009.01010.x/abstract



## Unfished section: comparing p values from different appraoches


```r
# mean.t <- t.test.out$estimate
# t.t <- t.test.out$statistic
# p.t <- t.test.out$p.value
# 
# 
# #Isolate the F and p-value - there has to be an easier way...
# mean.aov <-  NA
# F.aov <- summary(m.aov)[[2]][[1]][[4]][1]
# p.aov <- summary(m.aov)[[2]][[1]][[5]][1]
# 
# 
# mean.lme <- summary(m.lme.full)$tTable[2,"Value"]
# t.lme    <- summary(m.lme.full)$tTable[2,"t-value"]
# t.p.lme    <- summary(m.lme.full)$tTable[2,"p-value"]
# 
# 
# #F
# F.lme <- anova(m.lme.full)[["F-value"]][2]
# F.p.lme <- anova(m.lme.full)[["p-value"]][2]
# 
# #Likelihood rati (?)
# LR.lme <- anova(m.lme.full,
#       m.lme.null)[["L.Ratio"]][[2]]
# LR.p.lme <- anova(m.lme.full,
#       m.lme.null)[["p-value"]][[2]]
# 
# mean.lmer <- summary(m.lmer.full)$coefficients[2]
# t.lmer    <- summary(m.lmer.full)$coefficients[6]
# t.p.lmer <- NA
# 
# F.lmer <- anova(m.lmer)$"F value"
# F.p.lmer <- NA
# 
# Chi2.lmer <- Anova(m.lmer)$Chisq
# Chi2.p.lmer <- Anova(m.lmer)$"Pr(>Chisq)"
# 
# 
# 
# #Chi2
# library(car)
# Ch2.lme <- Anova(m.lme.full)[["Chisq"]]
# Ch2.p.lme <- Anova(m.lme.full)[["Pr(>Chisq)"]]
```
