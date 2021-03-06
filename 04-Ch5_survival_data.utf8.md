# Motulksy Chapter 5: Survial Analysis

*Nathan Brouwer*

## Introduction

In Chapter 5 of [Intutive Biostatistics, (2nd ed)](https://www.amazon.com/Harvey-Motulsky/e/B001IGQN0Q), Motusky introduces [survival analysis](https://en.wikipedia.org/wiki/Survival_analysis) by demonstrating the construction of a [Kaplan-Meir survival curve](https://en.wikipedia.org/wiki/Kaplan%E2%80%93Meier_estimator).  Survival analysis is also known as "time to event" or "failure time" analysis and can be used to determine the surival or longevity anything, including humans, states (fighting, sick), and widgets.  Kaplan-Meir methods are only one of many types of survival analysis.  Motulsky further elaborates on these methods in Chapter 20 (2nd Ed).  

The central point of survival analysis methods are that they allow you to use ["censored data"](https://en.wikipedia.org/wiki/Censoring_(statistics)) where the final fate or state of the study individual or object isn't known.  For example, agronomists might be interested if a certain chemical treatment increase the rate of seed germination, but some seeds might not germinate by the end of the study.  To determine mean time to germination, the researchers could take the mean germination date of those seeds that did germinate.  This would biased by ingoring seeds that didn't germinate.  Survival analysis allows the "censored" seeds that did not germinate to be included in the analysis, yielding an unbiased estimate.

Survival analysis is frequently used in biomedical studies such as those testing the effects of medication; see Fisher & Linn (1999) for an overview.  Survival analysis is less frequently used in other areas of biological research.  Its utility for environmental biologists and ecologists has frequently been highlighted for tasks such as seed germination (McNair et al 2012), pollinator visitation rates (CITE), animal contests (Moya-Larano Estacion and Wise 2000), expansion of invasive species (Reino et al 2009), plant disease biology (Scherm and Ojiambo 2004) and leaf survival (Egli and Schmid 2001).  In contrast, researchers using [radio telemetry](https://en.wikipedia.org/wiki/Wildlife_radio_telemetry) to study wildlife survival and movement frequently use methods based on survival analysis (CITE).  Avian ecologist also use survival methods to study nest survival (Heisy te al 2007)

The issue of censored data arrises not just in studies recording the outcomes of binary events.  For example, when chemical sensors or assays have a lower detection limit, data is censored by this lower limit.  For an ecological focused introduction to general issues of censored data [see Fox and Negrete-Yankelevich (2015)](https://www.amazon.com/Ecological-Statistics-Contemporary-theory-application/dp/0199672555/ref=sr_1_1?s=books&ie=UTF8&qid=1487083974&sr=1-1&keywords=fox+ecological+statistics).

## Survival analysis in R

**R has a large array of basic and advanced methods for survival analysis.  We will use the basic package ['survival'](https://cran.r-project.org/web/packages/survival/index.html) and its functions Surv() and survfit()**  R is a major platform for the development of new survival methods, such as [mixed-effects survival models](https://cran.r-project.org/web/packages/coxme/index.html).  **See the [CRAN Task View](https://cran.r-project.org/web/views/Survival.html) for more info.**

## This tutorial

* In the following tutorial we demonstrate how to take the data provided by Motulsky, analyze it in R, and produce the key figures of Chapter 5. 
* We pay special attention to formatting the data properly for survival analysis.
* Date formatting for survival analysis in particular involves converting raw dates input as "character data" (ie "10/13/1976"") to R's special "date" format.
* Start and end dates then need to be converted to elapsed time.
* This conversion is tricky.  (Excel might have functions that are more intutive, but I don't know).

**R Functions Used**
  
* c()
* paste()
* ifelse()
* is()
* as.Date()
* as.character()
* as.numeric()
* pander::pander()
* survival::Surv()
* survival::survfit()


**Key Vocab**
  
* survival analysis
* time-to-event
* censored data
* right-censoring

<br>

## Motulsky's Example Data

**Motulsky provides a short data table of survival times (Table 5.1).  This table could be loaded from a .csv file, but for convience we will use the following code builds this table.**


### Data entry

First, each column of data need to be made.  (Note that I split up the date into month, data and year to make data entry easier).


```r
#Parts of data table
## Starting date
start.month <- c(2,5,11,3,6,12, 12)
start.day <- c(7,19,14,4,15,1,15)
start.year <- c(1998,1998,1998,
                1999,1999,1999,
                1999)

##Ending data
###when individual died or was "censored"
end.month <- c(3,11,3, 5,5,9,8)
end.day <- c(2,30,3,4,4,4,15)
end.year <- c(2002,2004,2000,
              2005,2005,2004,
              2003)

## Fate
fate1 <- c("Died","moved","Died","Study ended","Died","Died","Died-car crash")
```

<br>

## Data prep

Next, I'll assemble the dates using paste().  First the start date.


```r
start.date <- paste(start.month,
                    start.day,
                    start.year, 
                    sep = "/")
```


Then the end date.

```r
end.date <- paste(end.month,
                  end.day,
                  end.year,
                  sep = "/")
```
## Assemble table 5.1

Assemble the final table

```r
dat.tab <- data.frame(start.date = start.date,
           end.date,
           fate = fate1)
```


--------------------------------------
 start.date   end.date       fate     
------------ ---------- --------------
  2/7/1998    3/2/2002       Died     

 5/19/1998   11/30/2004     moved     

 11/14/1998   3/3/2000       Died     

  3/4/1999    5/4/2005   Study ended  

 6/15/1999    5/4/2005       Died     

 12/1/1999    9/4/2004       Died     

 12/15/1999  8/15/2003  Died-car crash
--------------------------------------
**Motulsky Table 5.1** "Sample survival-data details for the data plotted in Figure 5.1" (page 38, 2nd edition)



## Peparing data for analysis

The data in table 5.1 need to be processed inorder to enter into R's survival functions.  Two things need to be done.

* Determine elapsed time til death/censure
* Convert notes on fate to binary 0/1


### Convert start.date to R's "date"" format

* Dates can be very tricky in R.  
* The as.Date() command does this conversion.

### Conversion to character data
When we made the date columns and added to the datatable R automatically converted them "factor" format.  The original vector was "character", which is the format we need it to be in.

We can see what type of data soemthing is using is()

```r
#start.date was charcter data...
# is(start.date)[1]

#but data.frame() converted it to factor
# is(dat.tab$start.date)[1]
```

We can convert it back using as.charcter()

```r
#convert start date
dat.tab$start.date <- as.character(dat.tab$start.date)

#convert end date
dat.tab$end.date <- as.character(dat.tab$end.date)
```

#### Conversion to "date" format

* The as.Date() function will convert the start and end date column to a species "date" format that will make clacualting elapsed time easy.  
* We need to specify th order the the month, day, and year appear in the column using the second argument "%m/%d/%Y"


```r
#convert start.date
dat.tab$start.date <- as.Date(dat.tab$start.date ,format = "%m/%d/%Y")

#convert end.date
dat.tab$end.date <- as.Date(dat.tab$end.date ,format = "%m/%d/%Y")
```

We can check what the conversion did using is()

```r
#is(dat.tab$end.date)
```


#### Calculate elapsed days

Once in R's "date"" format we can do math with the columns to calculate the time difference in days between the end and the start date.

```r
dat.tab$days <- dat.tab$end.date -dat.tab$start.date 
```


The time data has a special format in R , "difftime"

```r
#is(dat.tab$days)
```


For this to work properly we need to convert the data expliclity to numeric data.  All this data conversion is kind of a pain but is essential to get R to work on the data.

```r
dat.tab$days  <- as.numeric(dat.tab$days )
```


Motulsky converts the data to years

```r
dat.tab$years <- dat.tab$days/365
```


#### Convert fates to 0/1
Motulsky list fates as detailed comments in table 5.1.  To analyze the data I need to convert to 

* "0" for "alive at end of study"
    + this includes one individual who died for a reason other than what the study was focused on
* "1" for "deceased at end of study"



```r
#recode fates
fate2 <- ifelse(fate1 == "Died",1,0)
```

#### Assemble table 5.2

* We can now put Motulsky's Table 5.2
* This is the format that the data will actually be analyzed in R

Assemble the table


```r
dat.tab2 <- data.frame(year = dat.tab$years,
                       fate.01 = fate2)
```

The final table

```r
pander(dat.tab2)
```


----------------
 year   fate.01 
------ ---------
4.066      1    

 6.54      0    

1.301      1    

6.173      0    

 5.89      1    

4.764      1    

3.668      0    
----------------




## Survival analysis in R

Load the 'survival' package.   The package can be downloaded from CRAN in RStudio by clicking on the "packages" tabe next to the "plot" tab.  Next click on "Install" and type in "survival".


```r
library(survival)
```

### Convert "days" column to a survival object

The Surv() function formats the data for the final analysis.  Note the capital "S".


```r
library(survival)
surv.object <- Surv(time = dat.tab2$year,
     event = dat.tab2$fate.01)
```

### Surv() ouput

The processed survival data has a unique format (this is a theme for survival analysis in R...)

* A number indicates how long the individual survived, in years; 
* A "+" indicates that they were still alive at the end of the study (or left the study for a reason unrelated to the study question)


```r
surv.object
```

```
## [1] 4.065753  6.539726+ 1.301370  6.172603+ 5.890411  4.764384  3.668493+
```

## Survival analysis

* The survfit() function take the processed data and fits the actual survival model.
* R uses different default than those used by Motulsky to produce Figure 5.1. 
* To reproduce Motulsky's results we set the following argument
  + conf.type = "log-log"


### Fit the survival model

```r
surv.fitted.Motulsky <- survfit(surv.object ~ 1,
                       conf.type = "log-log")
```

### Plot the survival model

Calling plot() on the fitted survival model, surv.fitted, produces a plot similar to the lower panels in Motulsky's figure 5.1.


```r
plot(surv.fitted.Motulsky)
```

<img src="04-Ch5_survival_data_files/figure-html/unnamed-chunk-21-1.png" width="672" />

### R's default survival model

R's default setting produce a slightly similar output where the upper 95% confidence interval equals 1 for the entire curve

```r
surv.fitted.deafult <- survfit(surv.object ~ 1)
plot(surv.fitted.deafult)
```

<img src="04-Ch5_survival_data_files/figure-html/unnamed-chunk-22-1.png" width="672" />

surfit() takes two arguements that can impact the shape of the curve and 95%CIs, "conf.type" as noted above and "type".  What these different setting do is beyond the scope of this tutorial.  See ?survfit.formula for full details.  (Please note, however, that adjusting the setting simply to adjust the confidence interval to suit your hypothesis would be a form of [p-hacking](https://en.wikipedia.org/wiki/Data_dredging))

<br>

## Plot survival curve with annotation

* We can add the location of the "censored" individuals using the "mark.time = T"" arguement.  
*  "mark = ..."" defines the plotting symbol.  16 happens to be a filled circle.

This plot matches the lower left-hand plot in Motulsky's figure 5.1.

```r
#mark censored individuals
plot(surv.fitted.deafult, 
     mark.time = T, 
     mark = 16)
```

<img src="04-Ch5_survival_data_files/figure-html/unnamed-chunk-23-1.png" width="672" />

<br>

## Beyond Kaplan-Meir survival curves (not complete)

In Chapter 29 (2nd edition) Motulsky elaborates on survival analyses by discuss to test if two survival curves are significantly different using a [log-rank test](https://en.wikipedia.org/wiki/Survival_analysis#Log-rank_test:_Testing_for_differences_in_survival_in_the_aml_data).  In Chapter 29 he uses a different dataset thatn Chapter 5.  Here, we'll briefly introduce this method by modifying our current data in our dat.tab2 object.

First, we'll make a duplicate of the data


```r
dat.tab2.duplicate <- dat.tab2
```

Then we'll change the data slightly by adding some random noise with the rnorm() comamnd. we'll take the origina "year" column and add a random number drawn from a normal distribution with mean 0 and an SD of 3

```r
dat.tab2.duplicate$year <- dat.tab2.duplicate$year + rnorm(n = 7,mean = 0, sd = 2)
```


Now change the fates randomly to 0 or 1, biased slightly towards 1

```r
dat.tab2.duplicate$fate.01 <- rbinom(n = 7,prob = 0.6,size = 1)
```

Now add an arbitrary group label to the original and new data

```r
dat.tab2$group <- 1
dat.tab2.duplicate$group <- 2
```



```r
dat.tab.new <- rbind(dat.tab2.duplicate,dat.tab2)
```



```r
surv.object.new <- Surv(time = dat.tab.new$year,
     event = dat.tab.new$fate.01)

coxph.group <-survfit(surv.object.new~group, data = dat.tab.new)

coxph.group <-coxph(surv.object.new~group, data = dat.tab.new)

summary(coxph.group)
```

```
## Call:
## coxph(formula = surv.object.new ~ group, data = dat.tab.new)
## 
##   n= 14, number of events= 7 
## 
##          coef exp(coef) se(coef)      z Pr(>|z|)
## group -0.2050    0.8146   0.8763 -0.234    0.815
## 
##       exp(coef) exp(-coef) lower .95 upper .95
## group    0.8146      1.228    0.1462     4.538
## 
## Concordance= 0.479  (se = 0.121 )
## Rsquare= 0.004   (max possible= 0.837 )
## Likelihood ratio test= 0.06  on 1 df,   p=0.8132
## Wald test            = 0.05  on 1 df,   p=0.815
## Score (logrank) test = 0.05  on 1 df,   p=0.8147
```


