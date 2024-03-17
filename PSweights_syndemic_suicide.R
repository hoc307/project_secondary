#Syndemic DAG
#This code is for the creation of the DAG for the syndemic analysis
#The code is written in R and uses the dagitty package

#A1: import data from cvs merginging----------------------------
#install.packages('dplyr')
#install.packages("tidyverse")
#install.packages("lme4")
#install.packages("Matrix")
#install.packages("flexplot")
#install.packages("lubridate")
#install.packages("reshape2")
library(Matrix)
library(lme4)
library(lubridate)
library(readxl)
library(reshape2)
library(tidyverse)
library(dplyr)
x0 <- read_excel("/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Secondary/Data/0_all_data_Secondary_(baseline-4m-8m-12m) .xlsx", 
                 sheet = "baseline")
x0$time<- 1
x0_a <- read_excel("/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Secondary/Data/resolved files for q1_9, q1_10, q1_11, and q1_14 .xlsx", 
                   sheet = "baseline")
x0_a <- x0_a %>%
  rename("q1_9x"  = "q1_9",
         "q1_10x" = "q1_10",
         "q1_11x" = "q1_11",
         "q1_14x" = "q1_14")
x0<- merge(x=x0,y=x0_a, by="no.", all.x=TRUE) 
x0_b <- read_excel("/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Secondary/Data/resolved files for q7_19 and q7_20.xlsx", 
                   sheet = "baseline")
x0_b <- x0_b %>%
  rename("q7_19_1x"  = "q7_19_1",
         "q7_19_2x" = "q7_19_2",
         "q7_19_3x" = "q7_19_3",
         "q7_20_1x"  = "q7_20_1",
         "q7_20_2x" = "q7_20_2",
         "q7_20_3x" = "q7_20_3")
x0<- merge(x=x0,y=x0_b,by="no.",all.x=TRUE)
x1 <- read_excel("/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Secondary/Data/0_all_data_Secondary_(baseline-4m-8m-12m) .xlsx", 
                 sheet = "4-months")
x1$time<- 2
x1_a <- read_excel("/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Secondary/Data/resolved files for q1_9, q1_10, q1_11, and q1_14 .xlsx", 
                   sheet = "4-months")
x1_a <- x1_a %>%
  rename("q1_9x"  = "q1_9",
         "q1_10x" = "q1_10",
         "q1_11x" = "q1_11",
         "q1_14x" = "q1_14")

x1<-merge(x=x1,y=x1_a,by="no.", all.x=TRUE)

x1_b <- read_excel("/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Secondary/Data/resolved files for q7_19 and q7_20.xlsx", 
                   sheet = "4-months")

x1_b <- x1_b %>%
  rename("q7_19_1x"  = "q7_19_1",
         "q7_19_2x" = "q7_19_2",
         "q7_19_3x" = "q7_19_3",
         "q7_20_1x"  = "q7_20_1",
         "q7_20_2x" = "q7_20_2",
         "q7_20_3x" = "q7_20_3")
x1<-merge(x=x1,y=x1_b,by="no.",all.x=TRUE)

x2 <- read_excel("/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Secondary/Data/0_all_data_Secondary_(baseline-4m-8m-12m) .xlsx", 
                 sheet = "8-months")
x2$time<- 3
x2_a <- read_excel("/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Secondary/Data/resolved files for q1_9, q1_10, q1_11, and q1_14 .xlsx", 
                   sheet = "8-months")

x2_a <- x2_a %>%
  rename("q1_9x"  = "q1_9",
         "q1_10x" = "q1_10",
         "q1_11x" = "q1_11",
         "q1_14x" = "q1_14")

x2<-merge(x=x2,y=x2_a,by="no.",all.x=TRUE)

x2_b <- read_excel("/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Secondary/Data/resolved files for q7_19 and q7_20.xlsx", sheet = "8-months")
x2_b <- x2_b %>%
  rename("q7_19_1x"  = "q7_19_1",
         "q7_19_2x" = "q7_19_2",
         "q7_19_3x" = "q7_19_3",
         "q7_20_1x"  = "q7_20_1",
         "q7_20_2x" = "q7_20_2",
         "q7_20_3x" = "q7_20_3")

x2<-merge(x=x2,y=x2_b,by="no.",all.x=TRUE)


x3 <- read_excel("/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Secondary/Data/0_all_data_Secondary_(baseline-4m-8m-12m) .xlsx", 
                 sheet = "12-months")
x3$time<-4 
x3$suicide_total_score<-x3$sum_suicide
x3_a <- read_excel("/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Secondary/Data/resolved files for q1_9, q1_10, q1_11, and q1_14 .xlsx", 
                   sheet = "12-months")
x3_a <- x3_a %>%
  rename("q1_9x"  = "q1_9",
         "q1_10x" = "q1_10",
         "q1_11x" = "q1_11",
         "q1_14x" = "q1_14")
x3<- merge(x=x3,y=x3_a,by="no.",all.x=TRUE)

x3_b <- read_excel("/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Secondary/Data/resolved files for q7_19 and q7_20.xlsx", 
                   sheet = "12-months")
x3_b <- x3_b %>%
  rename("q7_19_1x"  = "q7_19_1",
         "q7_19_2x" = "q7_19_2",
         "q7_19_3x" = "q7_19_3",
         "q7_20_1x"  = "q7_20_1",
         "q7_20_2x" = "q7_20_2",
         "q7_20_3x" = "q7_20_3")

x3<-merge(x=x3,y=x3_b,by="no.",all.x=TRUE)

fastmerge <- function(d1, d2) {
  d1.names <- names(d1)
  d2.names <- names(d2)
  
  # columns in d1 but not in d2
  d2.add <- setdiff(d1.names, d2.names)
  
  # columns in d2 but not in d1
  d1.add <- setdiff(d2.names, d1.names)
  
  # add blank columns to d2
  if(length(d2.add) > 0) {
    for(i in 1:length(d2.add)) {
      d2[d2.add[i]] <- NA
    }
  }
  
  # add blank columns to d1
  if(length(d1.add) > 0) {
    for(i in 1:length(d1.add)) {
      d1[d1.add[i]] <- NA
    }
  }
  
  return(rbind(d1, d2))
}
x<-fastmerge(x0,x1)
x<-fastmerge(x,x2)
x<-fastmerge(x,x3)


x$age_cat<-ifelse(x$q1_1<10,"26_29",
                  ifelse(x$q1_1>=10,"15_25",NA))

x$edu_cat<- ifelse(x$q1_11x<=2,"Secondorybelow",
                   ifelse(x$q1_11x>=3,"Tertiaryorabove",NA))
table(x$q1_11x, useNA="always")
x$student<- ifelse(x$q1_10x<=3,"Secondorybelow",
                   ifelse(x$q1_10x>=4,"Tertiaryorabove",NA))
table(x$q1_10x, useNA="always")
x$edu_cat1<-ifelse(is.na(x$edu_cat),x$student,x$edu_cat)
table(x$q1_11x,x$q1_10x, useNA="always")

x$employ<-ifelse(x$q1_12==0,"Unemployed",
                 ifelse(x$q1_12==1,"Full_time",
                        ifelse(x$q1_12==2,"Part_time",NA)))
table(x$employ, x$time, useNA="always")

x$housing<-ifelse(x$q1_13==1,"Home",
                  ifelse(x$q1_13==2,"Dormitory",
                         ifelse(x$q1_13==3|x$q1_13==4,"Rentingwithothers",
                                ifelse(x$q1_13==5,"Rentingalone",
                                       ifelse(x$q1_13>=6,"Temporaryorother",NA)))))
table(x$housing, x$time, useNA="always")
table(x$q1_14x, x$time,useNA="always")
x$income_cat<-ifelse(x$q1_14x<=2,"5000bhatorbelow",
                     ifelse(x$q1_14x>=3 & x$q1_14x<=4,"5001_15000bhat",
                            ifelse(x$q1_14x>=5,"15001bhatorabove",NA)))
x$income_cat<-factor(x$income_cat,levels=c("15001bhatorabove","5001_15000bhat","5000bhatorbelow"))


x$adhere<-ifelse(x$q2_20>=90,0,
                 ifelse(x$q2_20<90,1,11))


library(gmodels)

library(dplyr)
#subsetting participants to taking ART


table(x$q2_12)
x$reg_partner<-ifelse(x$q2_12==0 | x$q2_12==2 |x$q2_12==3,"No",
                      ifelse(x$q2_12==1,"Yes",NA))

x$HIVstigma_total<- x$q5_5_q5_5_1 + x$q5_5_q5_5_2 + x$q5_5_q5_5_3 + x$q5_5_q5_5_4 + x$q5_5_q5_5_5 + x$q5_5_q5_5_6 +  x$q5_5_q5_5_7 + x$q5_5_q5_5_8 + x$q5_5_q5_5_9 + x$q5_5_q5_5_10 + x$q5_5_q5_5_11 + x$q5_5_q5_5_12

summary(x$HIVstigma_total)
x$HIVstigma_cat<-ifelse(x$HIVstigma_total>=11,1,
                        ifelse(x$HIVstigma_total<11,0,NA))
table(x$HIVstigma_cat, x$time, useNA="always")


x$socialsupport_total <- x$q6_9_q6_9_1 + x$q6_9_q6_9_2 + x$q6_9_q6_9_3 + x$q6_9_q6_9_4 
+ x$q6_9_q6_9_5 + x$q6_9_q6_9_6 + x$q6_9_q6_9_7 +x$q6_9_q6_9_8 + x$q6_9_q6_9_9
+ x$q6_9_q6_9_10 + x$q6_9_q6_9_11 + x$q6_9_q6_9_12
summary(x$socialsupport_total)
x$social_cat<-ifelse(x$socialsupport_total>=12,1,
                     ifelse(x$socialsupport_total<12,0,NA))
table(x$social_cat, x$time, useNA="always")



x$amphetamine<-ifelse( x$q8_1_q8_1_9>=1 | x$q8_1_q8_1_10>=1 | x$q8_1_q8_1_16>=1,1,
                       ifelse(x$q8_1_q8_1_9==0 & x$q8_1_q8_1_10==0 & x$q8_1_q8_1_16==0 
                              ,0,NA))


x$bingedrink<-ifelse(x$q8_2>=3,1,
                     ifelse(x$q8_2<3,0,NA))

table(x$q8_1_q8_1_2, x$time, useNA="always") # nolint
x$drink <- ifelse(x$q8_1_q8_1_2 == 0, 0, NA)

x$bingedrink1 <- ifelse(is.na(x$bingedrink), x$drink, x$bingedrink)

table(x$bingedrink1, x$time, useNA = "always")


x$ipv<- ifelse(x$q10_1_q10_1_1>=1|x$q10_1_q10_1_2>=1|x$q10_1_q10_1_3>=1| # nolint # nolint: infix_spaces_linter.
                 x$q10_1_q10_1_4>=1,1,
               ifelse(x$q10_1_q10_1_1==0 & x$q10_1_q10_1_2==0 & x$q10_1_q10_1_3==0 &
                        x$q10_1_q10_1_4==0,0,NA))
table(x$ipv, x$time, useNA="always")

x$ipva<- ifelse(x$q10_1_q10_1_1>=2|x$q10_1_q10_1_2>=2|x$q10_1_q10_1_3>=2|
                  x$q10_1_q10_1_4>=2,1,
                ifelse(x$q10_1_q10_1_1<=1 & x$q10_1_q10_1_2<=1 & x$q10_1_q10_1_3<=10 &
                         x$q10_1_q10_1_4<=1  ,0,NA))


table(x$ipva, x$time, useNA="always")
table(x$q10_2)
x$bullied<-ifelse(x$q10_2==1,1,
                  ifelse(x$q10_2==0,0,NA))
table(x$bullied, x$time, useNA="always")
x$bullied_gay<-x$q10_4
table(x$bullied_gay, x$time, useNA="always")
x$syndemic<- x$depression + x$amphetamine + x$bingedrink1 + x$ipva + x$bullied_gay + x$suicide 

summary(x$syndemic)
table(x$syndemic)

x$syndemic_cat<-factor(x$syndemic,levels=c("1","2","3","4","5","6"))

table(x$syndemic_cat)
x$syndemic_group<-ifelse(x$syndemic==0,"0",
                         ifelse(x$syndemic==1,"1",
                                ifelse(x$syndemic==2,"2",
                                       ifelse(x$syndemic==3,"3",
                                              ifelse(x$syndemic>=4,"4+",NA)))))
table(x$syndemic_group)
CrossTable(x$syndemic_group, x$time)

#creating LTFU variable

x$LTFU<- ifelse(is.na(x$depression),1,
                ifelse(x$depression==1 | x$depression==0,0,NA))
table(x$LTFU, x$time, useNA="always")

#subsetting to those who started ART

xa<-x%>%
  filter(x$q2_17==1 | is.na(x$q2_16))

y0<-x%>%
  filter(time==1)

#conduct latent class analysis for syndemic suicide
library(poLCA)
library(lme4)
#install.packages("clubSandwich")
#install.packages("interactionR")
library(interactionR)
library(clubSandwich)
library(reshape2)
library(dplyr)
library(ggplot2)
#SET THE dataset to baseline online from secondary project
# syndemic variables:  
# y0$HIVstigma_mean  y0$ipva  y0$bullied_gay  y0$depression  y0$amphetamine  y0$bingedrink1
# covariates are the following variables y0$age_cat y0$edu_cat1 y0$income_cat y0$employ y0$identity  y0$hivtime  y0$sexwork_provide y0$groupsex y0$reg_partner 
# recode social supprot as a sum score
# 
y0$syndemic_score<- y0$HIVstigma_cat + y0$ipva + y0$bullied_gay + y0$depression + y0$amphetamine + y0$bingedrink1
summary(y0$syndemic_score)
y0$syndemic_cat<- ifelse(y0$syndemic_score==0,"0",
                         ifelse(y0$syndemic_score==1,"1",
                                ifelse(y0$syndemic_score==2,"2",
                                       ifelse(y0$syndemic_score==3,"3",
                                              ifelse(y0$syndemic_score>=4,"4+",NA)))))

y0$syndemic_cat1<- ifelse(y0$syndemic_score<=1,"0-1",
                          ifelse(y0$syndemic_score==2,"2",
                                 ifelse(y0$syndemic_score==3,"3",
                                        ifelse(y0$syndemic_score>=4,"4+",NA))))

#recode y0$q11_1  for >=3 then v0$attempt =1 and <3 then y0$attempt =0
y0$attetmpt<-ifelse(y0$q11_1>=3 | y0$q11_3>=2 ,1,0)
library(gmodels)
#always useNA="always" to include NA in the table and show percentage
CrossTable(y0$attetmpt)
table(y0$attetmpt, useNA="always")
#make a summed variable y0$ideation to add y0$q11_1 to y0$q11_4 but for y0$q11_2 and y0$q11_3 -1 from each variable to avoid double counting
y0$suicidality<- y0$q11_1 + y0$q11_2 + y0$q11_3 + y0$q11_4
summary(y0$suicidality)
y0$suicide_total<- y0$q11_1 + y0$q11_2 + y0$q11_3 + y0$q11_4
y0$ideation<-ifelse(y0$suicidality>3 | y0$attetmpt==1,1,0)
table(y0$ideation, useNA="always")
library(gmodels)
summary(y0$ideation)
hist(y0$ideation)
#assess the difference between y0suicidality and y0suicide_total
y0$identity<-ifelse(y0$q1_2==1 | y0$q1_2==6,"Gay",
                    ifelse(y0$q1_2==3 |y0$q1_2==5,"Bisexual",NA))
table(y0$identity, useNA="always")

table(y0$q1_1_5)
y0$hivtime <- ifelse(y0$q1_1_5<=13,"lessthanayear",
                     ifelse(y0$q1_1_5>13,"morethanayear",NA))

y0$hivtime2 <- ifelse(y0$q1_1_5<=7,1,
                      ifelse(y0$q1_1_5>7,0,NA))
y0$hivtime3 <- ifelse(y0$q1_1_5<=4,1,
                      ifelse(y0$q1_1_5>4,0,NA))

table(y0$hivtime, useNA="always")
table(y0$hivtime2, useNA="always")
table(y0$hivtime3, useNA="always")
table(y0$q7_8)
table(y0$hivtime,useNA="always")
y0$sexwork_provide<-ifelse(y0$q7_8==1 | y0$q7_8==3,"Yes",
                           ifelse(y0$q7_8==0 | y0$q7_8==2, "No",NA))
table(y0$sexwork_provide, useNA="always")

table(y0$q7_7)
y0$groupsex <- ifelse(y0$q7_7>=1,"Yes",
                      ifelse(y0$q7_7==0,"No",NA))
table(y0$groupsex, useNA="always")
#install.packages("ltm")
library(ltm)




#check coviariates 
# table(y0$age_cat,useNA="always")  
# table(y0$edu_cat1,useNA="always")
# table(y0$income_cat,useNA="always")
# table(y0$employ,useNA="always")
# check the identity variable
#     table(y0$identity,useNA="always")
#     table(y0$hivtime,useNA="always")
#     table(y0$sexwork_provide,useNA="always")
#     table(y0$groupsex,useNA="always")
#     table(y0$reg_partner,useNA="always")
# create dummy variables for covariates so each variable using fastDummies package
library(fastDummies)
library(dplyr) 
colnames(y0)
y0$age_cat
y0$edu_cat1
y0$identity
y0$employ
y0$hivtime
y0$sexwork_provide
y0$reg_partner
depression_alpha<-data.frame(y0$q9_1_q9_1_1, y0$q9_1_q9_1_2, y0$q9_1_q9_1_3, y0$q9_1_q9_1_4,
                             rev(y0$q9_1_q9_1_5), y0$q9_1_q9_1_6, y0$q9_1_q9_1_7, rev(y0$q9_1_q9_1_8),
                             y0$q9_1_q9_1_9, y0$q9_1_q9_1_10)
cronbach.alpha(depression_alpha)
#install.packages("psych")
library(psych)
#install.packages("irr")
library(irr)
icc(depression_alpha, model= "twoway", type="agreement", unit="single")
HIV_stigma_alpha<-data.frame(y0$q5_5_q5_5_1, y0$q5_5_q5_5_2, y0$q5_5_q5_5_3, y0$q5_5_q5_5_4, y0$q5_5_q5_5_5,y0$q5_5_q5_5_6, y0$q5_5_q5_5_7, y0$q5_5_q5_5_8, y0$q5_5_q5_5_9, y0$q5_5_q5_5_10,
                             y0$q5_5_q5_5_11, y0$q5_5_q5_5_12)
cronbach.alpha(HIV_stigma_alpha)
icc(HIV_stigma_alpha, model="twoway",type="agreement", unit="single")

suicide_alpha <- data.frame(y0$q11_1,y0$q11_2,y0$q11_3,y0$q11_4)
cronbach.alpha(suicide_alpha)
y0$socialsupport_total <- y0$q6_9_q6_9_1 + y0$q6_9_q6_9_2 + y0$q6_9_q6_9_3 + y0$q6_9_q6_9_4 + y0$q6_9_q6_9_5 + y0$q6_9_q6_9_6+ y0$q6_9_q6_9_7 +y0$q6_9_q6_9_8 + y0$q6_9_q6_9_9 + y0$q6_9_q6_9_10 + y0$q6_9_q6_9_11 + y0$q6_9_q6_9_12
#center mean for y0$socialsupport_total
summary(y0$socialsupport_total)
y0$socialsupport_cen<-scale(y0$socialsupport_total, center = TRUE, scale = FALSE)
summary(y0$socialsupport_total, useNA="always")
y0$socialsupport_mean<-ifelse(y0$socialsupport_total<=37.99,1,0)
y0$socialsupport_1q<-ifelse(y0$socialsupport_total<=35.25,1,0)
table(y0$socialsupport_1q)

y0$suicidality<- y0$q11_1 + y0$q11_2 + y0$q11_3 + y0$q11_4
summary(y0$suicidality)
y0$suicide_total<- y0$q11_1 + y0$q11_2 + y0$q11_3 + y0$q11_4

#assess the difference between y0suicidality and y0suicide_total
summary(y0$suicidality) 
summary(y0$suicide_total)

x$suicidality<- x$q11_1 + x$q11_2 + x$q11_3 + x$q11_4
table(x$suicidality,x$time, useNA="always")
x$HIVstigma_total<- x$q5_5_q5_5_1 + x$q5_5_q5_5_2 + x$q5_5_q5_5_3 + x$q5_5_q5_5_4 + x$q5_5_q5_5_5 + x$q5_5_q5_5_6 +  x$q5_5_q5_5_7 + x$q5_5_q5_5_8 + x$q5_5_q5_5_9 + x$q5_5_q5_5_10 + x$q5_5_q5_5_11 + x$q5_5_q5_5_12

summary(x$HIVstigma_total)
y0$HIVstigma_cat<-ifelse(y0$HIVstigma_total>=37.99,1,0)
y0$HIVstigma_total<- y0$q5_5_q5_5_1 + y0$q5_5_q5_5_2 + y0$q5_5_q5_5_3 + y0$q5_5_q5_5_4 + y0$q5_5_q5_5_5 + y0$q5_5_q5_5_6 +  y0$q5_5_q5_5_7 + y0$q5_5_q5_5_8 + y0$q5_5_q5_5_9 + y0$q5_5_q5_5_10 + y0$q5_5_q5_5_11 + y0$q5_5_q5_5_12
y0$HIVstigma_mean<-ifelse(y0$HIVstigma_total>=37.99,1,0)
summary(y0$HIVstigma_total)

#evaluate the means of suicidality by syndemic variables
# create codes for means suicidality of all syndemic variables 
#code sum score for depression 
#
# Reverse coding
y0$q9_1_q9_1_5r <- max(y0$q9_1_q9_1_5) + 1 - y0$q9_1_q9_1_5
y0$q9_1_q9_1_8r <- max(y0$q9_1_q9_1_8) + 1 - y0$q9_1_q9_1_8

# Calculate sum score
y0$depress_sum <- rowSums(y0[,c("q9_1_q9_1_1", "q9_1_q9_1_2", "q9_1_q9_1_3", "q9_1_q9_1_4", "q9_1_q9_1_5r", "q9_1_q9_1_6", "q9_1_q9_1_7", "q9_1_q9_1_8r", "q9_1_q9_1_9", "q9_1_q9_1_10")], na.rm = TRUE)
summary(y0$depress_sum)
hist(y0$depress_sum)
y0$depress_sum_cen<-y0$depress_sum-mean(y0$depress_sum,na.rm=T)
# recode the following variables into new variables so each variable only has value 0=1 and 1=2  for y0$HIVstigma_mean  y0$ipva  y0$bullied_gay  y0$depression  y0$amphetamine  y0$bingedrink1
table(y0$HIVstigma_mean)
table(y0$ipva)
table(y0$bullied_gay)
table(y0$depression)
table(y0$amphetamine)

y0<-dummy_cols(y0,select_columns = c("age_cat","edu_cat1","gedu","income_cat","employ","identity","hivtime","sexwork_provide","groupsex","reg_partner"),remove_selected_columns = FALSE)
# recode the dummy variables into new variables so each variable only has value 0=1 and 1=2
# 
# Recode variables  
y0$age_cat_26_29_1 <- ifelse(y0$age_cat_26_29 == 0, 1, 2)
y0$edu_cat1_Secondaryorbelow_1 <- ifelse(y0$edu_cat1_Secondorybelow == 0, 1, 2)
y0$income_cat_5001_15000bhat_1 <- ifelse(y0$income_cat_5001_15000bhat == 0, 1, 2)
y0$income_cat_5000bhatorbelow_1 <- ifelse(y0$income_cat_5000bhatorbelow == 0, 1, 2)
y0$income_low_1 <- ifelse(y0$income_cat_15001bhatorabove == 0, 2, 1)
y0$employ_Part_time_1 <- ifelse(y0$employ_Part_time == 0, 1, 2)
y0$employ_Unemployed_1 <- ifelse(y0$employ_Unemployed == 0, 1, 2)
y0$hivtime_lessthanayear_1 <- ifelse(y0$hivtime_lessthanayear == 0, 1, 2)
y0$sexwork_provide_Yes_1 <- ifelse(y0$sexwork_provide_Yes == 0, 1, 2)
y0$reg_partner_Yes_1 <- ifelse(y0$reg_partner_Yes == 0, 1, 2)
y0$HIVstigma_median <- ifelse(y0$HIVstigma_mean == 0, 1, 2)
y0$HIVstigma_median_1 <- ifelse(y0$HIVstigma_mean == 0, 1, 2)
y0$socialsupport_mean_1 <- ifelse(y0$socialsupport_mean == 0, 1, 2)
y0$ipva_1 <- ifelse(y0$ipva == 0, 1, 2)
y0$bullied_gay_1 <- ifelse(y0$bullied_gay == 0, 1, 2)
y0$bullied_gen_1 <- ifelse(y0$q10_2 == 0, 1, 2)
y0$depression_1 <- ifelse(y0$depression == 0, 1, 2)
y0$amphetamine_1 <- ifelse(y0$amphetamine == 0, 1, 2)
y0$bingedrink1_1 <- ifelse(y0$bingedrink1 == 0, 1, 2)
y0$socialsupport_1q_1 <- ifelse(y0$socialsupport_1q == 0, 1, 2)
y0$age_cat_26_29_1<-recode(y0$age_cat_26_29, `0` = 1, `1` = 2)
y0$edu_cat1_Secondaryorbelow_1 <- recode(y0$edu_cat1_Secondorybelow, `0` = 1, `1` = 2)
y0$income_cat_5001_15000bhat_1 <- recode(y0$income_cat_5001_15000bhat, `0` = 1, `1` = 2)
y0$income_cat_5000bhatorbelow_1 <- recode(y0$income_cat_5000bhatorbelow, `0` = 1, `1` = 2)
y0$income_low_1<-recode(y0$income_cat_15001bhatorabove, `0` = 2, `1` = 1)
y0$employ_Part_time_1 <- recode(y0$employ_Part_time, `0` = 1, `1` = 2)
y0$employ_Unemployed_1 <- recode(y0$employ_Unemployed, `0` = 1, `1` = 2)
y0$identity_Bisexual_1 <- recode(y0$identity_Bisexual, `0` = 1, `1` = 2)
y0$hivtime_lessthanayear_1 <- recode(y0$hivtime_lessthanayear, `0` = 1, `1` = 2)
y0$sexwork_provide_Yes_1 <- recode(y0$sexwork_provide_Yes, `0` = 1, `1` = 2)
y0$groupsex_Yes_1 <- recode(y0$groupsex_Yes, `0` = 1, `1` = 2)
y0$reg_partner_Yes_1 <- recode(y0$reg_partner_Yes, `0` = 1, `1` = 2)
y0$HIVstigma_median <- recode(y0$HIVstigma_mean, `0` = 1, `1` = 2)
y0$HIVstigma_median_1 <- recode(y0$HIVstigma_mean, `0` = 1, `1` = 2)
y0$socialsupport_mean_1 <- recode(y0$socialsupport_mean, `0` = 1, `1` = 2)
y0$ipva_1 <- recode(y0$ipva, `0` = 1, `1` = 2)
y0$bullied_gay_1 <- as.numeric(recode(y0$bullied_gay, `0` = 1, `1` = 2))
y0$bullied_gen_1[y0$q10_2==0] <- 1
y0$bullied_gen_1[y0$q10_2==1] <- 2
table(y0$bullied_gen_1,useNA="always")
y0$depression_1 <- recode(y0$depression, `0` = 1, `1` = 2)
y0$amphetamine_1 <- recode(y0$amphetamine, `0` = 1, `1` = 2)
y0$bingedrink1_1 <- recode(y0$bingedrink1, `0` = 1, `1` = 2)
y0$socialsupport_1q_1 <- recode(y0$socialsupport_1q, `0` = 1, `1` = 2)
y0$HIVstigma_median_0<-ifelse(y0$HIVstigma_median==2,1,0)
y0$q2_5_1
y0$hivtime_0<-ifelse(y0$hivtime=="lessthanayear",1,0)
y0$hivtime_0<-as.numeric(y0$hivtime_0)
table(x$q1_16)
y0$gedu<-ifelse(y0$q1_16<=4 ,1,0)
y0$gedu_1<-ifelse(y0$q1_16<=4,2,1)
y0$gedu3<-ifelse(y0$q1_16<=3 ,1,0)
y0$gedu_3<-ifelse(y0$q1_16<=3,2,1)
y0$gedu2<-ifelse(y0$q1_16<=2 ,1,0)
y0$gedu_2<-ifelse(y0$q1_16<=2,2,1)
y0$geedu_0<-ifelse(y0$q1_16<=3 & y0$edu_cat1_Secondaryorbelow_1==2,1,0)
y0$gedu_cat<-ifelse(y0$q1_16<=2,1,
                    ifelse(y0$q1_16>2 & y0$q1_16<5,2,
                           ifelse(y0$q1_16>=5,3,NA)))

table(y0$HIVstigma_median_0)
y0$hivtime_1<-ifelse(y0$hivtime=="morethanayear",1,0)
y0$edu_cat_0<-ifelse(y0$edu_cat1_Tertiaryorabove,1,0)
y0$age_cat_0<-ifelse(y0$age_cat_26_29,1,0)
y0$edu_cat_1<-ifelse(y0$edu_cat1_Secondorybelow==1,1,0)

table(y0$gedu_3)
means_by_com1 <- aggregate(suicidality ~ edu_cat1 + gedu_cat+ sexwork_provide_Yes_1 + lcaf3e , data = y0, FUN = function(x) c(Mean = mean(x, na.rm = TRUE), Count = sum(!is.na(x))))
means_by_com1 <- aggregate(suicidality ~ edu_cat1 + gedu_cat + lcaf3e + lcaf3a + lcaf3c, data = y0, FUN = function(x) mean(x, na.rm = TRUE))

table(y0$gedu_2)


table(y0$geedu_0)


#B1: Make table 1-----------------------------------------

categorical_predictors <- c("age_cat", "edu_cat1",  "employ","income_cat", "identity","reg_partner","hivtime",  "sexwork_provide" ,"groupsex_Yes_1",  "bullied_gay_1", "bullied_gen_0", "ipva_1","HIVstigma_median", "amphetamine_1","bingedrink1_1","depression", "socialsupport_1q_1")

# Initialize a data frame to store the results
results_t1 <- data.frame()

# Predictors to reverse the order of their levels
reverse_order_predictors <- c("age_cat", "edu_cat1", "income_cat", "reg_partner")

# Calculate means, standard deviations, and perform Wilcoxon tests or ANOVAs
for (predictor in categorical_predictors) {
  levels <- unique(y0[[predictor]])
  
  # Reverse the order of the levels for certain predictors
  if (predictor %in% reverse_order_predictors) {
    levels <- rev(levels)
  }
  
  for (level in levels) {
    n <- sum(y0[[predictor]] == level, na.rm = TRUE)
    percent <- n / nrow(y0) * 100
    mean <- mean(y0$suicidality[y0[[predictor]] == level], na.rm = TRUE)
    sd <- sd(y0$suicidality[y0[[predictor]] == level], na.rm = TRUE)
 mean2<-mean(y0$suicidality_t4[y0[[predictor]] == level], na.rm = TRUE) 
   sd2<-sd(y0$suicidality_t4[y0[[predictor]] == level], na.rm = TRUE) 
    if (length(unique(y0[[predictor]])) == 2) {
      test <- wilcox.test(y0$suicidality ~ y0[[predictor]], exact = FALSE)
      w_value <- test$statistic
      p_value <- round(test$p.value, 4)
    } else {
      test <- summary(aov(y0$suicidality ~ y0[[predictor]]))
      w_value <- test[[1]]["F value"][1,]
      p_value <- round(test[[1]]["Pr(>F)"][1,], 4)
    }
   if (length(unique(y0[[predictor]])) == 2) {
     test2 <- wilcox.test(y0$suicidality_t4 ~ y0[[predictor]], exact = FALSE)
     w_value2 <- test2$statistic
     p_value2 <- round(test2$p.value, 4)
   } else {
     test2 <- summary(aov(y0$suicidality_t4 ~ y0[[predictor]])) # Corrected from suicidality_4 to suicidality_t4
     w_value2 <- test2[[1]]["F value"][1,]
     p_value2 <- round(test2[[1]]["Pr(>F)"][1,], 4)
   }
    # Store the results in the data frame
    results_t1 <- rbind(results_t1, data.frame(Predictor = predictor, Level = level, N = n, Percent = percent, Mean = mean, SD = sd, W_Value = w_value, P_Value = p_value, Mean2=mean2,SD2=sd2,W_Value2=w, P_Value2=p_value2))
  }
}

# Print the results
print(results_t1)

# Write the results to a CSV file
write.csv(results_t1, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/results_t1.csv", row.names = TRUE)

mean(y0$HIVstigma_total)
SD(y0$HIVstigma_total)
mean(y0$depress_sum)
SD(y0$depress_sum)
mean(y0$socialsupport_total)
SD(y0$socialsupport_total)

#B2: Make corplots--------------------------

# Select the binary variables
binary_vars <- y0[c("bullied_gay_1", "bullied_gen_1", "ipva_1","HIVstigma_median","socialsupport_1q_1", "amphetamine_1","bingedrink1_1","sexwork_provide_Yes_1","income_low_1","depression","suicidality")]

# Compute the correlation matrix
cor_matrix <- cor(binary_vars, method = "pearson")

# Print the correlation matrix
print(cor_matrix)

# Install the corrplot package if not already installed
if (!require(corrplot)) {
  install.packages("corrplot")
}
library(corrplot)
# Define the new labels
new_labels <- c("Homophobic bullying", "General bullying", "Intimate partner violence", "Internalized HIV Stigma","Low social support", "Amphetamine use", "Binge drinking","Sex work", "Low income", "Depression","Suicidality")

# Compute the correlation matrix
cor_matrix <- cor(binary_vars, method = "pearson")

# Rename the column names of the correlation matrix
colnames(cor_matrix) <- new_labels
rownames(cor_matrix) <- new_labels

# Define a greyscale color palette
greyscale_palette <- colorRampPalette(c("white", "grey"))

par(pin = c(5, 5))  # Adjust these values to make the plot bigger

# Generate the correlation plot in greyscale with text labels on the y-axis
corrplot::corrplot(cor_matrix, method = "color", type = "lower", 
                   order = "hclust", addCoef.col = "black", 
                   tl.pos = "ld", col = greyscale_palette(200),
                   number.cex = 0.6,  # Adjust this value to make the numbers smaller
                   cl.pos = 'b', cl.align = 'l',  # Add a color legend at the bottom left
                   tl.cex = 0.7,  # Adjust this value to make the variable names smaller
                   tl.srt = 90)  # Rotate the text labels 90 degrees


# Set the plot dimensions
par(pin = c(5, 5))  # Adjust these values to make the plot bigger

# Generate the correlation plot in greyscale with text labels on the y-axis
corrplot::corrplot(cor_matrix, method = "color", type = "lower", 
                   order = "hclust", addCoef.col = "black", 
                   tl.pos = "ld", col = greyscale_palette(200),
                   number.cex = 0.6,  # Adjust this value to make the numbers smaller
                   cl.pos = 'b', cl.align = 'l',  # Add a color legend at the bottom left
                   tl.cex = 0.70,  # Adjust this value to make the variable names smaller
                   tl.srt = 90,  # Rotate the text labels 90 degrees
                   tl.col = "black",  # Make the variable names black
                   tl.font = 2)  # Make the variable names bolded


# Add rectangles around the correlation coefficients

  # Lines 595 to 599 have been deleted


# Add text labels on the left side of the plot
text(x = rep(-0.1, nrow(cor_matrix)), y = seq(from = 1, to = 0, length.out = nrow(cor_matrix)), 
     labels = rownames(cor_matrix), srt = 90, adj = c(1, 0.5), xpd = TRUE)
# Define the new labels
new_labels <- c("Homophobic bullying", "General bullying", "Intimate partner violence", "Internalized HIV Stigma","Low social support", "Amphetamine use", "Binge drinking","Sex work", "Low income", "Depression","Suicidality")

# Perform hierarchical clustering
hc <- hclust(dist(cor_matrix))

# Plot the dendrogram with the new labels
plot(hc, labels = new_labels)

#C1: runnning the LCA models----------------------------
#model 3a with SAVA 
f3a<-as.formula(cbind(bullied_gay_1,bullied_gen_1,ipva_1,amphetamine_1,bingedrink1_1)~ 1+suicidality + hivtime +  sexwork_provide, socialsupport_1q_1 + HIVstigma_median) 

#model 3a to include SAVA + HVI stigma and low social support
f3c<-as.formula(cbind(HIVstigma_median,bullied_gay_1,bullied_gen_1,ipva_1,amphetamine_1,bingedrink1_1,socialsupport_1q_1)~ 1 +suicidality +  sexwork_provide + hivtime ) 

#model 3a to include sexwork and low income
f3e<-as.formula(cbind(bullied_gay_1,bullied_gen_1,ipva_1,amphetamine_1,bingedrink1_1,HIVstigma_median,socialsupport_1q_1, sexwork_provide_Yes_1, income_low_1)~ 1 +suicidality+ age_cat + reg_partner  +identity + hivtime + depression)

library(poLCA)
set.seed(123)
lca_f3a<-poLCA(f3a, data=y0,nclass=3, maxiter=10000,nrep=50)
set.seed(123)
lca_f3c<-poLCA(f3c, data=y0,nclass=3, maxiter=10000,nrep=50)
#LCA analysis for 3a
set.seed(123)
lca_f3e<-poLCA(f3e, data=y0,nclass=3, maxiter=10000,nrep=50)

#C2: print item response category probabilities for LCA models 3a, 3c, and 3a-------
# Define a function to extract probabilities and class sizes
extract_probs <- function(lca_model) {
  # Extract conditional item response probabilities
  prob_list <- lca_model$probs
  # Convert list to data frame
  prob_df <- do.call(rbind, prob_list)
  # Transpose the data frame so that each class is a column and each variable is a row
  prob_df <- t(prob_df)
  # Extract Pr(2) for each class and each variable
  class1_pr2 <- sprintf("%.3f", prob_df[2, seq(1, ncol(prob_df), 3)])
  class2_pr2 <- sprintf("%.3f", prob_df[2, seq(2, ncol(prob_df), 3)])
  class3_pr2 <- sprintf("%.3f", prob_df[2, seq(3, ncol(prob_df), 3)])
  # Calculate class sizes and percentages
  class_sizes <- table(lca_model$predclass)
  class_percentages <- class_sizes / sum(class_sizes) * 100
  # Create a data frame
  final_df <- data.frame(
    "Class 1: Pr(2)" = c(sprintf("N = %d, %.1f%%", class_sizes[1], class_percentages[1]), class1_pr2),
    "Class 2: Pr(2)" = c(sprintf("N = %d, %.1f%%", class_sizes[2], class_percentages[2]), class2_pr2),
    "Class 3: Pr(2)" = c(sprintf("N = %d, %.1f%%", class_sizes[3], class_percentages[3]), class3_pr2)
  )
  # Set row names
  row.names(final_df) <- c("Class Size", names(lca_model$probs))
  # Return the data frame
  return(final_df)
}
# Create a list of models
lca_models <- list(lca_f3a, lca_f3c, lca_f3a)
# Initialize an empty list to store the results
results_posterior_f3 <- list()
# Loop over the models
for (i in seq_along(lca_models)) {
  # Apply the function to each model and store the result
  results_posterior_f3[[i]] <- extract_probs(lca_models[[i]])
}


# Combine the data frames into a single data frame
results_posterior_f3_df <- do.call(rbind, lapply(seq_along(results_posterior_f3), function(i) {
  cbind(Model = paste0("Model", i), results_posterior_f3[[i]])
}))
print(results_posterior_f3_df)
# Write the data frame to a CSV file
write.csv(results_posterior_f3_df, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/results_posterior_f3.csv", row.names = TRUE)

#C3:CLass renumeration and fit stats -----------------------------
lca_re <- function(fit) {
  # fit: A model object returned by poLCA
  
  # Calculate the entropy
  entropy <- 0
  for (i in 1:nrow(fit$posterior)) {
    for (j in 1:ncol(fit$posterior)) {
      # Avoid log(0) by ensuring the posterior probability is non-zero
      if (fit$posterior[i, j] > 0) {
        entropy <- entropy - fit$posterior[i, j] * log(fit$posterior[i, j])
      }
    }
  }
  # Normalize entropy by dividing by the total number of observations and classes
  normalized_entropy <- entropy / (nrow(fit$posterior) * ncol(fit$posterior))
  
  # Return the relative entropy
  return(normalized_entropy)
}


#conduct LCA for class enumeration for f3a
out_lca <- list()   # container of model fittings
npar <- ll <- bic <- abic <- caic <- awe <- icl<- re  <-  lmr_lrt_p <-smallest_class_prop <- c() # containers
set.seed(123)
for(k in 1:6){
  fit <- poLCA(formula=f3a, data=y0, nclass=k, maxiter=10000,
               tol=1e-5, nrep=20, verbose=F, calc.se=T)
  out_lca[[k]] <- fit
  npar[k] <- fit$npar
  ll[k]   <- fit$llik
  bic[k]  <- fit$bic
  abic[k] <- -2*(fit$llik) + fit$npar*log((fit$Nobs+2)/24)
  caic[k] <- -2*(fit$llik) + fit$npar*(log(fit$Nobs)+1)
  awe[k]  <- -2*(fit$llik) + 2*(fit$npar)*(log(fit$Nobs)+1.5)
  re[k]   <- round(lca_re(fit), 3)
  # Calculate the proportion of the smallest class
  smallest_class_prop[k] <- min(fit$P) / sum(fit$P)
  # LMR-LRT
  if(k > 1){
    lmr_lrt_stat <- -2 * (ll[k-1] - ll[k])
    lmr_lrt_p[k] <- sprintf("%.4f", 1 - pchisq(lmr_lrt_stat, df = 1))
  } else {
    lmr_lrt_p[k] <- NA
  }
}

class <- paste0("Class-", 1:6)
# Store information in a data frame
syndemiclcafits.f3a <- data.frame("Class"=class, "Npar"=npar, "LL"=ll,
                                  "BIC"=bic, "aBIC"=abic, "CAIC"=caic, "AWE"=awe, "RE"=re, "LMR-LRT_pvalue"=lmr_lrt_p,"Smallest_Class_Prop"=smallest_class_prop)
write.csv(syndemiclcafits.f3a, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/syndemiclcafits.f3a.csv")

#conduct LCA for class enumeration for f3a
out_lca <- list()   # container of model fittings
npar <- ll <- bic <- abic <- caic <- awe <- re  <-  lmr_lrt_p <-smallest_class_prop <- c() # containers
set.seed(123)
for(k in 1:6){
  fit <- poLCA(formula=f3a, data=y0, nclass=k, maxiter=10000,
               tol=1e-5, nrep=20, verbose=F, calc.se=T)
  out_lca[[k]] <- fit
  npar[k] <- fit$npar
  ll[k]   <- fit$llik
  bic[k]  <- fit$bic
  abic[k] <- -2*(fit$llik) + fit$npar*log((fit$Nobs+2)/24)
  caic[k] <- -2*(fit$llik) + fit$npar*(log(fit$Nobs)+1)
  awe[k]  <- -2*(fit$llik) + 2*(fit$npar)*(log(fit$Nobs)+1.5)
  re[k]   <- round(lca_re(fit), 3)
  # Calculate the proportion of the smallest class
  smallest_class_prop[k] <- min(fit$P) / sum(fit$P)
  # LMR-LRT
  if(k > 1){
    lmr_lrt_stat <- -2 * (ll[k-1] - ll[k])
    lmr_lrt_p[k] <- sprintf("%.4f", 1 - pchisq(lmr_lrt_stat, df = 1))
  } else {
    lmr_lrt_p[k] <- NA
  }
}

class <- paste0("Class-", 1:6)
# Store information in a data frame
syndemiclcafits.f3a <- data.frame("Class"=class, "Npar"=npar, "LL"=ll,
                                  "BIC"=bic, "aBIC"=abic, "CAIC"=caic, "AWE"=awe, "RE"=re, "LMR-LRT_pvalue"=lmr_lrt_p,"Smallest_Class_Prop"=smallest_class_prop)
write.csv(syndemiclcafits.f3a, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/syndemiclcafits.f3a.csv")


#conduct LCA for class enumeration for f3a
out_lca <- list()   # container of model fittings
npar <- ll <- bic <- abic <- caic <- awe <- re  <-  lmr_lrt_p <-smallest_class_prop <- c() # containers
set.seed(123)
for(k in 1:6){
  fit <- poLCA(formula=f3a, data=y0, nclass=k, maxiter=10000,
               tol=1e-5, nrep=20, verbose=F, calc.se=T)
  out_lca[[k]] <- fit
  npar[k] <- fit$npar
  ll[k]   <- fit$llik
  bic[k]  <- fit$bic
  abic[k] <- -2*(fit$llik) + fit$npar*log((fit$Nobs+2)/24)
  caic[k] <- -2*(fit$llik) + fit$npar*(log(fit$Nobs)+1)
  awe[k]  <- -2*(fit$llik) + 2*(fit$npar)*(log(fit$Nobs)+1.5)
  re[k]   <- round(lca_re(fit), 3)
  # Calculate the proportion of the smallest class
  smallest_class_prop[k] <- min(fit$P) / sum(fit$P)
  # LMR-LRT
  if(k > 1){
    lmr_lrt_stat <- -2 * (ll[k-1] - ll[k])
    lmr_lrt_p[k] <- sprintf("%.4f", 1 - pchisq(lmr_lrt_stat, df = 1))
  } else {
    lmr_lrt_p[k] <- NA
  }
}

class <- paste0("Class-", 1:6)
# Store information in a data frame
syndemiclcafits.f3a <- data.frame("Class"=class, "Npar"=npar, "LL"=ll,
                                  "BIC"=bic, "aBIC"=abic, "CAIC"=caic, "AWE"=awe, "RE"=re, "LMR-LRT_pvalue"=lmr_lrt_p,"Smallest_Class_Prop"=smallest_class_prop)
write.csv(syndemiclcafits.f3a, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/syndemiclcafits.f3a.csv")



#C4: Running LCA model 3a--------------------------
library(nnet)
library(sjPlot)
library(sjmisc)
library(gtsummary)
#install.packages("PSweight")
library(PSweight) 
# Load the ggplot2 library
library(ggplot2)
# Load the dplyr library
library(dplyr)
#IPW for f3a
#Change CLASS------- 
y0$lcaf3a <- factor(lca_f3a$predclass, levels = c(2, 3, 1))
lca_f3a$posterior1 <- lca_f3a$posterior[, c(2, 3, 1)]
y0$lcaf3c <- factor(lca_f3c$predclass, levels = c(2, 1, 3))
lca_f3c$posterior1 <- lca_f3c$posterior[, c(2, 1, 3)]
y0$lcaf3e <- factor(lca_f3e$predclass, levels = c(2, 3, 1))
lca_f3e$posterior1 <- lca_f3e$posterior[, c(2, 3, 1)]





# Convert data frame to matrix
y0 <- as.matrix(y0)
# Convert matrix back to data frame
y0 <- as.data.frame(y0)


library(PSweight)
library(geepack)
#set up the formula with minimal sufficient adjustment sets
psform1_f3a<-as.formula("lcaf3a~ 1 + age_cat + edu_cat1 + employ + income_cat+reg_partner+ sexwork_provide  + HIVstigma_median_0")
# Section 1b LCA model 3a weight  ---------------------
# Calculate summary statistics using the data frame

ssps1f3a_ow <- SumStat(psform1_f3a, method = "glm", weight="overlap", delta=0,data = y0)
ssps1f3a_ipw <- SumStat(psform1_f3a, method = "glm", weight="IPW", ,delta=0,data = y0)
y0_trim<-PStrim(y0, ps.formula = psform1_f3a, zname = NULL, ps.estimate = NULL,
                  delta = 0.065, optimal = FALSE, method = "glm", ps.control = list())
summary(y0_trim$trim_sum)
#trim at 0.065 and 0.935 for IPW
ssps1f3a_ow.smd<-summary(ssps1f3a_ow, weighted.var = TRUE, metric = "ASD")
ssps1f3a_ipw.smd<-summary(ssps1f3a_ipw, weighted.var = TRUE, metric = "ASD")
plot(ssps1f3a_ipw,type="density")
plot(ssps1f3a_ow,type="density")
# Extract the overlap weights and unweighted SMDs from the SumStat object
ssps1f3a_ow.smd_weighted_df<-as.data.frame(ssps1f3a_ow.smd$overlap)
ssps1f3a_ow.smd_unweighted_df<-as.data.frame(ssps1f3a_ow.smd$unweighted)
ssps1f3a_ipw.smd_weighted_df<-as.data.frame(ssps1f3a_ipw.smd$IPW)
ssps1f3a_ipw.smd_unweighted_df<-as.data.frame(ssps1f3a_ipw.smd$unweighted)
# Extract propensity score weights from the SumStat object
ps1f3a_ipw.w <- ssps1f3a_ipw$ps.weights$IPW
ps1f3a_ow.w <- ssps1f3a_ow$ps.weights$overlap
# Create a new data frame that contains the overlap weights and the SMDs
y0$ps1f3a_ipw.w<-ps1f3a_ipw.w
y0$ps1f3a_ow.w<-ps1f3a_ow.w

#Minimal sufficient adjustment sets for estimating the total effect of Depression on Suicidality Age, Amphetamine, Education, Employment, General bullying, Homophobic bullying, IPV, Income, LCA-SAVA, Sex work

psform2_f3a<-as.formula("depression~ 1 + age_cat + edu_cat1 + employ + income_cat + bullied_gen_1 + bullied_gay + ipva + sexwork_provide  + lcaf3a")

ssps2f3a_ow <- SumStat(psform2_f3a, method = "glm", weight="overlap", ,delta=0,data = y0)
ssps2f3a_ipw <- SumStat(psform2_f3a, method = "glm", weight="IPW", ,delta=0,data = y0)
ssps2f3a_ow.smd<-summary(ssps2f3a_ow, weighted.var = TRUE, metric = "ASD")
ssps2f3a_ipw.smd<-summary(ssps2f3a_ipw, weighted.var = TRUE, metric = "ASD")
plot(ssps2f3a_ipw,type="density")
plot(ssps2f3a_ow,type="density")
# Extract the overlap weights and unweighted SMDs from the SumStat object
ssps2f3a_ow.smd_weighted_df<-as.data.frame(ssps2f3a_ow.smd$overlap)
ssps2f3a_ow.smd_unweighted_df<-as.data.frame(ssps2f3a_ow.smd$unweighted)
ssps2f3a_ipw.smd_weighted_df<-as.data.frame(ssps2f3a_ipw.smd$IPW)
ssps2f3a_ipw.smd_unweighted_df<-as.data.frame(ssps2f3a_ipw.smd$unweighted)
# Extract propensity score weights from the SumStat object
ps2f3a_ipw.w <- ssps2f3a_ipw$ps.weights$IPW
ps2f3a_ow.w <- ssps2f3a_ow$ps.weights$overlap
# Create a new data frame that contains the overlap weights and the SMDs
y0$ps2f3a_ipw.w<-ps2f3a_ipw.w
y0$ps2f3a_ow.w<-ps2f3a_ow.w
# Section 1c LCA model 3a combine cross-sectional weight  ---------------------
# Calculate the propensity scores = gamma by case * post probs
#Method 1 combine posterior directly with IPW 
#cps12p_f3a_ow.w ----- 
cps12p_f3a_ow.w <- rowSums(ps1f3a_ow.w*ps2f3a_ow.w*lca_f3a$posterior)
cps12p_f3a_ipw.w <- rowSums(ps1f3a_ipw.w*ps2f3a_ipw.w*lca_f3a$posterior)
y0$cps12p_f3a_ow.w<-cps12p_f3a_ow.w
y0$cps12p_f3a_ipw.w<-cps12p_f3a_ipw.w
library(ggplot2)
# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3a
# # Make all column names unique

library(ggplot2)
ggplot(data = y0, aes(x = as.factor(lcaf3a), y = cps12p_f3a_ipw.w)) +
geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
 labs(title = "Overlap Weights by Latent Class Model 1 - SAVA Syndemic", x = "Latent Class", y = "Overlap Weights")

# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3a
ggplot(data = y0, aes(x = as.factor(lcaf3a), y = cps12p_f3a_ow.w)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "IPW by Latent Class Model 1 - SAVA Syndemic", x = "Latent Class", y = "IPS Weights")


#trimmming the weight
#install.packages("datawizard")
library(datawizard)

trim_weights <- function(weights, lower_pct, upper_pct){
  
  # Calculate percentile thresholds
  lower <- quantile(weights, lower_pct, na.rm=TRUE) 
  upper <- quantile(weights, upper_pct, na.rm=TRUE)
  
  # Winsorize weights
  winsorized <- pmax(weights, lower)
  winsorized <- pmin(winsorized, upper)
  
  return(winsorized)
  
}
# Apply trimming to cps345p_f3a_ipw.w
y0$cps12p_f3a_ipw.wtm <- trim_weights(y0$cps12p_f3a_ipw.w, 0.02, 0.98)

# cps345p_f3a_ow.wtm --------
# Apply trimming to cps345p_f3a_ow.w
y0$cps12p_f3a_ow.wtm <- trim_weights(y0$cps12p_f3a_ow.w, 0.02, 0.98)



# Density plot for IP weights in greyscale
ggplot(data = y0, aes(x = cps12p_f3a_ow.wtm, fill = lcaf3a)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Inverse Propensity Score Weights by LCA Model 1 - SAVA Syndemic", x = "IPS Weights", y = "Density") +
  scale_fill_grey()  # Set fill colors to shades of grey

# Density plot for overlap weights in greyscale
ggplot(data = y0, aes(x = cps12p_f3a_ipw.wtm, fill = lcaf3a)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Overlap Weights by LCA Model 1 - SAVA Syndemic", x = "Overlap Weights", y = "Density") +
  scale_fill_grey()  # Set fill colors to shades of grey
#fitting the GEE model with interaction
library(geepack)
y0$suicidality<-as.numeric(y0$suicidality)
y0$suicidality_t4<-as.numeric(y0$suicidality_t4)
geemf3a_ow <- geeglm(suicidality ~ lcaf3a * depression, data = y0, id = lcaf3a, weights = cps12p_f3a_ow.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3a_ow)

# Fit a simpler GEE model without the interaction term
geemf3a0_ow <- geeglm(suicidality ~ lcaf3a + depression, data = y0, id = lcaf3a, weights = cps12p_f3a_ow.w, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3a0_ipw)

# Fit a GEE model with interaction using the overlap weights
geemf3a_ipw <- geeglm(suicidality ~ lcaf3a * depression, data = y0, id = lcaf3a, weights = cps12p_f3a_ipw.w, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3a_ow)

# Fit a simpler GEE model without the interaction term
geemf3a0_ipw<- geeglm(suicidality ~ lcaf3a + depression, data = y0, id = lcaf3a, weights = cps12p_f3a_ipw.w, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3a0_ow)
tab_model(geemf3a_ipw,geemf3a_ow)
anova(geemf3a0_ow, geemf3a_ow, test="score")
y0$suicidality<-as.numeric(y0$suicidality)
#Minimal sufficient adjustment sets for estimating the total effect of LTFU on Suicidality at T4: Amphetamine, Education, Guardian's Education, LCA-SAVA, Sex work, Suicidality, Time from HIV diagnosis
#set up the formula with minimal sufficient adjustment sets
psform3_f3a<-as.formula("ltfu~ 1 + amphetamine + edu_cat1 + gedu + lcaf3a + sexwork_provide  +suicidality+ hivtime_0")
# Calculate summary statistics using the data frame
ssps3f3a_ow <- SumStat(psform3_f3a, method = "glm", weight="overlap", ,delta=0,data = y0)
ssps3f3a_ipw <- SumStat(psform3_f3a, method = "glm", weight="IPW", ,delta=0,data = y0)
y0_trim<-PStrim(y0, ps.formula = psform3_f3a, zname = NULL, ps.estimate = NULL,
                  delta = 0.000000005, optimal = FALSE, method = "glm", ps.control = list())
#trim at 0.000000005 percentile
summary(y0_trim$trim_sum)
#trimm at 0.001 and 0.999 for IPW
ssps3f3a_ow.smd<-summary(ssps3f3a_ow, weighted.var = TRUE, metric = "ASD")
ssps3f3a_ipw.smd<-summary(ssps3f3a_ipw, weighted.var = TRUE, metric = "ASD")
plot(ssps3f3a_ipw,type="density")
plot(ssps3f3a_ow,type="density")
# Extract the overlap weights and unweighted SMDs from the SumStat object
ssps3f3a_ow.smd_weighted_df<-as.data.frame(ssps3f3a_ow.smd$overlap)
ssps3f3a_ow.smd_unweighted_df<-as.data.frame(ssps3f3a_ow.smd$unweighted)
ssps3f3a_ipw.smd_weighted_df<-as.data.frame(ssps3f3a_ipw.smd$IPW)
ssps3f3a_ipw.smd_unweighted_df<-as.data.frame(ssps3f3a_ipw.smd$unweighted)
# Extract propensity score weights from the SumStat object
ps3f3a_ipw.w <- ssps3f3a_ipw$ps.weights$IPW
ps3f3a_ow.w <- ssps3f3a_ow$ps.weights$overlap


# Create a new data frame that contains the overlap weights and the SMDs

y0$ps3f3a_ipw.w<-ps3f3a_ipw.w
y0$ps3f3a_ow.w<-ps3f3a_ow.w
#PS 4 3a 
#Minimal sufficient adjustment sets for estimating the total effect of LCA-SAVA on Suicidality at T4: 
#1. Age, Education, Employment, Guardian's Education, HIV stigma, Regular partner, Sex work, Time from HIV diagnosis
# 2. Education, Employment, HIV stigma, Income, Regular partner, Sex work, Time from HIV diagnosis
psform4_f3a<-as.formula("lcaf3a~ 1 + age_cat + edu_cat1 + employ +reg_partner+ sexwork_provide  + HIVstigma_median_0 + hivtime_0")
# Calculate summary statistics using the data frame
ssps4f3a_ow <- SumStat(psform4_f3a, method = "glm", weight="overlap", ,delta=0,data = y0)
ssps4f3a_ipw <- SumStat(psform4_f3a, method = "glm", weight="IPW", ,delta=0,data = y0)
y04_trim<-PStrim(y0, ps.formula = psform1_f3a, zname = NULL, ps.estimate = NULL,
                  delta = 0.1, optimal = FALSE, method = "glm", ps.control = list())
summary(y0_trim$trim_sum)
#trim at 0.065 and 0.935 for IPW
ssps4f3a_ow.smd<-summary(ssps4f3a_ow, weighted.var = TRUE, metric = "ASD")
ssps4f3a_ipw.smd<-summary(ssps4f3a_ipw, weighted.var = TRUE, metric = "ASD")
plot(ssps4f3a_ipw,type="density")
plot(ssps4f3a_ow,type="density")
# Extract the overlap weights and unweighted SMDs from the SumStat object
ssps4f3a_ow.smd_weighted_df<-as.data.frame(ssps4f3a_ow.smd$overlap)
ssps4f3a_ow.smd_unweighted_df<-as.data.frame(ssps4f3a_ow.smd$unweighted)
ssps4f3a_ipw.smd_weighted_df<-as.data.frame(ssps4f3a_ipw.smd$IPW)
ssps4f3a_ipw.smd_unweighted_df<-as.data.frame(ssps4f3a_ipw.smd$unweighted)
# Extract propensity score weights from the SumStat object
ps4f3a_ipw.w <- ssps4f3a_ipw$ps.weights$IPW
ps4f3a_ow.w <- ssps4f3a_ow$ps.weights$overlap
# Create a new data frame that contains the overlap weights and the SMDs
y0$ps4f3a_ipw.w<-ps4f3a_ipw.w
y0$ps4f3a_ow.w<-ps4f3a_ow.w
y0$ps4f3a_ipw.w<-ps4f3a_ipw.w
y0$ps4f3a_ow.w<-ps4f3a_ow.w
#minimial sufficient set for PS5-3a
##3.Education, General bullying, HIV stigma, Homophobic bullying, IPV, LCA-SAVA
psform5_f3a<-as.formula("depression~ 1 +edu_cat1 + bullied_gen_1 + bullied_gay + lcaf3a + ipva +  HIVstigma_median_0")
# Calculate summary weight with PSweight package 
ssps5f3a_ow <- SumStat(psform5_f3a, method = "glm", weight="overlap", ,delta=0,data = y0)
ssps5f3a_ipw <- SumStat(psform5_f3a, method = "glm", weight="IPW", ,delta=0,data = y0)
y05_trim<-PStrim(y0, ps.formula = psform5_f3a, zname = NULL, ps.estimate = NULL,
                   delta = 0.15, optimal = FALSE, method = "glm", ps.control = list())
summary(y0_trim$trim_sum) 
#trim at 0.15  for IPW
ssps5f3a_ow.smd<-summary(ssps5f3a_ow, weighted.var = TRUE, metric = "ASD")
ssps5f3a_ipw.smd<-summary(ssps5f3a_ipw, weighted.var = TRUE, metric = "ASD")
plot(ssps5f3a_ipw,type="density")
plot(ssps5f3a_ow,type="density")
# Extract the overlap weights and unweighted SMDs from the SumStat object
ssps5f3a_ow.smd_weighted_df<-as.data.frame(ssps5f3a_ow.smd$overlap)
ssps5f3a_ow.smd_unweighted_df<-as.data.frame(ssps5f3a_ow.smd$unweighted)
ssps5f3a_ipw.smd_weighted_df<-as.data.frame(ssps5f3a_ipw.smd$IPW)
ssps5f3a_ipw.smd_unweighted_df<-as.data.frame(ssps5f3a_ipw.smd$unweighted)
# Extract propensity score weights from the SumStat object
ps5f3a_ipw.w <- ssps5f3a_ipw$ps.weights$IPW
ps5f3a_ow.w <- ssps5f3a_ow$ps.weights$overlap
# Create a new data frame that contains the overlap weights and the SMDs
# Section 1d LCA model 3a combine T4 weight  ---------------------
# combine propensity scores = gamma by case * post probs
cps345p_f3a_ipw.w <- rowSums(ps3f3a_ipw.w*ps4f3a_ipw.w*ps5f3a_ipw.w*lca_f3a$posterior)
length(ps3f3a_ipw.w)
length(ps4f3a_ipw.w)
length(ps5f3a_ipw.w)
length(y0$lcaf3a_posterior)
cps345p_f3a_ow.w <- rowSums(ps3f3a_ow.w*ps4f3a_ow.w*ps5f3a_ow.w*lca_f3a$posterior)
y0$cps345p_f3a_ipw.w <-cps345p_f3a_ipw.w 
y0$cps345p_f3a_ow.w<-cps345p_f3a_ow.w
y0$cps345p_f3a_ipw.w <-cps345p_f3a_ipw.w 
y0$cps345p_f3a_ow.w<-cps345p_f3a_ow.w
summary(y0$cps345p_f3a_ipw.w)
summary(y0$cps345p_f3a_ow.w)
# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3a
ggplot(data = y0, aes(x = as.factor(lcaf3a), y = cps345p_f3a_ow.w)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "Overlap Weights by Latent Class Model 1 (t4) - SAVA Syndemic", x = "Latent Class", y = "Overlap Weights")
# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3a
ggplot(data = y0, aes(x = as.factor(lcaf3a), y = cps345p_f3a_ipw.w)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "IPW by Latent Class Model 1(t4) - SAVA Syndemic", x = "Latent Class", y = "IPS Weights")
library(ggplot2)
# Density plot for IP weights in greyscale
ggplot(data =  y0, aes(x = cps345p_f3a_ipw.w, fill = lcaf3a)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Inverse Propensity Score Weights by LCA Model 1 (t4)- SAVA Syndemic", x = "IPS Weights", y = "Density") +
  scale_fill_grey()  # Set fill colors to shades of grey

# Density plot for overlap weights in greyscale
ggplot(data = y0, aes(x = cps345p_f3a_ow.w, fill = lcaf3a)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Overlap Weights by LCA Model 1(t4) - SAVA Syndemic", x = "Overlap Weights", y = "Density") +scale_fill_grey()  # Set fill colors to shades of grey



# Function to trim weights
trim_weights1 <- function(weights, lower_bound, upper_bound) {
  p <- quantile(weights, probs = c(lower_bound, upper_bound), na.rm = TRUE)
  trimmed_weights <- ifelse(weights < p[1], p[1], ifelse(weights > p[2], p[2], weights))
  return(trimmed_weights)
}

# Define trimming thresholds
lower_bound <- 0.025  # Example lower bound
upper_bound <- 0.975  # Example upper bound
#install.packages("datawizard")
library(datawizard)

trim_weights <- function(weights, lower_pct, upper_pct){
  
  # Calculate percentile thresholds
  lower <- quantile(weights, lower_pct, na.rm=TRUE) 
  upper <- quantile(weights, upper_pct, na.rm=TRUE)
  
  # Winsorize weights
  winsorized <- pmax(weights, lower)
  winsorized <- pmin(winsorized, upper)
  
  return(winsorized)
  
}
# Apply trimming to cps345p_f3a_ipw.w
y0$cps345p_f3a_ipw.wtm <- trim_weights(y0$cps345p_f3a_ipw.w, 0.05, 0.95)

# cps345p_f3a_ow.wtm --------
# Apply trimming to cps345p_f3a_ow.w
y0$cps345p_f3a_ow.wtm <- trim_weights(y0$cps345p_f3a_ow.w, 0.05, 0.95)



# Summarize the trimmed weights
summary(y0$cps345p_f3a_ipw.w)
summary(y0$cps345p_f3a_ipw.wtm)
summary(y0$cps345p_f3a_ow.w)
summary(y0$cps345p_f3a_ow.wtm)

# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3a
ggplot(data = y0, aes(x = as.factor(lcaf3a), y = cps345p_f3a_ow.wtm)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "Overlap Weights by Latent Class Model 1 (t4) - SAVA Syndemic", x = "Latent Class", y = "Overlap Weights")
# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3a
ggplot(data = y0, aes(x = as.factor(lcaf3a), y = cps345p_f3a_ipw.wtm)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "IPW by Latent Class Model 1(t4) - SAVA Syndemic", x = "Latent Class", y = "IPS Weights")
# Create a boxplot to visualize the weights based on the treatment variable lcaf3a
# Load the ggplot2 library
library(ggplot2)
# Density plot for IP weights in greyscale
ggplot(data =  y0, aes(x = cps345p_f3a_ipw.wtm, fill = lcaf3a)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Inverse Propensity Score Weights by LCA Model 1 (t4)- SAVA Syndemic", x = "IPS Weights", y = "Density") +
  scale_fill_grey()  # Set fill colors to shades of grey

# Density plot for overlap weights in greyscale
ggplot(data = y0, aes(x = cps345p_f3a_ow.wtm, fill = lcaf3a)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Overlap Weights by LCA Model 1(t4) - SAVA Syndemic", x = "Overlap Weights", y = "Density") +scale_fill_grey()  # Set fill colors to shades of grey

# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3a
ggplot(data = y0, aes(x = as.factor(lcaf3a), y = cps12p_f3a_ow.wtm)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "Overlap Weights by Latent Class Model 1 (t4) - SAVA Syndemic", x = "Latent Class", y = "Overlap Weights")
# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3a
ggplot(data = y0, aes(x = as.factor(lcaf3a), y = cps12p_f3a_ipw.wtm)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "IPW by Latent Class Model 1(t4) - SAVA Syndemic", x = "Latent Class", y = "IPS Weights")
# Create a boxplot to visualize the weights based on the treatment variable lcaf3a
# Load the ggplot2 library
library(ggplot2)
# Density plot for IP weights in greyscale
ggplot(data =  y0, aes(x = cps12p_f3a_ipw.wtm, fill = lcaf3a)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Inverse Propensity Score Weights by LCA Model 1 (t4)- SAVA Syndemic", x = "IPS Weights", y = "Density") +
  scale_fill_grey()  # Set fill colors to shades of grey

# Density plot for overlap weights in greyscale
ggplot(data = y0, aes(x = cps12p_f3a_ow.wtm, fill = lcaf3a)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Overlap Weights by LCA Model 1(t4) - SAVA Syndemic", x = "Overlap Weights", y = "Density") +scale_fill_grey()  # Set fill colors to shades of grey

# Create a boxplot to visualize the weights based on the treatment variable lcaf3a
# Load the ggplot2 library
y0$suicidality_t4<-as.numeric(y0$suicidality_t4)
geemf3at_ipwtm <- geeglm(suicidality_t4 ~ lcaf3a * depression, data = y0, id = lcaf3a, weights = cps345p_f3a_ipw.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3at_ipwtm)

y0$suicidality_t4<-as.numeric(y0$suicidality_t4)
geemf3at_ipw <- geeglm(suicidality_t4 ~ lcaf3a * depression, data = y0, id = lcaf3a, weights = cps345p_f3a_ipw.w, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3at_ipw,geemf3at_ipwtm)



# Fit a simpler GEE model without the interaction term
geemf3a0t_ipw <- geeglm(suicidality_t4 ~ lcaf3a + depression, data = y0, id = lcaf3a, weights = cps345p_f3a_ipw.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3a0t_ipw)

# Fit a GEE model with interaction using the overlap weights
geemf3at_owtm <- geeglm(suicidality_t4 ~ lcaf3a * depression, data = y0, id = lcaf3a, weights = cps345p_f3a_ow.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3at_owtm)

geemf3at_ow <- geeglm(suicidality_t4 ~ lcaf3a * depression, data = y0, id = lcaf3a, weights = cps345p_f3a_ow.w, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3at_ow,geemf3at_owtm) # (2.43-2.17)/2.43=11% 

# Fit a simpler GEE model without the interaction term
geemf3a0t_ow<- geeglm(suicidality_t4 ~ lcaf3a + depression, data = y0, id = lcaf3a, weights = cps345p_f3a_ow.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3a0t_ow)
tab_model(geemf3at_ipw,geemf3at_ow)
anova(geemf3a0t_ow, geemf3at_ow, test="score")

# Section 2a LCA model 3b weight  ---------------------

#Constructing weights for LCA model 3c
#Minimal sufficient adjustment set used: 2.Age, Education, Employment, Income, Regular partner, Sex work

psform1_f3c<-as.formula("lcaf3c~ 1 + age_cat + edu_cat1 + employ + income_cat+reg_partner+ sexwork_provide")
# Calculate summary statistics using the data frame
ssps1f3c_ow <- SumStat(psform1_f3c, method = "glm", weight="overlap", delta=0,data = y0)
ssps1f3c_ipw <- SumStat(psform1_f3c, method = "glm", weight="IPW", delta=0,data = y0)
y01_trim_3a<-PStrim(y0, ps.formula = psform1_f3c, zname = NULL, ps.estimate = NULL,
                  delta = 0.13, optimal = FALSE, method = "glm", ps.control = list())
summary(y0_trim$trim_sum)
#trim at 0.13  for IPW
ssps1f3c_ow.smd<-summary(ssps1f3c_ow, weighted.var = TRUE, metric = "ASD")
ssps1f3c_ipw.smd<-summary(ssps1f3c_ipw, weighted.var = TRUE, metric = "ASD")
plot(ssps1f3c_ipw,type="density")
plot(ssps1f3c_ow,type="density")
# Extract the overlap weights and unweighted SMDs from the SumStat object
ssps1f3c_ow.smd_weighted_df<-as.data.frame(ssps1f3c_ow.smd$overlap)
ssps1f3c_ow.smd_unweighted_df<-as.data.frame(ssps1f3c_ow.smd$unweighted)
ssps1f3c_ipw.smd_weighted_df<-as.data.frame(ssps1f3c_ipw.smd$IPW)
ssps1f3c_ipw.smd_unweighted_df<-as.data.frame(ssps1f3c_ipw.smd$unweighted)
# Extract propensity score weights from the SumStat object
ps1f3c_ipw.w <- ssps1f3c_ipw$ps.weights$IPW
ps1f3c_ow.w <- ssps1f3c_ow$ps.weights$overlap
# Create a new data frame that contains the overlap weights and the SMDs
y0$ps1f3c_ipw.w<-ps1f3c_ipw.w
y0$ps1f3c_ow.w<-ps1f3c_ow.w
y0$ps1f3c_ipw.w<-ps1f3c_ipw.w
y0$ps1f3c_ow.w<-ps1f3c_ow.w
#Minimal sufficient adjustment sets for estimating the total effect of Depression on Suicidality: #Amphetamine, General bullying, Homophobic bullying, IPV, LCA-SAVA+SS

psform2_f3c<-as.formula("depression~ 1 + amphetamine + bullied_gen_1 + bullied_gay +  ipva +  lcaf3c")

ssps2f3c_ow <- SumStat(psform2_f3c, method = "glm", weight="overlap", ,delta=0,data = y0)
ssps2f3c_ipw <- SumStat(psform2_f3c, method = "glm", weight="IPW", ,delta=0,data = y0)
ssps2f3c_ow.smd<-summary(ssps2f3c_ow, weighted.var = TRUE, metric = "ASD")
ssps2f3c_ipw.smd<-summary(ssps2f3c_ipw, weighted.var = TRUE, metric = "ASD")
plot(ssps2f3c_ipw,type="density")
plot(ssps2f3c_ow,type="density")
# Extract the overlap weights and unweighted SMDs from the SumStat object
ssps2f3c_ow.smd_weighted_df<-as.data.frame(ssps2f3c_ow.smd$overlap)
ssps2f3c_ow.smd_unweighted_df<-as.data.frame(ssps2f3c_ow.smd$unweighted)
ssps2f3c_ipw.smd_weighted_df<-as.data.frame(ssps2f3c_ipw.smd$IPW)
ssps2f3c_ipw.smd_unweighted_df<-as.data.frame(ssps2f3c_ipw.smd$unweighted)
# Extract propensity score weights from the SumStat object
ps2f3c_ipw.w <- ssps2f3c_ipw$ps.weights$IPW
ps2f3c_ow.w <- ssps2f3c_ow$ps.weights$overlap
# Create a new data frame that contains the overlap weights and the SMDs
y0$ps2f3c_ipw.w<-ps2f3c_ipw.w
y0$ps2f3c_ow.w<-ps2f3c_ow.w
y0$ps2f3c_ipw.w<-ps2f3c_ipw.w
y0$ps2f3c_ow.w<-ps2f3c_ow.w

# Section 2b LCA model 3c combine cross-sectional weight  ---------------------
# Calculate the propensity scores = gamma by case * post probs
cps12p_f3c_ipw.w <- rowSums(ps1f3c_ipw.w*ps2f3c_ipw.w*lca_f3c$posterior)
cps12p_f3c_ow.w <- rowSums(ps1f3c_ow.w*ps2f3c_ow.w*lca_f3c$posterior)
y0$cps12p_f3c_ipw.w<-cps12p_f3c_ipw.w
y0$cps12p_f3c_ow.w<-cps12p_f3c_ow.w
y0$cps12p_f3c_ipw.w<-cps12p_f3c_ipw.w
y0$cps12p_f3c_ow.w<-cps12p_f3c_ow.w

# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3c
ggplot(data = y0, aes(x = as.factor(lcaf3c), y = cps12p_f3c_ow.w)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "Overlap Weights by Latent Class Model 1 - SAVA+SS", x = "Latent Class", y = "Overlap Weights")
# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3c
ggplot(data = y0, aes(x = as.factor(lcaf3c), y = cps12p_f3c_ipw.w)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "IPW by Latent Class Model 2 - SAVA+SS", x = "Latent Class", y = "IPS Weights")
# Create a boxplot to visualize the weights based on the treatment variable lcaf3a
# Load the ggplot2 library
library(ggplot2)
# Density plot for IP weights in greyscale
ggplot(data = y0, aes(x = cps12p_f3c_ipw.w, fill = lcaf3c)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Inverse Propensity Score Weights by LCA Model 2 - SAVA+SS", x = "IPS Weights", y = "Density") +
  scale_fill_grey()  # Set fill colors to shades of grey

# Density plot for overlap weights in greyscale
ggplot(data = y0, aes(x = cps12p_f3c_ow.w, fill = lcaf3c)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Overlap Weights by Model 2-SAVA+SS", x = "Overlap Weights", y = "Density") +
  scale_fill_grey()  # Set fill colors to shades of grey
# Apply trimming to cps345p_f3a_ipw.w
y0$cps12p_f3c_ipw.wtm <- trim_weights(y0$cps12p_f3a_ipw.w, 0.005, 0.995)

# cps345p_f3a_ow.wtm --------
# Apply trimming to cps345p_f3a_ow.w
y0$cps12p_f3c_ow.wtm <- trim_weights(y0$cps12p_f3a_ow.w, 0.005, 0.995)


ggplot(data = y0, aes(x = as.factor(lcaf3c), y = cps12p_f3c_ow.wtm)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "Overlap Weights by Latent Class Model 1 - SAVA+SS", x = "Latent Class", y = "Overlap Weights")
# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3c
ggplot(data = y0, aes(x = as.factor(lcaf3c), y = cps12p_f3c_ipw.wtm)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "IPW by Latent Class Model 2 - SAVA+SS", x = "Latent Class", y = "IPS Weights")
# Create a boxplot to visualize the weights based on the treatment variable lcaf3a
# Load the ggplot2 library
library(ggplot2)
# Density plot for IP weights in greyscale
ggplot(data = y0, aes(x = cps12p_f3c_ipw.wtm, fill = lcaf3c)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Inverse Propensity Score Weights by LCA Model 2 - SAVA+SS", x = "IPS Weights", y = "Density") +
  scale_fill_grey()  # Set fill colors to shades of grey

# Density plot for overlap weights in greyscale
ggplot(data = y0, aes(x = cps12p_f3c_ow.wtm, fill = lcaf3c)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Overlap Weights by Model 2-SAVA+SS", x = "Overlap Weights", y = "Density") +
  scale_fill_grey()  # Set fill colors to shades of grey


#fitting the GEE model with interaction
#S-3c analysis------
library(geepack)
geemf3c_ipw <- geeglm(suicidality ~ lcaf3c * depression, data = y0, id = lcaf3c, weights = cps12p_f3c_ipw.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3c_ipw)


# 
# Fit a simpler GEE model without the interaction term
geemf3c0_ipw <- geeglm(suicidality ~ lcaf3c + depression, data = y0, id = lcaf3c, weights = cps12p_f3c_ipw.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3c0_ipw)

# Fit a GEE model with interaction using the overlap weights
geemf3c_ow <- geeglm(suicidality ~ lcaf3c * depression, data = y0, id = lcaf3c, weights = cps12p_f3c_ow.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3c_ow)

# Fit a simpler GEE model without the interaction term
geemf3c0_ow<- geeglm(suicidality ~ lcaf3c + depression, data = y0, id = lcaf3c, weights = cps12p_f3c_ow.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3c0_ow)
anova(geemf3c0_ow, geemf3c_ow, test="score")


#Minimal sufficient adjustment sets for estimating the total effect of LTFU on Suicidality at T4:Amphetamine, Guardian's Education, Sex work
#set up the formula with minimal sufficient adjustment sets

psform3_f3c<-as.formula("ltfu~ 1 + amphetamine + gedu + sexwork_provide+suicidality")
# Calculate summary statistics using the data frame
ssps3f3c_ow <- SumStat(psform3_f3c, method = "glm", weight="overlap", delta=0,data = y0)
ssps3f3c_ipw <- SumStat(psform3_f3c, method = "glm", weight="IPW", delta=0,data = y0)


summary(y0_trim$trim_sum)
#trimm at 0.000000003 and 0.999 for IPW
ssps3f3c_ow.smd<-summary(ssps3f3c_ow, weighted.var = TRUE, metric = "ASD")
ssps3f3c_ipw.smd<-summary(ssps3f3c_ipw, weighted.var = TRUE, metric = "ASD")
plot(ssps3f3c_ipw,type="density")
plot(ssps3f3c_ow,type="density")
# Extract the overlap weights and unweighted SMDs from the SumStat object
ssps3f3c_ow.smd_weighted_df<-as.data.frame(ssps3f3c_ow.smd$overlap)
ssps3f3c_ow.smd_unweighted_df<-as.data.frame(ssps3f3c_ow.smd$unweighted)
ssps3f3c_ipw.smd_weighted_df<-as.data.frame(ssps3f3c_ipw.smd$IPW)
ssps3f3c_ipw.smd_unweighted_df<-as.data.frame(ssps3f3c_ipw.smd$unweighted)
# Extract propensity score weights from the SumStat object
ps3f3c_ipw.w <- ssps3f3c_ipw$ps.weights$IPW
ps3f3c_ow.w <- ssps3f3c_ow$ps.weights$overlap
# Create a new data frame that contains the overlap weights and the SMDs
y0$ps3f3c_ipw.w<-ps3f3c_ipw.w
y0$ps3f3c_ow.w<-ps3f3c_ow.w
y0$ps3f3c_ipw.w<-ps3f3c_ipw.w
y0$ps3f3c_ow.w<-ps3f3c_ow.w

#PS 4 3c
#Minimal sufficient adjustment sets for estimating the total effect of LCA-SAVA on Suicidality at T4: 
 #2.AAge, Education, Employment, Guardian's Education, Regular partner

psform4_f3c<-as.formula("lcaf3c~ 1 + age_cat + edu_cat1 + employ + gedu + reg_partner")
# Calculate summary statistics using the data frame

ssps4f3c_alt <- SumStat(psform4_f3c, method = "glm", weight=c("IPW","overlap","matching","entropy"), delta=0,data = y0)

ssps4f3c_ow <- SumStat(psform4_f3c, method = "glm", weight="overlap", ,delta=0,data = y0)
ssps4f3c_ipw <- SumStat(psform4_f3c, method = "glm", weight="IPW", ,delta=0,data = y0)

ssps4f3c_ow.smd<-summary(ssps4f3c_ow, weighted.var = TRUE, metric = "ASD")
ssps4f3c_ipw.smd<-summary(ssps4f3c_ipw, weighted.var = TRUE, metric = "ASD")
plot(ssps4f3c_ipw,type="density")
plot(ssps4f3c_ow,type="density")
# Extract the overlap weights and unweighted SMDs from the SumStat object
ssps4f3c_ow.smd_weighted_df<-as.data.frame(ssps4f3c_ow.smd$overlap)
ssps4f3c_ow.smd_unweighted_df<-as.data.frame(ssps4f3c_ow.smd$unweighted)
ssps4f3c_ipw.smd_weighted_df<-as.data.frame(ssps4f3c_ipw.smd$IPW)
ssps4f3c_ipw.smd_unweighted_df<-as.data.frame(ssps4f3c_ipw.smd$unweighted)
# Extract propensity score weights from the SumStat object
ps4f3c_ipw.w <- ssps4f3c_ipw$ps.weights$IPW
ps4f3c_ow.w <- ssps4f3c_ow$ps.weights$overlap

# PS5-3c ------ 
#minimial sufficient set for PS5-3c
#Minimal sufficient adjustment sets for estimating the total effect of Depression on Suicidality at T4:
#Amphetamine, General bullying, Homophobic bullying, IPV, LCA-SAVA+SS
psform5_f3c<-as.formula("depression~ 1 +amphetamine + bullied_gen_1+ bullied_gay + lcaf3c + ipva")
# Calculate summary weight with PSweight package 
ssps5f3c_ow <- SumStat(psform5_f3c, method = "glm", weight="overlap", ,delta=0,data = y0)
ssps5f3c_ipw <- SumStat(psform5_f3c, method = "glm", weight="IPW", ,delta=0,data = y0)
y05_trim<-PStrim(y0, ps.formula = psform5_f3c, zname = NULL, ps.estimate = NULL,
                   delta = 0.05, optimal = FALSE, method = "glm", ps.control = list())
summary(y0_trim$trim_sum)
#trim at 0.065 and 0.935 for IPW
ssps5f3c_ow.smd<-summary(ssps5f3c_ow, weighted.var = TRUE, metric = "ASD")
ssps5f3c_ipw.smd<-summary(ssps5f3c_ipw, weighted.var = TRUE, metric = "ASD")
plot(ssps5f3c_ipw,type="density")
plot(ssps5f3c_ow,type="density")
# Extract the overlap weights and unweighted SMDs from the SumStat object
ssps5f3c_ow.smd_weighted_df<-as.data.frame(ssps5f3c_ow.smd$overlap)
ssps5f3c_ow.smd_unweighted_df<-as.data.frame(ssps5f3c_ow.smd$unweighted)
ssps5f3c_ipw.smd_weighted_df<-as.data.frame(ssps5f3c_ipw.smd$IPW)
ssps5f3c_ipw.smd_unweighted_df<-as.data.frame(ssps5f3c_ipw.smd$unweighted)
# Extract propensity score weights from the SumStat object
ps5f3c_ipw.w <- ssps5f3c_ipw$ps.weights$IPW
ps5f3c_ow.w <- ssps5f3c_ow$ps.weights$overlap
# Create a new data frame that contains the overlap weights and the SMDs
y0$ps5f3c_ipw.w<-ps5f3c_ipw.w
y0$ps5f3c_ow.w<-ps5f3c_ow.w
y0$ps5f3c_ipw.w<-ps5f3c_ipw.w
y0$ps5f3c_ow.w<-ps5f3c_ow.w
 #PS3,Ps4,Ps5 Combine weights  3c ------
# combine propensity scores = gamma by case * post probs
# #cps345p_f3c_ipw.w ------
cps345p_f3c_ipw.w <- rowSums(ps3f3c_ipw.w*ps4f3c_ipw.w*ps5f3c_ipw.w*lca_f3c$posterior)
cps345p_f3c_ow.w <- rowSums(ps3f3c_ow.w*ps4f3c_ow.w*ps5f3c_ow.w*lca_f3c$posterior)

y0$cps345p_f3c_ipw.w <-cps345p_f3c_ipw.w 
y0$cps345p_f3c_ow.w<-cps345p_f3c_ow.w
y0$cps345p_f3c_ipw.w <-cps345p_f3c_ipw.w 
y0$cps345p_f3c_ow.w<-cps345p_f3c_ow.w

summary(y0$cps345p_f3c_ipw.w)
summary(y0$cps345p_f3c_ow.w)



# Define trimming thresholds
lower_bound <- 0.03  # Example lower bound
upper_bound <- 0.97  # Example upper bound
#install.packages("datawizard")
library(datawizard)

trim_weights <- function(weights, lower_pct, upper_pct){
  
  # Calculate percentile thresholds
  lower <- quantile(weights, lower_pct, na.rm=TRUE) 
  upper <- quantile(weights, upper_pct, na.rm=TRUE)
  
  # Winsorize weights
  winsorized <- pmax(weights, lower)
  winsorized <- pmin(winsorized, upper)
  
  return(winsorized)
  
}
# Apply trimming to cps345p_f3c_ipw.w
y0$cps345p_f3c_ipw.wtm <- trim_weights(y0$cps345p_f3c_ipw.w, lower_bound, upper_bound)

# Apply trimming to cps345p_f3c_ow.w
y0$cps345p_f3c_ow.wtm <- trim_weights(y0$cps345p_f3c_ow.w, lower_bound, upper_bound)

# Summarize the trimmed weights
summary(y0$cps345p_f3c_ipw.w)
summary(y0$cps345p_f3c_ipw.wtm)
summary(y0$cps345p_f3c_ow.w)
summary(y0$cps345p_f3c_ow.wtm)
# Apply trimming to cps345p_f3c_ipw.w
y0$cps345p_f3c_ipw.wtm <- trim_weights(y0$cps345p_f3c_ipw.w, lower_bound, upper_bound)

# Apply trimming to cps345p_f3c_ow.w
y0$cps345p_f3c_ow.wtm <- trim_weights(y0$cps345p_f3c_ow.w, lower_bound, upper_bound)
y0$cps345p_f3c_ipw.wtm <- trim_weights(y0$cps345p_f3c_ipw.w, lower_bound, upper_bound)

# Apply trimming to cps345p_f3c_ow.w
y0$cps345p_f3c_ow.wtm <- trim_weights(y0$cps345p_f3c_ow.w, lower_bound, upper_bound)
# Summarize the trimmed weights
summary(y0$cps345p_f3c_ipw.wtm)
summary(y0$cps345p_f3c_ow.wtm)

# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3c
ggplot(data = y0, aes(x = as.factor(lcaf3c), y = cps345p_f3c_ow.wtm)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "Overlap Weights by Latent Class Model 2 (t4) - SAVA+SS", x = "Latent Class", y = "Overlap Weights")
# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3c
ggplot(data = y0, aes(x = as.factor(lcaf3c), y = cps345p_f3c_ipw.wtm)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "IPW by Latent Class Model 1(t4) - SAVA+SS", x = "Latent Class", y = "IPS Weights")
# Create a boxplot to visualize the weights based on the treatment variable lcaf3c
# Load the ggplot2 library
library(ggplot2)
# Density plot for IP weights in greyscale
ggplot(data = y0, aes(x = cps345p_f3c_ipw.wtm, fill = lcaf3c)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Inverse Propensity Score Weights by LCA Model 2 (t4)- SAVA+SS", x = "IPS Weights", y = "Density") +
  scale_fill_grey()  # Set fill colors to shades of grey

# Density plot for overlap weights in greyscale
ggplot(data = y0, aes(x = cps345p_f3c_ow.wtm, fill = lcaf3c)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Overlap Weights by LCA Model 2(t4) - SAVA+SS", x = "Overlap Weights", y = "Density") +scale_fill_grey()  # Set fill colors to shades of grey

geemf3at2_ipwtm<- geeglm(suicidality_t2 ~ lcaf3c * depression , data = y0,  id = lcaf3a, weights = cps345p_f3c_ipw.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3at2_ipwtm)

geemf3at3_ipwtm<- geeglm(suicidality_t3 ~ lcaf3c * depression, data =y0 , id = lcaf3a, weights = cps345p_f3c_ipw.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3at3_ipwtm)

geemf3ct_ipwtm<- geeglm(suicidality_t4 ~ lcaf3c * depression, data =y0 , id = lcaf3c, weights = cps345p_f3c_ipw.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3ct_ipwtm)


geemf3ct_owtm <- geeglm(suicidality_t4 ~ lcaf3c * depression, data = y0 , id = lcaf3c, weights = cps345p_f3c_ow.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3ct_owtm)



tab_model(geemf3at_ow,geemf3at_ipw, geemf3c_ipw,geemf3c_ow)

geemf3c_int4_ipw <- geeglm(suicidality_t4 ~ lcaf3c * depression, data = y0 , id = lcaf3c, weights = ipwt3c, family = gaussian(), corstr = "exchangeable")


tab_model(geemf3ct_ipwtm, geemf3ct_owtm)
tab_model(geemf3at_ipwtm, geemf3at_owtm)

# PS1 LCA model 3e weight  ---------------------
#Constructing weights for LCA model PS1- 3e
# Minimal sufficient adjustment sets containing Age, Education, Guardian's Education, Regular partner, Time from HIV diagnosis for estimating the total effect of LCA-Social Factors on Suicidality:
#set up the formula with minimal sufficient adjustment sets

psform1_f3e<-as.formula("lcaf3e~ 1 + age_cat + edu_cat1 + gedu+ reg_partner + hivtime_1") 
# Calculate summary statistics using the data frame
ssps1f3e_ow <- SumStat(psform1_f3e, method = "glm", weight="overlap", delta=0,data = y0)
ssps1f3e_ipw <- SumStat(psform1_f3e, method = "glm", weight="IPW", delta=0,data = y0)
y0_trim_3e<-PStrim(y0, ps.formula = psform1_f3e, zname = NULL, ps.estimate = NULL,
                     delta = 0.13, optimal = FALSE, method = "glm", ps.control = list())
summary(y0_trim$trim_sum)
#trim at 0.13  for IPW
ssps1f3e_ow.smd<-summary(ssps1f3e_ow, weighted.var = TRUE, metric = "ASD")
ssps1f3e_ipw.smd<-summary(ssps1f3e_ipw, weighted.var = TRUE, metric = "ASD")
plot(ssps1f3e_ipw,type="density")
plot(ssps1f3e_ow,type="density")
# Extract the overlap weights and unweighted SMDs from the SumStat object
ssps1f3e_ow.smd_weighted_df<-as.data.frame(ssps1f3e_ow.smd$overlap)
ssps1f3e_ow.smd_unweighted_df<-as.data.frame(ssps1f3e_ow.smd$unweighted)
ssps1f3e_ipw.smd_weighted_df<-as.data.frame(ssps1f3e_ipw.smd$IPW)
ssps1f3e_ipw.smd_unweighted_df<-as.data.frame(ssps1f3e_ipw.smd$unweighted)
# Extract propensity score weights from the SumStat object
ps1f3e_ipw.w <- ssps1f3e_ipw$ps.weights$IPW
ps1f3e_ow.w <- ssps1f3e_ow$ps.weights$overlap
# Create a new data frame that contains the overlap weights and the SMDs
y0$ps1f3e_ipw.w<-ps1f3e_ipw.w
y0$ps1f3e_ow.w<-ps1f3e_ow.w

#PS2-3e: Minimal sufficient adjustment sets for estimating the total effect of Depression on Suicidality: 
#Amphetamine, General bullying, Homophobic bullying, IPV, Income, LCA-Social Factors

#set up the formula with minimal sufficient adjustment sets
psform2_f3e<-as.formula("depression~ 1 + amphetamine + bullied_gen_1 + bullied_gay +  ipva + income_cat+ lcaf3e")

ssps2f3e_ow <- SumStat(psform2_f3e, method = "glm", weight="overlap", ,delta=0,data = y0)
ssps2f3e_ipw <- SumStat(psform2_f3e, method = "glm", weight="IPW", ,delta=0,data = y0)
ssps2f3e_ow.smd<-summary(ssps2f3e_ow, weighted.var = TRUE, metric = "ASD")
ssps2f3e_ipw.smd<-summary(ssps2f3e_ipw, weighted.var = TRUE, metric = "ASD")
plot(ssps2f3e_ipw,type="density")
plot(ssps2f3e_ow,type="density")
# Extract the overlap weights and unweighted SMDs from the SumStat object
ssps2f3e_ow.smd_weighted_df<-as.data.frame(ssps2f3e_ow.smd$overlap)
ssps2f3e_ow.smd_unweighted_df<-as.data.frame(ssps2f3e_ow.smd$unweighted)
ssps2f3e_ipw.smd_weighted_df<-as.data.frame(ssps2f3e_ipw.smd$IPW)
ssps2f3e_ipw.smd_unweighted_df<-as.data.frame(ssps2f3e_ipw.smd$unweighted)
# Extract propensity score weights from the SumStat object
ps2f3e_ipw.w <- ssps2f3e_ipw$ps.weights$IPW
ps2f3e_ow.w <- ssps2f3e_ow$ps.weights$overlap
# Create a new data frame that contains the overlap weights and the SMDs
y0$ps2f3e_ipw.w<-ps2f3e_ipw.w
y0$ps2f3e_ow.w<-ps2f3e_ow.w


# Section 2b LCA model 3e combine cross-sectional weight  ---------------------
# Calculate the propensity scores = gamma by case * post probs
cps12p_f3e_ipw.w <- rowSums(ps1f3e_ipw.w*ps2f3e_ipw.w*lca_f3e$posterior)
cps12p_f3e_ow.w <- rowSums(ps1f3e_ow.w*ps2f3e_ow.w*lca_f3e$posterior)
y0$cps12p_f3e_ipw.w<-cps12p_f3e_ipw.w
y0$cps12p_f3e_ow.w<-cps12p_f3e_ow.w
y0$cps12p_f3e_ipw.w<-cps12p_f3e_ipw.w
y0$cps12p_f3e_ow.w<-cps12p_f3e_ow.w
# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3e
ggplot(data = y0, aes(x = as.factor(lcaf3e), y = cps12p_f3e_ow.w)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "Overlap Weights by Latent Class Model 1 - SAVA+SS", x = "Latent Class", y = "Overlap Weights")
# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3e
ggplot(data = y0, aes(x = as.factor(lcaf3e), y = cps12p_f3e_ipw.w)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "IPW by Latent Class Model 2 - SAVA+SS", x = "Latent Class", y = "IPS Weights")
# Create a boxplot to visualize the weights based on the treatment variable lcaf3a
# Load the ggplot2 library
library(ggplot2)
# Density plot for IP weights in greyscale
ggplot(data = y0, aes(x = cps12p_f3e_ipw.w, fill = lcaf3e)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Inverse Propensity Score Weights by LCA Model 2 - SAVA+SS", x = "IPS Weights", y = "Density") +
  scale_fill_grey()  # Set fill colors to shades of grey

# Density plot for overlap weights in greyscale
ggplot(data = y0, aes(x = cps12p_f3e_ow.w, fill = lcaf3e)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Overlap Weights by LCA Model 2 - SAVA+SS", x = "Overlap Weights", y = "Density") +
  scale_fill_grey()  # Set fill colors to shades of grey
#trim the weights

# Define trimming thresholds
lower_bound <- 0.05  # Example lower bound
upper_bound <- 0.95  # Example upper bound
#install.packages("datawizard")
library(datawizard)

trim_weights <- function(weights, lower_pct, upper_pct){
  
  # Calculate percentile thresholds
  lower <- quantile(weights, lower_pct, na.rm=TRUE) 
  upper <- quantile(weights, upper_pct, na.rm=TRUE)
  
  # Winsorize weights
  winsorized <- pmax(weights, lower)
  winsorized <- pmin(winsorized, upper)
  
  return(winsorized)
  
}

#**Trim 3e weight 1----------
# Apply trimming to cps12p_f3e_ow.w
y0$cps12p_f3e_ow.wtm <- trim_weights(y0$cps12p_f3e_ow.w, 0.001, 0.999)
y0$cps12p_f3e_ipw.wtm <- trim_weights(y0$cps12p_f3e_ipw.w,  0.001, 0.999)

# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3e
ggplot(data = y0, aes(x = as.factor(lcaf3e), y = cps12p_f3e_ow.wtm)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "Overlap Weights by Latent Class Model 2 (t4) - SAVA+SS", x = "Latent Class", y = "Overlap Weights")
# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3e
ggplot(data = y0, aes(x = as.factor(lcaf3e), y = cps12p_f3e_ipw.wtm)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "IPW by Latent Class Model 1(t4) - SAVA+SS", x = "Latent Class", y = "IPS Weights")
# Create a boxplot to visualize the weights based on the treatment variable lcaf3e
# Load the ggplot2 library
library(ggplot2)
# Density plot for IP weights in greyscale
ggplot(data = y0, aes(x = cps12p_f3e_ipw.wtm, fill = lcaf3e)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Inverse Propensity Score Weights by LCA Model 2 (t4)- SAVA+SS", x = "IPS Weights", y = "Density") +
  scale_fill_grey()  # Set fill colors to shades of grey

# Density plot for overlap weights in greyscale
ggplot(data = y0, aes(x = cps12p_f3e_ow.wtm, fill = lcaf3e)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Overlap Weights by LCA Model 2(t4) - SAVA+SS", x = "Overlap Weights", y = "Density") +scale_fill_grey()  # Set fill colors to shades of grey

#fitting the GEE model with interaction
# PS12-3e analysis-----------
library(geepack)
geemf3e_ipw <- geeglm(suicidality ~ lcaf3e * depression, data = y0, id = lcaf3e, weights = cps12p_f3e_ipw.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3e_ipw)

# Fit a simpler GEE model without the interaction term
geemf3e0_ipw <- geeglm(suicidality ~ lcaf3e + depression, data = y0, id = lcaf3e, weights = cps12p_f3e_ipw.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3e0_ipw)

# Fit a GEE model with interaction using the overlap weights
geemf3e_ow <- geeglm(suicidality ~ lcaf3e * depression, data = y0, id = lcaf3e, weights = cps12p_f3e_ow.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3e_ow)

# Fit a simpler GEE model without the interaction term
geemf3e0_ow<- geeglm(suicidality ~ lcaf3e + depression, data = y0, id = lcaf3e, weights = cps12p_f3e_ow.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3e0_ow)



anova(geemf3e0_ow, geemf3e_ow, test="score")

#3e-PS3 model 3e weight  ---------------------
#Minimal sufficient adjustment sets for estimating the total effect of LTFU on Suicidality at T4:#2.Amphetamine, Depression, Employment, Sex work, Time from HIV diagnosis
#set up the formula with minimal sufficient adjustment sets

psform3_f3e<-as.formula("ltfu~ 1 + depression  + sexwork_provide +employ + hivtime_0")
# Calculate summary statistics using the data frame
ssps3f3e_ow <- SumStat(psform3_f3e, method = "glm", weight="overlap", delta=0,data = y0)
ssps3f3e_ipw <- SumStat(psform3_f3e, method = "glm", weight="IPW", delta=0,data = y0)
ssps3f3e_ow.smd<-summary(ssps3f3e_ow, weighted.var = TRUE, metric = "ASD")
ssps3f3e_ipw.smd<-summary(ssps3f3e_ipw, weighted.var = TRUE, metric = "ASD")
plot(ssps3f3e_ipw,type="density")
plot(ssps3f3e_ow,type="density")
# Extract the overlap weights and unweighted SMDs from the SumStat object
ssps3f3e_ow.smd_weighted_df<-as.data.frame(ssps3f3e_ow.smd$overlap)
ssps3f3e_ow.smd_unweighted_df<-as.data.frame(ssps3f3e_ow.smd$unweighted)
ssps3f3e_ipw.smd_weighted_df<-as.data.frame(ssps3f3e_ipw.smd$IPW)
ssps3f3e_ipw.smd_unweighted_df<-as.data.frame(ssps3f3e_ipw.smd$unweighted)
# Extract propensity score weights from the SumStat object
ps3f3e_ipw.w <- ssps3f3e_ipw$ps.weights$IPW
ps3f3e_ow.w <- ssps3f3e_ow$ps.weights$overlap
# Create a new data frame that contains the overlap weights and the SMDs
y0$ps3f3e_ipw.w<-ps3f3e_ipw.w
y0$ps3f3e_ow.w<-ps3f3e_ow.w

#PS 4 3e
#Minimal sufficient adjustment sets containing Age, Education, Guardian's #Education, Regular partner, T-HIV diagnose for estimating the total effect of #LCA-Social Factors on Suicidality at T4:

#set up the formula with minimal sufficient adjustment sets
psform4_f3e<-as.formula("lcaf3e~ 1 + age_cat_0 + edu_cat_0 +  gedu + reg_partner  + hivtime_0")
ssps4f3e_alt <- SumStat(psform4_f3e, method = "glm", weight=c("IPW","overlap","matching","entropy"), delta=0,data = y0)
ssps4f3e_ow <- SumStat(psform4_f3e, method = "glm", weight="overlap", ,delta=0,data = y0)
ssps4f3e_ipw <- SumStat(psform4_f3e, method = "glm", weight="IPW", ,delta=0,data = y0)
ssps4f3e_ow.smd<-summary(ssps4f3e_ow, weighted.var = TRUE, metric = "ASD")
ssps4f3e_ipw.smd<-summary(ssps4f3e_ipw, weighted.var = TRUE, metric = "ASD")
plot(ssps4f3e_ipw,type="density")
plot(ssps4f3e_ow,type="density")
# Extract the overlap weights and unweighted SMDs from the SumStat object
ssps4f3e_ow.smd_weighted_df<-as.data.frame(ssps4f3e_ow.smd$overlap)
ssps4f3e_ow.smd_unweighted_df<-as.data.frame(ssps4f3e_ow.smd$unweighted)
ssps4f3e_ipw.smd_weighted_df<-as.data.frame(ssps4f3e_ipw.smd$IPW)
ssps4f3e_ipw.smd_unweighted_df<-as.data.frame(ssps4f3e_ipw.smd$unweighted)
# Extract propensity score weights from the SumStat object
ps4f3e_ipw.w <- ssps4f3e_ipw$ps.weights$IPW
ps4f3e_ow.w <- ssps4f3e_ow$ps.weights$overlap

# PS5-3e ------ 
#minimial sufficient set for PS5-3e
#Minimal sufficient adjustment sets for estimating the total effect of Depression on Suicidality at T4:#1.Age, Education, General bullying, Guardian's Education, HIV stigma, Homophobic bullying, LCA-Social Factors, T-HIV diagnose

psform5_f3e<-as.formula("depression~ 1 + age_cat_0+ edu_cat_0 +  gedu  + lcaf3e + hivtime_0")
# Calculate summary weight with PSweight package 
ssps5f3e_ow <- SumStat(psform5_f3e, method = "glm", weight="overlap", ,delta=0,data = y0)
ssps5f3e_ipw <- SumStat(psform5_f3e, method = "glm", weight="IPW", ,delta=0,data = y0)
#trim at 0.065 and 0.935 for IPW
ssps5f3e_ow.smd<-summary(ssps5f3e_ow, weighted.var = TRUE, metric = "ASD")
ssps5f3e_ipw.smd<-summary(ssps5f3e_ipw, weighted.var = TRUE, metric = "ASD")
plot(ssps5f3e_ipw,type="density")
plot(ssps5f3e_ow,type="density")
# Extract the overlap weights and unweighted SMDs from the SumStat object
ssps5f3e_ow.smd_weighted_df<-as.data.frame(ssps5f3e_ow.smd$overlap)
ssps5f3e_ow.smd_unweighted_df<-as.data.frame(ssps5f3e_ow.smd$unweighted)
ssps5f3e_ipw.smd_weighted_df<-as.data.frame(ssps5f3e_ipw.smd$IPW)
ssps5f3e_ipw.smd_unweighted_df<-as.data.frame(ssps5f3e_ipw.smd$unweighted)
# Extract propensity score weights from the SumStat object
ps5f3e_ipw.w <- ssps5f3e_ipw$ps.weights$IPW
ps5f3e_ow.w <- ssps5f3e_ow$ps.weights$overlap
# Create a new data frame that contains the overlap weights and the SMDs
y0$ps5f3e_ipw.w<-ps5f3e_ipw.w
y0$ps5f3e_ow.w<-ps5f3e_ow.w
y0$ps5f3e_ipw.w<-ps5f3e_ipw.w
y0$ps5f3e_ow.w<-ps5f3e_ow.w
#S 2c-com 3e ------
# combine propensity scores = gamma by case * post probs
cps345p_f3e_ipw.w <- rowSums(ps3f3e_ipw.w*ps4f3e_ipw.w*ps5f3e_ipw.w*lca_f3e$posterior)
cps345p_f3e_ow.w <- rowSums(ps3f3e_ow.w*ps4f3e_ow.w*ps5f3e_ow.w*lca_f3e$posterior)

y0$cps345p_f3e_ipw.w <-cps345p_f3e_ipw.w 
y0$cps345p_f3e_ow.w<-cps345p_f3e_ow.w
y0$cps345p_f3e_ipw.w <-cps345p_f3e_ipw.w 
y0$cps345p_f3e_ow.w<-cps345p_f3e_ow.w

summary(y0$cps345p_f3e_ipw.w)
summary(y0$cps345p_f3e_ow.w)

# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3e
ggplot(data = y0, aes(x = as.factor(lcaf3e), y = cps345p_f3e_ow.w)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "Overlap Weights by Latent Class Model 2 (t4) - SAVA+SS", x = "Latent Class", y = "Overlap Weights")
# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3e
ggplot(data = y0, aes(x = as.factor(lcaf3e), y = cps345p_f3e_ipw.w)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "IPW by Latent Class Model 1(t4) - SAVA+SS", x = "Latent Class", y = "IPS Weights")
# Create a boxplot to visualize the weights based on the treatment variable lcaf3e
# Load the ggplot2 library
library(ggplot2)
# Density plot for IP weights in greyscale
ggplot(data = y0, aes(x = cps345p_f3e_ipw.w, fill = lcaf3e)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Inverse Propensity Score Weights by LCA Model 2 (t4)- SAVA+SS", x = "IPS Weights", y = "Density") +
  scale_fill_grey()  # Set fill colors to shades of grey

# Density plot for overlap weights in greyscale
ggplot(data = y0, aes(x = cps345p_f3e_ow.w, fill = lcaf3e)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Overlap Weights by LCA Model 2(t4) - SAVA+SS", x = "Overlap Weights", y = "Density") +scale_fill_grey()  # Set fill colors to shades of grey


#install.packages("datawizard")


trim_weights <- function(weights, lower_pct, upper_pct){
  library(datawizard)
  # Calculate percentile thresholds
  lower <- quantile(weights, lower_pct, na.rm=TRUE) 
  upper <- quantile(weights, upper_pct, na.rm=TRUE)
  
  # Winsorize weights
  winsorized <- pmax(weights, lower)
  winsorized <- pmin(winsorized, upper)
  
  return(winsorized)
  
}

# Apply trimming to cps345p_f3e_ipw.w
y0$cps345p_f3e_ipw.wtm <- trim_weights(y0$cps345p_f3e_ipw.w, 0.025, 0.925)
# Apply trimming to cps345p_f3e_ow.w
y0$cps345p_f3e_ow.wtm <- trim_weights(y0$cps345p_f3e_ow.w, 0.025, 0.925)


ggplot(data = y0, aes(x = as.factor(lcaf3e), y = cps345p_f3e_ipw.wtm)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "Overlap Weights by Latent Class Model 2 (t4) - SAVA+Socia Class", x = "Latent Class", y = "Overlap Weights")

ggplot(data = y0, aes(x = as.factor(lcaf3e), y = cps345p_f3e_ow.wtm)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "Overlap Weights by Latent Class Model 2 (t4) - SAVA+Socia Class", x = "Latent Class", y = "Overlap Weights")

# Density plot for IP weights in greyscale
ggplot(data = y0, aes(x = cps345p_f3e_ipw.wtm, fill = lcaf3e)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Inverse Propensity Score Weights by LCA Model 2 (t4)- SAVA+SS", x = "IPS Weights", y = "Density") +
  scale_fill_grey()  # Set fill colors to shades of grey

# Density plot for overlap weights in greyscale
ggplot(data = y0, aes(x = cps345p_f3e_ow.wtm, fill = lcaf3e)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Overlap Weights by LCA Model 2(t4) - SAVA+SS", x = "Overlap Weights", y = "Density") +scale_fill_grey()  # Set fill colors to shades of grey

geemf3et_ow.wtm <- geeglm(suicidality_t4 ~ lcaf3e * depression, data = y0 , id = lcaf3e, weights = cps345p_f3e_ow.wtm, family = gaussian(), corstr = "exchangeable")
geemf3et_ow <- geeglm(suicidality_t4 ~ lcaf3e * depression, data = y0 , id = lcaf3e, weights = cps345p_f3e_ow.w, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3et_ow,geemf3et_ow.wtm)

#Setting weights to y0------
y0$cps345p_f3e_ipw.w<-cps345p_f3e_ipw.w
y0$cps345p_f3e_ow.w<-cps345p_f3e_ow.w
cps345p_f3e_ipw.wtm<-y0$cps345p_f3e_ipw.wtm
cps345p_f3e_ow.wtm<-y0$cps345p_f3e_ow.wtm
cps345p_f3a_ipw.w<-y0$cps345p_f3a_ipw.w
y0$cps345p_f3a_ow.w<-cps345p_f3a_ow.w
cps345p_f3a_ipw.wtm<-y0$cps345p_f3a_ipw.wtm
cps345p_f3a_ow.wtm<-y0$cps345p_f3a_ow.wtm
cps345p_f3c_ipw.w<-y0$cps345p_f3c_ipw.w
cps345p_f3c_ow.w<-y0$cps345p_f3c_ow.w
cps345p_f3c_ipw.wtm<-y0$cps345p_f3c_ipw.wtm
cps345p_f3c_ow.wtm<-y0$cps345p_f3c_ow.wtm


#Setting weights to y0------
y0$ipw3a  <-y0$cps12p_f3a_ipw.w
y0$ow3a  <-y0$cps12p_f3a_ow.w
y0$ipwt3a  <-y0$cps12p_f3a_ipw.wtm
y0$owt3a  <-y0$cps12p_f3a_ow.wtm

y0$ipw3c  <-y0$cps12p_f3c_ipw.w
y0$ow3c  <-y0$cps12p_f3c_ow.w
y0$ipwt3c  <-y0$cps12p_f3c_ipw.wtm
y0$owt3c  <-y0$cps12p_f3c_ow.wtm

y0$ipw3e  <-y0$cps12p_f3e_ipw.w
y0$ow3e  <-y0$cps12p_f3e_ow.w
y0$ipwt3e  <-y0$cps12p_f3e_ipw.wtm
y0$owt3e  <-y0$cps12p_f3e_ow.wtm

y0$ipw3at4<-y0$cps345p_f3a_ipw.w
y0$ow3at4<-y0$cps345p_f3a_ow.w
y0$ipwt3at4<-y0$cps345p_f3a_ipw.wtm
y0$owt3at4<-y0$cps345p_f3a_ow.wtm

y0$ipw3ct4<-y0$cps345p_f3c_ipw.w
y0$ow3ct4<-y0$cps345p_f3c_ow.w
y0$ipwt3ct4<-y0$cps345p_f3c_ipw.wtm
y0$owt3ct4<-y0$cps345p_f3c_ow.wtm

y0$ipw3et4<-y0$cps345p_f3e_ipw.w
y0$ow3et4<-y0$cps345p_f3e_ow.w
y0$ipwt3et4<-y0$cps345p_f3e_ipw.wtm
y0$owt3et4<-y0$cps345p_f3e_ow.wtm


#analysis -----
y0$lcaf3c <- relevel(y0$lcaf3c, ref = "3")
#evaluate whether to account for clustering effect 
library(multilevel)
ICC1(aov(suicidality~lcaf3a, data=y0)) # 0.4083 indicates strong clustering 
library(geepack)
# Fit the GEE model
geemf3a <- geeglm(suicidality ~ lcaf3a * depression, data = y0, id = lcaf3a, weights = owt3a, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3a)
# Fit a simpler GEE model without the interaction term
geemf3a0 <- geeglm(suicidality ~ lcaf3a + depression, data = y0, id = lcaf3a, weights = owt3a, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3a,geemf3a0)

# Compare the models
# Create a data frame with the QIC values
qic_value3a<-geepack::QIC(geemf3a)
qic_value3a0<-geepack::QIC(geemf3a0)
qic_values_3a <- data.frame(
  Model = c("geemf3a", "geemf3a0"),
  QIC = c(qic_value3a["QIC"], qic_value3a0["QIC"]),
  QICu = c(qic_value3a["QICu"], qic_value3a0["QICu"]),
  QuasiLik = c(qic_value3a["Quasi Lik"], qic_value3a0["Quasi Lik"]),
  CIC = c(qic_value3a["CIC"], qic_value3a0["CIC"]),
  Params = c(qic_value3a["params"], qic_value3a0["params"]),
  QICC = c(qic_value3a["QICC"], qic_value3a0["QICC"])
)

# Write the data frame to a CSV file
write.csv(qic_values_3a, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Secondary/Data/qic_values_3a.csv", row.names = FALSE)
#anova test
anova(geemf3a,geemf3a0, test="score")
#emmeans for categorical depression

#make sure lcaf3a is a factor 
y0$lcaf3a<-as.factor(y0$lcaf3a)
#wald test for heterogeneity
# Fit separate models for each level of lcaf3e stratified by depression
y03a1<-y0%>%filter(lcaf3a==1)
y03a2<-y0%>%filter(lcaf3a==2)
y03a3<-y0%>%filter(lcaf3a==3)

#will use linear regression since analysis is within strate of lcaf3a
m3a1 <- geeglm(suicidality ~ depression, data = y03a1, id = no., 
               weights = owt3a, family = gaussian(), corstr = "exchangeable")
m3a2 <- geeglm(suicidality ~ depression, data = y03a2, id = no., 
               weights =owt3a, family = gaussian(), corstr = "exchangeable")
m3a3 <- geeglm(suicidality ~ depression, data = y03a3, id = no., 
               weights = owt3a, family = gaussian(), corstr = "exchangeable")

tab_model(m3a1,m3a2,m3a3)

geemf3a_int4 <- geeglm(suicidality_t4 ~ lcaf3a * depression, data = y0 , id = lcaf3a, weights = owt3at4, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3a_int4)
tab_model(geemf3a,geemf3a_int4)



y0$lcaf3c <- factor(lca_f3c$predclass, levels = c(3, 1, 2))
ICC1(aov(suicidality~lcaf3c, data=y0)) # 0.365 indicates strong clustering
# Fit the GEE model
geemf3c <- geeglm(suicidality ~ lcaf3c * depression, data = y0, id = lcaf3c, weights =   owt3c, family = gaussian(), corstr = "exchangeable")
#compare GEE vs linear regression
tab_model(geemf3c,lmf3c)
tab_model(geemf3c)
# Fit a simpler GEE model without the interaction term
geemf3c0 <- geeglm(suicidality ~ lcaf3c + depression, data = y0, id = lcaf3c, weights =   owt3c, family = gaussian(), corstr = "exchangeable")
#install.packages("glmtoolbox")
# Compare the models
# Create a data frame with the QIC values

qic_value3c<-geepack::QIC(geemf3c)
qic_value3c0<-geepack::QIC(geemf3c0)
qic_values_3c <- data.frame(
  Model = c("geemf3c", "geemf3c0"),
  QIC = c(qic_value3c["QIC"], qic_value3c0["QIC"]),
  QICu = c(qic_value3c["QICu"], qic_value3c0["QICu"]),
  QuasiLik = c(qic_value3c["Quasi Lik"], qic_value3c0["Quasi Lik"]),
  CIC = c(qic_value3c["CIC"], qic_value3c0["CIC"]),
  Params = c(qic_value3c["params"], qic_value3c0["params"]),
  QICC = c(qic_value3c["QICC"], qic_value3c0["QICC"])
)

# Write the data frame to a CSV file
write.csv(qic_values_3c, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/qic_values_3c.csv", row.names = FALSE)

#anova test
anova(geemf3c,geemf3c0, test="score")


#make sure lcaf3c is a factor 
y0$lcaf3c<-as.factor(y0$lcaf3c)
#wald test for heterogeneity
# Fit separate models for each level of lcaf3c stratified by depression
y03c1<-y0%>%filter(lcaf3c==1)
y03c2<-y0%>%filter(lcaf3c==2)
y03c3<-y0%>%filter(lcaf3c==3)

#ID as ID
m3c1 <- geeglm(suicidality ~ depression, data = y03c1, id = no., 
               weights =   owt3c, family = gaussian(), corstr = "exchangeable")

m3c2 <- geeglm(suicidality ~ depression, data = y03c2, id = no., 
               weights =   owt3c, family = gaussian(), corstr = "exchangeable")

m3c3 <- geeglm(suicidality ~ depression, data = y03c3, id = no., 
               weights =   owt3c, family = gaussian(), corstr = "exchangeable")

tab_model(m3c1,m3c2,m3c3)

geemf3c_int4 <- geeglm(suicidality_t4 ~ lcaf3c * depression, data = y0 , id = lcaf3c, weights =   owt3ct4, family = gaussian(), corstr = "exchangeable")
summary(geemf3c_int4)
tab_model(geemf3c,geemf3c_int4)


#IPW for full model
# Switch the reference group of the lcaf3b variable from 1 to 3
y0$lcaf3e <- factor(lca_f3e$predclass, levels = c(2, 3, 1))

#evaluate whether to account for clustering effect 
library(multilevel)
ICC1(aov(suicidality~lcaf3e, data=y0)) # 0.315 indicates strong clustering 

# Fit the GEE model
geemf3e <- geeglm(suicidality ~ lcaf3e * depression, data = y0, id = lcaf3e, weights =   owt3e, family = gaussian(), corstr = "exchangeable")
#compare GEE vs linear regression
tab_model(geemf3e,lmf3e)
# Fit a simpler GEE model without the interaction term
geemf3e0 <- geeglm(suicidality ~ lcaf3e + depression, data = y0, id = lcaf3e, weights =   owt3e, family = gaussian(), corstr = "exchangeable")
# Compare the models
tab_model(geemf3e,geemf3e0)
qic_value3e<-geepack::QIC(geemf3e)
qic_value3e0<-geepack::QIC(geemf3e0)
qic_values_3e <- data.frame(
  Model = c("geemf3e", "geemf3e0"),
  QIC = c(qic_value3e["QIC"], qic_value3e0["QIC"]),
  QICu = c(qic_value3e["QICu"], qic_value3e0["QICu"]),
  QuasiLik = c(qic_value3e["Quasi Lik"], qic_value3e0["Quasi Lik"]),
  CIC = c(qic_value3e["CIC"], qic_value3e0["CIC"]),
  Params = c(qic_value3e["params"], qic_value3e0["params"]),
  QICC = c(qic_value3e["QICC"], qic_value3e0["QICC"]))

# Write the data frame to a CSV file
write.csv(qic_values_3e, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/qic_values_3e.csv", row.names = FALSE)

#anova test
anova(geemf3e,geemf3e0, test="score")



#make sure lcaf3e is a factor 
y0$lcaf3e<-as.factor(y0$lcaf3e)
#wald test for heterogeneity
# Fit separate models for each level of lcaf3e stratified by depression
y03e1<-y0%>%filter(lcaf3e==1)
y03e2<-y0%>%filter(lcaf3e==2)
y03e3<-y0%>%filter(lcaf3e==3)

#will use GEE still analysis is within strate of lcaf3e
m3e1 <- geeglm(suicidality ~ depression, data = y03e1, id = no., 
               weights =   owt3e, family = gaussian(), corstr = "exchangeable")

m3e2 <- geeglm(suicidality ~ depression, data = y03e2, id = no., 
               weights =   owt3e, family = gaussian(), corstr = "exchangeable")

m3e3 <- geeglm(suicidality ~ depression, data = y03e3, id = no., 
               weights =   owt3e, family = gaussian(), corstr = "exchangeable")

tab_model(m3e1,m3e2,m3e3)

geemf3e_int4 <- geeglm(suicidality_t4 ~ lcaf3e * depression, data = y0 , id = lcaf3e, weights =   owt3et4, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3e,geemf3e_int4)



#will use linear regression since analysis is within strate of lcaf3a
m3a1t4 <- geeglm(suicidality_t4 ~ depression, data = y03a1, id = no., 
                 weights =   owt3at4, family = gaussian(), corstr = "exchangeable")
m3a2t4 <- geeglm(suicidality_t4 ~ depression, data = y03a2, id = no., 
                 weights =   owt3at4, family = gaussian(), corstr = "exchangeable")
m3a3t4 <- geeglm(suicidality_t4 ~ depression, data = y03a3, id = no., 
                 weights =   owt3at4, family = gaussian(), corstr = "exchangeable")
tab_model(m3a1,m3a1t4)
tab_model(m3a2,m3a2t4)
tab_model(m3a3,m3a3t4)

#lcaf3c
y03c1<-y0%>%filter(lcaf3c==1)
y03c2<-y0%>%filter(lcaf3c==2)
y03c3<-y0%>%filter(lcaf3c==3)
y0ipw_3ct<-ipw_3ct
tab_model(m3c1,m3c2,m3c3)
#will use 
m3c1t4 <- geeglm(suicidality_t4 ~ depression, data = y03c1, id = no., 
                 weights =   owt3ct4, family = gaussian(), corstr = "exchangeable")
m3c2t4 <- geeglm(suicidality_t4 ~ depression, data = y03c2, id = no., 
                 weights =   owt3ct4, family = gaussian(), corstr = "exchangeable")
m3c3t4 <- geeglm(suicidality_t4 ~ depression, data = y03c3, id = no., 
                 weights =   owt3ct4, family = gaussian(), corstr = "exchangeable")   
tab_model(m3c1,m3c1t4)
tab_model(m3c2,m3c2t4)
tab_model(m3c3,m3c3t4)

y03e1<-y0%>%filter(lcaf3e==1)
y03e2<-y0%>%filter(lcaf3e==2)
y03e3<-y0%>%filter(lcaf3e==3)

tab_model(m3e1,m3e2,m3e3)
#will use linear regression since analysis is within strate of lcaf3e
m3e1t4 <- geeglm(suicidality_t4 ~ depression, data = y03e1, id = no., 
                 weights =   owt3et4, family = gaussian(), corstr = "exchangeable")
m3e2t4 <- geeglm(suicidality_t4 ~ depression, data = y03e2, id = no., 
                 weights =   owt3et4, family = gaussian(), corstr = "exchangeable")
m3e3t4 <- geeglm(suicidality_t4 ~ depression, data = y03e3, id = no., 
                 weights =   owt3et4, family = gaussian(), corstr = "exchangeable")

tab_model(m3e1,m3e1t4)
tab_model(m3e2,m3e2t4)
tab_model(m3e3,m3e3t4)
tab_model(m3e1,m3e1t4,geemf3e,geemf3e_int4)

tab_model(m3a1,m3a1t4,m3a2,m3a2t4,m3a3,m3a3t4,geemf3a,geemf3a_int4)

library(emmeans)
em3a<-emmeans::emmeans(geemf3a, specs=c("lcaf3a","depression"))
em3a_con<-contrast(em3a, method="pairwise", simple="depression")
em3a_conci<-confint(contrast(em3a, method="pairwise", simple="depression"))
em3a_con_df<-as.data.frame(em3a_con)
em3a_conci_df<-as.data.frame(em3a_conci)
# Write the data frame to a CSV file
write.csv(em3a_con_df, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/em3a_con_df.csv", row.names = FALSE)

# Write the data frame to a CSV file
write.csv(em3a_conci_df, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/em3a_conci_df.csv", row.names = FALSE)

em3at4<-emmeans::emmeans(geemf3a_int4 , specs=c("lcaf3a","depression"))
em3at4_con<-contrast(em3at4, method="pairwise", simple="depression")
em3at4_conci<-confint(contrast(em3at4, method="pairwise", simple="depression"))
em3at4_con_df<-as.data.frame(em3at4_con)
em3at4_conci_df<-as.data.frame(em3at4_conci)
# Write the data frame to a CSV file
write.csv(em3at4_con_df, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/em3at4_con_df.csv", row.names = FALSE)
# Write the data frame to a CSV file
write.csv(em3at4_conci_df, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/em3at4_conci_df.csv", row.names = FALSE)

#emmeans for categorical depression
em3c<-emmeans::emmeans(geemf3c, specs=c("lcaf3c","depression"))
em3c_con<-contrast(em3c, method="pairwise", simple="depression")
em3c_conci<-confint(contrast(em3c, method="pairwise", simple="depression"))

em3c_con_df<-as.data.frame(em3c_con)
em3c_conci_df<-as.data.frame(em3c_conci)
# Write the data frame to a CSV file
write.csv(em3c_con_df, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/em3c_con_df.csv", row.names = FALSE)

# Write the data frame to a CSV file
write.csv(em3c_conci_df, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/em3c_conci_df.csv", row.names = FALSE)

em3ct4<-emmeans::emmeans(geemf3c_int4 , specs=c("lcaf3c","depression"))
em3ct4_con<-contrast(em3ct4, method="pairwise", simple="depression")
em3ct4_conci<-confint(contrast(em3ct4, method="pairwise", simple="depression"))
em3ct4_con_df<-as.data.frame(em3ct4_con)
em3ct4_conci_df<-as.data.frame(em3ct4_conci)
# Write the data frame to a CSV file
write.csv(em3ct4_con_df, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/em3ct4_con_df.csv", row.names = FALSE)
# Write the data frame to a CSV file
write.csv(em3ct4_conci_df, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/em3ct4_conci_df.csv", row.names = FALSE)

#emmeans for categorical depression
em3e<-emmeans::emmeans(geemf3e, specs=c("lcaf3e","depression"))
em3e_con<-contrast(em3e, method="pairwise", simple="depression")
em3e_conci<-confint(contrast(em3e, method="pairwise", simple="depression"))
em3e_con_df<-as.data.frame(em3e_con)
em3e_conci_df<-as.data.frame(em3e_conci)
# Write the data frame to a CSV file
write.csv(em3e_con_df, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/em3e_con_df.csv", row.names = FALSE)
# Write the data frame to a CSV file
write.csv(em3e_conci_df, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/em3e_conci_df.csv", row.names = FALSE)


em3et4<-emmeans::emmeans(geemf3e_int4 , specs=c("lcaf3e","depression"))
em3et4_con<-contrast(em3et4, method="pairwise", simple="depression")
em3et4_conci<-confint(contrast(em3et4, method="pairwise", simple="depression"))
em3et4_con_df<-as.data.frame(em3et4_con)
em3et4_conci_df<-as.data.frame(em3et4_conci)
# Write the data frame to a CSV file
write.csv(em3et4_con_df, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/em3et4_con_df.csv", row.names = FALSE)
# Write the data frame to a CSV file
write.csv(em3et4_conci_df, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/em3et4_conci_df.csv", row.names = FALSE)

geemf3a0_int4 <- geeglm(suicidality_t4 ~ lcaf3a + depression, data = y0 , id = lcaf3e, weights =   owt3at4, family = gaussian(), corstr = "exchangeable")

geemf3c0_int4 <- geeglm(suicidality_t4 ~ lcaf3c + depression, data = y0 , id = lcaf3e, weights =   owt3ct4, family = gaussian(), corstr = "exchangeable")

geemf3e0_int4 <- geeglm(suicidality_t4 ~ lcaf3e +  depression, data = y0 , id = lcaf3e, weights =   owt3et4, family = gaussian(), corstr = "exchangeable")


geepack::QIC(geemf3a)
geepack::QIC(geemf3a0)

geepack::QIC(geemf3c)
geepack::QIC(geemf3c0)

geepack::QIC(geemf3e)
geepack::QIC(geemf3e0)

geepack::QIC(geemf3a_int4)
geepack::QIC(geemf3a0_int4)

geepack::QIC(geemf3c_int4)
geepack::QIC(geemf3c0_int4)

geepack::QIC(geemf3e_int4)
geepack::QIC(geemf3e0_int4)

tab_model(geemf3a0, geemf3a0_int4)
tab_model(geemf3c0,geemf3c0_int4)
tab_model(geemf3e0,geemf3e0_int4)

anova(geemf3a0, geemf3a, test="score")
anova(geemf3c0, geemf3c, test="score")
anova(geemf3e0, geemf3e, test="score")
anova(geemf3a0_int4, geemf3a_int4, test="score")
anova(geemf3c0_int4, geemf3c_int4, test="score")
anova(geemf3e0_int4, geemf3e_int4, test="score")

#Table 3 ------
tab_model(geemf3a,geemf3a_int4)
tab_model(geemf3c,geemf3c_int4)
tab_model(geemf3e,geemf3e_int4)
summary(geemf3a)
summary(geemf3c)
summary(geemf3e)
summary(geemf3a_int4)
summary(geemf3c_int4)
summary(geemf3e_int4)


geem1 <- geeglm(suicidality ~ bullied_gay * depression, data = y0, id = no., family = gaussian(), corstr = "exchangeable")
tab_model(geem1)
geem2 <- geeglm(suicidality ~ bullied_gen_1 * depression, data = y0, id = no., family = gaussian(), corstr = "exchangeable")
tab_model(geem2)

geem3 <- geeglm(suicidality ~ ipva * depression, data = y0, id = no., family = gaussian(), corstr = "exchangeable")
tab_model(geem3)

geem4 <- geeglm(suicidality ~ amphetamine * depression, data = y0, id = no., family = gaussian(), corstr = "exchangeable")
tab_model(geem4)

geem5 <- geeglm(suicidality ~ bingedrink1_1 * depression, data = y0, id = no., family = gaussian(), corstr = "exchangeable")

tab_model(geem5)
geem6<- geeglm(suicidality ~ HIVstigma_median * depression, data = y0,
              id = no., family = gaussian(), corstr = "exchangeable")
tab_model(geem6)
geem7<- geeglm(suicidality ~ socialsupport_1q_1* depression, data = y0,
              id = no., family = gaussian(), corstr = "exchangeable")
tab_model(geem7)
geem8<- geeglm(suicidality ~sexwork_provide* depression, data = y0,
               id = no., family = gaussian(), corstr = "exchangeable")
tab_model(geem8)
geem9<- geeglm(suicidality ~income_low_1* depression, data = y0,
               id = no., family = gaussian(), corstr = "exchangeable")
tab_model(geem9)

geemt1 <- geeglm(suicidality_t4 ~ bullied_gay * depression, data = y0, id = no., family = gaussian(), corstr = "exchangeable")
tab_model(geemt1)
geemt2 <- geeglm(suicidality_t4 ~ bullied_gen_1 * depression, data = y0, id = no., family = gaussian(), corstr = "exchangeable")
tab_model(geemt2)

geemt3 <- geeglm(suicidality_t4 ~ ipva * depression, data = y0, id = no., family = gaussian(), corstr = "exchangeable")
tab_model(geemt3)

geemt4 <- geeglm(suicidality_t4 ~ amphetamine * depression, data = y0, id = no., family = gaussian(), corstr = "exchangeable")
tab_model(geemt4)

geemt5 <- geeglm(suicidality_t4 ~ bingedrink1_1 * depression, data = y0, id = no., family = gaussian(), corstr = "exchangeable")

tab_model(geemt5)
geemt6<- geeglm(suicidality_t4 ~ HIVstigma_median * depression, data = y0,
               id = no., family = gaussian(), corstr = "exchangeable")
tab_model(geemt6)
geemt7<- geeglm(suicidality_t4 ~ socialsupport_1q_1* depression, data = y0,
               id = no., family = gaussian(), corstr = "exchangeable")
tab_model(geemt7)
geemt8<- geeglm(suicidality_t4 ~sexwork_provide* depression, data = y0,
               id = no., family = gaussian(), corstr = "exchangeable")
tab_model(geemt8)
geemt9<- geeglm(suicidality_t4 ~income_low_1* depression, data = y0,
               id = no., family = gaussian(), corstr = "exchangeable")
tab_model(geemt9)

tab_model(geem1,geemt1)
tab_model(geem2,geemt2)
tab_model(geem3,geemt3)
tab_model(geem4,geemt4)
tab_model(geem5,geemt5)
tab_model(geem6,geemt6)
tab_model(geem7,geemt7)
tab_model(geem8,geemt8)
tab_model(geem9,geemt9)

geemf3a_ipw <- geeglm(suicidality ~ lcaf3a * depression, data = y0, id = lcaf3a, weights = ipwt3a, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3a)

geemf3a_int4_ipw <- geeglm(suicidality_t4 ~ lcaf3a * depression, data = y0 , id = lcaf3a, weights = ipwt3at4, family = gaussian(), corstr = "exchangeable")

geemf3c_ipw <- geeglm(suicidality ~ lcaf3c * depression, data = y0, id = lcaf3c, weights =   ipwt3c, family = gaussian(), corstr = "exchangeable")

geemf3c_int4_ipw <- geeglm(suicidality_t4 ~ lcaf3c * depression, data = y0 , id = lcaf3c, weights =   ipwt3ct4, family = gaussian(), corstr = "exchangeable")

geemf3e_ipw <- geeglm(suicidality ~ lcaf3e * depression, data = y0, id = lcaf3e, weights =   ipwt3e, family = gaussian(), corstr = "exchangeable")

geemf3e_int4_ipw <- geeglm(suicidality_t4 ~ lcaf3e * depression, data = y0 , id = lcaf3e, weights =   ipwt3et4, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3a,geemf3a_ipw,geemf3a_int4,geemf3a_int4_ipw)
tab_model(geemf3c,geemf3c_ipw,geemf3c_int4,geemf3c_int4_ipw)
tab_model(geemf3e,geemf3e_ipw,geemf3e_int4,geemf3e_int4_ipw)
tab_model(geemf3a_ipw,geemf3a_int4_ipw)
tab_model(geemf3c_ipw,geemf3c_int4_ipw)
tab_model(geemf3e_ipw,geemf3e_int4_ipw)
#Tab3 no IPCW -----------
cps45p_f3e_ow.w <- rowSums(ps4f3e_ow.w*ps5f3e_ow.w*lca_f3e$posterior)
y0$cps45p_f3e_ow.w<-cps45p_f3e_ow.w
summary(y0$cps45p_f3e_ow.w)
# Create a greyscale boxplot to visualize the weights based on the treatment variable lcaf3e
ggplot(data = y0, aes(x = as.factor(lcaf3e), y = cps45p_f3e_ow.w)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "Overlap Weights by Latent Class Model 2 (t4) - SAVA+SS", x = "Latent Class", y = "Overlap Weights")

library(ggplot2)

# Density plot for overlap weights in greyscale
ggplot(data = y0, aes(x = cps45p_f3e_ow.w, fill = lcaf3e)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Overlap Weights by LCA Model 2(t4) - SAVA+SS", x = "Overlap Weights", y = "Density") +scale_fill_grey()  # Set fill colors to shades of grey


# Apply trimming to cps345p_f3e_ow.w
y0$cps45p_f3e_ow.wtm <- trim_weights(y0$cps45p_f3e_ow.w, 0.03, 0.97)

ggplot(data = y0, aes(x = as.factor(lcaf3e), y = cps45p_f3e_ow.wtm)) +
  geom_boxplot(fill = "grey", color = "black") +  # Using grey fill and black border for greyscale effect
  labs(title = "Overlap Weights by Latent Class Model 2 (t4) - SAVA+Socia Class", x = "Latent Class", y = "Overlap Weights")

# Density plot for IP weights in greyscale
ggplot(data = y0, aes(x = cps45p_f3e_ipw.wtm, fill = lcaf3e)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Inverse Propensity Score Weights by LCA Model 2 (t4)- SAVA+SS", x = "IPS Weights", y = "Density") +
  scale_fill_grey()  # Set fill colors to shades of grey

# Density plot for overlap weights in greyscale
ggplot(data = y0, aes(x = cps45p_f3e_ow.wtm, fill = lcaf3e)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Overlap Weights by LCA Model 2(t4) - SAVA+SS", x = "Overlap Weights", y = "Density") +scale_fill_grey()  # Set fill colors to shades of grey

geemf3et_ow.wtm_no3 <- geeglm(suicidality_t4 ~ lcaf3e * depression, data = y0 , id = lcaf3e, weights = cps45p_f3e_ow.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3et_ow.wtm, geemf3et_ow.wtm_no3)
geemf3et_ow_no3 <- geeglm(suicidality_t4 ~ lcaf3e * depression, data = y0 , id = lcaf3e, weights = cps45p_f3e_ow.w, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3et_ow_no3,geemf3et_ow, geemf3et_ow.wtm,geemf3et_ow_no3)

geemf3e_ow.wtm <- geeglm(suicidality ~ lcaf3e * depression, data = y0 , id = lcaf3e, weights = cps12p_f3e_ow.wtm, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3et_ow.wtm, geemf3et_ow.wtm_no3)
geemf3e_ow <- geeglm(suicidality ~ lcaf3e * depression, data = y0 , id = lcaf3e, weights = cps12p_f3e_ow.w, family = gaussian(), corstr = "exchangeable")
tab_model(geemf3e_ow,geemf3e_ow.wtm)

#Sup Table 3- Main effects ---------
tab_model(geemf3a0,geemf3a0_int4)
tab_model(geemf3c0,geemf3c0_int4)
tab_model(geemf3e0,geemf3e0_int4)


# Create a list of models
models <- list(geemf3a, geemf3a0, geemf3c, geemf3c0, geemf3e, geemf3e0, geemf3a_int4, geemf3a0_int4, geemf3c_int4, geemf3c0_int4, geemf3e_int4, geemf3e0_int4)


# Create a list of model names
model_names <- c("geemf3a", "geemf3a0", "geemf3c", "geemf3c0", "geemf3e", "geemf3e0", "geemf3a_int4", "geemf3a0_int4", "geemf3c_int4", "geemf3c0_int4", "geemf3e_int4", "geemf3e0_int4")

# Initialize an empty data frame to store the summary statistics
summary_stats <- data.frame()

# Loop over the models
for (i in seq_along(models)) {
  # Get the QIC summary for the current model
  qic_summary <- geepack::QIC(models[[i]])
  
  # Get the model name
  model_name <- model_names[i]
  
  # Create a data frame with the summary statistics for the current model
  model_summary <- data.frame(Model = model_name, QIC = qic_summary["QIC"], QICu = qic_summary["QICu"], QuasiLik = qic_summary["Quasi Lik"], CIC = qic_summary["CIC"], Params = qic_summary["params"], QICC = qic_summary["QICC"])
  
  # Add the summary statistics for the current model to the overall summary statistics data frame
  summary_stats <- rbind(summary_stats, model_summary)
}

# Print the summary statistics data frame
print(summary_stats)
write.csv(summary_stats, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/summary_stats.csv", row.names = TRUE)

#Sup table 4 - Sensitivity analysis -----------------

# untrimmed OW

geemf3e_untrimmed <- geeglm(suicidality ~ lcaf3e * depression, data = y0, id = lcaf3e, weights = ow3e, family = gaussian(), corstr = "exchangeable")
geemf3et4_ow <- geeglm(suicidality_t4 ~ lcaf3e * depression, data = y0, id = lcaf3e, weights = ow3et4, family = gaussian(), corstr = "exchangeable")


geemf3et4_owt <- geeglm(suicidality_t4 ~ lcaf3e * depression, data = y0, id = lcaf3e, weights = owt3et4, family = gaussian(), corstr = "exchangeable")

# by IPW trimmed 
geemf3e_ipwt <- geeglm(suicidality ~ lcaf3e * depression, data = y0, id = lcaf3e, weights = ipwt3e, family = gaussian(), corstr = "exchangeable")
geemf3et4_ipwt <- geeglm(suicidality_t4 ~ lcaf3e * depression, data = y0, id = lcaf3e, weights = ipwt3et4, family = gaussian(), corstr = "exchangeable")
#by IPW untrimmed
geemf3e_ipw <- geeglm(suicidality ~ lcaf3e * depression, data = y0, id = lcaf3e, weights = ipw3e, family = gaussian(), corstr = "exchangeable")


geemf3et4_ipw <- geeglm(suicidality_t4 ~ lcaf3e * depression, data = y0, id = lcaf3e, weights = ipw3et4, family = gaussian(), corstr = "exchangeable")

# by outcome regression
geemf3e_uw <- geeglm(suicidality ~ lcaf3e * depression + age_cat+edu_cat1 + gedu+reg_partner+hivtime + income_cat, data = y0, id = lcaf3e, family = gaussian(), corstr = "exchangeable")

geemf3et4_uw<- geeglm(suicidality_t4 ~ lcaf3e * depression+ age_cat+edu_cat1+reg_partner+ +hivtime + gedu, data = y0, id = lcaf3e, family = gaussian(), corstr = "exchangeable")

geemf3et4_uw2<- geeglm(suicidality_t4 ~ lcaf3e * depression+ age_cat+edu_cat1+reg_partner+ +hivtime + gedu + suicidality, data = y0, id = lcaf3e, family = gaussian(), corstr = "exchangeable")

tab_model(geemf3et4_uw2)

tab_model(geemf3e_untrimmed ,geemf3et4_untrimmed)
tab_model(geemf3e_ipw,geemf3et4_ipw)
tab_model(geemf3e_ipwt,geemf3et4_ipwt)
tab_model(geemf3e_uw,geemf3et4_uw)
install.packages("dotwhisker")
library(dotwhisker)
library(dplyr)
dwplot(list(geemf3et4_ow,geemf3et4_owt,geemf3et4_ipw,geemf3et4_ipwt,geemf3et4_uw,geemf3et4_uw2), show_stats = TRUE, stats_size = 5)

# Assuming your models are stored in these variables:
# geemf3et4_ow, geemf3et4_owt, geemf3et4_ipw, geemf3et4_ipwt, geemf3et4_uw, geemf3et4_uw2

# Step 1: Extract tidy versions of your model summaries
models_list <- list(geemf3et4_ow, geemf3et4_owt, geemf3et4_ipw, geemf3et4_ipwt, geemf3et4_uw, geemf3et4_uw2)
tidy_models <- lapply(models_list, broom::tidy)

# Assuming tidy_models is a list of data frames from broom::tidy() for each model
model_names <- c("geemf3et4_ow", "geemf3et4_owt", "geemf3et4_ipw", "geemf3et4_ipwt") # Names of the first four models

# Add a 'model' column to each data frame to distinguish between models
tidy_models_common <- lapply(1:4, function(i) {
  df <- tidy_models[[i]][tidy_models[[i]]$term %in% common_vars, ]
  df$model <- model_names[i] # Add model identifier
  return(df)
})

# Combine the processed outputs
combined_df <- do.call(rbind, tidy_models_common)

# Ensure the row names are unique after combining, as duplicate row names can cause issues
rownames(combined_df) <- NULL

# Now, plot using dwplot
dwplot(combined_df, show_stats = TRUE, stats_size = 5)
# Step 2: Identify common variables in the first four models
common_vars <- Reduce(intersect, lapply(tidy_models[1:4], function(df) df$term))

# Step 3: Subset each of the first four model's output to include only common variables
tidy_models_common <- lapply(tidy_models[1:4], function(df) df[df$term %in% common_vars, ])

# Step 4: Combine the processed outputs. You might need to add an identifier for each model if not already present
combined_df <- do.call(rbind, tidy_models_common)
# This assumes you have a way to distinguish between models in your combined_df, for example, by adding a model identifier column if it doesn't already exist

# Now, plot using dwplot
dwplot(combined_df, show_stats = TRUE, stats_size = 5)
#Sup table 5 ------------------------
# Define the variables to recode
library(dplyr)
vars_to_recode <- c("bullied_gay_1", "bullied_gen_1", "ipva_1", "amphetamine_1", "bingedrink1_1", "HIVstigma_median", "socialsupport_1q_1", "sexwork_provide_Yes_1", "income_low_1")

# Recode the variables and create new ones
for (var in vars_to_recode) {
  y0 <- y0 %>%
    mutate(!!paste0(var, "_re") := ifelse(.data[[var]] == 2, 1, ifelse(.data[[var]] == 1, 0, .data[[var]])))
}

# Reiterate preparation of the models list and extraction of tidy models
models_list <- list(geemf3et4_ow, geemf3et4_owt, geemf3et4_ipw, geemf3et4_ipwt, geemf3et4_uw, geemf3et4_uw2)
tidy_models <- lapply(models_list, broom::tidy)

# Identify common variables in the first four models
common_vars <- Reduce(intersect, lapply(tidy_models[1:4], function(df) df$term))

# Add a 'model' column and filter to common variables for the first four models
tidy_models_common <- lapply(1:4, function(i) {
  df <- tidy_models[[i]]
  df <- df[df$term %in% common_vars, ]
  df$model <- model_names[i]
  return(df)
})

# Combine the processed outputs, ensuring no empty data frames are included
combined_df <- do.call(rbind, lapply(tidy_models_common, function(df) if(nrow(df) > 0) df else NULL))

# Check if combined_df is not empty to proceed
if(nrow(combined_df) > 0) {
  # Ensure the row names are unique after combining
  rownames(combined_df) <- NULL
  
  # Plot using dwplot
  dwplot(combined_df, show_stats = TRUE, stats_size = 5)
} else {
  cat("No common variables found across the first four models or other issues encountered.")
}




# Define the recoded variables
recoded_vars <- paste0(vars_to_recode, "_re")
summary(y0$syndemic_sumscore )
y0$syndemic_sumscore_cat3<-ifelse(y0$syndemic_sumscore>=4,3,
                                    ifelse(y0$syndemic_sumscore<=3 & y0$syndemic_sumscore>=1)
# Calculate the sum score
y0$syndemic_sumscore <- rowSums(y0[recoded_vars], na.rm = TRUE)

# Calculate the sum score for each level of lcaf3a
y0 %>%
  group_by(lcaf3a) %>%
  summarise(syndemic_sumscore = mean(syndemic_sumscore, na.rm = TRUE),
suidicality_mean = mean(suicidality, na.rm = TRUE),
suicidality_t4_mean=mean(suicidality_t4,na.rm=TRUE))
y0 %>%
  group_by(lcaf3c) %>%
  summarise(syndemic_sumscore = mean(syndemic_sumscore, na.rm = TRUE),
            suidicality_mean = mean(suicidality, na.rm = TRUE),
            suicidality_t4_mean=mean(suicidality_t4,na.rm=TRUE))
y0 %>%
  group_by(lcaf3e) %>%
  summarise(syndemic_sumscore = mean(syndemic_sumscore, na.rm = TRUE),
            suidicality_mean = mean(suicidality, na.rm = TRUE),
            suicidality_t4_mean=mean(suicidality_t4,na.rm=TRUE))

table(y0$lcaf3c, y0$lcaf3e)


table(y0$lcaf3c,y0$lcaf3e)

# Calculate the mean scores for continuous variables
mean_suicidality <- aggregate(y0$suicidality, by = list(y0$lcaf3c, y0$lcaf3e), FUN = mean, na.rm = TRUE)
mean_suicidality_t4 <- aggregate(y0$suicidality_t4, by = list(y0$lcaf3c, y0$lcaf3e), FUN = mean, na.rm = TRUE)
prop_HIVstigma_median_0 <- aggregate(y0$HIVstigma_median_0, by = list(y0$lcaf3c, y0$lcaf3e), FUN = function(x) mean(x == 1))
mean_syndemic_sumscore <- aggregate(y0$syndemic_sumscore, by = list(y0$lcaf3c, y0$lcaf3e), FUN = mean, na.rm = TRUE)
# Calculate the proportions for binary variables
prop_low_income1 <- aggregate(y0$income_low_1, by = list(y0$lcaf3c, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop_depression <- aggregate(y0$depression, by = list(y0$lcaf3c, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop_sexwork_provide <- aggregate(y0$sexwork_provide_Yes, by = list(y0$lcaf3c, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop_socialsupport_1q <- aggregate(y0$socialsupport_1q, by = list(y0$lcaf3c, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop_bingedrink <- aggregate(y0$bingedrink1, by = list(y0$lcaf3c, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop_bullied_gay <- aggregate(y0$bullied_gay, by = list(y0$lcaf3c, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop_bullied_gen_1 <- aggregate(y0$bullied_gen_1, by = list(y0$lcaf3c, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop_ipva <- aggregate(y0$ipva, by = list(y0$lcaf3c, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop_reg_partner_Yes <- aggregate(y0$reg_partner_Yes, by = list(y0$lcaf3c, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop_amphetamine <- aggregate(y0$amphetamine, by = list(y0$lcaf3c, y0$lcaf3e), FUN = function(x) mean(x == 1))
# List of all tables
tables_lca32 <- list(mean_suicidality, mean_suicidality_t4, mean_syndemic_sumscore, prop_depression,
               prop_low_income1, prop_sexwork_provide, prop_HIVstigma_median_0,  prop_socialsupport_1q,                          prop_bullied_gay, prop_bullied_gen_bin, prop_ipva,
               prop_bingedrink, prop_amphetamine)

# Function to merge two tables
merge_tables_lca32 <- function(x, y) {
  merge(x, y, by = c("Group.1", "Group.2"), all = TRUE)
}

# Merge all tables
merged_table_lca32<- Reduce(merge_tables_lca32, tables_lca32)
# Print the merged table
mtable_lca32<-print(merged_table_lca32)
write.csv(mtable_lca32, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/mtable_lca32.csv", row.names = FALSE)

mean1_suicidality <- aggregate(y0$suicidality, by = list(y0$lcaf3a, y0$lcaf3e), FUN = mean, na.rm = TRUE)
mean1_suicidality_t4 <- aggregate(y0$suicidality_t4, by = list(y0$lcaf3a, y0$lcaf3e), FUN = mean, na.rm = TRUE)
prop1_HIVstigma_median_0 <- aggregate(y0$HIVstigma_median_0, by = list(y0$lcaf3a, y0$lcaf3e), FUN = function(x) mean(x == 1))
mean1_syndemic_sumscore <- aggregate(y0$syndemic_sumscore, by = list(y0$lcaf3a, y0$lcaf3e), FUN = mean, na.rm = TRUE)
# Calculate the proportions for binary variables
prop1_low_income1 <- aggregate(y0$income_low_1, by = list(y0$lcaf3a, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop1_depression <- aggregate(y0$depression, by = list(y0$lcaf3a, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop1_sexwork_provide <- aggregate(y0$sexwork_provide_Yes, by = list(y0$lcaf3a, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop1_socialsupport_1q <- aggregate(y0$socialsupport_1q, by = list(y0$lcaf3a, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop1_bingedrink <- aggregate(y0$bingedrink1, by = list(y0$lcaf3a, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop1_bullied_gay <- aggregate(y0$bullied_gay, by = list(y0$lcaf3a, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop1_bullied_gen_1 <- aggregate(y0$bullied_gen_1, by = list(y0$lcaf3a, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop1_ipva <- aggregate(y0$ipva, by = list(y0$lcaf3a, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop1_reg_partner_Yes <- aggregate(y0$reg_partner_Yes, by = list(y0$lcaf3a, y0$lcaf3e), FUN = function(x) mean(x == 1))
prop1_amphetamine <- aggregate(y0$amphetamine, by = list(y0$lcaf3a, y0$lcaf3e), FUN = function(x) mean(x == 1))
# List of all tables
tables_lca31 <- list(mean1_suicidality, mean1_suicidality_t4, mean1_syndemic_sumscore, prop1_depression,
                     prop1_low_income1, prop1_sexwork_provide, prop1_HIVstigma_median_0,prop1_socialsupport_1q,                          prop1_bullied_gay, prop1_bullied_gen_bin, prop1_ipva,
                     prop1_bingedrink, prop1_amphetamine)
library(DescTools)
table(y0$lcaf3a, y0$lcaf3e)

# Function to merge two tables
merge_tables_lca31 <- function(x, y) {
  merge(x, y, by = c("Group.1", "Group.2"), all = TRUE)
}

# Merge all tables
merged_table_lca31<- Reduce(merge_tables_lca31, tables_lca31)
# Print the merged table
mtable_lca31<-print(merged_table_lca31)
write.csv(mtable_lca31, "/Users/dougcheung/Library/Mobile Documents/com~apple~CloudDocs/Thomas Group/Syndemic secondary/data/mtable_lca31.csv", row.names = FALSE)

# Sup table 2 SMD------------------------------------
# Install and load the cobalt package
if (!require(cobalt)) {
  install.packages("cobalt")
}
library(cobalt)

# Calculate the SMD before weighting
pre_weights_smd_3e_lca <- bal.tab(lcaf3e ~ age_cat_15_25 + edu_cat1_Secondaryorbelow1 +  employ_Full_time + reg_partner_Yes   + hivtime_morethanayear + depression, 
                                  data = y0, 
                                  binary = "std")
# Calculate the SMD after OW weighting
post_weights_smd_3et4_lca <- bal.tab(lcaf3e ~ age_cat_15_25 + edu_cat1_Secondaryorbelow1 + employ_Full_time + reg_partner_Yes   + hivtime_morethanayear + depression, 
                                     data = y0, 
                                     binary = "std",weights=y0$cps345p_f3e_ow.wtm)
post_weights_smd_3et1_lca <- bal.tab(lcaf3e ~ age_cat_15_25 + edu_cat1_Secondaryorbelow1 + employ_Full_time + reg_partner_Yes   + hivtime_morethanayear + depression, 
                                     data = y0, 
                                     binary = "std",weights=y0$cps12p_f3e_ow.wtm)

post_ipw_smd_3et1_lca <- bal.tab(lcaf3e ~ age_cat_15_25 + edu_cat1_Secondaryorbelow1 + employ_Full_time + reg_partner_Yes   + hivtime_morethanayear + depression, 
                                 data = y0, 
                                 binary = "std",weights=y0$ipwt3et4)
print(pre_weights_smd_3e_lca)
print(post_weights_smd_3et4_lca)
print(post_weights_smd_3et1_lca)
print(post_ipw_smd_3et1_lca)