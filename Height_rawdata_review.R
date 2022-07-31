### HIMBA RAW HEIGHT DATA ANALYSIS ###

library(dplyr)
setwd("~/Desktop/Height")

#### Read In Final Dataset created below ###
height_raw_adults_filtered <- read.table("height_raw_adults_filtered_dbgap.txt", header=TRUE)
##############################

### ROH & Froh
# Read in hom files for roh
H3A_ROH500 <- read.table("H3Africa_height_snp50_missing2_het1_kb500.hom.indiv", header=TRUE)
H3A_ROH1500 <- read.table("H3Africa_height_snp50_missing2_het1_kb1500.hom.indiv", header=TRUE)
H3A_ROH5000 <- read.table("H3Africa_height_snp50_missing2_het1_kb5000.hom.indiv", header=TRUE)
MEGAx_ROH500 <- read.table("MegaEx_height_snp50_missing2_het1_kb500.hom.indiv", header=TRUE)
MEGAx_ROH1500 <- read.table("MegaEx_height_snp50_missing2_het1_kb1500.hom.indiv", header=TRUE)
MEGAx_ROH5000 <- read.table("MegaEx_height_snp50_missing2_het1_kb5000.hom.indiv", header=TRUE)

# calculate froh; denominator is the length of the genome in the bim files
H3A_FROH500 <- H3A_ROH500 %>%
  mutate(FROH_500 = (KB*1000)/2791433159) 
H3A_FROH1500 <- H3A_ROH1500 %>%
  mutate(FROH_1500 = (KB*1000)/2791433159) 
H3A_FROH5000 <- H3A_ROH5000 %>%
  mutate(FROH_5000 = (KB*1000)/2791433159)
MEGAx_FROH500 <- MEGAx_ROH500 %>%
  mutate(FROH_500 = (KB*1000)/2794139053)  
MEGAx_FROH1500 <- MEGAx_ROH1500 %>%
  mutate(FROH_1500 = (KB*1000)/2794139053)
MEGAx_FROH5000 <- MEGAx_ROH5000 %>%
  mutate(FROH_5000 = (KB*1000)/2794139053)

# Combine platforms
Himba_FROH500 <- rbind(H3A_FROH500, MEGAx_FROH500)
Himba_FROH1500 <- rbind(H3A_FROH1500, MEGAx_FROH1500)
Himba_FROH5000 <- rbind(H3A_FROH5000, MEGAx_FROH5000)
head(Himba_FROH500)
head(Himba_FROH1500)
head(Himba_FROH5000)

# Height data
#setwd("~/Desktop/")
height_raw <- read.table("himba_height_raw.txt", sep='\t', header=TRUE)
View(height_raw)

# ADULTS ONLY 18+, FILTERED OUT 2014 measurements (error), FILTERED OUT NA'S, OUTLIERS : FINAL DATASET  = HEIGHT_RAW_ADULTS_FILTERED ### 245 indivs

length(unique(height_raw$ID))

height_raw_adults_filtered <- height_raw %>%
  filter(YOB <= 1998) %>%
  filter(Year != 2014)

length(unique(height_raw_adults_filtered$ID))  ##271
View(height_raw_adults_filtered)

height_raw_adults_filtered <- height_raw_adults_filtered[!(height_raw_adults_filtered$YOB == 1998 & height_raw_adults_filtered$Year < 2016), ]
height_raw_adults_filtered <- height_raw_adults_filtered[!(height_raw_adults_filtered$YOB == 1997 & height_raw_adults_filtered$Year < 2015), ]
height_raw_adults_filtered <- height_raw_adults_filtered[!(height_raw_adults_filtered$YOB == 1996 & height_raw_adults_filtered$Year < 2014), ]
height_raw_adults_filtered <- height_raw_adults_filtered[!(height_raw_adults_filtered$YOB == 1995 & height_raw_adults_filtered$Year < 2013), ]
height_raw_adults_filtered <- height_raw_adults_filtered[!(height_raw_adults_filtered$YOB == 1994 & height_raw_adults_filtered$Year < 2012), ]
height_raw_adults_filtered <- height_raw_adults_filtered[!(height_raw_adults_filtered$YOB == 1993 & height_raw_adults_filtered$Year < 2011), ]

View(height_raw_adults_filtered)
length(unique(height_raw_adults_filtered$ID)) #270

# Add in HMB IDs, Froh, and Fosterage data
id_conversion <- read.table("Himba_ID_conversion.txt", header=TRUE)
head(id_conversion)
Fost <- read.table("Fosterage_data.txt", header=TRUE, sep="\t")
head(Fost)

height_raw_adults_filtered$HMB_ID = id_conversion$HMB_ID[match(height_raw_adults_filtered$ID, id_conversion$Scelza_ID)]

height_raw_adults_filtered$FROH_500 = Himba_FROH500$FROH_500[match(height_raw_adults_filtered$HMB_ID, Himba_FROH500$IID)]
height_raw_adults_filtered$FROH_1500 = Himba_FROH1500$FROH_1500[match(height_raw_adults_filtered$HMB_ID, Himba_FROH1500$IID)]
height_raw_adults_filtered$FROH_5000 = Himba_FROH5000$FROH_5000[match(height_raw_adults_filtered$HMB_ID, Himba_FROH5000$IID)]

height_raw_adults_filtered$Fosterage = Fost$Fosterage[match(height_raw_adults_filtered$ID, Fost$Participant_ID)]

View(height_raw_adults_filtered)

# Replace blank spaces with NAs
height_raw_adults_filtered[height_raw_adults_filtered==""] <- NA
View(height_raw_adults_filtered)

## Filter out NAs
height_raw_adults_filtered <- height_raw_adults_filtered %>%
  filter(FROH_1500 != 'NA') %>%
  filter(Fosterage != 'NA')
View(height_raw_adults_filtered)

## Number of Indivs
length(unique(height_raw_adults_filtered$ID)) ## n=246

### Remove Outliers +/-3 sd from Mean (1 male), Add in Mean Height Column  #####

raw_means <- height_raw_adults_filtered %>%
  group_by(HMB_ID) %>%  
  summarize(mean(Height))  
View(raw_means)
nrow(raw_means) ## 246 
raw_means$Sex = height_raw_adults_filtered$Sex[match(raw_means$HMB_ID, height_raw_adults_filtered$HMB_ID)]
View(raw_means)
mean(raw_means$`mean(Height)`)
sd <- sd(raw_means$`mean(Height)`)
dataset_mean_height <- mean(raw_means$`mean(Height)`)
outliers <- 3*sd
low_outliers <- dataset_mean_height - outliers 
low_outliers ### 145.3573
up_outliers <- dataset_mean_height + outliers 
up_outliers ### 192.1762

raw_means %>%
  filter(`mean(Height)` > up_outliers)  ## on raw_means, indiv HMB508 
raw_means %>%
  filter(`mean(Height)` < low_outliers)  ## nobody 

length(unique((height_raw_adults_filtered$ID))) ### 246
# filter out outlier:
height_raw_adults_filtered <- height_raw_adults_filtered %>%
  filter(HMB_ID != "HMB508")
length(unique((height_raw_adults_filtered$ID))) ### 245 


height_raw_adults_filtered$Mean_Height = raw_means$`mean(Height)`[match(height_raw_adults_filtered$HMB_ID, raw_means$HMB_ID)]
View(height_raw_adults_filtered)
length(unique(height_raw_adults_filtered$ID))

## Write out QCed dataset; this is the dataset included in GitHub, but with de-identified IDs to match the dbGaP IDs
#write.table(height_raw_adults_filtered, "height_raw_adults_filtered_dbgap.txt", sep=" ", row.names=FALSE, col.names=TRUE, quote=FALSE)


##### SUBSETTING #####
View(height_raw_adults_filtered)
length(unique(height_raw_adults_filtered$HMB_ID))

## only 1 measurement per indiv:
df = height_raw_adults_filtered[order(height_raw_adults_filtered[,'HMB_ID']),]
filtered_indiv_height_means <- df[!duplicated(df$HMB_ID),] 
View(filtered_indiv_height_means)
length(unique(filtered_indiv_height_means$HMB_ID))
nrow(filtered_indiv_height_means)
filtered_indiv_height_means <- filtered_indiv_height_means %>%
  select(HMB_ID, Sex, YOB, Fosterage, FROH_1500, Mean_Height)
View(filtered_indiv_height_means)
mean(filtered_indiv_height_means$Mean_Height) # 168.6655
mean(filtered_indiv_height_means$FROH_1500) # 0.02661421
# or
FROH_mean <- height_raw_adults_filtered %>%
  group_by(HMB_ID) %>%
  summarize(mean(FROH_1500)) #group_by so every ID only counted once
View(FROH_mean)
mean(FROH_mean$`mean(FROH_1500)`) # 0.02661421
##

Females_RAFilt <- height_raw_adults_filtered %>%
  filter(Sex == "Female") %>%  
  filter(Fosterage == "No" )  
View(Females_RAFilt)
height_mean_females <- Females_RAFilt %>%
  group_by(HMB_ID) %>%
  summarize(mean(Height))
View(height_mean_females)
mean(height_mean_females$`mean(Height)`) ##### 164.2821 - just for nonfostered females 

FemalesFost_RAfilt <- height_raw_adults_filtered %>%
  filter(Sex == "Female") %>%  
  filter(Fosterage == "Yes")  
View(FemalesFost_RAfilt)

AllFemales_RAfilt <- height_raw_adults_filtered %>%
  filter(Sex == "Female")
height_mean_AllFemales <- AllFemales_RAfilt %>%
  group_by(HMB_ID) %>%
  summarize(mean(Height))
View(height_mean_AllFemales)
mean(height_mean_AllFemales$`mean(Height)`) #### 164.2555  
sd(height_mean_AllFemales$`mean(Height)`) #### 5.243481
length(unique(AllFemales_RAfilt$ID)) ### 146 


Males_RAfilt <- height_raw_adults_filtered %>%
  filter(Sex == "Male") %>%  
  filter(Fosterage == "No")  
View(Males_RAfilt)
height_mean_males <- Males_RAfilt %>%
  group_by(HMB_ID) %>% 
  summarize(mean(Height))
View(height_mean_males)
mean(height_mean_males$`mean(Height)`) ##### 175.5098 - just for unfostered males (not included)

MalesFost_RAfilt <- height_raw_adults_filtered %>%
  filter(Sex == "Male") %>%  
  filter(Fosterage == "Yes")  
View(MalesFost_RAfilt)

AllMales_RAfilt <-height_raw_adults_filtered %>%
  filter(Sex == "Male")
height_mean_AllMales <- AllMales_RAfilt %>%
  group_by(HMB_ID) %>%
  summarize(mean(Height))
View(height_mean_AllMales)
length(unique(height_mean_AllMales$HMB_ID))
mean(height_mean_AllMales$`mean(Height)`) #### 175.1692 
sd(height_mean_AllMales$`mean(Height)`) #### 5.791261 


length(unique(AllMales_RAfilt$ID)) ### 99 
length(unique(MalesFost_RAfilt$ID)) ### 36 males fostered
length(unique(Males_RAfilt$ID)) #### 63 males NOT fostered
length(unique(Females_RAFilt$ID)) #### 115 females NOT fostered
length(unique(FemalesFost_RAfilt$ID)) ### 31 females fostered
length(unique(height_raw_adults_filtered$ID)) ### 245 total 

range(height_raw_adults_filtered$Year)

AllFost <- filtered_indiv_height_means %>%
  filter(Fosterage == "Yes")
head(AllFost)
mean(AllFost$Mean_Height) ### 169.7537 
AllNotFost <- filtered_indiv_height_means %>%
  filter(Fosterage == "No")
head(AllNotFost)
mean(AllNotFost$Mean_Height) ### 168.2559 
mean(AllFost$FROH_1500) ### 0.02711636 
mean(AllNotFost$FROH_1500) ### 0.02642519

mean(filtered_indiv_height_means$FROH_1500) ### 0.02661421 
#mean(FROH_mean$`mean(FROH_1500)`) #### 0.02661421 
#mean(AllFemales_RAfilt$FROH_1500) ### 0.02618
#mean(AllMales_RAfilt$FROH_1500) ### 0.02650
#mean(Females_RAFilt$FROH_1500) ### 0.02592
#mean(FemalesFost_RAfilt$FROH_1500) ### 0.027289
#mean(Males_RAfilt$FROH_1500) ### 0.02650297
#mean(MalesFost_RAfilt$FROH_1500) ### 0.02650

#######################################################################
# Number height observations per individual
library(dplyr)
obs <- height_raw_adults_filtered %>% group_by(HMB_ID) %>% 
  summarize(num_obs = length(Year)) %>% arrange(desc(num_obs))
obs %>% filter(num_obs == 1) %>% nrow() # n=134
obs %>% filter(num_obs == 2) %>% nrow() # n=72
obs %>% filter(num_obs == 3) %>% nrow() # n=35
obs %>% filter(num_obs == 4) %>% nrow() # n=4

# Age Range
range(height_raw_adults_filtered$YOB) # YOB range: 1919 - 1998
height_raw_adults_filtered %>% filter(YOB == 1919) # 1 male; 3 obs: '11, '12, '17
height_raw_adults_filtered %>% filter(YOB == 1998) # earliest obs = age 18 in 2016

# Measurements per year
height_raw_adults_filtered %>% group_by(Year) %>% 
  summarize(obs_per_yr = length(ID)) %>% arrange(desc(obs_per_yr)) # the most is n=180 in 2016

########################################
#### HISTOGRAM PLOTS: HEIGHT & FROH ###

female_indivheightmean <- filtered_indiv_height_means %>%
  filter(Sex == 'Female')
head(female_indivheightmean)
male_indivheightmean <- filtered_indiv_height_means %>%
  filter(Sex == 'Male')
head(male_indivheightmean)

newpurple <- rgb(0,0, 167, 127, max=255)
newpink <- rgb(204,0,0,127, max=255)

par(mfrow=c(1,2))
par(mar = c(5.1, 4.1, 2.1, 1.1), oma=c(0,0,0,0)) # mar=c(bottom, left, top, right)
hist(male_indivheightmean$Mean_Height, 
     main="",
     xlab="Height (cm)",
     ylim=c(0,60), xlim=c(145,200), col=rgb(0,0,167, 127, max=255))
title("A", adj=0)
hist(female_indivheightmean$Mean_Height, 
     col=rgb(204,0,0, 127, max=255), 
     add= TRUE)
legend("topright", legend=c("Males", "Females"), col=c(newpurple, newpink), pch=15, cex=0.8)
hist(filtered_indiv_height_means$FROH_1500,
     breaks=seq(0,0.09,0.005),
     xlab = expression("FROH 1500+ KB"),
     main="",
     col="#44AA99")
title("B", adj=0)
par(mfrow=c(1,1)) # return to single plot
par(mar = c(5.1, 4.1, 4.1, 2.1), oma=c(0,0,0,0)) # return to normal margins


shapiro.test(female_indivheightmean$Mean_Height) # Normal distribution of female heights
shapiro.test(male_indivheightmean$Mean_Height) # Normal distribution of male heights
shapiro.test(filtered_indiv_height_means$Mean_Height) # Normal dist of everyone heights
    
#col2rgb("#F0E442")
#col2rgb("#CC79A7")

###############################################################################
### R script to read the GRM binary file ###
# From Yang Lab: https://yanglab.westlake.edu.cn/software/gcta/#MakingaGRM

ReadGRMBin=function(prefix, AllN=F, size=4){
  sum_i=function(i){
    return(sum(1:i))
  }
  BinFileName=paste(prefix,".grm.bin",sep="")
  NFileName=paste(prefix,".grm.N.bin",sep="")
  IDFileName=paste(prefix,".grm.id",sep="")
  id = read.table(IDFileName)
  n=dim(id)[1]
  BinFile=file(BinFileName, "rb");
  grm=readBin(BinFile, n=n*(n+1)/2, what=numeric(0), size=size)
  NFile=file(NFileName, "rb");
  if(AllN==T){
    N=readBin(NFile, n=n*(n+1)/2, what=numeric(0), size=size)
  }
  else N=readBin(NFile, n=1, what=numeric(0), size=size)
  i=sapply(1:n, sum_i)
  return(list(diag=grm[i], off=grm[-i], id=id, N=N))
}

setwd("~/Desktop")
pre_grm <- ReadGRMBin("himba_final245", AllN=F, size=4)

############################
# Convert output to a matrix
sum_i=function(i){
  return(sum(1:i))
}

gcta2matrix<-function(diag,off,id){
  mat <- matrix(NA, nrow=length(id), ncol=length(id))
  for(i in c(1:length(id))){
    for(j in c(i:length(id))){
      if(i==j){
        mat[i,j] <- diag[i] # diag value for self-self value
      }else{ # j>i
        count_all<- sum_i(j-1) +i
        k <- count_all - (j-1)
        mat[i,j] <- off[k] # off-diag
        mat[j,i] <- mat[i,j]
      }
    }
  }
  colnames(mat) <- id
  rownames(mat) <- id
  return(mat)
}

gcta_grm<-gcta2matrix(pre_grm$diag, pre_grm$off, pre_grm$id$V2)
gcta_grm_lmekin <- bdsmatrix(dim(gcta_grm)[1], gcta_grm[lower.tri(gcta_grm, diag=T)], dimnames=list(pre_grm$id$V2,pre_grm$id$V2))

# bend the matrix to be positive definite
install.packages("vegan")
library(vegan)
is.positive.definite(gcta_grm)
original.eigen <- eigen(gcta_grm)
index.negative.eigen <- which(original.eigen$values<10e-4)
original.eigen$values[index.negative.eigen] <-  10e-4
new_gcta_grm <- round(original.eigen$vectors%*%diag(original.eigen$values)%*%solve(original.eigen$vectors), 5)
test <-mantel(gcta_grm, new_gcta_grm)

new_gcta_grm_lmekin <- bdsmatrix(dim(new_gcta_grm)[1], new_gcta_grm[lower.tri(new_gcta_grm, diag=T)], dimnames=list(pre_grm$id$V2,pre_grm$id$V2))

#########################
## Log transform FROH ##

temp <- filtered_indiv_height_means %>% # use this df because only 1 row per individual(i.e. not multiple height measurements per indiv so Froh distribution will be true, not skewed)
  mutate(log_froh1500 = log(FROH_1500))
hist(temp$log_froh1500, xlab="log(Froh1500)", main="")
shapiro.test(temp$log_froh1500) # p=0.0004

# Log-transform Froh in dataset for models (with all height measurements)
height_raw_adults_filtered <- height_raw_adults_filtered %>%
  mutate(log_froh500 = log(FROH_500)) %>%
  mutate(log_froh1500 = log(FROH_1500)) %>%
  mutate(log_froh5000 = log(FROH_5000))
# Cannot take log of 0 (becomes -inf) but some indivs have Froh_5000 values of 0, so replace the "-inf" with a value slightly more negative than the most negative log(FROH_5000) value (-6.286...)
height_raw_adults_filtered$log_froh5000[height_raw_adults_filtered$log_froh5000=="-Inf"] <- -6.3

############################################################
### REVISED LINEAR MODELS #####
################################
install.packages("coxme")
library(coxme)

# Note: HMB_ID is the same as ID (same person) but had to change (1|ID) to (1|HMB_ID) because the GRM uses HMB_IDs

height_baseline.lmekin <- lmekin(Height ~ 1+(1|HMB_ID) + YOB + Sex*Fosterage, data=height_raw_adults_filtered, varlist = new_gcta_grm_lmekin)
height_baseline.lmekin

height_raw_Froh1500.lmekin <- lmekin(Height ~ 1+(1|HMB_ID) + log_froh1500 + YOB + Sex*Fosterage, data=height_raw_adults_filtered, varlist = new_gcta_grm_lmekin)
height_raw_Froh1500.lmekin
# de-transform logged value effect size
exp(0.020050016) # 1.020252

# Other ROH thresholds
height_raw_Froh500.lmekin <- lmekin(Height ~ 1+(1|HMB_ID) + log_froh500 + YOB + Sex*Fosterage, data=height_raw_adults_filtered, varlist = new_gcta_grm_lmekin)
height_raw_Froh500.lmekin

height_raw_Froh5000.lmekin <- lmekin(Height ~ 1+(1|HMB_ID) + log_froh5000 + YOB + Sex*Fosterage, data=height_raw_adults_filtered, varlist = new_gcta_grm_lmekin)
height_raw_Froh5000.lmekin

####################################################
#### PLOTTING MODELS ##### 
########################

library(tidyverse)
library(lme4)
library(lmerTest)

# lmekin model cannot be plotted (i.e. used to get predicted values), so used same mixed effects model without GRM just for plotting lines
model_froh.lmer <- lmer(Height ~ 1+(1|HMB_ID) + log_froh1500 + YOB + Sex*Fosterage, data=height_raw_adults_filtered)

height_raw_adults_filtered$pred <- predict(model_froh.lmer)

Females_RAFilt <- height_raw_adults_filtered %>%
  filter(Sex == "Female") %>%  
  filter(Fosterage == "No" )
FemalesFost_RAfilt <- height_raw_adults_filtered %>%
  filter(Sex == "Female") %>%  
  filter(Fosterage == "Yes")  
Males_RAfilt <- height_raw_adults_filtered %>%
  filter(Sex == "Male") %>%  
  filter(Fosterage == "No")  
MalesFost_RAfilt <- height_raw_adults_filtered %>%
  filter(Sex == "Male") %>%  
  filter(Fosterage == "Yes")  

height_froh_scatterplot <- ggplot(Males_RAfilt, aes(FROH_1500, Height)) + geom_point(color="#56B4E9", pch=1) + 
  geom_point(data=MalesFost_RAfilt, aes(FROH_1500, Height), color="#F0E442", pch=17) +
  geom_point(data=Females_RAFilt, aes(FROH_1500, Height), color="#CC79A7", pch=1) +
  geom_point(data=FemalesFost_RAfilt, aes(FROH_1500, Height), color="#E69F00", pch=17) +
  scale_x_continuous(name="FROH 1500") +
  
  geom_smooth(data=Males_RAfilt, aes(FROH_1500, pred), color="#56B4E9", method="glm") +
  geom_smooth(data=MalesFost_RAfilt, aes(FROH_1500, pred), color="#F0E442", method="glm") +
  geom_smooth(data=Females_RAFilt, aes(FROH_1500, pred), color="#CC79A7", method="glm") +
  geom_smooth(data=FemalesFost_RAfilt, aes(FROH_1500, pred), color="#E69F00", method="glm")

# create to grab the legend only
legend_for_plot <- ggplot(Males_RAfilt, aes(FROH_1500, Height, color="#56B4E9")) + geom_point(pch=1) + 
  geom_point(data=MalesFost_RAfilt, aes(FROH_1500, Height, color="#F0E442"), pch=17) +
  geom_point(data=Females_RAFilt, aes(FROH_1500, Height, color="#CC79A7"), pch=1) +
  geom_point(data=FemalesFost_RAfilt, aes(FROH_1500, Height, color="#E69F00"), pch=17) +
  scale_color_manual(name="Himba Adults",
                     breaks=c("Males", "Males Fostered", "Females", "Females Fostered"),
                     values=c("Males"="#56B4E9", "Males Fostered"="#F0E442", "Females"="#CC79A7", "Females Fostered"="#E69F00"))

library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
} # legend function to grab legend from another plot
legend_height_scatterplot <- get_legend(legend_for_plot) # grab legend

grid.arrange(height_froh_scatterplot, legend_height_scatterplot, ncol=2, widths=c(4.5, 1)) # plot the scatter plot and legend

#######################################################################
### Thin Test (from Swinford et al 2022) ###
##### THIN TEST & RMSE #####

setwd("~/Desktop/UC Davis/HENN LAB/Himba Project/F_ROH/DifFroh_Analysis")

thinned_1 <- read.table("1_ThinTest_snp50_missing2_het1_kb1500.hom.indiv", header=TRUE)
thinned_2 <- read.table("2_ThinTest_snp50_missing2_het1_kb1500.hom.indiv", header=TRUE)
thinned_3 <- read.table("3_ThinTest_snp50_missing2_het1_kb1500.hom.indiv", header=TRUE)
thinned_4 <- read.table("4_ThinTest_snp50_missing2_het1_kb1500.hom.indiv", header=TRUE)
thinned_5 <- read.table("5_ThinTest_snp50_missing2_het1_kb1500.hom.indiv", header=TRUE)
thinned_6 <- read.table("6_ThinTest_snp50_missing2_het1_kb1500.hom.indiv", header=TRUE)
thinned_7 <- read.table("7_ThinTest_snp50_missing2_het1_kb1500.hom.indiv", header=TRUE)
thinned_8 <- read.table("8_ThinTest_snp50_missing2_het1_kb1500.hom.indiv", header=TRUE)
thinned_9 <- read.table("9_ThinTest_snp50_missing2_het1_kb1500.hom.indiv", header=TRUE)
thinned_10 <- read.table("10_ThinTest_snp50_missing2_het1_kb1500.hom.indiv", header=TRUE)

thin1_froh <- thinned_1 %>%
  mutate(Froh1_1500 = (KB*1000)/2787160584)
thin2_froh <- thinned_2 %>%
  mutate(Froh2_1500 = (KB*1000)/2789881009)
thin3_froh <- thinned_3 %>%
  mutate(Froh3_1500 = (KB*1000)/2790651978)
thin4_froh <- thinned_4 %>%
  mutate(Froh4_1500 = (KB*1000)/2790821199)
thin5_froh <- thinned_5 %>%
  mutate(Froh5_1500 = (KB*1000)/2790972299)
thin6_froh <- thinned_6 %>%
  mutate(Froh6_1500 = (KB*1000)/2790291177)
thin7_froh <- thinned_7 %>%
  mutate(Froh7_1500 = (KB*1000)/2787645606)
thin8_froh <- thinned_8 %>%
  mutate(Froh8_1500 = (KB*1000)/2791488892)
thin9_froh <- thinned_9 %>%
  mutate(Froh9_1500 = (KB*1000)/2787721777)
thin10_froh <- thinned_10 %>%
  mutate(Froh10_1500 = (KB*1000)/2790940619)

ThinTest_Froh <- H3Africa_auto_Froh %>%
  select(IID, Froh_1500)
ThinTest_Froh$Froh1_1500 = thin1_froh$Froh1_1500[match(ThinTest_Froh$IID, thin1_froh$IID)]
ThinTest_Froh$Froh2_1500 = thin2_froh$Froh2_1500[match(ThinTest_Froh$IID, thin2_froh$IID)]
ThinTest_Froh$Froh3_1500 = thin3_froh$Froh3_1500[match(ThinTest_Froh$IID, thin3_froh$IID)]
ThinTest_Froh$Froh4_1500 = thin4_froh$Froh4_1500[match(ThinTest_Froh$IID, thin4_froh$IID)]
ThinTest_Froh$Froh5_1500 = thin5_froh$Froh5_1500[match(ThinTest_Froh$IID, thin5_froh$IID)]
ThinTest_Froh$Froh6_1500 = thin6_froh$Froh6_1500[match(ThinTest_Froh$IID, thin6_froh$IID)]
ThinTest_Froh$Froh7_1500 = thin7_froh$Froh7_1500[match(ThinTest_Froh$IID, thin7_froh$IID)]
ThinTest_Froh$Froh8_1500 = thin8_froh$Froh8_1500[match(ThinTest_Froh$IID, thin8_froh$IID)]
ThinTest_Froh$Froh9_1500 = thin9_froh$Froh9_1500[match(ThinTest_Froh$IID, thin9_froh$IID)]
ThinTest_Froh$Froh10_1500 = thin10_froh$Froh10_1500[match(ThinTest_Froh$IID, thin10_froh$IID)]
head(ThinTest_Froh)

# PLOT OF THIN TESTS (Looks good!)
plot(ThinTest_Froh$Froh_1500, ThinTest_Froh$Froh1_1500,
     xlab="FROH (original thinned set)",
     ylab="FROH: Thin Tests (n=10)",
     main="Individual Comparison of Multiple Thin Tests (plink)")
points(ThinTest_Froh$Froh_1500, ThinTest_Froh$Froh2_1500, col="gold")
points(ThinTest_Froh$Froh_1500, ThinTest_Froh$Froh3_1500, col="red")
points(ThinTest_Froh$Froh_1500, ThinTest_Froh$Froh4_1500, col="blue")
points(ThinTest_Froh$Froh_1500, ThinTest_Froh$Froh5_1500, col="purple")
points(ThinTest_Froh$Froh_1500, ThinTest_Froh$Froh6_1500, col="pink")
points(ThinTest_Froh$Froh_1500, ThinTest_Froh$Froh7_1500, col="turquoise")
points(ThinTest_Froh$Froh_1500, ThinTest_Froh$Froh8_1500, col="green")
points(ThinTest_Froh$Froh_1500, ThinTest_Froh$Froh9_1500, col="gray50")
points(ThinTest_Froh$Froh_1500, ThinTest_Froh$Froh10_1500, col="magenta")
legend("topleft", legend=c("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9", "test 10"), col=c("black", "gold", "red", "blue", "purple", "pink", "turquoise", "green", "gray50", "magenta"), pch=1, cex=0.8)

# Set up for RMSE Calculation
ThinTest_FrohDifs <- ThinTest_Froh %>%
  mutate(Dif1 = abs(Froh1_1500-Froh_1500)) %>%
  mutate(Dif2 = abs(Froh2_1500-Froh_1500)) %>%
  mutate(Dif3 = abs(Froh3_1500-Froh_1500)) %>%
  mutate(Dif4 = abs(Froh4_1500-Froh_1500)) %>%
  mutate(Dif5 = abs(Froh5_1500-Froh_1500)) %>%
  mutate(Dif6 = abs(Froh6_1500-Froh_1500)) %>%
  mutate(Dif7 = abs(Froh7_1500-Froh_1500)) %>%
  mutate(Dif8 = abs(Froh8_1500-Froh_1500)) %>%
  mutate(Dif9 = abs(Froh9_1500-Froh_1500)) %>%
  mutate(Dif10 = abs(Froh10_1500-Froh_1500)) %>%
  mutate(AvgDif = rowMeans(.[, 13:22]))
ExpectedDif <- rep(0, 504)
ThinTest_FrohDifs <- cbind(ThinTest_FrohDifs, ExpectedDif)
View(ThinTest_FrohDifs)

## RMSE Calculation! ##
#install.packages("Metrics")
library(Metrics)
rmse(ThinTest_FrohDifs$AvgDif, ThinTest_FrohDifs$ExpectedDif) # RMSE = 0.00058
###


####################################################
### Power Calculation ###
#########################

# Use filtered_indiv_height_means dataset: only one row per individuals and uses mean heights per indiv

install.packages("data.table")
library(data.table)

sim_height <- function(size=250, # sample size
                       froh.data = NA, # a vector of simulated fROH in individuals
                       h2.froh = 0.004, # proportion of height variance explained by fROH
                       beta.froh = -9){ # effect size by fROH (unit as 100%)
  height_froh <- froh.data*beta.froh
  height_rest <- rnorm(size, 0, sd = sqrt(var(height_froh)*(1-h2.froh)/h2.froh))
  height <- height_froh + height_rest
  return(height)
}

n <- 250 # assign a sample to n
h2.froh <- 0.004 # proportion of variance of height explained by froh
sd_height <- sd(filtered_indiv_height_means$Mean_Height)
mean_height <- mean(filtered_indiv_height_means$Mean_Height)
froh.effect <- -0.012*100*sd_height # McQuillan et al., scale unit of rROH from 1% to 1 

# Simulate froh by random sampling from the current froh distribution
pwrs2 <- c()
for(j in c(1:10)){
  ps <- c() # record p-val
  for(i in c(1:1000)){
    sim.froh <- sample(filtered_indiv_height_means$FROH_1500, size=n, replace=T)
    sim.height <- sim_height(size=n, froh.data=sim.froh,
                             h2.froh=h2.froh, 
    )
    p <- as.numeric(summary(lm(sim.height~sim.froh))$coefficients[2,4])
    ps <- c(ps, p)
  }
  pwr <- length(which(ps<0.05))/ length(ps)
  pwrs2 <- c(pwrs2, pwr)
}

pwrs2
range(pwrs2)

# Results with 10 reps
# N=250: 15~20%
# N=1000: 50~53%
# N=2000: 80~84%
# For ~80% 

#############################
### Check a single time point

# We know that not all years have the same number of measurements...which one had the most? 2016?
height_raw_adults_filtered %>% group_by(Year) %>% 
  summarize(num = length(HMB_ID)) %>% arrange(desc(num))
# 2016 is n=180, 2017 is n=68, continues descending

# Filter for year=2016 only

heights_2016 <- height_raw_adults_filtered %>%
  filter(Year == 2016)

# Use PCs because other function will not work without a random effect (i.e. with ID), so cannot use the GRM
pcs <- read.table("~/Dropbox/Natalie'sStuff/HimbaHeight_BrennaG/himba_height_3pcs.txt", header=T)
head(pcs)
heights_2016$PC1 = pcs$pc1[match(heights_2016$HMB_ID, pcs$IID)]
heights_2016$PC2 = pcs$pc2[match(heights_2016$HMB_ID, pcs$IID)]
heights_2016$PC3 = pcs$pc3[match(heights_2016$HMB_ID, pcs$IID)]

# model for single time point (2016)
heights_2016.lm <- lm(Height ~ log_froh1500 + YOB + Sex*Fosterage + PC1 + PC2 + PC3, data=heights_2016)
summary(heights_2016.lm) # still no significance

### Variance in height
indiv_height_var <- height_raw_adults_filtered %>% group_by(HMB_ID) %>% 
  summarize(indiv_var = var(Height)) %>% arrange(desc(indiv_var)) %>% 
  filter(!is.na(indiv_var))
hist(indiv_height_var$indiv_var,
     breaks=seq(0,100,2))
View(indiv_height_var)
median(indiv_height_var$indiv_var) # 0.72
mean(indiv_height_var$indiv_var) # 3.12

test_remOutlier <- height_raw_adults_filtered[-38,]
test_remOutlier %>% filter(HMB_ID == "HMB037")

test_remOutlier.lmekin <- lmekin(Height ~ 1+(1|HMB_ID) + log_froh1500 + YOB + Sex*Fosterage, data=test_remOutlier, varlist = new_gcta_grm_lmekin)
test_remOutlier.lmekin # No change after removing extreme outlier
