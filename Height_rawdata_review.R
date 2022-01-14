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


##### LINEAR MODELS ######
height_baseline.lm <- lm(Height ~ 1+(1|ID) + YOB + Sex*Fosterage, data=height_raw_adults_filtered)
summary(height_baseline.lm)

height_raw_Froh1500.lm <- lm(Height ~ 1+(1|ID) + FROH_1500 + YOB + Sex*Fosterage, data=height_raw_adults_filtered)
summary(height_raw_Froh1500.lm)

# Models with other ROH thresholds
height_raw_Froh500.lm <- lm(Height ~ 1+(1|ID) + FROH_500 + YOB + Sex*Fosterage, data=height_raw_adults_filtered)
summary(height_raw_Froh500.lm)
height_raw_Froh5000.lm <- lm(Height ~ 1+(1|ID) + FROH_5000 + YOB + Sex*Fosterage, data=height_raw_adults_filtered)
summary(height_raw_Froh5000.lm)


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

#### PLOTTING ##### 
#install.packages("remotes")
#remotes::install_github("ashenoy-cmbi/grafify@*release")

plot(Males_RAfilt$FROH_1500, Males_RAfilt$Height, xlab=expression("F"[ROH]*" 1500"), ylab="Height (cm)", xlim=c(0.00, 0.1), ylim=c(145,200), col="#56B4E9", pch=1, main="")
points(MalesFost_RAfilt$FROH_1500, MalesFost_RAfilt$Height, pch=17, col="#F0E442")
points(Females_RAFilt$FROH_1500, Females_RAFilt$Height, pch=1, col="#CC79A7")
points(FemalesFost_RAfilt$FROH_1500, FemalesFost_RAfilt$Height, pch=17, col="#E69F00")
points(height_raw_adults_filtered$FROH_1500, predict(height_raw_Froh1500.lm), pch=16, col="#000000")
legend("topright", legend=c("Females", "Females Fostered", "Males", "Males Fostered"),col=c("#CC79A7", "#E69F00", "#56B4E9", "#F0E442"), pch=c(1,17,1,17))

female_indivheightmean <- filtered_indiv_height_means %>%
  filter(Sex == 'Female')
head(female_indivheightmean)
male_indivheightmean <- filtered_indiv_height_means %>%
  filter(Sex == 'Male')
head(male_indivheightmean)

hist(male_indivheightmean$Mean_Height, main="", xlab="Height (cm)", ylim=c(0,60), xlim=c(145,200), col=rgb(0,0,167, 127, max=255))
hist(female_indivheightmean$Mean_Height, col=rgb(204,0,0, 127, max=255), add= TRUE)

newpurple <- rgb(0,0, 167, 127, max=255)
newpink <- rgb(204,0,0,127, max=255)

legend("topright", legend=c("Males", "Females"), col=c(newpurple, newpink), pch=15)

shapiro.test(female_indivheightmean$Mean_Height) # Normal distribution of female heights
shapiro.test(male_indivheightmean$Mean_Height) # Normal distribution of male heights
shapiro.test(filtered_indiv_height_means$Mean_Height) # Normal dist of everyone heights
    
#col2rgb("#F0E442")
#col2rgb("#CC79A7")
       
       