# Uploading all the data, subsetting eye only, processing further... 

AUSNZ <- read.csv2("Eye_CZ_PC_PROLIFIC_AUSTRALIA_NZ.csv")
# AUSNZ2 <- read.csv2("Face_CZ_PC_PROLIFIC_AUSTRALIA_NZ_Checked_RUN_2.csv",T)
# AUS_NZ <- rbind.data.frame(AUSNZ1, AUSNZ2) 
# rm(AUSNZ,AUSNZ2)

COL <- read.csv("COL_Faces_eyes_27_09_25.csv",T)
CZ <- read.csv("CZ_Faces_eyes_14_04_25.csv",T)
RSA <- read.csv2("RJA_CZ_eye_SocMed.csv",T)
TUR <- read.csv("TR_Faces_eyes_14_04_25.csv",T)
VN <- read.csv("VN_Faces_eyes_14_04_25.csv",T)


# Eyes are the only interest of now: 
# Take out all the raters who rated faces...
Pref_levels <- c("CZ_16_F_eye", "CZ_16_M_eye", "CZ_19_F_eye","CZ_19_M_eye")

# Colombia: 
IDS <- COL$Rec_Session_Id[COL$Task_Name %in% Pref_levels]

length(levels(as.factor(IDS)))
length(levels(as.factor(COL$Rec_Session_Id))) # All right. It seems to work! 

# Now - subset: 
COL_eyes <- COL[COL$Rec_Session_Id %in% IDS,]

levels(as.factor(COL_eyes$Passport.nationality))

# Czechia:
IDS <- CZ$Rec_Session_Id[CZ$Task_Name %in% Pref_levels]
length(levels(as.factor(IDS)))
length(levels(as.factor(CZ$Rec_Session_Id))) # All right. It seems to work! 
levels(as.factor(CZ$Block_Name)) 
CZ_eyes <- CZ[CZ$Rec_Session_Id %in% IDS,]


# AUS - NZ
IDS <- AUSNZ$Rec_Session_Id[AUSNZ$Task_Name %in% Pref_levels]
levels(as.factor(AUSNZ$Block_Name)) 

length(levels(as.factor(IDS)))
length(levels(as.factor(AUSNZ$Rec_Session_Id))) # All rated eyes (it's correct, they came from Prolific 
# and the survey was designed like this)

# Now - subset: 
AUS_NZ_eyes <- AUSNZ[AUSNZ$Rec_Session_Id %in% IDS,]


# RSA 
IDS <- RSA$Rec_Session_Id1[RSA$Task_Name %in% Pref_levels]
levels(as.factor(RSA$Block_Name)) 

length(levels(as.factor(IDS)))
length(levels(as.factor(RSA$Rec_Session_Id1))) # SOme rejected before getting there (see below) Or did not 
# fill in correctly the quick quiz
RSA[!RSA$Rec_Session_Id1 %in% IDS,]

# Now - subset: 
RSA_eyes <- RSA[RSA$Rec_Session_Id1 %in% IDS,]

# TUR
IDS <- TUR$Rec_Session_Id[TUR$Task_Name %in% Pref_levels]
levels(as.factor(TUR$Block_Name)) 

length(levels(as.factor(IDS)))
length(levels(as.factor(TUR$Rec_Session_Id))) # All right. It seems to work! 

# Now - subset: 
TUR_eyes <- TUR[TUR$Rec_Session_Id %in% IDS,]


# VN 
IDS <- VN$Rec_Session_Id[VN$Task_Name %in% Pref_levels]
levels(as.factor(VN$Block_Name)) 

length(levels(as.factor(IDS)))
length(levels(as.factor(VN$Rec_Session_Id))) # All right. It seems to work! 

# Now - subset: 
VN_eyes <- VN[VN$Rec_Session_Id %in% IDS,]


# Specify the objects you want to keep
objects_to_keep <- c("AUS_NZ_eyes", "COL_eyes", "CZ_eyes", "RSA_eyes", "TUR_eyes", "VN_eyes", "Pref_levels")  # Replace with your object names

# Remove all other objects
rm(list = setdiff(ls(), objects_to_keep))


# Now keep only the same-name columns that are relevant
# Rename the columns that are relevant so that the tables can be combined (long format)
colnames(AUS_NZ_eyes)[1:7]
colnames(COL_eyes)[1:7]
colnames(CZ_eyes)[1:7]
colnames(RSA_eyes)[1:7]   
colnames(RSA_eyes)[1:7] <- colnames(CZ_eyes)[1:7] 
colnames(RSA_eyes)[1:7]   

colnames(TUR_eyes)[1:7]
colnames(VN_eyes)[1:7]

colnames(AUS_NZ_eyes)[30:33]
colnames(COL_eyes)[38:41]
colnames(CZ_eyes)[40:43]

colnames(RSA_eyes)[29:32]
colnames(RSA_eyes)[29:32]<-colnames(AUS_NZ_eyes)[30:33]    
colnames(RSA_eyes)[29:32]

colnames(TUR_eyes)[38:41]
colnames(VN_eyes)[38:41]
 
# Score test - not needed, it would exclude the wrong participants automatically 
# WHY: those who scored less than 3 out of five maximum possible points were not allowed to continue.
# Instead focus on the attention checks: 
colnames(AUS_NZ_eyes)[22:24] # No butter
colnames(AUS_NZ_eyes)[22] <- "Attention_Check_Butter"
colnames(AUS_NZ_eyes)[22:24]
colnames(COL_eyes)[c(26,27,32)]
colnames(CZ_eyes)[c(26,27,32)] 
colnames(RSA_eyes)[22:24] # No butter
colnames(RSA_eyes)[22:24]<- colnames(COL_eyes)[c(26,27,32)]
colnames(RSA_eyes)[22:24] 
colnames(TUR_eyes)[c(26,27,32)] <- colnames(COL_eyes)[c(26,27,32)]
colnames(TUR_eyes)[c(26,27,32)]
colnames(VN_eyes)[c(26,27,32)]

# And to see who completed the survey: 
colnames(AUS_NZ_eyes[108]) 
colnames(COL_eyes[128])
colnames(CZ_eyes[127]) 
colnames(RSA_eyes[104]) 
colnames(RSA_eyes)[104] <- "Completed"
colnames(TUR_eyes[130])
colnames(VN_eyes[128])

# TOK = temporarily OK 
colnames(AUS_NZ_eyes)[c(1:7,30:33,22:24,108)]
AUS_NZ_eyes_TOK <- AUS_NZ_eyes[,c(1:7,30:33,22:24,108)]

colnames(COL_eyes)[c(1:7,38:41,26,27,32,128)]
COL_eyes_TOK <- COL_eyes[c(1:7,38:41,26,27,32,128)]

colnames(CZ_eyes)[c(1:7,40:43,26,27,32,127)]
CZ_eyes_TOK <- CZ_eyes[c(1:7,40:43,26,27,32,127)]

colnames(RSA_eyes)[c(1:7,29:32,22:24,104)]
RSA_eyes_TOK <- RSA_eyes[c(1:7,29:32,22:24,104)]

colnames(TUR_eyes)[c(1:7,38:41,26,27,32,130)]
TUR_eyes_TOK <- TUR_eyes[c(1:7,38:41,26,27,32,130)]

colnames(VN_eyes)[c(1:7,38:41,26,27,32,128)]
VN_eyes_TOK <- VN_eyes[c(1:7,38:41,26,27,32,128)]


# Getting & Adding demography: 
# We also need to get rid of "test" if any survived
demography_names <- c("height", 
                      "weight",
                      "__42__",
                      "__45__",
                      "Sex_ATB",
                      "Mother_Tongue",
                      "PasspNat",
                      "Self_Ethnicity",
                      "Age",
                      "Family_SOC",
                      "Travel_Abroad",
                      "People_Diff_Eth",
                      "Marital_Stat",
                      "USE_SM",
                      "HowOftenPassive",
                      "HowOftenPosting",
                      "HowOftenLiking",
                      "AP_Facebook",
                      "AP_Instagram",
                      "AP_TikTok",
                      "AP_TwitterX",
                      "AP_YouTube",
                      "How_freq_Facebook",
                      "How_freq_Instagram",
                      "How_freq_TikTok",
                      "How_freq_TwitterX",
                      "How_freq_YouTube",
                      "SNS_FOMO",
                      "SNS_online_better_friends",
                      "SNS_vs_other_hobbies",
                      "SNS_too_much_time"
  
)

# Adding a file that denotes where are the columns that are relevant for our affairs (done externally in Excel as far 
# as I remember).
COLS <- read.csv2("Demography_loc.csv",T)

AUS_N <- as.numeric(COLS$AUS_NZ)
COL_N <- as.numeric(COLS$Colombia)
CZ_N <- as.numeric(COLS$Czechia)
RSA_N <- as.numeric(COLS$RSA)
TR_N <- as.numeric(COLS$TUR)
VN_N <- as.numeric(COLS$VN)


head(AUS_NZ_eyes[,c(1,AUS_N)])
head(COL_eyes[,c(1,COL_N)])
head(CZ_eyes[,c(1,CZ_N)])
head(RSA_eyes[,c(1,RSA_N)])
head(TUR_eyes[,c(1,TR_N)])
head(VN_eyes[,c(1,VN_N)])

AUS_DEM <- as.data.frame(AUS_NZ_eyes[,c(1,as.numeric(AUS_N))])
colnames(AUS_DEM)[2:ncol(AUS_DEM)] <- demography_names

COL_DEM <- as.data.frame(COL_eyes[,c(1,as.numeric(COL_N))])
colnames(COL_DEM)[2:ncol(COL_DEM)] <- demography_names

CZ_DEM <- as.data.frame(CZ_eyes[,c(1,as.numeric(CZ_N))])
colnames(CZ_DEM)[2:ncol(CZ_DEM)] <- demography_names

RSA_DEM <- as.data.frame(RSA_eyes[,c(1,as.numeric(RSA_N))])
colnames(RSA_DEM)[2:ncol(RSA_DEM)] <- demography_names

TUR_DEM <- as.data.frame(TUR_eyes[,c(1,as.numeric(TR_N))])
colnames(TUR_DEM)[2:ncol(TUR_DEM)] <- demography_names

VN_DEM <- as.data.frame(VN_eyes[,c(1,as.numeric(VN_N))])
colnames(VN_DEM)[2:ncol(VN_DEM)] <- demography_names



DEM_DEM <- rbind.data.frame(
  AUS_DEM,
  COL_DEM,
  CZ_DEM,
  RSA_DEM,
  TUR_DEM,
  VN_DEM
)

DEM_DEM[DEM_DEM == ""] <- NA

# num_aus <- length(levels(as.factor(AUS_NZ_eyes$Rec_Session_Id)))
# num_col <- length(levels(as.factor(COL_eyes$Rec_Session_Id)))
# num_cz <- length(levels(as.factor(CZ_eyes$Rec_Session_Id)))
# num_rsa <- length(levels(as.factor(RSA_eyes$Rec_Session_Id)))
# num_tur <- length(levels(as.factor(TUR_eyes$Rec_Session_Id)))
# num_vn <- length(levels(as.factor(VN_eyes$Rec_Session_Id)))

num_aus <- nrow(AUS_NZ_eyes)
num_col <- nrow(COL_eyes)
num_cz <- nrow(CZ_eyes)
num_rsa <- nrow(RSA_eyes)
num_tur <- nrow(TUR_eyes)
num_vn <- nrow(VN_eyes)

DEM_DEM$culture <- c(rep("AUS_NZ",num_aus),
                     rep("COL",num_col),
                     rep("CZ",num_cz),
                     rep("RSA",num_rsa),
                     rep("TUR",num_tur),
                     rep("VN",num_vn)
)


# ------------
colnames(COL_eyes_TOK)
colnames(CZ_eyes_TOK)<-colnames(COL_eyes_TOK) 
colnames(AUS_NZ_eyes_TOK)
colnames(RSA_eyes_TOK)
colnames(TUR_eyes_TOK)
colnames(VN_eyes_TOK)



All_EYES <- rbind.data.frame(
  AUS_NZ_eyes_TOK,
  COL_eyes_TOK,
  CZ_eyes_TOK,
  RSA_eyes_TOK,
  TUR_eyes_TOK,
  VN_eyes_TOK)

All_EYES <- cbind.data.frame(All_EYES, DEM_DEM) # I do not understand why to put it together now - but it does not 
# (at least) harm anything. So let's proceed and see what happens.

# PS some variables may be duplicated, I hope not - and if they are - again - it's unharmful.

summary(All_EYES)

All_EYES[All_EYES == ""] <- NA

summary(All_EYES)

# Process the demography: 

# 1) A primitive function to get word from a string of NA,Na,NA,slovo,NA,NA...  
get_word2 <- function(WwW) {
  word <- WwW[!is.na(WwW)]
  if (length(word) == 0) {
    return(NA)
  } else {
    return(word)
  }
}

# Testing
print(All_EYES)

# Create a summary table - will sum up all the demographic variables: 
# Parcitipans who did not fill the study properly will be excluded. 
Dem_sum <- data.frame(levels(as.factor(All_EYES$Rec_Session_Id)))
colnames(Dem_sum)[1] <- "Particitpant_ID"

Dem_sum$Culture  <- tapply((All_EYES$culture),All_EYES$Rec_Session_Id, unique)

options(max.print = 2000)
summary(as.factor(Dem_sum$Particitpant_ID), maxsum = 1e4)

Dem_sum$finished <- tapply(as.factor(All_EYES$Completed),All_EYES$Rec_Session_Id, unique)
Dem_sum$finished <- ifelse(Dem_sum$finished==2,"TRUE","FALSE")

# ATCH 1 
Dem_sum$ATCH1 <- tapply(All_EYES$Attention_Check_Butter, All_EYES$Rec_Session_Id, get_word2)
summary(as.factor(Dem_sum$ATCH1 )) 

# ATCH 2 
Dem_sum$ATCH2 <- tapply(All_EYES$Attention_Check_CCCP, All_EYES$Rec_Session_Id, get_word2)
summary(as.factor(Dem_sum$ATCH2 )) 

# ATCH 3 
All_EYES$Attention_Check_Green <- ifelse(All_EYES$Attention_Check_Green=="",NA,All_EYES$Attention_Check_Green)
Dem_sum$ATCH3 <- tapply(All_EYES$Attention_Check_Green, All_EYES$Rec_Session_Id, get_word2)
summary(as.factor(Dem_sum$ATCH3 ))  

# Age, weight, height 
Dem_sum$Age <- tapply(All_EYES$Age,All_EYES$Rec_Session_Id,mean, na.rm=T)
Dem_sum$Age[Dem_sum$Age=="NaN"]<-NA

Dem_sum$weight <- tapply(as.numeric(All_EYES$weight),All_EYES$Rec_Session_Id,mean, na.rm=T)
Dem_sum$weight[Dem_sum$weight=="NaN"]<-NA

Dem_sum$height <- tapply(as.numeric(All_EYES$height),All_EYES$Rec_Session_Id,mean, na.rm=T)
Dem_sum$height[Dem_sum$height=="NaN"]<-NA

# Some Turkish raters wrote year of birth instead of age - let's fix it: 
# MIND that the data were collected in 2024! 
Dem_sum$Age <- ifelse(Dem_sum$Age>100,2024-Dem_sum$Age,Dem_sum$Age)


# Sex at birth 
Dem_sum$SexATB <- tapply(All_EYES$Sex_ATB, All_EYES$Rec_Session_Id, get_word2)

# Mother tongue 
Dem_sum$MotherT <- tapply(All_EYES$Mother_Tongue, All_EYES$Rec_Session_Id, get_word2)

# Passport nationality
Dem_sum$PasspNat <- tapply(All_EYES$PasspNat, All_EYES$Rec_Session_Id, get_word2)

# idiotTAG: Vojtech is an idiot, do not be like Vojtech. This works: 
#View(Dem_sum[!is.na(Dem_sum$PasspNat) & Dem_sum$PasspNat == "South Africa", ])
#View(Dem_sum[which(Dem_sum$PasspNat == "South Africa"), ])

# Self-reported ethnicity: 
Dem_sum$SelfEthn <- tapply(All_EYES$Self_Ethnicity, All_EYES$Rec_Session_Id, get_word2)

# Socioeconomic backgroud: 
Dem_sum$SocBack <- tapply(All_EYES$Family_SOC, All_EYES$Rec_Session_Id, get_word2)

# Travel abroad: 
Dem_sum$TravelAbroad <- tapply(All_EYES$Travel_Abroad, All_EYES$Rec_Session_Id, get_word2)

# Meeting people of different ethnicities 
Dem_sum$MeetDiffEth <- tapply(All_EYES$People_Diff_Eth, All_EYES$Rec_Session_Id, get_word2)

# Marital status
Dem_sum$MaritalStat <- tapply(All_EYES$Marital_Stat, All_EYES$Rec_Session_Id, get_word2)
# Let's later inspect what the "false" means...

# "42 45" (colour vision check): 
All_EYES$`__42__` <- ifelse(All_EYES$`__42__`=="",NA,All_EYES$`__42__`)
Dem_sum$test42 <- tapply(All_EYES$`__42__`, All_EYES$Rec_Session_Id, get_word2)

# Colombian participant ID = 1397253 used -42, which, I beleive is an accidental mistake. The scale 
# is "clickable" and one can go both ways. So 42 and -42 are equivalent. 
Dem_sum$test42[which(Dem_sum$test42==-42)] <- "42"

All_EYES$`__45__` <- ifelse(All_EYES$`__45__`=="",NA,All_EYES$`__45__`)
Dem_sum$test45 <- tapply(All_EYES$`__45__`, All_EYES$Rec_Session_Id, get_word2)

# SOCIAL MEDIA USE: 
# at all? 
Dem_sum$USE_SM <- tapply(All_EYES$USE_SM, All_EYES$Rec_Session_Id, get_word2)

# SMU-i
Dem_sum$SMUi_1 <- tapply(All_EYES$HowOftenPassive, All_EYES$Rec_Session_Id, get_word2)

Dem_sum$SMUi_2 <- tapply(All_EYES$HowOftenPosting, All_EYES$Rec_Session_Id, get_word2)

Dem_sum$SMUi_3 <- tapply(All_EYES$HowOftenLiking, All_EYES$Rec_Session_Id, get_word2)


# ACTIVE / PASSIVE USE
Dem_sum$Facebook_AP <- tapply(All_EYES$AP_Facebook, All_EYES$Rec_Session_Id, get_word2)
Dem_sum$Instagram_AP <- tapply(All_EYES$AP_Instagram, All_EYES$Rec_Session_Id, get_word2)
Dem_sum$TikTok_AP <- tapply(All_EYES$AP_TikTok, All_EYES$Rec_Session_Id, get_word2)
Dem_sum$TwitterX_AP <- tapply(All_EYES$AP_TwitterX, All_EYES$Rec_Session_Id, get_word2)
Dem_sum$YouTube_AP <- tapply(All_EYES$AP_YouTube, All_EYES$Rec_Session_Id, get_word2)

# Frequency of use
Dem_sum$Freq_Facebook <- tapply(All_EYES$How_freq_Facebook, All_EYES$Rec_Session_Id, get_word2)
Dem_sum$Freq_Instagram <- tapply(All_EYES$How_freq_Instagram, All_EYES$Rec_Session_Id, get_word2)
Dem_sum$Freq_TikTok <- tapply(All_EYES$How_freq_TikTok, All_EYES$Rec_Session_Id, get_word2)
Dem_sum$Freq_TwitterX <- tapply(All_EYES$How_freq_TwitterX, All_EYES$Rec_Session_Id, get_word2)
Dem_sum$Freq_YouTube <- tapply(All_EYES$How_freq_YouTube, All_EYES$Rec_Session_Id, get_word2)


# NEGATIVE USE
Dem_sum$NEG_FOMO <- tapply(All_EYES$SNS_FOMO, All_EYES$Rec_Session_Id, get_word2)
Dem_sum$NEG_better_friends <- tapply(All_EYES$SNS_online_better_friends, All_EYES$Rec_Session_Id, get_word2)
Dem_sum$NEG_hobbies <- tapply(All_EYES$SNS_vs_other_hobbies, All_EYES$Rec_Session_Id, get_word2)
Dem_sum$NEG_too_much_time <- tapply(All_EYES$SNS_too_much_time, All_EYES$Rec_Session_Id, get_word2)

# Now - check if everything is OK - especially if the Social Media Related variables are coded correctly 
# Should be: Higher number = More (more intensive use, more serious problems, etc.)
# The tables I uploaded are RAW data! 

# A/P use is reversed: 
Dem_sum$Facebook_AP <- 5 - Dem_sum$Facebook_AP
Dem_sum$Instagram_AP <- 5 - Dem_sum$Instagram_AP
Dem_sum$TikTok_AP <- 5 - Dem_sum$TikTok_AP
Dem_sum$TwitterX_AP <- 5 -Dem_sum$TwitterX_AP
Dem_sum$YouTube_AP <- 5 - Dem_sum$YouTube_AP

# Negative use is also reversed: 1 = totally agree, 2 = mostly agree, 3 = undecided, 4 = mostly disagree, 5 = totally disagree, 5 = totally disagree
# Should be 1 = least, 5 = most 
Dem_sum$NEG_FOMO <- 6 - Dem_sum$NEG_FOMO
Dem_sum$NEG_better_friends <- 6 - Dem_sum$NEG_better_friends
Dem_sum$NEG_hobbies <- 6 - Dem_sum$NEG_hobbies
Dem_sum$NEG_too_much_time <- 6 - Dem_sum$NEG_too_much_time

# All else is correct! 

# 2nd check: There must not be any "test" or other bullshitous terms in the data - this means 
# that the participant was not a participant but me (or somebody else) testing the survey/s

# SexATB, MotherTongue and Self_Ethnicity - these were open and would be filled with test if test was the case
table(Dem_sum$SexATB) # 1× "TEST
table(Dem_sum$SelfEthn) # 
table(Dem_sum$MotherT) # 1× "TSEST" (which is a typo)

Dem_sum <- Dem_sum[Dem_sum$MotherT!="TSEST",]
Dem_sum <- Dem_sum[Dem_sum$SexATB!="TEST",]


# Age, weight, height (probably only age is relevant as I only later realised 
# that weight and height need not be always reported (and perceived) in kg and cm as SI units)
summary(Dem_sum$Age)
summary(Dem_sum$weight)
summary(Dem_sum$height)

Dem_sum <- Dem_sum[Dem_sum$MotherT!="TSEST",]
Dem_sum <- Dem_sum[Dem_sum$SexATB!="TEST",]

# Let's clean the table: 
# Participant who is 17 must unfortunately be excluded (to joung for international ethics rules)
# The person who weights 25 kgs is also quite unreliable. 
# The same applies to 280 cm person (who is probably just me testing anyway)
# However, I hereby alert the reader that I deliberately keep  in the data the bunch of "80cm people".
# This is likely just a mistake of using inches instead of cm - but I do not want to exclude them.  

par(mfrow=c(1,3))
hist(Dem_sum$Age, breaks=50)
hist(Dem_sum$weight, breaks=50)
hist(Dem_sum$height, breaks=50)

Dem_sum <- Dem_sum[Dem_sum$Age>17,]
Dem_sum <- Dem_sum[Dem_sum$weight>30 & Dem_sum$weight<160,]
Dem_sum <- Dem_sum[Dem_sum$height<210,]

par(mfrow=c(1,1))

# The two attention checks: Who did not click "1968" and "Green"  does not deserve to be here: 
Dem_sum <- (Dem_sum[which(Dem_sum$ATCH2==1968), ])
Dem_sum <- (Dem_sum[which(Dem_sum$ATCH3=="Green"), ])

# And participation of those who did not read 42 and 45 correctly is also questionable: 
(Dem_sum[which(Dem_sum$test42!="42"), ])  # 1 person
(Dem_sum[which(Dem_sum$test45!="45"), ])  # 15 and 88 are... not right. The rest can be kept 
Dem_sum <- (Dem_sum[which(Dem_sum$test42=="42"), ])
Dem_sum <- (Dem_sum[which(Dem_sum$test45=="45" | Dem_sum$test45=="46"), ])

# Now, last round of exclusion (hopefully): 
# Participants must have made it all the way up to the question: "Do you use social media"? 
#View(Dem_sum[is.na(Dem_sum$USE_SM),])

Excls <- (All_EYES[All_EYES$Rec_Session_Id %in% Dem_sum[is.na(Dem_sum$USE_SM),]$Particitpant_ID,])
table(as.factor(Excls$Rec_Session_Id)) # Nechat ty, co se na to vysrali až po ohodnocení všech obličejů

ExclIDs <-table(as.factor(Excls$Rec_Session_Id))[table(as.factor(Excls$Rec_Session_Id))>100]
ExclIDs <- names(ExclIDs)

Incls <- Excls[Excls$Rec_Session_Id %in% ExclIDs,]
table(as.factor(Incls$Rec_Session_Id)) # Leave them aside... if N is high enough no need to add them...

# Now, the rest can be divided based on Social Media Use underlying factors: 
Dem_sum <- Dem_sum[!is.na(Dem_sum$USE_SM),]

# 1057237 is a Czech (probably still me testing)
Dem_sum <- Dem_sum[Dem_sum$Particitpant_ID!=1057237,] 

# Check how many survived: 
table(Dem_sum$Culture) # Can be...
table(Incls$culture) # 2,3,2,3 it would be that helpful, even though 25 looks better than 22

# Let's calculate the SNS underlying think, add it to Dem_sum,
# Then combine with Incl... In there, the same variable will be labeled "NC" = not classified 
# and that's it

# Let's assign the NAs <- 1 (the least possible answer)
Dem_sum[is.na(Dem_sum)] <- 1

seldemog <- Dem_sum[c("Particitpant_ID", "Culture", "SMUi_1","SMUi_2","SMUi_3",
                      "Facebook_AP","Instagram_AP","TikTok_AP","TwitterX_AP","YouTube_AP",
                      "Freq_Facebook","Freq_Instagram","Freq_TikTok","Freq_TwitterX","Freq_YouTube",
                      "NEG_FOMO","NEG_better_friends","NEG_hobbies","NEG_too_much_time")]


# The dataset's ready, let's do some checks...

# 1) Values are within expected ranges
# SMU_i_Boer should be between 1 and 7
# negative_ should be between 1 and 5 
# AP shoud be between 1 and 6
rng <- sapply(seldemog, function(x) range(x, na.rm=TRUE))
print(rng)

# 2) SMU items are strictly integers 1..K
#    We should also introduce two types of smu_names_ object:
#    There are two attitudes to select the final variables: 
#    smu_names_12 -> will be used when we combine both the SMU-i and negative use.
#    smu_names_8 -> this time, we are only considering variables 
#    that theoretically refer to social media use intensity (unlike negative use does)
smu_names_12 <- c("SMUi_1","SMUi_2","SMUi_3",
                  "Freq_Facebook","Freq_Instagram","Freq_TikTok","Freq_TwitterX","Freq_YouTube",
                  "NEG_FOMO","NEG_better_friends","NEG_hobbies","NEG_too_much_time")
for (nm in smu_names_12) {
  cat("\n", nm, ":\n"); print(sort(unique(seldemog[[nm]]))) # cat -> "concatenate and print"
}

smu_names_8 <- c("SMUi_1","SMUi_2","SMUi_3",
                  "Freq_Facebook","Freq_Instagram","Freq_TikTok","Freq_TwitterX","Freq_YouTube")

# 3) Crosstabs to ensure coercion is sane
ct <- function(x,y) { print(table(x,y,useNA="ifany")) }
ct(Dem_sum$SMUi_1, seldemog$SMUi_1)

sel12items <- seldemog[,smu_names_12]
summary.data.frame(sel12items)

sel8items <- seldemog[,smu_names_8]
summary.data.frame(sel8items)


