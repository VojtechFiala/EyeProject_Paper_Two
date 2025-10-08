source("EYEP2_1_raw_data_assembly.R")

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

library(psych)
library(nFactors)
library(lavaan)


# EIGHT ITEMS: 

## 1) Build the polychoric correlation matrix (handles 7-pt + 6-pt mix)
pc  <- polychoric(sel8items)   # expect benign warnings about unequal categories
rho <- cor.smooth(pc$rho)      # ensure positive-definite

## 2) Inspect eigenvalues (scree)
ev <- eigen(rho, symmetric = TRUE)$values
print(round(ev, 3))

# 3) We are interested in number of underlying factors:
fa.parallel(rho, n.obs = nrow(sel8items), fm="ml", fa="both")
# Elbow says one or two, estimate says 3 factors. 

## 4) Numeric decision aids (parallel + Kaiser + acceleration)
ap      <- parallel(subject = nrow(sel8items),
                    var = ncol(sel8items),
                    rep = 500, cent = .05)
ap_eig  <- ap$eigen$qevpea
kaiserK <- sum(ev > 1)                         # Kaiser (very rough)
paK     <- sum(ev > ap_eig)                    # Parallel analysis cutoff

cat("\nKaiser suggests:", kaiserK, "factor(s)\n")
cat("Parallel suggests:", paK, "factor(s)\n")

# choose nfact from the parallel/scree; then:
efa1_8_ml <- fa(r = rho, nfactors = 1, fm = "ml", rotate = "oblimin")

efa2_8_ml <- fa(r = rho, nfactors = 2, fm = "ml", rotate = "oblimin")

# Mind that 3 and 4 factors is "too many".  
efa3_8_ml <- fa(r = rho, nfactors = 3, fm = "ml", rotate = "oblimin")
efa4_8_ml <- fa(r = rho, nfactors = 4, fm = "ml", rotate = "oblimin")

print(efa1_8_ml$loadings, cutoff=.10)
print(efa2_8_ml$loadings, cutoff=.10)
print(efa3_8_ml$loadings, cutoff=.10)
print(efa4_8_ml$loadings, cutoff=.10)


# 3) We are interested in number of underlying factors:
fa.parallel(rho, n.obs = nrow(sel8items), fm="uls", fa="both")

ap      <- parallel(subject = nrow(sel8items),
                    var = ncol(sel8items),
                    rep = 500, cent = .05)
ap_eig  <- ap$eigen$qevpea
kaiserK <- sum(ev > 1)                         # Kaiser (very rough)
paK     <- sum(ev > ap_eig)                    # Parallel analysis cutoff

cat("\nKaiser suggests:", kaiserK, "factor(s)\n")
cat("Parallel suggests:", paK, "factor(s)\n")

# choose nfact from the parallel/scree; then:
efa1_8_uls <- fa(r = rho, nfactors = 1, fm = "uls", rotate = "oblimin")
efa2_8_uls <- fa(r = rho, nfactors = 2, fm = "uls", rotate = "oblimin")
efa3_8_uls <- fa(r = rho, nfactors = 3, fm = "uls", rotate = "oblimin")
efa4_8_uls <- fa(r = rho, nfactors = 4, fm = "uls", rotate = "oblimin")

print(efa1_8_uls$loadings, cutoff=.10)
print(efa2_8_uls$loadings, cutoff=.10)
print(efa3_8_uls$loadings, cutoff=.10)
print(efa4_8_uls$loadings, cutoff=.10)

# One factor solution is relatively well-supported: 
#   When I try to do it with one factor, the loadings are "reasonable",
# When adding additional factors, the loadings are "silly" (e.g., only one item loads on the second factor,
# *> 0.90, etc. 

# (a) get the underlying factors from the FA objects: 
fa_fit_8_ml <- fa(sel8items, nfactors = 1, fm = "ml", rotate="oblimin")

# store scores in seldemog: 
seldemog$fa_8_ml <- fa_fit_8_ml$scores  

fa_fit_8_uls <- fa(sel8items, nfactors = 1, fm = "uls", rotate="oblimin")
seldemog$fa_8_uls <- fa_fit_8_uls$scores  

cor(seldemog$fa_8_ml, seldemog$fa_8_uls) # Not identical, but very close...


# (b) Run CFAs with the corresponding variables and isolate the model prediction, too: 

## declare them as ordered factors with fixed levels; this prevents accidental re-indexing 
## if some categories are missing.
for (nm in c("SMUi_1","SMUi_2","SMUi_3")) {
  seldemog[[nm]] <- factor(seldemog[[nm]], levels = 1:7, ordered = TRUE)
}
## For per-platform frequency items (1..6):
for (nm in c("Freq_Instagram","Freq_TikTok","Freq_TwitterX","Freq_YouTube")) {
  seldemog[[nm]] <- factor(seldemog[[nm]], levels = 1:6, ordered = TRUE)
}

####
# 1
####

# 7 variables that load >0.3
smu7_names <- c("SMUi_1","SMUi_2","SMUi_3","Freq_Instagram","Freq_TikTok","Freq_TwitterX","Freq_YouTube")

model_1f <- '
  SMU =~ SMUi_1 + SMUi_2 + SMUi_3 +
         Freq_Instagram + Freq_TikTok + Freq_TwitterX + Freq_YouTube
'

fit_1f <- cfa(model_1f, data = seldemog,
              ordered   = smu7_names,     # tell lavaan these are ordinal
              estimator = "WLSMV",        # robust for ordinal items
              parameterization = "theta", # recommended for ordinals
              std.lv    = TRUE)           # factor variance = 1; loadings comparable

summary(fit_1f, fit.measures = TRUE, standardized = TRUE)

mi <- modificationIndices(fit_1f, sort.=TRUE)

# Show the top candidates between observed indicators only:
mi_pairs <- mi[mi$op == "~~" & mi$lhs %in% smu7_names & mi$rhs %in% smu7_names, ]

mi_pairs

####
# 2
####

# Let's add covariance between Freq_TikTok and Freq_TwitterX  in the model, mi_pairs suggest it may be needed: 

model_1f_cov1 <- '
  SMU =~ SMUi_1 + SMUi_2 + SMUi_3 +
         Freq_Instagram + Freq_TikTok + Freq_TwitterX + Freq_YouTube
  Freq_TikTok ~~ Freq_TwitterX
'

fit_1f_cov1 <- cfa(model_1f_cov1, data = seldemog,
                   ordered = smu7_names, estimator = "WLSMV",
                   parameterization = "theta", std.lv = TRUE)

summary(fit_1f_cov1, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_1f_cov1, c("cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr",
                           "cfi.scaled","tli.scaled","rmsea.scaled"))
fitMeasures(fit_1f, c("cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr",
                      "cfi.scaled","tli.scaled","rmsea.scaled"))


# Extract the principal values, informing about quality of the fit: 
fitMeasures(fit_1f, c("cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr","chisq","df","pvalue"))

lavTestLRT(fit_1f, fit_1f_cov1)  # significant Δχ² supports freeing the residual

# Standardised loadings matrix (expect all positive, ideally ≥ .40)
inspect(fit_1f_cov1, "std")$lambda # Yes, except for twitter which, however, is getting close

## Get factor scores
fs_8_no_cov <- lavPredict(fit_1f, method = "EBM")   # empirical Bayes scores (robust)
as.numeric(fs_8_no_cov); range(fs_8_no_cov)
seldemog$CFA_smu_score_8_no_covariance <- as.numeric(fs_8_no_cov)

## Get factor scores # 2
fs_8_cov <- lavPredict(fit_1f_cov1, method = "EBM")   # empirical Bayes scores (robust)
as.numeric(fs_8_cov); range(fs_8_cov)
seldemog$CFA_smu_score_8_covariance <- as.numeric(fs_8_cov)

cor(seldemog[,19:22]) # It's all nearly the same...


#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

#Twelve ITEMS: 

## 1) Build the polychoric correlation matrix (handles 7-pt + 6-pt mix)
pc  <- polychoric(sel12items)   # expect benign warnings about unequal categories
rho <- cor.smooth(pc$rho)      # ensure positive-definite

## 2) Inspect eigenvalues (scree)
ev <- eigen(rho, symmetric = TRUE)$values
print(round(ev, 3))

fa.parallel(rho, n.obs = nrow(sel12items), fm="ml", fa="both")
# Elbow again suggest one is the solution...

## 4) Numeric decision aids (parallel + Kaiser + acceleration)
ap      <- parallel(subject = nrow(sel12items),
                    var = ncol(sel12items),
                    rep = 500, cent = .05)
ap_eig  <- ap$eigen$qevpea
kaiserK <- sum(ev > 1)                         # Kaiser (very rough)
paK     <- sum(ev > ap_eig)                    # Parallel analysis cutoff

cat("\nKaiser suggests:", kaiserK, "factor(s)\n")
cat("Parallel suggests:", paK, "factor(s)\n")
# It's 3 and 2...

# choose nfact from the parallel/scree; then:
efa1_12_ml <- fa(r = rho, nfactors = 1, fm = "ml", rotate = "oblimin")
efa2_12_ml <- fa(r = rho, nfactors = 2, fm = "ml", rotate = "oblimin")
efa3_12_ml <- fa(r = rho, nfactors = 3, fm = "ml", rotate = "oblimin")
efa4_12_ml <- fa(r = rho, nfactors = 4, fm = "ml", rotate = "oblimin")

print(efa1_12_ml$loadings, cutoff=.10) # Take it from here
print(efa2_12_ml$loadings, cutoff=.10) # and from here
print(efa3_12_ml$loadings, cutoff=.10)
print(efa4_12_ml$loadings, cutoff=.10)

# The two factor solution looks good this time. Except the instagram think in ML1
# Let's isolate the underlying F1 and F2 from two-factor solution and see how it relates to 
# factors obtained by other means.

fa.parallel(rho, n.obs = nrow(sel12items), fm="uls", fa="both")

# choose nfact from the parallel/scree; then:
efa1_12_uls <- fa(r = rho, nfactors = 1, fm = "uls", rotate = "oblimin")
efa2_12_uls <- fa(r = rho, nfactors = 2, fm = "uls", rotate = "oblimin")
efa3_12_uls <- fa(r = rho, nfactors = 3, fm = "uls", rotate = "oblimin")
efa4_12_uls <- fa(r = rho, nfactors = 4, fm = "uls", rotate = "oblimin")

print(efa1_12_uls$loadings, cutoff=.10)
print(efa2_12_uls$loadings, cutoff=.10)
print(efa3_12_uls$loadings, cutoff=.10)
print(efa4_12_uls$loadings, cutoff=.10)

# let's 
# (a) get the underlying factors from the FA objects: 
fa_fit_12_ml <- fa(sel12items, nfactors = 1, fm = "ml", rotate="oblimin")
seldemog$fa_12_ml_1FA <- fa_fit_12_ml$scores  

fa_fit_12_ml <- fa(sel12items, nfactors = 2, fm = "ml", rotate="oblimin")
fa_fit_12_ml$scores[,1]
seldemog$fa_12_ml_2FA <- fa_fit_12_ml$scores[,1]

fa_fit_12_ml$scores[,2]
seldemog$fa_12_ml_2FA_aka_NEG <- fa_fit_12_ml$scores[,2]


fa_fit_12_uls <- fa(sel12items, nfactors = 1, fm = "uls", rotate="oblimin")
seldemog$fa_12_uls_1FA <- fa_fit_12_uls$scores  

fa_fit_12_uls <- fa(sel12items, nfactors = 2, fm = "uls", rotate="oblimin")
fa_fit_12_uls$scores[,1]
seldemog$fa_12_uls_2FA <- fa_fit_12_uls$scores[,1]

fa_fit_12_uls$scores[,2]
seldemog$fa_12_ml_2FA_aka_NEG <- fa_fit_12_uls$scores[,2]

cor(seldemog[,19:27]) # The "negative use/smu" is the only factor that correlates less with the rest...

# I could also consider using the three-factor solution but not see it reasonable & parsimonious...

# (b) Run CFAs with the corresponding variables and isolate the model prediction, too: 

## declare them as ordered factors with fixed levels; this prevents accidental re-indexing 
## if some categories are missing.
for (nm in c("SMUi_1","SMUi_2","SMUi_3")) {
  seldemog[[nm]] <- factor(seldemog[[nm]], levels = 1:7, ordered = TRUE)
}
## For per-platform frequency items (1..6):
for (nm in c("Freq_Instagram","Freq_TikTok","Freq_TwitterX","Freq_YouTube","Freq_Facebook")) {
  seldemog[[nm]] <- factor(seldemog[[nm]], levels = 1:6, ordered = TRUE)
}
## For negative use items (1..5):
for (nm in c("NEG_FOMO","NEG_better_friends","NEG_hobbies","NEG_too_much_time")) {
  seldemog[[nm]] <- factor(seldemog[[nm]], levels = 1:5, ordered = TRUE)
}

# We need to get two scores: 
#  - SMU-individ_freq score
#  - negative/smu-i score
smu <- c("SMUi_1","SMUi_2","SMUi_3","Freq_Instagram","Freq_TikTok","Freq_TwitterX","Freq_YouTube")
neg <- c("NEG_FOMO","NEG_better_friends","NEG_hobbies","NEG_too_much_time")

model_2f <- '
  SMU =~ SMUi_1 + SMUi_2 + SMUi_3 + Freq_Instagram + Freq_TikTok + Freq_TwitterX + Freq_YouTube
  NEG =~ NEG_FOMO + NEG_better_friends + NEG_hobbies + NEG_too_much_time
  SMU ~~ NEG
  Freq_TikTok ~~ Freq_TwitterX
'

fit_2f <- cfa(model_2f, data = seldemog,
              ordered = c(smu, neg), estimator = "WLSMV",
              parameterization = "theta", std.lv = TRUE)

summary(fit_2f, fit.measures=TRUE, standardized=TRUE)

# Standardised loadings matrix (expect all positive, ideally ≥ .40)
inspect(fit_2f, "std")$lambda # Yes, except for YT, but we already know it is worse...

# Now - the model without negative...
model_2f_without_negatives <- '
  ONE =~ SMUi_1 + SMUi_2 + SMUi_3 + 
  Freq_Instagram + Freq_TikTok + Freq_TwitterX + Freq_YouTube + 
  NEG_FOMO + NEG_better_friends + NEG_hobbies + NEG_too_much_time
  Freq_TikTok ~~ Freq_TwitterX
'
fit_2f_without_negatives <- cfa(model_2f_without_negatives, data = seldemog,
                                ordered = c(smu, neg), estimator = "WLSMV",
                                parameterization = "theta", std.lv = TRUE)

lavTestLRT(fit_2f, fit_2f_without_negatives)  # scaled diff test
# the two-factor solution fits substantially better.

## Get factor scores
# 1: SMU score from model with both SMU and Negative Score (separately)
# 2: Negative score from model with both SMU and Negative Score
# 3: SMU Score from model where this score is predicted both by SMU and Negative...
fs <- lavPredict(fit_2f, method = "EBM")   # empirical Bayes scores (robust), You are interested in both...
(fs)[,1]; range(fs[,1])
(fs)[,2]; range(fs[,2])

seldemog$CFA_smu_score_12_SMU_negsep <- (fs)[,1]
seldemog$CFA_smu_score_12_NEG <- (fs)[,2]

fs <- lavPredict(fit_2f_without_negatives, method = "EBM")   # empirical Bayes scores (robust), You are interested in both...
(fs); range(fs)

seldemog$CFA_smu_score_12_SMU_negNEsep <- (fs)

cor_subscales <- cor(seldemog[,20:31])

write.csv2(cor_subscales, file="EYE_Proj_cor_subscales_FA.csv")

# Let's stick to the most simple scale: 
seldemog$fa_8_ml

# And put it into the data frame. And finally kurva jdem samplovat.
range(seldemog$fa_8_ml)

seldemog$SMU_score <- as.numeric(seldemog$fa_8_ml)

seldemog$cultur <- as.factor(Dem_sum$Culture)

tapply(seldemog$SMU_score, seldemog$cultur, median) # Definitely split within country, not across countries.
# Plus do not forget to discuss that our result may partly depend on a nontrivial interaction between
# country of origin and locally popular social media (perhaps people in Vietnam may be less into western 
# social media & this may cause we in fact accidentally measure their adherence to western norms.


# Within country median split: 
# This was proposed by Chat - and it's elegant but maybe too complex. 
if ("cultur" %in% names(seldemog)) {
  med_by_cty <- tapply(seldemog$SMU_score, seldemog$cultur, median, na.rm = TRUE)
  seldemog$SMU_heavy <- as.integer(seldemog$SMU_score >= med_by_cty[as.character(seldemog$cultur)])
} else {
  cut <- median(seldemog$SMU_score, na.rm = TRUE)
  seldemog$SMU_heavy <- as.integer(seldemog$SMU_score >= cut)
}

seldemog$SMU_heavy

seldemog$check_SMU_split <- med_by_cty[seldemog$Culture]

# There were parts with "Oldemog". These are not there anymore, since Oldemog served for an old split.

# Now - identify those who lie (in their country), among 25% (12.5 & 12.5%, sidewise) from the centre: 
seldemog$SMU_score;med_by_cty

# Do the ID align after all? 
View(data.frame(seldemog_ID = seldemog$Particitpant_ID,Dem_sum_ID = Dem_sum$Particitpant_ID))
# To toho vidim...
cor(as.numeric(seldemog$Particitpant_ID),as.numeric(Dem_sum$Particitpant_ID))
# Now better: Yes, it assigns a correct row (the one that correspond to the participant in question)
# and the vector can be added to the table "seldemog"

seldemog$Fami_back <- ifelse(Dem_sum$SocBack=="Rich",1,
                             ifelse(Dem_sum$SocBack=="Upper CL",2,
                                    ifelse(Dem_sum$SocBack=="Middle",3,
                                           ifelse(Dem_sum$SocBack=="Lower CL",4,
                                                  ifelse(Dem_sum$SocBack=="Poor",5,NA)))))

table(seldemog$Fami_back,Dem_sum$SocBack)

seldemog$travel_abroad <- ifelse(Dem_sum$TravelAbroad=="Often",1,
                             ifelse(Dem_sum$TravelAbroad=="Rather often",2,
                                    ifelse(Dem_sum$TravelAbroad=="Occasionally",3,
                                           ifelse(Dem_sum$TravelAbroad=="Rarely",4,
                                                  ifelse(Dem_sum$TravelAbroad=="Very rarely",5,6)))))

table(seldemog$travel_abroad,Dem_sum$TravelAbroad)

cor.test(seldemog$SMU_score, seldemog$Fami_back)
cor.test(seldemog$SMU_score, seldemog$travel_abroad)


## 1) Compute 37.5% and 62.5% quantiles within each country
cty <- seldemog$cultur
sc  <- seldemog$SMU_score

# Mind that the order is kept, we can proceed with the upper & lowe quantile, using tapply + function
(lower_q <- tapply(sc, cty, function(x) quantile(x, probs = 0.375, na.rm = TRUE, type = 7)))
med_by_cty # This should fit in the middle
(upper_q <- tapply(sc, cty, function(x) quantile(x, probs = 0.625, na.rm = TRUE, type = 7)))

## 2) Map those per-country cutoffs back to each row
lo <- lower_q[ as.character(cty) ];lo
hi <- upper_q[ as.character(cty) ];hi
# These are our borders in a vector-like manner (ensuring there is one lower and upper 
# border for each participant, corresponding to their ountries' quantiles)

## 3) Flag “within middle 25% band”; there are no NAs, so it's just 0/1, where 1 is within
# and the borders are included in the band
seldemog$NearMedian25 <- as.integer(sc >= lo & sc <= hi)

## Make a labeled factor - one may forgot meaning of 0/1 simply...
seldemog$NearMedian25_f <- factor(ifelse(is.na(seldemog$NearMedian25), NA,
                                         ifelse(seldemog$NearMedian25==1, "Within25%", "Outside")),
                                  levels = c("Outside","Within25%"))

## 4) Sanity check: proportion per country (should be ~0.25, small deviations due to ties)
round(tapply(seldemog$NearMedian25, cty, function(z) mean(z==1, na.rm=TRUE)), 3)


#-------------------------------------------------------------------------------
# Exploring how intensively participants indeed use Social Media: 
hist(seldemog$SMU_score, breaks=100, ylab="Frequency", xlab="", main="Distribution of SMU_score across the whole sample (N=318)")

par(mfrow=c(1,1))

svg("SMU_scores_split_RatedEye.svg",width=20, height=15, pointsize=25)

layout(matrix(1:6,byrow=T,nrow=2), widths=c(2,2,2),heights=c(1,1))

hist(seldemog$SMU_score[seldemog$cultur=="AUS_NZ"], breaks=100, 
     ylab="Frequency", xlab="", main="Distribution of SMU_score \n across the Australian / NewZ sample (N=53)")
abline(v=med_by_cty[1], col="darkred", lwd=5, lty=2)

hist(seldemog$SMU_score[seldemog$cultur=="COL"], breaks=100, 
     ylab="Frequency", xlab="", main="Distribution of SMU_score \n across the Colombian sample (N=32)")
abline(v=med_by_cty[2], col="darkred", lwd=5, lty=2)

hist(seldemog$SMU_score[seldemog$cultur=="CZ"], breaks=100, 
     ylab="Frequency", xlab="", main="Distribution of SMU_score \n across the Czech sample (N=152)")
abline(v=med_by_cty[3], col="darkred", lwd=5, lty=2)

hist(seldemog$SMU_score[seldemog$cultur=="RSA"], breaks=100, 
     ylab="Frequency", xlab="", main="Distribution of SMU_score \n across the South African sample (N=47)")
abline(v=med_by_cty[4], col="darkred", lwd=5, lty=2)

hist(seldemog$SMU_score[seldemog$cultur=="TUR"], breaks=100, 
     ylab="Frequency", xlab="", main="Distribution of SMU_score \n across the Turkish sample (N=79)")
abline(v=med_by_cty[5], col="darkred", lwd=5, lty=2)

hist(seldemog$SMU_score[seldemog$cultur=="VN"], breaks=100, 
     ylab="Frequency", xlab="", main="Distribution of SMU_score \n across the Vietnamese sample (N=72)")
abline(v=med_by_cty[6], col="darkred", lwd=5, lty=2)

dev.off()

# We need: 
# - SMU_score (just for sure)
# - SMU_heavy (the split)
# - NearMedian25 (probably useless, but...)
# - Fami_back
# - travel_abroad

# Now subset just eye ratings:
Eye_set <- c("CZ_16_F_eye","CZ_16_M_eye","CZ_19_F_eye","CZ_19_M_eye")
All_EYES_sub <- All_EYES[All_EYES$Task_Name %in% Eye_set,] 

All_EYES_sub[is.na(All_EYES_sub$Rec_Session_Id),]


# Merging: 
All_EYES_sub$CZ16F_eye_col[All_EYES_sub$CZ16F_eye_col==""]<-NA
All_EYES_sub$CZ16M_eye_col[All_EYES_sub$CZ16M_eye_col==""]<-NA
All_EYES_sub$CZ19F_eye_col[All_EYES_sub$CZ19F_eye_col==""]<-NA
All_EYES_sub$CZ19M_eye_col[All_EYES_sub$CZ19M_eye_col==""]<-NA

# Make a function to get the sole column that contains value...
get_non_na <- function(x) {
  return(x[!is.na(x)][1])
}

# Apply the function across rows to create the new column
All_EYES_sub$EyeCol <- apply(All_EYES_sub[,8:11], 1, get_non_na)
All_EYES_sub$order <- seq(from=1, to=nrow(All_EYES_sub))

All_EYES_fin <- All_EYES_sub[,c(1,2,3,6,48,49,50)]

All_EYES_fin[is.na(All_EYES_fin$Rec_Session_Id),]


# LOAD IDs of the faces
# The point is that every face is added an ID in Labvanced. I took the IDs, checked which face 
# it is, which ID it has in my data and paired the information, saving it in a table.
Face_IDs <- read.csv2("CZ1619_Order_TRU.csv",T);Face_IDs$Set
# Add dash
Face_IDs$Set <- paste0(c(substr(Face_IDs$Set,1,2)),"_",(substr(Face_IDs$Set,3,7)));Face_IDs$Set

All_EYES_fin$Set <- substr(All_EYES_fin$Task_Name,1,7)

All_EYES_fin$Set_ID <- paste0(All_EYES_fin$Set, "_", All_EYES_fin$Trial_Id)

length(levels(as.factor(All_EYES_fin$Set)))

Face_IDs$Set_ID <- paste0(Face_IDs$Set, "_", Face_IDs$Trial_Id)

All_EYES_fin <- merge(All_EYES_fin, Face_IDs, by="Set_ID")

All_EYES_fin <-All_EYES_fin[order(All_EYES_fin$order), ]

All_EYES_fin$Real_ID <- paste0(All_EYES_fin[,14],"_",All_EYES_fin[,9])

colnames(Dem_sum)[1]<-"Rec_Session_Id"

colnames(seldemog)[colnames(seldemog) == "Particitpant_ID"] <- "Rec_Session_Id"

length(levels(as.factor(seldemog$Rec_Session_Id)))
length(levels(as.factor(All_EYES_fin$Rec_Session_Id)))

setdiff(All_EYES_fin$Rec_Session_Id, seldemog$Rec_Session_Id)

All_EYES_fin <- merge(All_EYES_fin, seldemog, by="Rec_Session_Id")

length(levels(as.factor(All_EYES_fin$Rec_Session_Id)))

summary(as.factor(seldemog$Culture)) # Some numbers are lower but still acceptable...
# If we exclude all to have NAs in demography, we're still OK

All_EYES_fin <-All_EYES_fin[order(All_EYES_fin$order), ]


# Adding eye colour measures: 
LAB <- read.csv2("LAB_eyes.csv",T)
head(LAB)
head(All_EYES_fin)[14]

LAB$ID2 <- c(rep("CZ_16_F",50), rep("CZ_19_F",56), rep("CZ_16_M",50),rep("CZ_19_M",39))

All_EYES_fin$Real_ID
LAB$Real_ID <- paste0(substr(LAB$ID,1,nchar(LAB$ID)-4),"_",LAB$ID2)

length(levels(as.factor(All_EYES_fin$Real_ID)))
length(levels(as.factor(LAB$Real_ID)))

# Scale L* a* b* values (and do it per face):
stan_L_iris <- function(x){(x-mean(LAB$L_iris))/sd(LAB$L_iris)}
unstan_L_iris <- function(x){x*sd(LAB$L_iris)+mean(LAB$L_iris)}

stan_a_iris <- function(x){(x-mean(LAB$a_iris))/sd(LAB$a_iris)}
unstan_a_iris <- function(x){x*sd(LAB$a_iris)+mean(LAB$a_iris)}

stan_b_iris <- function(x){(x-mean(LAB$b_iris))/sd(LAB$b_iris)}
unstan_b_iris <- function(x){x*sd(LAB$b_iris)+mean(LAB$b_iris)}

LAB$L_iris_scaled <- stan_L_iris(LAB$L_iris)
LAB$a_iris_scaled <- stan_a_iris(LAB$a_iris)
LAB$b_iris_scaled <- stan_b_iris(LAB$b_iris)


Check1 <- data.frame(
  A=levels(as.factor(All_EYES_fin$Real_ID)),
  B=levels(as.factor(LAB$Real_ID))
)

(Check1$A==Check1$B)

LAB$Real_ID <- as.character(LAB$Real_ID)
All_EYES_fin$Real_ID <- as.character(All_EYES_fin$Real_ID)


All_EYES_fin <- merge(LAB, All_EYES_fin, by="Real_ID") # LAB Tím pádem strkáš na začátek

All_EYES_fin <-All_EYES_fin[order(All_EYES_fin$order), ]

View(All_EYES_fin[,c(1:6,7,8:11,16:18,23,24,54:61)])

save(All_EYES_fin, file="All_EYES_fin_07_10_25.Rdata")
write.csv(All_EYES_fin, file="All_EYES_fin_07_10_25.csv",row.names=F)

All_EYES_fin[is.na(All_EYES_fin$Rec_Session_Id),]
All_EYES_fin[is.na(All_EYES_fin$EyeCol),]
# No NAs... 

All_EYES_fin[is.na(All_EYES_fin$ATCH2),]


rm(list=ls())
