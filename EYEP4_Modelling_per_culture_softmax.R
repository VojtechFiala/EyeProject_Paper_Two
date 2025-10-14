library(rethinking)

# Upload data, subset by culture, calculate...
load("All_EYES_fin_07_10_25.Rdata")

# This time, all the participants should be already from the "reliable" group - all finished the whole survey etc.

All_EYES_Data <- All_EYES_fin

summary(as.factor(All_EYES_Data$culture))

View(All_EYES_Data[is.na(All_EYES_Data$EyeCol),])

summary(as.factor(All_EYES_Data$EyeCol))

# Let's first create a short summary table that contains: 
# Row - one for each face
# columns: 1 = number it was marked blue, brown, other (altogether) 
# 2-4 = the number it was marked blue brown other in AUS/NZ
# 5-7 = the number it was marked blue brown other in Colombia
# 8-10 = the number it was marked blue brown other in Czechia
# 11-13 = the number it was marked blue brown other in RSA
# 14-16 = the number it was marked blue brown other in Turkey
# 17-19 = the number it was marked blue brown other in Vietnam

SumCols <- with(All_EYES_Data, table(as.factor(Real_ID),EyeCol))
SumCols <- as.data.frame.matrix(SumCols)
Nums <- as.data.frame(rowSums(SumCols))
SumCols$Nums <- Nums[1,]

SumCols$BlueRatio <- SumCols[,1]/SumCols[,4]
SumCols$BrownRatio <- SumCols[,2]/SumCols[,4]
SumCols$OtherRatio <- SumCols[,3]/SumCols[,4]
SumCols$Cul <- "ALL"


# The same only for Australia: 
AUS_data <- All_EYES_fin[All_EYES_fin$culture=="AUS_NZ",]

SumColsAUS <- with(AUS_data, table(as.factor(Real_ID),EyeCol))
SumColsAUS <- as.data.frame.matrix(SumColsAUS)
Nums <- as.data.frame(rowSums(SumColsAUS))
SumColsAUS$Nums <- Nums[1,]

SumColsAUS$BlueRatio <- SumColsAUS[,1]/SumColsAUS[,4]
SumColsAUS$BrownRatio <- SumColsAUS[,2]/SumColsAUS[,4]
SumColsAUS$OtherRatio <- SumColsAUS[,3]/SumColsAUS[,4]
SumColsAUS$Cul <- "AUS"


# The same only for Colombia: 
COL_data <- All_EYES_fin[All_EYES_fin$culture=="COL",]

SumColsCOL <- with(COL_data, table(as.factor(Real_ID),EyeCol))
SumColsCOL <- as.data.frame.matrix(SumColsCOL)
Nums <- as.data.frame(rowSums(SumColsCOL))
SumColsCOL$Nums <- Nums[1,]

SumColsCOL$BlueRatio <- SumColsCOL[,1]/SumColsCOL[,4]
SumColsCOL$BrownRatio <- SumColsCOL[,2]/SumColsCOL[,4]
SumColsCOL$OtherRatio <- SumColsCOL[,3]/SumColsCOL[,4]
SumColsCOL$Cul <- "COL"


# The same only for Czechia: 
CZ_data <- All_EYES_fin[All_EYES_fin$culture=="CZ",]

SumColsCZ <- with(CZ_data, table(as.factor(Real_ID),EyeCol))
SumColsCZ <- as.data.frame.matrix(SumColsCZ)
Nums <- as.data.frame(rowSums(SumColsCZ))
SumColsCZ$Nums <- Nums[1,]

SumColsCZ$BlueRatio <- SumColsCZ[,1]/SumColsCZ[,4]
SumColsCZ$BrownRatio <- SumColsCZ[,2]/SumColsCZ[,4]
SumColsCZ$OtherRatio <- SumColsCZ[,3]/SumColsCZ[,4]
SumColsCZ$Cul <- "CZ"


# The same only for RSA: 
RSA_data <- All_EYES_fin[All_EYES_fin$culture=="RSA",]

SumColsRSA <- with(RSA_data, table(as.factor(Real_ID),EyeCol))
SumColsRSA <- as.data.frame.matrix(SumColsRSA)
Nums <- as.data.frame(rowSums(SumColsRSA))
SumColsRSA$Nums <- Nums[1,]

SumColsRSA$BlueRatio <- SumColsRSA[,1]/SumColsRSA[,4]
SumColsRSA$BrownRatio <- SumColsRSA[,2]/SumColsRSA[,4]
SumColsRSA$OtherRatio <- SumColsRSA[,3]/SumColsRSA[,4]
SumColsRSA$Cul <- "RSA"


# The same only for TUR: 
TUR_data <- All_EYES_fin[All_EYES_fin$culture=="TUR",]

SumColsTUR <- with(TUR_data, table(as.factor(Real_ID),EyeCol))
SumColsTUR <- as.data.frame.matrix(SumColsTUR)
Nums <- as.data.frame(rowSums(SumColsTUR))
SumColsTUR$Nums <- Nums[1,]

SumColsTUR$BlueRatio <- SumColsTUR[,1]/SumColsTUR[,4]
SumColsTUR$BrownRatio <- SumColsTUR[,2]/SumColsTUR[,4]
SumColsTUR$OtherRatio <- SumColsTUR[,3]/SumColsTUR[,4]
SumColsTUR$Cul <- "TUR"


# The same only for VN: 
VN_data <- All_EYES_fin[All_EYES_fin$culture=="VN",]

SumColsVN <- with(VN_data, table(as.factor(Real_ID),EyeCol))
SumColsVN <- as.data.frame.matrix(SumColsVN)
Nums <- as.data.frame(rowSums(SumColsVN))
SumColsVN$Nums <- Nums[1,]

SumColsVN$BlueRatio <- SumColsVN[,1]/SumColsVN[,4]
SumColsVN$BrownRatio <- SumColsVN[,2]/SumColsVN[,4]
SumColsVN$OtherRatio <- SumColsVN[,3]/SumColsVN[,4]
SumColsVN$Cul <- "VN"

sum_all <- rbind.data.frame(SumCols,SumColsAUS,SumColsCOL,SumColsCZ,SumColsRSA,SumColsTUR,SumColsVN)

tapply(as.factor(All_EYES_fin$EyeCol), All_EYES_fin$culture,summary)

save(sum_all, file="Sum_all_numbers_and_ratios.Rdata")
write.csv2(sum_all, file="Sum_all_numbers_and_ratios.csv")

sum_all_wide <- cbind.data.frame(SumColsAUS[,c(5:8)],SumColsCOL[,(5:8)],SumColsCZ[,(5:8)],
                                 SumColsRSA[,(5:8)],SumColsTUR[,(5:8)],SumColsVN[,(5:8)])

summary(as.factor(All_EYES_Data$EyeCol[All_EYES_Data$Sample=="TUR"]))



# Eye colour summary per culture
eye_data <- list(
  AUS.NZ = c(Blue = 1849, Brown = 1396, Other = 840),
  COL = c(Blue = 916, Brown = 749, Other = 585),
  CZ = c(Blue = 4943, Brown = 3784, Other = 2288),
  RSA = c(Blue = 1978, Brown = 1589, Other = 813),
  TUR = c(Blue = 2430, Brown = 1630, Other = 1110),
  VN = c(Blue = 1674, Brown = 1344, Other = 1092)
)

# Convert to matrix
eye_mat <- do.call(rbind, eye_data)

# Optional: Normalize proportions for each row if you want all bars to be equal width
eye_prop <- t(apply(eye_mat, 1, function(x) x / sum(x)))

# Define colours
eye_colours <- c("Blue" = "#1f77b4", "Brown" = "#8c564b", "Other" = "#7f7f7f")

# Create the bar plot
barplot(
  t(eye_prop),
  beside = FALSE,  horiz = TRUE,
  col = eye_colours,
  border = NA,
  names.arg = rownames(eye_mat),
  xlab = "Proportion",
  main = "Eye Colour Distribution by Culture",
  cex.lab = 1,
  cex.names = 0.8
)





# Consensual model: 

code_eyecol <- "
data{
int N; // number of observations (vetsi)
int Nr; // number of raters
int K; // number of outcome values (3)
array[N] int colour; // outcome
array[N] real Lightness;
array[N] real redness;
array[N] real yellowness;
array[N] int rID;
}
parameters{
real a; // intercepts [může se taky psát: vector[1] a
matrix[16,Nr] zr; // rater effects
vector<lower=0>[16] sigmaR;
cholesky_factor_corr[16] L_Rho_R;
vector[K-1] bL; // coefficients on Lightness
vector[K-1] ba; // coefficients on redness
vector[K-1] bb; // coefficients on yellowness
vector[K-1] bint_ab; // interaction red:yellow channel
vector[K-1] bint_aL; // interaction red:ligtness channel
vector[K-1] bint_bL; // interaction yellow:lightness channel
vector[K-1] b_trujka; // triple interaction
}
transformed parameters{
    matrix[Nr,16] vr;
    vr = (diag_pre_multiply(sigmaR, L_Rho_R) * zr)';
}
model{
vector[K] p;
vector[K] s;

L_Rho_R ~ lkj_corr_cholesky( 1 );
sigmaR ~ exponential( 1 );
to_vector( zr ) ~ normal( 0 , 1 );

a ~ normal(0,1.5); // může bejt taky s indexem 1
bL ~ normal(0,1);
ba ~ normal(0,1);
bb ~ normal(0,1);
bint_ab ~ normal(0,1);
bint_aL ~ normal(0,1);
bint_bL ~ normal(0,1);
b_trujka ~ normal(0,1);
for(i in 1:N){
s[1] = 0 + bL[1]*Lightness[i] + ba[1]*redness[i] + bb[1]*yellowness[i] + bint_ab[1]*redness[i]*yellowness[i] + bint_aL[1]*redness[i]*Lightness[i] + bint_bL[1]*yellowness[i]*Lightness[i] + b_trujka[1]*redness[i]*yellowness[i]*Lightness[i] + vr[rID[i],2] + vr[rID[i],4]*Lightness[i] + vr[rID[i],6]*redness[i] + vr[rID[i],8]*yellowness[i] + vr[rID[i],10]*redness[i]*yellowness[i] + vr[rID[i],12]*redness[i]*Lightness[i] + vr[rID[i],14]*yellowness[i]*Lightness[i] + vr[rID[i],16]*redness[i]*yellowness[i]*Lightness[i]; //BLUE 
s[2] = 0 - bL[1]*Lightness[i] - ba[1]*redness[i] - bb[1]*yellowness[i] - bint_ab[1]*redness[i]*yellowness[i] - bint_aL[1]*redness[i]*Lightness[i] - bint_bL[1]*yellowness[i]*Lightness[i] - b_trujka[1]*redness[i]*yellowness[i]*Lightness[i] - vr[rID[i],2] - vr[rID[i],4]*Lightness[i] - vr[rID[i],6]*redness[i] - vr[rID[i],8]*yellowness[i] - vr[rID[i],10]*redness[i]*yellowness[i] - vr[rID[i],12]*redness[i]*Lightness[i] - vr[rID[i],14]*yellowness[i]*Lightness[i] - vr[rID[i],16]*redness[i]*yellowness[i]*Lightness[i]; // BROWN 
s[3] = a + bL[2]*Lightness[i] + ba[2]*redness[i] + bb[2]*yellowness[i] + bint_ab[2]*redness[i]*yellowness[i] + bint_aL[2]*redness[i]*Lightness[i] + bint_bL[2]*yellowness[i]*Lightness[i] + b_trujka[2]*redness[i]*yellowness[i]*Lightness[i] + vr[rID[i],1] + vr[rID[i],3]*Lightness[i] + vr[rID[i],5]*redness[i] + vr[rID[i],7]*yellowness[i] + vr[rID[i],9]*redness[i]*yellowness[i] + vr[rID[i],11]*redness[i]*Lightness[i] + vr[rID[i],13]*yellowness[i]*Lightness[i] + vr[rID[i],15]*redness[i]*yellowness[i]*Lightness[i]; // OTHER
p = softmax(s);
target +=categorical_lpmf(colour[i]|p);
}
}
generated quantities{
vector[N] log_lik;
vector[K] p;
vector[K] s;
for (i in 1:N){
s[1] = 0 + bL[1]*Lightness[i] + ba[1]*redness[i] + bb[1]*yellowness[i] + bint_ab[1]*redness[i]*yellowness[i] + bint_aL[1]*redness[i]*Lightness[i] + bint_bL[1]*yellowness[i]*Lightness[i] + b_trujka[1]*redness[i]*yellowness[i]*Lightness[i] + vr[rID[i],2] + vr[rID[i],4]*Lightness[i] + vr[rID[i],6]*redness[i] + vr[rID[i],8]*yellowness[i] + vr[rID[i],10]*redness[i]*yellowness[i] + vr[rID[i],12]*redness[i]*Lightness[i] + vr[rID[i],14]*yellowness[i]*Lightness[i] + vr[rID[i],16]*redness[i]*yellowness[i]*Lightness[i]; //BLUE 
s[2] = 0 - bL[1]*Lightness[i] - ba[1]*redness[i] - bb[1]*yellowness[i] - bint_ab[1]*redness[i]*yellowness[i] - bint_aL[1]*redness[i]*Lightness[i] - bint_bL[1]*yellowness[i]*Lightness[i] - b_trujka[1]*redness[i]*yellowness[i]*Lightness[i] - vr[rID[i],2] - vr[rID[i],4]*Lightness[i] - vr[rID[i],6]*redness[i] - vr[rID[i],8]*yellowness[i] - vr[rID[i],10]*redness[i]*yellowness[i] - vr[rID[i],12]*redness[i]*Lightness[i] - vr[rID[i],14]*yellowness[i]*Lightness[i] - vr[rID[i],16]*redness[i]*yellowness[i]*Lightness[i]; // BROWN 
s[3] = a + bL[2]*Lightness[i] + ba[2]*redness[i] + bb[2]*yellowness[i] + bint_ab[2]*redness[i]*yellowness[i] + bint_aL[2]*redness[i]*Lightness[i] + bint_bL[2]*yellowness[i]*Lightness[i] + b_trujka[2]*redness[i]*yellowness[i]*Lightness[i] + vr[rID[i],1] + vr[rID[i],3]*Lightness[i] + vr[rID[i],5]*redness[i] + vr[rID[i],7]*yellowness[i] + vr[rID[i],9]*redness[i]*yellowness[i] + vr[rID[i],11]*redness[i]*Lightness[i] + vr[rID[i],13]*yellowness[i]*Lightness[i] + vr[rID[i],15]*redness[i]*yellowness[i]*Lightness[i]; // OTHER
p = softmax(s);
log_lik[i]=categorical_lpmf( colour[i] | p);
}
}
"


# LAB per culture: 


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# AUSTRALIA
# Scaling functions for L* a* b* 

# LAB per culture: 
str(AUS_data)

# Getting the data coded: 
dat_list <- list(
  # int N; // number of observations (vetsi)
  N = nrow(AUS_data),
  # int Nr; // number of raters
  Nr = length(unique(AUS_data$Rec_Session_Id)),
  # int K; // number of outcome values (3)
  K = length(levels(as.factor(AUS_data$EyeCol))),
  # array[N] int colour; // outcome
  colour = as.numeric(as.factor(AUS_data$EyeCol)),
  # array[N] real Lightness (L*);
  Lightness = as.numeric(AUS_data$L_iris_scaled),
  # array[N] real redness (a*); 
  redness = as.numeric(AUS_data$a_iris_scaled),
  # array[N] real yellowness (b*);
  yellowness = as.numeric(AUS_data$b_iris_scaled),
  # array[N] int rID;
  rID = as.integer(as.factor(AUS_data$Rec_Session_Id))
)


summary.data.frame(dat_list)

table(dat_list$colour, AUS_data$EyeCol)
# The order (if in doubts, do focus on the case and try to tell if I am right):  
# blue and brown are subtracted from one another  (no intercept here) and the category "other" has its own independent  intercept.  
#   Blue Brown Other
# 1 1849     0     0
# 2    0  1396     0
# 3    0     0   840


Mod_AUS <- stan(model_code=code_eyecol, data=dat_list, chains=2, cores=2, iter=5e3)

post_1 <- extract.samples(Mod_AUS)
save(Mod_AUS, file="AUS_NZ_model_13_10_25.Rdata")
saveRDS(post_1, file="AUS_NZ_POST_13_10_25.RDS")

AUS_NZ_precis <- precis(Mod_AUS, depth = 3)
write.csv2(AUS_NZ_precis, file="AUS_NZ_precis.csv")


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# COLOMBIA

# Getting the data coded: 
dat_list <- list(
  # int N; // number of observations (vetsi)
  N = nrow(COL_data),
  # int Nr; // number of raters
  Nr = length(unique(COL_data$Rec_Session_Id)),
  # int K; // number of outcome values (3)
  K = length(levels(as.factor(COL_data$EyeCol))),
  # array[N] int colour; // outcome
  colour = as.numeric(as.factor(COL_data$EyeCol)),
  # array[N] real Lightness (L*);
  Lightness = as.numeric(COL_data$L_iris_scaled),
  # array[N] real redness (a*); 
  redness = as.numeric(COL_data$a_iris_scaled),
  # array[N] real yellowness (b*);
  yellowness = as.numeric(COL_data$b_iris_scaled),
  # array[N] int rID;
  rID = as.integer(as.factor(COL_data$Rec_Session_Id))
)

summary.data.frame(dat_list)

table(dat_list$colour, COL_data$EyeCol)

#   Blue Brown Other
# 1  916     0     0
# 2    0   749     0
# 3    0     0   585

Mod_COL <- stan(model_code=code_eyecol, data=dat_list, chains=2, cores=2, iter=5e3)

post_1 <- extract.samples(Mod_COL)
save(Mod_COL, file="COL_model_13_10_25.Rdata")
saveRDS(post_1, file="COL_POST_13_10_25.RDS")

COL_precis <- precis(Mod_COL, depth = 3)
write.csv2(COL_precis, file="COL_precis.csv")


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# CZECHIA

# Getting the data coded: 
dat_list <- list(
  # int N; // number of observations (vetsi)
  N = nrow(CZ_data),
  # int Nr; // number of raters
  Nr = length(unique(CZ_data$Rec_Session_Id)),
  # int K; // number of outcome values (3)
  K = length(levels(as.factor(CZ_data$EyeCol))),
  # array[N] int colour; // outcome
  colour = as.numeric(as.factor(CZ_data$EyeCol)),
  # array[N] real Lightness (L*);
  Lightness = as.numeric(CZ_data$L_iris_scaled),
  # array[N] real redness (a*); 
  redness = as.numeric(CZ_data$a_iris_scaled),
  # array[N] real yellowness (b*);
  yellowness = as.numeric(CZ_data$b_iris_scaled),
  # array[N] int rID;
  rID = as.integer(as.factor(CZ_data$Rec_Session_Id))
)

summary.data.frame(dat_list)

table(dat_list$colour, CZ_data$EyeCol)

#   Blue Brown Other
# 1 4943     0     0
# 2    0  3784     0
# 3    0     0  2288

Mod_CZ <- stan(model_code=code_eyecol, data=dat_list, chains=2, cores=2, iter=5e3)

post_1 <- extract.samples(Mod_CZ)
save(Mod_CZ, file="CZ_model_13_10_25.Rdata")
saveRDS(post_1, file="CZ_POST_13_10_25.RDS")

CZ_precis <- precis(Mod_CZ, depth = 3)
write.csv2(CZ_precis, file="CZ_precis.csv")


# posem


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# RSA 

# Getting the data coded: 
dat_list <- list(
  # int N; // number of observations (vetsi)
  N = nrow(RSA_data),
  # int Nr; // number of raters
  Nr = length(unique(RSA_data$Rec_Session_Id)),
  # int K; // number of outcome values (3)
  K = length(levels(as.factor(RSA_data$EyeCol))),
  # array[N] int colour; // outcome
  colour = as.numeric(as.factor(RSA_data$EyeCol)),
  # array[N] real Lightness (L*);
  Lightness = as.numeric(RSA_data$L_iris_scaled),
  # array[N] real redness (a*); 
  redness = as.numeric(RSA_data$a_iris_scaled),
  # array[N] real yellowness (b*);
  yellowness = as.numeric(RSA_data$b_iris_scaled),
  # array[N] int rID;
  rID = as.integer(as.factor(RSA_data$Rec_Session_Id))
)

summary.data.frame(dat_list)

table(dat_list$colour, RSA_data$EyeCol)

#   Blue Brown Other
# 1 1978     0     0
# 2    0  1589     0
# 3    0     0   813

Mod_RSA <- stan(model_code=code_eyecol, data=dat_list, chains=2, cores=2, iter=5e3)

post_1 <- extract.samples(Mod_RSA)
save(Mod_RSA, file="RSA_model_13_10_25.Rdata")
saveRDS(post_1, file="RSA_POST_13_10_25.RDS")

RSA_precis <- precis(Mod_RSA, depth = 3)
write.csv2(RSA_precis, file="RSA_precis.csv")


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# TUR 

# Getting the data coded: 
dat_list <- list(
  # int N; // number of observations (vetsi)
  N = nrow(TUR_data),
  # int Nr; // number of raters
  Nr = length(unique(TUR_data$Rec_Session_Id)),
  # int K; // number of outcome values (3)
  K = length(levels(as.factor(TUR_data$EyeCol))),
  # array[N] int colour; // outcome
  colour = as.numeric(as.factor(TUR_data$EyeCol)),
  # array[N] real Lightness (L*);
  Lightness = as.numeric(TUR_data$L_iris_scaled),
  # array[N] real redness (a*); 
  redness = as.numeric(TUR_data$a_iris_scaled),
  # array[N] real yellowness (b*);
  yellowness = as.numeric(TUR_data$b_iris_scaled),
  # array[N] int rID;
  rID = as.integer(as.factor(TUR_data$Rec_Session_Id))
)

summary.data.frame(dat_list)

table(dat_list$colour, TUR_data$EyeCol)

#   Blue Brown Other
# 1 2430     0     0
# 2    0  1630     0
# 3    0     0  1110


Mod_TUR <- stan(model_code=code_eyecol, data=dat_list, chains=2, cores=2, iter=5e3)

post_1 <- extract.samples(Mod_TUR)
save(Mod_TUR, file="TUR_model_13_10_25.Rdata")
saveRDS(post_1, file="TUR_POST_13_10_25.RDS")

TUR_precis <- precis(Mod_TUR, depth = 3)
write.csv2(TUR_precis, file="TUR_precis.csv")


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# VN 

# Getting the data coded: 
dat_list <- list(
  # int N; // number of observations (vetsi)
  N = nrow(VN_data),
  # int Nr; // number of raters
  Nr = length(unique(VN_data$Rec_Session_Id)),
  # int K; // number of outcome values (3)
  K = length(levels(as.factor(VN_data$EyeCol))),
  # array[N] int colour; // outcome
  colour = as.numeric(as.factor(VN_data$EyeCol)),
  # array[N] real Lightness (L*);
  Lightness = as.numeric(VN_data$L_iris_scaled),
  # array[N] real redness (a*); 
  redness = as.numeric(VN_data$a_iris_scaled),
  # array[N] real yellowness (b*);
  yellowness = as.numeric(VN_data$b_iris_scaled),
  # array[N] int rID;
  rID = as.integer(as.factor(VN_data$Rec_Session_Id))
)

summary.data.frame(dat_list)

table(dat_list$colour, VN_data$EyeCol)

#    Blue Brown Other
# 1  1674     0     0
# 2     0  1344     0
# 3     0     0  1092


Mod_VN <- stan(model_code=code_eyecol, data=dat_list, chains=2, cores=2, iter=5e3)

post_1 <- extract.samples(Mod_VN)
save(Mod_VN, file="VN_model_13_10_25.Rdata")
saveRDS(post_1, file="VN_POST_13_10_25.RDS")

VN_precis <- precis(Mod_VN, depth = 3)
write.csv2(VN_precis, file="VN_precis.csv")