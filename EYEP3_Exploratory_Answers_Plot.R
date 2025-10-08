# What I want / need: 

# Upload data, subset by culture, calculate...
load("All_EYES_fin_08_10_25.Rdata")

summary(as.factor(All_EYES_fin$culture))

# We do not need to "clean the data" since we only include participants who answered all
# the questions (rated all faces, reported whether & how they use social media, travel abroad, etc.)
# plus we know they all passed attention checks are older than 18 years, etc. 

AUS_data <- All_EYES_fin[All_EYES_fin$culture=="AUS_NZ",] # 4085 rows is OK
summary(as.factor(AUS_data$Rec_Session_Id)) # 100 or 95 is OK 

COL_data <- All_EYES_fin[All_EYES_fin$culture=="COL",] # 2250 rows is OK 
summary(as.factor(COL_data$Rec_Session_Id)) # 100 or 95 is OK 

CZ_data <- All_EYES_fin[All_EYES_fin$culture=="CZ",] # 11015 rows is OK 
hist(summary(as.factor(CZ_data$Rec_Session_Id), maxsum=1e4)) # 100 or 95 is OK 

RSA_data <- All_EYES_fin[All_EYES_fin$culture=="RSA",] # OK
summary(as.factor(RSA_data$Rec_Session_Id)) # OK

TUR_data <- All_EYES_fin[All_EYES_fin$culture=="TUR",] # OK
summary(as.factor(TUR_data$Rec_Session_Id)) # OK

VN_data <- All_EYES_fin[All_EYES_fin$culture=="VN",] # OK
summary(as.factor(VN_data$Rec_Session_Id)) # OK


# 95 and 100 is legit - one half saw faces from 2016 ~ 50F and 50M
# another half saw faces from 2019 ~ 39M and 56F
All_EYES_fin$Full_Rater_ID <- paste0(All_EYES_fin$culture, "_", All_EYES_fin$Rec_Session_Id);All_EYES_fin$Full_Rater_ID

All_EYES_Data_16 <- All_EYES_fin[c(All_EYES_fin$ID2=="CZ_16_F" | All_EYES_fin$ID2=="CZ_16_M"),]
All_EYES_Data_19 <- All_EYES_fin[c(All_EYES_fin$ID2=="CZ_19_F" | All_EYES_fin$ID2=="CZ_19_M"),]

EYES_16 <- All_EYES_Data_16[,c(1,17,62)]
EYES_19 <- All_EYES_Data_19[,c(1,17,62)]

EYES_16 <- reshape(EYES_16,
                             idvar = "Full_Rater_ID", # RATEROVO ID
                             timevar = "Real_ID", # Ksichtovo ID
                             direction = "wide")

EYES_16$Culture <- substr(EYES_16$Full_Rater_ID,1,3)

EYES_19 <- reshape(EYES_19,
                   idvar = "Full_Rater_ID",
                   timevar = "Real_ID",
                   direction = "wide")

EYES_19$Culture <- substr(EYES_19$Full_Rater_ID,1,3)


# identify the “face” columns
face.cols <- setdiff(names(EYES_16), c("Full_Rater_ID","Culture"))

# the six culture labels and the three colours
cultures <- unique(EYES_16$Culture)
colours  <- c("Blue","Brown","Other")

# prepare an empty result matrix
res.mat <- matrix(
  0,
  nrow = length(cultures)*length(colours),
  ncol = length(face.cols),
  dimnames = list(
    # row names = “culture:colour”
    paste0(
      rep(cultures, each=length(colours)),
      ":",
      rep(colours, times=length(cultures))
    ),
    # col names = face1, face2, …
    face.cols
  )
)

# ---------- fill it by looping ----------
for(f in face.cols){
  # a table of counts: rows = cultures, cols = colours
  cnt <- table(EYES_16$Culture, EYES_16[[f]])
  # convert to row‐wise proportions
  prop <- prop.table(cnt, margin = 1)
  
  # now shove those proportions into res.mat, filling zeros if missing
  for(c in cultures) {
    for(col in colours) {
      res.mat[paste0(c,":",col), f] <-
        if(col %in% colnames(prop)) prop[c, col] else 0
    }
  }
}

# turn into a data frame
res.df <- as.data.frame(res.mat)

class(res.mat)

# inspect
res.df

# Order the columns according to frequency of blue
order_blue <- colMeans(res.df[c(1,4,7,10,13,16),])
ord <- order(order_blue, decreasing = T)

res.df.sorted <- res.df[, ord]

# Six Rows, one column...

AUS_16 <- res.df.sorted[c(1:3),]
COL_16 <- res.df.sorted[c(4:6),]
CZ_16 <- res.df.sorted[c(7:9),]

RSA_16 <- res.df.sorted[c(10:12),]
TUR_16 <- res.df.sorted[c(13:15),]
VN_16 <- res.df.sorted[c(16:18),]

Add_Colnames <- substr(colnames(AUS_16),8,nchar(colnames(AUS_16))-2)


tiff("Rated_eye_2016_NEW.tif",width=32,height=20,units="cm",res=600,compression = "lzw")

layout(matrix(1:6,byrow=T,nrow=6), widths=c(1),heights=c(1,1,1,1,1,1.7))

# Make sure rows are in the order you want: Blue, Brown, Other
AUS_16_2 <- as.matrix(AUS_16)

# [1] "AUS:Blue"  "AUS:Brown" "AUS:Other"

# Choose colors for the stacks
cols <- c("skyblue", "saddlebrown", "lightgrey")

par(mar=c(1.1, 1.1, 1.1, 1.1),mgp=c(0,-2,-3))

# Draw the stacked barplot
barplot(
  AUS_16_2,
  beside    = FALSE,        # stacked (not side-by-side)
  col       = cols,         # one color per row
  border    = NA,           # no border lines
  space     = 0,            # bars flush together
  las       = 1,            # make column labels perpendicular if you want
  ylim      = c(0, 1),      # since proportions sum to 1
  ylab      = " ",
  xaxt      = "n",
  main      = "Eye-colour proportions per face: 2016"
)


# Colombia: 

COL_16_2 <- as.matrix(COL_16)
# [1] "AUS:Blue"  "AUS:Brown" "AUS:Other"

# Draw the stacked barplot

par(mar=c(1.1, 1.1, 1.1, 1.1),mgp=c(0,-2,-3))

barplot(
  COL_16_2,
  beside    = FALSE,        # stacked (not side-by-side)
  col       = cols,         # one color per row
  border    = NA,           # no border lines
  space     = 0,            # bars flush together
  las       = 1,            # make column labels perpendicular if you want
  ylim      = c(0, 1),      # since proportions sum to 1
  ylab      = " ",
  xaxt      = "n"
#  main      = "Eye-colour proportions per face"
)


# Czechia 

CZ_16_2 <- as.matrix(CZ_16)
# [1] "AUS:Blue"  "AUS:Brown" "AUS:Other"

# Draw the stacked barplot

par(mar=c(1.1, 1.1, 1.1, 1.1),mgp=c(0,-2,-3))

barplot(
  CZ_16_2,
  beside    = FALSE,        # stacked (not side-by-side)
  col       = cols,         # one color per row
  border    = NA,           # no border lines
  space     = 0,            # bars flush together
  las       = 1,            # make column labels perpendicular if you want
  ylim      = c(0, 1),      # since proportions sum to 1
  ylab      = " ",
  xaxt      = "n"
#  main      = "Eye-colour proportions per face: 2016"
)


# RSA 

RSA_16_2 <- as.matrix(RSA_16)
# [1] "AUS:Blue"  "AUS:Brown" "AUS:Other"

# Draw the stacked barplot

par(mar=c(1.1, 1.1, 1.1, 1.1),mgp=c(0,-2,-3))

barplot(
  RSA_16_2,
  beside    = FALSE,        # stacked (not side-by-side)
  col       = cols,         # one color per row
  border    = NA,           # no border lines
  space     = 0,            # bars flush together
  las       = 1,            # make column labels perpendicular if you want
  ylim      = c(0, 1),      # since proportions sum to 1
  ylab      = " ",
  xaxt      = "n"
  #  main      = "Eye-colour proportions per face"
)


# TUR 

TUR_16_2 <- as.matrix(TUR_16)
# [1] "AUS:Blue"  "AUS:Brown" "AUS:Other"

# Draw the stacked barplot

par(mar=c(1.1, 1.1, 1.1, 1.1),mgp=c(0,-2,-3))

barplot(
  TUR_16_2,
  beside    = FALSE,        # stacked (not side-by-side)
  col       = cols,         # one color per row
  border    = NA,           # no border lines
  space     = 0,            # bars flush together
  las       = 1,            # make column labels perpendicular if you want
  ylim      = c(0, 1),      # since proportions sum to 1
  ylab      = " ",
  xaxt      = "n"
  #  main      = "Eye-colour proportions per face"
)


# VN 

VN_16_2 <- as.matrix(VN_16)
colnames(VN_16_2) <- Add_Colnames

# Draw the stacked barplot
par(mar = c(7.1, 1.1, 1.1, 1.1), mgp = c(0, 0, -3))

# Draw without automatic axes
bp <- barplot(
  VN_16_2,
  beside = FALSE,
  col = cols,
  border = NA,
  space = 0,
  las = 2,
  ylim = c(0, 1),
  ylab = " ",
  xaxt = "n",  # suppress default x-axis
  yaxt = "n"   # suppress default y-axis
)

# Redraw Y axis normally (avoids the left shift)
axis(side = 2, las = 1, mgp = c(3, -2, 0))

# Redraw X axis labels (keep them lowered as before)
axis(side = 1, at = bp, labels = colnames(VN_16_2), las = 2, tick = FALSE, mgp = c(0, 0, -3))

dev.off()





# POSEM!





# identify the “face” columns
face.cols <- setdiff(names(EYES_19), c("Full_Rater_ID","Culture"))

# the six culture labels and the three colours
cultures <- unique(EYES_19$Culture)
colours  <- c("Blue","Brown","Other")

# prepare an empty result matrix
res.mat <- matrix(
  0,
  nrow = length(cultures)*length(colours),
  ncol = length(face.cols),
  dimnames = list(
    # row names = “culture:colour”
    paste0(
      rep(cultures, each=length(colours)),
      ":",
      rep(colours, times=length(cultures))
    ),
    # col names = face1, face2, …
    face.cols
  )
)

# ---------- fill it by looping ----------
for(f in face.cols){
  # a table of counts: rows = cultures, cols = colours
  cnt <- table(EYES_19$Culture, EYES_19[[f]])
  # convert to row‐wise proportions
  prop <- prop.table(cnt, margin = 1)
  
  # now shove those proportions into res.mat, filling zeros if missing
  for(c in cultures) {
    for(col in colours) {
      res.mat[paste0(c,":",col), f] <-
        if(col %in% colnames(prop)) prop[c, col] else 0
    }
  }
}

# turn into a data frame
res.df <- as.data.frame(res.mat)

class(res.mat)

# inspect
res.df

# Order the columns according to frequency of blue
order_blue <- colMeans(res.df[c(1,4,7,10,13,16),])
ord <- order(order_blue, decreasing = T)

res.df.sorted <- res.df[, ord]

# Six Rows, one column...

AUS_19 <- res.df.sorted[c(1:3),]
COL_19 <- res.df.sorted[c(4:6),]
CZ_19 <- res.df.sorted[c(7:9),]

RSA_19 <- res.df.sorted[c(10:12),]
TUR_19 <- res.df.sorted[c(13:15),]
VN_19 <- res.df.sorted[c(16:18),]

Add_Colnames <- substr(colnames(AUS_19),8,nchar(colnames(AUS_19))-2)


tiff("Rated_eye_2019.tif",width=32,height=20,units="cm",res=600,compression = "lzw")

layout(matrix(1:6,byrow=T,nrow=6), widths=c(1),heights=c(1,1,1,1,1,1.7))

# Make sure rows are in the order you want: Blue, Brown, Other
AUS_19_2 <- as.matrix(AUS_19)

# [1] "AUS:Blue"  "AUS:Brown" "AUS:Other"

# Choose colors for the stacks
cols <- c("skyblue", "saddlebrown", "lightgrey")

par(mar=c(1.1, 1.1, 1.1, 1.1),mgp=c(0,-2,-3))

# Draw the stacked barplot
barplot(
  AUS_19_2,
  beside    = FALSE,        # stacked (not side-by-side)
  col       = cols,         # one color per row
  border    = NA,           # no border lines
  space     = 0,            # bars flush together
  las       = 2,            # make column labels perpendicular if you want
  ylim      = c(0, 1),      # since proportions sum to 1
  ylab      = " ",
  xaxt      = "n",
  main      = "Eye-colour proportions per face: 2019"
)


# Colombia: 

COL_19_2 <- as.matrix(COL_19)
# [1] "AUS:Blue"  "AUS:Brown" "AUS:Other"

# Draw the stacked barplot

par(mar=c(1.1, 1.1, 1.1, 1.1),mgp=c(0,-2,-3))

barplot(
  COL_19_2,
  beside    = FALSE,        # stacked (not side-by-side)
  col       = cols,         # one color per row
  border    = NA,           # no border lines
  space     = 0,            # bars flush together
  las       = 2,            # make column labels perpendicular if you want
  ylim      = c(0, 1),      # since proportions sum to 1
  ylab      = " ",
  xaxt      = "n"
  #  main      = "Eye-colour proportions per face"
)


# Czechia 

CZ_19_2 <- as.matrix(CZ_19)
# [1] "AUS:Blue"  "AUS:Brown" "AUS:Other"

# Draw the stacked barplot

par(mar=c(1.1, 1.1, 1.1, 1.1),mgp=c(0,-2,-3))

barplot(
  CZ_19_2,
  beside    = FALSE,        # stacked (not side-by-side)
  col       = cols,         # one color per row
  border    = NA,           # no border lines
  space     = 0,            # bars flush together
  las       = 2,            # make column labels perpendicular if you want
  ylim      = c(0, 1),      # since proportions sum to 1
  ylab      = " ",
  xaxt      = "n"
  #  main      = "Eye-colour proportions per face: 2016"
)


# RSA 

RSA_19_2 <- as.matrix(RSA_19)
# [1] "AUS:Blue"  "AUS:Brown" "AUS:Other"

# Draw the stacked barplot

par(mar=c(1.1, 1.1, 1.1, 1.1),mgp=c(0,-2,-3))

barplot(
  RSA_19_2,
  beside    = FALSE,        # stacked (not side-by-side)
  col       = cols,         # one color per row
  border    = NA,           # no border lines
  space     = 0,            # bars flush together
  las       = 2,            # make column labels perpendicular if you want
  ylim      = c(0, 1),      # since proportions sum to 1
  ylab      = " ",
  xaxt      = "n"
  #  main      = "Eye-colour proportions per face"
)


# TUR 

TUR_19_2 <- as.matrix(TUR_19)
# [1] "AUS:Blue"  "AUS:Brown" "AUS:Other"

# Draw the stacked barplot

par(mar=c(1.1, 1.1, 1.1, 1.1),mgp=c(0,-2,-3))

barplot(
  TUR_19_2,
  beside    = FALSE,        # stacked (not side-by-side)
  col       = cols,         # one color per row
  border    = NA,           # no border lines
  space     = 0,            # bars flush together
  las       = 2,            # make column labels perpendicular if you want
  ylim      = c(0, 1),      # since proportions sum to 1
  ylab      = " ",
  xaxt      = "n"
  #  main      = "Eye-colour proportions per face"
)


# VN 

VN_19_2 <- as.matrix(VN_19)
colnames(VN_19_2)<-Add_Colnames

# [1] "AUS:Blue"  "AUS:Brown" "AUS:Other"

# Draw the stacked barplot

par(mar=c(7.1, 1.1, 1.1, 1.1),mgp=c(0,0,-3))

barplot(
  VN_19_2,
  beside    = FALSE,        # stacked (not side-by-side)
  col       = cols,         # one color per row
  border    = NA,           # no border lines
  space     = 0,            # bars flush together
  las       = 2,            # make column labels perpendicular if you want
  ylim      = c(0, 1),      # since proportions sum to 1
  ylab      = " "
  #  main      = "Eye-colour proportions per face"
)

dev.off()