# LOAD PACKAGES ####
library(R.matlab)
library(ggplot2)
library(MASS)
library(stats)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)

# MAIN DIRECTORY ####
data.dir = "C:/Users/Julia/Documents/01 - Projects/01 - LSCP/04 - Modeling team stuff/ivectors/bilingual_test_balancedv2_tv150//bilingual_test_balancedv2_tv150/"

# LABELS (SPEAKER & LANGUAGE) ####
perUtterancenames = c(paste("E_", (rep(1, 10))%o%((1:10)), ":", ((1:10))%o%(rep(1, 10)), sep = ""), paste("X_",(rep(1,10))%o%((1:10)),":",((1:10))%o%(rep(1,10)),sep=""))
speakers = gsub(":.*", "", perUtterancenames)
names(speakers) = perUtterancenames
language = gsub("_.*", "", perUtterancenames)
names(language) = perUtterancenames

# MDS ####

# Bilingual background
setwd(paste(data.dir, "bilingual_background/", sep = ""))
m = readMat("model_ivs1_perUtterance.mat"); m = m[[1]]

# 1. Calculate distances and MDS
d = dist(t(m))
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1] # MDS coordinate 1
y   = fit$points[, 2] # MDS coordinate 2

# 2. Organise in data frame
mds.bil = data.frame(speakers, language, x, y)
mds.bil$background = rep("Mixed", 200)
mds.bil$LDA        = rep("pre-LDA", 200)
mds.bil$x          = scale(mds.bil$x, center = FALSE, scale = max(mds.bil$x, na.rm = TRUE))
mds.bil$y          = scale(mds.bil$y, center = FALSE, scale = max(mds.bil$y, na.rm = TRUE))

# 3. Perform LDA and repeat steps 1 & 2
l       = lda(t(m), speakers)
project = t(m)%*%l$scaling
x1      = project #[,1:19,drop=F]

d   = dist(x1)
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1]
y   = fit$points[, 2]

mds.bilLDA = data.frame(speakers, language, x, y)
mds.bilLDA$background = rep("Mixed", 200)
mds.bilLDA$LDA        = rep("post-LDA", 200)
mds.bilLDA$x = scale(mds.bilLDA$x, center = FALSE, scale = max(mds.bilLDA$x, na.rm = TRUE))
mds.bilLDA$y = scale(mds.bilLDA$y, center = FALSE, scale = max(mds.bilLDA$y, na.rm = TRUE))


# Xitsonga background
setwd(paste(data.dir, "xitsonga_background/", sep = ""))
m = readMat("model_ivs1_perUtterance.mat"); m = m[[1]]

# 1.
d   = dist(t(m))
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1] # MDS coordinate 1
y   = fit$points[, 2] # MDS coordinate 2

# 2.
mds.xit = data.frame(speakers, language, x, y)
mds.xit$background = rep("Xitsonga", 200)
mds.xit$LDA        = rep("pre-LDA", 200)
mds.xit$x = scale(mds.xit$x, center = FALSE, scale = max(mds.xit$x, na.rm = TRUE))
mds.xit$y = scale(mds.xit$y, center = FALSE, scale = max(mds.xit$y, na.rm = TRUE))

# 3.
l       = lda(t(m), speakers)
project = t(m)%*%l$scaling
x1      = project #[,1:19,drop=F]

d   = dist(x1)
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1]
y   = fit$points[, 2]

mds.xitLDA = data.frame(speakers, language, x, y)
mds.xitLDA$background = rep("Xitsonga", 200)
mds.xitLDA$LDA        = rep("post-LDA", 200)
mds.xitLDA$x   = scale(mds.xitLDA$x, center = FALSE, scale = max(mds.xitLDA$x, na.rm = TRUE))
mds.xitLDA$y   = scale(mds.xitLDA$y, center = FALSE, scale = max(mds.xitLDA$y, na.rm = TRUE))


# English background
setwd(paste(data.dir, "english_background/", sep = ""))
m = readMat("model_ivs1_perUtterance.mat"); m = m[[1]]

# 1.
d   = dist(t(m))
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1] # MDS coordinate 1
y   = fit$points[, 2] # MDS coordinate 2

# 2.
mds.eng = data.frame(speakers, language, x, y)
mds.eng$background = rep("English", 200)
mds.eng$LDA        = rep("pre-LDA", 200)
mds.eng$x   = scale(mds.eng$x, center = FALSE, scale = max(mds.eng$x, na.rm = TRUE))
mds.eng$y   = scale(mds.eng$y, center = FALSE, scale = max(mds.eng$y, na.rm = TRUE))

# 3.
l       = lda(t(m), speakers)
project = t(m)%*%l$scaling
x1      = project #[,1:19,drop=F]

d   = dist(x1)
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1]
y   = fit$points[, 2]

mds.engLDA = data.frame(speakers, language, x, y)
mds.engLDA$background = rep("English", 200)
mds.engLDA$LDA        = rep("post-LDA", 200)
mds.engLDA$x   = scale(mds.engLDA$x, center = FALSE, scale = max(mds.engLDA$x, na.rm = TRUE))
mds.engLDA$y   = scale(mds.engLDA$y, center = FALSE, scale = max(mds.engLDA$y, na.rm = TRUE))