# LOAD PACKAGES ####
library(R.matlab)
library(ggplot2)
library(MASS)
library(stats)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)

ppi = 400 # Printing resolution for figures

# MAIN DIRECTORY ####
data.dir = "C:/Users/Julia/Documents/01 - Projects/01 - LSCP/04 - Modeling team stuff/ivectors/bilingual_test_balancedv2_tv150//bilingual_test_balancedv2_tv150/"

# LABELS (SPEAKER & LANGUAGE) ####
perUtterancenames = c(paste("E_", (rep(1, 10))%o%((1:10)), ":", ((1:10))%o%(rep(1, 10)), sep = ""), paste("X_",(rep(1,10))%o%((1:10)),":",((1:10))%o%(rep(1,10)),sep=""), paste("E2_",(rep(1,10))%o%((1:2)),":",((1:10))%o%(rep(1,2)),sep=""), paste("X2_",(rep(1,10))%o%((1:2)),":",((1:10))%o%(rep(1,2)),sep=""))
speakers = gsub(":.*", "", perUtterancenames)
names(speakers) = perUtterancenames
language = gsub("_.*", "", perUtterancenames)
names(language) = perUtterancenames


# MDS ####

# English background
data.subfolder = paste(data.dir, "english_background/", sep = "")
datafile = paste(data.subfolder, "model_ivs1_perUtterance.mat", sep = "")
m = readMat(datafile); m = m[[1]]

mextraE = readMat('extra_speakers/extra_english_speakers_english_background.mat')
mextraE = mextraE[[1]]
mextraX = readMat('extra_speakers/extra_xitsonga_speakers_english_background.mat')
mextraX = mextraX[[1]]

mextra = cbind(mextraE,mextraX)
m2 = cbind(m,mextra)

#Perform LDA
l       = lda(t(m), language[1:200])

# Evaluate with 4 extra speakers:
predict(l, t(mextra))$class

# Project
project = t(m2)%*%l$scaling
x1      = project #[,1:19,drop=F]

d   = dist(x1)
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1]
y   = fit$points[, 2]

mds.engLDA = data.frame(speakers, language, x, y)
mds.engLDA$background = rep("English", 240)
mds.engLDA$LDA        = rep("post-LDA", 240)
mds.engLDA$x   = scale(mds.engLDA$x, center = FALSE, scale = sd(mds.engLDA$x)) #max(mds.engLDA$x, na.rm = TRUE))
mds.engLDA$y   = scale(mds.engLDA$y, center = FALSE, scale = sd(mds.engLDA$y)) #max(mds.engLDA$y, na.rm = TRUE))

ggplot(mds.engLDA, aes(x = x, y = y))+
  geom_point(aes(color = language, shape = language), size = 2.5)

# Xitsonga background
data.subfolder = paste(data.dir, "xitsonga_background/", sep = "")
datafile = paste(data.subfolder, "model_ivs1_perUtterance.mat", sep = "")
m = readMat(datafile); m = m[[1]]

mextraE = readMat('extra_speakers/extra_english_speakers_xitsonga_background.mat')
mextraE = mextraE[[1]]
mextraX = readMat('extra_speakers/extra_xitsonga_speakers_xitsonga_background.mat')
mextraX = mextraX[[1]]

mextra = cbind(mextraE,mextraX)
m2 = cbind(m,mextra)

#Perform LDA
l       = lda(t(m), language[1:200])

# Evaluate with 4 extra speakers:
predict(l, t(mextra))$class

# Project
project = t(m2)%*%l$scaling
x1      = project #[,1:19,drop=F]

d   = dist(x1)
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1]
y   = fit$points[, 2]

mds.xitLDA = data.frame(speakers, language, x, y)
mds.xitLDA$background = rep("Xitsonga", 240)
mds.xitLDA$LDA        = rep("post-LDA", 240)
mds.xitLDA$x   = scale(mds.xitLDA$x, center = FALSE, scale = sd(mds.xitLDA$x)) #max(mds.engLDA$x, na.rm = TRUE))
mds.xitLDA$y   = scale(mds.xitLDA$y, center = FALSE, scale = sd(mds.xitLDA$y)) #max(mds.engLDA$y, na.rm = TRUE))

ggplot(mds.xitLDA, aes(x = x, y = y))+
  geom_point(aes(color = language, shape = language), size = 2.5)


# Bilingual background
data.subfolder = paste(data.dir, "bilingual_background/", sep = "")
datafile = paste(data.subfolder, "model_ivs1_perUtterance.mat", sep = "")
m = readMat(datafile); m = m[[1]]

mextraE = readMat('extra_speakers/extra_english_speakers_bilingual_background.mat')
mextraE = mextraE[[1]]
mextraX = readMat('extra_speakers/extra_xitsonga_speakers_bilingual_background.mat')
mextraX = mextraX[[1]]

mextra = cbind(mextraE,mextraX)
m2 = cbind(m,mextra)


#Perform LDA
l       = lda(t(m), language[1:200])

# Evaluate with 4 extra speakers:
predict(l, t(mextra))$class

# Project
project = t(m2)%*%l$scaling
x1      = project #[,1:19,drop=F]

d   = dist(x1)
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1]
y   = fit$points[, 2]

mds.bilLDA = data.frame(speakers, language, x, y)
mds.bilLDA$background = rep("Mixed", 240)
mds.bilLDA$LDA        = rep("post-LDA", 240)
mds.bilLDA$x = scale(mds.bilLDA$x, center = FALSE, scale = sd(mds.bilLDA$x)) #max(mds.bilLDA$x, na.rm = TRUE))
mds.bilLDA$y = scale(mds.bilLDA$y, center = FALSE, scale = sd(mds.bilLDA$y)) #max(mds.bilLDA$y, na.rm = TRUE))

ggplot(mds.bilLDA, aes(x = x, y = y))+
  geom_point(aes(color = language, shape = language), size = 2.5)
