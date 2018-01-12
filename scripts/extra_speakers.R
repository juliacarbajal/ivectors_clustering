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
data.dir = "data/bilingual_test_balancedv2_tv150/"

# LABELS (SPEAKER & LANGUAGE) ####
perUtterancenames = c(paste("E_", (rep(1, 10))%o%((1:10)), ":", ((1:10))%o%(rep(1, 10)), sep = ""), paste("X_",(rep(1,10))%o%((1:10)),":",((1:10))%o%(rep(1,10)),sep=""), paste("E2_",(rep(1,10))%o%((1:2)),":",((1:10))%o%(rep(1,2)),sep=""), paste("X2_",(rep(1,10))%o%((1:2)),":",((1:10))%o%(rep(1,2)),sep=""))
speakers = gsub(":.*", "", perUtterancenames)
names(speakers) = perUtterancenames
language = gsub("_.*", "", perUtterancenames)
names(language) = perUtterancenames

# English background
data.subfolder = paste(data.dir, "english_background/", sep = "")
datafile = paste(data.subfolder, "model_ivs1_perUtterance.mat", sep = "")
m = readMat(datafile); m = m[[1]]

mextraE = readMat('data/extra_speakers/extra_english_speakers_english_background.mat')
mextraE = mextraE[[1]]
mextraX = readMat('data/extra_speakers/extra_xitsonga_speakers_english_background.mat')
mextraX = mextraX[[1]]

mextra = cbind(mextraE,mextraX)
m2 = cbind(m,mextra)

# 1.
d   = dist(t(m2))
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1] # MDS coordinate 1
y   = fit$points[, 2] # MDS coordinate 2

# 2.
mds.eng = data.frame(speakers, language, x, y)
mds.eng$background = rep("English", 240)
mds.eng$LDA        = rep("pre-LDA", 240)
mds.eng$x   = scale(mds.eng$x, center = FALSE, scale = sd(mds.eng$x)) #max(mds.eng$x, na.rm = TRUE))
mds.eng$y   = scale(mds.eng$y, center = FALSE, scale = sd(mds.eng$y)) #max(mds.eng$y, na.rm = TRUE))

# 3.
l       = lda(t(m), speakers[1:200]) #LDA performed only on the first 20 subjects
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

MDS.dataE = rbind(mds.eng, mds.engLDA)
#MDS.data$background = factor(MDS.data$background, levels = c("English", "Xitsonga", "Mixed"))
MDS.dataE$LDA        = factor(MDS.dataE$LDA, levels = c("pre-LDA", "post-LDA"))

ggplot(MDS.dataE,aes(x=x,y=y)) + geom_point(aes(color = language, shape = language), size = 2.5) + facet_grid(~LDA)+
  xlim(-2.5,2.2)+ylim(-3.5,3.2) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 18))+
  scale_shape_manual(name="Language", values=c(16,1,17,2))+
  scale_color_manual(name = "Language", values = c("black", "black", "grey48", "grey48"))


# Xitsonga background
data.subfolder = paste(data.dir, "xitsonga_background/", sep = "")
datafile = paste(data.subfolder, "model_ivs1_perUtterance.mat", sep = "")
m = readMat(datafile); m = m[[1]]

mextraE = readMat('data/extra_speakers/extra_english_speakers_xitsonga_background.mat')
mextraE = mextraE[[1]]
mextraX = readMat('data/extra_speakers/extra_xitsonga_speakers_xitsonga_background.mat')
mextraX = mextraX[[1]]

mextra = cbind(mextraE,mextraX)
m2 = cbind(m,mextra)

# 1.
d   = dist(t(m2))
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1] # MDS coordinate 1
y   = fit$points[, 2] # MDS coordinate 2

# 2.
mds.xit = data.frame(speakers, language, x, y)
mds.xit$background = rep("Xitsonga", 240)
mds.xit$LDA        = rep("pre-LDA", 240)
mds.xit$x   = scale(mds.xit$x, center = FALSE, scale = sd(mds.xit$x)) #max(mds.eng$x, na.rm = TRUE))
mds.xit$y   = scale(mds.xit$y, center = FALSE, scale = sd(mds.xit$y)) #max(mds.eng$y, na.rm = TRUE))

# 3.
l       = lda(t(m), speakers[1:200]) #LDA performed only on the first 20 subjects
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

MDS.dataX = rbind(mds.xit, mds.xitLDA)
#MDS.data$background = factor(MDS.data$background, levels = c("English", "Xitsonga", "Mixed"))
MDS.dataX$LDA        = factor(MDS.dataX$LDA, levels = c("pre-LDA", "post-LDA"))

ggplot(MDS.dataX,aes(x=x,y=y)) + geom_point(aes(color = language, shape = language), size = 2.5) + facet_grid(~LDA)+
xlim(-2.5,2.2)+ylim(-3.5,3.2) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 18))+
  scale_shape_manual(name="Language", values=c(16,1,17,2))+
  scale_color_manual(name = "Language", values = c("black", "black", "grey48", "grey48"))

# Bilingual background
data.subfolder = paste(data.dir, "bilingual_background/", sep = "")
datafile = paste(data.subfolder, "model_ivs1_perUtterance.mat", sep = "")
m = readMat(datafile); m = m[[1]]

mextraE = readMat('data/extra_speakers/extra_english_speakers_bilingual_background.mat')
mextraE = mextraE[[1]]
mextraX = readMat('data/extra_speakers/extra_xitsonga_speakers_bilingual_background.mat')
mextraX = mextraX[[1]]

mextra = cbind(mextraE,mextraX)
m2 = cbind(m,mextra)

# 1.
d   = dist(t(m2))
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1] # MDS coordinate 1
y   = fit$points[, 2] # MDS coordinate 2

# 2.
mds.bil = data.frame(speakers, language, x, y)
mds.bil$background = rep("Mixed", 240)
mds.bil$LDA        = rep("pre-LDA", 240)
mds.bil$x   = scale(mds.bil$x, center = FALSE, scale = sd(mds.bil$x)) #max(mds.eng$x, na.rm = TRUE))
mds.bil$y   = scale(mds.bil$y, center = FALSE, scale = sd(mds.bil$y)) #max(mds.eng$y, na.rm = TRUE))

# 3.
l       = lda(t(m), speakers[1:200]) #LDA performed only on the first 20 subjects
project = t(m2)%*%l$scaling
x1      = project #[,1:19,drop=F]

d   = dist(x1)
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1]
y   = fit$points[, 2]

mds.bilLDA = data.frame(speakers, language, x, y)
mds.bilLDA$background = rep("Mixed", 240)
mds.bilLDA$LDA        = rep("post-LDA", 240)
mds.bilLDA$x   = scale(mds.bilLDA$x, center = FALSE, scale = sd(mds.bilLDA$x)) #max(mds.engLDA$x, na.rm = TRUE))
mds.bilLDA$y   = scale(mds.bilLDA$y, center = FALSE, scale = sd(mds.bilLDA$y)) #max(mds.engLDA$y, na.rm = TRUE))

MDS.dataB = rbind(mds.bil, mds.bilLDA)
#MDS.data$background = factor(MDS.data$background, levels = c("English", "Xitsonga", "Mixed"))
MDS.dataB$LDA        = factor(MDS.dataB$LDA, levels = c("pre-LDA", "post-LDA"))

ggplot(MDS.dataB,aes(x=x,y=y)) + geom_point(aes(color = language, shape = language), size = 2.5) + facet_grid(~LDA) +
  xlim(-2.5,2.2)+ylim(-3.5,3.2) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 18))+
  scale_shape_manual(name="Language", values=c(16,1,17,2))+
  scale_color_manual(name = "Language", values = c("black", "black", "grey48", "grey48"))


# PLOT OF THEM ALL TOGETHER! ####
# Join everything
MDS.data = rbind(MDS.dataE,MDS.dataX,MDS.dataB)
MDS.data$background = factor(MDS.data$background, levels = c("English", "Xitsonga", "Mixed"))
MDS.data$LDA        = factor(MDS.data$LDA, levels = c("pre-LDA", "post-LDA"))

png("figures/MDS_scaleSD_extra_speakers.png", width=13*ppi, height=5*ppi, res=ppi)
ggplot(subset(MDS.data,LDA=="post-LDA"),aes(x=x,y=y)) + geom_point(aes(color = language, shape = language), size = 2.5) +
  facet_grid(~background) +
  xlim(-2.5,2.2)+ylim(-3.5,3.2) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 18))+
  scale_shape_manual(name="Language", values=c(16,1,17,2))+
  scale_color_manual(name = "Language", values = c("black", "black", "grey48", "grey48"))
dev.off()