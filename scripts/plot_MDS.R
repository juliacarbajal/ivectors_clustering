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
perUtterancenames = c(paste("E_", (rep(1, 10))%o%((1:10)), ":", ((1:10))%o%(rep(1, 10)), sep = ""), paste("X_",(rep(1,10))%o%((1:10)),":",((1:10))%o%(rep(1,10)),sep=""))
speakers = gsub(":.*", "", perUtterancenames)
names(speakers) = perUtterancenames
language = gsub("_.*", "", perUtterancenames)
names(language) = perUtterancenames
gender = c(rep("M",50),rep("F",50),rep("M",50),rep("F",50))

# MDS ####

# Bilingual background
data.subfolder = paste(data.dir, "bilingual_background/", sep = "")
datafile = paste(data.subfolder, "model_ivs1_perUtterance.mat", sep = "")
m = readMat(datafile); m = m[[1]]

# 1. Calculate distances and MDS
d   = dist(t(m))
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1] # MDS coordinate 1
y   = fit$points[, 2] # MDS coordinate 2

# 2. Organise in data frame
mds.bil = data.frame(speakers, language, gender, x, y)
mds.bil$background = rep("Mixed", 200)
mds.bil$LDA        = rep("pre-LDA", 200)
mds.bil$x          = scale(mds.bil$x, center = FALSE, scale = sd(mds.bil$x)) #max(mds.bil$x, na.rm = TRUE))
mds.bil$y          = scale(mds.bil$y, center = FALSE, scale = sd(mds.bil$y)) #max(mds.bil$y, na.rm = TRUE))

# 3. Perform LDA and repeat steps 1 & 2
l       = lda(t(m), speakers)
project = t(m)%*%l$scaling
x1      = project #[,1:19,drop=F]

d   = dist(x1)
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1]
y   = fit$points[, 2]

mds.bilLDA = data.frame(speakers, language, gender, x, y)
mds.bilLDA$background = rep("Mixed", 200)
mds.bilLDA$LDA        = rep("post-LDA", 200)
mds.bilLDA$x = scale(mds.bilLDA$x, center = FALSE, scale = sd(mds.bilLDA$x)) #max(mds.bilLDA$x, na.rm = TRUE))
mds.bilLDA$y = scale(mds.bilLDA$y, center = FALSE, scale = sd(mds.bilLDA$y)) #max(mds.bilLDA$y, na.rm = TRUE))


# Xitsonga background
data.subfolder = paste(data.dir, "xitsonga_background/", sep = "")
datafile = paste(data.subfolder, "model_ivs1_perUtterance.mat", sep = "")
m = readMat(datafile); m = m[[1]]

# 1.
d   = dist(t(m))
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1] # MDS coordinate 1
y   = fit$points[, 2] # MDS coordinate 2

# 2.
mds.xit = data.frame(speakers, language, gender, x, y)
mds.xit$background = rep("Xitsonga", 200)
mds.xit$LDA        = rep("pre-LDA", 200)
mds.xit$x = scale(mds.xit$x, center = FALSE, scale = sd(mds.xit$x)) #max(mds.xit$x, na.rm = TRUE))
mds.xit$y = scale(mds.xit$y, center = FALSE, scale = sd(mds.xit$y)) #max(mds.xit$y, na.rm = TRUE))

# 3.
l       = lda(t(m), speakers)
project = t(m)%*%l$scaling
x1      = project #[,1:19,drop=F]

d   = dist(x1)
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1]
y   = fit$points[, 2]

mds.xitLDA = data.frame(speakers, language, gender, x, y)
mds.xitLDA$background = rep("Xitsonga", 200)
mds.xitLDA$LDA        = rep("post-LDA", 200)
mds.xitLDA$x   = scale(mds.xitLDA$x, center = FALSE, scale = sd(mds.xitLDA$x)) #max(mds.xitLDA$x, na.rm = TRUE))
mds.xitLDA$y   = scale(mds.xitLDA$y, center = FALSE, scale = sd(mds.xitLDA$y)) #max(mds.xitLDA$y, na.rm = TRUE))


# English background
data.subfolder = paste(data.dir, "english_background/", sep = "")
datafile = paste(data.subfolder, "model_ivs1_perUtterance.mat", sep = "")
m = readMat(datafile); m = m[[1]]

# 1.
d   = dist(t(m))
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1] # MDS coordinate 1
y   = fit$points[, 2] # MDS coordinate 2

# 2.
mds.eng = data.frame(speakers, language, gender, x, y)
mds.eng$background = rep("English", 200)
mds.eng$LDA        = rep("pre-LDA", 200)
mds.eng$x   = scale(mds.eng$x, center = FALSE, scale = sd(mds.eng$x)) #max(mds.eng$x, na.rm = TRUE))
mds.eng$y   = scale(mds.eng$y, center = FALSE, scale = sd(mds.eng$y)) #max(mds.eng$y, na.rm = TRUE))

# 3.
l       = lda(t(m), speakers)
project = t(m)%*%l$scaling
x1      = project #[,1:19,drop=F]

d   = dist(x1)
fit = cmdscale(d, eig = T, k = 2)
x   = fit$points[, 1]
y   = fit$points[, 2]

mds.engLDA = data.frame(speakers, language, gender, x, y)
mds.engLDA$background = rep("English", 200)
mds.engLDA$LDA        = rep("post-LDA", 200)
mds.engLDA$x   = scale(mds.engLDA$x, center = FALSE, scale = sd(mds.engLDA$x)) #max(mds.engLDA$x, na.rm = TRUE))
mds.engLDA$y   = scale(mds.engLDA$y, center = FALSE, scale = sd(mds.engLDA$y)) #max(mds.engLDA$y, na.rm = TRUE))


# Join all MDS in one dataframe per LDA
MDS.data_preLDA  = rbind(mds.eng, mds.xit, mds.bil)
MDS.data_postLDA = rbind(mds.engLDA, mds.xitLDA, mds.bilLDA)

# Join everything
MDS.data = rbind(MDS.data_preLDA, MDS.data_postLDA)
MDS.data$background = factor(MDS.data$background, levels = c("English", "Xitsonga", "Mixed"))
MDS.data$LDA        = factor(MDS.data$LDA, levels = c("pre-LDA", "post-LDA"))

# PLOT ####
saveplot = 0 # Set to 1 if saving plot

if (saveplot == 1) png("figures/MDS_scaleSD.png", width=13*ppi, height=7*ppi, res=ppi)
ggplot(MDS.data, aes(x = x, y = y))+
  geom_point(aes(color = language, shape = language), size = 2.5) +
  #geom_point(aes(color = gender, shape = language), size = 2.5) + # Un-comment this line if coloring by gender
  facet_grid(LDA ~ background) +
  scale_color_manual(name = "Language", values = c("black", "grey48")) + # Comment this line if coloring by gender
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 18)) +
  scale_shape_discrete(name = "Language") +
  xlim(-2.5,2.2)+ylim(-3.5,3.2)+ #xlim(-1.5,1.5)+ylim(-1.5,1.2)+
  xlab("Coordinate 1") +
  ylab("Coordinate 2") +
  ggtitle("Background") +
  theme(plot.title = element_text(size = 18, vjust = 1)) +
  theme(axis.title.x = element_text(vjust = -0.1)) +
  theme(axis.title.y = element_text(vjust = 0.9))
if (saveplot == 1) dev.off()