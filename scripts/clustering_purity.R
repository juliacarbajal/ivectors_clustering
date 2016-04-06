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

# DEFINE FUNCTIONS ####
ClusterPurity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

# IVECTOR LABELS (SPEAKER & LANGUAGE)
perUtterancenames = c(paste("E_", (rep(1, 10))%o%((1:10)), ":", ((1:10))%o%(rep(1, 10)), sep = ""), paste("X_",(rep(1,10))%o%((1:10)),":",((1:10))%o%(rep(1,10)),sep=""))
speakers = gsub(":.*", "", perUtterancenames)
names(speakers) = perUtterancenames
language = gsub("_.*", "", perUtterancenames)
names(language) = perUtterancenames


# SELECT LINKAGE RULE ####
linkage.options = c("complete", "single", "average", "median", "centroid", "mcquitty", "ward.D", "ward.D2")

# CLUSTERING + PURITY ####
purity.data = data.frame()

for (cmethod in linkage.options) {
  clustmethod = cmethod #"mcquitty"

# English background
setwd(paste(data.dir, "english_background/", sep = "")) 
m = readMat("model_ivs1_perUtterance.mat"); m = m[[1]]

# Hierarchical clustering
hcE = hclust(dist(t(m)), method = clustmethod)
#plot(hcE,labels=speakers)

# Cut tree at different levels (k = number of clusters) and calculate their purity
groupsEeng = cutree(hcE, k = 1:200)
purityEeng = vector()
for (i in 1:200) purityEeng = c(purityEeng, ClusterPurity(groupsEeng[, i], language))

# English With LDA (same process after projecting on LDA dimensions)
l = lda(t(m), speakers)
project = t(m)%*%l$scaling
x1 = project #[, 1:5, drop = F]

hcE = hclust(dist(x1), method = clustmethod)
#plot(hcE,labels=speakers)

groupsEengLDA = cutree(hcE, k = 1:200)
purityEengLDA = vector()
for (i in 1:200) purityEengLDA = c(purityEengLDA, ClusterPurity(groupsEengLDA[, i], language))


# Xitsonga background
setwd(paste(data.dir,"xitsonga_background/", sep = ""))
m = readMat("model_ivs1_perUtterance.mat"); m = m[[1]]

hcE = hclust(dist(t(m)), method = clustmethod)
#plot(hcE,labels=speakers)

groupsExit = cutree(hcE, k = 1:200)
purityExit = vector()
for (i in 1:200) purityExit = c(purityExit, ClusterPurity(groupsExit[, i], language))

# Xitsonga With LDA
l = lda(t(m), speakers)
project = t(m)%*%l$scaling
x1 = project #[,1:5,drop=F]

hcE = hclust(dist(x1), method = clustmethod)
#plot(hcE,labels=speakers)

groupsExitLDA = cutree(hcE, k = 1:200)
purityExitLDA = vector()
for (i in 1:200) purityExitLDA = c(purityExitLDA, ClusterPurity(groupsExitLDA[, i], language))


# Bilingual background

setwd(paste(data.dir, "bilingual_background/", sep = ""))
m = readMat("model_ivs1_perUtterance.mat"); m = m[[1]]

hcE = hclust(dist(t(m)), method = clustmethod)
#plot(hcE,labels=speakers)

groupsEbil = cutree(hcE, k = 1:200)
purityEbil = vector()
for (i in 1:200) purityEbil = c(purityEbil, ClusterPurity(groupsEbil[, i], language))

# Bilingual With LDA
l = lda(t(m), speakers)
project = t(m)%*%l$scaling
x1 = project #[,1:5,drop=F]
hcE = hclust(dist(x1), method = clustmethod)
#plot(hcE,labels=speakers)

groupsEbilLDA = cutree(hcE, k = 1:200)
purityEbilLDA = vector()
for (i in 1:200) purityEbilLDA = c(purityEbilLDA, ClusterPurity(groupsEbilLDA[, i], language))


# PUT EVERYTHING INTO A DATA FRAME
Nclusters = 1:200

# Without LDA
purityE = data.frame(Nclusters, purityEeng, purityExit, purityEbil)
purityE_long = gather(purityE, background, purityNLDA, purityEeng:purityEbil)
levels(purityE_long$background)[levels(purityE_long$background) == "purityEeng"] <- "English"
levels(purityE_long$background)[levels(purityE_long$background) == "purityExit"] <- "Xitsonga"
levels(purityE_long$background)[levels(purityE_long$background) == "purityEbil"] <- "Mixed"
purityE_long$background = factor(purityE_long$background)

# With LDA
purityELDA = data.frame(Nclusters, purityEengLDA, purityExitLDA, purityEbilLDA)
purityELDA_long = gather(purityELDA, background, purityLDA, purityEengLDA:purityEbilLDA)
levels(purityELDA_long$background)[levels(purityELDA_long$background) == "purityEengLDA"] <- "English"
levels(purityELDA_long$background)[levels(purityELDA_long$background) == "purityExitLDA"] <- "Xitsonga"
levels(purityELDA_long$background)[levels(purityELDA_long$background) == "purityEbilLDA"] <- "Mixed"
purityELDA_long$background = factor(purityELDA_long$background)

# Merge with and without LDA results
puritymerge = merge(purityE_long, purityELDA_long)
purity      = gather(puritymerge, LDA, purity, c(purityNLDA, purityLDA))
levels(purity$LDA)[levels(purity$LDA) == "purityNLDA"] <- "pre-LDA"
levels(purity$LDA)[levels(purity$LDA) == "purityLDA"]  <- "post-LDA"

# PLOT PURITY CURVES
ggplot(purity, aes(x = Nclusters, y = purity)) +
  geom_line(aes(color = background, linetype = background), size = 1.5) +
  ggtitle(paste("Euclidean distance, method=", clustmethod)) +
  xlab("Number of clusters") +
  ylab("Purity\n") +
  scale_x_continuous(limits = c(0,200)) +
  facet_grid(LDA~.) +
  scale_linetype_manual(name = "Background", values = c(3, 2, 1)) +
  scale_color_manual(name = "Background", values = c("black", "grey60", "grey30")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 18))+
  theme(legend.justification = c(1, 0), legend.position = c(1, 0)) +
  guides(linetype = guide_legend(keywidth = 3, keyheight = 1),
         colour   = guide_legend(keywidth = 3, keyheight = 1)) +
  theme(axis.title.x = element_text(vjust = -0.2))

purity$linkage = rep(clustmethod,120)
purity.data = rbind(purity.data,purity)

}

purity1 = purity.data %>%
  filter(purity == 1) %>%
  group_by(background,LDA,linkage) %>%
  summarise(N = min(Nclusters)) %>%
  ungroup()

ggplot(purity1,aes(x=background,y=N))+
  geom_jitter(aes(shape=background), size = 4, position=position_jitter(width = 0.1, height = 0))+
  facet_wrap(~LDA)+
  xlab("Background") +
  ylab("Number of clusters\n") 