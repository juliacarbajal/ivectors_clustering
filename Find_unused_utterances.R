library(dplyr)
library(tidyr)

setwd("C:/Users/Julia/Documents/01 - Projects/01 - LSCP/04 - Modeling team stuff/ivectors/clustering")

# LOAD ENGLISH DATA NAMES ####

# All TIMIT files:
filenames = list.files("G:/TIMIT_mfcc/TIMIT13", pattern="*.htk",full.names=FALSE)
filenames = as.data.frame(filenames)

# Files used:
# English training:
english_training = read.table("English_training_list.txt",header=F)
colnames(english_training) = "filenames"

# Bilingual training:
bilingual_training_e = read.table("Bilingual_training_english_list.txt",header=F)
colnames(bilingual_training_e) = "filenames"

# English test:
english_test = read.table("English_test_list.txt",header=F)
colnames(english_test) = "filenames"

# Discard from the TIMIT list all files that were used in training and test:
unused_english = anti_join(filenames,english_training)
unused_english2 = anti_join(unused_english,english_test)
unused_english3 = anti_join(unused_english2,bilingual_training_e) #Note: all the ones used in the bilingual are also used in the english_training

# unused_english3 is a list containing all the files that were not used so far (I can choose more data from these to test).


# LOAD XITSONGA DATA NAMES ####

# All XIT files:
filenamesX1 = list.files("G:/Xitsonga_mfcc", pattern="*.mfc",full.names=FALSE)
filenamesX1 = as.data.frame(filenamesX1)
colnames(filenamesX1) = "filenames"
filenamesX2 = list.files("G:/Xitsonga2", pattern="*.mfc",full.names=FALSE)
filenamesX2 = as.data.frame(filenamesX2)
colnames(filenamesX2) = "filenames"
filenamesX3 = list.files("G:/Xitsonga3", pattern="*.mfc",full.names=FALSE)
filenamesX3 = as.data.frame(filenamesX3)
colnames(filenamesX3) = "filenames"
filenamesX = rbind(rbind(filenamesX1,filenamesX2),filenamesX3) %>%
  separate(filenames, into = c("database1","database2","subject","file"))

# Files used:
# Xitsonga training:
xitsonga_training = read.table("Xitsonga_training_list.txt",header=F)
colnames(xitsonga_training) = "filenames"
xitsonga_training = xitsonga_training %>%
  separate(filenames, into = c("database1","database2","subject","file"))

# Bilingual training:
bilingual_training_x = read.table("Bilingual_training_xitsonga_list.txt",header=F)
colnames(bilingual_training_x) = "filenames"
bilingual_training_x = bilingual_training_x %>%
  separate(filenames, into = c("database1","database2","subject","file"))

# Xitsonga test:
xitsonga_test = read.table("Xitsonga_test_list.txt",header=F)
colnames(xitsonga_test) = "filenames"
xitsonga_test = xitsonga_test %>%
  separate(filenames, into = c("database1","database2","subject","file"))

# Discard from the TIMIT list all files that were used in training and test:
unused_xitsonga = anti_join(filenamesX,xitsonga_training,by="subject")
unused_xitsonga2 = anti_join(unused_xitsonga,xitsonga_test,by="subject")
unused_xitsonga3 = anti_join(unused_xitsonga2,bilingual_training_x,by="subject") 
