install.packages("rattle")
install.packages("RGtk2")
library(rattle)
library(RGtk2)
rattle()
#reading dataset into csv
library(readxl)
install.packages("readxl")
write.csv(iris,"irisdataset.csv")
