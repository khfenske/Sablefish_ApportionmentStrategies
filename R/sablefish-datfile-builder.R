require(PBSmodelling)

getwd()
dir.admb.single <- file.path(wd,"admb","Single_area")
testdat <- readList("C:/Repositories/Sablefish_ApportionmentStrategies/admb/Single_area/tem_single2015.dat")
head(testdat)
names(testdat)

#note that catch is in 1000 mt units when read in
