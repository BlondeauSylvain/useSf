
######################
rm(list=ls())
library(dplyr)
library(sf)
library(cartograflow)

#library(rgeos)

map <- st_read("data MGP/MGP_TER.shp")

ordre1 <- function(bkg, code) {
      carte_sf <- st_as_sf(bkg, "sf")
      contig<-st_intersects(x = carte_sf, y = carte_sf, sparse = FALSE)
      var<-paste(carte_sf$code,sep = "")
      print(var)
      rownames(contig)<-var
      colnames(contig)<-var

      print(class(contig))
      print(contig)

      print("test pour new version")
      for (i in 1:nrow(contig)) {
        for (j in 1:ncol(contig))
        {
          if (contig[i, j] == TRUE) {
            contig[i, j] <- 1
          }
          if (contig[i, i] != 0) {
            contig[i, i] <- 0
          }
        }
      }
        tab <- flowtabmat(contig, matlist = "L")
        colnames(tab) <- c("CODE_i", "CODE_j", "cij")
        tab$CODE_i<-as.factor(tab$CODE_i)
        tab$CODE_j<-as.factor(tab$CODE_j)
        ordre_1 <- tab[tab[, "cij"] != 0, ]

}

contig_1 <- ordre1(map, "EPT_NUM")





















