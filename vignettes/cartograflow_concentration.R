## ----include=FALSE, message=FALSE---------------------------------------------

rm(list=ls())

library(sf)
library(dplyr)
library(cartograflow)
library(cartography)


## ----flowdata_preprocess, warning=FALSE, echo=TRUE----------------------------

# Load Statistical information
data<-read.csv2("./data/MOBPRO_ETP.csv",
                header=TRUE,
                sep=";",
                stringsAsFactors=FALSE,
                encoding="UTF-8",
                dec=".",
                check.names=FALSE)
str(data)

# Variable typing
data$i<-as.character(data$i)
data$j<-as.character(data$j)
data$Fij<-as.numeric(data$Fij)
data$count<-as.numeric(data$count)

# Selecting useful variables
tabflow<-data%>%select(i,j,Fij)


## ----data_preprocess, warning=FALSE, echo=TRUE--------------------------------

# Load a list of geo codes
ID_CODE<-read.csv2("./data/COD_GEO_EPT.csv",
                   header=TRUE,
                   sep=";",
                   stringsAsFactors=FALSE,
                   encoding="UTF-8",
                   dec=".",
                   check.names=FALSE)
#head(ID_CODE)

CODE<-ID_CODE%>% dplyr::select(COD_GEO_EPT)

colnames(CODE)<-c("CODGEO")
#head(CODE)

map<-st_read("./data/MGP_TER.shp")


## ----analysis, fig.show='hold',message = TRUE , warning=FALSE-----------------

# Computes Gini's coefficent
# head(tabflow,3)

tabgini<-flowgini(ODpts = tabflow,
                  origin="i",destination = "j",valflow = "Fij",
                  lorenz.plot = FALSE)

# Interpretation ; The flows are quite concentrated on a few links, the Gini coefficent is equal to 73.16% 

# Plot an interactive Lorenz curve

head(tabgini)

flowgini(ODpts = tabflow,
         origin="i",destination = "j",valflow = "Fij",
         lorenz.plot = TRUE)


## ----echo=TRUE, fig.show='hold', message=TRUE, warning=FALSE------------------

# Compute critflow parameter

flowanalysis(tabgini,
             critflow = 0.8,
             result = "signif")

# Interpretation : Flow values up to 13442 are the 80% largest one corresponding to 23,14 % of the total links' features.


## ----echo=TRUE, fig.show='hold', message=TRUE, warning=FALSE------------------

# Graphic parameters
knitr::opts_chunk$set(fig.width=6, fig.height=6)
par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-150

# Overlay a spatial background 
par(bg = "NA")
#plot(st_geometry(map), col = "#67786c")

# Add the corresponding background 
plot(st_geometry(map), col = NA, border=NA, bg="#dfe6e1")
plot(st_geometry(map), col = "light grey", add=TRUE)

# For mapping flow up to 11238

flowmap(tab=tabflow,
        fij="Fij",origin.f = "i",destination.f = "j",
        bkg = map,code="EPT_NUM",nodes.X="X",nodes.Y = "Y",
        add=TRUE,
        filter=TRUE,
        threshold=13442,    
        taille=15,           
        a.head = 1,
        a.length = 0.11,
        a.angle = 30,
        a.col="#138913")


# Map Legend
legendPropLines(pos="topleft",
                title.txt="Commuters up to 13442\n (80% of the largest flows)",
                title.cex=0.8,   
                cex=0.5,
                values.cex= 0.7,  
                var=c(13442,max(tabflow$Fij)), 
                lwd=15, 
                frame = FALSE,
                col="#138913",
                values.rnd = 0
                )

#Map cosmetic

layoutLayer(title = "Significant professional mobility in Greater Paris",
           coltitle ="black",
           author = "Cartograflow, 2020",
           sources = "Data : INSEE, 2017 ; Basemap : APUR, RIATE, 2018.",
           scale = 2,
           tabtitle = FALSE,
           frame = TRUE,
           col = "grey",
            )
# north arrow
north("topright")


## ----echo=TRUE, fig.show='hold', warning=FALSE--------------------------------

flowanalysis(tabgini,
             critlink = 0.1,
             result = "density")

# Interpretation : Flows up to 45772 are the 58.12% largest one corresponding to 10 % of the total links

# Graphic parameters
knitr::opts_chunk$set(fig.width=6, fig.height=6)
par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-150

# Overlay a spatial background 
par(bg = "NA")

# Add the corresponding background 
plot(st_geometry(map), col = NA, border=NA, bg="#dfe6e1")
plot(st_geometry(map), col = "light grey", add=TRUE)


# For mapping 10% of the total features as flows up to 45772 commuters

flowmap(tab=tabgini,
        fij="Fij",origin.f = "i",destination.f = "j",
        bkg = map,code="EPT_NUM",nodes.X="X",nodes.Y = "Y",
        add=TRUE,
        plota = TRUE,
        filter=TRUE,
        threshold=45772,    
        taille=15,           
        a.head = 1,
        a.length = 0.15,
        a.angle = 30,
        a.col="#138913")

# Map Legend
legendPropLines(pos="topleft",
                title.txt="Number of commuters up to 45772\n(10% of the links)",
                title.cex=0.8,   
                cex=0.5,
                values.cex= 0.7,  
                var=c(45772,max(tabgini$Fij)), 
                lwd=15, 
                frame = FALSE,
                col="#138913",
                values.rnd = 0
                )

#Map cosmetic
layoutLayer(title = "Low density professional mobility in Greater Paris",
           coltitle ="black",
           author = "Cartograflow, 2020",
           sources = "Data : INSEE, 2017 ; Basemap : APUR, RIATE, 2018.",
           scale = 2,
           tabtitle = FALSE,
           frame = TRUE,
           col = "grey"
            )

# north arrow
north("topright")


## ----lecho=TRUE, fig.show='hold'----------------------------------------------

sessionInfo()


