## ----include=FALSE, message=FALSE---------------------------------------------

rm(list=ls())

library(sf)
library(dplyr)
library(igraph)
library(cartograflow)
library(cartography)


## ----flowdata_preprocess, warning=FALSE, echo=TRUE----------------------------

# Load Statistical information

data<-read.csv("./data/MOBPRO_ETP.csv",
                header=TRUE,
                sep=";",
                stringsAsFactors=FALSE,
                encoding="UTF-8",
                dec=".",
                check.names=FALSE)

data$Fij<-as.numeric(data$Fij)
tabflow<-data

## ----data_preprocess, warning=FALSE, echo=TRUE--------------------------------

# Load a list of geographic codes
CODE<-read.csv2("./data/MOBPRO_ETP.csv",
                header=TRUE,
                   sep=";",
                   stringsAsFactors=FALSE,
                   encoding="UTF-8",
                   dec=".",
                   check.names=FALSE
                )

# Load a spatial shape map background of geo codes
map<-st_read("./data/MGP_TER.shp")


## ----echo=TRUE, include=TRUE, message=FALSE, fig.show='hold', warning=FALSE----

library(igraph)
## Neighbouring graph (ordre 1)
graph_ckij_1<-flowcontig(bkg = map, code="EPT_NUM", k=1, algo = "automatic")

#ordre max = "3"


## ----echo=FALSE, include=FALSE, message=FALSE, fig.show='hold', warning=FALSE----

# Plot Neighbouring graph (ordre 1)
flowmap(tab=graph_ckij_1,
        fij="ordre",origin.f = "i",destination.f = "j",
        bkg = map,code="EPT_NUM",nodes.X="X",nodes.Y = "Y",
        filter=FALSE)

mtext("Study Area Neighbourhood Plot (k=1)", side=3)


## ----echo=TRUE, fig.show='hold', message=FALSE, warning=FALSE-----------------
library(rlang)

head(tabflow)
head(graph_ckij_1)

reduc_k1<-flowreduct(tabflow,
                  graph_ckij_1,
                  metric = "ordinal")


head(reduc_k1)

## ----echo=TRUE, fig.show='hold', message=FALSE, warning=FALSE-----------------

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


#Flowmap : flow travelled between border areas

flowmap(tab=reduc_k1,
        fij="flow",origin.f = "i",destination.f = "j",
        bkg = map,code="EPT_NUM",nodes.X="X",nodes.Y = "Y",
        filter=TRUE,
        taille=8,
        a.head = 1,
        a.length = 0.11,
        a.col="#0e7fe3",
        add=TRUE
        )

# Map Legend
legendPropLines(pos="topleft",
                title.txt="Number of commuters in adjacent places\n(k=1)",
                title.cex=0.8,    
                cex=0.5,
                values.cex= 0.7,  
                var=c(min(reduc_k1$flow),max(reduc_k1$flow)), 
                col="#0e7fe3",
                lwd=8, 
                frame = FALSE,
                values.rnd = 0
                )

# Map cosmetic
layoutLayer(title = "Professional mobility in Greater Paris between 1-neighbouring municipalities",
            author = "Cartograflow, 2020",
            sources = "Data : INSEE, 2017 ; Basemap : APUR, RIATE, 2018.",
            scale = 2,
            tabtitle = FALSE,
            frame = TRUE,
            col = "grey",
            coltitle ="black")


## ----echo=TRUE, fig.show='hold', message=FALSE, warning=FALSE-----------------

library(igraph)

## Neighbouring graph (Max(k)=3)
graph_ckij_3<-flowcontig(bkg=map, code="EPT_NUM",k=3)

# Flow reduction

head(tabflow)

reduc_k3<-flowreduct(tabflow,
                  graph_ckij_3,
                  metric = "ordinal")

head(reduc_k3)

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


#Flowmap : flow travelled between border areas

flowmap(tab=reduc_k3,
        fij="flow",origin.f = "i",destination.f = "j",
        bkg = map,code="EPT_NUM",nodes.X="X",nodes.Y = "Y",
        filter=TRUE,
        taille=8,
        a.head = 1,
        a.length = 0.11,
        a.col="#0e7fe3",
        add=TRUE 
        )

# Map Legend
legendPropLines(pos="topleft",
                title.txt="Number of commuters between non adjacent places\n(k=3)",
                title.cex=0.8,    
                cex=0.5,
                values.cex= 0.7,  
                var=c(min(reduc_k3$flow),max(reduc_k3$flow)), 
                col="#0e7fe3",
                lwd=8, 
                frame = FALSE,
                values.rnd = 0
                )


# Map cosmetic
layoutLayer(title = "Professional mobility in Greater Paris between 3-neighbouring municipalities",
            author = "Cartograflow, 2020",
            sources = "Data : INSEE, 2017 ; Basemap : APUR, RIATE, 2018.",
            scale = 2,
            tabtitle = FALSE,
            frame = TRUE,
            col = "grey",
            coltitle ="black")


## ----flows_criteria, echo=TRUE, fig.show='hold', fig.width=7, message=FALSE, warning=FALSE, ECHO=FALSE----

#Computes k=1
library(igraph)

## Neighbouring graph k=1
graph_ckij_1<-flowcontig(bkg=map, code="EPT_NUM",k=1)

# Flow reduction
reduc_k1<-flowreduct(tabflow,
                  graph_ckij_1,
                  metric = "ordinal")
head(reduc_k1)
#Extract the mean
reduc_k1_mean<-mean(reduc_k1$flow)

mean<-mean(tabflow$Fij)

# mean value of reduc_k1_mean =18591
# mean value of tabflow =13220


## ----echo=TRUE, fig.show='hold', message=FALSE, warning=FALSE-----------------

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


#Flowmap : flow travelled between border areas

flowmap(tab=reduc_k1,
        fij="flow",origin.f = "i",destination.f = "j",
        bkg = map,code="EPT_NUM",nodes.X="X",nodes.Y = "Y",
        filter=TRUE,
        threshold = 13220, #reduc_k1 ean value is 18591 ; tabflow mean value is 13220
        taille=8,
        a.head = 1,
        a.length = 0.11,
        a.col="#0e7fe3",
        add=TRUE 
        )


# Map Legend
legendPropLines(pos="topleft",
                title.txt="Number of above-average flows that occur between adjacent areas\n(k=1)",
                title.cex=0.8,    
                cex=0.5,
                values.cex= 0.7,  
                var=c(min(reduc_k1$flow),max(reduc_k1$flow)), 
                col="#0e7fe3",
                lwd=8, 
                frame = FALSE,
                values.rnd = 0
                )


# Map cosmetic
layoutLayer(title = "Professional mobility in Greater Paris between 1-neighbouring municipalities",
            author = "Cartograflow, 2020",
            sources = "Data : INSEE, 2017 ; Basemap : APUR, RIATE, 2018.",
            scale = 2,
            tabtitle = FALSE,
            frame = TRUE,
            col = "grey",
            coltitle ="black")


## ----lecho=TRUE, fig.show='hold'----------------------------------------------

sessionInfo()


