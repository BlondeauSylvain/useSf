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

# Map Background
map<-st_read("./data/MGP_TER.shp")


## ----echo=TRUE, fig.show='hold'-----------------------------------------------

head(tabflow)

tab<-flowjointure(geom="area",DF.flow=tabflow,origin = "i",destination = "j",
                   bkg=map,id="EPT_NUM",x="X",y="Y")

tab.distance<-flowdist(tab,
                       dist.method = "euclidian",
                       result = "dist")

tab.distance<-tab.distance %>% select(i,j,distance)
tab<-tab %>% select(i,j,ydata)
head(tab.distance)


## ----echo=TRUE, fig.show='hold'-----------------------------------------------

#reduce the flow dataset from a selected distance travelled (eg. 8.5 km)
library(rlang)

tab.flow<-flowreduct(tab,
                     tab.distance,
                     metric = "continous",
                     d.criteria = "dmax", #max distance parameter 
                     d = 8567)        #max distance value - Q1 : 8567 km

#select for all i,j flow values up to 0
flow.d<-tab.flow %>%
        select(i,j,flowfilter) %>%
        filter(flowfilter !=0)
head(flow.d)


## ----echo=TRUE, fig.show='hold'-----------------------------------------------

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


#Flowmap : flow travelled less than 8.5 km  (as the first quartile Q1)

flowmap(tab=flow.d,
        fij="flowfilter",origin.f = "i",destination.f = "j",
        bkg = map,code="EPT_NUM",nodes.X="X",nodes.Y = "Y",
        filter=TRUE,
        taille=8,           
        a.head = 1,
        a.length = 0.11,
        a.col="#f7714f",
        add=TRUE)

#Map legend
legendPropLines(pos="topleft",
                title.txt="Number of commuters\n(distance travelled less than 8,5 km)",
                title.cex=0.8,    
                cex=0.5,
                values.cex= 0.7,  
                var=c(min(flow.d$flowfilter),8567), 
                col="#f7714f",
                lwd=8,
                frame = FALSE,
                values.rnd = 0
                )


#Map cosmetic
layoutLayer(title = "Professional mobility in Greater Paris : short distance travelled",
            author = "Cartograflow, 2020",
            sources = "Data : INSEE, 2017 ; Basemap : APUR, RIATE, 2018.",
            scale = 2,
            tabtitle = FALSE,
            frame = TRUE,
            col = "grey",
            coltitle ="black"
            )

# north arrow
north("topright")


## ----echo=FALSE, fig.show='hold', message=FALSE, warning=FALSE----------------

#head(tabflow)

tab<-flowjointure(geom="area",DF.flow=tabflow,origin = "i",destination = "j",
                   bkg=map,id="EPT_NUM",x="X",y="Y")

tab.distance<-flowdist(tab,
                       dist.method = "euclidian",
                       result = "dist")

tab.distance<-tab.distance %>% select(i,j,distance)
tab<-tab %>% select(i,j,ydata)
head(tab.distance)

tab.flow<-flowreduct(tab,
                     tab.distance,
                     metric = "continous",
                     d.criteria = "dmin",  
                     d = 19234)        #Q2 : 14518 km - Q3:19234 km

#select for all i,j flow values above to 0
flow.d<-tab.flow%>%
       select(i,j,flowfilter)%>%
        filter(flowfilter !=0)

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


#Flowmap : flow travelled up to 20 km (as the third quartile Q3)


par(mar=c(0,0,1,0))

extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-150

flowmap(tab=flow.d,
        fij="flowfilter",origin.f = "i",destination.f = "j",
        bkg = map,code="EPT_NUM",nodes.X="X",nodes.Y = "Y",
        filter=TRUE,
        taille=8,           
        a.head = 1,
        a.length = 0.11,
        a.col="#f7714f",
        add=TRUE)

# Map legend
legendPropLines(pos="topleft",
                title.txt="Number of commuters\n(distance travelled more than 19 km)",
                title.cex=0.8,    
                cex=0.5,
                values.cex= 0.7,  
                var=c(19234, max(flow.d$flowfilter)), 
                col="#f7714f",
                lwd=8, 
                frame = FALSE,
                values.rnd = 0
                )
# Map cosmetics
layoutLayer(title = "Professional mobility in Greater Paris : long distance travelled",
            author = "Cartograflow, 2020",
            sources = "Data : INSEE, 2017 ; Basemap : APUR, RIATE, 2018.",
            scale = 2,
            tabtitle = FALSE,
            frame = TRUE,
            col = "grey",
            coltitle ="black")

# north arrow
north("topright")


## ----echo=FALSE, fig.show='hold', message=FALSE, warning=FALSE----------------

#head(tabflow)

tab<-flowjointure(geom="area",DF.flow=tabflow,origin = "i",destination = "j",
                   bkg=map,id="EPT_NUM",x="X",y="Y")

tab.distance<-flowdist(tab,
                       dist.method = "euclidian",
                       result = "dist")

tab.distance<-tab.distance %>% select(i,j,distance)
tab<-tab %>% select(i,j,ydata)
head(tab.distance)

tab.flow<-flowreduct(tab,
                     tab.distance,
                     metric = "continous",
                     d.criteria = "dmax",  
                     d = 19234)       #Q3:19234 km

#select for all i,j flow values up to 8567  
flow.d<-tab.flow%>%
       select(i,j,flowfilter)%>%
        filter(flowfilter >8567)      #Q1=8567km


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


#Flowmap : flow travelled up to (Q3)

flowmap(tab=flow.d,
        fij="flowfilter",origin.f = "i",destination.f = "j",
        bkg = map,code="EPT_NUM",nodes.X="X",nodes.Y = "Y",
        filter=TRUE,
        taille=8,           
        a.head = 1,
        a.length = 0.11,
        a.col="#f7714f",
        add=TRUE)

# Map legend
legendPropLines(pos="topleft",
                title.txt="Number of commuters\n(distance travelled between 8,5 and 19 km)",
                title.cex=0.8,    
                cex=0.5,
                values.cex= 0.7,  
                var=c(8567, max(flow.d$flowfilter)), 
                col="#f7714f",
                lwd=8, 
                frame = FALSE,
                values.rnd = 0
                )

# Map cosmetic
layoutLayer(title = "Professional mobility in Greater Paris : median distance travelled",
            author = "Cartograflow, 2020",
            sources = "Data : INSEE, 2017 ; Basemap : APUR, RIATE, 2018.",
            scale = 5,
            tabtitle = TRUE,
            frame = TRUE,
            col = "grey",
            coltitle ="black")


## ----lecho=TRUE, fig.show='hold'----------------------------------------------

sessionInfo()


