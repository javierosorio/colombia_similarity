# HEADER ##################################################################################################
# 
# Are Many Sets of Eyes Better Than One?
# Evaluating Multiple Databases of Armed Actors in Colombia
# Javier Osorio
# International Interactions
# 1/12/2023




# CONVENTIONS ##################################################################################################
#    IN = INDEPAZ
#    RC = Rutas del Conflicto
#    CL = Claudia Lopez
#    CD = CEDE
#    UC = UCDP
#    RE = Restrepo
#    VI = ViPAA 




# SCRIPT STRUCTURE ##################################################################################################
#
# 0. SETUP
# 1. SPIDER WEB PLOT
# 2. GET THE DATA
# 3. ASSESS COVERAGE
# 4. EXPLORE MISSIGNESS 
# 5. EXPLORE JACARD AND SORENSEN
# 6. JACCARD INDIEX OVER YEARS FOR GUERRILLA AND PARAMILITARIES
# 7. VISUALIZE JACCARD OVER YEARS FOR GUERRILLA AND PARAMILITARIES
# 8. AVERAGE JACCARD SIMILARITY GUERRILLA AND PARAMILITARIES 
# 10. LOCAL JACCARD SIMILARITY PER ARMED GROUP 
# 11. AVERAGE JACCARD SIMILARITY PER ARMED GROUP OVER TIME
# 12. STATISTICAL ANALYSIS OF HOMICIDES 
# 13. REPLICATE DUBE AND VARGAS WITH DIFFERENT MEASURES 
# 14. JACCARD FOR EACH PAIR OF MEASUREMENT SETS  







# 0. SETUP ##################################################################################################

# Clean the environment
rm(list = ls())

# Setup working directory (if needed)
#setwd("C:/Users/javie/Dropbox/Colombia_Similarity")
#setwd("E:/Dropbox/Colombia_Similarity")


# Install devtools if needed
# library(devtools)
#devtools::install_github("tidyverse/tidyverse")

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(foreign,plm,ggplot2,tidyverse,adiv,stargazer,readxl,tidyr,plyr,dplyr,janitor,plotrix,cowplot,udpipe,magicfor,RColorBrewer, cowplot,viridis,reshape)






# 1. SPIDER WEB PLOT ########################################################################################

  
# Replicate Figure 1. Measurement Set Characteristics
  
# Library
library(fmsb)
library(ggplot2)


## Generate spider plot ##################################



# Function to create nice spyder web graphs
create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 0,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}



# Create individual data frames per characteristic
Max<-c(5,5,5,5,5,5,5)
Min<-c(1,1,1,1,1,1,1)
Var<-c(1,2,2,1,2,1,2)
News<-c(4,3,4,0,4,4,1)
NGOs<-c(4,3,5,5,5,5,1)
Gov<-c(2,1,1,3,2,1,3)
Test<-c(1,1,2,2,2,2,1)
IQA<-c(1,3,2,1,3,1,1)
Rep<-c(1,1,1,1,2,1,1)

# Integrate data components
spider.data<- cbind(Var,News,NGOs,Gov,Test,IQA,Rep)
spider.data<- as.data.frame(spider.data)
spider.data<- rbind(Max,Min,spider.data)

# Assign labels
rownames(spider.data) <- c("Max","Min","IN" , "UC" , "RE" , "CL" , "VI", "RC","CD" )

# Gen individual spider data
spider.in <- spider.data[c("Max", "Min", "IN"),]
spider.uc <- spider.data[c("Max", "Min", "UC"),]
spider.re <- spider.data[c("Max", "Min", "RE"),]
spider.cl <- spider.data[c("Max", "Min", "CL"),]
spider.vi <- spider.data[c("Max", "Min", "VI"),]
spider.rc <- spider.data[c("Max", "Min", "RC"),]
spider.cd <- spider.data[c("Max", "Min", "CD"),]


# Plot individual spider web graphs

pdf("./graphs/spider/spider_in.pdf", width=2.5, height=2.2)
par(mar=c(.1,.1,.1,.1))
radarchart(spider.in, plwd=3 , plty=1, pcol="#5d5e61", pfcol=scales::alpha("#8e9299", 0.5))
dev.off() 

pdf("./graphs/spider/spider_uc.pdf", width=2.5, height=2.2)
par(mar=c(.1,.1,.1,.1))
radarchart(spider.uc, plwd=3 , plty=1, pcol="#5d5e61", pfcol=scales::alpha("#8e9299", 0.5))
dev.off() 

pdf("./graphs/spider/spider_re.pdf", width=2.5, height=2.2)
par(mar=c(.1,.1,.1,.1))
radarchart(spider.re, plwd=3 , plty=1, pcol="#5d5e61", pfcol=scales::alpha("#8e9299", 0.5))
dev.off() 

pdf("./graphs/spider/spider_cl.pdf", width=2.5, height=2.2)
par(mar=c(.1,.1,.1,.1))
radarchart(spider.cl, plwd=3 , plty=1, pcol="#5d5e61", pfcol=scales::alpha("#8e9299", 0.5))
dev.off() 

pdf("./graphs/spider/spider_vi.pdf", width=2.5, height=2.2)
par(mar=c(.1,.1,.1,.1))
radarchart(spider.vi, plwd=3 , plty=1, pcol="#5d5e61", pfcol=scales::alpha("#8e9299", 0.5))
dev.off() 

pdf("./graphs/spider/spider_rc.pdf", width=2.5, height=2.2)
par(mar=c(.1,.1,.1,.1))
radarchart(spider.rc, plwd=3 , plty=1, pcol="#5d5e61", pfcol=scales::alpha("#8e9299", 0.5))
dev.off() 

pdf("./graphs/spider/spider_cd.pdf", width=2.5, height=2.2)
par(mar=c(.1,.1,.1,.1))
radarchart(spider.cd, plwd=3 , plty=1, pcol="#5d5e61", pfcol=scales::alpha("#8e9299", 0.5))
dev.off() 
#




# Restore par mar
old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)






# 2. GET THE DATA ###########################################################################################

  
  
  ## GET INDIVIDUAL DATA BASES ############################

  # Get raw data 
  CDall <- read.csv(file = "./data_final/CDall.csv")
  CLall <- read.csv(file = "./data_final/CLall.csv")
  INall <- read.csv(file = "./data_final/INall.csv")
  RCall <- read.csv(file = "./data_final/RCall.csv")
  VIall <- read.csv(file = "./data_final/VIall.csv")
  REall <- read.csv(file = "./data_final/REall.csv")
  UCall <- read.csv(file = "./data_final/UCall.csv")
  
  
  # Generate subset of Guerrilla and Paramilitaries
  CDall.GP <- subset(CDall, select=c("mun", "year", "Paramilitaries", "Guerrilla"))
  CLall.GP <- subset(CLall, select=c("mun", "year", "Paramilitaries", "Guerrilla"))
  INall.GP <- subset(INall, select=c("mun", "year", "Paramilitaries"))
  RCall.GP <- subset(RCall, select=c("mun", "year", "Paramilitaries", "Guerrilla"))
  VIall.GP <- subset(VIall, select=c("mun", "year", "Paramilitaries", "Guerrilla"))
  REall.GP <- subset(REall, select=c("mun", "year", "Paramilitaries", "Guerrilla"))
  UCall.GP <- subset(UCall, select=c("mun", "year", "Paramilitaries", "Guerrilla"))
  
  
  # Rename variables per each database
  names(CDall.GP)[3]<-"Paramilitaries_CD"
  names(CDall.GP)[4]<-"Guerrilla_CD"
  names(CLall.GP)[3]<-"Paramilitaries_CL"
  names(CLall.GP)[4]<-"Guerrilla_CL"
  names(INall.GP)[3]<-"Paramilitaries_IN"
  names(RCall.GP)[3]<-"Paramilitaries_RC"
  names(RCall.GP)[4]<-"Guerrilla_RC"
  names(VIall.GP)[3]<-"Paramilitaries_VI"
  names(VIall.GP)[4]<-"Guerrilla_VI"
  names(REall.GP)[3]<-"Paramilitaries_RE"
  names(REall.GP)[4]<-"Guerrilla_RE"
  names(UCall.GP)[3]<-"Paramilitaries_UC"
  names(UCall.GP)[4]<-"Guerrilla_UC"  
  
  
  ## Check data dimensions 
  table(VIall.GP$year)
  table(CDall.GP$year)
  table(CLall.GP$year)
  table(INall.GP$year)
  table(RCall.GP$year)
  table(REall.GP$year)
  table(UCall.GP$year)
  

  ## Eliminate duplicates by getting unique rows 
  VIall.GP <- unique(VIall.GP)
  CDall.GP <- unique(CDall.GP)
  CLall.GP <- unique(CLall.GP)
  INall.GP <- unique(INall.GP)
  RCall.GP <- unique(RCall.GP)
  REall.GP <- unique(REall.GP)
  UCall.GP <- unique(UCall.GP)
  
  VIall.GP <- VIall.GP %>% distinct(mun, year, .keep_all = TRUE)
  CDall.GP <- CDall.GP %>% distinct(mun, year, .keep_all = TRUE)
  CLall.GP <- CLall.GP %>% distinct(mun, year, .keep_all = TRUE)
  INall.GP <- INall.GP %>% distinct(mun, year, .keep_all = TRUE)
  RCall.GP <- RCall.GP %>% distinct(mun, year, .keep_all = TRUE)
  REall.GP <- REall.GP %>% distinct(mun, year, .keep_all = TRUE)
  UCall.GP <- UCall.GP %>% distinct(mun, year, .keep_all = TRUE)
  
  
  
  

  ## MERGE DATABASES ############################
  
  
  #Get mun year template
  munyear_temp <- read_csv("./data_final/Col_data.csv")
  munyear_temp <- munyear_temp %>% select(mun,year)
  table(munyear_temp$year)
  dim(munyear_temp)
  

  # merge data with themplate
  ALL.GP <- merge(munyear_temp,VIall.GP,by=c("mun", "year"),all.x = TRUE)
  ALL.GP <- merge(ALL.GP,  CDall.GP,by=c("mun", "year"),all.x = TRUE)
  ALL.GP <- merge(ALL.GP,  CLall.GP,by=c("mun", "year"),all.x = TRUE)
  ALL.GP <- merge(ALL.GP,  INall.GP,by=c("mun", "year"),all.x = TRUE)
  ALL.GP <- merge(ALL.GP,  RCall.GP,by=c("mun", "year"),all.x = TRUE)
  ALL.GP <- merge(ALL.GP,  REall.GP,by=c("mun", "year"),all.x = TRUE)
  ALL.GP <- merge(ALL.GP,  UCall.GP,by=c("mun", "year"),all.x = TRUE)
  
  # Order by mun year
  ALL.GP <-ALL.GP[with(ALL.GP, order(mun, year)),]
  
  
  table(ALL.GP$year)
  
 
  



# 3. ASSESS TEMPORAL COVERAGE   #############################################################################

  ## Replicate Figure 2a 
  
  
  # Generate coverage data frame
  coverage<-ALL.GP
  
  # Gen index per per metric 
  coverage$VI <- rowSums(coverage[,c("Paramilitaries_VI", "Guerrilla_VI")], na.rm=TRUE)
  coverage$CD <- rowSums(coverage[,c("Paramilitaries_CD", "Guerrilla_CD")], na.rm=TRUE)
  coverage$CL <- rowSums(coverage[,c("Paramilitaries_CL", "Guerrilla_CL")], na.rm=TRUE)
  coverage$IN<-ALL.GP$Paramilitaries_IN
  coverage$RC <- rowSums(coverage[,c("Paramilitaries_RC", "Guerrilla_RC")], na.rm=TRUE)
  coverage$RE <- rowSums(coverage[,c("Paramilitaries_RE", "Guerrilla_RE")], na.rm=TRUE)
  coverage$UC <- rowSums(coverage[,c("Paramilitaries_UC", "Guerrilla_UC")], na.rm=TRUE)
  
  
  
  # keep index
  coverage<-subset(coverage, select=c(year, VI, CD, CL, IN, RC, RE, UC))
  
  # Aggregate data
  coverage <-aggregate(coverage, by=list(coverage$year), FUN=mean, na.rm=TRUE)
  
  # Replace NAs with 0
  coverage[is.na(coverage)] = 0
  
  # Replace >0 with 1
  coverage$VI[coverage$VI>0]<-1
  coverage$CD[coverage$CD>0]<-1
  coverage$CL[coverage$CL>0]<-1
  coverage$IN[coverage$IN>0]<-1
  coverage$RC[coverage$RC>0]<-1
  coverage$RE[coverage$RE>0]<-1
  coverage$UC[coverage$UC>0]<-1
  
  # Eliminate extra variable
  coverage<-subset(coverage, select=-c(Group.1))
  
  
  # Convert wide to long
  coverage.long <- gather(coverage, measurement, covered, VI, CD, CL, IN, RC, RE, UC,  factor_key=TRUE)
  
  # Replace 0/1 to N/Y
  
  coverage.long$coverage[coverage.long$covered==0]<-"No"
  coverage.long$coverage[coverage.long$covered==1]<-"Yes"
  
  
  # Convert variables as factors
  coverage.long$year<-as.factor(coverage.long$year)
  coverage.long$measurement<-as.factor(coverage.long$measurement)
  
  
  # Plot coverage
  g.cover<- ggplot(data = coverage.long, aes(x=measurement, y=year, fill=coverage)) + 
    scale_fill_manual(values = c("gray", "black"))+
    geom_tile(color = "white")+
    theme(text = element_text(size = 9),legend.text = element_text(size = 10))+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  
  g.cover 
  
  
  pdf("./graphs/coverage/coverage_2.pdf", width=3, height=3)
  g.cover 
  dev.off()
  
  png("./graphs/coverage/coverage_2.png")
  g.cover 
  dev.off()
  





# 4. EXPLORE MISSIGNESS   ###################################################################################

  
  ## Explore missing data by measurement set and actor  #############################
  
  ## Replicate Figure 2B
  
  ## Examples of missing data visualizations
  ## https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
  ## https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html
  
  
  
  # Eliminate mun and year
  ALL.GP.nomy <-subset(ALL.GP, select=-c(mun, year))
  
  
  # Calculate the Percent of missing values
  
  # Generate missing values
  missing.values <- ALL.GP.nomy %>%
    gather(key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    group_by(key) %>%
    #mutate(total = n()) %>%
    dplyr::mutate(total = n()) %>%
    group_by(key, total, isna) %>%
    #summarise(num.isna = n()) %>%
    dplyr::summarise(num.isna = n()) %>%
    mutate(pct = num.isna / total * 100)
  
  # Generate levels
  levels <-
    (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key
  
  # Generate plot
  percentage.plot <- missing.values %>%
    ggplot() +
    geom_bar(aes(x = reorder(key, desc(pct)), 
                 y = pct, fill=isna), 
             stat = 'identity', alpha=0.8) +
    scale_x_discrete(limits = levels) +
    scale_fill_manual(name = "", 
                      values = c('black', 'gray'), labels = c("Recorded", "Missing")) +
    coord_flip() +
    labs(x = "", y = "% of missing values") + 
    theme_classic()+theme(legend.position="bottom", 
                          legend.text = element_text(size=8),
                          text = element_text(size = 9))
  
  #labs(title = "Percentage of missing values", x ='Variable', y = "% of missing values")
  
  percentage.plot 
  
  
  pdf("./graphs/missing/missing_2.pdf", width=3, height=3)
  percentage.plot 
  dev.off()
  
  png("./graphs/missing/missing_2.png")
  percentage.plot 
  dev.off()  
  
  
  
  ## Explore missing data of Guerrilla and Paramilitaries  #############################
  

  # Guerrilla

  # Generate a Guerrilla dataframe
  Guerrilla <- data.frame(
    CD = (ALL.GP$Guerrilla_CD),
    CL = (ALL.GP$Guerrilla_CL),
    RC = (ALL.GP$Guerrilla_RC),
    VI = (ALL.GP$Guerrilla_VI),
    RE = (ALL.GP$Guerrilla_RE),
    UC = (ALL.GP$Guerrilla_UC))
  
  # Check missing
  summary(Guerrilla)
  
  # Loop to check missing data
  for(i in colnames(Guerrilla)){ 
    # Count the number of rows
    n<-nrow(Guerrilla)
    # Count the number of missing data per variable
    md<-sum(is.na(Guerrilla[,i])) 
    # Concatenate the information in a single line  
    cat(i , ":  Missing =", md, "    % missing =", (md/n)*100 ,"\n")
  }
  
  
  # Generate two versions of the database
  GuerrillaAll.z<-Guerrilla    # Data with zeros
  GuerrillaAll.dna<-Guerrilla  # Data dropping missing data
  
  # Fill in NAs with 0  
  GuerrillaAll.z[is.na(GuerrillaAll.z)] <- 0      # This is not a good assumption
  dim(GuerrillaAll.z)
  
  # Drop NAs
  GuerrillaAll.dna <- na.omit(GuerrillaAll.dna) 
  dim(GuerrillaAll.dna)
  

  # Generate smaller database
  
  # Keep only CD and VI
  Guerrilla.CDVI <- subset(Guerrilla, select = -c(CL, RC))
  # CL has 70% missing data
  # RC has 80 missing data
  
  # Data dropping missing data
  Guerrilla.CDVI.dna<-Guerrilla.CDVI
  Guerrilla.CDVI.dna <- na.omit(Guerrilla.CDVI.dna) 
  
  
  
  
  

  # Paramilitaries

  
  # Generate a Paramilitaries dataframe
  Paras <- data.frame(
    CD = (ALL.GP$Paramilitaries_CD),
    CL = (ALL.GP$Paramilitaries_CL),
    RC = (ALL.GP$Paramilitaries_RC),
    IN = (ALL.GP$Paramilitaries_IN),
    VI = (ALL.GP$Paramilitaries_VI),
    RE = (ALL.GP$Paramilitaries_RE),
    UC = (ALL.GP$Paramilitaries_UC))
  
  

  # Check missing
  summary(Paras)
  
  # Loop to check missing data
  for(i in colnames(Paras)){ 
    # Count the number of rows
    n<-nrow(Paras)
    # Count the number of missing data per variable
    md<-sum(is.na(Paras[,i])) 
    # Concatenate the information in a single line  
    cat(i , ":  Missing =", md, "    % missing =", (md/n)*100 ,"\n")
  }
  
  
  # Generate two versions of the database
  ParasAll.z<-Paras    # Data with zeros
  ParasAll.dna<-Paras  # Data dropping missing data
  
  # Fill in NAs with 0  (this is problematic, but sim scores cannot run with NAs)
  ParasAll.z[is.na(ParasAll.z)] <- 0
  
  # Eliminate NAs
  ParasAll.dna <- na.omit(ParasAll.dna) 
  
  
  
  # Generate smaller database
  
  # Keep only CD and VI
  Paras.CDVIRC <- subset(Paras, select = -c(CL, IN))
  # CL has 70% missing data
  # IN has 73% missing data
  
  # Eliminate NAs
  Paras.CDVIRC.dna<-Paras.CDVIRC
  Paras.CDVIRC.dna <- na.omit(Paras.CDVIRC.dna) 
  
  # Keep only CD and VI
  Paras.CDVI <- subset(Paras, select = -c(CL, RC, IN))
  # CL has 70% missing data
  # IN has 73% missing data
  # RC has 40% missing data
  
  # Eliminate NAs
  Paras.CDVI.dna<-Paras.CDVI
  Paras.CDVI.dna <- na.omit(Paras.CDVI.dna) 
  
  
  

  
  


# 5. EXPLORE JACARD AND SORENSEN   ##########################################################################


  ## Guerrilla and Paramilitaries for dropped NAs
  
  ### Jaccard for dropped NAs
  betastatjac(GuerrillaAll.dna)
  #betastatjac(ParasAll.dna)     # 0 observations
  
  ### Sorensen for dropped NAs
  betastatsor(GuerrillaAll.dna)
  #betastatsor(Paras.dna)        # 0 observations
  
  
  
  

  ## Guerrilla and Paramilitaries Reduced version for dropped NAs
  
  ### Jaccard for dropped NAs
  betastatjac(Guerrilla.CDVI.dna)
  betastatjac(Paras.CDVIRC.dna)
  betastatjac(Paras.CDVI.dna)
  
  
  
  ### Sorensen for dropped NAs
  betastatsor(Guerrilla.CDVI.dna)
  betastatsor(Paras.CDVIRC.dna)
  betastatsor(Paras.CDVI.dna)
  
  




# 6. JACCARD INDIEX OVER YEARS FOR GUERRILLA AND PARAMILITARIES #############################################

  
## Generate main data frames  ######################################### 
  
  
  # Eliminate mun and year
  ALL.GP.nom <-subset(ALL.GP, select=-c(mun))
  ALL.GP.nom$year<-as.factor(ALL.GP.nom$year)
  
  # Create a sequence of years
  seq.y <- seq(1988, 2017, by=1)
  seq.y <- as.factor(seq.y)
  
   
  # Generate databases 
  
  ## Generate a Guerrilla dataframe
  Guerrilla <- data.frame(
    year = (ALL.GP$year),
    CD = (ALL.GP$Guerrilla_CD),
    CL = (ALL.GP$Guerrilla_CL),
    RC = (ALL.GP$Guerrilla_RC),
    VI = (ALL.GP$Guerrilla_VI),
    RE = (ALL.GP$Guerrilla_RE),
    UC = (ALL.GP$Guerrilla_UC))
  
  ## Generate a Paras dataframe
  Paras <- data.frame(
    year = (ALL.GP$year),
    CD = (ALL.GP$Paramilitaries_CD),
    CL = (ALL.GP$Paramilitaries_CL),
    RC = (ALL.GP$Paramilitaries_RC),
    VI = (ALL.GP$Paramilitaries_VI),
    IN = (ALL.GP$Paramilitaries_IN),
    RE = (ALL.GP$Paramilitaries_RE),
    UC = (ALL.GP$Paramilitaries_UC))
  
  
  

## Loop Guerrilla Jaccard index over time   ######################################### 
  
for (i in seq.y) {
  
  # seq.y
  # i <- 1993
  
  
  # CONTENT
  # A) Remove all the little components
  # B) Generate subset by year
  # C) Eliminate rows with NAs
  # D) Reduce dimentionality of rows with NAs
  # E) Calculate Jaccard index
  # F) Calculate weights
  # G) Generate output  
  

  # A) Remove all the little components
  
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  if(exists("temp.data")==TRUE){rm(temp.data)}else{NA}
  if(exists("sub.name")==TRUE){rm(sub.name)}else{NA}
  rm(list=ls(pattern="sub.ls"))
  rm(list=ls(pattern="dataloop.b.sub."))
  rm(list=ls(pattern="^jaccard."))
  if(exists("jack.b")==TRUE){rm(jack.b)}else{NA}
  rm(list=ls(pattern="^weight.b."))
  rm(list=ls(pattern="^wgt.sub."))
  rm(list=ls(pattern="^w."))
  rm(list=ls(pattern="^temp.w."))
  rm(list=ls(pattern="^numrow."))
  
  

  # B) Generate subset by year
  dataloop <- subset(Guerrilla, year==i)
  # Eliminate columns with all NAs
  dataloop <- Filter(function(dataloop)!all(is.na(dataloop)), dataloop)
  dim(dataloop)
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  # Extract variables per year
  V <- names(dataloop)
  # Generate a "V" object per year containing variable names
  assign(paste0("VG_", i), V)
  # Get n rows
  numrow <-nrow(dataloop)
  numcol <-ncol(dataloop)
  
  

  # C) Eliminate rows with NAs
  
  dataloop.a <- NA
  dataloop.a <- na.omit(dataloop) # <<<< eliminates row if there is a NA
  dim(dataloop.a)
  # Count number of columns and rows
  numcol.a <-ncol(dataloop.a)
  numrow.a <-nrow(dataloop.a)
  numrow.a
  
  

  # D) Reduce dimentionality of rows with NAs
  
  dataloop.b <- NA
  dataloop.b <- dataloop[rowSums(is.na(dataloop)) > 0, ]     # <<<< Keep all rows with at least one NA
  dim(dataloop.b)
  # Count number of columns and rows
  numrow.b <-nrow(dataloop.b)
  numrow.b
  
  # Unite database as character
  dataloop.b[] <- lapply(dataloop.b, as.character)
  dataloop.b <- dataloop.b %>% unite("split", sep = "_", na.rm = TRUE)
  dim(dataloop.b)
  
  # Generate groups object
  dataloop.b$lenght <- nchar(dataloop.b$split)            # Get length
  dataloop.b<- subset(dataloop.b, lenght>=3)              # Subset larger than one annotation
  
  # Subset data by groups by groups into a list object
  for(x in unique(dataloop.b$lenght)){
    # Subset data
    assign(paste0("dataloop.b.sub.",x),subset(dataloop.b, lenght==x, select="split")) 
  }
  
  # Get list of dataloop.b.# data frames
  sub.data <- mget(ls(pattern = "dataloop.b.sub.*"))
  sub.data
  
  # Create function to extract last element from string
  substrRight <- function(x, n){
    sapply(x, function(xx)
      substr(xx, (nchar(xx)-n+1), nchar(xx)))}
  
  
  # If sub.data is not empty
  if(length(sub.data)>0){
    # Extract data from list into data frame
    for(n in 1:length(sub.data)){
      # Extract last part of object name
      sub.name <- substrRight(c(names(sub.data)[n]),5)
      # Create data frame
      temp.data <- as.data.frame(sub.data[[n]])
      # Split columns by _
      temp.data <- as.data.frame(do.call('rbind', strsplit(as.character(temp.data$split),'_')))
      # Convert to data frame and numeric
      temp.data <- data.frame(temp.data)
      temp.data <- temp.data %>% mutate_all(as.numeric) %>% as.data.frame()
      # Get row and column names
      assign(paste0("numcol.",sub.name), ncol(temp.data))
      assign(paste0("numrow.",sub.name), nrow(temp.data))
      assign(paste0("mean.",sub.name), mean(colMeans(temp.data)))
      assign(paste0("wgt.",sub.name), nrow(temp.data)/numrow)
      # Export object (Good one) 
      assign(paste0(names(sub.data)[n]), data.frame(temp.data))
    }}
  
  

  # E) Calculate Jaccard index
  

  # Group a
  jaccard.a <-NA
  jaccard.a <- betastatjac(dataloop.a)[4]  # Calculate Jaccard
  names(jaccard.a) <- NULL                 # Eliminate number name
  
  

  # Group b
  
  # If sub.data is not empty
  if(length(sub.data)>0){
    
    # Calculate Jaccard index in Loop with ifs
    for(n in 1:length(sub.data)){
      
      # Get list of sub objects
      sub.ls <- mget(ls(pattern = "sub.*"))                        # All sub objects
      sub.ls.data   <- mget(ls(sub.ls,pattern = "dataloop.b.*"))   # Sub rows
      sub.ls.numrow <- mget(ls(sub.ls,pattern = "numrow.sub.*"))   # Sub rows
      sub.ls.numcol <- mget(ls(sub.ls,pattern = "numcol.sub.*"))   # sub cols
      sub.ls.mean   <- mget(ls(sub.ls,pattern = "mean.sub.*"))     # mean
      sub.ls.w      <- mget(ls(sub.ls,pattern = "wgt.sub."))       # Weight
      
      # Extract last part of object name
      sub.name <- substrRight(c(names(sub.data)[n]),5)
      
      # Generate empty Jaccard
      #jaccard.b <- NA 
      
      # If sub.data is not empty
      if(length(sub.data)>0 & sub.ls.mean[n]>0){
        # Begin ifs 
        if(sub.ls.numrow[n]>2 ){    #          # If 2+ rows and non-zero content
          print(sub.name)[n]                                     # Print data name
          print(sub.ls.mean[n])                                  # Print mean
          # Jaccard
          jack.b <- betastatjac(as.data.frame(sub.ls.data[n]))   # Calculate Jaccard
          jack.b <- jack.b[4]                                    # Get similarity index
          names(jack.b) <- NULL                                  # Clear column names
          print(jack.b)                                          # Print Jaccard
          assign(paste0("jaccard.b.", sub.name), jack.b)         # assign name 
          #Weights
          wgt <- sub.ls.w[n]
          wgt <- as.data.frame(wgt)
          assign(paste0("weight.b.", sub.name), wgt)      
        }
        else{NA}
      }
    }
  }
  
  
  
  
  

  # F) Calculate weights
  

  # Get Jaccard index for each data segment
  jack.ls <- mget(ls(pattern = "jaccard."))   # Sub rows
  jack.ls <- as.data.frame(jack.ls)
  jack.ls
  
  # Get weights for a
  temp.w.a <- c(numrow.a/numrow)
  temp.w.a
  

  # Get weights for bs 
  
  # Get weights for all bs
  temp.w.b <- mget(ls(pattern = "weight.b."))
  
  # If temp.w.b is not empty, then calculate B's weight
  if(length(temp.w.b)>0 ){   # & sub.ls.mean[n]>0
    # Extract b weights
    temp.w.b <- as.numeric(temp.w.b[[1]])  # Convert b weight to numeric
    temp.w.b                               # Print b weight
    # Create weights Matrix of all weights
    temp.w.all <- mget(ls(pattern = "^temp.w.*"))
    temp.w.all <- unlist(temp.w.all)
    temp.w.all 
    # Calculate Jaccard Weighted Average
    jaccard.w<-weighted.mean(jack.ls,temp.w.all)
    jaccard.w
  }else{
    NA #do nothing
  }
  
  
  
  

  # G) Generate output
  # Save weighted jaccard 
  ifelse(length(temp.w.b)==0, 
         # Assign Jaccard 
         assign(paste0("JG_", i), jaccard.a),
         # Assign weighted jaccard
         ifelse(length(temp.w.b)>0 | numrow.b==0, 
                # Assign weighted jaccard
                assign(paste0("JG_", i), jaccard.w), 
                # Else zero for single column
                assign(paste0("JG_", i), 0))
  ) # End if 
  
  
  

  # F) Return output 
  #label.jac
  output   <- mget(ls(pattern = "JG_"))
  output
  #return(output)
  
  

  
  
} # End of loop



## Loop Paras Jaccard index over time #########################################  
  
# Select specific years (some were giving trouble)  
seq.y2<-c(1988,1989,
          1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,
          2000,2001,2002,2003,2004,2005     ,2007,2008,2009,
          2010,2011,2012,2013,2014,2015,2016,2017)
  
# Run loop for specific years
for (i in seq.y2) {
  
  # seq.y
  # i <- 1993
  

  # CONTENT
  # A) Remove all the little components
  # B) Generate subset by year
  # C) Eliminate rows with NAs
  # D) Reduce dimentionality of rows with NAs
  # E) Calculate Jaccard index
  # F) Calculate weights
  # G) Generate output  
  

  # A) Remove all the little components
  
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  if(exists("temp.data")==TRUE){rm(temp.data)}else{NA}
  if(exists("sub.name")==TRUE){rm(sub.name)}else{NA}
  rm(list=ls(pattern="sub.ls"))
  rm(list=ls(pattern="dataloop.b.sub."))
  rm(list=ls(pattern="^jaccard."))
  if(exists("jack.b")==TRUE){rm(jack.b)}else{NA}
  rm(list=ls(pattern="^weight.b."))
  rm(list=ls(pattern="^wgt.sub."))
  rm(list=ls(pattern="^w."))
  rm(list=ls(pattern="^temp.w."))
  rm(list=ls(pattern="^numrow."))
  
  

  # B) Generate subset by year
  
  dataloop <- subset(Paras, year==i)
  # Eliminate columns with all NAs
  dataloop <- Filter(function(dataloop)!all(is.na(dataloop)), dataloop)
  dim(dataloop)
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  # Extract variables per year
  V <- names(dataloop)
  # Generate a "V" object per year containing variable names
  assign(paste0("VP_", i), V)
  # Get n rows
  numrow <-nrow(dataloop)
  numcol <-ncol(dataloop)
  
  

  # C) Eliminate rows with NAs
  
  dataloop.a <- NA
  dataloop.a <- na.omit(dataloop) # <<<< eliminates row if there is a NA
  dim(dataloop.a)
  # Count number of columns and rows
  numcol.a <-ncol(dataloop.a)
  numrow.a <-nrow(dataloop.a)
  numrow.a
  
  

  # D) Reduce dimentionality of rows with NAs
  
  dataloop.b <- NA
  dataloop.b <- dataloop[rowSums(is.na(dataloop)) > 0, ]     # <<<< Keep all rows with at least one NA
  dim(dataloop.b)
  # Count number of columns and rows
  numrow.b <-nrow(dataloop.b)
  numrow.b
  
  # Unite database as character
  dataloop.b[] <- lapply(dataloop.b, as.character)
  dataloop.b <- dataloop.b %>% unite("split", sep = "_", na.rm = TRUE)
  dim(dataloop.b)
  
  # Generate groups object
  dataloop.b$lenght <- nchar(dataloop.b$split)            # Get length
  dataloop.b<- subset(dataloop.b, lenght>=3)              # Subset larger than one annotation
  
  # Subset data by groups by groups into a list object
  for(x in unique(dataloop.b$lenght)){
    # Subset data
    assign(paste0("dataloop.b.sub.",x),subset(dataloop.b, lenght==x, select="split")) 
  }
  
  # Get list of dataloop.b.# data frames
  sub.data <- mget(ls(pattern = "dataloop.b.sub.*"))
  sub.data
  
  # Create function to extract last element from string
  substrRight <- function(x, n){
    sapply(x, function(xx)
      substr(xx, (nchar(xx)-n+1), nchar(xx)))}
  
  
  # If sub.data is not empty
  if(length(sub.data)>0){
    # Extract data from list into data frame
    for(n in 1:length(sub.data)){
      # Extract last part of object name
      sub.name <- substrRight(c(names(sub.data)[n]),5)
      # Create data frame
      temp.data <- as.data.frame(sub.data[[n]])
      # Split columns by _
      temp.data <- as.data.frame(do.call('rbind', strsplit(as.character(temp.data$split),'_')))
      # Convert to data frame and numeric
      temp.data <- data.frame(temp.data)
      temp.data <- temp.data %>% mutate_all(as.numeric) %>% as.data.frame()
      # Get row and column names
      assign(paste0("numcol.",sub.name), ncol(temp.data))
      assign(paste0("numrow.",sub.name), nrow(temp.data))
      assign(paste0("mean.",sub.name), mean(colMeans(temp.data)))
      assign(paste0("wgt.",sub.name), nrow(temp.data)/numrow)
      # Export object (Good one) 
      assign(paste0(names(sub.data)[n]), data.frame(temp.data))
    }}
  
  

  # E) Calculate Jaccard index
  

  # Group a
  jaccard.a <-NA
  jaccard.a <- betastatjac(dataloop.a)[4]  # Calculate Jaccard
  names(jaccard.a) <- NULL                 # Eliminate number name
  
  

  # Group b
  
  # If sub.data is not empty
  if(length(sub.data)>0){
    
    # Calculate Jaccard index in Loop with ifs
    for(n in 1:length(sub.data)){
      
      # Get list of sub objects
      sub.ls <- mget(ls(pattern = "sub.*"))                        # All sub objects
      sub.ls.data   <- mget(ls(sub.ls,pattern = "dataloop.b.*"))   # Sub rows
      sub.ls.numrow <- mget(ls(sub.ls,pattern = "numrow.sub.*"))   # Sub rows
      sub.ls.numcol <- mget(ls(sub.ls,pattern = "numcol.sub.*"))   # sub cols
      sub.ls.mean   <- mget(ls(sub.ls,pattern = "mean.sub.*"))     # mean
      sub.ls.w      <- mget(ls(sub.ls,pattern = "wgt.sub."))       # Weight
      
      # Extract last part of object name
      sub.name <- substrRight(c(names(sub.data)[n]),5)
      
      # Generate empty Jaccard
      #jaccard.b <- NA 
      
      # If sub.data is not empty
      if(length(sub.data)>0 & sub.ls.mean[n]>0){
        # Begin ifs 
        if(sub.ls.numrow[n]>2 ){    #          # If 2+ rows and non-zero content
          print(sub.name)[n]                                     # Print data name
          print(sub.ls.mean[n])                                  # Print mean
          # Jaccard
          jack.b <- betastatjac(as.data.frame(sub.ls.data[n]))   # Calculate Jaccard
          jack.b <- jack.b[4]                                    # Get similarity index
          names(jack.b) <- NULL                                  # Clear column names
          print(jack.b)                                          # Print Jaccard
          assign(paste0("jaccard.b.", sub.name), jack.b)         # assign name 
          #Weights
          wgt <- sub.ls.w[n]
          wgt <- as.data.frame(wgt)
          assign(paste0("weight.b.", sub.name), wgt)      
        }
        else{NA}
      }
    }
  }
  
  
  
  
  

  # F) Calculate weights
  

  # Get Jaccard index for each data segment
  jack.ls <- mget(ls(pattern = "jaccard."))   # Sub rows
  jack.ls <- as.data.frame(jack.ls)
  jack.ls
  
  # Get weights for a
  temp.w.a <- c(numrow.a/numrow)
  temp.w.a
  

  # Get weights for bs 
  
  # Get weights for all bs
  temp.w.b <- mget(ls(pattern = "weight.b."))
  
  # If temp.w.b is not empty, then calculate B's weight
  if(length(temp.w.b)>0 ){   # & sub.ls.mean[n]>0
    # Extract b weights
    temp.w.b <- as.numeric(temp.w.b[[1]])  # Convert b weight to numeric
    temp.w.b                               # Print b weight
    # Create weights Matrix of all weights
    temp.w.all <- mget(ls(pattern = "^temp.w.*"))
    temp.w.all <- unlist(temp.w.all)
    temp.w.all 
    # Calculate Jaccard Weighted Average
    jaccard.w<-weighted.mean(jack.ls,temp.w.all)
    jaccard.w
  }else{
    NA #do nothing
  }
  
  
  
  

  # G) Generate output
  # Save weighted jaccard 
  ifelse(length(temp.w.b)==0, 
         # Assign Jaccard 
         assign(paste0("JP_", i), jaccard.a),
         # Assign weighted jaccard
         ifelse(length(temp.w.b)>0 | numrow.b==0, 
                # Assign weighted jaccard
                assign(paste0("JP_", i), jaccard.w), 
                # Else zero for single column
                assign(paste0("JP_", i), 0))
  ) # End if 
  
  
  

  # F) Return output 
  #label.jac
  output   <- mget(ls(pattern = "JP_"))
  output
  #return(output)
  
  
  
  
  
} # End of loop

  
  
## Special code for 2006
for (i in 2006) {
  
  # seq.y
  # i <- 1993
  

  # CONTENT
  # A) Remove all the little components
  # B) Generate subset by year
  # C) Eliminate rows with NAs
  # D) Reduce dimentionality of rows with NAs
  # E) Calculate Jaccard index
  # F) Calculate weights
  # G) Generate output  
  

  # A) Remove all the little components
  
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  if(exists("temp.data")==TRUE){rm(temp.data)}else{NA}
  if(exists("sub.name")==TRUE){rm(sub.name)}else{NA}
  rm(list=ls(pattern="sub.ls"))
  rm(list=ls(pattern="dataloop.b.sub."))
  rm(list=ls(pattern="^jaccard."))
  if(exists("jack.b")==TRUE){rm(jack.b)}else{NA}
  rm(list=ls(pattern="^weight.b."))
  rm(list=ls(pattern="^wgt.sub."))
  rm(list=ls(pattern="^w."))
  rm(list=ls(pattern="^temp.w."))
  rm(list=ls(pattern="^numrow."))
  
  

  # B) Generate subset by year
  dataloop <- subset(Paras, year==i)
  # Eliminate columns with all NAs
  dataloop <- Filter(function(dataloop)!all(is.na(dataloop)), dataloop)
  dim(dataloop)
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  # Extract variables per year
  V <- names(dataloop)
  # Generate a "V" object per year containing variable names
  assign(paste0("VP_", i), V)
  # Get n rows
  numrow <-nrow(dataloop)
  numcol <-ncol(dataloop)
  
  

  # C) Eliminate rows with NAs
  
  dataloop.a <- NA
  dataloop.a <- na.omit(dataloop) # <<<< eliminates row if there is a NA
  dim(dataloop.a)
  # Count number of columns and rows
  numcol.a <-ncol(dataloop.a)
  numrow.a <-nrow(dataloop.a)
  numrow.a
  
  

  # D) Reduce dimentionality of rows with NAs
  
  dataloop.b <- NA
  dataloop.b <- dataloop[rowSums(is.na(dataloop)) > 0, ]     # <<<< Keep all rows with at least one NA
  dim(dataloop.b)
  # Count number of columns and rows
  numrow.b <-nrow(dataloop.b)
  numrow.b
  
  # Unite database as character
  dataloop.b[] <- lapply(dataloop.b, as.character)
  dataloop.b <- dataloop.b %>% unite("split", sep = "_", na.rm = TRUE)
  dim(dataloop.b)
  
  # Generate groups object
  dataloop.b$lenght <- nchar(dataloop.b$split)            # Get length
  dataloop.b<- subset(dataloop.b, lenght>=3)              # Subset larger than one annotation
  
  # Subset data by groups by groups into a list object
  for(x in unique(dataloop.b$lenght)){
    # Subset data
    assign(paste0("dataloop.b.sub.",x),subset(dataloop.b, lenght==x, select="split")) 
  }
  
  # Get list of dataloop.b.# data frames
  sub.data <- mget(ls(pattern = "dataloop.b.sub.*"))
  sub.data
  
  # Create function to extract last element from string
  substrRight <- function(x, n){
    sapply(x, function(xx)
      substr(xx, (nchar(xx)-n+1), nchar(xx)))}
  
  
  # If sub.data is not empty
  if(length(sub.data)>0){
    # Extract data from list into data frame
    for(n in 1:length(sub.data)){
      # Extract last part of object name
      sub.name <- substrRight(c(names(sub.data)[n]),5)
      # Create data frame
      temp.data <- as.data.frame(sub.data[[n]])
      # Split columns by _
      temp.data <- as.data.frame(do.call('rbind', strsplit(as.character(temp.data$split),'_')))
      # Convert to data frame and numeric
      temp.data <- data.frame(temp.data)
      temp.data <- temp.data %>% mutate_all(as.numeric) %>% as.data.frame()
      # Get row and column names
      assign(paste0("numcol.",sub.name), ncol(temp.data))
      assign(paste0("numrow.",sub.name), nrow(temp.data))
      assign(paste0("mean.",sub.name), mean(colMeans(temp.data)))
      assign(paste0("wgt.",sub.name), nrow(temp.data)/numrow)
      # Export object (Good one) 
      assign(paste0(names(sub.data)[n]), data.frame(temp.data))
    }}
  
  

  # E) Calculate Jaccard index
  

  # Group a
  jaccard.a <-NA
  jaccard.a <- betastatjac(dataloop.a)[4]  # Calculate Jaccard
  names(jaccard.a) <- NULL                 # Eliminate number name
  jaccard.a
  

  # Group b
  
  # # If sub.data is not empty
  # if(length(sub.data)>0){
  #   
  #   # Calculate Jaccard index in Loop with ifs
  #   for(n in 1:length(sub.data)){
  #     
  #     # Get list of sub objects
  #     sub.ls <- mget(ls(pattern = "sub.*"))                        # All sub objects
  #     sub.ls.data   <- mget(ls(sub.ls,pattern = "dataloop.b.*"))   # Sub rows
  #     sub.ls.numrow <- mget(ls(sub.ls,pattern = "numrow.sub.*"))   # Sub rows
  #     sub.ls.numcol <- mget(ls(sub.ls,pattern = "numcol.sub.*"))   # sub cols
  #     sub.ls.mean   <- mget(ls(sub.ls,pattern = "mean.sub.*"))     # mean
  #     sub.ls.w      <- mget(ls(sub.ls,pattern = "wgt.sub."))       # Weight
  #     
  #     # Extract last part of object name
  #     sub.name <- substrRight(c(names(sub.data)[n]),5)
  #     
  #     # Generate empty Jaccard
  #     #jaccard.b <- NA 
  #     
  #     # If sub.data is not empty
  #     if(length(sub.data)>0 & sub.ls.mean[n]>0){
  #       # Begin ifs 
  #       if(sub.ls.numrow[n]>2 ){    #          # If 2+ rows and non-zero content
  #         print(sub.name)[n]                                     # Print data name
  #         print(sub.ls.mean[n])                                  # Print mean
  #         # Jaccard
  #         jack.b <- betastatjac(as.data.frame(sub.ls.data[n]))   # Calculate Jaccard
  #         jack.b <- jack.b[4]                                    # Get similarity index
  #         names(jack.b) <- NULL                                  # Clear column names
  #         print(jack.b)                                          # Print Jaccard
  #         assign(paste0("jaccard.b.", sub.name), jack.b)         # assign name 
  #         #Weights
  #         wgt <- sub.ls.w[n]
  #         wgt <- as.data.frame(wgt)
  #         assign(paste0("weight.b.", sub.name), wgt)      
  #       }
  #       else{NA}
  #     }
  #   }
  # }
  
  
  
  
  

  # F) Calculate weights
  

  # Get Jaccard index for each data segment
  jack.ls <- mget(ls(pattern = "jaccard."))   # Sub rows
  jack.ls <- as.data.frame(jack.ls)
  jack.ls
  
  # Get weights for a
  temp.w.a <- c(numrow.a/numrow)
  temp.w.a
  

  # Get weights for bs
  
  # Get weights for all bs
  temp.w.b <- mget(ls(pattern = "weight.b."))
  
  # # If temp.w.b is not empty, then calculate B's weight
  # if(length(temp.w.b)>0 ){   # & sub.ls.mean[n]>0
  #   # Extract b weights
  #   temp.w.b <- as.numeric(temp.w.b[[1]])  # Convert b weight to numeric
  #   temp.w.b                               # Print b weight
  #   # Create weights Matrix of all weights
  #   temp.w.all <- mget(ls(pattern = "^temp.w.*"))
  #   temp.w.all <- unlist(temp.w.all)
  #   temp.w.all 
  #   # Calculate Jaccard Weighted Average
  #   jaccard.w<-weighted.mean(jack.ls,temp.w.all)
  #   jaccard.w
  # }else{
  #   NA #do nothing
  # }
  # 
  
  
  

  # G) Generate output
  # Save weighted jaccard 
  ifelse(length(temp.w.b)==0, 
         # Assign Jaccard 
         assign(paste0("JP_", i), jaccard.a),
         # Assign weighted jaccard
         ifelse(length(temp.w.b)>0 | numrow.b==0, 
                # Assign weighted jaccard
                assign(paste0("JP_", i), jaccard.w), 
                # Else zero for single column
                assign(paste0("JP_", i), 0))
  ) # End if 
  
  
  

  # F) Return output 
  #label.jac
  output   <- mget(ls(pattern = "JP_"))
  output
  #return(output)
  
  
  
  
  
} # End of loop







# 7. VISUALIZE JACCARD OVER YEARS FOR GUERRILLA AND PARAMILITARIES ##########################################

## Visualization  ----------------------------


## Generate data for Jaccard dataframe #########################################
  
# Extracts the 4th value of the jaccard index, wich corresponds to the similarity index "sim"
y <- c(1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
JG <- c(JG_1988 , JG_1989 , JG_1990 , JG_1991 , JG_1992 , JG_1993 , JG_1994 , JG_1995 , JG_1996 , JG_1997 , JG_1998 , JG_1999 , JG_2000 , JG_2001 , JG_2002 , JG_2003 , JG_2004 , JG_2005 , JG_2006 , JG_2007 , JG_2008 , JG_2009 , JG_2010 , JG_2011 , JG_2012 , JG_2013 , JG_2014 , JG_2015 , JG_2016 , JG_2017 )
JP <- c(JP_1988 , JP_1989 , JP_1990 , JP_1991 , JP_1992 , JP_1993 , JP_1994 , JP_1995 , JP_1996 , JP_1997 , JP_1998 , JP_1999 , JP_2000 , JP_2001 , JP_2002 , JP_2003 , JP_2004 , JP_2005 , JP_2006 , JP_2007 , JP_2008 , JP_2009 , JP_2010 , JP_2011 , JP_2012 , JP_2013 , JP_2014 , JP_2015 , JP_2016 , JP_2017 )

# Generate dataframe
Jaccard <- rbind(JG, JP)
Jaccard <- data.frame(Jaccard)
names(Jaccard) <- c(y)

# Turn into matrix
mJaccard <- as.matrix(Jaccard)
mJaccard


  
## Local Jaccard by actor type #########################################

# Replicate Figure 5a - Local Jaccard Similarity by Actor Type

# Generate heatmap data frame
hm <- data.frame(sample = rep(colnames(Jaccard), each = nrow(Jaccard)),
                 probe = rownames(Jaccard),
                 expression = unlist(Jaccard),
                 stringsAsFactors = FALSE)


# Change values for JP and JG
hm$probe[hm$probe == "JP"] <- "Paramilitaries"
hm$probe[hm$probe == "JG"] <- "Guerrilla"



# Plot heatmap with text

# Create labs
VG.labs<-c(paste(VG_1988, collapse=", "), paste(VG_1989, collapse=", "), paste(VG_1990, collapse=", "), paste(VG_1991, collapse=", "), paste(VG_1992, collapse=", "), paste(VG_1993, collapse=", "), paste(VG_1994, collapse=", "), paste(VG_1995, collapse=", "), paste(VG_1996, collapse=", "), paste(VG_1997, collapse=", "), paste(VG_1998, collapse=", "), paste(VG_1999, collapse=", "), paste(VG_2000, collapse=", "), paste(VG_2001, collapse=", "), paste(VG_2002, collapse=", "), paste(VG_2003, collapse=", "), paste(VG_2004, collapse=", "), paste(VG_2005, collapse=", "), paste(VG_2006, collapse=", "), paste(VG_2007, collapse=", "), paste(VG_2008, collapse=", "), paste(VG_2009, collapse=", "), paste(VG_2010, collapse=", "), paste(VG_2011, collapse=", "), paste(VG_2012, collapse=", "), paste(VG_2013, collapse=", "), paste(VG_2014, collapse=", "), paste(VG_2015, collapse=", "), paste(VG_2016, collapse=", "), paste(VG_2017, collapse=", "))
VP.labs<-c(paste(VP_1988, collapse=", "), paste(VP_1989, collapse=", "), paste(VP_1990, collapse=", "), paste(VP_1991, collapse=", "), paste(VP_1992, collapse=", "), paste(VP_1993, collapse=", "), paste(VP_1994, collapse=", "), paste(VP_1995, collapse=", "), paste(VP_1996, collapse=", "), paste(VP_1997, collapse=", "), paste(VP_1998, collapse=", "), paste(VP_1999, collapse=", "), paste(VP_2000, collapse=", "), paste(VP_2001, collapse=", "), paste(VP_2002, collapse=", "), paste(VP_2003, collapse=", "), paste(VP_2004, collapse=", "), paste(VP_2005, collapse=", "), paste(VP_2006, collapse=", "), paste(VP_2007, collapse=", "), paste(VP_2008, collapse=", "), paste(VP_2009, collapse=", "), paste(VP_2010, collapse=", "), paste(VP_2011, collapse=", "), paste(VP_2012, collapse=", "), paste(VP_2013, collapse=", "), paste(VP_2014, collapse=", "), paste(VP_2015, collapse=", "), paste(VP_2016, collapse=", "), paste(VP_2017, collapse=", ") )

# Plot
hml <-ggplot(hm, aes(x = probe, y = sample, fill = expression)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_tile() +
  scale_fill_gradientn(colours = magma(10),limits=c(0,1), name = "Jaccard \n Index") +
  labs(x="", y="") + 
  theme(axis.text.x = element_text(angle = 0), axis.ticks = element_blank()) +
  annotate("text", x = 1, y = 1:30, label = VG.labs, size =3, colour = "white") +
  annotate("text", x = 2, y = 1:30, label = VP.labs, size =3, colour = "white") +
  theme(legend.position="bottom", legend.direction = "horizontal") +
  theme(text = element_text(size = 14),legend.text = element_text(size = 9))
hml

hml <-  add_sub(hml, "CD=CEDE; CL=Claudia Lopez; RC=Rutas del Conflicto; VI=ViPAA; RE=Restrepo: UC=UCDP", 
                x=5.3, y=0.5, hjust = 5,
                size =8)



# Show plot
ggdraw(hml)  # The text looks odd in Rstudio, but looks nice in PDF

# Save graph
pdf("./graphs/jaccard_local/Jaccard_year_2.pdf", width=5, height=7)
ggdraw(hml)
dev.off()

# Save graph
png("./graphs/jaccard_local/Jaccard_year_2.png")
ggdraw(hml)
dev.off()






##  Time Series Plot Jaccard Actor Type over time ###########################

# Replicate Figure 7a - Local Jaccard  Similarity over time by Actor Type

hml.type.t <- ggplot(data=hm, aes(x=sample, y=expression, group=probe)) +
  geom_line(aes(color=probe),size=1) +
  ylim(0,1) + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(color='') + 
  theme(text = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab(element_blank()) + ylab("Jaccard index") +
  scale_x_discrete(breaks = seq(1988, 2018, by = 2))

ggdraw(hml.type.t) 

# Save graph
pdf("./graphs/jaccard_time/Jaccard_year_t_2.pdf", width=5, height=4)
ggdraw(hml.type.t)
dev.off()

png("./graphs/jaccard_time/Jaccard_year_t_2.png")
ggdraw(hml.type.t)
dev.off()








## Discussion  ----------------------------

## Local Jaccard results for discussion in the paper ##################################################


# Guerrilla
  
Jaccard.JG <-Jaccard[1,]
mean(as.numeric(Jaccard.JG))

# 88-92
mean(as.numeric(subset(Jaccard.JG,select =c("1988","1989","1990","1991","1992"))))
# 93-99
mean(as.numeric(subset(Jaccard.JG,select =c("1993","1994","1995","1996","1997","1998","1999"))))
# 00-10
mean(as.numeric(subset(Jaccard.JG,select =c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010"))))
# 11-17
mean(as.numeric(subset(Jaccard.JG,select =c("2011","2012","2013","2014","2015","2016","2017"))))


# 88-00
mean(as.numeric(subset(Jaccard.JG,select =c("1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000"))))

min(Jaccard.JG)
max(Jaccard.JG)





# Paramilitaries

Jaccard.JP <-Jaccard[2,]
mean(as.numeric(Jaccard.JP))

# 88-92
mean(as.numeric(subset(Jaccard.JP,select =c("1988","1989","1990","1991","1992"))))
# 93-05
mean(as.numeric(subset(Jaccard.JP,select =c("1993","1994","1996","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005"))))
# 06-10
mean(as.numeric(subset(Jaccard.JP,select =c("2006","2007","2008","2009","2010"))))
# 11-17
mean(as.numeric(subset(Jaccard.JP,select =c("2011","2012","2013","2014","2015","2016","2017"))))



min(Jaccard.JP)
max(Jaccard.JP)








# 8. AVERAGE JACCARD SIMILARITY GUERRILLA AND PARAMILITARIES ################################################

# Replicate Figure 4a Average Jaccard Similarity by Actor Type
  
# Calculate Average Jaccard 
JG.mean<-mean(JG)
JP.mean<-mean(JP)
JG.se<-std.error(JG)
JP.se<-std.error(JP)

# Round up to two decimals
JG.mean<-round(JG.mean, digits = 3)
JP.mean<-round(JP.mean, digits = 3)
JG.se<-round(JG.se, digits = 4)
JP.se<-round(JP.se, digits = 4)

# Generate database
J.mean <- data.frame(group=c("Guerrilla", "Paramilitaries"),
                     jaccard=c(JG.mean, JP.mean),
                     se=c(JG.se,JP.se))

# Bar plot
b<-ggplot(data=J.mean, aes(x=group, y=jaccard)) +
  geom_bar(stat="identity", fill="gray35") +
  geom_errorbar(aes(ymin=jaccard-se, ymax=jaccard+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + 
  scale_y_continuous(limits=c(0,1)) +
  labs(x="", y="Jaccard similarity") +
  geom_text(aes(label=jaccard), position=position_dodge(width=0.9), vjust=4, color="white") + #-0.25
  theme_minimal() +
  theme(text = element_text(size = 14),legend.text = element_text(size = 9))

b

# Save graph
pdf("./graphs/jaccard_average/Jaccard_average_2.pdf", width=3, height=3)
b
dev.off()


png("./graphs/jaccard_average/Jaccard_average_2.png")
b
dev.off()







# 9. ACTOR LEVEL SIMILARITY #################################################################################

## Get actor names by database  ----------------------------

# CD
CD.names<-names(CDall)
CD.names

# CL
CL.names<-names(CLall)
CL.names

# IN
IN.names<-names(INall)  # No Guerrilla
IN.names

# RC
RC.names<-names(RCall)
RC.names

# VI
VI.names<-names(VIall)
VI.names

# RE
RE.names<-names(REall)
RE.names


# UC
UC.names<-names(UCall)
UC.names




## FARC  ----------------------------



CD.FARC <- subset(CDall, select=c("mun", "year", "FARC"))
RC.FARC <- subset(RCall, select=c("mun", "year", "FARC"))
VI.FARC <- subset(VIall, select=c("mun", "year", "FARC"))
RE.FARC <- subset(REall, select=c("mun", "year", "FARC"))
UC.FARC <- subset(UCall, select=c("mun", "year", "FARC"))



## Eliminate duplicates by getting unique rows 
CD.FARC <- unique(CD.FARC)
RC.FARC <- unique(RC.FARC)
VI.FARC <- unique(VI.FARC)
RE.FARC <- unique(RE.FARC)
UC.FARC <- unique(UC.FARC)


CD.FARC <- CD.FARC %>% distinct(mun, year, .keep_all = TRUE)
RC.FARC <- RC.FARC %>% distinct(mun, year, .keep_all = TRUE)
VI.FARC <- VI.FARC %>% distinct(mun, year, .keep_all = TRUE)
RE.FARC <- RE.FARC %>% distinct(mun, year, .keep_all = TRUE)
UC.FARC <- UC.FARC %>% distinct(mun, year, .keep_all = TRUE)

names(CD.FARC)[3] <- "FARC1" 
names(RC.FARC)[3] <- "FARC2" 
names(VI.FARC)[3] <- "FARC3" 
names(RE.FARC)[3] <- "FARC4" 
names(UC.FARC)[3] <- "FARC5" 



## Merge 


#Get mun year template
munyear_temp <- read_csv("./data_final/Col_data.csv")
munyear_temp <- munyear_temp %>% select(mun,year)
table(munyear_temp$year)
dim(munyear_temp)


# merge data with themplate
ALL.FARC.1 <- merge(munyear_temp,VI.FARC,by=c("mun", "year"),all.x = TRUE)
ALL.FARC.1 <- merge(ALL.FARC.1,  CD.FARC,by=c("mun", "year"),all.x = TRUE)
ALL.FARC.1 <- merge(ALL.FARC.1,  RC.FARC,by=c("mun", "year"),all.x = TRUE)
ALL.FARC.1 <- merge(ALL.FARC.1,  RE.FARC,by=c("mun", "year"),all.x = TRUE)
ALL.FARC.1 <- merge(ALL.FARC.1,  UC.FARC,by=c("mun", "year"),all.x = TRUE)


## Names
names(ALL.FARC.1)
names(ALL.FARC.1)[3]<-"VI"
names(ALL.FARC.1)[4]<-"CD"
names(ALL.FARC.1)[5]<-"RC"
names(ALL.FARC.1)[6]<-"RE"
names(ALL.FARC.1)[7]<-"UC"


# Order by mun year
ALL.FARC.1 <-ALL.FARC.1[with(ALL.FARC.1, order(mun, year)),]

# Eliminate mun variable
ALL.FARC <- ALL.FARC.1[-1]

# Create a sequence of years
seq.y <- seq(1988, 2017, by=1)
seq.y <- as.factor(seq.y)
seq.y




# Loop ALL.FARC Jaccard index over
seq.y2<-c(1988,1989,
          1990,1991,1992,          1995,1996,1997,1998,1999,
          2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,
          2010,2011,2012,2013,2014,2015,2016,2017)

for (i in seq.y2) {
  
  # seq.y
  # i <- 1993
  

  # CONTENT
  # A) Remove all the little components
  # B) Generate subset by year
  # C) Eliminate rows with NAs
  # D) Reduce dimentionality of rows with NAs
  # E) Calculate Jaccard index
  # F) Calculate weights
  # G) Generate output  
  

  # A) Remove all the little components
  
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  if(exists("temp.data")==TRUE){rm(temp.data)}else{NA}
  if(exists("sub.name")==TRUE){rm(sub.name)}else{NA}
  rm(list=ls(pattern="sub.ls"))
  rm(list=ls(pattern="dataloop.b.sub."))
  rm(list=ls(pattern="^jaccard."))
  if(exists("jack.b")==TRUE){rm(jack.b)}else{NA}
  rm(list=ls(pattern="^weight.b."))
  rm(list=ls(pattern="^wgt.sub."))
  rm(list=ls(pattern="^w."))
  rm(list=ls(pattern="^temp.w."))
  rm(list=ls(pattern="^numrow."))
  
  

  # B) Generate subset by year
  dataloop <- subset(ALL.FARC, year==i)
  # Eliminate columns with all NAs
  dataloop <- Filter(function(dataloop)!all(is.na(dataloop)), dataloop)
  dim(dataloop)
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  # Extract variables per year
  V <- names(dataloop)
  # Generate a "V" object per year containing variable names
  assign(paste0("V.FARC_", i), V)
  # Get n rows
  numrow <-nrow(dataloop)
  numcol <-ncol(dataloop)
  
  

  # C) Eliminate rows with NAs
  
  dataloop.a <- NA
  dataloop.a <- na.omit(dataloop) # <<<< eliminates row if there is a NA
  dim(dataloop.a)
  # Count number of columns and rows
  numcol.a <-ncol(dataloop.a)
  numrow.a <-nrow(dataloop.a)
  numrow.a
  
  

  # D) Reduce dimentionality of rows with NAs
  
  dataloop.b <- NA
  dataloop.b <- dataloop[rowSums(is.na(dataloop)) > 0, ]     # <<<< Keep all rows with at least one NA
  dim(dataloop.b)
  # Count number of columns and rows
  numrow.b <-nrow(dataloop.b)
  numrow.b
  
  # Unite database as character
  dataloop.b[] <- lapply(dataloop.b, as.character)
  dataloop.b <- dataloop.b %>% unite("split", sep = "_", na.rm = TRUE)
  dim(dataloop.b)
  
  # Generate groups object
  dataloop.b$lenght <- nchar(dataloop.b$split)            # Get length
  dataloop.b<- subset(dataloop.b, lenght>=3)              # Subset larger than one annotation
  
  # Subset data by groups by groups into a list object
  for(x in unique(dataloop.b$lenght)){
    # Subset data
    assign(paste0("dataloop.b.sub.",x),subset(dataloop.b, lenght==x, select="split")) 
  }
  
  # Get list of dataloop.b.# data frames
  sub.data <- mget(ls(pattern = "dataloop.b.sub.*"))
  sub.data
  
  # Create function to extract last element from string
  substrRight <- function(x, n){
    sapply(x, function(xx)
      substr(xx, (nchar(xx)-n+1), nchar(xx)))}
  
  
  # If sub.data is not empty
  if(length(sub.data)>0){
    # Extract data from list into data frame
    for(n in 1:length(sub.data)){
      # Extract last part of object name
      sub.name <- substrRight(c(names(sub.data)[n]),5)
      # Create data frame
      temp.data <- as.data.frame(sub.data[[n]])
      # Split columns by _
      temp.data <- as.data.frame(do.call('rbind', strsplit(as.character(temp.data$split),'_')))
      # Convert to data frame and numeric
      temp.data <- data.frame(temp.data)
      temp.data <- temp.data %>% mutate_all(as.numeric) %>% as.data.frame()
      # Get row and column names
      assign(paste0("numcol.",sub.name), ncol(temp.data))
      assign(paste0("numrow.",sub.name), nrow(temp.data))
      assign(paste0("mean.",sub.name), mean(colMeans(temp.data)))
      assign(paste0("wgt.",sub.name), nrow(temp.data)/numrow)
      # Export object (Good one) 
      assign(paste0(names(sub.data)[n]), data.frame(temp.data))
    }}
  
  

  # E) Calculate Jaccard index
  

  # Group a
  jaccard.a <-NA
  jaccard.a <- betastatjac(dataloop.a)[4]  # Calculate Jaccard
  names(jaccard.a) <- NULL                 # Eliminate number name
  
  

  # Group b
  
  # If sub.data is not empty
  if(length(sub.data)>0){
    
    # Calculate Jaccard index in Loop with ifs
    for(n in 1:length(sub.data)){
      
      # Get list of sub objects
      sub.ls <- mget(ls(pattern = "sub.*"))                        # All sub objects
      sub.ls.data   <- mget(ls(sub.ls,pattern = "dataloop.b.*"))   # Sub rows
      sub.ls.numrow <- mget(ls(sub.ls,pattern = "numrow.sub.*"))   # Sub rows
      sub.ls.numcol <- mget(ls(sub.ls,pattern = "numcol.sub.*"))   # sub cols
      sub.ls.mean   <- mget(ls(sub.ls,pattern = "mean.sub.*"))     # mean
      sub.ls.w      <- mget(ls(sub.ls,pattern = "wgt.sub."))       # Weight
      
      # Extract last part of object name
      sub.name <- substrRight(c(names(sub.data)[n]),5)
      
      # Generate empty Jaccard
      #jaccard.b <- NA 
      
      # If sub.data is not empty
      if(length(sub.data)>0 & sub.ls.mean[n]>0){
        # Begin ifs 
        if(sub.ls.numrow[n]>2 ){    #          # If 2+ rows and non-zero content
          print(sub.name)[n]                                     # Print data name
          print(sub.ls.mean[n])                                  # Print mean
          # Jaccard
          jack.b <- betastatjac(as.data.frame(sub.ls.data[n]))   # Calculate Jaccard
          jack.b <- jack.b[4]                                    # Get similarity index
          names(jack.b) <- NULL                                  # Clear column names
          print(jack.b)                                          # Print Jaccard
          assign(paste0("jaccard.b.", sub.name), jack.b)         # assign name 
          #Weights
          wgt <- sub.ls.w[n]
          wgt <- as.data.frame(wgt)
          assign(paste0("weight.b.", sub.name), wgt)      
        }
        else{NA}
      }
    }
  }
  
  
  
  # F) Calculate weights
  

  # Get Jaccard index for each data segment
  jack.ls <- mget(ls(pattern = "jaccard."))   # Sub rows
  jack.ls <- as.data.frame(jack.ls)
  jack.ls
  
  # Get weights for a
  temp.w.a <- c(numrow.a/numrow)
  temp.w.a
  

  # Get weights for bs 
  
  # Get weights for all bs
  temp.w.b <- mget(ls(pattern = "weight.b."))
  
  # If temp.w.b is not empty, then calculate B's weight
  if(length(temp.w.b)>0 ){   # & sub.ls.mean[n]>0
    # Extract b weights
    temp.w.b <- as.numeric(temp.w.b[[1]])  # Convert b weight to numeric
    temp.w.b                               # Print b weight
    # Create weights Matrix of all weights
    temp.w.all <- mget(ls(pattern = "^temp.w.*"))
    temp.w.all <- unlist(temp.w.all)
    temp.w.all 
    # Calculate Jaccard Weighted Average
    jaccard.w<-weighted.mean(jack.ls,temp.w.all)
    jaccard.w
  }else{
    NA #do nothing
  }
  
  

  # G) Generate output
  # Save weighted jaccard 
  ifelse(length(temp.w.b)==0, 
         # Assign Jaccard 
         assign(paste0("J.FARC_", i), jaccard.a),
         # Assign weighted jaccard
         ifelse(length(temp.w.b)>0 | numrow.b==0, 
                # Assign weighted jaccard
                assign(paste0("J.FARC_", i), jaccard.w), 
                # Else zero for single column
                assign(paste0("J.FARC_", i), 0))
  ) # End if 
  
  

  # F) Return output 
  #label.jac
  output   <- mget(ls(pattern = "J.FARC_"))
  output
  #return(output)
  
  
  
} # End of loop






## Special code for 1993 and 1994
for (i in c(1993,1994)) {
  
  # seq.y
  # i <- 1993
  

  # CONTENT
  # A) Remove all the little components
  # B) Generate subset by year
  # C) Eliminate rows with NAs
  # D) Reduce dimentionality of rows with NAs
  # E) Calculate Jaccard index
  # F) Calculate weights
  # G) Generate output  
  

  # A) Remove all the little components
  
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  if(exists("temp.data")==TRUE){rm(temp.data)}else{NA}
  if(exists("sub.name")==TRUE){rm(sub.name)}else{NA}
  rm(list=ls(pattern="sub.ls"))
  rm(list=ls(pattern="dataloop.b.sub."))
  rm(list=ls(pattern="^jaccard."))
  if(exists("jack.b")==TRUE){rm(jack.b)}else{NA}
  rm(list=ls(pattern="^weight.b."))
  rm(list=ls(pattern="^wgt.sub."))
  rm(list=ls(pattern="^w."))
  rm(list=ls(pattern="^temp.w."))
  rm(list=ls(pattern="^numrow."))
  
  

  # B) Generate subset by year
  dataloop <- subset(ALL.FARC, year==i)
  # Eliminate columns with all NAs
  dataloop <- Filter(function(dataloop)!all(is.na(dataloop)), dataloop)
  dim(dataloop)
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  # Extract variables per year
  V <- names(dataloop)
  # Generate a "V" object per year containing variable names
  assign(paste0("V.FARC_", i), V)
  # Get n rows
  numrow <-nrow(dataloop)
  numcol <-ncol(dataloop)
  
  

  # C) Eliminate rows with NAs
  
  dataloop.a <- NA
  dataloop.a <- na.omit(dataloop) # <<<< eliminates row if there is a NA
  dim(dataloop.a)
  # Count number of columns and rows
  numcol.a <-ncol(dataloop.a)
  numrow.a <-nrow(dataloop.a)
  numrow.a
  
  

  # D) Reduce dimentionality of rows with NAs
  
  dataloop.b <- NA
  dataloop.b <- dataloop[rowSums(is.na(dataloop)) > 0, ]     # <<<< Keep all rows with at least one NA
  dim(dataloop.b)
  # Count number of columns and rows
  numrow.b <-nrow(dataloop.b)
  numrow.b
  
  # Unite database as character
  dataloop.b[] <- lapply(dataloop.b, as.character)
  dataloop.b <- dataloop.b %>% unite("split", sep = "_", na.rm = TRUE)
  dim(dataloop.b)
  
  # Generate groups object
  dataloop.b$lenght <- nchar(dataloop.b$split)            # Get length
  dataloop.b<- subset(dataloop.b, lenght>=3)              # Subset larger than one annotation
  
  # Subset data by groups by groups into a list object
  for(x in unique(dataloop.b$lenght)){
    # Subset data
    assign(paste0("dataloop.b.sub.",x),subset(dataloop.b, lenght==x, select="split")) 
  }
  
  # Get list of dataloop.b.# data frames
  sub.data <- mget(ls(pattern = "dataloop.b.sub.*"))
  sub.data
  
  # Create function to extract last element from string
  substrRight <- function(x, n){
    sapply(x, function(xx)
      substr(xx, (nchar(xx)-n+1), nchar(xx)))}
  
  
  # If sub.data is not empty
  if(length(sub.data)>0){
    # Extract data from list into data frame
    for(n in 1:length(sub.data)){
      # Extract last part of object name
      sub.name <- substrRight(c(names(sub.data)[n]),5)
      # Create data frame
      temp.data <- as.data.frame(sub.data[[n]])
      # Split columns by _
      temp.data <- as.data.frame(do.call('rbind', strsplit(as.character(temp.data$split),'_')))
      # Convert to data frame and numeric
      temp.data <- data.frame(temp.data)
      temp.data <- temp.data %>% mutate_all(as.numeric) %>% as.data.frame()
      # Get row and column names
      assign(paste0("numcol.",sub.name), ncol(temp.data))
      assign(paste0("numrow.",sub.name), nrow(temp.data))
      assign(paste0("mean.",sub.name), mean(colMeans(temp.data)))
      assign(paste0("wgt.",sub.name), nrow(temp.data)/numrow)
      # Export object (Good one) 
      assign(paste0(names(sub.data)[n]), data.frame(temp.data))
    }}
  
  

  # E) Calculate Jaccard index
  

  # Group a
  jaccard.a <-NA
  jaccard.a <- betastatjac(dataloop.a)[4]  # Calculate Jaccard
  names(jaccard.a) <- NULL                 # Eliminate number name
  jaccard.a
  

  # Group b
  
  # # If sub.data is not empty
  # if(length(sub.data)>0){
  #   
  #   # Calculate Jaccard index in Loop with ifs
  #   for(n in 1:length(sub.data)){
  #     
  #     # Get list of sub objects
  #     sub.ls <- mget(ls(pattern = "sub.*"))                        # All sub objects
  #     sub.ls.data   <- mget(ls(sub.ls,pattern = "dataloop.b.*"))   # Sub rows
  #     sub.ls.numrow <- mget(ls(sub.ls,pattern = "numrow.sub.*"))   # Sub rows
  #     sub.ls.numcol <- mget(ls(sub.ls,pattern = "numcol.sub.*"))   # sub cols
  #     sub.ls.mean   <- mget(ls(sub.ls,pattern = "mean.sub.*"))     # mean
  #     sub.ls.w      <- mget(ls(sub.ls,pattern = "wgt.sub."))       # Weight
  #     
  #     # Extract last part of object name
  #     sub.name <- substrRight(c(names(sub.data)[n]),5)
  #     
  #     # Generate empty Jaccard
  #     #jaccard.b <- NA 
  #     
  #     # If sub.data is not empty
  #     if(length(sub.data)>0 & sub.ls.mean[n]>0){
  #       # Begin ifs 
  #       if(sub.ls.numrow[n]>2 ){    #          # If 2+ rows and non-zero content
  #         print(sub.name)[n]                                     # Print data name
  #         print(sub.ls.mean[n])                                  # Print mean
  #         # Jaccard
  #         jack.b <- betastatjac(as.data.frame(sub.ls.data[n]))   # Calculate Jaccard
  #         jack.b <- jack.b[4]                                    # Get similarity index
  #         names(jack.b) <- NULL                                  # Clear column names
  #         print(jack.b)                                          # Print Jaccard
  #         assign(paste0("jaccard.b.", sub.name), jack.b)         # assign name 
  #         #Weights
  #         wgt <- sub.ls.w[n]
  #         wgt <- as.data.frame(wgt)
  #         assign(paste0("weight.b.", sub.name), wgt)      
  #       }
  #       else{NA}
  #     }
  #   }
  # }
  
  
  
  # F) Calculate weights
  

  # Get Jaccard index for each data segment
  jack.ls <- mget(ls(pattern = "jaccard."))   # Sub rows
  jack.ls <- as.data.frame(jack.ls)
  jack.ls
  
  # Get weights for a
  temp.w.a <- c(numrow.a/numrow)
  temp.w.a
  

  # Get weights for bs
  
  # Get weights for all bs
  temp.w.b <- mget(ls(pattern = "weight.b."))
  
  # # If temp.w.b is not empty, then calculate B's weight
  # if(length(temp.w.b)>0 ){   # & sub.ls.mean[n]>0
  #   # Extract b weights
  #   temp.w.b <- as.numeric(temp.w.b[[1]])  # Convert b weight to numeric
  #   temp.w.b                               # Print b weight
  #   # Create weights Matrix of all weights
  #   temp.w.all <- mget(ls(pattern = "^temp.w.*"))
  #   temp.w.all <- unlist(temp.w.all)
  #   temp.w.all 
  #   # Calculate Jaccard Weighted Average
  #   jaccard.w<-weighted.mean(jack.ls,temp.w.all)
  #   jaccard.w
  # }else{
  #   NA #do nothing
  # }
  # 
  
  
  

  # G) Generate output
  # Save weighted jaccard 
  ifelse(length(temp.w.b)==0, 
         # Assign Jaccard 
         assign(paste0("J.FARC_", i), jaccard.a),
         # Assign weighted jaccard
         ifelse(length(temp.w.b)>0 | numrow.b==0, 
                # Assign weighted jaccard
                assign(paste0("J.FARC_", i), jaccard.w), 
                # Else zero for single column
                assign(paste0("J.FARC_", i), 0))
  ) # End if 
  

  # F) Return output 
  #label.jac
  output   <- mget(ls(pattern = "J.FARC_"))
  output
  #return(output)
  
  
  

} # End of loop




## ELN  ----------------------------



  CD.ELN <- subset(CDall, select=c("mun", "year", "ELN"))
  RC.ELN <- subset(RCall, select=c("mun", "year", "ELN"))
  VI.ELN <- subset(VIall, select=c("mun", "year", "ELN"))
  UC.ELN <- subset(UCall, select=c("mun", "year", "ELN"))
  
  
  ## Eliminate duplicates by getting unique rows 
  CD.ELN <- unique(CD.ELN)
  RC.ELN <- unique(RC.ELN)
  VI.ELN <- unique(VI.ELN)
  UC.ELN <- unique(UC.ELN)
  
  
  CD.ELN <- CD.ELN %>% distinct(mun, year, .keep_all = TRUE)
  RC.ELN <- RC.ELN %>% distinct(mun, year, .keep_all = TRUE)
  VI.ELN <- VI.ELN %>% distinct(mun, year, .keep_all = TRUE)
  UC.ELN <- UC.ELN %>% distinct(mun, year, .keep_all = TRUE)
  
  names(CD.ELN)[3] <- "ELN1" 
  names(RC.ELN)[3] <- "ELN2" 
  names(VI.ELN)[3] <- "ELN3" 
  names(UC.ELN)[3] <- "ELN4" 
  
  

  ## Merge 
  

  #Get mun year template
  munyear_temp <- read_csv("./data_final/Col_data.csv")
  munyear_temp <- munyear_temp %>% select(mun,year)
  table(munyear_temp$year)
  dim(munyear_temp)
  

  # merge data with themplate
  ALL.ELN.1 <- merge(munyear_temp,VI.ELN,by=c("mun", "year"),all.x = TRUE)
  ALL.ELN.1 <- merge(ALL.ELN.1,  CD.ELN,by=c("mun", "year"),all.x = TRUE)
  ALL.ELN.1 <- merge(ALL.ELN.1,  RC.ELN,by=c("mun", "year"),all.x = TRUE)
  ALL.ELN.1 <- merge(ALL.ELN.1,  UC.ELN,by=c("mun", "year"),all.x = TRUE)
  
  
  ## Names
  names(ALL.ELN.1)
  names(ALL.ELN.1)[3]<-"VI"
  names(ALL.ELN.1)[4]<-"CD"
  names(ALL.ELN.1)[5]<-"RC"
  names(ALL.ELN.1)[6]<-"UC"
  
  
  # Order by mun year
  ALL.ELN.1 <-ALL.ELN.1[with(ALL.ELN.1, order(mun, year)),]
  
  # Eliminate mun variable
  ALL.ELN <- ALL.ELN.1[-1]




# Loop ALL.ELN Jaccard index over
seq.y2<-c(1993,1994,1995,1996,1997,1998,1999,
          2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,
          2010,2011,2012,2013,2014,2015,2016,2017)
for (i in seq.y2) {
  
  # seq.y
  # i <- 1993
  

  # CONTENT
  # A) Remove all the little components
  # B) Generate subset by year
  # C) Eliminate rows with NAs
  # D) Reduce dimentionality of rows with NAs
  # E) Calculate Jaccard index
  # F) Calculate weights
  # G) Generate output  
  

  # A) Remove all the little components
  
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  if(exists("temp.data")==TRUE){rm(temp.data)}else{NA}
  if(exists("sub.name")==TRUE){rm(sub.name)}else{NA}
  rm(list=ls(pattern="sub.ls"))
  rm(list=ls(pattern="dataloop.b.sub."))
  rm(list=ls(pattern="^jaccard."))
  if(exists("jack.b")==TRUE){rm(jack.b)}else{NA}
  rm(list=ls(pattern="^weight.b."))
  rm(list=ls(pattern="^wgt.sub."))
  rm(list=ls(pattern="^w."))
  rm(list=ls(pattern="^temp.w."))
  rm(list=ls(pattern="^numrow."))
  
  

  # B) Generate subset by year
  dataloop <- subset(ALL.ELN, year==i)
  # Eliminate columns with all NAs
  dataloop <- Filter(function(dataloop)!all(is.na(dataloop)), dataloop)
  dim(dataloop)
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  # Extract variables per year
  V <- names(dataloop)
  # Generate a "V" object per year containing variable names
  assign(paste0("V.ELN_", i), V)
  # Get n rows
  numrow <-nrow(dataloop)
  numcol <-ncol(dataloop)
  
  

  # C) Eliminate rows with NAs
  
  dataloop.a <- NA
  dataloop.a <- na.omit(dataloop) # <<<< eliminates row if there is a NA
  dim(dataloop.a)
  # Count number of columns and rows
  numcol.a <-ncol(dataloop.a)
  numrow.a <-nrow(dataloop.a)
  numrow.a
  
  

  # D) Reduce dimentionality of rows with NAs
  
  dataloop.b <- NA
  dataloop.b <- dataloop[rowSums(is.na(dataloop)) > 0, ]     # <<<< Keep all rows with at least one NA
  dim(dataloop.b)
  # Count number of columns and rows
  numrow.b <-nrow(dataloop.b)
  numrow.b
  
  # Unite database as character
  dataloop.b[] <- lapply(dataloop.b, as.character)
  dataloop.b <- dataloop.b %>% unite("split", sep = "_", na.rm = TRUE)
  dim(dataloop.b)
  
  # Generate groups object
  dataloop.b$lenght <- nchar(dataloop.b$split)            # Get length
  dataloop.b<- subset(dataloop.b, lenght>=3)              # Subset larger than one annotation
  
  # Subset data by groups by groups into a list object
  for(x in unique(dataloop.b$lenght)){
    # Subset data
    assign(paste0("dataloop.b.sub.",x),subset(dataloop.b, lenght==x, select="split")) 
  }
  
  # Get list of dataloop.b.# data frames
  sub.data <- mget(ls(pattern = "dataloop.b.sub.*"))
  sub.data
  
  # Create function to extract last element from string
  substrRight <- function(x, n){
    sapply(x, function(xx)
      substr(xx, (nchar(xx)-n+1), nchar(xx)))}
  
  
  # If sub.data is not empty
  if(length(sub.data)>0){
    # Extract data from list into data frame
    for(n in 1:length(sub.data)){
      # Extract last part of object name
      sub.name <- substrRight(c(names(sub.data)[n]),5)
      # Create data frame
      temp.data <- as.data.frame(sub.data[[n]])
      # Split columns by _
      temp.data <- as.data.frame(do.call('rbind', strsplit(as.character(temp.data$split),'_')))
      # Convert to data frame and numeric
      temp.data <- data.frame(temp.data)
      temp.data <- temp.data %>% mutate_all(as.numeric) %>% as.data.frame()
      # Get row and column names
      assign(paste0("numcol.",sub.name), ncol(temp.data))
      assign(paste0("numrow.",sub.name), nrow(temp.data))
      assign(paste0("mean.",sub.name), mean(colMeans(temp.data)))
      assign(paste0("wgt.",sub.name), nrow(temp.data)/numrow)
      # Export object (Good one) 
      assign(paste0(names(sub.data)[n]), data.frame(temp.data))
    }}
  
  

  # E) Calculate Jaccard index
  

  # Group a
  jaccard.a <-NA
  jaccard.a <- betastatjac(dataloop.a)[4]  # Calculate Jaccard
  names(jaccard.a) <- NULL                 # Eliminate number name
  
  

  # Group b
  
  # If sub.data is not empty
  if(length(sub.data)>0){
    
    # Calculate Jaccard index in Loop with ifs
    for(n in 1:length(sub.data)){
      
      # Get list of sub objects
      sub.ls <- mget(ls(pattern = "sub.*"))                        # All sub objects
      sub.ls.data   <- mget(ls(sub.ls,pattern = "dataloop.b.*"))   # Sub rows
      sub.ls.numrow <- mget(ls(sub.ls,pattern = "numrow.sub.*"))   # Sub rows
      sub.ls.numcol <- mget(ls(sub.ls,pattern = "numcol.sub.*"))   # sub cols
      sub.ls.mean   <- mget(ls(sub.ls,pattern = "mean.sub.*"))     # mean
      sub.ls.w      <- mget(ls(sub.ls,pattern = "wgt.sub."))       # Weight
      
      # Extract last part of object name
      sub.name <- substrRight(c(names(sub.data)[n]),5)
      
      # Generate empty Jaccard
      #jaccard.b <- NA 
      
      # If sub.data is not empty
      if(length(sub.data)>0 & sub.ls.mean[n]>0){
        # Begin ifs 
        if(sub.ls.numrow[n]>2 ){    #          # If 2+ rows and non-zero content
          print(sub.name)[n]                                     # Print data name
          print(sub.ls.mean[n])                                  # Print mean
          # Jaccard
          jack.b <- betastatjac(as.data.frame(sub.ls.data[n]))   # Calculate Jaccard
          jack.b <- jack.b[4]                                    # Get similarity index
          names(jack.b) <- NULL                                  # Clear column names
          print(jack.b)                                          # Print Jaccard
          assign(paste0("jaccard.b.", sub.name), jack.b)         # assign name 
          #Weights
          wgt <- sub.ls.w[n]
          wgt <- as.data.frame(wgt)
          assign(paste0("weight.b.", sub.name), wgt)      
        }
        else{NA}
      }
    }
  }
  
  
  
  
  # F) Calculate weights
  

  # Get Jaccard index for each data segment
  jack.ls <- mget(ls(pattern = "jaccard."))   # Sub rows
  jack.ls <- as.data.frame(jack.ls)
  jack.ls
  
  # Get weights for a
  temp.w.a <- c(numrow.a/numrow)
  temp.w.a
  

  # Get weights for bs 
  
  # Get weights for all bs
  temp.w.b <- mget(ls(pattern = "weight.b."))
  
  # If temp.w.b is not empty, then calculate B's weight
  if(length(temp.w.b)>0 ){   # & sub.ls.mean[n]>0
    # Extract b weights
    temp.w.b <- as.numeric(temp.w.b[[1]])  # Convert b weight to numeric
    temp.w.b                               # Print b weight
    # Create weights Matrix of all weights
    temp.w.all <- mget(ls(pattern = "^temp.w.*"))
    temp.w.all <- unlist(temp.w.all)
    temp.w.all 
    # Calculate Jaccard Weighted Average
    jaccard.w<-weighted.mean(jack.ls,temp.w.all)
    jaccard.w
  }else{
    NA #do nothing
  }
  
  
  
  
  # G) Generate output
  # Save weighted jaccard 
  ifelse(length(temp.w.b)==0, 
         # Assign Jaccard 
         assign(paste0("J.ELN_", i), jaccard.a),
         # Assign weighted jaccard
         ifelse(length(temp.w.b)>0 | numrow.b==0, 
                # Assign weighted jaccard
                assign(paste0("J.ELN_", i), jaccard.w), 
                # Else zero for single column
                assign(paste0("J.ELN_", i), 0))
  ) # End if 
  
  
  

  # F) Return output 
  #label.jac
  output   <- mget(ls(pattern = "J.ELN_"))
  output
  #return(output)
  
  
  
  
  
} # End of loop





# Special case for 1988
for (i in c(1988)) {
  
  # seq.y
  # i <- 1993
  

  # CONTENT
  # A) Remove all the little components
  # B) Generate subset by year
  # C) Eliminate rows with NAs
  # D) Reduce dimentionality of rows with NAs
  # E) Calculate Jaccard index
  # F) Calculate weights
  # G) Generate output  
  

  # A) Remove all the little components
  
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  if(exists("temp.data")==TRUE){rm(temp.data)}else{NA}
  if(exists("sub.name")==TRUE){rm(sub.name)}else{NA}
  rm(list=ls(pattern="sub.ls"))
  rm(list=ls(pattern="dataloop.b."))
  rm(list=ls(pattern="dataloop.b.sub."))
  rm(list=ls(pattern="^jaccard."))
  if(exists("jack.b")==TRUE){rm(jack.b)}else{NA}
  rm(list=ls(pattern="^weight.b."))
  rm(list=ls(pattern="^wgt.sub."))
  rm(list=ls(pattern="^w."))
  rm(list=ls(pattern="^temp.w."))
  rm(list=ls(pattern="^numrow."))
  
  

  # B) Generate subset by year
  dataloop <- subset(ALL.ELN, year==i)
  # Eliminate columns with all NAs
  dataloop <- Filter(function(dataloop)!all(is.na(dataloop)), dataloop)
  dim(dataloop)
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  # Extract variables per year
  V <- names(dataloop)
  # Generate a "V" object per year containing variable names
  assign(paste0("V.ELN_", i), V)
  # Get n rows
  numrow <-nrow(dataloop)
  numcol <-ncol(dataloop)
  
  

  # C) Eliminate rows with NAs
  
  dataloop.a <- NA
  dataloop.a <- na.omit(dataloop) # <<<< eliminates row if there is a NA
  dim(dataloop.a)
  # Count number of columns and rows
  numcol.a <-ncol(dataloop.a)
  numrow.a <-nrow(dataloop.a)
  numrow.a
  # This is a special case wint  single variable
  # and numrow.a==numrow
  
  

  # D) Reduce dimentionality of rows with NAs
  
  dataloop.b <- NA
  dataloop.b <- dataloop[rowSums(is.na(dataloop)) > 0, ]     # <<<< Keep all rows with at least one NA
  dim(dataloop.b)
  # Count number of columns and rows
  numrow.b <-nrow(dataloop.b)
  numrow.b
  
  # # Unite database as character
  # dataloop.b[] <- lapply(dataloop.b, as.character)
  # dataloop.b <- dataloop.b %>% unite("split", sep = "_", na.rm = TRUE)
  # dim(dataloop.b)
  # 
  # # Generate groups object
  # dataloop.b$lenght <- nchar(dataloop.b$split)            # Get length
  # dataloop.b<- subset(dataloop.b, lenght>=3)              # Subset larger than one annotation
  # 
  # # Subset data by groups by groups into a list object
  # for(x in unique(dataloop.b$lenght)){
  #   # Subset data
  #   assign(paste0("dataloop.b.sub.",x),subset(dataloop.b, lenght==x, select="split")) 
  # }
  # 
  # # Get list of dataloop.b.# data frames
  # sub.data <- mget(ls(pattern = "dataloop.b.sub.*"))
  # sub.data
  # 
  # # Create function to extract last element from string
  # substrRight <- function(x, n){
  #   sapply(x, function(xx)
  #     substr(xx, (nchar(xx)-n+1), nchar(xx)))}
  # 
  # 
  # # If sub.data is not empty
  # if(length(sub.data)>0){
  #   # Extract data from list into data frame
  #   for(n in 1:length(sub.data)){
  #     # Extract last part of object name
  #     sub.name <- substrRight(c(names(sub.data)[n]),5)
  #     # Create data frame
  #     temp.data <- as.data.frame(sub.data[[n]])
  #     # Split columns by _
  #     temp.data <- as.data.frame(do.call('rbind', strsplit(as.character(temp.data$split),'_')))
  #     # Convert to data frame and numeric
  #     temp.data <- data.frame(temp.data)
  #     temp.data <- temp.data %>% mutate_all(as.numeric) %>% as.data.frame()
  #     # Get row and column names
  #     assign(paste0("numcol.",sub.name), ncol(temp.data))
  #     assign(paste0("numrow.",sub.name), nrow(temp.data))
  #     assign(paste0("mean.",sub.name), mean(colMeans(temp.data)))
  #     assign(paste0("wgt.",sub.name), nrow(temp.data)/numrow)
  #     # Export object (Good one) 
  #     assign(paste0(names(sub.data)[n]), data.frame(temp.data))
  #   }}
  
  
  # E) Calculate Jaccard index
  
  # # Group a
  # jaccard.a <-NA
  # jaccard.a <- betastatjac(dataloop.a)[4]  # Calculate Jaccard
  # names(jaccard.a) <- NULL                 # Eliminate number name
  # jaccard.a
  

  # Group b
  
  # # If sub.data is not empty
  # if(length(sub.data)>0){
  #   
  #   # Calculate Jaccard index in Loop with ifs
  #   for(n in 1:length(sub.data)){
  #     
  #     # Get list of sub objects
  #     sub.ls <- mget(ls(pattern = "sub.*"))                        # All sub objects
  #     sub.ls.data   <- mget(ls(sub.ls,pattern = "dataloop.b.*"))   # Sub rows
  #     sub.ls.numrow <- mget(ls(sub.ls,pattern = "numrow.sub.*"))   # Sub rows
  #     sub.ls.numcol <- mget(ls(sub.ls,pattern = "numcol.sub.*"))   # sub cols
  #     sub.ls.mean   <- mget(ls(sub.ls,pattern = "mean.sub.*"))     # mean
  #     sub.ls.w      <- mget(ls(sub.ls,pattern = "wgt.sub."))       # Weight
  #     
  #     # Extract last part of object name
  #     sub.name <- substrRight(c(names(sub.data)[n]),5)
  #     
  #     # Generate empty Jaccard
  #     #jaccard.b <- NA 
  #     
  #     # If sub.data is not empty
  #     if(length(sub.data)>0 & sub.ls.mean[n]>0){
  #       # Begin ifs 
  #       if(sub.ls.numrow[n]>2 ){    #          # If 2+ rows and non-zero content
  #         print(sub.name)[n]                                     # Print data name
  #         print(sub.ls.mean[n])                                  # Print mean
  #         # Jaccard
  #         jack.b <- betastatjac(as.data.frame(sub.ls.data[n]))   # Calculate Jaccard
  #         jack.b <- jack.b[4]                                    # Get similarity index
  #         names(jack.b) <- NULL                                  # Clear column names
  #         print(jack.b)                                          # Print Jaccard
  #         assign(paste0("jaccard.b.", sub.name), jack.b)         # assign name 
  #         #Weights
  #         wgt <- sub.ls.w[n]
  #         wgt <- as.data.frame(wgt)
  #         assign(paste0("weight.b.", sub.name), wgt)      
  #       }
  #       else{NA}
  #     }
  #   }
  # }
  
  
  
  # F) Calculate weights
  

  # # Get Jaccard index for each data segment
  # jack.ls <- mget(ls(pattern = "jaccard."))   # Sub rows
  # jack.ls <- as.data.frame(jack.ls)
  # jack.ls
  # 
  # # Get weights for a
  # temp.w.a <- c(numrow.a/numrow)
  # temp.w.a
  # 

  # # Get weights for bs
  # 
  # # Get weights for all bs
  # temp.w.b <- mget(ls(pattern = "weight.b."))
  
  # # If temp.w.b is not empty, then calculate B's weight
  # if(length(temp.w.b)>0 ){   # & sub.ls.mean[n]>0
  #   # Extract b weights
  #   temp.w.b <- as.numeric(temp.w.b[[1]])  # Convert b weight to numeric
  #   temp.w.b                               # Print b weight
  #   # Create weights Matrix of all weights
  #   temp.w.all <- mget(ls(pattern = "^temp.w.*"))
  #   temp.w.all <- unlist(temp.w.all)
  #   temp.w.all 
  #   # Calculate Jaccard Weighted Average
  #   jaccard.w<-weighted.mean(jack.ls,temp.w.all)
  #   jaccard.w
  # }else{
  #   NA #do nothing
  # }
  # 
  
  

  # # G) Generate output
  # # Save weighted jaccard 
  # ifelse(length(temp.w.b)==0, 
  #        # Assign Jaccard 
  #        assign(paste0("J.ELN_", i), jaccard.a),
  #        # Assign weighted jaccard
  #        ifelse(length(temp.w.b)>0 | numrow.b==0, 
  #               # Assign weighted jaccard
  #               assign(paste0("J.ELN_", i), jaccard.w), 
  #               # Else zero for single column
  #               assign(paste0("J.ELN_", i), 0))
  # ) # End if 
  
  # IF there is only one variable in dataloop.a
  ifelse(dim(dataloop.a)[2]==1,
         # Assign zero for single column
         assign(paste0("J.ELN_", i), 0),
         NA
  )
  
  
  
  # F) Return output 
  #label.jac
  output   <- mget(ls(pattern = "J.ELN_"))
  output
  #return(output)
  
  
  
  
  
} # End of loop





# Special case for  1989, 1990, 1991, 1992
for (i in c(1989, 1990, 1991, 1992)) {
  
  # seq.y
  # i <- 1993
  

  # CONTENT
  # A) Remove all the little components
  # B) Generate subset by year
  # C) Eliminate rows with NAs
  # D) Reduce dimentionality of rows with NAs
  # E) Calculate Jaccard index
  # F) Calculate weights
  # G) Generate output  
  

  # A) Remove all the little components
  
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  if(exists("temp.data")==TRUE){rm(temp.data)}else{NA}
  if(exists("sub.name")==TRUE){rm(sub.name)}else{NA}
  rm(list=ls(pattern="sub.ls"))
  rm(list=ls(pattern="dataloop.b."))
  rm(list=ls(pattern="dataloop.b.sub."))
  rm(list=ls(pattern="^jaccard."))
  if(exists("jack.b")==TRUE){rm(jack.b)}else{NA}
  rm(list=ls(pattern="^weight.b."))
  rm(list=ls(pattern="^wgt.sub."))
  rm(list=ls(pattern="^w."))
  rm(list=ls(pattern="^temp.w."))
  rm(list=ls(pattern="^numrow."))
  


  # B) Generate subset by year
  dataloop <- subset(ALL.ELN, year==i)
  # Eliminate columns with all NAs
  dataloop <- Filter(function(dataloop)!all(is.na(dataloop)), dataloop)
  dim(dataloop)
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  # Extract variables per year
  V <- names(dataloop)
  # Generate a "V" object per year containing variable names
  assign(paste0("V.ELN_", i), V)
  # Get n rows
  numrow <-nrow(dataloop)
  numcol <-ncol(dataloop)
  
  

  # C) Eliminate rows with NAs
  
  dataloop.a <- NA
  dataloop.a <- na.omit(dataloop) # <<<< eliminates row if there is a NA
  dim(dataloop.a)
  # Count number of columns and rows
  numcol.a <-ncol(dataloop.a)
  numrow.a <-nrow(dataloop.a)
  numrow.a
  # This is a special case wint  single variable
  # and numrow.a==numrow
  
  

  # D) Reduce dimentionality of rows with NAs
  
  dataloop.b <- NA
  dataloop.b <- dataloop[rowSums(is.na(dataloop)) > 0, ]     # <<<< Keep all rows with at least one NA
  dim(dataloop.b)
  # Count number of columns and rows
  numrow.b <-nrow(dataloop.b)
  numrow.b
  
  # # Unite database as character
  # dataloop.b[] <- lapply(dataloop.b, as.character)
  # dataloop.b <- dataloop.b %>% unite("split", sep = "_", na.rm = TRUE)
  # dim(dataloop.b)
  # 
  # # Generate groups object
  # dataloop.b$lenght <- nchar(dataloop.b$split)            # Get length
  # dataloop.b<- subset(dataloop.b, lenght>=3)              # Subset larger than one annotation
  # 
  # # Subset data by groups by groups into a list object
  # for(x in unique(dataloop.b$lenght)){
  #   # Subset data
  #   assign(paste0("dataloop.b.sub.",x),subset(dataloop.b, lenght==x, select="split")) 
  # }
  # 
  # # Get list of dataloop.b.# data frames
  # sub.data <- mget(ls(pattern = "dataloop.b.sub.*"))
  # sub.data
  # 
  # # Create function to extract last element from string
  # substrRight <- function(x, n){
  #   sapply(x, function(xx)
  #     substr(xx, (nchar(xx)-n+1), nchar(xx)))}
  # 
  # 
  # # If sub.data is not empty
  # if(length(sub.data)>0){
  #   # Extract data from list into data frame
  #   for(n in 1:length(sub.data)){
  #     # Extract last part of object name
  #     sub.name <- substrRight(c(names(sub.data)[n]),5)
  #     # Create data frame
  #     temp.data <- as.data.frame(sub.data[[n]])
  #     # Split columns by _
  #     temp.data <- as.data.frame(do.call('rbind', strsplit(as.character(temp.data$split),'_')))
  #     # Convert to data frame and numeric
  #     temp.data <- data.frame(temp.data)
  #     temp.data <- temp.data %>% mutate_all(as.numeric) %>% as.data.frame()
  #     # Get row and column names
  #     assign(paste0("numcol.",sub.name), ncol(temp.data))
  #     assign(paste0("numrow.",sub.name), nrow(temp.data))
  #     assign(paste0("mean.",sub.name), mean(colMeans(temp.data)))
  #     assign(paste0("wgt.",sub.name), nrow(temp.data)/numrow)
  #     # Export object (Good one) 
  #     assign(paste0(names(sub.data)[n]), data.frame(temp.data))
  #   }}
  
  

  # E) Calculate Jaccard index
  

  # Group a
  jaccard.a <-NA
  jaccard.a <- betastatjac(dataloop.a)[4]  # Calculate Jaccard
  names(jaccard.a) <- NULL                 # Eliminate number name
  jaccard.a
  

  # Group b
  
  # # If sub.data is not empty
  # if(length(sub.data)>0){
  #   
  #   # Calculate Jaccard index in Loop with ifs
  #   for(n in 1:length(sub.data)){
  #     
  #     # Get list of sub objects
  #     sub.ls <- mget(ls(pattern = "sub.*"))                        # All sub objects
  #     sub.ls.data   <- mget(ls(sub.ls,pattern = "dataloop.b.*"))   # Sub rows
  #     sub.ls.numrow <- mget(ls(sub.ls,pattern = "numrow.sub.*"))   # Sub rows
  #     sub.ls.numcol <- mget(ls(sub.ls,pattern = "numcol.sub.*"))   # sub cols
  #     sub.ls.mean   <- mget(ls(sub.ls,pattern = "mean.sub.*"))     # mean
  #     sub.ls.w      <- mget(ls(sub.ls,pattern = "wgt.sub."))       # Weight
  #     
  #     # Extract last part of object name
  #     sub.name <- substrRight(c(names(sub.data)[n]),5)
  #     
  #     # Generate empty Jaccard
  #     #jaccard.b <- NA 
  #     
  #     # If sub.data is not empty
  #     if(length(sub.data)>0 & sub.ls.mean[n]>0){
  #       # Begin ifs 
  #       if(sub.ls.numrow[n]>2 ){    #          # If 2+ rows and non-zero content
  #         print(sub.name)[n]                                     # Print data name
  #         print(sub.ls.mean[n])                                  # Print mean
  #         # Jaccard
  #         jack.b <- betastatjac(as.data.frame(sub.ls.data[n]))   # Calculate Jaccard
  #         jack.b <- jack.b[4]                                    # Get similarity index
  #         names(jack.b) <- NULL                                  # Clear column names
  #         print(jack.b)                                          # Print Jaccard
  #         assign(paste0("jaccard.b.", sub.name), jack.b)         # assign name 
  #         #Weights
  #         wgt <- sub.ls.w[n]
  #         wgt <- as.data.frame(wgt)
  #         assign(paste0("weight.b.", sub.name), wgt)      
  #       }
  #       else{NA}
  #     }
  #   }
  # }
  
  
  
  
  

  # F) Calculate weights
  

  # # Get Jaccard index for each data segment
  # jack.ls <- mget(ls(pattern = "jaccard."))   # Sub rows
  # jack.ls <- as.data.frame(jack.ls)
  # jack.ls
  # 
  # # Get weights for a
  # temp.w.a <- c(numrow.a/numrow)
  # temp.w.a
  # 

  # # Get weights for bs
  # 
  # # Get weights for all bs
  # temp.w.b <- mget(ls(pattern = "weight.b."))
  
  # # If temp.w.b is not empty, then calculate B's weight
  # if(length(temp.w.b)>0 ){   # & sub.ls.mean[n]>0
  #   # Extract b weights
  #   temp.w.b <- as.numeric(temp.w.b[[1]])  # Convert b weight to numeric
  #   temp.w.b                               # Print b weight
  #   # Create weights Matrix of all weights
  #   temp.w.all <- mget(ls(pattern = "^temp.w.*"))
  #   temp.w.all <- unlist(temp.w.all)
  #   temp.w.all 
  #   # Calculate Jaccard Weighted Average
  #   jaccard.w<-weighted.mean(jack.ls,temp.w.all)
  #   jaccard.w
  # }else{
  #   NA #do nothing
  # }
  # 
  
  
  

  # # G) Generate output
  # # Save weighted jaccard 
  # ifelse(length(temp.w.b)==0, 
  #        # Assign Jaccard 
  #        assign(paste0("J.ELN_", i), jaccard.a),
  #        # Assign weighted jaccard
  #        ifelse(length(temp.w.b)>0 | numrow.b==0, 
  #               # Assign weighted jaccard
  #               assign(paste0("J.ELN_", i), jaccard.w), 
  #               # Else zero for single column
  #               assign(paste0("J.ELN_", i), 0))
  # ) # End if 
  
  # IF there is only one variable in dataloop.a
  ifelse(dim(dataloop.a)[2]>=1,
         # Assign zero for single column
         assign(paste0("J.ELN_", i), jaccard.a),
         NA
  )
  
  
  

  # F) Return output 
  #label.jac
  # output   <- mget(ls(pattern = "J.ELN_"))
  # output
  #return(output)
  
  
  
  
  
} # End of loop






## AUC  ----------------------------


  
  CD.AUC <- subset(CDall, select=c("mun", "year", "AUC"))
  RC.AUC <- subset(RCall, select=c("mun", "year", "AUC"))
  VI.AUC <- subset(VIall, select=c("mun", "year", "AUC"))
  RE.AUC <- subset(REall, select=c("mun", "year", "AUC"))
  UC.AUC <- subset(UCall, select=c("mun", "year", "AUC"))
  
  
  ## Eliminate duplicates by getting unique rows 
  CD.AUC <- unique(CD.AUC)
  RC.AUC <- unique(RC.AUC)
  VI.AUC <- unique(VI.AUC)
  RE.AUC <- unique(RE.AUC)
  UC.AUC <- unique(UC.AUC)
  
  
  CD.AUC <- CD.AUC %>% distinct(mun, year, .keep_all = TRUE)
  RC.AUC <- RC.AUC %>% distinct(mun, year, .keep_all = TRUE)
  VI.AUC <- VI.AUC %>% distinct(mun, year, .keep_all = TRUE)
  RE.AUC <- RE.AUC %>% distinct(mun, year, .keep_all = TRUE)
  UC.AUC <- UC.AUC %>% distinct(mun, year, .keep_all = TRUE)
  
  names(CD.AUC)[3] <- "CD" 
  names(RC.AUC)[3] <- "RC" 
  names(VI.AUC)[3] <- "VI" 
  names(RE.AUC)[3] <- "RE" 
  names(UC.AUC)[3] <- "UC" 
  


  ## Merge 
  

  #Get mun year template
  munyear_temp <- read_csv("./data_final/Col_data.csv")
  munyear_temp <- munyear_temp %>% select(mun,year)
  table(munyear_temp$year)
  dim(munyear_temp)
  

  # merge data with themplate
  ALL.AUC.1 <- merge(munyear_temp,VI.AUC,by=c("mun", "year"),all.x = TRUE)
  ALL.AUC.1 <- merge(ALL.AUC.1,  CD.AUC,by=c("mun", "year"),all.x = TRUE)
  ALL.AUC.1 <- merge(ALL.AUC.1,  RC.AUC,by=c("mun", "year"),all.x = TRUE)
  ALL.AUC.1 <- merge(ALL.AUC.1,  RE.AUC,by=c("mun", "year"),all.x = TRUE)
  ALL.AUC.1 <- merge(ALL.AUC.1,  UC.AUC,by=c("mun", "year"),all.x = TRUE)
  
  dim(ALL.AUC.1)
  table(ALL.AUC.1$year)
  
  
  # Order by mun year
  ALL.AUC.1 <- ALL.AUC.1[with(ALL.AUC.1, order(mun, year)),]
  
  # Eliminate mun variable
  ALL.AUC <- ALL.AUC.1[-1]
  
  # Check names
  names(ALL.AUC)



#Create a sequence of years
seq.y <- seq(1988, 2017, by=1)
seq.y <- as.factor(seq.y)





# Loop ALL.AUC Jaccard index over
seq.y2<-c(       1993,1994,1995,1996,1997,1998,1999,
  2000,2001,2002,2003,2004,2005,               2009,
  2010,2011,2012,2013,2014,2015,2016,2017)


for (i in seq.y2) {
  
  # seq.y
  # i <- 1993
  

  # CONTENT
  # A) Remove all the little components
  # B) Generate subset by year
  # C) Eliminate rows with NAs
  # D) Reduce dimentionality of rows with NAs
  # E) Calculate Jaccard index
  # F) Calculate weights
  # G) Generate output  
  

  # A) Remove all the little components
  
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  if(exists("temp.data")==TRUE){rm(temp.data)}else{NA}
  if(exists("sub.name")==TRUE){rm(sub.name)}else{NA}
  rm(list=ls(pattern="sub.ls"))
  rm(list=ls(pattern="dataloop.b.sub."))
  rm(list=ls(pattern="^jaccard."))
  if(exists("jack.b")==TRUE){rm(jack.b)}else{NA}
  rm(list=ls(pattern="^weight.b."))
  rm(list=ls(pattern="^wgt.sub."))
  rm(list=ls(pattern="^w."))
  rm(list=ls(pattern="^temp.w."))
  rm(list=ls(pattern="^numrow."))
  
  

  # B) Generate subset by year
  dataloop <- subset(ALL.AUC, year==i)
  # Eliminate columns with all NAs
  dataloop <- Filter(function(dataloop)!all(is.na(dataloop)), dataloop)
  dim(dataloop)
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  # Extract variables per year
  V <- names(dataloop)
  # Generate a "V" object per year containing variable names
  assign(paste0("V.AUC_", i), V)
  # Get n rows
  numrow <-nrow(dataloop)
  numcol <-ncol(dataloop)
  
  

  # C) Eliminate rows with NAs
  
  dataloop.a <- NA
  dataloop.a <- na.omit(dataloop) # <<<< eliminates row if there is a NA
  dim(dataloop.a)
  # Count number of columns and rows
  numcol.a <-ncol(dataloop.a)
  numrow.a <-nrow(dataloop.a)
  numrow.a
  
  

  # D) Reduce dimentionality of rows with NAs
  
  dataloop.b <- NA
  dataloop.b <- dataloop[rowSums(is.na(dataloop)) > 0, ]     # <<<< Keep all rows with at least one NA
  dim(dataloop.b)
  # Count number of columns and rows
  numrow.b <-nrow(dataloop.b)
  numrow.b
  
  # Unite database as character
  dataloop.b[] <- lapply(dataloop.b, as.character)
  dataloop.b <- dataloop.b %>% unite("split", sep = "_", na.rm = TRUE)
  dim(dataloop.b)
  
  # Generate groups object
  dataloop.b$lenght <- nchar(dataloop.b$split)            # Get length
  dataloop.b<- subset(dataloop.b, lenght>=3)              # Subset larger than one annotation
  
  # Subset data by groups by groups into a list object
  for(x in unique(dataloop.b$lenght)){
    # Subset data
    assign(paste0("dataloop.b.sub.",x),subset(dataloop.b, lenght==x, select="split")) 
  }
  
  # Get list of dataloop.b.# data frames
  sub.data <- mget(ls(pattern = "dataloop.b.sub.*"))
  sub.data
  
  # Create function to extract last element from string
  substrRight <- function(x, n){
    sapply(x, function(xx)
      substr(xx, (nchar(xx)-n+1), nchar(xx)))}
  
  
  # If sub.data is not empty
  if(length(sub.data)>0){
    # Extract data from list into data frame
    for(n in 1:length(sub.data)){
      # Extract last part of object name
      sub.name <- substrRight(c(names(sub.data)[n]),5)
      # Create data frame
      temp.data <- as.data.frame(sub.data[[n]])
      # Split columns by _
      temp.data <- as.data.frame(do.call('rbind', strsplit(as.character(temp.data$split),'_')))
      # Convert to data frame and numeric
      temp.data <- data.frame(temp.data)
      temp.data <- temp.data %>% mutate_all(as.numeric) %>% as.data.frame()
      # Get row and column names
      assign(paste0("numcol.",sub.name), ncol(temp.data))
      assign(paste0("numrow.",sub.name), nrow(temp.data))
      assign(paste0("mean.",sub.name), mean(colMeans(temp.data)))
      assign(paste0("wgt.",sub.name), nrow(temp.data)/numrow)
      # Export object (Good one) 
      assign(paste0(names(sub.data)[n]), data.frame(temp.data))
    }}
  
  

  # E) Calculate Jaccard index
  

  # Group a
  jaccard.a <-NA
  jaccard.a <- betastatjac(dataloop.a)[4]  # Calculate Jaccard
  names(jaccard.a) <- NULL                 # Eliminate number name
  
  

  # Group b
  
  # If sub.data is not empty
  if(length(sub.data)>0){
    
    # Calculate Jaccard index in Loop with ifs
    for(n in 1:length(sub.data)){
      
      # Get list of sub objects
      sub.ls <- mget(ls(pattern = "sub.*"))                        # All sub objects
      sub.ls.data   <- mget(ls(sub.ls,pattern = "dataloop.b.*"))   # Sub rows
      sub.ls.numrow <- mget(ls(sub.ls,pattern = "numrow.sub.*"))   # Sub rows
      sub.ls.numcol <- mget(ls(sub.ls,pattern = "numcol.sub.*"))   # sub cols
      sub.ls.mean   <- mget(ls(sub.ls,pattern = "mean.sub.*"))     # mean
      sub.ls.w      <- mget(ls(sub.ls,pattern = "wgt.sub."))       # Weight
      
      # Extract last part of object name
      sub.name <- substrRight(c(names(sub.data)[n]),5)
      
      # Generate empty Jaccard
      #jaccard.b <- NA 
      
      # If sub.data is not empty
      if(length(sub.data)>0 & sub.ls.mean[n]>0){
        # Begin ifs 
        if(sub.ls.numrow[n]>2 ){    #          # If 2+ rows and non-zero content
          print(sub.name)[n]                                     # Print data name
          print(sub.ls.mean[n])                                  # Print mean
          # Jaccard
          jack.b <- betastatjac(as.data.frame(sub.ls.data[n]))   # Calculate Jaccard
          jack.b <- jack.b[4]                                    # Get similarity index
          names(jack.b) <- NULL                                  # Clear column names
          print(jack.b)                                          # Print Jaccard
          assign(paste0("jaccard.b.", sub.name), jack.b)         # assign name 
          #Weights
          wgt <- sub.ls.w[n]
          wgt <- as.data.frame(wgt)
          assign(paste0("weight.b.", sub.name), wgt)      
        }
        else{NA}
      }
    }
  }
  
  
  
  
  

  # F) Calculate weights
  

  # Get Jaccard index for each data segment
  jack.ls <- mget(ls(pattern = "jaccard."))   # Sub rows
  jack.ls <- as.data.frame(jack.ls)
  jack.ls
  
  # Get weights for a
  temp.w.a <- c(numrow.a/numrow)
  temp.w.a
  

  # Get weights for bs 
  
  # Get weights for all bs
  temp.w.b <- mget(ls(pattern = "weight.b."))
  
  # If temp.w.b is not empty, then calculate B's weight
  if(length(temp.w.b)>0 ){   # & sub.ls.mean[n]>0
    # Extract b weights
    temp.w.b <- as.numeric(temp.w.b[[1]])  # Convert b weight to numeric
    temp.w.b                               # Print b weight
    # Create weights Matrix of all weights
    temp.w.all <- mget(ls(pattern = "^temp.w.*"))
    temp.w.all <- unlist(temp.w.all)
    temp.w.all 
    # Calculate Jaccard Weighted Average
    jaccard.w<-weighted.mean(jack.ls,temp.w.all)
    jaccard.w
  }else{
    NA #do nothing
  }
  
  
  
  

  # G) Generate output
  # Save weighted jaccard 
  ifelse(length(temp.w.b)==0, 
         # Assign Jaccard 
         assign(paste0("J.AUC_", i), jaccard.a),
         # Assign weighted jaccard
         ifelse(length(temp.w.b)>0 | numrow.b==0, 
                # Assign weighted jaccard
                assign(paste0("J.AUC_", i), jaccard.w), 
                # Else zero for single column
                assign(paste0("J.AUC_", i), 0))
  ) # End if 
  
  
  

  # F) Return output 
  #label.jac
  output   <- mget(ls(pattern = "J.AUC_"))
  output
  #return(output)
  
  
} # End of loop





# Special Cases for 1988
for (i in c(1988)) {
  
  # seq.y
  # i <- 1993
  
  # CONTENT
  # A) Remove all the little components
  # B) Generate subset by year
  # C) Eliminate rows with NAs
  # D) Reduce dimentionality of rows with NAs
  # E) Calculate Jaccard index
  # F) Calculate weights
  # G) Generate output  
  

  # A) Remove all the little components
  
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  if(exists("temp.data")==TRUE){rm(temp.data)}else{NA}
  if(exists("sub.name")==TRUE){rm(sub.name)}else{NA}
  rm(list=ls(pattern="sub.ls"))
  rm(list=ls(pattern="dataloop.b."))
  rm(list=ls(pattern="dataloop.b.sub."))
  rm(list=ls(pattern="^jaccard."))
  if(exists("jack.b")==TRUE){rm(jack.b)}else{NA}
  rm(list=ls(pattern="^weight.b."))
  rm(list=ls(pattern="^wgt.sub."))
  rm(list=ls(pattern="^w."))
  rm(list=ls(pattern="^temp.w."))
  rm(list=ls(pattern="^numrow."))
  
  

  # B) Generate subset by year
  dataloop <- subset(ALL.AUC, year==i)
  # Eliminate columns with all NAs
  dataloop <- Filter(function(dataloop)!all(is.na(dataloop)), dataloop)
  dim(dataloop)
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  # Extract variables per year
  V <- names(dataloop)
  # Generate a "V" object per year containing variable names
  assign(paste0("V.AUC_", i), V)
  # Get n rows
  numrow <-nrow(dataloop)
  numcol <-ncol(dataloop)
  
  

  # C) Eliminate rows with NAs
  
  dataloop.a <- NA
  dataloop.a <- na.omit(dataloop) # <<<< eliminates row if there is a NA
  dim(dataloop.a)
  # Count number of columns and rows
  numcol.a <-ncol(dataloop.a)
  numrow.a <-nrow(dataloop.a)
  numrow.a
  # This is a special case wint  single variable
  # and numrow.a==numrow
  
  

  # D) Reduce dimentionality of rows with NAs
  
  dataloop.b <- NA
  dataloop.b <- dataloop[rowSums(is.na(dataloop)) > 0, ]     # <<<< Keep all rows with at least one NA
  dim(dataloop.b)
  # Count number of columns and rows
  numrow.b <-nrow(dataloop.b)
  numrow.b
  
  # # Unite database as character
  # dataloop.b[] <- lapply(dataloop.b, as.character)
  # dataloop.b <- dataloop.b %>% unite("split", sep = "_", na.rm = TRUE)
  # dim(dataloop.b)
  # 
  # # Generate groups object
  # dataloop.b$lenght <- nchar(dataloop.b$split)            # Get length
  # dataloop.b<- subset(dataloop.b, lenght>=3)              # Subset larger than one annotation
  # 
  # # Subset data by groups by groups into a list object
  # for(x in unique(dataloop.b$lenght)){
  #   # Subset data
  #   assign(paste0("dataloop.b.sub.",x),subset(dataloop.b, lenght==x, select="split")) 
  # }
  # 
  # # Get list of dataloop.b.# data frames
  # sub.data <- mget(ls(pattern = "dataloop.b.sub.*"))
  # sub.data
  # 
  # # Create function to extract last element from string
  # substrRight <- function(x, n){
  #   sapply(x, function(xx)
  #     substr(xx, (nchar(xx)-n+1), nchar(xx)))}
  # 
  # 
  # # If sub.data is not empty
  # if(length(sub.data)>0){
  #   # Extract data from list into data frame
  #   for(n in 1:length(sub.data)){
  #     # Extract last part of object name
  #     sub.name <- substrRight(c(names(sub.data)[n]),5)
  #     # Create data frame
  #     temp.data <- as.data.frame(sub.data[[n]])
  #     # Split columns by _
  #     temp.data <- as.data.frame(do.call('rbind', strsplit(as.character(temp.data$split),'_')))
  #     # Convert to data frame and numeric
  #     temp.data <- data.frame(temp.data)
  #     temp.data <- temp.data %>% mutate_all(as.numeric) %>% as.data.frame()
  #     # Get row and column names
  #     assign(paste0("numcol.",sub.name), ncol(temp.data))
  #     assign(paste0("numrow.",sub.name), nrow(temp.data))
  #     assign(paste0("mean.",sub.name), mean(colMeans(temp.data)))
  #     assign(paste0("wgt.",sub.name), nrow(temp.data)/numrow)
  #     # Export object (Good one) 
  #     assign(paste0(names(sub.data)[n]), data.frame(temp.data))
  #   }}
  
  

  # E) Calculate Jaccard index
  

  # # Group a
  # jaccard.a <-NA
  # jaccard.a <- betastatjac(dataloop.a)[4]  # Calculate Jaccard
  # names(jaccard.a) <- NULL                 # Eliminate number name
  # jaccard.a
  

  # Group b
  
  # # If sub.data is not empty
  # if(length(sub.data)>0){
  #   
  #   # Calculate Jaccard index in Loop with ifs
  #   for(n in 1:length(sub.data)){
  #     
  #     # Get list of sub objects
  #     sub.ls <- mget(ls(pattern = "sub.*"))                        # All sub objects
  #     sub.ls.data   <- mget(ls(sub.ls,pattern = "dataloop.b.*"))   # Sub rows
  #     sub.ls.numrow <- mget(ls(sub.ls,pattern = "numrow.sub.*"))   # Sub rows
  #     sub.ls.numcol <- mget(ls(sub.ls,pattern = "numcol.sub.*"))   # sub cols
  #     sub.ls.mean   <- mget(ls(sub.ls,pattern = "mean.sub.*"))     # mean
  #     sub.ls.w      <- mget(ls(sub.ls,pattern = "wgt.sub."))       # Weight
  #     
  #     # Extract last part of object name
  #     sub.name <- substrRight(c(names(sub.data)[n]),5)
  #     
  #     # Generate empty Jaccard
  #     #jaccard.b <- NA 
  #     
  #     # If sub.data is not empty
  #     if(length(sub.data)>0 & sub.ls.mean[n]>0){
  #       # Begin ifs 
  #       if(sub.ls.numrow[n]>2 ){    #          # If 2+ rows and non-zero content
  #         print(sub.name)[n]                                     # Print data name
  #         print(sub.ls.mean[n])                                  # Print mean
  #         # Jaccard
  #         jack.b <- betastatjac(as.data.frame(sub.ls.data[n]))   # Calculate Jaccard
  #         jack.b <- jack.b[4]                                    # Get similarity index
  #         names(jack.b) <- NULL                                  # Clear column names
  #         print(jack.b)                                          # Print Jaccard
  #         assign(paste0("jaccard.b.", sub.name), jack.b)         # assign name 
  #         #Weights
  #         wgt <- sub.ls.w[n]
  #         wgt <- as.data.frame(wgt)
  #         assign(paste0("weight.b.", sub.name), wgt)      
  #       }
  #       else{NA}
  #     }
  #   }
  # }
  
  
  
  # F) Calculate weights
  

  # # Get Jaccard index for each data segment
  # jack.ls <- mget(ls(pattern = "jaccard."))   # Sub rows
  # jack.ls <- as.data.frame(jack.ls)
  # jack.ls
  # 
  # # Get weights for a
  # temp.w.a <- c(numrow.a/numrow)
  # temp.w.a
  

  # # Get weights for bs
  # 
  # # Get weights for all bs
  # temp.w.b <- mget(ls(pattern = "weight.b."))
  
  # # If temp.w.b is not empty, then calculate B's weight
  # if(length(temp.w.b)>0 ){   # & sub.ls.mean[n]>0
  #   # Extract b weights
  #   temp.w.b <- as.numeric(temp.w.b[[1]])  # Convert b weight to numeric
  #   temp.w.b                               # Print b weight
  #   # Create weights Matrix of all weights
  #   temp.w.all <- mget(ls(pattern = "^temp.w.*"))
  #   temp.w.all <- unlist(temp.w.all)
  #   temp.w.all 
  #   # Calculate Jaccard Weighted Average
  #   jaccard.w<-weighted.mean(jack.ls,temp.w.all)
  #   jaccard.w
  # }else{
  #   NA #do nothing
  # }
  # 
  
  
  

  # # G) Generate output
  # # Save weighted jaccard 
  # ifelse(length(temp.w.b)==0, 
  #        # Assign Jaccard 
  #        assign(paste0("J.AUC_", i), jaccard.a),
  #        # Assign weighted jaccard
  #        ifelse(length(temp.w.b)>0 | numrow.b==0, 
  #               # Assign weighted jaccard
  #               assign(paste0("J.AUC_", i), jaccard.w), 
  #               # Else zero for single column
  #               assign(paste0("J.AUC_", i), 0))
  # ) # End if 
  
  # IF there is only one variable in dataloop.a
  ifelse(dim(dataloop.a)[2]==1,
         # Assign zero for single column
         assign(paste0("J.AUC_", i), 0),
         NA
  )
  
  

  # F) Return output 
  #label.jac
  # output   <- mget(ls(pattern = "J.ELN_"))
  # output
  #return(output)
  
  
  
} # End of loop





# Special Cases for 1989,1990,1991,1992
for (i in c(1989,1990,1991,1992)) {
  
  # seq.y
  # i <- 1993
  

  # CONTENT
  # A) Remove all the little components
  # B) Generate subset by year
  # C) Eliminate rows with NAs
  # D) Reduce dimentionality of rows with NAs
  # E) Calculate Jaccard index
  # F) Calculate weights
  # G) Generate output  
  

  # A) Remove all the little components
  
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  if(exists("temp.data")==TRUE){rm(temp.data)}else{NA}
  if(exists("sub.name")==TRUE){rm(sub.name)}else{NA}
  rm(list=ls(pattern="sub.ls"))
  rm(list=ls(pattern="dataloop.b."))
  rm(list=ls(pattern="dataloop.b.sub."))
  rm(list=ls(pattern="^jaccard."))
  if(exists("jack.b")==TRUE){rm(jack.b)}else{NA}
  rm(list=ls(pattern="^weight.b."))
  rm(list=ls(pattern="^wgt.sub."))
  rm(list=ls(pattern="^w."))
  rm(list=ls(pattern="^temp.w."))
  rm(list=ls(pattern="^numrow."))
  
  

  # B) Generate subset by year
  dataloop <- subset(ALL.AUC, year==i)
  # Eliminate columns with all NAs
  dataloop <- Filter(function(dataloop)!all(is.na(dataloop)), dataloop)
  dim(dataloop)
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  # Extract variables per year
  V <- names(dataloop)
  # Generate a "V" object per year containing variable names
  assign(paste0("V.AUC_", i), V)
  # Get n rows
  numrow <-nrow(dataloop)
  numcol <-ncol(dataloop)
  
  

  # C) Eliminate rows with NAs
  
  dataloop.a <- NA
  dataloop.a <- na.omit(dataloop) # <<<< eliminates row if there is a NA
  dim(dataloop.a)
  # Count number of columns and rows
  numcol.a <-ncol(dataloop.a)
  numrow.a <-nrow(dataloop.a)
  numrow.a
  # This is a special case wint  single variable
  # and numrow.a==numrow
  
  

  # D) Reduce dimentionality of rows with NAs
  
  dataloop.b <- NA
  dataloop.b <- dataloop[rowSums(is.na(dataloop)) > 0, ]     # <<<< Keep all rows with at least one NA
  dim(dataloop.b)
  # Count number of columns and rows
  numrow.b <-nrow(dataloop.b)
  numrow.b
  
  # # Unite database as character
  # dataloop.b[] <- lapply(dataloop.b, as.character)
  # dataloop.b <- dataloop.b %>% unite("split", sep = "_", na.rm = TRUE)
  # dim(dataloop.b)
  # 
  # # Generate groups object
  # dataloop.b$lenght <- nchar(dataloop.b$split)            # Get length
  # dataloop.b<- subset(dataloop.b, lenght>=3)              # Subset larger than one annotation
  # 
  # # Subset data by groups by groups into a list object
  # for(x in unique(dataloop.b$lenght)){
  #   # Subset data
  #   assign(paste0("dataloop.b.sub.",x),subset(dataloop.b, lenght==x, select="split")) 
  # }
  # 
  # # Get list of dataloop.b.# data frames
  # sub.data <- mget(ls(pattern = "dataloop.b.sub.*"))
  # sub.data
  # 
  # # Create function to extract last element from string
  # substrRight <- function(x, n){
  #   sapply(x, function(xx)
  #     substr(xx, (nchar(xx)-n+1), nchar(xx)))}
  # 
  # 
  # # If sub.data is not empty
  # if(length(sub.data)>0){
  #   # Extract data from list into data frame
  #   for(n in 1:length(sub.data)){
  #     # Extract last part of object name
  #     sub.name <- substrRight(c(names(sub.data)[n]),5)
  #     # Create data frame
  #     temp.data <- as.data.frame(sub.data[[n]])
  #     # Split columns by _
  #     temp.data <- as.data.frame(do.call('rbind', strsplit(as.character(temp.data$split),'_')))
  #     # Convert to data frame and numeric
  #     temp.data <- data.frame(temp.data)
  #     temp.data <- temp.data %>% mutate_all(as.numeric) %>% as.data.frame()
  #     # Get row and column names
  #     assign(paste0("numcol.",sub.name), ncol(temp.data))
  #     assign(paste0("numrow.",sub.name), nrow(temp.data))
  #     assign(paste0("mean.",sub.name), mean(colMeans(temp.data)))
  #     assign(paste0("wgt.",sub.name), nrow(temp.data)/numrow)
  #     # Export object (Good one) 
  #     assign(paste0(names(sub.data)[n]), data.frame(temp.data))
  #   }}
  
  

  # E) Calculate Jaccard index
  

  # # Group a
  # jaccard.a <-NA
  # jaccard.a <- betastatjac(dataloop.a)[4]  # Calculate Jaccard
  # names(jaccard.a) <- NULL                 # Eliminate number name
  # jaccard.a
  

  # Group b
  
  # # If sub.data is not empty
  # if(length(sub.data)>0){
  #   
  #   # Calculate Jaccard index in Loop with ifs
  #   for(n in 1:length(sub.data)){
  #     
  #     # Get list of sub objects
  #     sub.ls <- mget(ls(pattern = "sub.*"))                        # All sub objects
  #     sub.ls.data   <- mget(ls(sub.ls,pattern = "dataloop.b.*"))   # Sub rows
  #     sub.ls.numrow <- mget(ls(sub.ls,pattern = "numrow.sub.*"))   # Sub rows
  #     sub.ls.numcol <- mget(ls(sub.ls,pattern = "numcol.sub.*"))   # sub cols
  #     sub.ls.mean   <- mget(ls(sub.ls,pattern = "mean.sub.*"))     # mean
  #     sub.ls.w      <- mget(ls(sub.ls,pattern = "wgt.sub."))       # Weight
  #     
  #     # Extract last part of object name
  #     sub.name <- substrRight(c(names(sub.data)[n]),5)
  #     
  #     # Generate empty Jaccard
  #     #jaccard.b <- NA 
  #     
  #     # If sub.data is not empty
  #     if(length(sub.data)>0 & sub.ls.mean[n]>0){
  #       # Begin ifs 
  #       if(sub.ls.numrow[n]>2 ){    #          # If 2+ rows and non-zero content
  #         print(sub.name)[n]                                     # Print data name
  #         print(sub.ls.mean[n])                                  # Print mean
  #         # Jaccard
  #         jack.b <- betastatjac(as.data.frame(sub.ls.data[n]))   # Calculate Jaccard
  #         jack.b <- jack.b[4]                                    # Get similarity index
  #         names(jack.b) <- NULL                                  # Clear column names
  #         print(jack.b)                                          # Print Jaccard
  #         assign(paste0("jaccard.b.", sub.name), jack.b)         # assign name 
  #         #Weights
  #         wgt <- sub.ls.w[n]
  #         wgt <- as.data.frame(wgt)
  #         assign(paste0("weight.b.", sub.name), wgt)      
  #       }
  #       else{NA}
  #     }
  #   }
  # }
  
  
  
  # F) Calculate weights
  
  # # Get Jaccard index for each data segment
  # jack.ls <- mget(ls(pattern = "jaccard."))   # Sub rows
  # jack.ls <- as.data.frame(jack.ls)
  # jack.ls
  # 
  # # Get weights for a
  # temp.w.a <- c(numrow.a/numrow)
  # temp.w.a
  # 

  # # Get weights for bs
  # 
  # # Get weights for all bs
  # temp.w.b <- mget(ls(pattern = "weight.b."))
  
  # # If temp.w.b is not empty, then calculate B's weight
  # if(length(temp.w.b)>0 ){   # & sub.ls.mean[n]>0
  #   # Extract b weights
  #   temp.w.b <- as.numeric(temp.w.b[[1]])  # Convert b weight to numeric
  #   temp.w.b                               # Print b weight
  #   # Create weights Matrix of all weights
  #   temp.w.all <- mget(ls(pattern = "^temp.w.*"))
  #   temp.w.all <- unlist(temp.w.all)
  #   temp.w.all 
  #   # Calculate Jaccard Weighted Average
  #   jaccard.w<-weighted.mean(jack.ls,temp.w.all)
  #   jaccard.w
  # }else{
  #   NA #do nothing
  # }
  # 
  
  
  

  # # G) Generate output
  # # Save weighted jaccard 
  # ifelse(length(temp.w.b)==0, 
  #        # Assign Jaccard 
  #        assign(paste0("J.ELN_", i), jaccard.a),
  #        # Assign weighted jaccard
  #        ifelse(length(temp.w.b)>0 | numrow.b==0, 
  #               # Assign weighted jaccard
  #               assign(paste0("J.ELN_", i), jaccard.w), 
  #               # Else zero for single column
  #               assign(paste0("J.ELN_", i), 0))
  # ) # End if 
  
  # IF there is only one variable in dataloop.a
  ifelse(dim(dataloop.a)[2]>=1,
         # Assign zero for single column
         assign(paste0("J.AUC_", i), 0),
         NA
  )
  
  
  
  # F) Return output 
  #label.jac
  # output   <- mget(ls(pattern = "J.ELN_"))
  # output
  #return(output)
  
  
} # End of loop





# Special Cases for 2006, 2007, 2008
for (i in c(2006,2007,2008)) {
  
  # seq.y
  # i <- 1993
  

  # CONTENT
  # A) Remove all the little components
  # B) Generate subset by year
  # C) Eliminate rows with NAs
  # D) Reduce dimentionality of rows with NAs
  # E) Calculate Jaccard index
  # F) Calculate weights
  # G) Generate output  
  

  # A) Remove all the little components
  
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  if(exists("temp.data")==TRUE){rm(temp.data)}else{NA}
  if(exists("sub.name")==TRUE){rm(sub.name)}else{NA}
  rm(list=ls(pattern="sub.ls"))
  rm(list=ls(pattern="dataloop.b."))
  rm(list=ls(pattern="dataloop.b.sub."))
  rm(list=ls(pattern="^jaccard."))
  if(exists("jack.b")==TRUE){rm(jack.b)}else{NA}
  rm(list=ls(pattern="^weight.b."))
  rm(list=ls(pattern="^wgt.sub."))
  rm(list=ls(pattern="^w."))
  rm(list=ls(pattern="^temp.w."))
  rm(list=ls(pattern="^numrow."))
  
  

  # B) Generate subset by year
  dataloop <- subset(ALL.AUC, year==i)
  # Eliminate columns with all NAs
  dataloop <- Filter(function(dataloop)!all(is.na(dataloop)), dataloop)
  dim(dataloop)
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  # Extract variables per year
  V <- names(dataloop)
  # Generate a "V" object per year containing variable names
  assign(paste0("V.AUC_", i), V)
  # Get n rows
  numrow <-nrow(dataloop)
  numcol <-ncol(dataloop)
  
  

  # C) Eliminate rows with NAs
  
  dataloop.a <- NA
  dataloop.a <- na.omit(dataloop) # <<<< eliminates row if there is a NA
  dim(dataloop.a)
  # Count number of columns and rows
  numcol.a <-ncol(dataloop.a)
  numrow.a <-nrow(dataloop.a)
  numrow.a
  
  
  

  # D) Reduce dimentionality of rows with NAs
  
  dataloop.b <- NA
  dataloop.b <- dataloop[rowSums(is.na(dataloop)) > 0, ]     # <<<< Keep all rows with at least one NA
  dim(dataloop.b)
  # Count number of columns and rows
  numrow.b <-nrow(dataloop.b)
  numrow.b
  
  # Unite database as character
  dataloop.b[] <- lapply(dataloop.b, as.character)
  dataloop.b <- dataloop.b %>% unite("split", sep = "_", na.rm = TRUE)
  dim(dataloop.b)
  
  # Generate groups object
  dataloop.b$lenght <- nchar(dataloop.b$split)            # Get length
  dataloop.b<- subset(dataloop.b, lenght>=3)              # Subset larger than one annotation
  
  # Subset data by groups by groups into a list object
  for(x in unique(dataloop.b$lenght)){
    # Subset data
    assign(paste0("dataloop.b.sub.",x),subset(dataloop.b, lenght==x, select="split"))
  }
  
  # Get list of dataloop.b.# data frames
  sub.data <- mget(ls(pattern = "dataloop.b.sub.*"))
  sub.data
  
  # Create function to extract last element from string
  substrRight <- function(x, n){
    sapply(x, function(xx)
      substr(xx, (nchar(xx)-n+1), nchar(xx)))}
  
  
  # If sub.data is not empty
  if(length(sub.data)>0){
    # Extract data from list into data frame
    for(n in 1:length(sub.data)){
      # Extract last part of object name
      sub.name <- substrRight(c(names(sub.data)[n]),5)
      # Create data frame
      temp.data <- as.data.frame(sub.data[[n]])
      # Split columns by _
      temp.data <- as.data.frame(do.call('rbind', strsplit(as.character(temp.data$split),'_')))
      # Convert to data frame and numeric
      temp.data <- data.frame(temp.data)
      temp.data <- temp.data %>% mutate_all(as.numeric) %>% as.data.frame()
      # Get row and column names
      assign(paste0("numcol.",sub.name), ncol(temp.data))
      assign(paste0("numrow.",sub.name), nrow(temp.data))
      assign(paste0("mean.",sub.name), mean(colMeans(temp.data)))
      assign(paste0("wgt.",sub.name), nrow(temp.data)/numrow)
      # Export object (Good one)
      assign(paste0(names(sub.data)[n]), data.frame(temp.data))
    }}
  
  

  # E) Calculate Jaccard index
  

  # Group a
  jaccard.a <-NA
  jaccard.a <- betastatjac(dataloop.a)[4]  # Calculate Jaccard
  names(jaccard.a) <- NULL                 # Eliminate number name
  jaccard.a
  

  # Group b
  
  # # If sub.data is not empty
  # if(length(sub.data)>0){
  # 
  #   # Calculate Jaccard index in Loop with ifs
  #   for(n in 1:length(sub.data)){
  # 
  #     # Get list of sub objects
  #     sub.ls <- mget(ls(pattern = "sub.*"))                        # All sub objects
  #     sub.ls.data   <- mget(ls(sub.ls,pattern = "dataloop.b.*"))   # Sub rows
  #     sub.ls.numrow <- mget(ls(sub.ls,pattern = "numrow.sub.*"))   # Sub rows
  #     sub.ls.numcol <- mget(ls(sub.ls,pattern = "numcol.sub.*"))   # sub cols
  #     sub.ls.mean   <- mget(ls(sub.ls,pattern = "mean.sub.*"))     # mean
  #     sub.ls.w      <- mget(ls(sub.ls,pattern = "wgt.sub."))       # Weight
  # 
  #     # Extract last part of object name
  #     sub.name <- substrRight(c(names(sub.data)[n]),5)
  # 
  #     # Generate empty Jaccard
  #     #jaccard.b <- NA
  # 
  #     # If sub.data is not empty
  #     if(length(sub.data)>0 & sub.ls.mean[n]>0){
  #       # Begin ifs
  #       if(sub.ls.numrow[n]>2 ){    #          # If 2+ rows and non-zero content
  #         print(sub.name)[n]                                     # Print data name
  #         print(sub.ls.mean[n])                                  # Print mean
  #         # Jaccard
  #         jack.b <- betastatjac(as.data.frame(sub.ls.data[n]))   # Calculate Jaccard
  #         jack.b <- jack.b[4]                                    # Get similarity index
  #         names(jack.b) <- NULL                                  # Clear column names
  #         print(jack.b)                                          # Print Jaccard
  #         assign(paste0("jaccard.b.", sub.name), jack.b)         # assign name
  #         #Weights
  #         wgt <- sub.ls.w[n]
  #         wgt <- as.data.frame(wgt)
  #         assign(paste0("weight.b.", sub.name), wgt)
  #       }
  #       else{NA}
  #     }
  #   }
  # }
  
  

  # F) Calculate weights
  

  # Get Jaccard index for each data segment
  jack.ls <- mget(ls(pattern = "jaccard."))   # Sub rows
  jack.ls <- as.data.frame(jack.ls)
  jack.ls
  
  # Get weights for a
  temp.w.a <- c(numrow.a/numrow)
  temp.w.a
  

  # Get weights for bs
  
  # Get weights for all bs
  temp.w.b <- mget(ls(pattern = "weight.b."))
  
  # If temp.w.b is not empty, then calculate B's weight
  if(length(temp.w.b)>0 ){   # & sub.ls.mean[n]>0
    # Extract b weights
    temp.w.b <- as.numeric(temp.w.b[[1]])  # Convert b weight to numeric
    temp.w.b                               # Print b weight
    # Create weights Matrix of all weights
    temp.w.all <- mget(ls(pattern = "^temp.w.*"))
    temp.w.all <- unlist(temp.w.all)
    temp.w.all
    # Calculate Jaccard Weighted Average
    jaccard.w<-weighted.mean(jack.ls,temp.w.all)
    jaccard.w
  }else{
    NA #do nothing
  }
  

  # G) Generate output
  # Save weighted jaccard
  ifelse(length(temp.w.b)==0,
         # Assign Jaccard
         assign(paste0("J.AUC_", i), jaccard.a),
         # Assign weighted jaccard
         ifelse(length(temp.w.b)>0 | numrow.b==0,
                # Assign weighted jaccard
                assign(paste0("J.AUC_", i), jaccard.w),
                # Else zero for single column
                assign(paste0("J.AUC_", i), 0))
  ) # End if
  
  # # IF there is only one variable in dataloop.a
  # ifelse(dim(dataloop.a)[2]==1,
  #        # Assign zero for single column
  #        assign(paste0("J.AUC_", i), 0),
  #        NA
  #        )
  
  
  # F) Return output 
  #label.jac
  # output   <- mget(ls(pattern = "J.AUC_"))
  # output
  #return(output)
  

  
} # End of loop








# 10. LOCAL JACCARD SIMILARITY PER ARMED GROUP  #############################################################

## Visualization  ----------------------------


# Replicate Figure 5b Local Jaccard Similarity by armed group   
  
# Generate variables for Jaccard dataframe 
y <- c(1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
J.FARC <- c(J.FARC_1988, J.FARC_1989, J.FARC_1990, J.FARC_1991, J.FARC_1992, J.FARC_1993, J.FARC_1994, J.FARC_1995, J.FARC_1996, J.FARC_1997, J.FARC_1998, J.FARC_1999, J.FARC_2000, J.FARC_2001, J.FARC_2002, J.FARC_2003, J.FARC_2004, J.FARC_2005, J.FARC_2006, J.FARC_2007, J.FARC_2008, J.FARC_2009, J.FARC_2010, J.FARC_2011, J.FARC_2012, J.FARC_2013, J.FARC_2014, J.FARC_2015, J.FARC_2016, J.FARC_2017)
J.ELN <- c(J.ELN_1988, J.ELN_1989, J.ELN_1990, J.ELN_1991, J.ELN_1992, J.ELN_1993, J.ELN_1994, J.ELN_1995, J.ELN_1996, J.ELN_1997, J.ELN_1998, J.ELN_1999, J.ELN_2000, J.ELN_2001, J.ELN_2002, J.ELN_2003, J.ELN_2004, J.ELN_2005, J.ELN_2006, J.ELN_2007, J.ELN_2008, J.ELN_2009, J.ELN_2010, J.ELN_2011, J.ELN_2012, J.ELN_2013, J.ELN_2014, J.ELN_2015, J.ELN_2016, J.ELN_2017)
J.AUC <- c(J.AUC_1988, J.AUC_1989, J.AUC_1990, J.AUC_1991, J.AUC_1992, J.AUC_1993, J.AUC_1994, J.AUC_1995, J.AUC_1996, J.AUC_1997, J.AUC_1998, J.AUC_1999, J.AUC_2000, J.AUC_2001, J.AUC_2002, J.AUC_2003, J.AUC_2004, J.AUC_2005, J.AUC_2006, J.AUC_2007, J.AUC_2008, J.AUC_2009, J.AUC_2010, J.AUC_2011, J.AUC_2012, J.AUC_2013, J.AUC_2014, J.AUC_2015, J.AUC_2016, J.AUC_2017)

# Create labs
VFARC.labs<-c(paste(V.FARC_1988, collapse=", "), paste(V.FARC_1989, collapse=", "), paste(V.FARC_1990, collapse=", "), paste(V.FARC_1991, collapse=", "), paste(V.FARC_1992, collapse=", "), paste(V.FARC_1993, collapse=", "), paste(V.FARC_1994, collapse=", "), paste(V.FARC_1995, collapse=", "), paste(V.FARC_1996, collapse=", "), paste(V.FARC_1997, collapse=", "), paste(V.FARC_1998, collapse=", "), paste(V.FARC_1999, collapse=", "), paste(V.FARC_2000, collapse=", "), paste(V.FARC_2001, collapse=", "), paste(V.FARC_2002, collapse=", "), paste(V.FARC_2003, collapse=", "), paste(V.FARC_2004, collapse=", "), paste(V.FARC_2005, collapse=", "), paste(V.FARC_2006, collapse=", "), paste(V.FARC_2007, collapse=", "), paste(V.FARC_2008, collapse=", "), paste(V.FARC_2009, collapse=", "), paste(V.FARC_2010, collapse=", "), paste(V.FARC_2011, collapse=", "), paste(V.FARC_2012, collapse=", "), paste(V.FARC_2013, collapse=", "), paste(V.FARC_2014, collapse=", "), paste(V.FARC_2015, collapse=", "), paste(V.FARC_2016, collapse=", "), paste(V.FARC_2017, collapse=", "))
VELN.labs<-c(paste(V.ELN_1988, collapse=", "), paste(V.ELN_1989, collapse=", "), paste(V.ELN_1990, collapse=", "), paste(V.ELN_1991, collapse=", "), paste(V.ELN_1992, collapse=", "), paste(V.ELN_1993, collapse=", "), paste(V.ELN_1994, collapse=", "), paste(V.ELN_1995, collapse=", "), paste(V.ELN_1996, collapse=", "), paste(V.ELN_1997, collapse=", "), paste(V.ELN_1998, collapse=", "), paste(V.ELN_1999, collapse=", "), paste(V.ELN_2000, collapse=", "), paste(V.ELN_2001, collapse=", "), paste(V.ELN_2002, collapse=", "), paste(V.ELN_2003, collapse=", "), paste(V.ELN_2004, collapse=", "), paste(V.ELN_2005, collapse=", "), paste(V.ELN_2006, collapse=", "), paste(V.ELN_2007, collapse=", "), paste(V.ELN_2008, collapse=", "), paste(V.ELN_2009, collapse=", "), paste(V.ELN_2010, collapse=", "), paste(V.ELN_2011, collapse=", "), paste(V.ELN_2012, collapse=", "), paste(V.ELN_2013, collapse=", "), paste(V.ELN_2014, collapse=", "), paste(V.ELN_2015, collapse=", "), paste(V.ELN_2016, collapse=", "), paste(V.ELN_2017, collapse=", ") )
VAUC.labs<-c(paste(V.AUC_1988, collapse=", "), paste(V.AUC_1989, collapse=", "), paste(V.AUC_1990, collapse=", "), paste(V.AUC_1991, collapse=", "), paste(V.AUC_1992, collapse=", "), paste(V.AUC_1993, collapse=", "), paste(V.AUC_1994, collapse=", "), paste(V.AUC_1995, collapse=", "), paste(V.AUC_1996, collapse=", "), paste(V.AUC_1997, collapse=", "), paste(V.AUC_1998, collapse=", "), paste(V.AUC_1999, collapse=", "), paste(V.AUC_2000, collapse=", "), paste(V.AUC_2001, collapse=", "), paste(V.AUC_2002, collapse=", "), paste(V.AUC_2003, collapse=", "), paste(V.AUC_2004, collapse=", "), paste(V.AUC_2005, collapse=", "), paste(V.AUC_2006, collapse=", "), paste(V.AUC_2007, collapse=", "), paste(V.AUC_2008, collapse=", "), paste(V.AUC_2009, collapse=", "), paste(V.AUC_2010, collapse=", "), paste(V.AUC_2011, collapse=", "), paste(V.AUC_2012, collapse=", "), paste(V.AUC_2013, collapse=", "), paste(V.AUC_2014, collapse=", "), paste(V.AUC_2015, collapse=", "), paste(V.AUC_2016, collapse=", "), paste(V.AUC_2017, collapse=", ") )

# Generate data frame
Jaccard.actors <- rbind(J.FARC, J.ELN)
Jaccard.actors <- rbind(Jaccard.actors, J.AUC)
Jaccard.actors <- data.frame(Jaccard.actors)
names(Jaccard.actors) <- c(y)

# Turn into matrix
mJaccard.actors <- as.matrix(Jaccard.actors)



## ggplot heatmap  ######################

hm.actors <- data.frame(sample = rep(colnames(Jaccard.actors), each = nrow(Jaccard.actors)),
                        probe = rownames(Jaccard.actors),
                        expression = unlist(Jaccard.actors),
                        stringsAsFactors = FALSE)

# Change values for FARC and ELN
hm.actors$probe[hm.actors$probe == "J.FARC"] <- "FARC"
hm.actors$probe[hm.actors$probe == "J.ELN"] <- "ELN"
hm.actors$probe[hm.actors$probe == "J.AUC"] <- "AUC"


hml.actors <-ggplot(hm.actors, aes(x = probe, y = sample, fill = expression)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_tile() +
  scale_fill_gradientn(colours = magma(10),limits=c(0,1), name = "Jaccard \n Index") +
  labs(x="", y="") +
  theme(axis.text.x = element_text(angle = 0), axis.ticks = element_blank()) +
  annotate("text", x = 1, y = 1:30, label = VAUC.labs, size =3, colour = "white") +
  annotate("text", x = 2, y = 1:30, label = VELN.labs, size =3, colour = "white") +
  annotate("text", x = 3, y = 1:30, label = VFARC.labs, size =3, colour = "white") +
  theme(legend.position="bottom", legend.direction = "horizontal") +
  theme(text = element_text(size = 14),legend.text = element_text(size = 9))

hml.actors

hml.actors <-  add_sub(hml.actors, "CD=CEDE; CL=Claudia Lopez; RC=Rutas del Conflicto; VI=ViPAA; RE=Restrepo, UC=UCDP", 
                       x=5.3, y=0.5, hjust = 5,
                       size =8)

# Show plot
ggdraw(hml.actors)  # The text looks odd in Rstudio, but looks nice in PDF

# Save graph
pdf("./graphs/jaccard_local/Jaccard_year_actors_2.pdf", width=5, height=7)
ggdraw(hml.actors)
dev.off()

png("./graphs/jaccard_local/Jaccard_year_actors_2.png")
ggdraw(hml.actors)
dev.off()




## Time Series Plot Jaccard Armed Group over time ###########################

hml.actors.t <- ggplot(data=hm.actors, aes(x=sample, y=expression, group=probe)) +
  geom_line(aes(color=probe),size=1) +
  ylim(0,1) + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(color='') + 
  theme(text = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab(element_blank()) + ylab("Jaccard index") +
  scale_x_discrete(breaks = seq(1988, 2018, by = 2))

ggdraw(hml.actors.t) 

# Save graph
pdf("./graphs/jaccard_time/Jaccard_year_actors_t_2.pdf", width=5, height=4)
ggdraw(hml.actors.t)
dev.off()

png("./graphs/jaccard_time/Jaccard_year_actors_t_ 2.png")
ggdraw(hml.actors.t)
dev.off()







## Discussion  ----------------------------


# AUC
Jaccard.actors.A<-Jaccard.actors[3,]

# 88-92 
mean(as.numeric(subset(Jaccard.actors.A,select =c("1988","1989","1990","1991","1992"))))
# 93-02
mean(as.numeric(subset(Jaccard.actors.A,select =c("1993","1994","1996","1996","1997","1998","1999","2000","2001","2002"))))
# 03-10
mean(as.numeric(subset(Jaccard.actors.A,select =c("2003","2004","2005","2006","2007","2008","2009","2010"))))
# 11-17
mean(as.numeric(subset(Jaccard.actors.A,select =c("2011","2012","2013","2014","2015","2016","2017"))))

min(Jaccard.actors.A)
max(Jaccard.actors.A)



# ELN
Jaccard.actors.E<-Jaccard.actors[2,]

# 88-10
mean(as.numeric(subset(Jaccard.actors.E,select =c("1988","1989","1990","1991","1992","1993","1994","1996","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010"))))
# 11-17
mean(as.numeric(subset(Jaccard.actors.E,select =c("2011","2012","2013","2014","2015","2016","2017"))))

min(Jaccard.actors.E)
max(Jaccard.actors.E)



# FARC
Jaccard.actors.F<-Jaccard.actors[1,]

mean(as.numeric(Jaccard.actors.F))
max(as.numeric(Jaccard.actors.F))

min(Jaccard.actors.F)
max(Jaccard.actors.F)








# 11. AVERAGE JACCARD SIMILARITY PER ARMED GROUP OVER TIME ##################################################

## Visualization  ----------------------------


# Replicate Figure 4b Average Jaccard Similarity by armed group  
  
# Calculate the means over time
J.FARC.mean<-mean(J.FARC)
J.ELN.mean<-mean(J.ELN)
J.AUC.mean<-mean(J.AUC)
J.FARC.se<-std.error(J.FARC)
J.ELN.se<-std.error(J.ELN)
J.AUC.se<-std.error(J.AUC)

# Round up to two decimals
J.FARC.mean<-round(J.FARC.mean, digits = 3)
J.ELN.mean<-round(J.ELN.mean, digits = 3)
J.AUC.mean<-round(J.AUC.mean, digits = 3)
J.FARC.se<-round(J.FARC.se, digits = 4)
J.ELN.se<-round(J.ELN.se, digits = 4)
J.AUC.se<-round(J.AUC.se, digits = 4)


# Generate database
J.actor.mean <- data.frame(group=c("FARC","ELN", "AUC"),
                           jaccard=c(J.FARC.mean, J.ELN.mean, J.AUC.mean),
                           se=c(J.FARC.se, J.ELN.se, J.AUC.se))

# Bar plot
c<-ggplot(data=J.actor.mean, aes(x=group, y=jaccard)) +
  geom_bar(stat="identity", fill="gray35") +
  geom_errorbar(aes(ymin=jaccard-se, ymax=jaccard+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +  
  scale_y_continuous(limits=c(0,1)) +
  labs(x="", y="Jaccard similarity") +
  geom_text(aes(label=jaccard), position=position_dodge(width=0.9), vjust=2.5, color="white") +
  theme_minimal() +
  theme(text = element_text(size = 14),legend.text = element_text(size = 9))
c

# Save graph
pdf("./graphs/jaccard_average/Jaccard_average_actor_2.pdf", width=3, height=3)
c
dev.off()

png("./graphs/jaccard_average/Jaccard_average_actor_2.png")
c
dev.off()




## Discussion  ----------------------------


# Subset data by type or group
hm.P <- subset(hm, probe=="Paramilitaries")
hm.G <- subset(hm, probe=="Guerrilla")
hm.actors.F <- subset(hm.actors, probe=="FARC")
hm.actors.E <- subset(hm.actors, probe=="ELN")
hm.actors.A <- subset(hm.actors, probe=="AUC")



# Guerrilla
# 1988-1992
mean(hm.G$expression[1:5])
# 1993-1999
mean(hm.G$expression[6:12])
# 2000-2010
mean(hm.G$expression[13:23])
# 2011-2017
mean(hm.G$expression[24:30])



# Paramilitaries
# 1988-1992 
mean(hm.P$expression[1:5])
# 1993-2005
mean(hm.P$expression[6:18])
# 2006-2010
mean(hm.P$expression[19:23])
# 2011-2017
mean(hm.P$expression[24:30])



# AUC
# 1988-1992  
mean(hm.actors.A$expression[c(1:5)])
# 1993-2002
mean(hm.actors.A$expression[6:15])
# 2003-2010 
mean(hm.actors.A$expression[c(16:23)])
# 2011-2017
mean(hm.actors.A$expression[24:30])



# ELN
# 1988-2010
mean(hm.actors.E$expression[1:23])
# 2011-2017
mean(hm.actors.E$expression[24:30])



# FARC
# min
min(hm.actors.F$expression)
# max
max(hm.actors.F$expression)








# 12. STATISTICAL ANALYSIS OF HOMICIDES #####################################################################


## 12.0. Get the data  ----------------------------



## Get the Colombia data 

COL_data <- read.dta("./data_final/Col_data.dta")
names(COL_data)


# Get the FARC, ELN, AUC data
AUC_data<-ALL.AUC.1
ELN_data<-ALL.ELN.1
FARC_data<-ALL.FARC.1

names(AUC_data)
names(AUC_data)[3]<-"AUC_VI"
names(AUC_data)[4]<-"AUC_CD"
names(AUC_data)[5]<-"AUC_RC"
names(AUC_data)[6]<-"AUC_RE"
names(AUC_data)[7]<-"AUC_UC"

names(ELN_data)
names(ELN_data)[3]<-"ELN_VI"
names(ELN_data)[4]<-"ELN_CD"
names(ELN_data)[5]<-"ELN_RC"
names(ELN_data)[6]<-"ELN_UC"

names(FARC_data)
names(FARC_data)[3]<-"FARC_VI"
names(FARC_data)[4]<-"FARC_CD"
names(FARC_data)[5]<-"FARC_RC"
names(FARC_data)[6]<-"FARC_RE"
names(FARC_data)[7]<-"FARC_UC"

# Check the dimensionality of the data
table(COL_data$year)
table(ALL.GP$year)
table(AUC_data$year)
table(ELN_data$year)
table(FARC_data$year)

# Deduplicate using "unique"
ALL.GP    <- unique(ALL.GP)
AUC_data  <- unique(AUC_data)
ELN_data  <- unique(ELN_data)
FARC_data <- unique(FARC_data)

# Merge all the  data
dim(COL_data)
table(COL_data$year)
COL_data <- left_join(COL_data,ALL.GP,by=c("mun", "year"))
COL_data <- left_join(COL_data,AUC_data,by=c("mun", "year"))
COL_data <- left_join(COL_data,ELN_data,by=c("mun", "year"))
COL_data <- left_join(COL_data,FARC_data,by=c("mun", "year"))
names(COL_data)
dim(COL_data)
table(COL_data$year)

# Generate all Paramilitaries if any other Para var = 1
COL_data$Paramilitaries_All <-0
COL_data$Paramilitaries_All[COL_data$Paramilitaries_VI==1] <- 1
COL_data$Paramilitaries_All[COL_data$Paramilitaries_CD==1] <- 1
COL_data$Paramilitaries_All[COL_data$Paramilitaries_CL==1] <- 1
COL_data$Paramilitaries_All[COL_data$Paramilitaries_IN==1] <- 1
COL_data$Paramilitaries_All[COL_data$Paramilitaries_RE==1] <- 1
COL_data$Paramilitaries_All[COL_data$Paramilitaries_UC==1] <- 1

# Make NA when all other Para vars are NA
COL_data$Paramilitaries_All[is.na(COL_data$Paramilitaries_VI) & is.na(COL_data$Paramilitaries_CD) & is.na(COL_data$Paramilitaries_CL) & is.na(COL_data$Paramilitaries_IN) & is.na(COL_data$Paramilitaries_RE) & is.na(COL_data$Paramilitaries_UC) ] <- NA
names(COL_data)


# Generate all Guerrilla if any other Guerrilla var = 1
COL_data$Guerrilla_All <-0
COL_data$Guerrilla_All[COL_data$Guerrilla_VI==1] <- 1
COL_data$Guerrilla_All[COL_data$Guerrilla_CD==1] <- 1
COL_data$Guerrilla_All[COL_data$Guerrilla_CL==1] <- 1
COL_data$Guerrilla_All[COL_data$Guerrilla_RE==1] <- 1
COL_data$Guerrilla_All[COL_data$Guerrilla_UC==1] <- 1


# Make NA when all other Guerrilla vars are NA
COL_data$Guerrilla_All[is.na(COL_data$Guerrilla_VI) & is.na(COL_data$Guerrilla_CD) & is.na(COL_data$Guerrilla_CL) & is.na(COL_data$Guerrilla_RE) & is.na(COL_data$Guerrilla_UC)]<-NA
names(COL_data)


# Generate all FARC if any other FARC var = 1
COL_data$FARC_All <-0
COL_data$FARC_All[COL_data$FRAC_VI==1] <- 1
COL_data$FARC_All[COL_data$FARC_CD==1] <- 1
COL_data$FARC_All[COL_data$FARC_RC==1] <- 1
COL_data$FARC_All[COL_data$FARC_RE==1] <- 1
COL_data$FARC_All[COL_data$FARC_UC==1] <- 1


# Make NA when all other FARC vars are NA
COL_data$FARC_All[is.na(COL_data$FRAC_VI) & is.na(COL_data$FARC_CD) & is.na(COL_data$FARC_RC) & is.na(COL_data$FARC_RE) & is.na(COL_data$FARC_UC)]<-NA
names(COL_data)


# Generate all ELN if any other ELN var = 1
COL_data$ELN_All <-0
COL_data$ELN_All[COL_data$ELN_VI==1] <- 1
COL_data$ELN_All[COL_data$ELN_CD==1] <- 1
COL_data$ELN_All[COL_data$ELN_RC==1] <- 1
COL_data$ELN_All[COL_data$ELN_UC==1] <- 1

# Make NA when all other ELN vars are NA
COL_data$ELN_All[is.na(COL_data$ELN_VI) & is.na(COL_data$ELN_CD) & is.na(COL_data$ELN_RC) & is.na(COL_data$ELN_UC)]<-NA
names(COL_data)


# Generate all AUC if any other AUC var = 1
COL_data$AUC_All <-0
COL_data$AUC_All[COL_data$AUC_VI==1] <- 1
COL_data$AUC_All[COL_data$AUC_CD==1] <- 1
COL_data$AUC_All[COL_data$AUC_RC==1] <- 1
COL_data$AUC_All[COL_data$AUC_RE==1] <- 1
COL_data$AUC_All[COL_data$AUC_UC==1] <- 1


# Make NA when all other AUC vars are NA
COL_data$AUC_All[is.na(COL_data$AUC_VI) & is.na(COL_data$AUC_CD) & is.na(COL_data$AUC_RC) & is.na(COL_data$AUC_RE) & is.na(COL_data$AUC_UC)]<-NA


# Show variable names
names(COL_data)

# Eliminate rows with empty municipalities
which(is.na(COL_data$mun))
COL_data<-subset(COL_data,COL_data$mun!="NA") 
which(is.na(COL_data$mun))




# Export data for statistical analysis

# Create ALL data frame
ALL <- subset(COL_data, select=c(mun, year, Paramilitaries_All, Guerrilla_All, FARC_All, ELN_All, AUC_All))

# Rename variables
names(ALL)[names(ALL) == "Paramilitaries_All"] <- "Paramilitaries"
names(ALL)[names(ALL) == "Guerrilla_All"] <- "Guerrilla"
names(ALL)[names(ALL) == "FARC_All"] <- "FARC"
names(ALL)[names(ALL) == "ELN_All"] <- "ELN"
names(ALL)[names(ALL) == "AUC_All"] <- "AUC"

# Check names
names(ALL)

write.dta(ALL, "./data_final/ALLall.dta")
#write.dta(ALL, "./taxation/replication/DATA/ALLall.dta")





# Export outcome variables
data.export <- subset(COL_data,select=c(mun,year,
                                     Paramilitaries_VI, Paramilitaries_CD, Paramilitaries_CL, Paramilitaries_IN, Paramilitaries_RE, Paramilitaries_UC, Paramilitaries_All, 
                                     Guerrilla_VI, Guerrilla_CD, Guerrilla_CL, Guerrilla_RE, Guerrilla_UC, Guerrilla_All, 
                                     FARC_VI, FARC_CD, FARC_RC, FARC_RE, FARC_UC, FARC_All,
                                     ELN_VI,  ELN_CD,  ELN_RC,  ELN_UC,  ELN_All,
                                     AUC_VI,  AUC_CD,  AUC_RC,  AUC_RE,  AUC_UC,  AUC_All))

write.csv2(data.export,"./data_final/data_actors.csv")
#write.csv2(data.export,"./Dube_price_shocks/data_actors.csv")




# Eliminate duplicates in COL_data from merge

names(COL_data)

# Tag duplicates
COL_data$dup<-duplicated(COL_data, incomparables=FALSE, fromLast=FALSE, nmax=1, by=key(c("mun", "year")))

# Checking duplicates
checktest<-subset(COL_data, select =c(mun,year,dup))
table(checktest$dup)

# Eliminate duplicates
COL_data<-subset(COL_data,dup=="FALSE")

# Eliminate if mun is NA
COL_data<-subset(COL_data,(!is.na(COL_data$mun))) 
which(is.na(COL_data$mun))

# Additional deduplication process
nrow(COL_data)
COL_data <- distinct(COL_data, mun, year, .keep_all= TRUE)
nrow(COL_data)





## 12.1. Descriptive statistics  ----------------------------


# This replicates Table 4 Descriptive statistics
  
descstar <- subset(COL_data,select=c(homicide_rate, 
                                     Paramilitaries_VI, Paramilitaries_CD, Paramilitaries_CL, Paramilitaries_IN, Paramilitaries_RE, Paramilitaries_UC, Paramilitaries_All, 
                                     Guerrilla_VI, Guerrilla_CD, Guerrilla_CL, Guerrilla_RE, Guerrilla_UC, Guerrilla_All, 
                                     FARC_VI, FARC_CD, FARC_RC, FARC_RE, FARC_UC, FARC_All,
                                     ELN_VI, ELN_CD, ELN_RC, ELN_UC, ELN_All,
                                     AUC_VI, AUC_CD, AUC_RC, AUC_RE, AUC_UC, AUC_All,
                                     cultivos2_log, oil_R, mines, elections, poplog, poplog_sqr, plan_colombia_mili, plan_colombia_eco, pastrana_peace, barco_peace, plan_patriota, medellin_offensive, pibR_pc_log, bases,  parques))

# stargazer(descstar, type = "text", title="Descriptive statistics", 
#           summary.stat = c("mean", "sd", "min", "max", "n"),
#           digits=1, out="./tables/desc.txt")

stargazer(descstar, type = "latex", title="Descriptive statistics", 
          summary.stat = c("mean", "sd", "min", "max", "n"),
          digits=1, out="./tables/desc.tex")





## 12.2. Analysis homicides by actor type  ----------------------------


### Run analysis -------------


# Replicate Table 16 Summary of Regression analysis by Type of Armed Actor  
# Panel 16a Paramilitaries
# Panel 16b Guerrilla
  
    
library(plm)

# Run the models
m.PVI  <-plm(homicide_rate ~ Paramilitaries_VI  + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year"))
m.PCD  <-plm(homicide_rate ~ Paramilitaries_CD  + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.PCL  <-plm(homicide_rate ~ Paramilitaries_CL  + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.PIN  <-plm(homicide_rate ~ Paramilitaries_IN  + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.PAll <-plm(homicide_rate ~ Paramilitaries_All + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.PRC  <-plm(homicide_rate ~ Paramilitaries_RC  + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.PRE  <-plm(homicide_rate ~ Paramilitaries_RE  + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.PUC  <-plm(homicide_rate ~ Paramilitaries_UC  + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 


m.GVI  <-plm(homicide_rate ~ Guerrilla_VI      + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.GCD  <-plm(homicide_rate ~ Guerrilla_CD      + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.GCL  <-plm(homicide_rate ~ Guerrilla_CL      + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.GAll <-plm(homicide_rate ~ Guerrilla_All     + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.GRC  <-plm(homicide_rate ~ Guerrilla_RC     + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.GRE  <-plm(homicide_rate ~ Guerrilla_RE     + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.GUC  <-plm(homicide_rate ~ Guerrilla_UC     + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 



# Table of Results Paramilitaries
# Panel 16a Paramilitaries

# stargazer(m.PVI, m.PCD, m.PRE, m.PCL, m.PIN, m.PRC, m.PUC, m.PAll,
#           type="text",
#           title="Regression Results", 
#           align=TRUE,
#           digits = 2,
#           omit.stat=c("f"),
#           out="./tables/results_P_v2.txt")


stargazer(m.PVI, m.PCD, m.PRE, m.PCL, m.PIN, m.PRC, m.PUC, m.PAll,
          type="latex",
          title="Regression Results", 
          align=TRUE,
          digits = 2,
          omit.stat=c("f"),
          out="./tables/results_P_v2.tex")



# Table of Results Guerrillas
# Panel 16b Guerrilla

# stargazer(m.GVI, m.GCD, m.GRE,  m.GCL, m.GRC, m.GUC, m.GAll,
#           type="text",
#           title="Regression Results", 
#           align=TRUE,
#           digits = 2,
#           omit.stat=c("f"),
#           out="./tables/results_G_v2.txt")

stargazer( m.GVI, m.GCD, m.GRE,  m.GCL, m.GRC, m.GUC, m.GAll,
           type="latex",
           title="Regression Results", 
           align=TRUE,
           digits = 2,
           omit.stat=c("f"),
           out="./tables/results_G_v2.tex")




### Plot coefficients Paramilitaries -------------

# Replicate Figure 6f Paramilitaries  
  
# Load packages
library(broom)       # To clean up data
library(ggplot2)     # Plot coefficients
library(dplyr)       # Subset data by row


# Get Number of observations
n.PVI  <- nobs(m.PVI)
n.PCD  <- nobs(m.PCD)
n.PCL  <- nobs(m.PCL)
n.PIN  <- nobs(m.PIN)
n.PRC  <- nobs(m.PRC)
n.PRE  <- nobs(m.PRE)
n.PUC  <- nobs(m.PUC)
n.PAll <- nobs(m.PAll)

# Get array of obs
n.P <- c(n.PVI,n.PCD,n.PCL,n.PIN,n.PRC,n.PRE,n.PUC,n.PAll)
n.P <- as.data.frame(n.P)


# Extract coefficients
g.PVI  <- tidy(m.PVI, conf.int = TRUE)
g.PCD  <- tidy(m.PCD, conf.int = TRUE)
g.PCL  <- tidy(m.PCL, conf.int = TRUE)
g.PIN  <- tidy(m.PIN, conf.int = TRUE)
g.PRC  <- tidy(m.PRC, conf.int = TRUE)
g.PRE  <- tidy(m.PRE, conf.int = TRUE)
g.PUC  <- tidy(m.PUC, conf.int = TRUE)
g.PAll <- tidy(m.PAll, conf.int = TRUE)

# Add model name to each data.frame
g.PVI   <-data.frame(g.PVI,Paramilitaries = "VI")
g.PCD   <-data.frame(g.PCD,Paramilitaries = "CD")
g.PCL   <-data.frame(g.PCL,Paramilitaries = "CL")
g.PIN   <-data.frame(g.PIN,Paramilitaries = "IN")
g.PRC   <-data.frame(g.PRC,Paramilitaries = "RC")
g.PRE   <-data.frame(g.PRE,Paramilitaries = "RE")
g.PUC   <-data.frame(g.PUC,Paramilitaries = "UC")
g.PAll  <-data.frame(g.PAll,Paramilitaries = "All")

# Create a combined data frame
ComFrm.P <- data.frame(rbind(g.PVI, g.PCD, g.PCL, g.PIN, g.PRC, g.PRE, g.PUC, g.PAll))  


#Subset to select coefficients using dplyr
ComFrm.P <- ComFrm.P %>% filter(  term == "Paramilitaries_VI" | 
                                    term == "Paramilitaries_CD" |
                                    term == "Paramilitaries_CL" |
                                    term == "Paramilitaries_IN" |
                                    term == "Paramilitaries_RC" |
                                    term == "Paramilitaries_RE" |
                                    term == "Paramilitaries_UC" |
                                    term == "Paramilitaries_All" )

# Add Number of Obs to Coefficients
ComFrm.P <- cbind(ComFrm.P,n.P)

# Standardize N as ratio
ComFrm.P$obs.st <- (ComFrm.P$n.P/max(ComFrm.P$n.P))*3


# Update point size
update_geom_defaults("pointrange", list(fatten=ComFrm.P$obs.st))


# Generate plot
g.P <- ggplot(ComFrm.P, aes(colour = Paramilitaries)) +
  geom_hline(yintercept = 0, colour = gray(0.5), lty = 2) +
  geom_linerange(aes(x = term, 
                     ymin = conf.low,
                     ymax = conf.high),
                 lwd = 2, position = position_dodge(width=0.5)) + 
  geom_pointrange(aes(x = term, 
                      y = estimate,
                      ymin = conf.low,
                      ymax = conf.high),
                  fatten = ComFrm.P$obs.st*2,
                  lwd=1, 
                  position = position_dodge(width=0.5),
                  shape = 16) + ylim(0, 131) + 
  labs(x="", y="Homicides from Paramilitaries") + 
  theme_bw() + 
  theme(legend.position="bottom") + guides(col = guide_legend(title = "", nrow = 2)) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  coord_flip()  +
  theme(text = element_text(size = 18),legend.text = element_text(size = 16))

g.P




# Save graph
pdf("./graphs/coeffplot/coeff_paras_v2.pdf", width=3.5, height=4.5)
g.P
dev.off()

png("./graphs/coeffplot/coeff_paras_v2.png")
g.P
dev.off()


  

### Plot coefficients Guerrilla -------------

# Replicate Figure 6g Guerrilla  

# Get Number of observations
n.GVI  <- nobs(m.GVI)
n.GCD  <- nobs(m.GCD)
n.GCL  <- nobs(m.GCL)
n.GRC  <- nobs(m.GRC)
n.GRE  <- nobs(m.GRE)
n.GUC  <- nobs(m.GUC)
n.GAll <- nobs(m.GAll)

# Get array of obs
n.G <- c(n.GVI,n.GCD,n.GCL,n.GRC,n.GRE,n.GUC,n.GAll)
n.G <- as.data.frame(n.G)
n.G

# Extract coefficients
g.GVI <- tidy(m.GVI, conf.int = TRUE)
g.GCD <- tidy(m.GCD, conf.int = TRUE)
g.GCL <- tidy(m.GCL, conf.int = TRUE)
g.GRC <- tidy(m.GRC, conf.int = TRUE)
g.GRE <- tidy(m.GRE, conf.int = TRUE)
g.GUC <- tidy(m.GUC, conf.int = TRUE)
g.GAll <- tidy(m.GAll, conf.int = TRUE)

# Add model name to each data.frame
g.GVI   <-data.frame(g.GVI,Guerrilla = "VI")
g.GCD   <-data.frame(g.GCD,Guerrilla = "CD")
g.GCL   <-data.frame(g.GCL,Guerrilla = "CL")
g.GRC   <-data.frame(g.GRC,Guerrilla = "RC")
g.GRE   <-data.frame(g.GRE,Guerrilla = "RE")
g.GUC   <-data.frame(g.GUC,Guerrilla = "UC")
g.GAll  <-data.frame(g.GAll,Guerrilla = "All")

# Create a combined data frame
ComFrm.G <- data.frame(rbind(g.GVI, g.GCD, g.GCL, g.GRC, g.GRE, g.GUC, g.GAll))  

#Subset to select coefficients using dplyr
ComFrm.G <- ComFrm.G %>% filter(  term == "Guerrilla_VI" | 
                                    term == "Guerrilla_CD" |
                                    term == "Guerrilla_CL" |
                                    term == "Guerrilla_RC" |
                                    term == "Guerrilla_RE" |
                                    term == "Guerrilla_UC" |
                                    term == "Guerrilla_All" )




# Add Number of observations
ComFrm.G <- cbind(ComFrm.G,n.G)

# Standardize N as ratio
ComFrm.G$obs.st <- (ComFrm.G$n.G/max(ComFrm.G$n.G))*3

# Update point size
update_geom_defaults("pointrange", list(fatten=ComFrm.G$obs.st))


# Generate plot
g.G <- ggplot(ComFrm.G, aes(colour = Guerrilla)) +
  geom_hline(yintercept = 0, colour = gray(0.5), lty = 2) +
  geom_linerange(aes(x = term, 
                     ymin = conf.low,
                     ymax = conf.high),
                 lwd = 2, position = position_dodge(width=0.5)) + 
  geom_pointrange(aes(x = term, 
                      y = estimate,
                      ymin = conf.low,
                      ymax = conf.high),
                  fatten = ComFrm.G$obs.st*2,
                  lwd=1, 
                  position = position_dodge(width=0.5),
                  shape = 16) + ylim(0, 131) + 
  labs(x="", y="Homicides from Guerrilla") + 
  theme_bw() + 
  theme(legend.position="bottom") + guides(col = guide_legend(title = "", nrow = 2)) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  coord_flip()  +
  theme(text = element_text(size = 18),legend.text = element_text(size = 16))

g.G


# Save graph
pdf("./graphs/coeffplot/coeff_guerrilla_v2.pdf", width=3.5, height=4.5)
g.G
dev.off()


# Save graph
png("./graphs/coeffplot/coeff_guerrilla_v2.png")
g.G
dev.off()







## 12.3. Compare homicide models P G using AIC AND BIC  ----------------------------


# Replicate Table 23 AIC and BIC Criteria by type of actor
  

### Compare models for Paramilitaries  ###############

# Calculate % missing 

library(gdata)

mis1.P<-(1-(nobs(m.PVI)/nobs(COL_data$mun)))*100
mis2.P<-(1-(nobs(m.PCD)/nobs(COL_data$mun)))*100
mis3.P<-(1-(nobs(m.PCL)/nobs(COL_data$mun)))*100
mis4.P<-(1-(nobs(m.PIN)/nobs(COL_data$mun)))*100
mis5.P<-(1-(nobs(m.PAll)/nobs(COL_data$mun)))*100
mis6.P<-(1-(nobs(m.PRC)/nobs(COL_data$mun)))*100
mis7.P<-(1-(nobs(m.PRE)/nobs(COL_data$mun)))*100
mis8.P<-(1-(nobs(m.PUC)/nobs(COL_data$mun)))*100



# Using AIC

Sum1 <- summary(m.PVI)
RSS1 <- sum(Sum1$residuals^2)
K1 <- max(m.PVI$assign)
N1 <- length(m.PVI$residuals)
n1 <- N1 - K1 - m.PVI$df.residual
AIC1.P = log(RSS1/n1) + (2*K1)/n1

Sum2 <- summary(m.PCD)
RSS2 <- sum(Sum2$residuals^2)
K2 <- max(m.PCD$assign)
N2 <- length(m.PCD$residuals)
n2 <- N2 - K2 - m.PCD$df.residual
AIC2.P = log(RSS2/n2) + (2*K2)/n2

Sum3 <- summary(m.PCL)
RSS3 <- sum(Sum3$residuals^2)
K3 <- max(m.PCL$assign)
N3 <- length(m.PCL$residuals)
n3 <- N3 - K3 - m.PCL$df.residual
AIC3.P = log(RSS3/n3) + (2*K3)/n3

Sum4 <- summary(m.PIN)
RSS4 <- sum(Sum4$residuals^2)
K4 <- max(m.PIN$assign)
N4 <- length(m.PIN$residuals)
n4 <- N4 - K4 - m.PIN$df.residual
AIC4.P = log(RSS4/n4) + (2*K4)/n4

Sum5 <- summary(m.PAll)
RSS5 <- sum(Sum5$residuals^2)
K5 <- max(m.PAll$assign)
N5 <- length(m.PAll$residuals)
n5 <- N5 - K5 - m.PAll$df.residual
AIC5.P = log(RSS5/n5) + (2*K5)/n5

Sum6 <- summary(m.PRC)
RSS6 <- sum(Sum6$residuals^2)
K6 <- max(m.PRC$assign)
N6 <- length(m.PRC$residuals)
n6 <- N6 - K6 - m.PRC$df.residual
AIC6.P = log(RSS6/n6) + (2*K6)/n6

Sum7 <- summary(m.PRE)
RSS7 <- sum(Sum7$residuals^2)
K7 <- max(m.PRE$assign)
N7 <- length(m.PRE$residuals)
n7 <- N7 - K7 - m.PRE$df.residual
AIC7.P = log(RSS7/n7) + (2*K7)/n7

Sum8 <- summary(m.PUC)
RSS8 <- sum(Sum8$residuals^2)
K8 <- max(m.PUC$assign)
N8 <- length(m.PUC$residuals)
n8 <- N8 - K8 - m.PUC$df.residual
AIC8.P = log(RSS8/n8) + (2*K8)/n8




# Using BIC

res1<-m.PVI$residuals
n1<-nrow(m.PVI$model)    
w1<-rep(1,n1) #not applicable
ll1<-0.5*(sum(log(w1))-n1*(log(2*pi)+1-log(n1)+log(sum(w1*res1^2))))
k.original1<-length(m.PVI$coefficients)
df.ll1<-k.original1+1 
BIC1.P<- -2 * ll1 + log(n1) * df.ll1

res2<-m.PCD$residuals
n2<-nrow(m.PCD$model)    
w2<-rep(1,n2) #not applicable
ll2<-0.5*(sum(log(w2))-n2*(log(2*pi)+1-log(n2)+log(sum(w2*res2^2))))
k.original2<-length(m.PCD$coefficients)
df.ll2<-k.original2+1 
BIC2.P<- -2 * ll2 + log(n2) * df.ll2

res3<-m.PCL$residuals
n3<-nrow(m.PCL$model)    
w3<-rep(1,n3) #not applicable
ll3<-0.5*(sum(log(w3))-n3*(log(2*pi)+1-log(n3)+log(sum(w3*res3^2))))
k.original3<-length(m.PCL$coefficients)
df.ll3<-k.original3+1 
BIC3.P<- -2 * ll3 + log(n3) * df.ll3

res4<-m.PIN$residuals
n4<-nrow(m.PIN$model)    
w4<-rep(1,n4) #not applicable
ll4<-0.5*(sum(log(w4))-n4*(log(2*pi)+1-log(n4)+log(sum(w4*res4^2))))
k.original4<-length(m.PIN$coefficients)
df.ll4<-k.original4+1 
BIC4.P<- -2 * ll4 + log(n4) * df.ll4

res5<-m.PAll$residuals
n5<-nrow(m.PAll$model)    
w5<-rep(1,n5) #not applicable
ll5<-0.5*(sum(log(w5))-n5*(log(2*pi)+1-log(n5)+log(sum(w5*res5^2))))
k.original5<-length(m.PAll$coefficients)
df.ll5<-k.original5+1 
BIC5.P<- -2 * ll5 + log(n5) * df.ll5

res6<-m.PRC$residuals
n6<-nrow(m.PRC$model)    
w6<-rep(1,n6) #not applicable
ll6<-0.5*(sum(log(w6))-n6*(log(2*pi)+1-log(n6)+log(sum(w6*res6^2))))
k.original6<-length(m.PRC$coefficients)
df.ll6<-k.original6+1 
BIC6.P<- -2 * ll6 + log(n6) * df.ll6

res7<-m.PRE$residuals
n7<-nrow(m.PRE$model)    
w7<-rep(1,n7) #not applicable
ll7<-0.5*(sum(log(w7))-n7*(log(2*pi)+1-log(n7)+log(sum(w7*res7^2))))
k.original7<-length(m.PRE$coefficients)
df.ll7<-k.original7+1 
BIC7.P<- -2 * ll7 + log(n7) * df.ll7

res8<-m.PUC$residuals
n8<-nrow(m.PUC$model)    
w8<-rep(1,n8) #not applicable
ll8<-0.5*(sum(log(w8))-n8*(log(2*pi)+1-log(n8)+log(sum(w8*res7^2))))
k.original7<-length(m.PUC$coefficients)
df.ll8<-k.original7+1 
BIC8.P<- -2 * ll8 + log(n8) * df.ll8





# Table of results
library(xtable)

# Create table data frame
AIC_BIC.P <- data.frame("Model" = c("ViPAA", "CEDE", "Claudia Lopez", "Indepaz", "All", "Rutas del Conflicto", "Restrepo", "UCDP"), 
                        "% Missing" = c(mis1.P, mis2.P, mis3.P, mis4.P, mis5.P, mis6.P , mis7.P, mis8.P),
                        "AIC" = c(AIC1.P, AIC2.P, AIC3.P, AIC4.P, AIC5.P, AIC6.P, AIC7.P, AIC8.P), 
                        "BIC" = c(BIC1.P, BIC2.P, BIC3.P, BIC4.P, BIC5.P, BIC6.P, BIC7.P, BIC8.P))

# # Print table in latex
# AIC_BIC_table.P <- xtable(AIC_BIC.P, caption="AIC and BIC Paramilitaries", digits = c(0,0,2,4,2))
# print(AIC_BIC_table.P, type="latex", file="./tables/AICBIC_P_v2.tex")




### Compare models for Guerrilla   ###############


# Calculate % missing

mis1.G<-(1-(nobs(m.GVI)/nobs(COL_data$mun)))*100
mis2.G<-(1-(nobs(m.GCD)/nobs(COL_data$mun)))*100
mis3.G<-(1-(nobs(m.GCL)/nobs(COL_data$mun)))*100
mis4.G<-(1-(nobs(m.GAll)/nobs(COL_data$mun)))*100
mis5.G<-(1-(nobs(m.GRC)/nobs(COL_data$mun)))*100
mis6.G<-(1-(nobs(m.GRE)/nobs(COL_data$mun)))*100
mis7.G<-(1-(nobs(m.GUC)/nobs(COL_data$mun)))*100


# Using AIC

Sum1 <- summary(m.GVI)
RSS1 <- sum(Sum1$residuals^2)
K1 <- max(m.GVI$assign)
N1 <- length(m.GVI$residuals)
n1 <- N1 - K1 - m.GVI$df.residual
AIC1.G = log(RSS1/n1) + (2*K1)/n1

Sum2 <- summary(m.GCD)
RSS2 <- sum(Sum2$residuals^2)
K2 <- max(m.GCD$assign)
N2 <- length(m.GCD$residuals)
n2 <- N2 - K2 - m.GCD$df.residual
AIC2.G = log(RSS2/n2) + (2*K2)/n2

Sum3 <- summary(m.GCL)
RSS3 <- sum(Sum3$residuals^2)
K3 <- max(m.GCL$assign)
N3 <- length(m.GCL$residuals)
n3 <- N3 - K3 - m.GCL$df.residual
AIC3.G = log(RSS3/n3) + (2*K3)/n3

Sum4 <- summary(m.PAll)
RSS4 <- sum(Sum4$residuals^2)
K4 <- max(m.PAll$assign)
N4 <- length(m.PAll$residuals)
n4 <- N4 - K4 - m.PAll$df.residual
AIC4.G = log(RSS4/n4) + (2*K4)/n4

Sum5 <- summary(m.PRC)
RSS5 <- sum(Sum5$residuals^2)
K5 <- max(m.PRC$assign)
N5 <- length(m.PRC$residuals)
n5 <- N5 - K5 - m.PRC$df.residual
AIC5.G = log(RSS5/n5) + (2*K5)/n5

Sum6 <- summary(m.PRE)
RSS6 <- sum(Sum6$residuals^2)
K6 <- max(m.PRE$assign)
N6 <- length(m.PRE$residuals)
n6 <- N6 - K6 - m.PRE$df.residual
AIC6.G = log(RSS6/n6) + (2*K6)/n6


Sum7 <- summary(m.PUC)
RSS7 <- sum(Sum7$residuals^2)
K7 <- max(m.PUC$assign)
N7 <- length(m.PUC$residuals)
n7 <- N7 - K7 - m.PUC$df.residual
AIC7.G = log(RSS7/n7) + (2*K7)/n7




# Using BIC

res1<-m.GVI$residuals
n1<-nrow(m.GVI$model)    
w1<-rep(1,n1) #not applicable
ll1<-0.5*(sum(log(w1))-n1*(log(2*pi)+1-log(n1)+log(sum(w1*res1^2))))
k.original1<-length(m.GVI$coefficients)
df.ll1<-k.original1+1 
BIC1.G<- -2 * ll1 + log(n1) * df.ll1

res2<-m.GCD$residuals
n2<-nrow(m.GCD$model)    
w2<-rep(1,n2) #not applicable
ll2<-0.5*(sum(log(w2))-n2*(log(2*pi)+1-log(n2)+log(sum(w2*res2^2))))
k.original2<-length(m.GCD$coefficients)
df.ll2<-k.original2+1 
BIC2.G<- -2 * ll2 + log(n2) * df.ll2

res3<-m.GCL$residuals
n3<-nrow(m.GCL$model)    
w3<-rep(1,n3) #not applicable
ll3<-0.5*(sum(log(w3))-n3*(log(2*pi)+1-log(n3)+log(sum(w3*res3^2))))
k.original3<-length(m.GCL$coefficients)
df.ll3<-k.original3+1 
BIC3.G<- -2 * ll3 + log(n3) * df.ll3

res4<-m.GAll$residuals
n4<-nrow(m.GAll$model)    
w4<-rep(1,n4) #not applicable
ll4<-0.5*(sum(log(w4))-n4*(log(2*pi)+1-log(n4)+log(sum(w4*res4^2))))
k.original4<-length(m.GAll$coefficients)
df.ll4<-k.original4+1 
BIC4.G<- -2 * ll4 + log(n4) * df.ll4

res5<-m.GRC$residuals
n5<-nrow(m.GRC$model)    
w5<-rep(1,n5) #not applicable
ll5<-0.5*(sum(log(w5))-n5*(log(2*pi)+1-log(n5)+log(sum(w5*res5^2))))
k.original5<-length(m.GRC$coefficients)
df.ll5<-k.original5+1 
BIC5.G<- -2 * ll5 + log(n5) * df.ll5

res6<-m.GRE$residuals
n6<-nrow(m.GRE$model)    
w6<-rep(1,n6) #not applicable
ll6<-0.6*(sum(log(w6))-n6*(log(2*pi)+1-log(n6)+log(sum(w6*res6^2))))
k.original6<-length(m.GRE$coefficients)
df.ll6<-k.original6+1 
BIC6.G<- -2 * ll6 + log(n6) * df.ll6


res7<-m.GUC$residuals
n7<-nrow(m.GUC$model)    
w7<-rep(1,n7) #not applicable
ll7<-0.6*(sum(log(w7))-n7*(log(2*pi)+1-log(n7)+log(sum(w7*res7^2))))
k.original7<-length(m.GUC$coefficients)
df.ll7<-k.original7+1 
BIC7.G<- -2 * ll7 + log(n7) * df.ll7





# Table of results
library(xtable)

# Create table in data frame
AIC_BIC.G <- data.frame("Model" = c("ViPAA", "CEDE", "Claudia Lopez", "All", "Rutas del Conflicto", "Restrepo", "UCDP"), 
                        "% Missing" = c(mis1.G, mis2.G, mis3.G, mis4.G, mis5.G, mis6.G, mis7.G),
                        "AIC" = c(AIC1.G, AIC2.G, AIC3.G, AIC4.G, AIC5.G, AIC6.G, AIC7.G), 
                        "BIC" = c(BIC1.G, BIC2.G, BIC3.G, BIC4.G, BIC5.G, BIC6.G, BIC7.G))

# # Print table in latex
# AIC_BIC_table.G <- xtable(AIC_BIC.G, caption="AIC and BIC Guerrilla", digits = c(0,0,2,4,2))
# print(AIC_BIC_table.G, type="latex", file="./tables/AICBIC_G_v2.tex")


  

### Merge Paramilitary and Guerrilla AIC & BIC for Table 23

# Merge databases
AIC_BIC.PG <- merge(AIC_BIC.P, AIC_BIC.G, by = "Model", all=TRUE)

# Delete repeated missing var
AIC_BIC.PG <- AIC_BIC.PG[-c(5)]

# Rename columns
colnames(AIC_BIC.PG)<- c("Model","Missing","AIC", "BIC", "AIC", "BIC")

# Print table in latex
AIC_BIC_table.PG <- xtable(AIC_BIC.PG, caption="AIC and BIC for Paramilitaries and Guerrilla", digits = c(0,0,2,4,2,4,2))
#print(AIC_BIC_table.PG, type="latex", file="./tables/AICBIC_PG_v2.tex")







## 12.4. Analysis homicides by armed group  ----------------------------


  
  
### Run analysis -------------

  
  # Replicate Table 17 Summary of Regression analysis by Specific Armed Group  
  # Panel 17a FARC
  # Panel 17b ELN
  # Panel 17c AUC 
    
    
# Show variable names
names(COL_data)


# Generate all FARC
COL_data$FARC_All <-0
COL_data$FARC_All[COL_data$FARC_VI==1] <- 1
COL_data$FARC_All[COL_data$FARC_CD==1] <- 1
COL_data$FARC_All[COL_data$FARC_RC==1] <- 1
COL_data$FARC_All[COL_data$FARC_RE==1] <- 1
COL_data$FARC_All[COL_data$FARC_UC==1] <- 1
COL_data$FARC_All[is.na(COL_data$FARC_VI) & is.na(COL_data$FARC_CD) & is.na(COL_data$FARC_RC) & is.na(COL_data$FARC_RE) & is.na(COL_data$FARC_UC)] <- NA


# Generate all ELN
COL_data$ELN_All <-0
COL_data$ELN_All[COL_data$ELN_VI==1] <- 1
COL_data$ELN_All[COL_data$ELN_CD==1] <- 1
COL_data$ELN_All[COL_data$ELN_RC==1] <- 1
COL_data$ELN_All[COL_data$ELN_UC==1] <- 1
COL_data$ELN_All[is.na(COL_data$ELN_VI) & is.na(COL_data$ELN_CD) & is.na(COL_data$ELN_RC) & is.na(COL_data$ELN_UC)]<-NA


# Generate all AUC
COL_data$AUC_All <-0
COL_data$AUC_All[COL_data$AUC_VI==1] <- 1
COL_data$AUC_All[COL_data$AUC_CD==1] <- 1
COL_data$AUC_All[COL_data$AUC_RC==1] <- 1
COL_data$AUC_All[COL_data$AUC_RE==1] <- 1
COL_data$AUC_All[COL_data$AUC_UC==1] <- 1
COL_data$AUC_All[is.na(COL_data$AUC_VI) & is.na(COL_data$AUC_CD) & is.na(COL_data$AUC_RC) & is.na(COL_data$AUC_RE) & is.na(COL_data$AUC_UC)]<-NA



  
  
# Run the models
m.FVI  <-plm(homicide_rate ~ FARC_VI  + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year"))
m.FCD  <-plm(homicide_rate ~ FARC_CD  + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.FRC  <-plm(homicide_rate ~ FARC_RC  + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.FRE  <-plm(homicide_rate ~ FARC_RE  + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.FUC  <-plm(homicide_rate ~ FARC_UC  + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.FAll <-plm(homicide_rate ~ FARC_All + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 

m.EVI  <-plm(homicide_rate ~ ELN_VI   + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.ECD  <-plm(homicide_rate ~ ELN_CD   + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.ERC  <-plm(homicide_rate ~ ELN_RC   + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.EUC  <-plm(homicide_rate ~ ELN_UC   + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.EAll <-plm(homicide_rate ~ ELN_All  + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 

m.AVI  <-plm(homicide_rate ~ AUC_VI   + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.ACD  <-plm(homicide_rate ~ AUC_CD   + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.ARC  <-plm(homicide_rate ~ AUC_RC   + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.ARE  <-plm(homicide_rate ~ AUC_RE   + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.AUC  <-plm(homicide_rate ~ AUC_UC   + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 
m.AAll <-plm(homicide_rate ~ AUC_All  + cultivos2_log + oil_R +  mines + elections + poplog +poplog_sqr +plan_colombia_mili +plan_colombia_eco +pastrana_peace +barco_peace +plan_patriota +medellin_offensive +pibR_pc_log + bases + parques, data=COL_data, model = "within", index=c("mun", "year")) 


  
  


# # Results FARC
# stargazer(m.FVI, m.FCD, m.FRC, m.FRE, m.FUC, m.FAll, 
#           type="text",
#           title="Regression Results", 
#           align=TRUE,
#           digits = 2,
#           omit.stat=c("f"),
#           out="./tables/results_F_v2.txt")
# 
# stargazer(m.FVI, m.FCD, m.FRC, m.FRE, m.FUC, m.FAll, 
#           type="latex",
#           title="Regression Results", 
#           align=TRUE,
#           digits = 2,
#           omit.stat=c("f"),
#           out="./tables/results_F_v2.tex")
# 
# 
# # Results ELN
# stargazer(m.EVI, m.ECD, m.ERC, m.EUC,  m.EAll, 
#           type="text",
#           title="Regression Results", 
#           align=TRUE,
#           digits = 2,
#           omit.stat=c("f"),
#           out="./tables/results_E_v2.txt")
# 
# stargazer(m.EVI, m.ECD, m.ERC,  m.EUC, m.EAll, 
#           type="latex",
#           title="Regression Results", 
#           align=TRUE,
#           digits = 2,
#           omit.stat=c("f"),
#           out="./tables/results_E_v2.tex")
# 
# 
# # Results AUC
# stargazer(m.AVI, m.ACD, m.ARC, m.ARE, m.AUC, m.AAll,
#           type="text",
#           title="Regression Results", 
#           align=TRUE,
#           digits = 2,
#           omit.stat=c("f"),
#           out="./tables/results_A_v2.txt")
# 
# stargazer(m.AVI, m.ACD, m.ARC, m.ARE, m.AUC, m.AAll,
#           type="latex",
#           title="Regression Results", 
#           align=TRUE,
#           digits = 2,
#           omit.stat=c("f"),
#           out="./tables/results_A_v2.tex")




# Results FARC ELN AUC
# stargazer(m.FVI, m.FCD, m.FRC, m.FRE, m.FUC, m.FAll, 
#           m.EVI, m.ECD, m.ERC, m.EUC, m.EAll, 
#           m.AVI, m.ACD, m.ARC, m.ARE, m.AUC, m.AAll,
#           type="text",
#           title="Regression Results", 
#           align=TRUE,
#           digits = 2,
#           omit.stat=c("f"),
#           out="./tables/results_FEA_v2.txt")

stargazer(m.FVI, m.FCD, m.FRC, m.FRE, m.FUC, m.FAll, 
          m.EVI, m.ECD, m.ERC, m.EUC, m.EAll, 
          m.AVI, m.ACD, m.ARC, m.ARE, m.ARE, m.AAll,
          type="latex",
          title="Regression Results", 
          align=TRUE,
          digits = 2,
          omit.stat=c("f"),
          out="./tables/results_FEA_v2.tex")






### Plot coefficients FARC -------------

# Replicate Figure 6h FARC  
  
# Load packages
library(broom)       # To clean up data
library(ggplot2)     # Plot coefficients
library(dplyr)       # Subset data by row

# Extract coefficients
g.FVI  <- tidy(m.FVI, conf.int = TRUE)
g.FCD  <- tidy(m.FCD, conf.int = TRUE)
g.FRC  <- tidy(m.FRC, conf.int = TRUE)
g.FRE  <- tidy(m.FRE, conf.int = TRUE)
g.FUC  <- tidy(m.FUC, conf.int = TRUE)
g.FAll <- tidy(m.FAll, conf.int = TRUE)

# Add model name to each data.frame
g.FVI   <-data.frame(g.FVI,FARC = "VI")
g.FCD   <-data.frame(g.FCD,FARC = "CD")
g.FRC   <-data.frame(g.FRC,FARC = "RC")
g.FRE   <-data.frame(g.FRE,FARC = "RE")
g.FUC   <-data.frame(g.FUC,FARC = "UC")
g.FAll  <-data.frame(g.FAll,FARC = "All")

# Create a combined data frame
ComFrm.F <- data.frame(rbind(g.FVI, g.FCD, g.FRC, g.FRE, g.FUC, g.FAll))  


#Subset to select coefficients using dplyr
ComFrm.F <- ComFrm.F %>% filter(  term == "FARC_VI" | 
                                    term == "FARC_CD" |
                                    term == "FARC_RC" |
                                    term == "FARC_RE" |
                                    term == "FARC_UC" |
                                    term == "FARC_All" )


# Get Number of observations
n.FVI  <- nobs(m.FVI)
n.FCD  <- nobs(m.FCD)
n.FRC  <- nobs(m.FRC)
n.FRE  <- nobs(m.FRE)
n.FUC  <- nobs(m.FUC)
n.FAll  <- nobs(m.FAll)


# Get array of obs
n.F <- c(n.FVI,n.FCD,n.FRC,n.FRE,n.FUC,n.FAll)
n.F <- as.data.frame(n.F)

# Add Number of Obs to Coefficients
ComFrm.F <- cbind(ComFrm.F,n.F)

# Standardize N as ratio
ComFrm.F$obs.st <- (ComFrm.F$n.F/max(ComFrm.F$n.F))*3


# Update point size
update_geom_defaults("pointrange", list(fatten=ComFrm.F$obs.st))

#Plot coefficients
g.F <- ggplot(ComFrm.F, aes(colour = FARC)) +
  geom_hline(yintercept = 0, colour = gray(0.5), lty = 2) +
  geom_linerange(aes(x = term, 
                     ymin = conf.low,
                     ymax = conf.high),
                 lwd = 2, position = position_dodge(width=0.5)) + 
  geom_pointrange(aes(x = term, 
                      y = estimate,
                      ymin = conf.low,
                      ymax = conf.high),
                  fatten = ComFrm.F$obs.st*2,
                  lwd=1, 
                  position = position_dodge(width=0.5),
                  shape = 16) + ylim(0, 131) + 
  labs(x="", y="Homicides from FARC") + 
  theme_bw() + 
  theme(legend.position="bottom") + guides(col = guide_legend(title = "", nrow = 2)) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  coord_flip()  +
  theme(text = element_text(size = 18),legend.text = element_text(size = 16))

g.F

# Save graph
pdf("./graphs/coeffplot/coeff_farc_v2.pdf", width=3.5, height=4.5)
g.F
dev.off()

png("./graphs/coeffplot/coeff_farc_v2.png")
g.F
dev.off()





### Plot coefficients ELN -------------

# Replicate Figure 6i ELN 
  
# Extract coefficients
g.EVI  <- tidy(m.EVI, conf.int = TRUE)
g.ECD  <- tidy(m.ECD, conf.int = TRUE)
g.ERC  <- tidy(m.ERC, conf.int = TRUE)
g.EUC  <- tidy(m.EUC, conf.int = TRUE)
g.EAll <- tidy(m.EAll, conf.int = TRUE)

# Add model name to each data.frame
g.EVI   <-data.frame(g.EVI,ELN = "VI")
g.ECD   <-data.frame(g.ECD,ELN = "CD")
g.ERC   <-data.frame(g.ERC,ELN = "RC")
g.EUC   <-data.frame(g.EUC,ELN = "UC")
g.EAll  <-data.frame(g.EAll,ELN = "All")

# Create a combined data frame
ComFrm.E <- data.frame(rbind(g.EVI, g.ECD, g.ERC, g.EUC, g.EAll))  


#Subset to select coefficients using dplyr
ComFrm.E <- ComFrm.E %>% filter(  term == "ELN_VI" | 
                                    term == "ELN_CD" |
                                    term == "ELN_RC" |
                                    term == "ELN_UC" |
                                    term == "ELN_All" )


# Get Number of observations
n.EVI  <- nobs(m.EVI)
n.ECD  <- nobs(m.ECD)
n.ERC  <- nobs(m.ERC)
n.EUC  <- nobs(m.EUC)
n.EAll  <- nobs(m.EAll)


# Get array of obs
n.E <- c(n.EVI,n.ECD,n.ERC,n.EUC,n.EAll)
n.E <- as.data.frame(n.E)

# Add Number of Obs to Coefficients
ComFrm.E <- cbind(ComFrm.E,n.E)

# Standardize N as ratio
ComFrm.E$obs.st <- (ComFrm.E$n.E/max(ComFrm.E$n.E))*3


# Update point size
update_geom_defaults("pointrange", list(fatten=ComFrm.E$obs.st))

#Plot coefficients
g.E <- ggplot(ComFrm.E, aes(colour = ELN)) +
  geom_hline(yintercept = 0, colour = gray(0.5), lty = 2) +
  geom_linerange(aes(x = term, 
                     ymin = conf.low,
                     ymax = conf.high),
                 lwd = 2, position = position_dodge(width=0.5)) + 
  geom_pointrange(aes(x = term, 
                      y = estimate,
                      ymin = conf.low,
                      ymax = conf.high),
                  fatten = ComFrm.E$obs.st*2,
                  lwd=1, 
                  position = position_dodge(width=0.5),
                  shape = 16) + ylim(0, 131) + 
  labs(x="", y="Homicides from ELN") + 
  theme_bw() + 
  theme(legend.position="bottom") + guides(col = guide_legend(title = "", nrow = 2)) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  coord_flip()  +
  theme(text = element_text(size = 18),legend.text = element_text(size = 16))

g.E

# Save graph
pdf("./graphs/coeffplot/coeff_eln_2.pdf", width=3.5, height=4.5)
g.E
dev.off()

png("./graphs/coeffplot/coeff_eln_2.png")
g.E
dev.off()




### Plot coefficients AUC -------------

# Replicate Figure 6j AUC 
  
# Extract coefficients
g.AVI  <- tidy(m.AVI, conf.int = TRUE)
g.ACD  <- tidy(m.ACD, conf.int = TRUE)
g.ARC  <- tidy(m.ARC, conf.int = TRUE)
g.ARE  <- tidy(m.ARE, conf.int = TRUE)
g.AUC  <- tidy(m.AUC, conf.int = TRUE)
g.AAll <- tidy(m.AAll, conf.int = TRUE)

# Add model name to each data.frame
g.AVI   <-data.frame(g.AVI,AUC = "VI")
g.ACD   <-data.frame(g.ACD,AUC = "CD")
g.ARC   <-data.frame(g.ARC,AUC = "RC")
g.ARE   <-data.frame(g.ARE,AUC = "RE")
g.AUC   <-data.frame(g.AUC,AUC = "UC")
g.AAll  <-data.frame(g.AAll,AUC = "All")

# Create a combined data frame
ComFrm.A <- data.frame(rbind(g.AVI, g.ACD, g.ARC, g.ARE, g.AUC, g.AAll))  


#Subset to select coefficients using dplyr
ComFrm.A <- ComFrm.A %>% filter(  term == "AUC_VI" | 
                                    term == "AUC_CD" |
                                    term == "AUC_RC" |
                                    term == "AUC_RE" |
                                    term == "AUC_UC" |
                                    term == "AUC_All" )

# Get Number of observations
n.AVI  <- nobs(m.AVI)
n.ACD  <- nobs(m.ACD)
n.ARC  <- nobs(m.ARC)
n.ARE  <- nobs(m.ARE)
n.AUC  <- nobs(m.AUC)
n.AAll  <- nobs(m.AAll)

# Get array of obs
n.A <- c(n.AVI,n.ACD,n.ARC,n.ARE,n.AUC,n.AAll)
n.A <- as.data.frame(n.A)

# Add Number of Obs to Coefficients
ComFrm.A <- cbind(ComFrm.A,n.A)

# Standardize N as ratio
ComFrm.A$obs.st <- (ComFrm.A$n.A/max(ComFrm.A$n.A))*3


# Update point size
update_geom_defaults("pointrange", list(fatten=ComFrm.A$obs.st))

#Plot coefficients
g.A <- ggplot(ComFrm.A, aes(colour = AUC)) +
  geom_hline(yintercept = 0, colour = gray(0.5), lty = 2) +
  geom_linerange(aes(x = term, 
                     ymin = conf.low,
                     ymax = conf.high),
                 lwd = 2, position = position_dodge(width=0.5)) + 
  geom_pointrange(aes(x = term, 
                      y = estimate,
                      ymin = conf.low,
                      ymax = conf.high),
                  fatten = ComFrm.A$obs.st*2,
                  lwd=1, 
                  position = position_dodge(width=0.5),
                  shape = 16) + ylim(0, 131) + 
  labs(x="", y="Homicides from AUC") + 
  theme_bw() + 
  theme(legend.position="bottom") + guides(col = guide_legend(title = "", nrow = 2)) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  coord_flip()  +
  theme(text = element_text(size=18),legend.text = element_text(size=16)) 

g.A

# Save graph
pdf("./graphs/coeffplot/coeff_auc_2.pdf", width=3.5, height=4.5)
g.A
dev.off()

png("./graphs/coeffplot/coeff_auc_2.png")
g.A
dev.off()








## 12.5. Compare homicide models F E A using AIC AND BIC   ----------------------------



  
# Replicate Table 23 AIC and BIC Criteria by armed group

    
### Compare models for FARC  ###############



# Calculate % missing

library(gdata)

mis1.F<-(1-(nobs(m.FVI)/nobs(COL_data$mun)))*100
mis2.F<-(1-(nobs(m.FCD)/nobs(COL_data$mun)))*100
mis3.F<-(1-(nobs(m.FRC)/nobs(COL_data$mun)))*100
mis4.F<-(1-(nobs(m.FAll)/nobs(COL_data$mun)))*100
mis5.F<-(1-(nobs(m.FRE)/nobs(COL_data$mun)))*100




# Using AIC
{
Sum1 <- summary(m.FVI)
RSS1 <- sum(Sum1$residuals^2)
K1 <- max(m.FVI$assign)
N1 <- length(m.FVI$residuals)
n1 <- N1 - K1 - m.FVI$df.residual
AIC1.F = log(RSS1/n1) + (2*K1)/n1

Sum2 <- summary(m.FCD)
RSS2 <- sum(Sum2$residuals^2)
K2 <- max(m.FCD$assign)
N2 <- length(m.FCD$residuals)
n2 <- N2 - K2 - m.FCD$df.residual
AIC2.F = log(RSS2/n2) + (2*K2)/n2

Sum3 <- summary(m.FRC)
RSS3 <- sum(Sum3$residuals^2)
K3 <- max(m.FRC$assign)
N3 <- length(m.FRC$residuals)
n3 <- N3 - K3 - m.FRC$df.residual
AIC3.F = log(RSS3/n3) + (2*K3)/n3

Sum4 <- summary(m.FAll)
RSS4 <- sum(Sum4$residuals^2)
K4 <- max(m.FAll$assign)
N4 <- length(m.FAll$residuals)
n4 <- N4 - K4 - m.FAll$df.residual
AIC4.F = log(RSS4/n4) + (2*K4)/n4

Sum5 <- summary(m.FRE)
RSS5 <- sum(Sum5$residuals^2)
K5 <- max(m.FRE$assign)
N5 <- length(m.FRE$residuals)
n5 <- N5 - K5 - m.FRE$df.residual
AIC5.F = log(RSS5/n5) + (2*K5)/n5
}



# Using BIC
{
res1<-m.FVI$residuals
n1<-nrow(m.FVI$model)    
w1<-rep(1,n1) #not applicable
ll1<-0.5*(sum(log(w1))-n1*(log(2*pi)+1-log(n1)+log(sum(w1*res1^2))))
k.original1<-length(m.FVI$coefficients)
df.ll1<-k.original1+1 
BIC1.F<- -2 * ll1 + log(n1) * df.ll1

res2<-m.FCD$residuals
n2<-nrow(m.FCD$model)    
w2<-rep(1,n2) #not applicable
ll2<-0.5*(sum(log(w2))-n2*(log(2*pi)+1-log(n2)+log(sum(w2*res2^2))))
k.original2<-length(m.FCD$coefficients)
df.ll2<-k.original2+1 
BIC2.F<- -2 * ll2 + log(n2) * df.ll2

res3<-m.FRC$residuals
n3<-nrow(m.FRC$model)    
w3<-rep(1,n3) #not applicable
ll3<-0.5*(sum(log(w3))-n3*(log(2*pi)+1-log(n3)+log(sum(w3*res3^2))))
k.original3<-length(m.FRC$coefficients)
df.ll3<-k.original3+1 
BIC3.F<- -2 * ll3 + log(n3) * df.ll3

res4<-m.FAll$residuals
n4<-nrow(m.FAll$model)    
w4<-rep(1,n4) #not applicable
ll4<-0.5*(sum(log(w4))-n4*(log(2*pi)+1-log(n4)+log(sum(w4*res4^2))))
k.original4<-length(m.FAll$coefficients)
df.ll4<-k.original4+1 
BIC4.F<- -2 * ll4 + log(n4) * df.ll4

res5<-m.FRE$residuals
n5<-nrow(m.FRE$model)    
w5<-rep(1,n5) #not applicable
ll5<-0.5*(sum(log(w5))-n5*(log(2*pi)+1-log(n5)+log(sum(w5*res5^2))))
k.original5<-length(m.FRE$coefficients)
df.ll5<-k.original5+1 
BIC5.F<- -2 * ll5 + log(n5) * df.ll5
}



# Table of resutls
library(xtable)

# Create table in data frame
AIC_BIC.F <- data.frame("Model" = c("ViPAA", "CEDE", "Rutas del Conflicto", "All" , "Restrepo"), 
                        "% Missing" = c(mis1.F, mis2.F, mis3.F, mis4.F, mis5.F),
                        "AIC" = c(AIC1.F, AIC2.F, AIC3.F, AIC4.F , AIC5.F), 
                        "BIC" = c(BIC1.F, BIC2.F, BIC3.F, BIC4.F, BIC5.F))

# Print table in latex
AIC_BIC_table.F <- xtable(AIC_BIC.F, caption="AIC and BIC for FARC", digits = c(0,0,2,4,2))
#print(AIC_BIC_table.F, type="latex", file="./tables/AICBIC_FARC_v2.tex")






### Compare models for ELN  ###############
  


# Calculate % missing

mis1.E<-(1-(nobs(m.EVI)/nobs(COL_data$mun)))*100
mis2.E<-(1-(nobs(m.ECD)/nobs(COL_data$mun)))*100
mis3.E<-(1-(nobs(m.ERC)/nobs(COL_data$mun)))*100
mis4.E<-(1-(nobs(m.EAll)/nobs(COL_data$mun)))*100




# Using AIC
{
Sum1 <- summary(m.EVI)
RSS1 <- sum(Sum1$residuals^2)
K1 <- max(m.EVI$assign)
N1 <- length(m.EVI$residuals)
n1 <- N1 - K1 - m.EVI$df.residual
AIC1.E = log(RSS1/n1) + (2*K1)/n1

Sum2 <- summary(m.ECD)
RSS2 <- sum(Sum2$residuals^2)
K2 <- max(m.ECD$assign)
N2 <- length(m.ECD$residuals)
n2 <- N2 - K2 - m.ECD$df.residual
AIC2.E = log(RSS2/n2) + (2*K2)/n2

Sum3 <- summary(m.ERC)
RSS3 <- sum(Sum3$residuals^2)
K3 <- max(m.ERC$assign)
N3 <- length(m.ERC$residuals)
n3 <- N3 - K3 - m.ERC$df.residual
AIC3.E = log(RSS3/n3) + (2*K3)/n3

Sum4 <- summary(m.EAll)
RSS4 <- sum(Sum4$residuals^2)
K4 <- max(m.EAll$assign)
N4 <- length(m.EAll$residuals)
n4 <- N4 - K4 - m.EAll$df.residual
AIC4.E = log(RSS4/n4) + (2*K4)/n4
}



# Using BIC
{
res1<-m.EVI$residuals
n1<-nrow(m.EVI$model)    
w1<-rep(1,n1) #not applicable
ll1<-0.5*(sum(log(w1))-n1*(log(2*pi)+1-log(n1)+log(sum(w1*res1^2))))
k.original1<-length(m.EVI$coefficients)
df.ll1<-k.original1+1 
BIC1.E<- -2 * ll1 + log(n1) * df.ll1

res2<-m.ECD$residuals
n2<-nrow(m.ECD$model)    
w2<-rep(1,n2) #not applicable
ll2<-0.5*(sum(log(w2))-n2*(log(2*pi)+1-log(n2)+log(sum(w2*res2^2))))
k.original2<-length(m.ECD$coefficients)
df.ll2<-k.original2+1 
BIC2.E<- -2 * ll2 + log(n2) * df.ll2

res3<-m.ERC$residuals
n3<-nrow(m.ERC$model)    
w3<-rep(1,n3) #not applicable
ll3<-0.5*(sum(log(w3))-n3*(log(2*pi)+1-log(n3)+log(sum(w3*res3^2))))
k.original3<-length(m.ERC$coefficients)
df.ll3<-k.original3+1 
BIC3.E<- -2 * ll3 + log(n3) * df.ll3

res4<-m.EAll$residuals
n4<-nrow(m.EAll$model)    
w4<-rep(1,n4) #not applicable
ll4<-0.5*(sum(log(w4))-n4*(log(2*pi)+1-log(n4)+log(sum(w4*res4^2))))
k.original4<-length(m.EAll$coefficients)
df.ll4<-k.original4+1 
BIC4.E<- -2 * ll4 + log(n4) * df.ll4
}




# Table of resutls
library(xtable)


# Create table in data frame
AIC_BIC.E <- data.frame("Model" = c("ViPAA", "CEDE", "Rutas del Conflicto", "All"), 
                        "% Missing" = c(mis1.E, mis2.E, mis3.E, mis4.E),
                        "AIC" = c(AIC1.E, AIC2.E, AIC3.E, AIC4.E), 
                        "BIC" = c(BIC1.E, BIC2.E, BIC3.E, BIC4.E))

# Print table in latex
AIC_BIC_table.E <- xtable(AIC_BIC.E, caption="AIC and BIC for ELN", digits = c(0,0,2,4,2))
#print(AIC_BIC_table.E, type="latex", file="./tables/AICBIC_ELN_v2.tex")






### Compare models for AUC  ###############
 

# Calculate % missing

mis1.A<-(1-(nobs(m.AVI)/nobs(COL_data$mun)))*100
mis2.A<-(1-(nobs(m.ACD)/nobs(COL_data$mun)))*100
mis3.A<-(1-(nobs(m.ARC)/nobs(COL_data$mun)))*100
mis4.A<-(1-(nobs(m.AAll)/nobs(COL_data$mun)))*100
mis5.A<-(1-(nobs(m.ARE)/nobs(COL_data$mun)))*100




# Using AIC
{
Sum1 <- summary(m.AVI)
RSS1 <- sum(Sum1$residuals^2)
K1 <- max(m.AVI$assign)
N1 <- length(m.AVI$residuals)
n1 <- N1 - K1 - m.AVI$df.residual
AIC1.A = log(RSS1/n1) + (2*K1)/n1

Sum2 <- summary(m.ACD)
RSS2 <- sum(Sum2$residuals^2)
K2 <- max(m.ACD$assign)
N2 <- length(m.ACD$residuals)
n2 <- N2 - K2 - m.ACD$df.residual
AIC2.A = log(RSS2/n2) + (2*K2)/n2

Sum3 <- summary(m.ARC)
RSS3 <- sum(Sum3$residuals^2)
K3 <- max(m.ARC$assign)
N3 <- length(m.ARC$residuals)
n3 <- N3 - K3 - m.ARC$df.residual
AIC3.A = log(RSS3/n3) + (2*K3)/n3

Sum4 <- summary(m.AAll)
RSS4 <- sum(Sum4$residuals^2)
K4 <- max(m.AAll$assign)
N4 <- length(m.AAll$residuals)
n4 <- N4 - K4 - m.AAll$df.residual
AIC4.A = log(RSS4/n4) + (2*K4)/n4

Sum5 <- summary(m.ARE)
RSS5 <- sum(Sum5$residuals^2)
K5 <- max(m.ARE$assign)
N5 <- length(m.ARE$residuals)
n5 <- N5 - K5 - m.ARE$df.residual
AIC5.A = log(RSS5/n5) + (2*K5)/n5
}



# Using BIC
{
res1<-m.AVI$residuals
n1<-nrow(m.AVI$model)    
w1<-rep(1,n1) #not applicable
ll1<-0.5*(sum(log(w1))-n1*(log(2*pi)+1-log(n1)+log(sum(w1*res1^2))))
k.original1<-length(m.AVI$coefficients)
df.ll1<-k.original1+1 
BIC1.A<- -2 * ll1 + log(n1) * df.ll1

res2<-m.ACD$residuals
n2<-nrow(m.ACD$model)    
w2<-rep(1,n2) #not applicable
ll2<-0.5*(sum(log(w2))-n2*(log(2*pi)+1-log(n2)+log(sum(w2*res2^2))))
k.original2<-length(m.ACD$coefficients)
df.ll2<-k.original2+1 
BIC2.A<- -2 * ll2 + log(n2) * df.ll2

res3<-m.ARC$residuals
n3<-nrow(m.ARC$model)    
w3<-rep(1,n3) #not applicable
ll3<-0.5*(sum(log(w3))-n3*(log(2*pi)+1-log(n3)+log(sum(w3*res3^2))))
k.original3<-length(m.ARC$coefficients)
df.ll3<-k.original3+1 
BIC3.A<- -2 * ll3 + log(n3) * df.ll3

res4<-m.AAll$residuals
n4<-nrow(m.AAll$model)    
w4<-rep(1,n4) #not applicable
ll4<-0.5*(sum(log(w4))-n4*(log(2*pi)+1-log(n4)+log(sum(w4*res4^2))))
k.original4<-length(m.AAll$coefficients)
df.ll4<-k.original4+1 
BIC4.A<- -2 * ll4 + log(n4) * df.ll4

res5<-m.ARE$residuals
n5<-nrow(m.ARE$model)    
w5<-rep(1,n5) #not applicable
ll5<-0.5*(sum(log(w5))-n5*(log(2*pi)+1-log(n5)+log(sum(w5*res5^2))))
k.original5<-length(m.ARE$coefficients)
df.ll5<-k.original5+1 
BIC5.A<- -2 * ll5 + log(n5) * df.ll5
}




# Table of resutls
library(xtable)


# Create table in data frame
AIC_BIC.A <- data.frame("Model" = c("ViPAA", "CEDE", "Rutas del Conflicto", "All", "Restrepo"), 
                        "% Missing" = c(mis1.A, mis2.A, mis3.A, mis4.A, mis5.A),
                        "AIC" = c(AIC1.A, AIC2.A, AIC3.A, AIC4.A, AIC5.A), 
                        "BIC" = c(BIC1.A, BIC2.A, BIC3.A, BIC4.A, BIC4.A))

# Print table in latex
AIC_BIC_table.A <- xtable(AIC_BIC.A, caption="AIC and BIC for AUC", digits = c(0,0,2,4,2))
#print(AIC_BIC_table.A, type="latex", file="./tables/AICBIC_AUC_v2.tex")








### Merge all AIC & BIC for FARC ELN and AUC ###############

# Merge databases
AIC_BIC.PGFEA <- merge(AIC_BIC.P, AIC_BIC.G, by = "Model", all=TRUE)
AIC_BIC.PGFEA <- merge(AIC_BIC.PGFEA, AIC_BIC.F, by = "Model", all=TRUE)
AIC_BIC.PGFEA <- merge(AIC_BIC.PGFEA, AIC_BIC.E, by = "Model", all=TRUE)
AIC_BIC.PGFEA <- merge(AIC_BIC.PGFEA, AIC_BIC.A, by = "Model", all=TRUE)


# get average of RC
misRC <-(AIC_BIC.PGFEA[6,c(8,11,14)])
misRC <-as.numeric(misRC)
misRC
meanRC<-mean(misRC)
meanRC

# Delete repeated missing var
AIC_BIC.PGFEA <- AIC_BIC.PGFEA[-c(5,8,11,14)]

# Insert average RC missingness
AIC_BIC.PGFEA[6,2]<-meanRC

# Rename columns
colnames(AIC_BIC.PGFEA)<- c("Model","Missing","AIC", "BIC", "AIC", "BIC", "AIC", "BIC", "AIC", "BIC", "AIC", "BIC")

# dataframe
AIC_BIC.PGFEA<-data.frame(AIC_BIC.PGFEA)

# Round up some numbers
AIC_BIC.PGFEA$Missing<-round(AIC_BIC.PGFEA$Missing, digits=2)
AIC_BIC.PGFEA$AIC<-round(AIC_BIC.PGFEA$AIC, digits=4)
AIC_BIC.PGFEA$AIC.1<-round(AIC_BIC.PGFEA$AIC.1, digits=4)
AIC_BIC.PGFEA$AIC.2<-round(AIC_BIC.PGFEA$AIC.2, digits=4)
AIC_BIC.PGFEA$AIC.3<-round(AIC_BIC.PGFEA$AIC.3, digits=4)
AIC_BIC.PGFEA$AIC.4<-round(AIC_BIC.PGFEA$AIC.4, digits=4)



# Print table in latex
# This integrates the AIC BIC results into Table 23

AIC_BIC_table.PGFEA <- AIC_BIC.PGFEA[order(AIC_BIC.PGFEA$Missing),]
AIC_BIC_table.PGFEA <- xtable(AIC_BIC.PGFEA, caption="AIC and BIC Criteria", digits = c(0,0,2,3,0,3,0,3,0,3,0,3,0))
print(AIC_BIC_table.PGFEA, type="latex", 
      format.args = list(big.mark = ",", decimal.mark = "."),
      file="./tables/AICBIC_PGFEA_v2.tex")



# Merge FEA AIC & BIC

# Merge databases
AIC_BIC.FEA <- merge(AIC_BIC.F, AIC_BIC.E, by = "Model", all=TRUE)
AIC_BIC.FEA <- merge(AIC_BIC.FEA, AIC_BIC.A, by = "Model", all=TRUE)


# get average of RC for FEA
misRC1 <-(AIC_BIC.FEA[3,c(2,5,8)])
misRC1 <-as.numeric(misRC1)
misRC1
meanRC1<-mean(misRC1)
meanRC1

# Delete repeated missing var
AIC_BIC.FEA <- AIC_BIC.FEA[-c(5,8)]

# Insert average RC missingness
AIC_BIC.FEA[3,2]<-meanRC1

# Rename columns
colnames(AIC_BIC.FEA)<- c("Model","Missing","AIC", "BIC", "AIC", "BIC", "AIC", "BIC")

# Print table in latex
AIC_BIC_table.FEA <- xtable(AIC_BIC.FEA, caption="AIC and BIC Criteria", digits = c(0,0,2,3,0,3,0,3,0))
# print(AIC_BIC_table.FEA, type="latex", 
#       format.args = list(big.mark = ",", decimal.mark = "."),
#       file="./tables/AICBIC_FEA_v2.tex")









# 13. REPLICATE DUBE AND VARGAS WITH DIFFERENT MEASURES #####################################################


## 13.0 Get the data   ----------------------------

dv_data <- read.csv("./data_final/dube_vargas_similarity.csv")
colnames(dv_data)


## Replicate Dube and Vargas' original analysis
# xi: xtivreg2 gueratt  lpop _Y* _R*  coca94indxyear (cofintxlinternalp   = rxltop3cof txltop3cof  rtxltop3cof)  oilprod88xlop , cluster(department) partial(_R* _Y*)  fe first
# xi: xtivreg2 paratt   lpop _Y* _R*  coca94indxyear (cofintxlinternalp   = rxltop3cof txltop3cof  rtxltop3cof)  oilprod88xlop , cluster(department) partial(_R* _Y*)  fe first

library(plm)

DV.Porig <-plm(gueratt ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                 X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + 
                 X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
               | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
               data=dv_data , model = "within", index=c("mun", "year")) # 
summary(DV.Porig)




## 13.1 Run analysis   ----------------------------

  
### First stage   ----------------------------
# DV.first  <-plm(cofintxlinternalp ~ rxltop3cof + txltop3cof + rtxltop3cof + lpop + coca94indxyear + oilprod88xlop + cofintxlinternalp + 
#                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4, 
#                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
# summary(DV.first)



### Paramilitaries   ----------------------------

# Regression analysis of Paramilitaries

  DV.P.VI  <-plm(paramilitaries_vi ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.P.VI)
  
  DV.P.CD  <-plm(paramilitaries_cd ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual",index=c("mun", "year")) # 
  summary(DV.P.CD)
  
  DV.P.CL  <-plm(paramilitaries_cl ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual",index=c("mun", "year")) # 
  summary(DV.P.CL)
  
  # DV.P.IN  <-plm(paramilitaries_in ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
  #                  X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
  #                | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
  #                data=dv_data , model = "within", effect = "individual",index=c("mun", "year")) # 
  # summary(DV.P.IN)
  
  DV.P.RE  <-plm(paramilitaries_re ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual",index=c("mun", "year")) # 
  summary(DV.P.RE)
  
  DV.P.UC  <-plm(paramilitaries_uc ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual",index=c("mun", "year")) # 
  summary(DV.P.UC)
  
  DV.P.All  <-plm(paramilitaries_all ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                    X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                  | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                  data=dv_data , model = "within", effect = "individual",index=c("mun", "year")) # 
  summary(DV.P.All)



# Plot coefficients of Paramilitaries

  
  # Replicate Figure 6a Paramilitary presence
  
  # Load packages
  library(broom)       # To clean up data
  library(ggplot2)     # Plot coefficients
  library(dplyr)       # Subset data by row
  
  
  # Get Number of observations
  n.DV.P.VI  <- nobs(DV.P.VI)
  n.DV.P.CD  <- nobs(DV.P.CD)
  n.DV.P.CL  <- nobs(DV.P.CL)
  n.DV.P.RE  <- nobs(DV.P.RE)
  n.DV.P.UC  <- nobs(DV.P.UC)
  n.DV.P.All  <- nobs(DV.P.All)
  
  
  # Get array of obs
  n.DV.P <- c(n.DV.P.VI,n.DV.P.CD,n.DV.P.CL,n.DV.P.RE,n.DV.P.UC,n.DV.P.All)
  n.DV.P <- as.data.frame(n.DV.P)
  
  
  # Extract coefficients
  g.DV.P.VI  <- tidy(DV.P.VI, conf.int = TRUE)
  g.DV.P.CD  <- tidy(DV.P.CD, conf.int = TRUE)
  g.DV.P.CL  <- tidy(DV.P.CL, conf.int = TRUE)
  g.DV.P.RE  <- tidy(DV.P.RE, conf.int = TRUE)
  g.DV.P.UC  <- tidy(DV.P.UC, conf.int = TRUE)
  g.DV.P.All <- tidy(DV.P.All, conf.int = TRUE)
  
  # Add model name to each data.frame
  g.DV.P.VI   <-data.frame(g.DV.P.VI,Paramilitaries = "VI")[c(3,4),]
  g.DV.P.CD   <-data.frame(g.DV.P.CD,Paramilitaries = "CD")[c(3,4),]
  g.DV.P.CL   <-data.frame(g.DV.P.CL,Paramilitaries = "CL")[c(3,4),]
  g.DV.P.RE   <-data.frame(g.DV.P.RE,Paramilitaries = "RE")[c(3,4),]
  g.DV.P.UC   <-data.frame(g.DV.P.UC,Paramilitaries = "UC")[c(3,4),]
  g.DV.P.All  <-data.frame(g.DV.P.All,Paramilitaries = "All")[c(3,4),]
  
  # Create a combined data frame
  ComFrm.DV.P <- data.frame(rbind(g.DV.P.VI, g.DV.P.CD, g.DV.P.CL, g.DV.P.RE, g.DV.P.UC, g.DV.P.All))  
  
  
  # Add Number of Obs to Coefficients
  ComFrm.DV.P <- cbind(ComFrm.DV.P,n.DV.P)
  
  # Standardize N as ratio
  ComFrm.DV.P$obs.st <- (ComFrm.DV.P$n.DV.P/max(ComFrm.DV.P$n.DV.P))*3
  
  
  # Update point size
  update_geom_defaults("pointrange", list(fatten=ComFrm.DV.P$obs.st))
  
  
  # Generate plot
  g.DV.P <- ggplot(ComFrm.DV.P, aes(colour = Paramilitaries)) +
    geom_hline(yintercept = 0, colour = gray(0.5), lty = 2) +
    geom_linerange(aes(x = term, 
                       ymin = conf.low,
                       ymax = conf.high),
                   lwd = 2, position = position_dodge(width=0.5)) + 
    geom_pointrange(aes(x = term, 
                        y = estimate,
                        ymin = conf.low,
                        ymax = conf.high),
                    fatten = ComFrm.DV.P$obs.st*2,
                    lwd=1, 
                    position = position_dodge(width=0.5),
                    shape = 16) + ylim(-.6, 1) + 
    labs(x="", y="Paramilitary Presence") + 
    theme_bw() + 
    theme(legend.position="bottom") + guides(col = guide_legend(title = "", nrow = 2)) +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
    coord_flip()  +
    theme(text = element_text(size = 18),legend.text = element_text(size = 16))+
    xlab("Coffe shocks        Oil shocks")
  
  g.DV.P
  
  # Save graph
  pdf("./graphs/coeffplot/coeff_dv_par_2.pdf", width=3.5, height=5.5)
  g.DV.P
  dev.off()
  
  png("./graphs/coeffplot/coeff_dv_par_2.png")
  g.DV.P
  dev.off()
  
  




### Guerrilla   ----------------------------

# Regression analysis of Guerrilla

  
  DV.G.VI  <-plm(guerrilla_vi ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.G.VI)
  
  DV.G.CD  <-plm(guerrilla_cd ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.G.CD)
  
  DV.G.CL  <-plm(guerrilla_cl ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.G.CL)
  
  DV.G.RE  <-plm(guerrilla_re ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.G.RE)
  
  DV.G.UC  <-plm(guerrilla_uc ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.G.UC)
  
  DV.G.All  <-plm(guerrilla_all ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                    X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                  | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                  data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.G.All)


# Plot coefficients of Guerrilla

  
  # Replicate Figure 6b Guerrilla presence
  
  # Load packages
  library(broom)       # To clean up data
  library(ggplot2)     # Plot coefficients
  library(dplyr)       # Subset data by row
  
  
  # Get Number of observations
  n.DV.G.VI  <- nobs(DV.G.VI)
  n.DV.G.CD  <- nobs(DV.G.CD)
  n.DV.G.CL  <- nobs(DV.G.CL)
  n.DV.G.RE  <- nobs(DV.G.RE)
  n.DV.G.UC  <- nobs(DV.G.UC)
  n.DV.G.All  <- nobs(DV.G.All)
  
  
  # Get array of obs
  n.DV.G <- c(n.DV.G.VI,n.DV.G.CD,n.DV.G.CL,n.DV.G.RE,n.DV.G.UC,n.DV.G.All)
  n.DV.G <- as.data.frame(n.DV.G)
  
  
  # Extract coefficients
  g.DV.G.VI  <- tidy(DV.G.VI, conf.int = TRUE)
  g.DV.G.CD  <- tidy(DV.G.CD, conf.int = TRUE)
  g.DV.G.CL  <- tidy(DV.G.CL, conf.int = TRUE)
  g.DV.G.RE  <- tidy(DV.G.RE, conf.int = TRUE)
  g.DV.G.UC  <- tidy(DV.G.UC, conf.int = TRUE)
  g.DV.G.All <- tidy(DV.G.All, conf.int = TRUE)
  
  # Add model name to each data.frame
  g.DV.G.VI   <-data.frame(g.DV.G.VI,Guerrilla = "VI")[c(3,4),]
  g.DV.G.CD   <-data.frame(g.DV.G.CD,Guerrilla = "CD")[c(3,4),]
  g.DV.G.CL   <-data.frame(g.DV.G.CL,Guerrilla = "CL")[c(3,4),]
  g.DV.G.RE   <-data.frame(g.DV.G.RE,Guerrilla = "RE")[c(3,4),]
  g.DV.G.UC   <-data.frame(g.DV.G.UC,Guerrilla = "UC")[c(3,4),]
  g.DV.G.All  <-data.frame(g.DV.G.All,Guerrilla = "All")[c(3,4),]
  
  # Create a combined data frame
  ComFrm.DV.G <- data.frame(rbind(g.DV.G.VI, g.DV.G.CD, g.DV.G.CL, g.DV.G.RE, g.DV.G.UC, g.DV.G.All))  
  
  
  # Add Number of Obs to Coefficients
  ComFrm.DV.G <- cbind(ComFrm.DV.G,n.DV.G)
  
  # Standardize N as ratio
  ComFrm.DV.G$obs.st <- (ComFrm.DV.G$n.DV.G/max(ComFrm.DV.G$n.DV.G))*3
  
  
  # Update point size
  update_geom_defaults("pointrange", list(fatten=ComFrm.DV.G$obs.st))
  
  
  # Generate plot
  g.DV.G <- ggplot(ComFrm.DV.G, aes(colour = Guerrilla)) +
    geom_hline(yintercept = 0, colour = gray(0.5), lty = 2) +
    geom_linerange(aes(x = term, 
                       ymin = conf.low,
                       ymax = conf.high),
                   lwd = 2, position = position_dodge(width=0.5)) + 
    geom_pointrange(aes(x = term, 
                        y = estimate,
                        ymin = conf.low,
                        ymax = conf.high),
                    fatten = ComFrm.DV.G$obs.st*2,
                    lwd=1, 
                    position = position_dodge(width=0.5),
                    shape = 16) + ylim(-.6, 1) + 
    labs(x="", y="Guerrilla Presence") + 
    theme_bw() + 
    theme(legend.position="bottom") + guides(col = guide_legend(title = "", nrow = 2)) +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
    coord_flip()  +
    theme(text = element_text(size = 18),legend.text = element_text(size = 16)) +
    xlab("Coffe shocks        Oil shocks")
  
  g.DV.G
  
  
  # Save graph
  pdf("./graphs/coeffplot/coeff_dv_guer_2.pdf", width=3.5, height=5.5)
  g.DV.G
  dev.off()
  
  png("./graphs/coeffplot/coeff_dv_guer_2.png")
  g.DV.G
  dev.off()
  




### FARC   ----------------------------

# Regression analysis of FARC

  
  DV.F.VI  <-plm(farc_vi ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.F.VI)
  
  DV.F.CD  <-plm(farc_cd ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.F.CD)
  
  DV.F.RC  <-plm(farc_rc ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.F.RC)
  
  DV.F.RE  <-plm(farc_re ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.F.RE)
  
  DV.F.UC  <-plm(farc_uc ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.F.UC)
  
  DV.F.All  <-plm(farc_all ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.F.All)  
  


# Plot coefficients of FARC

  
  # Replicate Figure 6c FARC presence
  
  # Load packages
  library(broom)       # To clean up data
  library(ggplot2)     # Plot coefficients
  library(dplyr)       # Subset data by row
  
  
  # Get Number of observations
  n.DV.F.VI  <- nobs(DV.F.VI)
  n.DV.F.CD  <- nobs(DV.F.CD)
  n.DV.F.RC  <- nobs(DV.F.RC)
  n.DV.F.RE  <- nobs(DV.F.RE)
  n.DV.F.UC  <- nobs(DV.F.UC)
  n.DV.F.All  <- nobs(DV.F.All)
  
  
  # Get array of obs
  n.DV.G <- c(n.DV.F.VI,n.DV.F.CD,n.DV.F.RC,n.DV.F.RE,n.DV.F.UC,n.DV.F.All)#)
  n.DV.G <- as.data.frame(n.DV.G)
  
  
  # Extract coefficients
  g.DV.F.VI  <- tidy(DV.F.VI, conf.int = TRUE)
  g.DV.F.CD  <- tidy(DV.F.CD, conf.int = TRUE)
  g.DV.F.RC  <- tidy(DV.F.RC, conf.int = TRUE)
  g.DV.F.RE  <- tidy(DV.F.RE, conf.int = TRUE)
  g.DV.F.UC  <- tidy(DV.F.UC, conf.int = TRUE)
  g.DV.F.All <- tidy(DV.F.All, conf.int = TRUE)
  
  # Add model name to each data.frame
  g.DV.F.VI   <-data.frame(g.DV.F.VI,FARC = "VI")[c(3,4),]
  g.DV.F.CD   <-data.frame(g.DV.F.CD,FARC = "CD")[c(3,4),]
  g.DV.F.RC   <-data.frame(g.DV.F.RC,FARC = "CL")[c(3,4),]
  g.DV.F.RE   <-data.frame(g.DV.F.RE,FARC = "RE")[c(3,4),]
  g.DV.F.UC   <-data.frame(g.DV.F.UC,FARC = "UC")[c(3,4),]
  g.DV.F.All  <-data.frame(g.DV.F.All,FARC = "All")[c(3,4),]
  
  # Create a combined data frame
  ComFrm.DV.F <- data.frame(rbind(g.DV.F.VI, g.DV.F.CD, g.DV.F.RC, g.DV.F.RE, g.DV.F.UC,g.DV.F.All))#))  
  
  
  # Add Number of Obs to Coefficients
  ComFrm.DV.F <- cbind(ComFrm.DV.F,n.DV.G)
  
  # Standardize N as ratio
  ComFrm.DV.F$obs.st <- (ComFrm.DV.F$n.DV.G/max(ComFrm.DV.F$n.DV.G))*3
  
  
  # Update point size
  update_geom_defaults("pointrange", list(fatten=ComFrm.DV.F$obs.st))
  
  
  # Generate plot
  g.DV.F <- ggplot(ComFrm.DV.F, aes(colour = FARC)) +
    geom_hline(yintercept = 0, colour = gray(0.5), lty = 2) +
    geom_linerange(aes(x = term, 
                       ymin = conf.low,
                       ymax = conf.high),
                   lwd = 2, position = position_dodge(width=0.5)) + 
    geom_pointrange(aes(x = term, 
                        y = estimate,
                        ymin = conf.low,
                        ymax = conf.high),
                    fatten = ComFrm.DV.F$obs.st*2,
                    lwd=1, 
                    position = position_dodge(width=0.5),
                    shape = 16) + ylim(-.6, 1) + 
    labs(x="", y="FARC Presence") + 
    theme_bw() + 
    theme(legend.position="bottom") + guides(col = guide_legend(title = "", nrow = 2)) +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
    coord_flip()  +
    theme(text = element_text(size = 18),legend.text = element_text(size = 16)) +
    xlab("Coffe shocks            Oil shocks")
  
  g.DV.F
  
  # Save graph
  pdf("./graphs/coeffplot/coeff_dv_farc_2.pdf", width=3.5, height=5.5)
  g.DV.F
  dev.off()
  
  png("./graphs/coeffplot/coeff_dv_farc_2.png")
  g.DV.F
  dev.off()
  




### ELN   ----------------------------

# Regression analysis of ELN

  
  DV.E.VI  <-plm(eln_vi ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.E.VI)
  
  DV.E.CD  <-plm(eln_cd ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.E.CD)
  
  DV.E.RC  <-plm(eln_rc ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.E.RC)
  
  DV.E.UC  <-plm(eln_uc ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.E.UC)
  
  DV.E.All  <-plm(eln_all ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.E.All)

  


## Plot coefficients of ELN

  # Load packages
  library(broom)       # To clean up data
  library(ggplot2)     # Plot coefficients
  library(dplyr)       # Subset data by row
  
  
  # Get Number of observations
  n.DV.E.VI  <- nobs(DV.E.VI)
  n.DV.E.CD  <- nobs(DV.E.CD)
  n.DV.E.RC  <- nobs(DV.E.RC)
  #n.DV.E.RE  <- nobs(DV.E.RE)
  n.DV.E.UC  <- nobs(DV.E.UC)
  n.DV.E.All  <- nobs(DV.E.All)
  
  
  # Get array of obs
  n.DV.G <- c(n.DV.E.VI,n.DV.E.CD,n.DV.E.RC,n.DV.E.UC,n.DV.E.All)#,n.DV.E.RE)
  n.DV.G <- as.data.frame(n.DV.G)
  
  
  # Extract coefficients
  g.DV.E.VI  <- tidy(DV.E.VI, conf.int = TRUE)
  g.DV.E.CD  <- tidy(DV.E.CD, conf.int = TRUE)
  g.DV.E.RC  <- tidy(DV.E.RC, conf.int = TRUE)
  #g.DV.E.RE  <- tidy(DV.E.RE, conf.int = TRUE)
  g.DV.E.UC  <- tidy(DV.E.UC, conf.int = TRUE)
  g.DV.E.All <- tidy(DV.E.All, conf.int = TRUE)
  
  # Add model name to each data.frame
  g.DV.E.VI   <-data.frame(g.DV.E.VI,ELN = "VI")[c(3,4),]
  g.DV.E.CD   <-data.frame(g.DV.E.CD,ELN = "CD")[c(3,4),]
  g.DV.E.RC   <-data.frame(g.DV.E.RC,ELN = "CL")[c(3,4),]
  #g.DV.E.RE   <-data.frame(g.DV.E.RE,ELN = "RE")[c(3,4),]
  g.DV.E.UC   <-data.frame(g.DV.E.UC,ELN = "UC")[c(3,4),]
  g.DV.E.All  <-data.frame(g.DV.E.All,ELN = "All")[c(3,4),]
  
  # Create a combined data frame
  ComFrm.DV.E <- data.frame(rbind(g.DV.E.VI, g.DV.E.CD, g.DV.E.RC, g.DV.E.UC,g.DV.E.All))#, g.DV.E.RE))  
  
  
  # Add Number of Obs to Coefficients
  ComFrm.DV.E <- cbind(ComFrm.DV.E,n.DV.G)
  
  # Standardize N as ratio
  ComFrm.DV.E$obs.st <- (ComFrm.DV.E$n.DV.G/max(ComFrm.DV.E$n.DV.G))*3
  
  # Update point size
  update_geom_defaults("pointrange", list(fatten=ComFrm.DV.E$obs.st))
  
  
  # Generate plot
  g.DV.E <- ggplot(ComFrm.DV.E, aes(colour = ELN)) +
    geom_hline(yintercept = 0, colour = gray(0.5), lty = 2) +
    geom_linerange(aes(x = term, 
                       ymin = conf.low,
                       ymax = conf.high),
                   lwd = 2, position = position_dodge(width=0.5)) + 
    geom_pointrange(aes(x = term, 
                        y = estimate,
                        ymin = conf.low,
                        ymax = conf.high),
                    fatten = ComFrm.DV.E$obs.st*2,
                    lwd=1, 
                    position = position_dodge(width=0.5),
                    shape = 16) + ylim(-0.6, 1) + 
    labs(x="", y="ELN Presence") + 
    theme_bw() + 
    theme(legend.position="bottom") + guides(col = guide_legend(title = "", nrow = 2)) +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
    coord_flip()  +
    theme(text = element_text(size = 18),legend.text = element_text(size = 16))+
    xlab("Coffe shocks            Oil shocks")
  
  g.DV.E
  
  # Save graph
  pdf("./graphs/coeffplot/coeff_dv_eln_2.pdf", width=3.5, height=5.5)
  g.DV.E
  dev.off()
  
  png("./graphs/coeffplot/coeff_dv_eln_2.png")
  g.DV.E
  dev.off()
  




### AUC   ----------------------------

# Regression analysis of AUC
{
  
  DV.A.VI  <-plm(auc_vi ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.A.VI)
  
  DV.A.CD  <-plm(auc_cd ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.A.CD)
  
  DV.A.RE  <-plm(auc_re ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.A.RE)
  
  DV.A.UC  <-plm(auc_uc ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.A.UC)
  
  DV.A.All  <-plm(auc_all ~ lpop+ coca94indxyear + oilprod88xlop + cofintxlinternalp + 
                   X_Yyear_1989 + X_Yyear_1990 + X_Yyear_1991 +  + X_Yyear_1992 + X_Yyear_1993 + X_Yyear_1994 +  + X_Yyear_1995 + X_Yyear_1996 + X_Yyear_1997 +  + X_Yyear_1998 + X_Yyear_1999 + X_Yyear_2000 +  + X_Yyear_2001 + X_Yyear_2002 + X_Yyear_2003 +  + X_Yyear_2004 + X_Yyear_2005 + X_Rregion_2 + X_Rregion_3 + X_Rregion_4 
                 | . - cofintxlinternalp + rxltop3cof + txltop3cof + rtxltop3cof ,
                 data=dv_data , model = "within", effect = "individual", index=c("mun", "year")) # 
  summary(DV.A.All)

  }


## Plot coefficients of AUC
{
  # Load packages
  library(broom)       # To clean up data
  library(ggplot2)     # Plot coefficients
  library(dplyr)       # Subset data by row
  
  
  # Get Number of observations
  n.DV.A.VI  <- nobs(DV.A.VI)
  n.DV.A.CD  <- nobs(DV.A.CD)
  #n.DV.A.RC  <- nobs(DV.A.RC)
  n.DV.A.RE  <- nobs(DV.A.RE)
  n.DV.A.UC  <- nobs(DV.A.UC)
  n.DV.A.All  <- nobs(DV.A.All)
  
  
  # Get array of obs
  n.DV.G <- c(n.DV.A.VI,n.DV.A.CD,n.DV.A.RE,n.DV.A.UC,n.DV.A.All)#,n.DV.A.RC)
  n.DV.G <- as.data.frame(n.DV.G)
  
  
  # Extract coefficients
  g.DV.A.VI  <- tidy(DV.A.VI, conf.int = TRUE)
  g.DV.A.CD  <- tidy(DV.A.CD, conf.int = TRUE)
  #g.DV.A.RC  <- tidy(DV.A.RC, conf.int = TRUE)
  g.DV.A.RE  <- tidy(DV.A.RE, conf.int = TRUE)
  g.DV.A.UC  <- tidy(DV.A.UC, conf.int = TRUE)
  g.DV.A.All <- tidy(DV.A.All, conf.int = TRUE)
  
  # Add model name to each data.frame
  g.DV.A.VI   <-data.frame(g.DV.A.VI,ELN = "VI")[c(3,4),]
  g.DV.A.CD   <-data.frame(g.DV.A.CD,ELN = "CD")[c(3,4),]
  #g.DV.A.RC   <-data.frame(g.DV.A.RC,ELN = "CL")[c(3,4),]
  g.DV.A.RE   <-data.frame(g.DV.A.RE,ELN = "RE")[c(3,4),]
  g.DV.A.UC   <-data.frame(g.DV.A.UC,ELN = "UC")[c(3,4),]
  g.DV.A.All  <-data.frame(g.DV.A.All,ELN = "All")[c(3,4),]
  
  # Create a combined data frame
  ComFrm.DV.A <- data.frame(rbind(g.DV.A.VI, g.DV.A.CD, g.DV.A.RE, g.DV.A.UC,g.DV.A.All))#, g.DV.A.RC))  
  
  
  # Add Number of Obs to Coefficients
  ComFrm.DV.A <- cbind(ComFrm.DV.A,n.DV.G)
  
  # Standardize N as ratio
  ComFrm.DV.A$obs.st <- (ComFrm.DV.A$n.DV.G/max(ComFrm.DV.A$n.DV.G))*3
  
  
  # Update point size
  update_geom_defaults("pointrange", list(fatten=ComFrm.DV.A$obs.st))
  
  
  # Generate plot
  g.DV.A <- ggplot(ComFrm.DV.A, aes(colour = ELN)) +
    geom_hline(yintercept = 0, colour = gray(0.5), lty = 2) +
    geom_linerange(aes(x = term, 
                       ymin = conf.low,
                       ymax = conf.high),
                   lwd = 2, position = position_dodge(width=0.5)) + 
    geom_pointrange(aes(x = term, 
                        y = estimate,
                        ymin = conf.low,
                        ymax = conf.high),
                    fatten = ComFrm.DV.A$obs.st*2,
                    lwd=1, 
                    position = position_dodge(width=0.5),
                    shape = 16) + ylim(-0.6, 1) + 
    labs(x="", y="AUC Presence") + 
    theme_bw() + 
    theme(legend.position="bottom") + guides(col = guide_legend(title = "", nrow = 2)) +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
    coord_flip()  +
    theme(text = element_text(size = 18),legend.text = element_text(size = 16)) +
    xlab("Coffe shocks            Oil shocks")
  
  
  g.DV.A
  
  
  
  # Save graph
  pdf("./graphs/coeffplot/coeff_dv_auc_2.pdf", width=3.5, height=5.5)
  g.DV.A
  dev.off()
  
  png("./graphs/coeffplot/coeff_dv_auc_2.png")
  g.DV.A
  dev.off()
  
  
}


# 14. JACCARD FOR EACH PAIR OF MEASUREMENT SETS  ############################################################


## 14.1 Generate Jaccard Similarity    ----------------------------


  ### Guerrilla    ----------------------------
  
  # Delete dataloop if exists
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  
  # Generate data loop
  dataloop <- Guerrilla
  
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  
  # Gen empty data frame
  jaccard.pair.g <- data.frame(NA)
  

  # Loop from min to max
  for(i in 1:dim(dataloop)[2]){
    for(j in 1:dim(dataloop)[2]){
      

      # Delete dataloop if exists
      if(exists("subdataloop")==TRUE){rm(subdataloop)}else{NA}
      if(exists("subdataloop1")==TRUE){rm(subdataloop1)}else{NA}
      if(exists("jaccard.pair1")==TRUE){rm(jaccard.pair1)}else{NA}
      

      # Generate index
      index <- paste0(i,j)
      

      # Ignore if i=j
      if(i!=j){
        
        

        # This is the main function
        
        # Subset the data to relevant pairs
        subdataloop <- dataloop[,c(i,j)]
        

        # Create databases
        
        # Eliminate any row with NA
        subdataloop1 <-  na.omit(subdataloop)
        

        # Calculate Jaccard Similarity per pair
        
        # Jaccard 
        jaccard.pair1 <- betastatjac(subdataloop1)[4]
        
        # Eliminate number name
        names(jaccard.pair1) <- NULL
        
        # Identify longest data base
        longest <- max(length(which(!is.na(subdataloop[1]))),length(which(!is.na(subdataloop[2]))))
        longest
        

        # Calculate weights
        w1 <- nrow(subdataloop1)/longest
        w2 <- 1-w1
        
        

        # Extract object
        jaccard.pair.g[index] <- (jaccard.pair1*w1)
        
        
      } # End of if
    } # End loop 2
  } # End loop 1
  
  
  
  ### Paramilitaries     ----------------------------
  
  # Delete dataloop if exists
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  
  # Generate data loop
  dataloop <- Paras
  
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  
  # Eliminate IN
  dataloop <- dataloop[,c(1:4,6:7)]
  names.dataloop.p <- names(dataloop)
  
  # Gen empty data frame
  jaccard.pair.p <- data.frame(NA)
  

  # Loop from min to max
  for(i in 1:dim(dataloop)[2]){
    for(j in 1:dim(dataloop)[2]){
      # for(i in c(1:5)){
      #   for(j in c(1:5)){
      #if(i!=j){     
      #print(paste0(i,j)) }}}
      

      # Delete dataloop if exists
      if(exists("subdataloop")==TRUE){rm(subdataloop)}else{NA}
      if(exists("subdataloop1")==TRUE){rm(subdataloop1)}else{NA}
      if(exists("jaccard.pair1")==TRUE){rm(jaccard.pair1)}else{NA}
      

      # Generate index
      index <- paste0(i,j)  
      

      # Ignore if i=j
      if(i!=j){
        

        # This is the main function
        
        # Subset the data to relevant pairs
        subdataloop <- dataloop[,c(i,j)]
        

        # Create databases
        
        # Eliminate any row with NA
        subdataloop1 <-  na.omit(subdataloop)
        

        # Calculate Jaccard Similarity per pair
        
        # Jaccard 
        jaccard.pair1 <- betastatjac(subdataloop1)[4]
        
        # Eliminate number name
        names(jaccard.pair1) <- NULL
        
        # Identify longest data base
        longest <- max(length(which(!is.na(subdataloop[1]))),length(which(!is.na(subdataloop[2]))))
        longest
        

        # Calculate weights
        w1 <- nrow(subdataloop1)/longest
        w2 <- 1-w1
        
        

        # Extract object
        jaccard.pair.p[index] <- (jaccard.pair1*w1)
        
      } # End of if
    } # End loop 2
  } # End loop 1
  
  

  ### FARC     ----------------------------
  
  # Delete dataloop if exists
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  
  # Generate data loop
  dataloop <- ALL.FARC
  
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  
  # Gen empty data frame
  jaccard.pair.f <- data.frame(NA)
  

  # Loop from min to max
  for(i in 1:dim(dataloop)[2]){
    for(j in 1:dim(dataloop)[2]){
      

      # Delete dataloop if exists
      if(exists("subdataloop")==TRUE){rm(subdataloop)}else{NA}
      if(exists("subdataloop1")==TRUE){rm(subdataloop1)}else{NA}
      if(exists("jaccard.pair1")==TRUE){rm(jaccard.pair1)}else{NA}
      

      # Generate index
      index <- paste0(i,j)
      

      # Ignore if i=j
      if(i!=j){
        

        # This is the main function
        
        # Subset the data to relevant pairs
        subdataloop <- dataloop[,c(i,j)]
        

        # Create databases
        
        # Eliminate any row with NA
        subdataloop1 <-  na.omit(subdataloop)
        

        # Calculate Jaccard Similarity per pair
        
        # Jaccard 
        jaccard.pair1 <- betastatjac(subdataloop1)[4]
        
        # Eliminate number name
        names(jaccard.pair1) <- NULL
        
        # Identify longest data base
        longest <- max(length(which(!is.na(subdataloop[1]))),length(which(!is.na(subdataloop[2]))))
        longest
        

        # Calculate weights
        w1 <- nrow(subdataloop1)/longest
        w2 <- 1-w1
        
        
        # Extract object
        jaccard.pair.f[index] <- (jaccard.pair1*w1)
        
      } # End of if
    } # End loop 2
  } # End loop 1
  
  

  ### ELN      ----------------------------
  
  # Delete dataloop if exists
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  
  # Generate data loop
  dataloop <- ALL.ELN
  
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  
  # Gen empty data frame
  jaccard.pair.e <- data.frame(NA)
  

  # Loop from min to max
  for(i in 1:dim(dataloop)[2]){
    for(j in 1:dim(dataloop)[2]){
      

      # Delete dataloop if exists
      if(exists("subdataloop")==TRUE){rm(subdataloop)}else{NA}
      if(exists("subdataloop1")==TRUE){rm(subdataloop1)}else{NA}
      if(exists("jaccard.pair1")==TRUE){rm(jaccard.pair1)}else{NA}
      

      # Generate index
      index <- paste0(i,j)
      

      # Ignore if i=j
      if(i!=j){
        

        # This is the main function
        
        # Subset the data to relevant pairs
        subdataloop <- dataloop[,c(i,j)]
        

        # Create databases
        
        # Eliminate any row with NA
        subdataloop1 <-  na.omit(subdataloop)
        

        # Calculate Jaccard Similarity per pair
        
        # Jaccard 
        jaccard.pair1 <- betastatjac(subdataloop1)[4]
        
        # Eliminate number name
        names(jaccard.pair1) <- NULL
        
        # Identify longest data base
        longest <- max(length(which(!is.na(subdataloop[1]))),length(which(!is.na(subdataloop[2]))))
        longest
        

        # Calculate weights
        w1 <- nrow(subdataloop1)/longest
        w2 <- 1-w1
        
        

        # Extract object
        jaccard.pair.e[index] <- (jaccard.pair1*w1)
        
      } # End of if
    } # End loop 2
  } # End loop 1
  
  
  
  
  ### AUC      ----------------------------
  
  # Delete dataloop if exists
  if(exists("dataloop")==TRUE){rm(dataloop)}else{NA}
  
  # Generate data loop
  dataloop <- ALL.AUC
  
  # Eliminate year variable
  dataloop <- dataloop[ -c(1) ]
  
  # Gen empty data frame
  jaccard.pair.a <- data.frame(NA)
  

  # Loop from min to max
  for(i in 1:dim(dataloop)[2]){
    for(j in 1:dim(dataloop)[2]){
      

      # Delete dataloop if exists
      if(exists("subdataloop")==TRUE){rm(subdataloop)}else{NA}
      if(exists("subdataloop1")==TRUE){rm(subdataloop1)}else{NA}
      if(exists("jaccard.pair1")==TRUE){rm(jaccard.pair1)}else{NA}
      

      # Generate index
      index <- paste0(i,j)
      

      # Ignore if i=j
      if(i!=j){
        

        # This is the main function
        
        # Subset the data to relevant pairs
        subdataloop <- dataloop[,c(i,j)]
        

        # Create databases
        
        # Eliminate any row with NA
        subdataloop1 <-  na.omit(subdataloop)
        

        # Calculate Jaccard Similarity per pair
        
        # Jaccard 
        jaccard.pair1 <- betastatjac(subdataloop1)[4]
        
        # Eliminate number name
        names(jaccard.pair1) <- NULL
        
        # Identify longest data base
        longest <- max(length(which(!is.na(subdataloop[1]))),length(which(!is.na(subdataloop[2]))))
        longest
        

        # Calculate weights
        w1 <- nrow(subdataloop1)/longest
        w2 <- 1-w1
        
        

        # Extract object
        jaccard.pair.a[index] <- (jaccard.pair1*w1)
        
      } # End of if
    } # End loop 2
  } # End loop 1
  




## 14.2 Plot Similarity by Pairs    ----------------------------


### Guerrilla    ----------------------------

  
# Replicate Figure 3b Pairwise Jaccard Similarity of Guerrilla
  
# Generate matrix

dim(jaccard.pair.g)
names(jaccard.pair.g)

# Generate matrix
matrix.pair.g <- data.frame(cbind(
  c(as.numeric(1),as.numeric(jaccard.pair.g[2]),as.numeric(jaccard.pair.g[3]),as.numeric(jaccard.pair.g[4]),as.numeric(jaccard.pair.g[5]),as.numeric(jaccard.pair.g[6])),
  c(as.numeric(jaccard.pair.g[7]),as.numeric(1),as.numeric(jaccard.pair.g[8]),as.numeric(jaccard.pair.g[9]),as.numeric(jaccard.pair.g[10]),as.numeric(jaccard.pair.g[11])),
  c(as.numeric(jaccard.pair.g[12]),as.numeric(jaccard.pair.g[13]),as.numeric(1),as.numeric(jaccard.pair.g[14]),as.numeric(jaccard.pair.g[15]),as.numeric(jaccard.pair.g[16])),
  c(as.numeric(jaccard.pair.g[17]),as.numeric(jaccard.pair.g[18]),as.numeric(jaccard.pair.g[19]),as.numeric(1),as.numeric(jaccard.pair.g[20]),as.numeric(jaccard.pair.g[21])),
  c(as.numeric(jaccard.pair.g[22]),as.numeric(jaccard.pair.g[23]),as.numeric(jaccard.pair.g[24]),as.numeric(jaccard.pair.g[25]),as.numeric(1),as.numeric(jaccard.pair.g[26])),
  c(as.numeric(jaccard.pair.g[27]),as.numeric(jaccard.pair.g[28]),as.numeric(jaccard.pair.g[29]),as.numeric(jaccard.pair.g[30]),as.numeric(jaccard.pair.g[31]),as.numeric(1))
))

# Assign row and column names
matrix.pair.g <- as.data.frame(matrix.pair.g)
names(matrix.pair.g) <- names(Guerrilla)[-1]
row.names(matrix.pair.g) <- names(Guerrilla)[-1]

# Melt the data
matrix.pair.g  <- as.matrix(matrix.pair.g)
matrix.pair.g  <- melt(matrix.pair.g)
matrix.pair.g  <- matrix.pair.g[,1:3]
colnames(matrix.pair.g) <- c("x", "y", "value")

table(matrix.pair.g$x)

# Order by c("IN", "CL", "RC", "VI", "RE", "CD","UC")
matrix.pair.g$ord.x <- factor(matrix.pair.g$x, ordered=TRUE, levels = c("CL", "RC", "VI", "RE", "CD","UC"))
matrix.pair.g$ord.y <- factor(matrix.pair.g$y, ordered=TRUE, levels = c("CL", "RC", "VI", "RE", "CD","UC"))





# Plot

pairf1.g2 <- ggplot(matrix.pair.g, aes(x=ord.x, y=ord.y, fill = value)) +
  geom_tile() + 
  scale_fill_gradientn(colours = magma(10),na.value = NA, limits=c(0,1),breaks=c(0,.2,.4,.6,.8,1)) +
  xlab("") + ylab("") +
  guides(fill=guide_legend(title="Jaccard")) + 
  coord_flip() +
  geom_text(aes(ord.x, ord.y, label=round(value, digits = 2)), color=ifelse(matrix.pair.g$value<1,"white","black"), check_overlap = TRUE) +
  geom_rect(aes(xmin = 3.5, xmax = 6.5, ymin = 3.5, ymax = 6.5),
            fill = NA, color = "red",linetype = "solid", size=1.5) + 
  geom_rect(aes(xmin = 0.5, xmax = 3.5, ymin = 0.5, ymax = 3.5),
            fill = NA, color = "red",linetype = "dashed", size=1.5) 
pairf1.g2

# Save graph
pdf("./graphs/pairs/pair_jaccard_g2.pdf", width=4, height=3.5)
pairf1.g2
dev.off()





### Paramilitaries    ----------------------------

# Replicate Figure 3a Pairwise Jaccard Similarity of Paramilitaries
  

# Generate matrix

names(Paras)
dim(jaccard.pair.p)
names(jaccard.pair.p)

# Generate matrix
matrix.pair.p <- data.frame(cbind(
  c(as.numeric(1),as.numeric(jaccard.pair.p[2]),as.numeric(jaccard.pair.p[3]),as.numeric(jaccard.pair.p[4]),as.numeric(jaccard.pair.p[5]),as.numeric(jaccard.pair.p[6])),
  c(as.numeric(jaccard.pair.p[7]),as.numeric(1),as.numeric(jaccard.pair.p[8]),as.numeric(jaccard.pair.p[9]),as.numeric(jaccard.pair.p[10]),as.numeric(jaccard.pair.p[11])),
  c(as.numeric(jaccard.pair.p[12]),as.numeric(jaccard.pair.p[13]),as.numeric(1),as.numeric(jaccard.pair.p[14]),as.numeric(jaccard.pair.p[15]),as.numeric(jaccard.pair.p[16])),
  c(as.numeric(jaccard.pair.p[17]),as.numeric(jaccard.pair.p[18]),as.numeric(jaccard.pair.p[19]),as.numeric(1),as.numeric(jaccard.pair.p[20]),as.numeric(jaccard.pair.p[21])),
  c(as.numeric(jaccard.pair.p[22]),as.numeric(jaccard.pair.p[23]),as.numeric(jaccard.pair.p[24]),as.numeric(jaccard.pair.p[25]),as.numeric(1),as.numeric(jaccard.pair.p[26])),
  c(as.numeric(jaccard.pair.p[27]),as.numeric(jaccard.pair.p[28]),as.numeric(jaccard.pair.p[29]),as.numeric(jaccard.pair.p[30]),as.numeric(jaccard.pair.p[31]),as.numeric(1))
))

# Assign row and column names
matrix.pair.p <- as.data.frame(matrix.pair.p)
names(matrix.pair.p) <- names.dataloop.p
row.names(matrix.pair.p) <- names.dataloop.p

# Melt the data
matrix.pair.p  <- as.matrix(matrix.pair.p)
matrix.pair.p  <- melt(matrix.pair.p)
matrix.pair.p  <- matrix.pair.p[,1:3]
colnames(matrix.pair.p) <- c("x", "y", "value")

table(matrix.pair.p$x) 

# Order by c("IN", "CL", "RC", "VI", "RE", "CD","UC")
matrix.pair.p$ord.x <- factor(matrix.pair.p$x, ordered=TRUE, levels = c("CL", "RC", "VI", "RE", "CD","UC"))
matrix.pair.p$ord.y <- factor(matrix.pair.p$y, ordered=TRUE, levels = c("CL", "RC", "VI", "RE", "CD","UC"))




# Plot

pairf1.p2 <- ggplot(matrix.pair.p, aes(x=ord.x, y=ord.y, fill = value)) +
  geom_tile() + 
  #scale_fill_gradient(low = "cyan1", high = "navyblue", na.value = NA, limits=c(0,1),breaks=c(0,.2,.4,.6,.8,1)) + 
  scale_fill_gradientn(colours = magma(10),na.value = NA, limits=c(0,1),breaks=c(0,.2,.4,.6,.8,1)) +
  xlab("") + ylab("") +
  guides(fill=guide_legend(title="Jaccard")) + 
  coord_flip() +
  #geom_text(aes(ord.x, ord.y, label=round(value, digits = 2)), colour = "black", check_overlap = TRUE)  +
  geom_text(aes(ord.x, ord.y, label=round(value, digits = 2)), color=ifelse(matrix.pair.p$value<1,"white","black"), check_overlap = TRUE) +
  geom_rect(aes(xmin = 3.5, xmax = 6.5, ymin = 3.5, ymax = 6.5),
            fill = NA, color = "red",linetype = "solid", size=1.5) + 
  geom_rect(aes(xmin = 0.5, xmax = 3.5, ymin = 0.5, ymax = 3.5),
            fill = NA, color = "red",linetype = "dashed", size=1.5)

pairf1.p2


# Save graph
pdf("./graphs/pairs/pair_jaccard_p2.pdf", width=4, height=3.5)
pairf1.p2
dev.off()




### FARC    ----------------------------

# Replicate Figure 3c Pairwise Jaccard Similarity of FARC
  
  
# Generate matrix

names(ALL.FARC)
dim(jaccard.pair.f)
names(jaccard.pair.f)

# Generate matrix
matrix.pair.f <- data.frame(cbind(
  c(as.numeric(1),as.numeric(jaccard.pair.f[2]),as.numeric(jaccard.pair.f[3]),as.numeric(jaccard.pair.f[4]),as.numeric(jaccard.pair.f[5])),
  c(as.numeric(jaccard.pair.f[6]),as.numeric(1),as.numeric(jaccard.pair.f[7]),as.numeric(jaccard.pair.f[8]),as.numeric(jaccard.pair.f[9])),
  c(as.numeric(jaccard.pair.f[10]),as.numeric(jaccard.pair.f[11]),as.numeric(1),as.numeric(jaccard.pair.f[12]),as.numeric(jaccard.pair.f[13])),
  c(as.numeric(jaccard.pair.f[14]),as.numeric(jaccard.pair.f[15]),as.numeric(jaccard.pair.f[16]),as.numeric(1),as.numeric(jaccard.pair.f[17])),
  c(as.numeric(jaccard.pair.f[18]),as.numeric(jaccard.pair.f[19]),as.numeric(jaccard.pair.f[20]),as.numeric(jaccard.pair.f[21]),as.numeric(1))
))


# Assign row and column names
matrix.pair.f <- as.data.frame(matrix.pair.f)
names(matrix.pair.f) <- names(ALL.FARC)[-1]
row.names(matrix.pair.f) <- names(ALL.FARC)[-1]

# Melt the data
matrix.pair.f  <- as.matrix(matrix.pair.f)
matrix.pair.f  <- melt(matrix.pair.f)
matrix.pair.f  <- matrix.pair.f[,1:3]
colnames(matrix.pair.f) <- c("x", "y", "value")

table(matrix.pair.f$x)

# Order by c("IN", "CL", "RC", "VI", "RE", "CD","UC")
matrix.pair.f$ord.x <- factor(matrix.pair.f$x, ordered=TRUE, levels = c("RC", "VI", "RE", "CD","UC"))
matrix.pair.f$ord.y <- factor(matrix.pair.f$y, ordered=TRUE, levels = c("RC", "VI", "RE", "CD","UC"))




# Plot

pairf1.f2 <- ggplot(matrix.pair.f, aes(x=ord.x, y=ord.y, fill = value)) +
  geom_tile() + 
  scale_fill_gradientn(colours = magma(10),na.value = NA, limits=c(0,1),breaks=c(0,.2,.4,.6,.8,1)) +
  xlab("") + ylab("") +
  guides(fill=guide_legend(title="Jaccard")) + 
  coord_flip() +
  geom_text(aes(ord.x, ord.y, label=round(value, digits = 2)), color=ifelse(matrix.pair.f$value<1,"white","black"), check_overlap = TRUE) +
  geom_rect(aes(xmin = 2.5, xmax = 5.5, ymin = 2.5, ymax = 5.5),
            fill = NA, color = "red",linetype = "solid", size=1.5) + 
  geom_rect(aes(xmin = 0.5, xmax = 2.5, ymin = 0.5, ymax = 2.5),
            fill = NA, color = "red",linetype = "dashed", size=1.5) 

pairf1.f2

# Save graph
pdf("./graphs/pairs/pair_jaccard_f2.pdf", width=4, height=3.5)
pairf1.f2
dev.off()




### ELN    ----------------------------

# Replicate Figure 3d Pairwise Jaccard Similarity of ELN
  

# Generate matrix

names(ALL.ELN)
dim(jaccard.pair.e)
names(jaccard.pair.e)

# Generate matrix
matrix.pair.e <- data.frame(cbind(
  c(as.numeric(1),as.numeric(jaccard.pair.e[2]),as.numeric(jaccard.pair.e[3]),as.numeric(jaccard.pair.e[4])),
  c(as.numeric(jaccard.pair.e[5]),as.numeric(1),as.numeric(jaccard.pair.e[6]),as.numeric(jaccard.pair.e[7])),
  c(as.numeric(jaccard.pair.e[8]),as.numeric(jaccard.pair.e[9]),as.numeric(1),as.numeric(jaccard.pair.e[10])),
  c(as.numeric(jaccard.pair.e[11]),as.numeric(jaccard.pair.e[12]),as.numeric(jaccard.pair.e[13]),as.numeric(1))
))


# Assign row and column names
matrix.pair.e <- as.data.frame(matrix.pair.e)
names(matrix.pair.e) <- names(ALL.ELN)[-1]
row.names(matrix.pair.e) <- names(ALL.ELN)[-1]

# Melt the data
matrix.pair.e  <- as.matrix(matrix.pair.e)
matrix.pair.e  <- melt(matrix.pair.e)
matrix.pair.e  <- matrix.pair.e[,1:3]
colnames(matrix.pair.e) <- c("x", "y", "value")

table(matrix.pair.e$x)

# Order by c("IN", "CL", "RC", "VI", "RE", "CD","UC")
matrix.pair.e$ord.x <- factor(matrix.pair.e$x, ordered=TRUE, levels = c("RC", "VI", "CD","UC"))
matrix.pair.e$ord.y <- factor(matrix.pair.e$y, ordered=TRUE, levels = c("RC", "VI", "CD","UC"))




# Plot

pairf1.e2 <- ggplot(matrix.pair.e, aes(x=ord.x, y=ord.y, fill = value)) +
  geom_tile() + 
  scale_fill_gradientn(colours = magma(10),na.value = NA, limits=c(0,1),breaks=c(0,.2,.4,.6,.8,1)) +
  xlab("") + ylab("") +
  guides(fill=guide_legend(title="Jaccard")) + 
  coord_flip() +
  geom_text(aes(ord.x, ord.y, label=round(value, digits = 2)), color=ifelse(matrix.pair.e$value<1,"white","black"), check_overlap = TRUE) +
  geom_rect(aes(xmin = 2.5, xmax = 4.5, ymin = 2.5, ymax = 4.5),
            fill = NA, color = "red",linetype = "solid", size=1.5) + 
  geom_rect(aes(xmin = 0.5, xmax = 2.5, ymin = 0.5, ymax = 2.5),
            fill = NA, color = "red",linetype = "dashed", size=1.5)

pairf1.e2

# Save graph
pdf("./graphs/pairs/pair_jaccard_e2.pdf", width=4, height=3.5)
pairf1.e2
dev.off()





### AUC    ----------------------------

  # Replicate Figure 3e Pairwise Jaccard Similarity of AUC
  

# Generate matrix

names(ALL.AUC)
dim(jaccard.pair.a)
names(jaccard.pair.a)

# Generate matrix
matrix.pair.a <- data.frame(cbind(
  c(as.numeric(1),as.numeric(jaccard.pair.a[2]),as.numeric(jaccard.pair.a[3]),as.numeric(jaccard.pair.a[4]),as.numeric(jaccard.pair.a[5])),
  c(as.numeric(jaccard.pair.a[6]),as.numeric(1),as.numeric(jaccard.pair.a[7]),as.numeric(jaccard.pair.a[8]),as.numeric(jaccard.pair.a[9])),
  c(as.numeric(jaccard.pair.a[10]),as.numeric(jaccard.pair.a[11]),as.numeric(1),as.numeric(jaccard.pair.a[12]),as.numeric(jaccard.pair.a[13])),
  c(as.numeric(jaccard.pair.a[14]),as.numeric(jaccard.pair.a[15]),as.numeric(jaccard.pair.a[16]),as.numeric(1),as.numeric(jaccard.pair.a[17])),
  c(as.numeric(jaccard.pair.a[18]),as.numeric(jaccard.pair.a[19]),as.numeric(jaccard.pair.a[20]),as.numeric(jaccard.pair.a[21]),as.numeric(1))
))


# Assign row and column names
matrix.pair.a <- as.data.frame(matrix.pair.a)
names(matrix.pair.a) <- names(ALL.AUC)[-1]
row.names(matrix.pair.a) <- names(ALL.AUC)[-1]

# Melt the data
matrix.pair.a  <- as.matrix(matrix.pair.a)
matrix.pair.a  <- melt(matrix.pair.a)
matrix.pair.a  <- matrix.pair.a[,1:3]
colnames(matrix.pair.a) <- c("x", "y", "value")

table(matrix.pair.a$x)

# Order by c("IN", "CL", "RC", "VI", "RE", "CD","UC")
matrix.pair.a$ord.x <- factor(matrix.pair.a$x, ordered=TRUE, levels = c("RC", "VI", "RE", "CD","UC"))
matrix.pair.a$ord.y <- factor(matrix.pair.a$y, ordered=TRUE, levels = c("RC", "VI", "RE", "CD","UC"))



# Plot

pairf1.a2 <- ggplot(matrix.pair.a, aes(x=ord.x, y=ord.y, fill = value)) +
  geom_tile() + 
  scale_fill_gradientn(colours = magma(10),na.value = NA, limits=c(0,1),breaks=c(0,.2,.4,.6,.8,1)) +
  xlab("") + ylab("") +
  guides(fill=guide_legend(title="Jaccard")) + 
  coord_flip() +
  geom_text(aes(ord.x, ord.y, label=round(value, digits = 2)), color=ifelse(matrix.pair.a$value<1,"white","black"), check_overlap = TRUE) +
  geom_rect(aes(xmin = 2.5, xmax = 5.5, ymin = 2.5, ymax = 5.5),
            fill = NA, color = "red",linetype = "solid", size=1.5) + 
  geom_rect(aes(xmin = 0.5, xmax = 2.5, ymin = 0.5, ymax = 2.5),
            fill = NA, color = "red",linetype = "dashed", size=1.5) 

pairf1.a2

# Save graph
pdf("./graphs/pairs/pair_jaccard_a2.pdf", width=4, height=3.5)
pairf1.a2
dev.off()



















##################################################################################################
# End of script
##################################################################################################
