library(BIENWorkflow)
library(ggplot2)
library(colorout)
library(tidyverse)
library(paletteer)
library(sf)
library(animation)
library(DescTools)
library(xtable)
library(classInt)
library(raster)
library(rgdal)
library(XML)
library(maps)
library(sp)
library(TeachingDemos)
library(outliers)
library(qs)
#library(ggforce)

# for new version
	# make sure the island species aren't lost
	# is there a simpler version of the results that was fast? 

computer='/Users/ctg'
source(paste0(computer,'/Dropbox/Projects/2023_Exposure/Code/1.5_Functions.r'))
#computer='/Users/corymerow'
dataDir=paste0(computer,'/Documents/SDMs/Exposure_2023/')
baseDir=paste0(computer,'/Dropbox/Projects/2023_Exposure')
plotDir=paste0(baseDir,'/Figs_v2')
intDir=paste0(baseDir,'/Int_v2') # intermediate products
metaDir=paste0(dataDir,'/db3/metadata')
.mkdir(plotDir); .mkdir(intDir); .mkdir(metaDir)
sumDirMonitor=paste0(computer,'/Dropbox/Projects/2023_Exposure/Monitoring_v2/')
.mkdir(sumDirMonitor);

world.shp=rgdal::readOGR(paste0(computer,'/Dropbox/Projects/BIEN/BIENWorkflow/inst/extdata/TM_WORLD_BORDERS_SIMPL-0.3/TM_WORLD_BORDERS_SIMPL-0.3.shp'))
world.shp=crop(world.shp,extent(-180,180,-61,86))
world.shp.hi=rgdal::readOGR(paste0(computer,'/Dropbox/Projects/Teaching/YaleBGCCourses/World/TM_WORLD_BORDERS.shp'))
world.shp.hi=crop(world.shp.hi,extent(-180,180,-61,86))

# common objects
#cellAttDir=paste0(computer,'/Dropbox/Projects/2023_Exposure/CellAttributes')
#spAttDir=paste0(computer,'/Dropbox/Projects/2023_Exposure/SpeciesAttributes')
spMeta=qread(paste0(metaDir,'/spAttributes_v2.qs'))
 # later join this with spAttr
#spAtt=readRDS(paste0(spAttDir,'/allSpAttr.rds'))
#spAtt=spAtt %>% mutate(redlistCategory=gsub('[[:space:]]','_',redlistCategory))
#cellAtt=readRDS(paste0(cellAttDir,'/allCellAttr.rds'))
#e.d=readRDS(paste0(computer,'/Dropbox/Projects/2023_Exposure/Ecoregions2017/ecoregionMetadata.rds')) %>% dplyr::select(- (COLOR:LICENSE))
	# qunatile index with metadata on different threshold rules
print('these are wrong')
qRuleMeta=readRDS('/Users/ctg/Documents/SDMs/Exposure_2023/db3/metadata/qRuleMetadata.rds') 
# the quantiles we've been considering all along
bestQuantiles=c(109,116,117,122,123, 127,134,135,140,141) 
	# sp index
#spCodes=readRDS('/Users/ctg/Documents/SDMs/Exposure_2023/exposure_2023_sensitivity/spCodes.rds')
#rangeSizes=readRDS('/Users/ctg/Dropbox/Projects/2023_Exposure/rangeSizes_v2.rds') 


#============================================================

#scp merow@ceiba.nceas.ucsb.edu:/home/serradiaz/exposure2023/db3.zip /Users/ctg/Documents/SDMs

