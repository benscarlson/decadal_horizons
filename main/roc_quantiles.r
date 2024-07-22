#!/usr/bin/env Rscript --vanilla

# This script implements the breezy philosophy: github.com/benscarlson/breezy

# ==== Breezy setup ====



'
Title

Usage:
roc_quantiles.r <dat> <out>

Control files:
  ctfs/species.csv

Parameters:
  dat: path to species presence data.
  out: path to output directory.

Options:

' -> doc

#---- Input Parameters ----
if(interactive()) {
  
  .pd <- here::here()
  .wd <- file.path(.pd,'analysis/poc/roc_quantiles')
  
  .cores <- 9
  #.rangesP <- file.path(.wd,'data/trinary/ppm')
  .rangesP <- file.path(.wd,'data/trinary_500')
  .parMethod <- 'mc'
  .outPF <- file.path(.wd,'data/roc_quantiles.csv')
  
} else {

  ag <- docopt::docopt(doc)
  
  .script <-  whereami::thisfile()
  
  .pd <- rprojroot::is_rstudio_project$make_fix_file(.script)()
  .wd <- getwd()

  source(file.path(.pd,'src','funs','input_parse.r'))
  
  .datP <- makePath(ag$dat)
  .outP <- makePath(ag$out)

}

#---- Initialize Environment ----

pd <- function(...) file.path(.pd,...)
wd <- function(...) file.path(.wd,...)

t0 <- Sys.time()

source(pd('src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(cutpointr)
    library(iterators)
    library(foreach)
    library(terra)
    library(qs)
  }))

conflicts_prefer(terra::trim)
#/

#Source all files in the auto load funs directory
list.files(pd('src/funs/auto'),full.names=TRUE) %>% walk(source)

#---- Local functions ----
#source(pd('src/poc/pep/absences.r'))

#---- Local parameters ----
rmodel <- rast(pd('data/pdh_forBen/misc/template.tif'))
.buffdist= 1e6

#---- Files and folders ----
if(file.exists(.outPF)) {invisible(file.remove(.outPF))}
dir.create(dirname(.outPF),recursive=TRUE,showWarnings=FALSE)

#---- Load control files ----#
# species <- read_csv(wd('ctfs/species.csv')) %>%
#   filter(as.logical(run)) %>% select(-run)

#TODO: load species from a control file
species <- tibble(file=list.files(.rangesP))

#---- Load data ----
message('Loading data...')

#---- Perform analysis ----

#TODO: could save these to a yml file
.var <- c('has12','hms12')
.gcm <- 'ACCESS-ESM1-5'
.ssp <- 'ssp126'

# ==== Start cluster and register backend ====
if(is.null(.parMethod)) {
  message('No parallel method defined, running sequentially.')
  #foreach package as %do% so it is loaded even if the parallel packages are not
  `%mypar%` <- `%do%`
} else if(.parMethod=='mpi') {
  message('Registering backend doMPI')
  library(doMPI)
  
  dir.create(.mpiLogP,showWarnings=FALSE,recursive=TRUE)
  #start the cluster. number of tasks, etc. are defined by slurm in the init script.
  message('Starting mpi cluster.')
  cl <- startMPIcluster(verbose=TRUE,logdir=.mpiLogP)
  registerDoMPI(cl)
  setRngDoMPI(cl) #set each worker to receive a different stream of random numbers
  
  `%mypar%` <- `%dopar%`
  
} else if(.parMethod=='mc') {
  #.cores <- strtoi(Sys.getenv('SLURM_CPUS_PER_TASK', unset=1)) #for testing on hpc
  message(glue('Registering backend doMC with {.cores} cores'))
  library(doMC)
  RNGkind("L'Ecuyer-CMRG")
  
  registerDoMC(.cores)
  
  `%mypar%` <- `%dopar%`
  
} else {
  stop('Invalid parallel method')
}

# ==== Perform analysis ====

#opts <- list(silent=FALSE) #Don't think this works

#Make the template raster with cell ids
rsp <- rmodel
values(rsp) <- 1:terra::ncell(rsp)
names(rsp) <- 'cell'

#Make the habitat mask
#Habitat mask should have NA for not habitat. Can use rmodel in this case
habitat_mask <- subst(rmodel,0,NA)

#Based on looking at maps of Bromus_riparius on pwo, I assume 
# 0 or NA - probably/definitely absent
# 1 - probably present
# 2 - possibly present

message(glue('Running for {nrow(species)} species'))

#500 species, two vars, one metric takes 15 min
tic()
Sys.time()
#foreach(i=icount(nrow(species)),.combine='rbind',.options.multicore=opts) %mypar% {
foreach(i=icount(nrow(species)),.combine='rbind') %mypar% {

  # i <- 20
  row <- species[i,]
  message(row$file)

  #TODO: ranges should really be 
  rngMoll <- rast(file.path(.rangesP,row$file))
  rng <- trim(project(rngMoll,rmodel,method='near',align_only=TRUE))
  
  #---- Presences ----
  
  # Extract presences from the map where value=1
  presMsk <- ifel(rng==1,1,NA)
  presIds <- mask(crop(rsp,presMsk),presMsk) #Crop the template then mask based on pres mask
  
  pres <- presIds %>% as.data.frame %>% as_tibble %>%
    mutate(occ=TRUE) #Extract cell ids
  
  #---- Absences ----
  # Take absences from a buffer outside the maximum range of the trinary map
  
  rngMsk0 <- ifel(rng >= 1,1,NA)
  rngMsk <- extend(rngMsk0,ext(rmodel)) # extend rngMask into the global raster because buffer will be bigger than the range raster
  rngBuf <- buffer(rngMsk,width=.buffdist,background=NA) #Returns T/F layer
  
  #Remove non-habitat and any potential presence locations
  rngHab <- mask(rngBuf,crop(habitat_mask,rngBuf)) #Mask the buffered range by habitat
  rngAbs <- mask(rngHab,rngMsk,inverse=TRUE) #Now clip out anywhere trinary map had 1 or 2
  
  #Get the cell ids from the template raster
  absIds <- mask(crop(rsp,rngAbs),rngAbs) #Crop the template then mask based on pres mask
  absp <- absIds %>% as.data.frame %>% as_tibble %>%
    mutate(occ=FALSE) #Extract cell ids
  
  spDat <- bind_rows(pres,absp)
  
  if(spDat %>% distinct(occ) %>% nrow != 2) {
    return(NULL)
  }
  #spDat %>% group_by(occ) %>% summarize(n=n())

  spRocDat <- expand_grid(var=.var,gcm=.gcm,ssp=.ssp) %>%
    mutate(vdat=pmap(list(var,gcm,ssp),function(var,gcm,ssp){
      # var <- 'has12'; gcm <- 'ACCESS-ESM1-5'; ssp <- 'ssp126'
      envDat <- qread(pd('data/pdh_forBen/timeseries',glue('{var}_{gcm}_{ssp}.qs')))
      #nrow(envDat)
      
      # Join the env data to the pres/abs data 
      spEnv <- envDat %>%
        select(cell,as.character(1981:2014)) %>%
        drop_na %>%
        inner_join(spDat %>% select(cell,occ),by='cell') %>%
        pivot_longer(-c(cell,occ),names_to='year') %>%
        mutate(across(c(cell,year,occ),as.integer))
      
      tibble(metric='sum_sens_spec') %>%
        mutate(cut_dat=map(metric,~{
          #.x <- 'sum_sens_spec'
          
          oc <- cutpointr(spEnv,value,occ, method = maximize_metric, metric = get(.x), 
                          pos_class=1, direction='<=') %>% 
            as_tibble %>%
            select(direction,optimal_cutpoint,sum_sens_spec,sensitivity,specificity,AUC) %>%
            mutate(optimal_quantile=ecdf(spEnv$value)(optimal_cutpoint))
          
        })) %>%
        unnest(cols=cut_dat)
  
    })) %>%
    mutate(species=row$file,.before=1,
           n_pres=nrow(filter(spDat,occ)),
           n_abs=nrow(filter(spDat,!occ))) %>%
    unnest(cols=vdat)
  
  spRocDat %>% write_csv(.outPF,append=file.exists(.outPF))
  
  return(tibble(file=row$file))
    
} -> status
toc()

#---- Finalize script ----#

message(glue('Script complete in {diffmin(t0)} minutes'))