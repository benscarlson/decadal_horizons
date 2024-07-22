#!/usr/bin/env Rscript --vanilla

# This script implements the breezy philosophy: github.com/benscarlson/breezy

# ==== Breezy setup ====

#TODO: make this a script that generates abs data and finds cutpoint for a range
# of variables, and metrics

'
Title

Usage:
absences.r <dat> <out>

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
  .datP <- file.path(.pd,'data/pdh_forBen/test1001sp')
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

#/

#Source all files in the auto load funs directory
list.files(pd('src/funs/auto'),full.names=TRUE) %>% walk(source)

#---- Local functions ----
source(pd('src/poc/pep/absences.r'))

#---- Local parameters ----
.rmodel <- rast(pd('data/pdh_forBen/misc/template.tif'))
.buffdist= 10e5

#---- Files and folders ----
if(file.exists(.outPF)) {invisible(file.remove(.outPF))}
dir.create(dirname(.outPF),recursive=TRUE,showWarnings=FALSE)

#---- Load control files ----#
# species <- read_csv(wd('ctfs/species.csv')) %>%
#   filter(as.logical(run)) %>% select(-run)

#TODO: load species from a control file
species <- tibble(file=list.files(.datP))

#---- Load data ----
message('Loading data...')

#---- Perform analysis ----
.var <- c('has12','hms12')
.gcm <- 'ACCESS-ESM1-5'
.ssp <- 'ssp126'

#Habitat mask should have NA for not habitat. Can use rmodel in this case
habitat_mask <- subst(.rmodel,0,NA)

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

opts <- list(silent=FALSE)


message(glue('Running for {nrow(species)} species'))

# ==== Perform analysis ====
tic()
foreach(i=icount(nrow(species)),.combine='rbind',.options.multicore=opts) %mypar% {
#foreach(i=1:100,.combine='rbind') %mypar% {

  # i <- 1
  row <- species[i,]
  message(row$file)
  #Only need cell id, (and maybe n if using weights) 
  spDat <- qread(file.path(.datP,row$file)) %>% as_tibble %>% distinct(cell,n)
  
  #Sanity checks
  invisible(assert_that(all(spDat$cell < ncell(.rmodel)))) #Make sure spp cell ids are within the range of rmodel cell ids
  invisible(assert_that(anyDuplicated(spDat$cell)==0)) #No cell id's are duplicated
  
  # pa==1 means presence, pa==0 means absence
  abcs <- cheap_absences(spDat,
                         .rmodel,
                         weights='n',
                         habitat_mask=habitat_mask,
                         partial_absences=FALSE,
                         include_presences=TRUE,
                         method='buffer',
                         buffdist=.buffdist)
  
  #abcs %>% group_by(pa) %>% summarize(n=n())

  #TODO: do expand.grid for var, gcm, ssp, since each is a unique combination
  spRocDat <- expand_grid(var=.var,gcm=.gcm,ssp=.ssp) %>%
    mutate(vdat=pmap(list(var,gcm,ssp),function(var,gcm,ssp){
      # var <- 'has12'; gcm <- 'ACCESS-ESM1-5'; ssp <- 'ssp126'
      envDat <- qread(pd('data/pdh_forBen/timeseries',glue('{var}_{gcm}_{ssp}.qs')))
      #nrow(envDat)
      
      # Join the env data to the pres/abs data 
      spEnv <- envDat %>%
        select(cell,as.character(1981:2014)) %>%
        drop_na %>%
        inner_join(abcs %>% select(cell,pa),by='cell') %>%
        pivot_longer(-c(cell,pa),names_to='year') %>%
        mutate(across(c(cell,year),as.integer),pa=factor(pa))
      
      tibble(metric='sum_sens_spec') %>%
        mutate(cut_dat=map(metric,~{
          #.x <- 'sum_sens_spec'
          
          oc <- cutpointr(spEnv,value,pa, method = maximize_metric, metric = get(.x), 
                          pos_class=1, direction='<=') %>% 
            as_tibble %>%
            select(direction,optimal_cutpoint,sum_sens_spec,sensitivity,specificity,AUC) %>%
            mutate(optimal_quantile=ecdf(spEnv$value)(optimal_cutpoint))
          
          
        })) %>%
        unnest(cols=cut_dat)
  
    })) %>%
    mutate(species=row$file,.before=1) %>%
    unnest(cols=vdat)
  
  spRocDat %>% write_csv(.outPF,append=file.exists(.outPF))
  
  return(tibble(file=row$file))
    
} -> status
toc()

#---- Finalize script ----#

message(glue('Script complete in {diffmin(t0)} minutes'))