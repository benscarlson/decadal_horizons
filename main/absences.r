#!/usr/bin/env Rscript --vanilla

# This script implements the breezy philosophy: github.com/benscarlson/breezy

# ==== Breezy setup ====

#TODO: make this a script that generates abs data and finds cutpoint for a range
# of variables, and metrics

'
Generate absence data

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
  
  .datP <- file.path(.pd,'data/pdh_forBen/test1001sp')
  .outP <- file.path(.wd,'data/pa')
  
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

#---- Load control files ----#
species <- read_csv(wd('ctfs/species.csv')) %>%
  filter(as.logical(run)) %>% select(-run)

#---- Load data ----
message('Loading data...')


#---- Perform analysis ----
# .var <- 'has12'
# .gcm <- 'ACCESS-ESM1-5'
# .ssp <- 'ssp126'

spp <- 'Abutilon_puberulentum'

#Only need cell id, (and maybe n if using weights) 
spDat <- qread(file.path(.datP,glue('{spp}.qs'))) %>% as_tibble %>% distinct(cell,n)

#Sanity checks
invisible(assert_that(all(sp_cells$cell < ncell(.rmodel)))) #Make sure spp cell ids are within the range of rmodel cell ids
invisible(assert_that(anyDuplicated(spDat$cell)==0)) #No cell id's are duplicated


plot(habitat_mask)



# pa==1 means presence, pa==0 means absence
abcs <- cheap_absences(pres,
                       rmodel,
                       weights='n',
                       habitat_mask=habitat_mask,
                       partial_absences=FALSE,
                       include_presences=TRUE,
                       method='buffer',
                       buffdist=.buffdist)

abcs %>% group_by(pa) %>% summarize(n=n())

envDat <- qread(pd('data/pdh_forBen/timeseries',glue('{.var}_{.gcm}_{.ssp}.qs')))
nrow(envDat)

# Join the env data to the pres/abs data 
spEnv <- envDat %>%
  select(cell,as.character(1981:2014)) %>%
  drop_na %>%
  inner_join(abcs %>% select(cell,pa),by='cell') %>%
  pivot_longer(-c(cell,pa),names_to='year') %>%
  mutate(across(c(cell,year),as.integer),pa=factor(pa))

spEnv %>% group_by(pa) %>% summarize(n=n())


#---- Save output ----

#---- Saving to the database ----#

invisible(dbExecute(db,'PRAGMA foreign_keys=ON'))
dbBegin(db)

sesid <- addSession(.sesnm,psesid,'population',db)

#---- Appending rows ----#
dat %>%
  dbAppendTable(db,'table',.) %>%
  checkRows(nrow(dat),db)

#---- Updating rows ----#
rs <- dbSendStatement(db, 'update table set col2 = $col2 where id=$id')

dbBind(rs,dat)

rs %>% 
  dbGetRowsAffected %>%
  checkRows(nrow(dat),db)

dbClearResult(rs)

#---- Saving a csv or other file ----#
message(glue('Saving to {.outPF}'))

dir.create(dirname(.outPF),recursive=TRUE,showWarnings=FALSE)

datout %>% write_csv(.outPF,na="")

#---- Saving a figure ----#
h=6; w=9; units='in'
  
if(fext(.outPF)=='pdf') {
  ggsave(.outPF,plot=p,height=h,width=w,device=cairo_pdf,units=units) #save pdf
} else if(fext(.outPF)=='png') {
  ggsave(.outPF,plot=p,width=w,height=h,type='cairo',units=units)
} else if(fext(.outPF)=='eps') {
  ggsave(.outPF,plot=p,width=w,height=h,device=cairo_ps,units=units,bg='transparent')
}
#---- Finalize script ----#

#-- Close transaction
if(.rollback) {
  message('Rolling back transaction because this is a test run.')
  dbRollback(db)
} else {
  message('Comitting transaction.')
  dbCommit(db)
}

dbDisconnect(db,shutdown=TRUE)

message(glue('Script complete in {diffmin(t0)} minutes'))