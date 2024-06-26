
#TODO: Move example code to quantiles.qmd

# #Set up data for the call
# envDat0 <- readRDS(pd('data/andreas/data/tas_Amon_EC-Earth3_dcppB-forecast_s2023-r1i4p1f1.rds'))
# 
# tic() #separate takes 85 seconds 
# envDat <- envDat0 %>%
#   pivot_longer(cols=-world_id) %>%
#   rename(cell=world_id) %>%
#   separate(name,into=c('year','month'),sep='-',convert=TRUE)
# toc()
# 
# spDat0 <- readRDS(pd('data/andreas/data/Amphibians.rds'))


#Return requested quantiles for each requested type
mQuantTypes <- function(vals,probs,types) {
  #Map over each type and return the requested quantiles per type
  tibble(quantileType=types) %>%
    mutate(qtypes=map(types,~{
      tibble(q=probs,q.val=quantile(vals,probs,type=.x,names=FALSE))
    })) %>%
    unnest(cols=qtypes)
}

almostEveryQuantileEver=function(occ.cells,envDat,by='none',probs=c(.95,.99,1),types=7){
  #Testing: types=1:2; probs=c(.95,.99); by=c('none','month')
  
  occ.cells=tibble(cell=occ.cells)
  
  #occ.cells contains duplicates in some datasets. Should fail, but for now just remove
  occ.cells <- occ.cells %>% distinct(cell)
  
  #Join to get values for occupied cells
  #TODO: try nest -> join -> unnest to see if it's faster
  spEnv <- envDat %>% 
    inner_join(occ.cells,by='cell')

  tibble(group=by) %>%
    mutate(qdat=map(group,~{

      #If none, set to NULL so that there is no grouping 
      if(.x=='none') {grp <- NULL} else {grp <- .x}

      spEnv %>%
        nest(data=-grp) %>%
        mutate(qdat=map(data,~{
          mQuantTypes(.x$value,probs=probs,types=types)
        })) %>%
        select(-data) %>%
        unnest(qdat) %>%
        rename(group_value=grp)
    })) %>%
    unnest(qdat)

}

#TODO: Move examples to quantiles.qmd
# #---- Call for one species ----
# almostEveryQuantileEver(spDat0[[1]],envDat,by=c('none','month'),probs=0.95) %>% View
# 
# #---- Call for all species
# # Could also do this in parallel
# spDat <- tibble(
#   species = names(spDat0),
#   occ.cells = unname(spDat0)
# ) 
# 
# #100: 12 seconds
# #1000: 120 seconds
# tic()
# spQuantiles <- spDat %>%
#   mutate(qdat=map(occ.cells,almostEveryQuantileEver,
#                   envDat=envDat,probs=0.95)) %>%
#   select(-occ.cells) %>%
#   unnest(cols=qdat)
# toc()
# 
# # Overall and per-month 95, 99 and 100% quantiles for all species
# #100: 15 seconds
# #1000: 147 seconds
# #7150: 1080 seconds
# tic()
# spQuantiles <- spDat %>%
#   mutate(qdat=map(occ.cells,almostEveryQuantileEver,
#                   envDat=envDat,by=c('none','month'),probs=c(0.95,0.99,1))) %>%
#   select(-occ.cells) %>%
#   unnest(cols=qdat)
# toc()
# 
# dir.create(wd(),recursive=TRUE)
# saveRDS(spQuantiles,file=wd('andreas_quantiles.rds'))
