
# #Quick tests to validate mQuant
# vals <- rnorm(100,0,1)
# probs <- c(0.95,0.99)
# qfun.pars=list(names=FALSE)
# weights <- 1:100
#   
# all(
#   quantile(vals,probs=probs,names=FALSE)==
#   mQuant(vals,qfun='quantile',probs=probs,types=7,qfun.pars=qfun.pars)$q.val)
# 
# all(
#   c(quantile(vals,probs=probs,type=1,names=FALSE),
#     quantile(vals,probs=probs,type=9,names=FALSE))==
#   mQuant(vals,qfun='quantile',probs=probs,types=c(1,9),qfun.pars=qfun.pars)$q.val)
# 
# qfun.pars=list(weights=weights)
# 
# all(
#   weighted.quantile(x=vals,probs=probs,weights=weights,type='Type7')==
#   mQuant(vals,qfun='weighted.quantile',probs=probs,types='Type7',qfun.pars=qfun.pars)$q.val)
# 
# all(
#   c(weighted.quantile(x=vals,probs=probs,weights=weights,type='Type7'),
#     weighted.quantile(x=vals,probs=probs,weights=weights,type='Harrell-Davis')) ==
#     mQuant(vals,qfun='weighted.quantile',probs=probs,types=c('Type7','Harrell-Davis'),
#            qfun.pars=qfun.pars)$q.val)

#Return requested quantiles for each requested type
mQuant <- function(vals,qfun,probs,types,qfun.pars) {

  #Map over each type and return the requested quantiles per type
  tibble(q.type=types) %>%
    mutate(qtypes=map(types,~{
      tibble(
        q=probs,
        q.val=rlang::exec(qfun,x=vals,probs=probs,type=.x,!!!qfun.pars)
      )
    })) %>%
    unnest(cols=qtypes)
}

thresholds=function(dat,qfun='quantile',by='none',probs=c(.95,.99,1),types=7,weights=NULL){
  #Testing: types=1:2; probs=c(.95,.99); by=c('none','month')
  # types=1:2; probs=0.95; by='none'
  # types=7; probs=0.95; by='year'
  
  if(qfun=='quantile') {
    qfun.pars <- list(names=FALSE)
  } else if(qfun=='weighted.quantile') {
    require(cNORM)
    qfun.pars <- list(weights=weights)
    # 7 is the default for quantile, set to default for weighted.quantile instead
    types <- ifelse(types==7,'Type7',types) 
  }

  tibble(group=by) %>%
    mutate(qdat=map(group,~{

      #If none, set to NULL so that there is no grouping 
      if(.x=='none') {grp <- NULL} else {grp <- .x}

      dat %>%
        nest(data=-grp) %>%
        mutate(qdat=map(data,~{
          mQuant(vals=.x$value,qfun=qfun,probs=probs,types=types,qfun.pars=qfun.pars)
        })) %>%
        select(-data) %>%
        unnest(qdat) %>%
        rename(group.val=grp)
    })) %>%
    unnest(qdat) %>%
    select(any_of(c('group','group.val','q.type','q','q.val'))) #group.val may not exist so use any_of

}
