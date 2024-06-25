# this version is all table based and uses only occupied cells and temp layers sent from Pep. This starts from scratch with the t2m layers that include islands. note that we only use the island layer for species that were omitted because they were ONLY on islands. This avoids comparing land cells with ocean cells for species on mainland
  # this was originally done when exploring the frequency of marignal cells, so it has some legacy naming from that 
# 0. /t2m_expProj has pep's calculations for a few of the focal thresholds.  these were only used to sp and cell IDs. Never got around to comparing the quantiles between his and my analysis.  
# 1. /t2mThresh_cm_v2/ Calculate thresholds. This was done with a million options for sensitivity analysis
# 2. /t2m_marg_v2/ Calculate exposure and marginal cells. done together because i found the exposure accidentally while looking for marginal cells. These contain exposure for every year since 1990 in nested tables
# 3. /t2m_stats_v2 / Reformat of 2, with binary exposure as columns and cellsXSP as rows. has marginal cells and exposure in nonmarginal cells separately.  Additionally made /t2m_spStats_v2/ as a convenience with just the 5 bad years, adding % exposed.
# 4. /t2m_spXCell unpack 2 to have spXcell with binary exposure for each year in wide format so that it looks like: ~2023_Exposure/SpeciesXCellXExposure_1990-2023_t2m-q099_99p_v2.rds
	# only 1 threshold per file. chose just the few key thresholds
	# done specifically so i can use past code. 
#==============================================================
#==============================================================
# 1. Calculate thresholds. 
#==============================================================
#==============================================================
# try every option
almostEveryQuantileEver=function(sp.f,clim,outDir,brokeDir){
	#  for testing : sp.f=all.sp.f[1]; clim=t2
	print(basename(sp.f))
	sp1=readRDS(sp.f)
	if(nrow(sp1)==0) {
		file.copy(sp.f,paste0(brokeDir,'/',basename(sp.f)));
		warning(basename(sp.f))
		return()}
	occ.cells=tibble(cell=unique(sp1$cellid))
	sp.clim=clim %>% right_join(occ.cells,by='cell') %>% pivot_longer(cols=!cell, names_to='year',values_to='t2m') %>% mutate(t2m.1=round(t2m,1)) %>% filter(!year==2023)
	
	#1.  clim x cell stats
	cell.means=sp.clim %>% group_by(cell) %>% summarize(mean.clim=mean(t2m),mean.clim.1=round(mean(t2m.1),1))
	#warmestCell=cell.means %>% filter(mean.clim==max(mean.clim)) 
	# skip omitting outliers first. seems kinda redundant with quantile estimation. could be more important for q1
	
	#2. calc quantile first over years then over cells with type 1-9
		# first over years
			# not rounded
		mythresh1=lapply(1:9,function(ty){sp.clim %>%  group_by(cell) %>%  reframe(year.q.val = quantile(t2m, c(.95,.99,1),type=ty), year.q = c(.95,.99,1)) %>% mutate(quantileType=ty,isRounded=F)}) %>% bind_rows
			# rounded
		mythresh1.1=lapply(1:9,function(ty){sp.clim %>%  group_by(cell) %>%  reframe(year.q.val = round(quantile(t2m.1, c(.95,.99,1),type=ty),1), year.q = c(.95,.99,1)) %>% mutate(quantileType=ty,isRounded=T)}) %>% bind_rows 
		# combine
		mythresh.a = mythresh1 %>% bind_rows(mythresh1.1 )
		# quantile over cells
		mythresh2=lapply(1:9,function(ty){ 
			tmp=mythresh.a %>% ungroup %>% group_by(year.q,isRounded) 
				# this ensures i don't cross quantile types
			tmp1=tmp %>% filter(quantileType==ty) %>% reframe(thresh.val = quantile(year.q.val, c(.95,.99,1),type=ty), cell.q = c(.95,.99,1)) %>% mutate(quantileType=ty)
			}) %>% bind_rows %>% mutate(year.cell.q=NA)
		
	# 3. calc quantile over all year x cell values. do both value and rounded value. this could raise the value a little because you're not averaging away the high variance extreme cells. not ideal because they're not independent, but its not like the cells are either...
		mythresh3=lapply(1:9,function(ty){sp.clim %>%  reframe(thresh.val = quantile(t2m, c(.95,.99,1),type=ty), year.cell.q = c(.95,.99,1)) %>% mutate(quantileType=ty,isRounded=F)}) %>% bind_rows
			# rounded
		mythresh3.1=lapply(1:9,function(ty){sp.clim  %>%  reframe(thresh.val = round(quantile(t2m.1, c(.95,.99,1),type=ty),1), year.cell.q = c(.95,.99,1)) %>% mutate(quantileType=ty,isRounded=T)}) %>% bind_rows 
		# combine
		mythresh.b = mythresh3 %>% bind_rows(mythresh3.1) %>% mutate(year.q=NA,cell.q=NA)

	#  4. organize all the outputs and store
	allMyThresh=mythresh2 %>% bind_rows(mythresh.b)
	saveRDS(allMyThresh,file=paste0(outDir,'/',basename(sp.f)))
}

# run it
outDir='/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2mThresh_cm_v2'
.mkdir(outDir)
brokeDir='/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_broke_v2'
.mkdir(brokeDir)
all.sp.f=list.files('/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_expProj',full.names=T)
clim=qread('/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_ts_islands.qs')
overwrite=FALSE
done=mclapply(all.sp.f, function(x) {
	out.f=paste0(outDir,'/',basename(x))
	if(!file.exists(	out.f) | overwrite ) 	almostEveryQuantileEver(x,clim,outDir,brokeDir)
},mc.cores=11)

saveRDS(list.files('/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_broke_v2'),file='/Users/ctg/Documents/SDMs/Exposure_2023/t2m_broke_v2.rds')

# add in broke species that occured only on islands. the islands werenmt considered for mainland species because the ocean might bias them compared to land cells
outDir='/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2mThresh_cm'
.mkdir(outDir)
brokeDir='/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_broke2'
.mkdir(brokeDir)
all.sp.f=list.files('/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_expProj',full.names=T)
clim=qread('/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_ts_islands.qs')
overwrite=FALSE
#  x=all.sp.f[grep("Accipiter_butleri.rds",all.sp.f)]
done=mclapply(all.sp.f, function(x) {
	out.f=paste0(outDir,'/',basename(x))
	if(!file.exists(	out.f) | overwrite ) 	almostEveryQuantileEver(x,clim,outDir,brokeDir)
},mc.cores=11)
# manually copied thme to the main dir

#==============================================================
#==============================================================
# 2. Calculate exposure and marginal cells 
#==============================================================
#==============================================================
	# marignal ceels is tricky because it needs to be split by (a) rounded or not, (b) cell by year or not, 
	# done separately because this is a different supplementary analysis
	# this could be combined in the previous loop, but i wanted to get this running
					
all.sp.f=list.files('/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_expProj',full.names=T)					
outDirMarg='/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_marg_v2'
.mkdir(outDirMarg)
outDir='/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2mThresh_cm_v2'
clim=qread('/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_ts_islands.qs')
overwrite=F
marg=mclapply(all.sp.f, function(x) { # x=all.sp.f[100]
	print(basename(x))
	out.f=paste0(outDirMarg,'/',basename(x))
	tr.file=paste0(outDir,'/',basename(x))
	if(file.exists(out.f) | !file.exists(tr.file)) 	return()
	# read in everything
	sp1=readRDS(x)
	occ.cells=tibble(cell=unique(sp1$cellid))
	# note that i don't remove 2023 here like i did when calculating thresh
	sp.clim=clim %>% right_join(occ.cells,by='cell') %>% pivot_longer(cols=!cell, names_to='year',values_to='t2m') %>% mutate(t2m.1=round(t2m,1))
	cell.means=sp.clim %>% group_by(cell) %>% summarize(mean.clim=mean(t2m),mean.clim.1=round(mean(t2m.1),1))
	allMyThresh=readRDS(tr.file)
	
		# store in a nested list of all the cellIDs that are marginal. later, make a list of all cellIDs exposed. then ask what proportion of range is exposed and what proportion of that exposure is marginal. 	
		# split by the comparison type: year/cell, year/cell/rounded, yearcell, yearcell/rounded	
	t.y.c=allMyThresh %>% filter(!isRounded,is.na(year.cell.q))	
	t.y.c.r=allMyThresh %>% filter(isRounded,is.na(year.cell.q))	
	t.yc=allMyThresh %>% filter(!isRounded,!is.na(year.cell.q))	
	t.yc.r=allMyThresh %>% filter(isRounded,!is.na(year.cell.q))	
	# could make the thresh wide format and join to cell.means like we do for exposure but that seems terrible now that there are so many thresholds. this is the same process as the exposure step, jut applied to means...
	# year by cell
	t.y.c1=t.y.c %>% mutate(marginalCells=sapply(1:nrow(t.y.c),function(y){
		cell.means %>% filter(mean.clim>t.y.c$thresh.val[y]) %>% pull(cell)
	}))  %>% mutate(nMarginalCells=sapply(marginalCells,length))  %>% mutate(rangeSize=nrow(cell.means)) %>% mutate(exposedYearCells=lapply(1:nrow(t.y.c),function(y){
		sp.clim %>% filter(t2m>t.y.c$thresh.val[y]) %>% dplyr::select(cell,year)
	})) %>% mutate(expEvents=sapply(exposedYearCells,nrow)) %>% mutate(expByYear=lapply(exposedYearCells,function(z) { z %>% count(year) })) #%>% #see how many marginal cells count towards exposure each year 
	#mutate(lapply(,function(zz){ }))
	#t.y.c1$exposedYearCells[[1]]$cell  %in% t.y.c1$marginalCells$cell 
		
	# year by cell roudned to 1 digit
	t.y.c.r1=t.y.c.r %>% mutate(marginalCells=sapply(1:nrow(t.y.c.r),function(y){
		cell.means %>% filter(mean.clim.1>t.y.c.r$thresh.val[y]) %>% pull(cell)
	}))  %>% mutate(nMarginalCells=sapply(marginalCells,length))  %>% mutate(rangeSize=nrow(cell.means)) %>% mutate(exposedYearCells=lapply(1:nrow(t.y.c.r),function(y){
		sp.clim %>% filter(t2m.1>t.y.c.r$thresh.val[y]) %>% dplyr::select(cell,year)
	})) %>% mutate(expEvents=sapply(exposedYearCells,nrow)) %>% mutate(expByYear=lapply(exposedYearCells,function(z) { z %>% count(year) }))

	# year cells. by definition is a marginal yearcell and an exposed cell the same? at least in the historical record. and then it can't be marginal in 2023 because that year wasn't used to fit...
	t.yc1=t.yc %>% mutate(marginalCells=lapply(1:nrow(t.yc),function(y){
		sp.clim %>% filter(year<2023 & t2m>t.yc$thresh.val[y]) %>% dplyr::select(cell,year)
	}))  %>% mutate(nMarginalCells=sapply(marginalCells,nrow))  %>% mutate(rangeSize=nrow(cell.means)) %>% mutate(exposedYearCells=lapply(1:nrow(t.yc),function(y){
		sp.clim %>% filter(t2m>t.yc$thresh.val[y]) %>% dplyr::select(cell,year)
	})) %>% mutate(expEvents=sapply(exposedYearCells,nrow)) %>% mutate(expByYear=lapply(exposedYearCells,function(z) { z %>% count(year) }))
		# only diff between marginal cells and exposed is 2023 exposure
	#fuck=sapply(t.yc1$exposedYearCells,nrow)
	# t.yc1$nMarginalCells==fuck
 # year cells rounded to 1 digit
	t.yc.r1=t.yc.r %>% mutate(marginalCells=lapply(1:nrow(t.yc.r),function(y){
		sp.clim %>% filter(year < 2023 & t2m>t.yc.r$thresh.val[y]) %>% dplyr::select(cell,year)
	}))  %>% mutate(nMarginalCells=sapply(marginalCells,nrow))  %>% mutate(rangeSize=nrow(cell.means)) %>% mutate(exposedYearCells=lapply(1:nrow(t.yc.r),function(y){
		sp.clim %>% filter(t2m>t.yc.r$thresh.val[y]) %>% dplyr::select(cell,year)
	})) %>% mutate(expEvents=sapply(exposedYearCells,nrow)) %>% mutate(expByYear=lapply(exposedYearCells,function(z) { z %>% count(year) }))
	
	# TESTS
	# why are marginal cells and exposed cells the same
	#	 t.y.c1$marginalCells; t.y.c1$exposedYearCells
	# 	 lapply(1:nrow(t.y.c1),function(w) all(	t.y.c1$marginalCells[[w]]==	t.y.c1$exposedYearCells[[w]]))
	# 	 	lapply(1:nrow(t.y.c.r1),function(w) all(t.y.c.r1$marginalCells[[w]]==t.y.c.r1$exposedYearCells[[w]]))
	# 	 # these should have 27 rows because there's only 1 qunatile .. and not be equal... well maybe they should...
	# 	 lapply(1:nrow(t.yc1),function(w) all(t.yc1$marginalCells[[w]]==t.yc1$exposedYearCells[[w]]))
	# 	 	lapply(1:nrow(t.yc.r1),function(w) all(t.yc.r1$marginalCells[[w]]==t.yc.r1$exposedYearCells[[w]]))
	
	# organize and store
	out= t.y.c1 %>% bind_rows(t.y.c.r1,t.yc1,	t.yc.r1)
	saveRDS(out,file=out.f)
},mc.cores=11)

# NOTE: Using yearcells leads to an equal definition of historical exposure and marginal cells. 

zip -r /Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_marg_v2.zip /Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_marg
rsync -avP /Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_marg_v2.zip merow@ceiba.nceas.ucsb.edu:/home/bien/ranges/

#==============================================================
#==============================================================
# 3. reformat. /t2m_stats_v2 / Reformat of 2, with counts of binary exposure as columns and thresholds as rows. has marginal cells and exposure in nonmarginal cells separately.  Additionally made /t2m_spStats_v2/ as a convenience with just the 5 bad years, adding % exposed. 
#==============================================================
#==============================================================

# understanding quantile patterns
	# this could be combined in the previous loop, but i wanted to get this running
	#1. find out how different exposure is in the 5 worst years. (pretty sure they'll be the same worst years no matter what)
	#2. find out how many exposed cells are marginal and make sure its small
	#3.check spatial patterns of 2023 for a range of 1
	#  
	
	# . find out how many of repeat exposures in the 5 worst years are in marginal cells
# 
outDirStats='/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_stats_v2'
.mkdir(outDirStats)
outDirSp='/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_spStats_v2'
.mkdir(outDirSp)
outDirMarg='/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_marg_v2'
overwrite=F
all.sp.f2=list.files(outDirMarg,full.names=T)
stats=mclapply(all.sp.f2, function(x1) { # x1=all.sp.f2[100]
	print(basename(x1))
	out.f=paste0(outDirStats,'/',basename(x1))
	mar.file=paste0(outDirMarg,'/',basename(x1))
	if(file.exists(out.f) | !file.exists(mar.file)) 	return()
	# read in everything
	mar=readRDS(mar.file) 
	mar=mar %>% mutate(quantileIndex=1:nrow(mar)) # for joins
	# to store stats
	toKeep=mar %>% dplyr::select(-exposedYearCells,-marginalCells,-expByYear)
			# occ.cells=tibble(cell=unique(sp1$cellid))
		# 	sp.clim=clim %>% right_join(occ.cells,by='cell') %>% pivot_longer(cols=!cell, names_to='year',values_to='t2m') %>% mutate(t2m.1=round(t2m,1))
		# 	cell.means=sp.clim %>% group_by(cell) %>% summarize(mean.clim=mean(t2m),mean.clim.1=round(mean(t2m.1),1))
		# 	allMyThresh=readRDS(tr.file)
	#------------------------------
	# unnest count of exposure by year. combine all the years before 1990
		# can do years and yearcells at the same time
	tmp1=mar  %>%  dplyr::select(quantileIndex,expByYear) %>% unnest(expByYear) 
	tmp1.pre90=tmp1 %>% filter(year<1990) %>% group_by(quantileIndex) %>% summarize(expPre1990=sum(n))
	tmp1=tmp1 %>% filter(year>=1990) %>%  mutate(year=paste0('X',year))
		# need to fill in empty years 
	new.vars=paste0('X',1990:2023)
	tmp2=mar %>% dplyr::select(quantileIndex) %>% mutate(!!!setNames(rep(0, length(new.vars)), new.vars),expPre1990=0)
	if(nrow(tmp1)>0){
		# put values in place 
		for(i in 1:nrow(tmp1)){ tmp2[tmp1$quantileIndex[i],tmp1$year[i]]=tmp1$n[i]
		}
		for(i in 1:nrow(tmp1.pre90)){ 
			tmp2$expPre1990[tmp1.pre90$quantileIndex[i]]= tmp1.pre90$expPre1990[i]
		}
	}	
	toKeep = toKeep %>% left_join(tmp2,by='quantileIndex')
	#------------------------------
	#unnest marginal cells and see if exposure was predicted in them
		# have to split by year.q/cell.q and year.cell.q

		# NOTE there may be some weird patterns; not sure i recorded marginal and exposed cells right...
	
	# 	# these aren't needed becasue marginal==historically exposed
	# 	tmp3=mar %>% filter(!is.na(year.cell.q)) %>% dplyr::select(quantileIndex,exposedYearCells) %>% unnest(exposedYearCells) 
	# 	tmp3.1=mar %>% filter(!is.na(year.cell.q)) %>% dplyr::select(quantileIndex,marginalCells) %>% unnest(marginalCells) 
	# 	inBoth1=tmp3 %>% inner_join(tmp3.1,by=c('quantileIndex','cell','year'))
	# 	tmp4=inBoth1 %>% filter(year>=1990) %>%  mutate(year=paste0('mar',year)) %>% group_by(quantileIndex,year) %>% count
	# 

	#------------------------------
	# find marginal cells that were counted as exposed for year.q and cell.q. ONLY APPLIES TO year + cell
		# i think i stored marginal cells for year.q/cell.q in the same format as year.cell.q so there's some redundant info in them. if so can toss year and take unique cells
		# exposed years and cells
	tmp3a=mar %>% filter(is.na(year.cell.q)) %>% dplyr::select(quantileIndex,exposedYearCells) %>% unnest(exposedYearCells) 
		# amrginal cells
	tmp3a.1=mar %>% filter(is.na(year.cell.q)) %>% dplyr::select(quantileIndex,marginalCells) %>% unnest(marginalCells) %>% rename(cell=marginalCells)
		#find when marginal contribute to exposure in each year
			# 	# need to fill in empty years 
	new.vars=paste0('mar',1990:2023)
	tmp5=mar %>% filter(is.na(year.cell.q)) %>% dplyr::select(quantileIndex) %>% mutate(!!!setNames(rep(0, length(new.vars)), new.vars))
	if(nrow(tmp3a.1)>0){
		inBoth2=tmp3a %>% inner_join(tmp3a.1,by=c('quantileIndex','cell'))
		tmp4a=inBoth2 %>% filter(year>=1990) %>%  mutate(year=paste0('mar',year)) %>% group_by(quantileIndex,year) %>% count
		for(i in 1:nrow(tmp4a)){ tmp5[tmp4a$quantileIndex[i],tmp4a$year[i]]=tmp4a$n[i]
		}
	}
	toKeep.yc=toKeep %>% filter(!is.na(year.cell.q))
	toKeep.y.c = toKeep %>% filter(is.na(year.cell.q)) %>% left_join(tmp5,by='quantileIndex')
	expNotMarg = (toKeep.y.c %>% dplyr::select(X1990:X2023)) - (toKeep.y.c %>% dplyr::select(mar1990:mar2023))
	names(expNotMarg)=sub('X','XnM',names(expNotMarg))
	toKeep.y.c = 	toKeep.y.c %>% bind_cols(expNotMarg)
	# fuck=toKeep %>% filter(!is.na(year.cell.q)) %>% dplyr::select(XnM1990:XnM2022) %>% rowSums
		# organize and store
	tk=toKeep.y.c %>% bind_rows(toKeep.yc)
	saveRDS(tk,file=out.f)
	
	# =======================
	# summarize to species level stats that I'll use to decide which quantile to use
	spStats=tk %>% dplyr::select(year.q:quantileIndex,X2015,X2016,X2019,X2020,X2023,XnM2015,XnM2016,XnM2019,XnM2020,XnM2023) %>% mutate(pExp2015=round(X2015/rangeSize,3),pExp2016=round(X2016/rangeSize,3),pExp2019=round(X2019/rangeSize,3),pExp2020=round(X2020/rangeSize,3),pExp2023=round(X2023/rangeSize,3))
	# data.frame(	spStats)
	saveRDS(spStats,file=paste0(outDirSp,'/',basename(x1)))

},mc.cores=10)

#==============================================================
#==============================================================
# 4. reformat to spXcell. specifically so i can use past code. 
#==============================================================
#==============================================================
outDirSpXCell=paste0(dataDir,'/db3/t2m_spXCell_v2'); .mkdir(outDirSpXCell)
outDirMarg='/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_marg_v2'
overwrite=F
all.sp.f2=list.files(outDirMarg,full.names=T)
qSubset=qRuleMeta[bestQuantiles,]
# the thing i need is in $exposedYearCells. need to make a big empty matrix and store this in there or can i adpat other code to work this ths and not have all th zeros



stats=mclapply(all.sp.f2, function(x1) { # x1=all.sp.f2[200]
	print(basename(x1))
	out.f=paste0(outDirSpXCell,'/',basename(x1))
	mar.file=paste0(outDirMarg,'/',basename(x1))
	if(file.exists(out.f) | !file.exists(mar.file)) 	return()
	# read in everything
	mar=readRDS(mar.file) 
	# subset; quantile index was wrong... 
	mar=mar %>% right_join(qSubset) %>% filter(quantileType==7)
	# to store stats
	toKeep=mar %>% dplyr::select(-exposedYearCells,-marginalCells,-expByYear)
	#------------------------------
	# unnest count of exposure by year. combine all the years before 1990
		# can do years and yearcells at the same time
		mar$expByYear
	tmp1=mar  %>%  dplyr::select(quantileIndex,expByYear) %>% unnest(expByYear) 
	tmp1.pre90=tmp1 %>% filter(year<1990) %>% group_by(quantileIndex) %>% summarize(expPre1990=sum(n))
	tmp1=tmp1 %>% filter(year>=1990) %>%  mutate(year=paste0('X',year))
		# need to fill in empty years 
	new.vars=paste0('X',1990:2023)
	tmp2=mar %>% dplyr::select(quantileIndex) %>% mutate(!!!setNames(rep(0, length(new.vars)), new.vars),expPre1990=0)
	if(nrow(tmp1)>0){
		# put values in place 
		for(i in 1:nrow(tmp1)){ tmp2[tmp1$quantileIndex[i],tmp1$year[i]]=tmp1$n[i]
		}
		for(i in 1:nrow(tmp1.pre90)){ 
			tmp2$expPre1990[tmp1.pre90$quantileIndex[i]]= tmp1.pre90$expPre1990[i]
		}
	}	
	toKeep = toKeep %>% left_join(tmp2,by='quantileIndex')
	
	#------------------------------
	# find marginal cells that were counted as exposed for year.q and cell.q. ONLY APPLIES TO year + cell
		# i think i stored marginal cells for year.q/cell.q in the same format as year.cell.q so there's some redundant info in them. if so can toss year and take unique cells
		# exposed years and cells
	tmp3a=mar %>% filter(is.na(year.cell.q)) %>% dplyr::select(quantileIndex,exposedYearCells) %>% unnest(exposedYearCells) 
		# amrginal cells
	tmp3a.1=mar %>% filter(is.na(year.cell.q)) %>% dplyr::select(quantileIndex,marginalCells) %>% unnest(marginalCells) %>% rename(cell=marginalCells)
		#find when marginal contribute to exposure in each year
			# 	# need to fill in empty years 
	new.vars=paste0('mar',1990:2023)
	tmp5=mar %>% filter(is.na(year.cell.q)) %>% dplyr::select(quantileIndex) %>% mutate(!!!setNames(rep(0, length(new.vars)), new.vars))
	if(nrow(tmp3a.1)>0){
		inBoth2=tmp3a %>% inner_join(tmp3a.1,by=c('quantileIndex','cell'))
		tmp4a=inBoth2 %>% filter(year>=1990) %>%  mutate(year=paste0('mar',year)) %>% group_by(quantileIndex,year) %>% count
		for(i in 1:nrow(tmp4a)){ tmp5[tmp4a$quantileIndex[i],tmp4a$year[i]]=tmp4a$n[i]
		}
	}
	toKeep.yc=toKeep %>% filter(!is.na(year.cell.q))
	toKeep.y.c = toKeep %>% filter(is.na(year.cell.q)) %>% left_join(tmp5,by='quantileIndex')
	expNotMarg = (toKeep.y.c %>% dplyr::select(X1990:X2023)) - (toKeep.y.c %>% dplyr::select(mar1990:mar2023))
	names(expNotMarg)=sub('X','XnM',names(expNotMarg))
	toKeep.y.c = 	toKeep.y.c %>% bind_cols(expNotMarg)
	# fuck=toKeep %>% filter(!is.na(year.cell.q)) %>% dplyr::select(XnM1990:XnM2022) %>% rowSums
		# organize and store
	tk=toKeep.y.c %>% bind_rows(toKeep.yc)
	saveRDS(tk,file=out.f)
	
	# =======================
	# summarize to species level stats that I'll use to decide which quantile to use
	spStats=tk %>% dplyr::select(year.q:quantileIndex,X2015,X2016,X2019,X2020,X2023,XnM2015,XnM2016,XnM2019,XnM2020,XnM2023) %>% mutate(pExp2015=round(X2015/rangeSize,3),pExp2016=round(X2016/rangeSize,3),pExp2019=round(X2019/rangeSize,3),pExp2020=round(X2020/rangeSize,3),pExp2023=round(X2023/rangeSize,3))
	# data.frame(	spStats)
	saveRDS(spStats,file=paste0(outDirSp,'/',basename(x1)))

},mc.cores=10)







# make long format of speciesID, cellID, isexposed in 2023
f=list.files('/Users/ctg/Documents/SDMs/Exposure_2023/climThresholds/t2m-q099_99p',full.names=T)

f=list.files('/Users/ctg/Documents/SDMs/Exposure_2023/climThresholds/t2m-q095_95p',full.names=T)
#scen='t2m-q099_100p'; scen='tp-q001_1p'
scen='tp-q005_5p';  scen='tp-q001_0p'; scen='tp-q0_0p'; scen='tp-q0_1p'
scen='t2m-q095_95p'; scen='t2m-q099_99p'; scen='t2m-q1_99p'; scen='t2m-q099_100p'; scen='t2m-q1_100p'; scen='t2m-q099_99p'

vars=list.files(paste0(computer,'/Documents/SDMs/Exposure_2023/climThresholds'))
for(scen in vars){
	f=list.files(paste0( '/Users/ctg/Documents/SDMs/Exposure_2023/climThresholds/',scen),full.names=T)
	mc.cores=12
	sp.chop=chopTasks(f,mc.cores)
	# do for any given year. 
	out=mclapply(1:mc.cores,function(x){
		sp1=sp.chop[[x]]
		print(x)
		print('===================================================')
		tmp=lapply(seq_along(sp1),function(y){
			print(y)
			sp.name=sub(paste0('_',scen,'.tif'),'',basename(sp1[y]))
			r=rast(sp1[y])
			notNA=cells(r)
			vals=terra::extract(r,notNA)
			data.frame(sp=sp.name,cellID=notNA,vals)		
		})
		bind_rows(tmp)
	},mc.cores=mc.cores) 
	out1=bind_rows(out)

	saveRDS(out1,file=paste0( '/Users/ctg/Dropbox/Projects/2023_Exposure/SpeciesXCellXExposure_1990-2023_',scen,'_v2.rds'))
}

# put table in raster
template=stack('/Users/ctg/Documents/SDMs/Exposure_2023/climDeparture/departure_tp_ref_1940-2022_proj_2023.tif')[[6]]>0
values(template)[!is.na(values(template))]=0
# think i can do this from the df
head(d1[[1]])
out1=readRDS('/Users/ctg/Dropbox/Projects/2023_Exposure/SpeciesXCellXExposure_1990-2023_t2m-q099_99p.rds')

















