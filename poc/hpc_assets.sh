
setopt interactivecomments
bindkey -e

pd=~/projects/decadal_horizons

#--- start HPC
ssh bsc23001@login.storrs.hpc.uconn.edu

cd /gpfs/sharedfs1/mcu08001/pdh_forBen

zfile=$pd/data/pdh_forBen/spCells_and_timeSeries.zip
zfile=/Users/benc/projects/decadal_horizons/data/pdh_forBen/spCells_and_timeSeries.zip

unzip -Zt spCells_and_timeSeries.zip | awk '{printf "%.2fGB\n", $3/1024/1024/1024;}'

#--- end HPC


rsync -Pr bsc23001@login.storrs.hpc.uconn.edu:/gpfs/sharedfs1/mcu08001/pdh_forBen $pd/data

#Try to download spCells and timeSeries.zip again

file=/gpfs/sharedfs1/mcu08001/pdh_forBen/spCells_and_timeSeries.zip

rsync -P bsc23001@login.storrs.hpc.uconn.edu:$file $pd/data/pdh_forBen
