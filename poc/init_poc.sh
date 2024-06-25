setopt interactivecomments
bindkey -e

pd=~/projects/decadal_horizons
src=$pd/src

cp $BZY_SCRIPT $src/poc/quantiles.r
cp $BZY_WF $src/poc/quantiles.sh

cp $BZY_QMD $src/poc/reports/poc-myreport.qmd
cp $BZY_WF_QMD $src/poc/reports/poc-myreport.sh

