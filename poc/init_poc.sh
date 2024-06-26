setopt interactivecomments
bindkey -e

pd=~/projects/decadal_horizons
src=$pd/src

cp $BZY_SCRIPT $src/poc/myscript.r
cp $BZY_WF $src/poc/myscript.sh

cp $BZY_QMD $src/poc/reports/myreport.qmd
cp $BZY_WF_QMD $src/poc/reports/myreport.sh

#---- Files ----

cp $BZY_QMD $src/poc/reports/quantiles.qmd
cp $BZY_WF_QMD $src/poc/reports/quantiles.sh

cp $BZY_QMD $src/poc/reports/roc-quantiles.qmd
cp $BZY_WF_QMD $src/poc/reports/roc-quantiles.sh



