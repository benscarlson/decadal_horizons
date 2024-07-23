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

cp $BZY_SCRIPT $src/main/roc_quantiles.r
cp $BZY_WF $src/poc/workflows/roc_quantiles.sh

cp $BZY_QMD $src/reports/roc_quantiles_results.qmd
cp $BZY_WF_QMD $src/poc/reports/roc_quantiles_results.sh


cp $BZY_WF $src/workflows/data.sh



