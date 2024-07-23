setopt interactivecomments
bindkey -e

pd=~/projects/decadal_horizons
wd=$pd/analysis/poc/roc_quantiles
src=$pd/src

mkdir -p $wd

cd $wd

ranges=$wd/data/trinary_500
out=$wd/data/roc_quantiles.csv
  
$src/main/roc_quantiles.r $ranges $out -p mc -c 8

