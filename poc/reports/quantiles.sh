setopt interactivecomments
bindkey -e

pd=~/projects/decadal_horizons
wd=$pd/analysis/poc/reports/quantiles
src=$pd/src

mkdir -p $wd

cd $wd

qmd=$src/poc/reports/quantiles.qmd

fbase=${${qmd##*/}%.qmd} #Get file name without extenstion
out=$wd/$fbase.html

mkdir -p ${out%/*}

quarto render $qmd -P wd:$wd
mv ${qmd%.*}.html $out
open $out

mkdir ~/projects/reports/reports/docs/decadal_horizons

cp $out ~/projects/reports/reports/docs/decadal_horizons
