setopt interactivecomments
bindkey -e

pd=~/projects/decadal_horizons
wd=$pd/analysis/poc/roc_quantiles
src=$pd/src

mkdir -p $wd

cd $wd

#qmd=$src/poc/reports/roc-quantiles.qmd
qmd=$src/poc/reports/roc-quantiles_2.qmd

fbase=${${qmd##*/}%.qmd} #Get file name without extenstion
out=$wd/$fbase.html

mkdir -p ${out%/*}

quarto render $qmd -P wd:$wd
mv ${qmd%.*}.html $out
open $out

RPT_HOME=~/projects/reports/reports/docs

mkdir $RPT_HOME/decadal_horizons

cp $out $RPT_HOME/decadal_horizons

rptsrc=~/projects/reports/reports

git -C $rptsrc status
git -C $rptsrc add .
git -C $rptsrc status
git -C $rptsrc commit -am 'add/update reports'
git -C $rptsrc push

echo https://benscarlson.github.io/reports/decadal_horizons/${out##*/}

