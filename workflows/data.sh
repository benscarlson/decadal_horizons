setopt interactivecomments
bindkey -e

pd=~/projects/decadal_horizons
wd=$pd/analysis/main
src=$pd/src

mkdir -p $wd

cd $wd

#----
#---- Pep's data
#----

zfile=$pd/data/pdh_forBen/spCells_and_timeSeries.zip
#zfile=/Users/benc/projects/decadal_horizons/data/pdh_forBen/spCells_and_timeSeries.zip

unzip -l $zfile | grep '/$' #show only the top-level directories

unzip -Zt $zfile | awk '{printf "%.2fGB\n", $3/1024/1024/1024;}'

unzip $zfile -d $pd/data/pdh_forBen

$pd/data/pdh_forBen

#----
#---- Trinary Maps ----
#----

ppm="https://www.dropbox.com/scl/fi/brlnmj7emk975yy0n9slz/TrinaryMaps_PPM.zip?rlkey=rgceqyzh06fm71b3byh7qou8n&st=eqqa9zbw&dl=0"
rb=https://www.dropbox.com/scl/fi/ruwwbz70vn66h64hlnxrp/TrinaryMaps_rb.zip?rlkey=twg5ajzroeaprrkovil5hvpam&st=zytrlnnr&dl=0

mkdir -p $pd/data/trinary
wget $ppm -O $pd/data/trinary/TrinaryMaps_PPM.zip

ls -lh $pd/data/trinary
unzip -Zt $pd/data/trinary/TrinaryMaps_PPM.zip | awk '{printf "%.2fGB\n", $3/1024/1024/1024;}'

#TODO: see if names from Pep's file match names in TrinaryMaps
# It seems Cory's folder structure is
# PPM/BinaryMaps
# PPM/TrinaryMaps
unzip -l $pd/data/trinary/TrinaryMaps_PPM.zip | head
unzip -l $pd/data/trinary/TrinaryMaps_PPM.zip | wc -l

 #Delete all these. Use approach in roc-quantiles_2.qmd to extract just the files I need instead of everything
unzip $pd/data/trinary/TrinaryMaps_PPM.zip -d $pd/data/trinary
mv $pd/data/trinary/Users/ctg/Documents/SDMs/BIEN_1123/_outputs/PPM $pd/data/trinary
rm -r $pd/data/trinary/Users #Top level folder in the zip file, now empty.
#Never mind, delete all the ranges I don't need them right now.
rm -r $pd/data/trinary/PPM

unzip -l $pd/data/trinary/TrinaryMaps_PPM.zip | grep Achillea_maritima
