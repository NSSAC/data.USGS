#!/usr/bin/env bash

PROJDIR="/project/bi_dpi"

outdir="${1:-$PROJDIR/data/USGS/Mineral_Commodity_Summaries_2025_Data_Release}"
mkdir -p $outdir
cd $outdir
pwd
module load curl || true

curl -vOJ https://www.sciencebase.gov/catalog/file/get/677eaf95d34e760b392c4970

mkdir tmp

mv *.zip tmp

function myunzip { 
    unzip "$1" -d "${1%.zip}"
}
unzip tmp/*.zip -d ./

for file in *.zip; do
  myunzip "$file"
done

rm -rf tmp *.zip
