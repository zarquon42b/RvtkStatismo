#! /bin/bash

if [[ "$1" == "1" ]]; then
    Rscript -e "cat(paste0(Sys.getenv('R_INCLUDE_DIR')))"

elif [[ "$1" == "2" ]]
then
    Rscript -e "cat(paste0(Sys.getenv('R_LIBS_SITE')))"

elif [[ "$1" == "3" ]]
then
    Rscript -e "cat(system.file(package='Rcpp'))"
fi

#echo $a
