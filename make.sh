#!/bin/sh

# CI script. This script expects R_HOME to be set to the home dir for R
# R will be run from ${R_HOME}/bin 

PATH=${PATH}:${R_HOME}/bin
export PATH
echo "Using path:" $PATH

if [ -z `which R` ]
then
    echo '*** Could not find the R executable. Exit(1)'
    exit 1
fi

case $1 in
    build)
        R CMD build .
        ;;
    check)
        R CMD check --as-cran --no-manual docplexcloud_1.0.0.tar.gz
        ;;
    *) 
        echo "`basename ${0}`: usage: (build|check)"
        exit 1
        ;;
esac