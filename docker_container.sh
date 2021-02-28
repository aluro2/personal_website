#!/bin/bash

docker run --rm -d -p 8787:8787 -v $(pwd):/home/rstudio -e DISABLE_AUTH=true --name personal-website-build aluro2/rstudio-blogdown-personal-website:v1

