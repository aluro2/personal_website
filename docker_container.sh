#!/bin/bash
echo "Building RStudio container for website build"

docker run --rm -d -p 8787:8787 -v $(pwd):/home/rstudio -e DISABLE_AUTH=true --name personal-website-build aluro2/rstudio-blogdown-personal-website:v1

echo "Container ready! Go to http://localhost:8787/"