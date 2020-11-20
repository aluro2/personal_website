
#Build dirs for htmlwidget dependencies
blogdown::build_dir()

# Build site, also building .Rmd files accordingly
blogdown::build_site(build_rmd = TRUE)
