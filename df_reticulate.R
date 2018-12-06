# R quick and dirty so I don't forget what I figured out today


library(reticulate)
# THIS is what fixes that stupid "no module named ..." message
# NOTE: this relies on there being most of datafinisher being 
# copied/linked over to the same director as this.
py$sys$path <- c(py$sys$path,paste0(getwd(),'/datafinisher'));
source_python('df_reticulate.py')

# afterward, can access objects via py$FOO
