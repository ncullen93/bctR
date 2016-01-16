
# Code for loading sample data


load.sample <- function(thres=1){
  sample.data <- '/users/nick/desktop/bctr/data/sample.data.Rdata'
  return(threshold.proportional(get(load(sample.data)),
                                thres))
}

load.signed.sample <- function(thres=1){
  return(round(threshold.proportional(get(load(sample.signed.data)),
                                      thres),
               8))
}

load.sparse.sample <- function(thres=0.02){
  return(threshold.proportional(get(load(sample.data)),
                                thres))
}

load.binary.sample <- function(thres){}

load.directed.sample <- function(thres){}

load.binary.directed.sample <- function(thres){}

load.directed.low.modularity.sample <- function(thres){}

load.binary.directed.low.modularity.sample <- function(thres){}

load.sample.group.qball <- function(){}

load.sample.group.dsi <- function(){}

load.sample.group.fmri <- function(){}