
# Obviously need to change this... but what is the best way?
TEST.DATA.DIR <- '/users/nick/desktop/bctR/tests/test_data/'


load.sample <- function(thres=1){
  return(threshold.proportional(get(load(file.path(TEST.DATA.DIR,
                                                   'sample.data.Rdata'))),
                                thres))
}

load.signed.sample <- function(thres=1){
  return(round(threshold.proportional(get(load(file.path(TEST.DATA.DIR,
                                                         'sample.signed.Rdata'))),
                                      thres),
               8))
}

load.sparse.sample <- function(thres=0.02){
  return(threshold.proportional(get(load(file.path(TEST.DATA.DIR,
                                                   'sample.data.Rdata'))),
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