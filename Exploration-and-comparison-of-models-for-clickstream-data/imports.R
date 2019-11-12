library(tibble)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tidyverse)
library(reshape2)
library(readr)
library(clickstream)
library("arulesSequences")
library(plotly)
library(HMM)
library(hablar)
library(readr)
library(broom)
library(ndtv)
library(networkD3)
library(threejs)
library(visNetwork)
library(igraph)
library(GGally)
library(network)
library(sna)
library(ggplot2)
library(MASS)
library(caret)
library(caTools)
library(RColorBrewer)
library(corrplot)
library(arm)
library(markovchain)

sweep_row_matrix <- function(x){
  sweep(x = x, 
        MARGIN = 1,
        STATS = rowSums(x),
        FUN = "/")
}

sweep_matrix <- function(x){
  sweep(x = x, 
        MARGIN = 1,
        STATS = rowSums(x),
        FUN = "/")
}

sweep_matrix_cols <- function(x){
  sweep(x = x, 
        MARGIN = 2,
        STATS = colSums(x),
        FUN = "/")
}
