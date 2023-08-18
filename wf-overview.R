#!/usr/bin/Rscript --vanilla
library(optparse)
library(stringr)

USCALE <- 2.1522e-2
ISCALE <- 4.61806e-3

option_list = list(
    make_option(c('-f', '--filename'), type='character', help='cvs filename')
)
 
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

if (is.null(opt$filename)) {
    print_help(opt_parser)
    stop('no csv filename provided')
}

name_prefix <- paste(head(str_split(opt$filename, '\\.')[[1]], -1),
                     collapse='.')


data <- read.csv(opt$filename)
svg(paste(name_prefix, '-hist.svg', sep=''))
par(mfrow=c(2, 3))
sapply(1:3, function(n) hist(data[, paste('U', n, sep='')] * USCALE,
                             main=paste('U', n, sep=''))) 
sapply(1:3, function(n) hist(data[, paste('I', n, sep='')] * ISCALE,
                             main=paste('I', n, sep=''))) 
dev.off()
