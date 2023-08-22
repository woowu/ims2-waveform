#!/usr/bin/Rscript --vanilla
library(optparse)
library(dplyr)

USCALE <- 2.1522e-2
ISCALE <- 4.61806e-3

option_list = list(
    make_option(c('-f', '--filename'), type='character', help='cvs filename'),
    make_option(c('-p', '--phase'), type='numeric', default=NULL,
                help='select a single phase')
)
 
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

if (is.null(opt$filename)) {
    print_help(opt_parser)
    stop('no csv filename provided')
}

source('~/R/local-script/ac-tools.R')
namebase <- sub('\\.[[:alnum:]]+$', '', basename(opt$filename))
data <- read_wf(opt$filename)

each_phase <- function(n) {
    print(paste('handling phase', n, sep=''))
    ri <- rms_df(data$Time, data[, paste('I', n, 'Scaled', sep='')])
    ru <- rms_df(data$Time, data[, paste('U', n, 'Scaled', sep='')])
    p <- function() {
        par(bg='cornsilk', mfrow=c(2, 1))
        hist(ru$Rms, main='', xlab=paste('U', n, ' RMS', sep=''))
        hist(ri$Rms, main='', xlab=paste('I', n, ' RMS', sep=''))
    }
    save_plot(p, name=paste(namebase, '-l', n, '-rms-hist', sep=''))
    p <- function() {
        par(bg='cornsilk', mfrow=c(2, 1))
        hist(data[, paste('U', n, 'Scaled', sep='')],
             main='', xlab=paste('U', n, sep='')) 
        hist(data[, paste('I', n, 'Scaled', sep='')],
             main='', xlab=paste('I', n, sep='')) 
    }
    save_plot(p, name=paste(namebase, '-l', n, '-inst-hist', sep=''))
}

if (is.null(opt$phase)) {
    phases <- 1:3
} else {
    phases <- opt$phase
}
sapply(phases, function(n) each_phase(n))
