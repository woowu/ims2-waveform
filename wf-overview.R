#!/usr/bin/Rscript --vanilla
library(optparse)

VOLTAGE = 230
IMAX = 100
PERIOD = 0.02

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
print(paste('load', opt$filename))
data <- read_wf(opt$filename)

if (is.null(opt$phase)) {
    phase <- 1:3
} else {
    phase <- opt$phase:opt$phase
}

print(paste('plot histograms'))
save_plot(function() plot.ui_hist(data, phase=phase),
          name=paste(namebase, '-hist', sep=''))
print(paste('plot timeline'))
save_plot(function() plot.rms_and_phase(data, phase=phase, threshold=c(.05, .1, .1745)),
          name=paste(namebase, '-timeline', sep=''))

save_plot(function() {
              plot.ui_oe(data, phase=phase,
                         name=paste(namebase, sep=''))
}, name=paste(namebase, '-oe', sep=''))

sapply(phase, function(n) {
    d <- read.csv(paste(namebase, '-oe-time-union-', n, '.csv', sep=''))
    sapply(d$Time, function(t) {
        save_plot(function() {
            plot.ui_inst(data, c(t - PERIOD, t + PERIOD), phase=n:n)
        }, name=paste(namebase, '-oe-inst-', t, sep=''))
        save_plot(function() {
            plot.ui_inst(data, c(t - 1.5, t + 1.5), phase=n:n)
        }, name=paste(namebase, '-oe-inst-', t, '-long', sep=''))
    })
})
