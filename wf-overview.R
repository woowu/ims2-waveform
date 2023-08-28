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

print(paste('calculate oe'))
oe <- ui_oe.calc(data, phase=phase)
sapply(names(oe), function(ol_or_ex) {
    li <- oe[[ol_or_ex]]
    sapply(names(li), function(u_or_i) {
        li <- li[[u_or_i]]
        sapply(names(li), function(phase) {
            li <- li[[phase]]
            if (length(li$time)) {
                fname=paste(namebase, '-', ol_or_ex,
                           '-', u_or_i,
                           substr(phase, 2, 2),
                           '.csv', sep='')
                d <- data.frame(Time=li$time, Value=li$value)
                write.csv(d, fname, row.names=F)
            }
        })
    })
})

print(paste('plot oe'))
save_plot(function() {
              plot.ui_oe(oe$ol, oe$ex,
                         time_scale=c(min(data$Time), max(data$Time)))
}, name=paste(namebase, '-oe', sep=''))

print(length(oe$ex$U$L2$time))
print(oe)
t <- lapply(names(oe), function(ol_or_ex) {
    li <- oe[[ol_or_ex]]
    t <- lapply(names(li), function(u_or_i) {
        li <- li[[u_or_i]]
        unname(sapply(names(li), function(phase) {
            if (length(li[[phase]]$time) > 0)
                li[[phase]]$time
            else
                c(NA)
        }))
    })
    names(t) <- names(li)
    t
})
names(t) <- names(oe)
oe_time <- list(U=union(t$ol$U, t$ex$U), I=union(t$ol$I, t$ex$I))
ui_time <- as.numeric(na.omit(union(oe_time$U, oe_time$I)))

print(paste('plot oe details'))
sapply(ui_time, function(t) {
    if (! is.na(t)) {
        marker.u=c()
        marker.i=c()
        if (! is.null(oe_time$U) && t %in% oe_time$U) marker.u=c(t)
        if (! is.null(oe_time$I) && t %in% oe_time$I) marker.i=c(t)
        save_plot(function() {
            plot.ui_inst(data, c(t - PERIOD, t + PERIOD), phase=phase,
                marker.u=marker.u,
                marker.i=marker.i)
        }, name=paste(namebase, '-oe-inst-', t, sep=''))
        save_plot(function() {
            plot.ui_inst(data, c(t - 7.5, t + 7.5), phase=phase,
                marker.u=marker.u,
                marker.i=marker.i)
        }, name=paste(namebase, '-oe-inst-', t, '-long', sep=''))
    }
})

print(paste('plot timeline'))
save_plot(function() plot.rms_and_phase(data, phase=phase,
                                        threshold=c(.05, .1, .1745),
                                        marker=ui_time),
          name=paste(namebase, '-timeline', sep=''))
