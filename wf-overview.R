#!/usr/bin/Rscript --vanilla
library(optparse)

VOLTAGE = 230
IMAX = 100
IB = 10
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

write.csv(data.frame(Time=detect_lost(data$Time)),
          paste(namebase, '-lost.csv', sep=''), row.names=F)

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

print(paste('plot oe details'))

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
event_time <- sort(as.numeric(na.omit(union(oe_time$U, oe_time$I))))

# event_time could be a large set, but times in the set many crowed together
# very closly. For what many times span no more than the width of our
# observation window, we plot the data in a single plot. For this purpose,
# we need to split the whole event_time set into groups.
#
grp <- list()
window <- 3 * PERIOD
der <- c(0, diff(event_time))
acc_dist <- 0
j <- 0
for (i in 1:length(der)) {
    acc_dist <- acc_dist + der[i]
    if (acc_dist >= window) {
        grp <- append(grp, list(event_time[j:(i-1)]))
        acc_dist <- 0
        j <- i
    }
}
grp <- append(grp, list(event_time[j:length(event_time)]))

# for each time group, we plot a detail u/i waveform around the
# median time of the group.
#
lapply(grp, function(g) {
    long_window <- 15
    marker.u=c()
    marker.i=c()
    for (t in g) {
        if (t %in% oe_time$U)
            marker.u <- append(marker.u, t)
        if (t %in% oe_time$I)
            marker.i <- append(marker.i, t)
    }
    m <- median(g)
    grp_name <- paste(sprintf('%.4f', g[1]), '-',
                      sprintf('%.4f', g[length(g)]),
                      sep='')
    save_plot(function() {
        plot.ui_inst(data, c(m - window/2, m + window/2),
                     phase=phase,
                     marker.u=marker.u,
                     marker.i=marker.i)
    }, name=paste(namebase, '-oe-inst-', grp_name, sep=''))
    save_plot(function() {
        plot.ui_inst(data, c(m - long_window/2, m + long_window/2), phase=phase,
                     marker.u=marker.u,
                     marker.i=marker.i)
    }, name=paste(namebase, '-oe-inst-', grp_name, '-long', sep=''),
    png='T', svg='F')
})

print(paste('plot timeline'))
save_plot(function() plot.rms_and_phase(data, phase=phase,
                                        threshold=c(.05, .1, .1745),
                                        marker=event_time),
          name=paste(namebase, '-timeline', sep=''))
