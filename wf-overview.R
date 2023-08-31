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
sapply(phase, function(n) {
    save_plot(function() plot.ui_hist(data, phase=n:n),
              name=paste(namebase, '-hist-l', n, sep=''))
})

print(paste('calculate oe'))
oe <- ui_oe.calc(data, phase=phase)
sapply(names(oe), function(ol_or_ex) {
    li <- oe[[ol_or_ex]]
    sapply(names(li), function(u_or_i) {
        li <- li[[u_or_i]]
        sapply(names(li), function(phase) {
            li <- li[[phase]]
            if (! is.null(li$time) && length(li$time)) {
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
sapply(phase, function(n) {
    save_plot(function() {
                  plot.ui_oe(oe$ol, oe$ex,
                             phase=n:n,
                             time_scale=c(min(data$Time), max(data$Time)))
    }, name=paste(namebase, '-oe-l', n, sep=''))
})

t <- lapply(names(oe), function(ol_or_ex) {
    li <- oe[[ol_or_ex]]
    t <- lapply(names(li), function(u_or_i) {
        li <- li[[u_or_i]]
        lapply(names(li), function(phase) {
                   li[[phase]]$time
        })
    })
    names(t) <- names(li)
    t
})
names(t) <- names(oe)

# Organize all the oe times in two forms. One is in ol and ex separably, the
# other is all together.
#
all_u <- unique(do.call(c, c(t$ol$U, t$ex$U)))
all_u <- all_u[! is.na(all_u)]
if (length(all_u) == 0) all_u <- c()
all_i <- unique(do.call(c, c(t$ol$I, t$ex$I)))
all_i <- all_i[! is.na(all_i)]
if (length(all_i) == 0) all_i <- c()
oe_time <- list(u=all_u, i=all_i)
event_time <- sort(unique(c(all_u, all_i)))

# event_time could be a large set, but times in the set many crowed together
# very closly. For what many times spanning no more than the width of our
# observation window, we plot the data in a single plot. For this purpose,
# we need to split the whole event_time set into groups.
#
grp <- list()
window <- PERIOD
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

print(paste('plot timeline'))
sapply(phase, function(n) {
    save_plot(function() plot.rms_and_phase(data, phase=n:n,
                                            threshold=c(.05, .1, .1745),
                                            marker=event_time),
              name=paste(namebase, '-timeline-l', n, sep=''))
})

print(paste('plot oe details'))

# for each time group, we plot a detail u/i waveform around the
# median time of the group.
#
lapply(grp, function(g) {
    margin <- 1.5 * PERIOD
    larger_margin <- 15
    marker.u=c()
    marker.i=c()
    for (t in g) {
        if (t %in% oe_time$u)
            marker.u <- append(marker.u, t)
        if (t %in% oe_time$i)
            marker.i <- append(marker.i, t)
    }
    g <- sort(g)
    window <- c(min(g) - margin, max(g) + margin)
    grp_name <- paste(sprintf('%.4f', g[1]), '-',
                      sprintf('%.4f', g[length(g)]),
                      sep='')
    sapply(phase, function(n) {
        save_plot(function() {
                      plot.ui_inst(data, window, phase=n:n,
                                   marker.u=marker.u,
                                   marker.i=marker.i)
                          },
                      name=paste(namebase, '-oe-inst-', grp_name, '-l', n, sep=''))

        window <- c(min(g) - larger_margin, max(g) + larger_margin)
        save_plot(function() {
                      plot.ui_inst(data, window, phase=n:n,
                                   marker.u=marker.u,
                                   marker.i=marker.i)
                      },
                      name=paste(namebase, '-oe-inst-', grp_name, '-long-l', n, sep=''),
                      png='T', svg='F')
    })
})
