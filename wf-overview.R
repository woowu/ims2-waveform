#!/usr/bin/Rscript --vanilla
library(optparse)
library(stringr)

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
print(paste(nrow(data), 'samples'))

if (is.null(opt$phase)) {
    phase <- 1:3
} else {
    phase <- opt$phase:opt$phase
}

time_of_lost <- detect_lost(data$Time)
if (! is.null(time_of_lost))
    write.csv(data.frame(Time=),
              paste(namebase, '-lost.csv', sep=''), row.names=F)

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

#------------------------------------------------------------------------------

print(paste('plot timeline'))
sapply(phase, function(n) {
    save_plot(function() plot.rms_and_phase(data, phase=n:n,
                                            threshold=c(.05, .1, .1745),
                                            marker=event_time),
              width=640, height=640, 
              name=paste('01-timeline_', namebase, '_l', n, sep=''))
})

print(paste('plot oe'))
sapply(phase, function(n) {
    save_plot(function() {
                  plot.ui_oe(oe$ol, oe$ex,
                             phase=n:n,
                             time_scale=c(min(data$Time), max(data$Time)))
    }, name=paste('02-oe_', namebase, '_l', n, sep=''))
})

print(paste('plot histograms'))
sapply(phase, function(n) {
    save_plot(function() plot.ui_hist(data, phase=n:n),
              name=paste('03-hist_', namebase, '_l', n, sep=''))
})

# event_time could be a large set, but times in the set many crowed together
# very closly. For what many times spanning no more than the width of our
# observation window, we plot the data in a single plot. For this purpose,
# we need to split the whole event_time set into groups.
#

small_grp <- group_time(time, PERIOD)
large_grp <- group_time(time, 10)

split_time_to_ui <- function(time, u, i) {
    time.u <- time[time %in% u]
    if (length(time.u) == 0) time.u <- c()
    time.i <- time[time %in% i]
    if (length(time.i) == 0) time.i <- c()
    list(u=time.u, i=time.i)
}

# for each time group, we plot a detail u/i waveform around the
# median time of the group.
#
grp_view <- list(s1=small_grp, s2=large_grp)
margin <- c(.5 * PERIOD, 7.5)
png_flag <- c(F, T)
svg_flag <- c(T, F)
lapply(1:length(grp_view), function(idx) {
    view <- grp_view[[idx]]
    margin <- margin[[idx]]
    png <- png_flag[idx]
    svg <- svg_flag[idx]
    name <- names(grp_view)[idx]

    lapply(1:length(view), function(idx) {
        g <- view[[idx]]
        if (is.null(g)) return(NULL)
        print(paste('plot oe detail (',
                    name, ') ',
                    idx, '/', length(view),
                    ': ', g, sep=''))
        markers <- split_time_to_ui(g, oe_time$u, oe_time$i)
        time_id <- paste(sprintf('%.4f', g[1]), '-',
                          sprintf('%.4f', g[length(g)]),
                          sep='')
        sapply(phase, function(n) {
            save_plot(function() {
                range <- c(min(g) - margin, max(g) + margin)
                if (range[2] - range[1] < 1.5 * PERIOD) {
                    e <- (1.5 * PERIOD - (max(g) - min(g))) / 2
                    range <- c(min(g) - e, max(g) + e)
                }
                plot.ui_inst(data,
                             c(min(g) - margin, max(g) + margin), phase=n:n,
                             marker.u=markers$u,
                             marker.i=markers$i)
            },
            name=paste('oe-inst_', namebase, '_',
                       name, '-', str_pad(idx, 3, pad='0'),
                       '-', time_id, '-l', n, sep=''),
                       png=png, svg=svg)
        })
    })
})
