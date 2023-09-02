#!/usr/bin/Rscript --vanilla
library(optparse)
library(stringr)

VOLTAGE = 230
IMAX = 100
IB = 10
AC_PERIOD = 0.02

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

lost_time <- detect_lost(data$Time)
if (! is.null(lost_time))
    write.csv(data.frame(Time=lost_time),
              paste(namebase, '-lost.csv', sep=''), row.names=F)

print(paste('calculate oe'))
oe <- ui_oe.calc(data, phase=phase, lost_time=lost_time)
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

plot_timeline <- function(data, event_time, lost_time) {
    print(paste('plot timeline'))
    sapply(phase, function(n) {
        save_plot(function() plot.rms_and_phase(data, phase=n:n,
                                                threshold=c(.05, .1, .1745),
                                                marker.oe=event_time,
                                                marker.lost=lost_time),
                  width=640, height=640, 
                  name=paste('01-timeline_', namebase, '_l', n, sep=''))
    })
}

plot_histogram <- function(data) {
    print(paste('plot histogram'))
    sapply(phase, function(n) {
        save_plot(function() plot.ui_hist(data, phase=n:n),
                  name=paste('03-hist_', namebase, '_l', n, sep=''))
    })
}

plot_oe <- function(oe) {
    print('plot oe')
    sapply(phase, function(n) {
        save_plot(function() {
                      plot.ui_oe(oe$ol, oe$ex,
                                 phase=n:n,
                                 time_scale=c(min(data$Time), max(data$Time)))
        }, name=paste('02-oe_', namebase, '_l', n, sep=''))
    })
}

plot_oe_detail <- function(event_time, oe_time) {
    # Show events in different scales. We may combine nearby events into
    # a same group in order to reduce number of plots
    #

    scale <- c(1.5 * AC_PERIOD, .5, 12)
    grp_size <- c(.5 * AC_PERIOD, .5 * .9, 12 * .9) 
    png_flag <- c(F, F, T)
    svg_flag <- c(T, T, F)
    grp <- lapply(grp_size, function(sz) group_time(event_time, sz))
    names(grp) <- c('s1', 's2', 's3')

    # An event is either a voltage event or a current event. Given a vector of
    # events, this function split it into two categories for voltage and
    # current respectively.
    #
    split_time_to_ui <- function(time, u, i) {
        time.u <- time[time %in% u]
        if (length(time.u) == 0) time.u <- c()
        time.i <- time[time %in% i]
        if (length(time.i) == 0) time.i <- c()
        list(u=time.u, i=time.i)
    }

    # For each event group, we plot a detail u/i waveform around it.
    #

    lapply(1:length(grp), function(idx) {
        # g is group list of a particular scale
        g <- grp[[idx]]

        png <- png_flag[idx]
        svg <- svg_flag[idx]
        scale <- scale[idx]
        scale_name <- names(grp)[idx]

        lapply(1:length(g), function(idx) {
            # gg is a list of events (time) forming a group
            gg <- g[[idx]]

            margin <- (scale - gg[length(gg)] + gg[1])/2
            range <- c(max(0, min(gg) - margin), max(gg) + margin)
            markers <- split_time_to_ui(gg, oe_time$u, oe_time$i)
            time_id <- paste(sprintf('%.4f', range[1]), '_',
                              sprintf('%.4f', range[2]),
                              sep='')
            print(paste('plot oe detail (',
                        scale_name, ') ',
                        idx, '/', length(g), ': ', time_id, sep=''))
            sapply(phase, function(n) {
                plot_name=paste('oe-inst_', namebase, '_',
                           scale_name, '-', str_pad(idx, 3, pad='0'),
                           '-', time_id, '-l', n, sep='')
                save_plot(
                        function() {
                            plot.ui_inst(data,
                                range,
                                phase=n:n,
                                marker.u=markers$u,
                                marker.i=markers$i)
                        },
                        name=plot_name, png=png, svg=svg
                )
            })
        })
    })
}

if (nrow(data) > 0) {
    plot_timeline(data, event_time, lost_time)
    plot_histogram(data)
}
if (! is.null(event_time)) {
    plot_oe(oe)
    plot_oe_detail(event_time, oe_time)
}
