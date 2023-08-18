#!/usr/bin/Rscript --vanilla

# After received IMS waveform data were saved in a csv file, this script do
# plotting of data to show actual waveforms and RMS values of data samples.
# From a big csv file, the script also provides options do a cut (zoom) on a
# group of the specified ranges of data.
#

library(optparse)
library(dplyr)
library(tidyverse)
library(dplR)
library(reshape2)
library(ggplot2)
library(cowplot)

USCALE <- 2.1522e-2
ISCALE <- 4.61806e-3
SAMPLES_PER_PERIOD <- 128
RATE <- 6.4e3
PAGE_W <- 8.268
PAGE_H <- 11.693

options(max.print=60 * RATE)

option_list = list(
    make_option(c('-f', '--filename'), type='character', help='cvs filename'),
    make_option(c('-z', '--zoom-in'), type='character',
                help='zooom-in sectons in secs, e.g., 195-199.5,301.5-305.2'),
    make_option(c('-P', '--no-plot'), action='store_true', default=FALSE,
                help='not to plot'),
    make_option(c('-p', '--phase'), type='numeric', default=NULL,
                help='select a single phase')
)
 
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

if (is.null(opt$filename)) {
    print_help(opt_parser)
    stop('no csv filename provided')
}

name_prefix <- paste(head(str_split(opt$filename, '\\.')[[1]], -1),
                     collapse='.')
if (! is.null(opt$phase))
    name_prefix <- paste(name_prefix, '-p', opt$phase, sep='')

data <- read.csv(opt$filename)
data <- data %>%
    mutate(U1Scaled = U1 * USCALE) %>%
    mutate(I1Scaled = I1 * ISCALE) %>%
    mutate(U2Scaled = U2 * USCALE) %>%
    mutate(I2Scaled = I2 * ISCALE) %>%
    mutate(U3Scaled = U3 * USCALE) %>%
    mutate(I3Scaled = I3 * ISCALE)

# If I don't have a full period to work with, just
# return NA values which can then be excluded later.
#
rms <- function(v, n) {
    if (n < SAMPLES_PER_PERIOD)
        return(NA)
    else
        return(sqrt(sum((v[max(1, n-127):n])^2)/SAMPLES_PER_PERIOD))
}

# RMS values are calculated from the previous whole period
#
data$U1Rms <- sapply(1:nrow(data), function(n) rms(data$U1Scaled, n))
data$I1Rms <- sapply(1:nrow(data), function(n) rms(data$I1Scaled, n))
data$U2Rms <- sapply(1:nrow(data), function(n) rms(data$U2Scaled, n))
data$I2Rms <- sapply(1:nrow(data), function(n) rms(data$I2Scaled, n))
data$U3Rms <- sapply(1:nrow(data), function(n) rms(data$U3Scaled, n))
data$I3Rms <- sapply(1:nrow(data), function(n) rms(data$I3Scaled, n))

# RMS values at the first SAMPLES_PER_PERIOD samples are NA's
#
data <- data[SAMPLES_PER_PERIOD:nrow(data),]

# low-pass smooth the Rms
data$U1RmsLowPass <- pass.filt(data$U1Rms, W=0.2, type='low', method='Butterworth')
data$U2RmsLowPass <- pass.filt(data$U2Rms, W=0.2, type='low', method='Butterworth')
data$U3RmsLowPass <- pass.filt(data$U3Rms, W=0.2, type='low', method='Butterworth')
data$I1RmsLowPass <- pass.filt(data$I1Rms, W=0.2, type='low', method='Butterworth')
data$I2RmsLowPass <- pass.filt(data$I2Rms, W=0.2, type='low', method='Butterworth')
data$I3RmsLowPass <- pass.filt(data$I3Rms, W=0.2, type='low', method='Butterworth')

# A function repsented as two vectors x and y. 'diff' calculate the derivative
# of the function.
#
diff <- function(x, y, n) {
    if (length(y) > n)
        return((y[n + 1] - y[n])/(x[n + 1] - x[n]))
    else
        return(NA)
}

# Drivatives calulation were commented out because the plotting
# of them are not used now.
#
# Derivatives of Rms low-pass'ed
#
#data$dU1Rms <- sapply(1:nrow(data), function(n) diff(data$Time, data$U1RmsLowPass, n))
#data$dU2Rms <- sapply(1:nrow(data), function(n) diff(data$Time, data$U2RmsLowPass, n))
#data$dU3Rms <- sapply(1:nrow(data), function(n) diff(data$Time, data$U3RmsLowPass, n))
#data$dI1Rms <- sapply(1:nrow(data), function(n) diff(data$Time, data$I1RmsLowPass, n))
#data$dI2Rms <- sapply(1:nrow(data), function(n) diff(data$Time, data$I2RmsLowPass, n))
#data$dI3Rms <- sapply(1:nrow(data), function(n) diff(data$Time, data$I3RmsLowPass, n))
# The 2nd Derivatives
#
#data$d2U1Rms <- sapply(1:nrow(data), function(n) diff(data$Time, data$dU1Rms, n))
#data$d2U2Rms <- sapply(1:nrow(data), function(n) diff(data$Time, data$dU2Rms, n))
#data$d2U3Rms <- sapply(1:nrow(data), function(n) diff(data$Time, data$dU3Rms, n))
#data$d2I1Rms <- sapply(1:nrow(data), function(n) diff(data$Time, data$dI1Rms, n))
#data$d2I2Rms <- sapply(1:nrow(data), function(n) diff(data$Time, data$dI2Rms, n))
#data$d2I3Rms <- sapply(1:nrow(data), function(n) diff(data$Time, data$dI3Rms, n))

# The value of the 2nd derivative are NA's in the last two rows.
#
data <- data[1:(nrow(data) - 2),]

if (opt$'no-plot') {
    quit()
}

if (is.null(opt$'zoom-in')) {
    range_spec <- list(c(min(data$Time), max(data$Time)))
} else {
    range_spec <- lapply(
               str_split(str_split(opt$'zoom-in', ',')[[1]], '-'),
               as.numeric)
}

plot_by_range <- function(range, data, phase) {
    timeHMS_formatter <- function(s) {
        h <- floor(s/3600)
        m <- floor((s/60) %% 60)
        s <- round(s %% 60)
        lab <- sprintf("%02d:%02d:%02d", h, m, s)
        lab <- sub("^00:", "", lab) # Remove leading 00: if present
    }

    plot <- function(data, ymin, ymax) {
        ggplot(data, aes(x=Time, y=value, color=variable)) +
            geom_line(size=.2) +
            scale_color_manual(values = c('orange', 'blue')) +
            labs(y='Voltage/Current') +
            scale_x_continuous(name='Time', labels=timeHMS_formatter) +
            ylim(ymin, ymax) +
            theme_bw() +
            theme(legend.position='none',
                  panel.background = element_rect(fill = "cornsilk", colour = NA),
                  plot.background = element_rect(fill = "cornsilk", colour = NA),
            )
    }

    data <- data %>% filter(Time >= range[1] & Time <= range[2])
    write.csv(data, paste(name_prefix, '-', range[1], '-', range[2],
                          '-processed.csv', sep=''), row.names=FALSE)

    if (is.null(phase)) {
        phases <- 1:3
    } else {
        phases <-phase:phase
    }
    inst_plots <- lapply(phases, function(phase) {
            plot(melt(data[, c('Time',
                               paste('U', phase, 'Scaled', sep=''),
                               paste('I', phase, 'Scaled', sep=''))
                          ], id = c('Time')), ymin=-350, ymax=350)
        })
    rms_plots <- lapply(phases, function(phase) {
            plot(melt(data[, c('Time',
                                paste('U', phase, 'Rms', sep=''),
                                paste('I', phase, 'Rms', sep=''))
                          ], id = c('Time')), ymin=0, ymax=250)
        })
    #d2rms_plots <- lapply(phases, function(phase) {
    #        plot(melt(data[, c('Time',
    #                            paste('d2U', phase, 'Rms', sep=''),
    #                            paste('d2I', phase, 'Rms', sep=''))
    #                      ], id = c('Time')))
    #    })

    if (is.null(phase)) {
        inst_row <- plot_grid(inst_plots[[1]], inst_plots[[2]], inst_plots[[3]], ncol=3)
        rms_row <- plot_grid(rms_plots[[1]], rms_plots[[2]], rms_plots[[3]], ncol=3)
        #d2rms_row <- plot_grid(d2rms_plots[[1]], d2rms_plots[[2]], d2rms_plots[[3]], ncol=3)
    } else {
        inst_row <- plot_grid(inst_plots[[1]], ncol=1)
        rms_row <- plot_grid(rms_plots[[1]], ncol=1)
        #d2rms_row <- plot_grid(d2rms_plots[[1]], ncol=1)
    }

    inst_title <- ggdraw() + 
        draw_label('Instantaneous', fontface = 'bold', x = 0, hjust = 0) +
        theme(plot.margin = margin(0, 0, 0, 7),
              plot.background = element_rect(fill = "cornsilk", color = NA)
        )
    rms_title <- ggdraw() + 
        draw_label('RMS', fontface = 'bold', x = 0, hjust = 0) +
        theme(plot.margin = margin(0, 0, 0, 7),
              plot.background = element_rect(fill = "cornsilk", color = NA)
        )
    #d2rms_title <- ggdraw() + 
    #    draw_label(expression(d^2 ~ RMS), fontface = 'bold', x = 0, hjust = 0) +
    #    theme(plot.margin = margin(0, 0, 0, 7),
    #          plot.background = element_rect(fill = "cornsilk", color = NA)
    #    )
    plot_grid(inst_title, inst_row,
              rms_title, rms_row,
              #d2rms_title, d2rms_row,
              ncol=1, rel_heights=c(.05, .45, .05, .45))
}

calc_data_size <- function(range, data) {
    nrow(data %>% filter(Time >= range[1] & Time <= range[2]))
}

save_by_index <- function(n, plots, range_spec, sizes, prefix) {
    name <- paste(prefix, range_spec[[n]][1], '-', range_spec[[n]][2], sep='')
    ggsave(paste(name, '.png', sep=''), plots[[n]],
           width=PAGE_H, height=PAGE_W, dpi=400)
    if (sizes[[n]] <= 20e3) {
        ggsave(paste(name, '.pdf', sep=''), plots[[n]],
               width=PAGE_H, height=PAGE_W)
        ggsave(paste(name, '.svg', sep=''), plots[[n]],
               width=PAGE_H, height=PAGE_W)
    }
}

plots <- lapply(range_spec, plot_by_range, data=data, phase=opt$phase)
sizes <- lapply(range_spec, calc_data_size, data=data)
lapply(1:length(range_spec), save_by_index, plots=plots, range_spec=range_spec,
       sizes=sizes,
       prefix=paste(name_prefix, '-', sep='')
)
