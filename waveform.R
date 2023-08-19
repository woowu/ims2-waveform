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

options(max.print=60 * RATE)

option_list = list(
    make_option(c('-f', '--filename'), type='character', help='cvs filename'),
    make_option(c('-z', '--zoom-in'), type='character',
                help='zooom-in sectons in secs, e.g., 195-199.5,301.5-305.2'),
    make_option(c('-P', '--no-plot'), action='store_true', default=FALSE,
                help='not to plot'),
    make_option(c('-p', '--phase'), type='numeric', default=NULL,
                help='select a single phase'),
    make_option(c('--hi-res'), action='store_true', default=FALSE,
                help='save HiRes png')
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
        sec <- floor(s %% 60)
        tenth_s <- round((s - floor(s)) * 100)
        lab <- sprintf("%02d:%02d:%02d.%02d", h, m, sec, tenth_s)
        lab <- sub("^00:", "", lab) # Remove leading 00: if present
    }

    plot2 <- function(data, ymin, ymax) {
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
            plot2(melt(data[, c('Time',
                               paste('U', phase, 'Scaled', sep=''),
                               paste('I', phase, 'Scaled', sep=''))
                          ], id = c('Time')), ymin=-350, ymax=350)
        })
    rms_plots <- lapply(phases, function(phase) {
            plot2(melt(data[, c('Time',
                                paste('U', phase, 'Rms', sep=''),
                                paste('I', phase, 'Rms', sep=''))
                          ], id = c('Time')), ymin=0, ymax=250)
        })

    if (is.null(phase)) {
        inst_row <- plot_grid(inst_plots[[1]], inst_plots[[2]], inst_plots[[3]], ncol=3)
        rms_row <- plot_grid(rms_plots[[1]], rms_plots[[2]], rms_plots[[3]], ncol=3)
    } else {
        inst_row <- plot_grid(inst_plots[[1]], ncol=1)
        rms_row <- plot_grid(rms_plots[[1]], ncol=1)
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
    plot_grid(inst_title, inst_row,
              rms_title, rms_row,
              ncol=1, rel_heights=c(.05, .45, .05, .45))
}

calc_data_size <- function(range, data) {
    nrow(data %>% filter(Time >= range[1] & Time <= range[2]))
}

save_by_index <- function(n, plots, range_spec, sizes, prefix) {
    name <- paste(prefix, range_spec[[n]][1], '-', range_spec[[n]][2], sep='')
    w <- ifelse(is.null(opt$phase), 21, 7)
    h <- 7
    if (opt$'hi-res') {
        ggsave(paste(name, '.png', sep=''), plots[[n]],
               width=w, height=h, dpi=400)
    } else {
        ggsave(paste(name, '.png', sep=''), plots[[n]],
               width=w, height=h)
    }
    ggsave(paste(name, '.svg', sep=''), plots[[n]],
           width=w, height=h)
}

plots <- lapply(range_spec, plot_by_range, data=data, phase=opt$phase)
sizes <- lapply(range_spec, calc_data_size, data=data)
lapply(1:length(range_spec), save_by_index, plots=plots, range_spec=range_spec,
       sizes=sizes,
       prefix=paste(name_prefix, '-', sep='')
)
