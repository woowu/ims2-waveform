#!/usr/bin/Rscript --vanilla
library(optparse)
library(dplyr)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(cowplot)

#USCALE <- 2.15372e-2
#ISCALE <- 4.60236e-3
USCALE <- 2.1522e-2
ISCALE <- 4.61806e-3
SAMPLES_PER_PERIOD <- 128
RATE <- 6.4e3

option_list = list(
    make_option(c("-f", "--filename"), type="character", help="cvs filename"),
    make_option(c("-z", "--zoom-in"), type="character", default='0-1', help="zooom-in sectons, e.g., 0.2-0.25,0.65-0.75")
)
 
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

if (is.null(opt$filename)) {
    print_help(opt_parser)
    stop('no csv filename provided')
}

range_spec <- lapply(
           str_split(str_split(opt$'zoom-in', ',')[[1]], '-'),
           as.numeric)

data <- read.csv(opt$filename)

data <- data %>%
    mutate(U1Scaled = U1 * USCALE) %>%
    mutate(I1Scaled = I1 * ISCALE) %>%
    mutate(U2Scaled = U2 * USCALE) %>%
    mutate(I2Scaled = I2 * ISCALE) %>%
    mutate(U3Scaled = U3 * USCALE) %>%
    mutate(I3Scaled = I3 * ISCALE) %>%
    mutate(Time = Seqno * (1/RATE))

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

range_spec <- lapply(range_spec, function(range, data) {
           sapply(as.integer(range * nrow(data)), function(n) as.integer(max(n, 1)))
        }, data=data)

plot_range <- function(range, data) {
    timeHMS_formatter <- function(s) {
        h <- floor(s/3600)
        m <- floor((s/60) %% 60)
        s <- round(s %% 60)
        lab <- sprintf("%02d:%02d:%02d", h, m, s)
        lab <- sub("^00:", "", lab) # Remove leading 00: if present
    }

    plot <- function(data) {
        ggplot(data, aes(x=Time, y=value, color=variable)) +
            geom_line() +
            scale_color_manual(values = c('red', 'blue')) +
            labs(y='Voltage/Current') +
            scale_x_continuous(name='Time', labels=timeHMS_formatter) +
            theme(legend.position='none')
    }

    data <- data[range[1]:range[2],]

    inst_plots <- lapply(1:3, function(phase) {
            plot(melt(data[, c('Time',
                               paste('U', phase, 'Scaled', sep=''),
                               paste('I', phase, 'Scaled', sep=''))
                          ], id = c('Time')))
        })
    rms_plots <- lapply(1:3, function(phase) {
            plot(melt(data[, c('Time',
                                paste('U', phase, 'Rms', sep=''),
                                paste('I', phase, 'Rms', sep=''))
                          ], id = c('Time')))
        })

    inst_row <- plot_grid(inst_plots[[1]], inst_plots[[2]], inst_plots[[3]], ncol=3)
    rms_row <- plot_grid(rms_plots[[1]], rms_plots[[2]], rms_plots[[3]], ncol=3)

    inst_title <- ggdraw() + 
        draw_label('Instantaneous', fontface = 'bold', x = 0, hjust = 0) +
        theme(plot.margin = margin(0, 0, 0, 7))
    rms_title <- ggdraw() + 
        draw_label('RMS', fontface = 'bold', x = 0, hjust = 0) +
        theme(plot.margin = margin(0, 0, 0, 7))
    plot_grid(inst_title, inst_row, rms_title, rms_row, ncol=1, rel_heights=c(.1, .4, .1, .4))
}

plots <- lapply(range_spec, plot_range, data=data)

pdf('plots.pdf', width=16.5, height=11.7)
plots
dev.off()
