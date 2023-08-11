#!/usr/bin/Rscript --vanilla
library(optparse)
library(dplyr)
library(reshape2)
library(ggplot2)

#USCALE <- 2.15372e-2
#ISCALE <- 4.60236e-3
USCALE <- 2.1522e-2
ISCALE <- 4.61806e-3
SAMPLES_PER_PERIOD <- 128

option_list = list(
    make_option(c("-f", "--filename"), type="character", help="cvs filename"),
    make_option(c("-s", "--frame-start"), type="integer", help="frame count start"),
    make_option(c("-e", "--frame-end"), type="integer", help="frame count end")
)
 
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

if (is.null(opt$filename)) {
    print_help(opt_parser)
    stop('no csv filename provided')
}

data <- read.csv(opt$filename)
if (! is.null(opt$'frame-start')) {
    data <- data %>% filter(FrameCount >= opt$'frame-start')
}
if (! is.null(opt$'frame-end')) {
    data <- data %>% filter(FrameCount <= opt$'frame-end')
}

data <- data %>%
    mutate(U1Scaled = U1 * USCALE) %>%
    mutate(I1Scaled = I1 * ISCALE) %>%
    mutate(U2Scaled = U2 * USCALE) %>%
    mutate(I2Scaled = I2 * ISCALE) %>%
    mutate(U3Scaled = U3 * USCALE) %>%
    mutate(I3Scaled = I3 * ISCALE)

rms <- function(v, n) {
    if (n < SAMPLES_PER_PERIOD)
        return(c(NA))
    else
        return(sqrt(sum((v[max(1, n-127):n])^2)/SAMPLES_PER_PERIOD))
}

data$U1Rms <- sapply(1:nrow(data), function(n) rms(data$U1Scaled, n))
data$I1Rms <- sapply(1:nrow(data), function(n) rms(data$I1Scaled, n))
data$U2Rms <- sapply(1:nrow(data), function(n) rms(data$U2Scaled, n))
data$I2Rms <- sapply(1:nrow(data), function(n) rms(data$I2Scaled, n))
data$U3Rms <- sapply(1:nrow(data), function(n) rms(data$U3Scaled, n))
data$I3Rms <- sapply(1:nrow(data), function(n) rms(data$I3Scaled, n))

data <- data[SAMPLES_PER_PERIOD:nrow(data),]

inst_phase1 <- data[, c('FrameCount','U1Scaled','I1Scaled')]
inst_phase1 <- melt(inst_phase1, id = c('FrameCount'))
inst_phase2 <- data[, c('FrameCount','U2Scaled','I2Scaled')]
inst_phase2 <- melt(inst_phase2, id = c('FrameCount'))
inst_phase3 <- data[, c('FrameCount','U3Scaled','I3Scaled')]
inst_phase3 <- melt(inst_phase3, id = c('FrameCount'))

rms_phase1 <- data[, c('FrameCount','U1Rms','I1Rms')]
rms_phase1 <- melt(rms_phase1, id = c('FrameCount'))
rms_phase2 <- data[, c('FrameCount','U2Rms','I2Rms')]
rms_phase2 <- melt(rms_phase2, id = c('FrameCount'))
rms_phase3 <- data[, c('FrameCount','U3Rms','I3Rms')]
rms_phase3 <- melt(rms_phase3, id = c('FrameCount'))

plot <- function(data) {
    return(ggplot(data, aes(x=FrameCount, y=value, color=variable)) +
        geom_line() +
        scale_color_manual(values = c('red', 'blue')) +
        labs(x = 'Frame count', y = 'Amplitude', color = '')
    )
}

suffix=''
if (! is.null(opt$'frame-start'))
    suffix <- paste(suffix, 's', opt$'frame-start', sep='')
if (! is.null(opt$'frame-end'))
    suffix <- paste(suffix, 'e', opt$'frame-end', sep='')
if (nchar(suffix) > 0)
    suffix <- paste('-', suffix, sep='')

ggsave(paste('phase-1-inst', suffix, '.png', sep=''), plot(inst_phase1))
ggsave(paste('phase-2-inst', suffix, '.png', sep=''), plot(inst_phase2))
ggsave(paste('phase-3-inst', suffix, '.png', sep=''), plot(inst_phase3))

ggsave(paste('phase-1-rms', suffix, '.png', sep=''), plot(rms_phase1))
ggsave(paste('phase-2-rms', suffix, '.png', sep=''), plot(rms_phase2))
ggsave(paste('phase-3-rms', suffix, '.png', sep=''), plot(rms_phase3))
