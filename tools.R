USCALE <- 2.1522e-2
ISCALE <- 4.61806e-3
RATE <- 6.4e3
SAMPLES_PER_PERIOD <- 128

# If I don't have a full period to work with, just
# return NA values which can then be excluded later.
#
rms <- function(v, n) {
    if (n < SAMPLES_PER_PERIOD)
        return(NA)
    else
        return(sqrt(sum((v[max(1, n-127):n])^2)/SAMPLES_PER_PERIOD))
}

# Create a data from from a time and instantaneous vector, replacing the
# instantaneous vector to a rms vector. time and v should have same length and
# must greater than SAMPLES_PER_PERIOD
#
rms_df <- function(time, v) {
    d <- data.frame(
               Time = time,
               Rms = sapply(1:length(time), function(n) rms(v, n))
               )
    # RMS values at the first SAMPLES_PER_PERIOD samples are NA's
    #
    d[SAMPLES_PER_PERIOD:nrow(d),]
}

# The ims waveform original data is in 'data', the 'plot_ui()' function plot
# voltage and current for a given time interval.
#
ui_inst <- function(data, t1=NULL, t2=NULL, phase, type='p') {
    if (! is.null(t1))
        data <- subset(data, Time >= t1)
    if (! is.null(t2))
        data <- subset(data, Time <= t2)
    par(bg='cornsilk', mfrow=c(2,length(phase)))
    sapply(phase, function(n) {
               plot(data$Time, data[, paste('U', n, sep='')] * USCALE,
                    main='',
                    xlab='Time (s)',
                    ylab=paste('U', n, ' (V)', sep=''),
                    col='orange', type=type)
               }
    )
    sapply(phase, function(n) {
               plot(data$Time, data[, paste('I', n, sep='')] * ISCALE,
                    main='',
                    xlab='Time (s)',
                    ylab=paste('I', n, ' (A)', sep=''),
                    col='blue', type=type)
               }
    )
}

ui_hist <- function(data, t1=NULL, t2=NULL, phase) { 
    par(bg='cornsilk', mfrow=c(2, length(phase)))
    sapply(phase, function(n) {
               hist(data[, paste('U', n, sep='')] * USCALE,
                    xlab='Voltage (V)', main='')
               }
    )
    sapply(phase, function(n) {
               hist(data[, paste('I', n, sep='')] * ISCALE,
                    xlab='Current (A)', main='')
               }
    )
}

cut_time <- function(d, t, ncycles=10, align=.5) {
    intvl <- c(t, t + ncycles * SAMPLES_PER_PERIOD * 1/RATE)
    intvl <- intvl - align * ncycles * SAMPLES_PER_PERIOD * 1/RATE
    subset(d, Time >= intvl[1] & Time <= intvl[2])
}

read_wf <- function(filename) {
    d <- read.csv(filename)
    if ('Seqno' %in% names(d)) {
        d$Time <- d$Seqno * 1/RATE
        d <- d[, -1]
    }
    if (! ('U1Scaled' %in% names(d)))
        d %>%
            mutate(U1Scaled = U1 * USCALE) %>%
            mutate(I1Scaled = I1 * ISCALE) %>%
            mutate(U2Scaled = U2 * USCALE) %>%
            mutate(I2Scaled = I2 * ISCALE) %>%
            mutate(U3Scaled = U3 * USCALE) %>%
            mutate(I3Scaled = I3 * ISCALE)
}

save_plot <- function(plot, name, dir='.') {
    svg(paste(dir, '/', name, '.svg', sep=''))
    plot()
    dev.off()
    png(paste(dir, '/', name, '.png', sep=''))
    plot()
    dev.off()
}

dev_new <- function(n=1, m=1, name = NULL) {
    if (! is.null(name)) {
        basename <- paste(head(str_split(name, '\\.')[[1]], -1),
                         collapse='.')
        ext <- tail(str_split(name, '\\.')[[1]], 1)
        if (ext == 'png') {
            png(paste(basename, ext, sep=''))
        } else if (ext == 'svg') {
            svg(paste(basename, ext, sep=''))
        } else if (ext == 'pdf') {
            pdf(paste(basename, ext, sep=''))
        }
    } else
        dev.new()
    par(bg='cornsilk', mfrow=c(n, m))
}

