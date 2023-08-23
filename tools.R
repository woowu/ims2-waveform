library(tidyverse)
library(dplR)

USCALE <- 2.1522e-2
ISCALE <- 4.61806e-3
RATE <- 6.4e3
SAMPLES_PER_PERIOD <- 128

# If I don't have a full period to work with, just
# return NA values which can then be excluded later.
# @v vector of data series
# @n over n samples to calculate the RMS
#
rms.calc <- function(v, n) {
    if (n < SAMPLES_PER_PERIOD)
        return(NA)
    else
        return(sqrt(sum((v[max(1, n-127):n])^2)/SAMPLES_PER_PERIOD))
}

# Create a data from from a time and instantaneous vector, replacing the
# instantaneous vector to a rms vector. time and v should have same length and
# must greater than SAMPLES_PER_PERIOD
# @series a list of (Time, Value)
# @return a list of (Time, Rms)
#
rms.map <- function(series) {
    d <- data.frame(
               Time = series$Time,
               Rms = sapply(1:length(series$Time), function(n) rms.calc(series$Value, n))
               )
    # RMS values at the first SAMPLES_PER_PERIOD samples are NA's
    #
    d <- d[SAMPLES_PER_PERIOD:nrow(d),]
    list(Time = d$Time, Rms = d$Rms)
}

# Reduce a rms series by just keeping the points of significant changes
# @rms a list of (Time, Rms) series
# @threshold output the value only when (new - prev)/prev > threshold
# @return a list of reduced rms series
#
rms.reduce <- function(rms, threshold=0) {
    r<- c()
    t <- c() 
    last <- -Inf

    for (i in 1:length(rms$Rms)) {
        val <- rms$Rms[i]
        if (is.nan(last * threshold) || abs((val - last)) > last * threshold) {
            last <- val
            t <- append(t, rms$Time[i])
            r <- append(r, val)
        }
    }
    print(length(t))
    list(Time = t, Rms = r)
}

#
# @state the state variable for the crossing detection
# @time time of the new point
# @value value of the new point
# @return updated state variable
#
cross.put_next <- function(state, time, value) {
    if (value == 0) return(state)
    if (is.null(state)) {
        return(list(last_value = value, last_time = time, cross_time = NULL))
    } else if (state$last_value < 0 & value > 0) {
        return(list(last_value = value, last_time = time, cross_time = time))
    } else {
        return(list(last_value = value, last_time = time,
                    cross_time = state$cross_time))
    }
}

# @ac_signal a list of (Time, U, I) series
# @freq.f0 fundamental frequency
# @threshold only output points when it deviates from the previous one for
#   the givin threshold value.
# @return a list of (Time, PhaseDiff). The elements of vector PhaseDiff are in
#   range of [0, 2pi), which defined as the radian difference between U and I
#   using I - U.
#
phase_ange.calc <- function(ac_signal, freq.f0=50, threshold=0) {
    period <- 1/freq.f0
    time <- c()
    phase <- c()

    state.u <- NULL
    state.i <- NULL

    for (i in 1:length(ac_signal$Time)) {
        t <- ac_signal$Time[i]
        state.u <- cross.put_next(state.u, t, ac_signal$U[i])
        state.i <- cross.put_next(state.i, t, ac_signal$I[i])

        # i is not crossing
        if (is.null(state.i$cross_time) || state.i$cross_time != t)
            next

        # when i is crossing but u has not yet crossed
        if (is.null(state.u$cross_time))
            next

        # now i is crossing and u is also crossing or crossed
        # at some time ago, we're able to do phase comparing
        diff <- t - state.u$cross_time

        # time diff should witin a period, we may lost some
        # samples
        if (diff >= period)
            next

        time <- append(time, t)
        phase <- append(phase, (diff/period)*2*pi)
    }
    li <- list(Time=time, PhaseDiff=phase)
    if (threshold == 0)
        return(li)

    t <- c()
    p <- c() 
    last <- -Inf 

    for (i in 1:length(li$PhaseDiff)) {
        val <- li$PhaseDiff[i]
        if (val >= pi) val <- -(2*pi - val)
        if (abs((val - last)) > threshold) {
            last <- val
            t <- append(t, li$Time[i])
            p <- append(p, li$PhaseDiff[i])
        }
    }
    list(Time = t, PhaseDiff = p)
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

# The ims waveform original data is in 'data', this function plot
# voltage and current for a given time interval.
#
plot.ui_inst <- function(data, t1=NULL, t2=NULL, phase, type='p') {
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

# Plot U/I histogram
#
plot.ui_hist <- function(data, t1=NULL, t2=NULL, phase) { 
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

# Plot U/I RMS with reduced samples, limited by a change rage threshold
#
plot.rms_reduced <- function(data, t1=NULL, t2=NULL, phase, threshold=0, type='p') {
    if (! is.null(t1))
        data <- subset(data, Time >= t1)
    if (! is.null(t2))
        data <- subset(data, Time <= t2)
    par(bg='cornsilk', mfrow=c(2,length(phase)))
    sapply(phase, function(n) {
               li.rms <- rms.reduce(rms.map(list(Time=data$Time,
                                                 Value=data[, paste('U', n, sep='')] * USCALE)),
                                    threshold)
               plot(li.rms$Time, li.rms$Rms,
                    main='',
                    xlab='Time (s)',
                    ylab=paste('U', n, ' (V)', sep=''),
                    col='orange', type=type)
    })
    sapply(phase, function(n) {
               li.rms <- rms.reduce(rms.map(list(Time=data$Time,
                                                 Value=data[, paste('I', n, sep='')] * ISCALE)),
                                    threshold)
               plot(li.rms$Time, li.rms$Rms,
                    main='',
                    xlab='Time (s)',
                    ylab=paste('I', n, ' (A)', sep=''),
                    col='orange', type=type)
    })
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

# @x the phase difference betwee voltage and current, defined as phase of
#   voltage minus phase of current. we suppose voltage phase is zero.
#
plot.phase_angle <- function(x = 0) {
    v.rms <- 220
    i.rms <- 50
    f.0 <- 50
    w <- 2*pi*f.0
    acq.freq <- 100*f.0

    xs <- seq(-1/f.0, 1/f.0, 1/acq.freq)
    u <- v.rms*sqrt(2)*cos(w*xs);
    i <- i.rms*sqrt(2)*cos(w*xs - x);
    plot(xs, u, type='l', col='orange', xlab='time', ylab='u/i')
    lines(xs, i, col='blue')
    abline(h=0, lty=3)

    abline(v=0, lty=3)
    abline(v=x/w, lty=3)
}

# Plot a fourier series.
# fourier.series: the function synthesis the series with
#   arguments t and w.
# ts: time series
#
# E.g.:
#
#  acq.freq <- 100                      # data acquisition frequency (Hz)
#  time     <- 6                        # measuring time interval (seconds)
#  ts       <- seq(0, time, 1/acq.freq) # vector of sampling time-points (s)
#  f.0      <- 1/time                   # fundamental frequency (Hz)
#  
#  dc.component       <- 0
#  component.freqs    <- c(3,10)        # frequency of signal components (in times of the fundamental)
#  component.delay    <- c(0,0)         # delay of signal components (radians)
#  component.strength <- c(.5,.25)      # strength of signal components
#  
#  f <- function(t, w) {
#    dc.component +
#    sum( component.strength * cos(component.freqs * w * t - component.delay))
#  }
#  
#  plot.fourier(f, f.0, ts)
#
plot.fourier <- function(fourier.series, f.0, ts) {
    w <- 2 * pi * f.0
    trajectory <- sapply(ts, function(t) fourier.series(t, w))
    plot(ts, trajectory, type='l', xlab='time', ylab='f(t)')
    abline(h=0, lty=3)
}
