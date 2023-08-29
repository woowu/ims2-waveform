library(tidyverse)
#library(dplR)

USCALE <- 2.1522e-2
ISCALE <- 4.61806e-3
RATE <- 6.4e3
SAMPLES_PER_PERIOD <- 128
MAX_TIME_POINTS_TO_PLOT <- 672 # don't save pnt more than this size
VOLTAGE = 230
IMAX = 100
IB = 10

color.u <- 'darkorange1'
color.i <- 'blue2'
color.u.alpha <- rgb(1, .498, 0, alpha=.7)   # darkorange1 + alpha
color.i.alpha <- rgb(0, 0, .933, alpha=.7)     # blue2 + alpha
color.oe_marker <- rgb(0, 0, 0, alpha=.35)

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
# @time
# @value
# @return a list of (time, rms)
#
rms.map <- function(time, value) {
    d <- data.frame(
               Time = time,
               Rms = sapply(1:length(time), function(n) rms.calc(value, n))
               )
    # RMS values at the first SAMPLES_PER_PERIOD - 1 samples are NA's
    #
    d$Rms[1:(SAMPLES_PER_PERIOD - 1)] <- 0
    #d <- d[SAMPLES_PER_PERIOD:nrow(d),]
    list(time = d$Time, rms = d$Rms)
}

# Reduce a rms series by just keeping the points of significant changes
# @time
# @rms
# @threshold output the value only when (new - prev)/prev > threshold
# @return a list of reduced rms series
#
rms.reduce <- function(time, rms, threshold=0) {
    r<- c()
    t <- c() 
    last <- -Inf
    len <- length(time)

    for (i in 1:len) {
        val <- rms[i]

        # the first, last and those deviated from the last value for over
        # the threshold points are output
        if (i %% floor(len/MAX_TIME_POINTS_TO_PLOT) == 0 
            || i == len
            || is.nan(last * threshold)
            || abs((val - last)) > last * threshold) {
            last <- val
            t <- append(t, time[i])
            r <- append(r, val)
        }
    }
    print(paste('rms series downsampled from ', length(time),
                ' to ', length(t), sep=''))
    list(time=t, rms=r)
}

#
# @state the state variable for the crossing detection
# @time time of the new point
# @value value of the new point
# @return updated state variable
#
cross.put_next <- function(state, time, value) {
    if (value == 0)
        return(state)
    if (is.null(state)) {
        return(list(last_value = value, last_time = time, cross_time = NULL))
    } else if (state$last_value < 0 & value > 0) {
        return(list(last_value = value, last_time = time, cross_time = time))
    } else {
        return(list(last_value = value, last_time = time,
                    cross_time = state$cross_time))
    }
}

# @time time, u and i form a time series of AC signals
# @u
# @i
# @freq.f0 fundamental frequency
# @threshold only output points when it deviates from the previous one for
#   the givin threshold value.
# @return a list of (time, theta). The elements of vector PhaseShift are in
#   range of [0, 2pi), which defined as the radian difference between U and I
#   using I - U.
#
phase_shift <- function(time, u, i, freq.f0=50, threshold=0) {
    period <- 1/freq.f0
    ts <- c()
    phase <- c()    # between (-pi, pi]

    state.u <- NULL
    state.i <- NULL

    for (idx in 1:length(time)) {
        t <- time[idx]
        state.u <- cross.put_next(state.u, t, u[idx])
        state.i <- cross.put_next(state.i, t, i[idx])

        # idx is not crossing
        if (is.null(state.i$cross_time) || state.i$cross_time != t)
            next

        # when idx is crossing but u has not yet crossed
        if (is.null(state.u$cross_time))
            next

        # now idx is crossing and u is also crossing or crossed
        # at some time ago, we're able to do phase comparing
        diff <- t - state.u$cross_time

        # need wait for a new crossing on U
        state.u$cross_time <- NULL

        # time diff should witin a period, we may lost some
        # samples
        if (diff >= period)
            next

        # convert the ange into range of (-p2, pi]
        #
        rad <- (diff/period)*2*pi
        if (rad > pi) rad <- rad - 2*pi

        ts <- append(ts, t)
        phase <- append(phase, rad)
    }
    li <- list(time=ts, theta=phase)
    if (threshold == 0)
        return(li)

    t <- c()
    p <- c() 
    last.rad <- 0
    len <- length(li$theta)

    for (idx in 1:len) {
        rad <- li$theta[idx]

        # calculate the difference within the boundary of (-pi, pi]
        #
        diff <- (rad - last.rad) %% (2*pi)
        if (diff >= pi) diff <- -(2*pi - diff)

        if (idx %% floor(len/MAX_TIME_POINTS_TO_PLOT) == 0
            || idx == len
            || abs(diff) > threshold) {
            last.rad <- rad
            t <- append(t, li$time[idx])
            p <- append(p, rad)
        }
    }
    list(time=t, theta=p)
}

# @k_start start value of the times of interquartile range
# @stop stop searching better k when the result size close to 'stop' times the
#   original data size
# @kp every next iteration, the k will increase kp * error
#
outlier <- function(time, value, stop=1e-6, k_start=2, kp=0.75) {
    der <- c(0, diff(value))

    k <- k_start
    pk <- k
    sp <- max(stop * length(time), 5)
    cnt <- 0
    err <- Inf

    while (cnt < 100) {
        ix <- which(abs(der) > k * IQR(der))
        err <- (length(ix) - sp)/sp
        if (abs(err) < .05) break

        # calculate the next k using proportional way
        #
        pk <- k
        k <- k + max(min(err * kp, 2), -2)

        # stop if algorithm keep bouncing
        #
        if (pk * k < 0) {
            bouncing <- bouncing + 1
        } else
            bouncing <- 0
        if (bouncing == 3) break

        cnt <- cnt + 1
    }
    #print(c(sp, length(ix), k, err, cnt))

    ol.time <- rep(NA, length(ix))
    ol.value <- rep(NA, length(ix))
    ol.time[ix] <- time[ix] 
    ol.value[ix] <- value[ix] 
    list(time=ol.time, value=ol.value)
}

# @threshold if absolute of the value greater than the threshold, the value
#   will be output
#
extremer <- function(time, value, threshold) {
    ix <- which(abs(value) > threshold)
    ol.time <- rep(NA, length(ix))
    ol.value <- rep(NA, length(ix))
    ol.time[ix] <- time[ix] 
    ol.value[ix] <- value[ix] 
    list(time=ol.time, value=ol.value)
}

cut_time <- function(d, t, ncycles=10, align=.5) {
    intvl <- c(t, t + ncycles * SAMPLES_PER_PERIOD * 1/RATE)
    intvl <- intvl - align * ncycles * SAMPLES_PER_PERIOD * 1/RATE
    subset(d, Time >= intvl[1] & Time <= intvl[2])
}

u_i_range_of_all_phases <- function(data) {
    li <- lapply(c('U', 'I'), function(q) {
               lapply(1:3, function(n) {
                          col <- paste(q, n, 'Scaled', sep='')
                          c(min(data[, col]), max(data[, col]))
               })
    })
    names(li) <- c('u', 'i')
    list(u=c(min(do.call(cbind, li$u)[1,]), max(do.call(cbind, li$u)[2,])),
         i=c(min(do.call(cbind, li$i)[1,]), max(do.call(cbind, li$i)[2,])))
}

ui_value_scale_to_plot_scale <- function(min_max) {
    if (min_max$u[1] > -VOLTAGE) min_max$u[1] <- min_max$u[1] * 1.1
    if (min_max$u[2] < VOLTAGE) min_max$u[2] <- min_max$u[2] * 1.1
    if (min_max$i[1] > -IB) min_max$i[1] <- min_max$u[1] * 1.1
    if (min_max$i[2] < IB) min_max$i[2] <- min_max$u[2] * 1.1
    min_max
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
# @marker.u a vector contains time values which should be used to draw vertical
#   ablines on U plot
# @marker.i a vector contains time values which should be used to draw vertical
#   ablines on I plot
#
plot.ui_inst <- function(data, time_range=c(-Inf, Inf), phase, type='p',
                         marker.u=c(), marker.i=c()) {
    par(bg='cornsilk', mfrow=c(2,length(phase)))

    data <- subset(data, Time >= time_range[1])
    data <- subset(data, Time <= time_range[2])

    quantity <- c('U', 'I')
    min_scale <- c(VOLTAGE/10, IB/10)
    scales <- c(USCALE, ISCALE)
    if (nrow(data) > 2 * RATE) {
        colors <- c(color.u.alpha, color.i.alpha)
    } else {
        colors <- c(color.u, color.i)
    }
    marker <- list(marker.u, marker.i)

    sapply(1:length(quantity), function(m) {
        sapply(phase, function(n) {
                   val_min <- min(data[, paste(quantity[m], n, sep='')] * scales[m])
                   val_max <- max(data[, paste(quantity[m], n, sep='')] * scales[m])
                   miny <- min(-min_scale[m] * sqrt(2), val_min)
                   maxy <- max(min_scale[m] * sqrt(2), val_max)
                   plot(data$Time,
                        data[, paste(quantity[m], n, sep='')] * scales[m],
                        main='',
                        xlab='Time (s)',
                        ylab=paste(quantity[m], n, sep=''),
                        ylim=c(miny, maxy),
                        col=colors[m], type=type,
                        panel.first=c(abline(h=c(0, val_min, val_max), lty=3),
                                      abline(v=marker[[m]], lty=3)))
                   #smoothScatter(data$Time,
                   #              data[, paste(quantity[m], n, sep='')] * scales[m],
                   #              main='',
                   #              xlab='Time (s)',
                   #              ylab=paste(quantity[m], n, sep=''),
                   #              ylim=c(miny, maxy))
                   #abline(h=c(0, val_min, val_max), lty=3)
                   #abline(v=marker[[m]], lty=3)
        })
    })
}

# Plot U/I histogram
#
plot.ui_hist <- function(data, time_range=c(-Inf, Inf), phase) { 
    par(bg='cornsilk', mfrow=c(2, length(phase)))

    data <- subset(data, Time >= time_range[1])
    data <- subset(data, Time <= time_range[2])
    if (nrow(data) == 0) stop('empty data')

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

# Plot U/I RMS as well as phase angle trajetories with reduced samples, limited
# by a change rage threshold
# @threshold sample reducing threshold of U, I and Phase shift 
#
plot.rms_and_phase <- function(data, time_range=c(-Inf, Inf), phase,
                               threshold=c(0, 0, 0),
                               marker=c(),
                               type='l') {
    par(bg='cornsilk', mfrow=c(3, length(phase)))

    data <- subset(data, Time >= time_range[1])
    data <- subset(data, Time <= time_range[2])
    if (nrow(data) < SAMPLES_PER_PERIOD) stop('data size is not enough')

    ui_mm <- u_i_range_of_all_phases(data)
    ui_mm.scale <- ui_value_scale_to_plot_scale(ui_mm)

    q <- c('U', 'I')
    qq <- c('u', 'i')
    scales <- c(USCALE, ISCALE)
    colors <- c('orange', 'blue')
    max_rms <- c(VOLTAGE, IMAX)

    sapply(1:length(q), function(m) {
        sapply(phase, function(n) {
               colname <- paste(q[m], n, sep='')
               r <- rms.map(time=data$Time,
                            value=data[, colname] * scales[m])
               li <- rms.reduce(r$time, r$rms, threshold=threshold[1])
               val_max <- max(data[, colname]) * scales[m]
               plot(li$time, li$rms,
                    main='',
                    xlab='Time (s)',
                    ylab=paste(q[m], n, sep=''),
                    ylim=c(0, ui_mm.scale[[qq[m]]][2]),
                    col=colors[m], type=type,
                    panel.first=c(abline(v=marker, lty=3, col=color.oe_marker)))
               abline(h=c(0, ui_mm[[qq[m]]][2]), lty=3)
        })
    })

    sapply(phase, function(n) {
           u <- data[, paste('U', n, 'Scaled', sep='')]
           i <- data[, paste('I', n, 'Scaled', sep='')]
           li <- phase_shift(time=data$Time,
                             u=u, i=i, threshold=threshold[3])
           pos1 <- seq(-pi, pi, by=pi/6)
           pos <- seq(-pi, pi, by=pi/2)
           tickmark <- expression(-pi, '', 0, '', pi)
           plot(li$time, li$theta,
                main='',
                xlab='Time (s)',
                ylab=expression(theta[u-i]),
                ylim=c(-pi, pi),
                yaxt='none',
                col='seagreen', type=type,
                panel.first=c(abline(v=marker, lty=3, col=color.oe_marker)))
           axis(2, at=pos1, label=F, tck=-.015)
           axis(2, at=pos, label=tickmark, tck=-.03)
           abline(h=pos, lty=3)
    })
}

# This is a wrapper function, using a data frame to call outlier and extremer,
# then return ol and oe list.
# @return a 4-layer list of <ol|ex>/<U|I>/L<n>/<time|value>
#
ui_oe.calc <- function (data, time_range=c(-Inf, Inf), phase) {
    data <- subset(data, Time >= time_range[1])
    data <- subset(data, Time <= time_range[2])

    type <- c('U', 'I')
    phase_name <- paste('L', phase, sep='')
    ex_threshold <- c(VOLTAGE * sqrt(2) * 1.05, IMAX * sqrt(2) * 1.05)

    ol <- lapply(1:length(type), function(m) {
        li <- lapply(phase, function(n) {
            value <- data[, paste(type[m], n, 'Scaled', sep='')]

            print(paste('calculate outlier on phase ',
                        n, ' for ', type[m], sep=''))
            li <- outlier(data$Time, value)
            li$time <- as.numeric(na.omit(li$time))
            li$value <- as.numeric(na.omit(li$value))
            li
        })
        names(li) <- phase_name
        li
    })
    names(ol) <- type

    ex <- lapply(1:length(type), function(m) {
        li <- lapply(phase, function(n) {
            value <- data[, paste(type[m], n, 'Scaled', sep='')]

            print(paste('calculate extermers on phase ',
                        n, ' for ', type[m], sep=''))
            li <- extremer(data$Time, value, threshold=ex_threshold[m])
            li$time <- as.numeric(na.omit(li$time))
            li$value <- as.numeric(na.omit(li$value))
            li
        })
        names(li) <- phase_name
        li
    })
    names(ex) <- type
    list(ol=ol, ex=ex)
}

# Plot U/I outliers and extremers
# @ol outliers, a 3-layer list of <U|I>/L<n>/<time|value>
# @ex extremers, a 3-layer list of <U|I>/L<n>/<time|value>
#
plot.ui_oe <- function(ol, ex, time_scale=NULL) {
    # the min/max U/I need firstly looked up from the data
    # then adjusted with some reasonable default scale.
    #
    oe <- list(ol=ol, ex=ex)
    mm <- lapply(names(oe), function(ol_or_ex) {
        li <- oe[[ol_or_ex]]
        mm <- lapply(names(li), function(u_or_i) {
            li <- li[[u_or_i]]
            mm <- do.call(cbind, lapply(names(li),
                        function(phase) {
                            c(min(li[[phase]]$value), max(li[[phase]]$value))
                        }))
            c(min(mm[1,]), max(mm[2,]))
        })
        names(mm) <- names(li)
        mm
    })
    names(mm) <- names(oe)
    umin <- min(cbind(mm$ol$U, mm$ex$U)[1,])
    umax <- max(cbind(mm$ol$U, mm$ex$U)[2,])
    imin <- min(cbind(mm$ol$I, mm$ex$I)[1,])
    imax <- max(cbind(mm$ol$I, mm$ex$I)[2,])

    umin <- min(umin, -VOLTAGE * sqrt(2))
    umax <- max(umax, VOLTAGE * sqrt(2))
    imin <- min(imin, -IB * sqrt(2))
    imax <- max(imax, IB * sqrt(2))

    # check the min/max time in all the data if the caller not
    # provided a time scale.
    #
    if (is.null(time_scale)) {
        mm <- lapply(names(oe), function(ol_or_ex) {
            li <- oe[[ol_or_ex]]
            mm <- lapply(names(li), function(u_or_i) {
                li <- li[[u_or_i]]
                mm <- do.call(cbind, lapply(names(li),
                            function(phase) {
                                c(min(li[[phase]]$time), max(li[[phase]]$time))
                            }))
                c(min(mm[1,]), max(mm[2,]))
            })
            names(mm) <- names(li)
            mm
        })
        names(mm) <- names(oe)
        mm <- do.call(cbind, list(mm$ol$U, mm$ol$I, mm$ex$U, mm$ex$I))
        time_scale <- c(min[1,], max[2,])
    }

    par(bg='cornsilk', mfrow=c(2,length(ol$U)))

    y_scale <- list(U=c(umin, umax), I=c(imin, imax))
    type <- c('U', 'I')
    colors <- c('orange', 'blue')

    # n is 'L1', 'L2', or 'L3'
    sapply(names(ol$U), function(n) {
        # m is index of c('U', 'I')
        sapply(1:length(type), function(m) {
            plot(ol[[type[m]]][[n]]$time, ol[[type[m]]][[n]]$value,
                 main='',
                 xlab='Time (s)',
                 ylab=paste(type[m], substr(n, 2, 2), sep=''),
                 xlim=time_scale,
                 ylim=y_scale[[type[m]]],
                 col=colors[m], type='p')
            points(ex[[type[m]]][[n]]$time, ex[[type[m]]][[n]]$value, pch=8, col='red')
            abline(h=0, lty=3)
        })
    })
    NULL
}

save_plot <- function(plot, name, dir='.', w=480, h=480, png=F, svg=T) {
    if (svg) {
        svg(paste(dir, '/', name, '.svg', sep=''))
        plot()
        dev.off()
    }
    if (png) {
        png(paste(dir, '/', name, '.png', sep=''), width=w, height=h, unit='px')
        plot()
        dev.off()
    }
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
plot.ui_phase_shift <- function(x = 0) {
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
