library(tidyverse)
library(dplR)
library(svglite)

USCALE <- 2.1522e-2
ISCALE <- 4.61806e-3
SR <- 6.4e3
F0 <- 50
SAMPLES_PER_PERIOD <- SR/F0
MAX_PLOT_WIDTH <- 400
SYS_DPI <- 96
VOLTAGE = 230
IMAX = 100
IB = 10
EPSILON = 1e-8

color.u <- 'darkorange1'
color.i <- 'blue2'
color.u.alpha <- rgb(1, .498, 0, alpha=.7)   # darkorange1 + alpha
color.i.alpha <- rgb(0, 0, .933, alpha=.7)   # blue2 + alpha
color.oe_marker <- rgb(0, 0, 0, alpha=.35)

# Time in a AC time series should be evenly spaced. If there is a jump, it
# means one or more samples lost.
# @return a list of time before which there are one or more lost samples
#
detect_lost <- function(time) {
    dt <- c(1/SR, diff(time))
    u <- time[which(abs(dt - (1/SR)) > EPSILON)]
    if (length(u) == 0) {
        return(NULL)
    } else {
        return(u)
    }
}

# For a given series s and a set of splits (j, k, n), it returns (1, ..., j_p),
# (j, ..., k_p), (k, ..., n_p), (n, ...), where [jkn]_p means the previous data
# point before [jkn]
#
split_series <- function(s, splits) {
    idx <- which(s == splits)
    li <- list()
    j <- 1
    seq <- 1
    for (i in idx) {
        if (i > j) {
            li[[seq]] <- s[j:(i - 1)]
            seq <- seq + 1
        }
        j <- i
    }
    li[[seq]] <- s[j:length(s)]
    return(li)
}

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
        if ((len > MAX_PLOT_WIDTH) && i %% floor(len/MAX_PLOT_WIDTH) == 0 
            || i == 1
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
# @param time time of the new point
# @param value value of the new point
# @param dir 1: positive crossing, -1: negative crossing, 0: both
# @param threshold only when the absolute of new value greater than the
#   threshold, the cross condition is hold
# @return updated state variable
#
cross.put_next <- function(state, time, value, dir=1, threshold=0) {
    s <- state
    if (is.null(s)) {
        return(list(last.val=value,
                    time.before_cross=time,
                    time.after_cross=NULL,
                    cross_time=NULL))
    } else if (s$last.val * value > 0 |
               dir == 1 & value < 0 |
               dir == -1 & value > 0) {
        # not crossing or not interesting crossing
        #
        s$last.val <- value
        s$time.before_cross <- time
        return(s)
    } else if (abs(value) <= threshold) {
        # crossed but the value not reached the threshold
        #
        s$time.after_cross <- time
        return(s)
    } else {
        # interested crossing confirmed
        #
        if (is.null(s$time.after_cross)) s$time.after_cross <- time
        s$cross_time <- .5 * (s$time.before_cross + s$time.after_cross)
        s$time.before_cross <- time
        s$time.after_cross <- NULL
        s$last.val <- value
        return(s)
    }
}

# @k_start start value of the times of interquartile range
# @stop stop searching better k when the result size close to 'stop' times the
#   original data size
# @kp every next iteration, the k will increase kp * error
#
outlier <- function(time, value, stop=1e-6, k_start=2, kp=0.75, skip_time) {
    der <- c(0, diff(value))

    k <- k_start
    k.prev <- k
    sp <- max(stop * length(time), 5)
    err <- Inf
    ix.hist <- c()
    cnt <- 0

    while (T) {
        ix <- which(abs(der) > k * IQR(der))

        # stop if error is enough small
        #
        err <- (length(ix) - sp)/sp
        if (abs(err) < .01) {
            print(paste('outlier searching stoped for sp reached. error:', err))
            break
        }

        # stop when numbers of outliers converge to a value for a while
        #
        ix.hist <- append(ix.hist, length(ix))
        if (length(ix.hist) > 20) {
            ix.hist <- ix.hist[-1]
            if (abs(sum(diff(ix.hist))) < 1 |
                abs(sum(diff(ix.hist[2:length(ix.hist)]))) < 1) {
                print('outlier searching stopped for result converged')
                print(ix.hist)
                break
            }
        }

        # increase k in proportional to the error
        #
        k.prev <- k
        k <- k + max(min(err * kp, 2), -2)

        cnt <- cnt + 1
        if (cnt %% 20 == 0) {
            print(paste('outlier searching: cnt',
                        cnt, 'sp', sp, 'len(ix)', length(ix),
                        'error', err, 'k', k.prev))
            print(ix.hist)
        }
    }
    print(paste('outliers:', paste(c(sp, length(ix), k, err, cnt), collapse=',')))

    ol.time <- rep(NA, length(ix))
    ol.value <- rep(NA, length(ix))

    ol.time[ix] <- time[ix] 
    ol.value[ix] <- value[ix] 

    if (! is.null(skip_time)) {
        excl_idx <- which(ol.time %in% skip_time)
        ol.time[excl_idx] <- NA
        ol.value[excl_idx] <- NA
        print(paste('skipped', length(excl_idx), ' sample losting'))
    }

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
    intvl <- c(t, t + ncycles * SAMPLES_PER_PERIOD * 1/SR)
    intvl <- intvl - align * ncycles * SAMPLES_PER_PERIOD * 1/SR
    subset(d, Time >= intvl[1] & Time <= intvl[2])
}

# Grouping a vector of time by put neiboring times into
# a same group
# @param time
# @param window window length
# @return a list of (start, end) vectors
#
group_time <- function(time, window) {
    grp <- list()
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
        d$Time <- d$Seqno * 1/SR
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
plot.ui_inst <- function(data, range=c(-Inf, Inf), phase, type='p',
                         marker.u=c(), marker.i=c()) {
    par(bg='cornsilk', mfrow=c(2,length(phase)))

    data <- subset(data, Time >= range[1])
    data <- subset(data, Time <= range[2])

    q <- c('U', 'I')
    scales <- c(USCALE, ISCALE)
    if (nrow(data) > 2 * SR) {
        colors <- c(color.u.alpha, color.i.alpha)
    } else {
        colors <- c(color.u, color.i)
    }
    marker <- list(marker.u, marker.i)

    sapply(1:length(q), function(m) {
        sapply(phase, function(n) {
                x_range <- c(min(data$Time), max(data$Time))
                y_range <- c(min(data[, paste(q[m], n, sep='')] * scales[m]),
                             max(data[, paste(q[m], n, sep='')] * scales[m]))
                if (! is.infinite(range[1]))
                    x_range[1] <- range[1]
                if (! is.infinite(range[2]))
                    x_range[2] <- range[2]
                plot(data$Time,
                     data[, paste(q[m], n, sep='')] * scales[m],
                     main=paste(q[m], ' inst - L', n, sep=''),
                     xlab='Time (s)',
                     ylab='',
                     xlim=x_range,
                     ylim=c(y_range[1] - abs(y_range[1]) * .05,
                            y_range[2] + abs(y_range[2]) * .05),
                     col=colors[m], type=type,
                     panel.first=c(abline(h=c(0, y_range[1], y_range[2]),
                                          lty=3)))
                if (! is.null(marker[[m]]))
                    axis(1, at=marker[[m]], label=F, tck=-.04, col.ticks='deeppink')
        })
    })
}

# Plot U/I histogram
#
plot.ui_hist <- function(data, range=c(-Inf, Inf), phase) { 
    par(bg='cornsilk', mfrow=c(2, length(phase)))

    data <- subset(data, Time >= range[1])
    data <- subset(data, Time <= range[2])
    if (nrow(data) == 0) stop('empty data')

    sapply(phase, function(n) {
               hist(data[, paste('U', n, sep='')] * USCALE,
                    xlab='Voltage (V)', main=paste('U hist - L', n, sep=''))
               }
    )
    sapply(phase, function(n) {
               hist(data[, paste('I', n, sep='')] * ISCALE,
                    xlab='Current (A)', main=paste('I hist - L', n, sep=''))
               }
    )
}

# @time time, u and i form a time series of AC signals
# @u
# @i
# @freq.f0 fundamental frequency
# @downsample_threshold only output points when it deviates from the previous one for
#   the givin threshold value.
# @return a list of (time, phi). The elements of vector PhaseShift are in
#   range of [-pi, pi), which defined as the radian difference between U and I
#   using phi_u - phi_i.
#
phase_shift <- function(time, u, i, freq.f0=50,
                        u.threshold=0, i.threshold=0,
                        downsample_threshold=0) {
    period <- 1/freq.f0
    ts <- c()
    phi <- c()    # between (-pi, pi]

    state.u <- NULL
    state.i <- NULL

    for (idx in 1:length(time)) {
        state.u <- cross.put_next(state.u, time[idx], u[idx], dir=1, threshold=u.threshold)
        state.i <- cross.put_next(state.i, time[idx], i[idx], dir=1, threshold=i.threshold)

        # Only go proceed when i crossed and u had a cross before.
        #
        if (is.null(state.i$cross_time) || is.null(state.u$cross_time) ||
            state.i$cross_time < state.u$cross_time)
            next

        # now i and u both have crossed, we're able to do phase comparing
        # phi_u - phi_i is time of i cross minus time of u cross
        #
        p <- ((state.i$cross_time - state.u$cross_time)/period) * 2*pi
        t <- state.u$cross_time

        #print(paste('idx', idx, 'phase determined:',
        #            state.i$cross_time,
        #            state.u$cross_time,
        #            p))

        state.u$cross_time <- NULL
        state.i$cross_time <- NULL

        # phi should witin a period, we may lost some
        # samples
        if (p >= 2*pi)
            next

        if (p > pi) p <- p - 2*pi

        ts <- append(ts, t)
        phi <- append(phi, p)
    }

    # smooth the phi
    #
    w <- 50 
    if (length(phi)/10 < w) w <- length(phi)/10
    phi <- pass.filt(phi, W=w, method='Butterworth')

    li <- list(time=ts, phi=phi)
    if (downsample_threshold == 0 || is.null(ts))
        return(li)

    # downsample the phi
    #

    t <- c()
    p <- c() 
    last.phi <- 0
    len <- length(li$phi)

    for (idx in 1:len) {
        phi <- li$phi[idx]

        # calculate the difference within the boundary of (-pi, pi]
        #
        diff <- (phi - last.phi) %% (2*pi)
        if (diff >= pi) diff <- -(2*pi - diff)

        if ((len >= MAX_PLOT_WIDTH && idx %% floor(len/MAX_PLOT_WIDTH)) == 0
            || idx == 1 
            || idx == len
            || abs(diff) > downsample_threshold) {
            last.phi <- phi
            t <- append(t, li$time[idx])
            p <- append(p, phi)
        }
    }
    list(time=t, phi=p)
}

# Detect current in/out changes
# @param time time
# @param curr current
#
#detect_curr_in_out <- function(time, curr) {
#    r <- rms.map(time=time, value=curr)
#    #r$rms <- pass.filt(r$rms, W=SAMPLES_PER_PERIOD, method='Butterworth')
#    der <- c(0, diff(r$rms))
#    cross <- c()
#    state <- NULL
#    for (idx in 1:length(der)) {
#        t <- r$time[idx]
#        state <- cross.put_next(state, t, der[idx], dir=0, threshold=.01)
#        if (! is.null(state$cross_time) && state$cross_time == t)
#            cross <- append(cross, t)
#    }
#    return(cross)
#}

# Plot U/I RMS as well as phase angle trajetories with reduced samples, limited
# by a change rage threshold
# @threshold sample reducing threshold of U, I and Phase shift 
#
plot.rms_and_phase <- function(data, range=c(-Inf, Inf), phase,
                               threshold=c(0, 0, 0),
                               marker.oe=c(),
                               marker.lost=c(),
                               type='l') {
    par(bg='cornsilk', mfrow=c(3, length(phase)))

    data <- subset(data, Time >= range[1])
    data <- subset(data, Time <= range[2])
    if (nrow(data) < SAMPLES_PER_PERIOD) stop('data size is not enough')

    q <- c('U', 'I')
    qq <- c('u', 'i')
    scales <- c(USCALE, ISCALE)

    li.rms <- lapply(1:length(q), function(m) {
        li <- lapply(phase, function(n) {
               colname <- paste(q[m], n, sep='')
               r <- rms.map(time=data$Time,
                            value=data[, colname] * scales[m])
               li <- rms.reduce(r$time, r$rms, threshold=threshold[1])
        })
        names(li) <- paste('l', phase, sep='')
        li
    })
    names(li.rms) <- qq

    li.mm <- lapply(1:length(qq), function(m) {
        li <- lapply(phase, function(n) {
                         val <- li.rms[[m]][[paste('l', n, sep='')]]$rms
                         c(min(val), max(val))
        })
        names(li) <- paste('l', phase, sep='')
        li
    })
    names(li.mm) <- qq

    minmax <- list(u=c(min(do.call(cbind, li.mm$u)[1,]), max(do.call(cbind, li.mm$u)[2,])),
         i=c(min(do.call(cbind, li.mm$i)[1,]), max(do.call(cbind, li.mm$i)[2,])))

    colors <- c('orange', 'blue')
    ticks <- list(u=seq(0, 330,50), i=seq(0, 150, 20))
    sapply(1:length(qq), function(m) {
        sapply(phase, function(n) {
                v <- li.rms[[m]][[paste('l', n, sep='')]]
                plot(v$time, v$rms,
                     main=paste(q[m], ' RMS - L', n, sep=''),
                     xlab='Time (s)',
                     ylab='',
                     ylim=c(0, minmax[[m]][2] * 1.05),
                     yaxt='none',
                     col=colors[m], type=type,
                     panel.first=c(abline(v=marker.lost, lty=3)))
                axis(2, las=2)
                if (! is.null(marker.oe))
                    axis(1, at=marker.oe, label=F, tck=-.04, col.ticks='deeppink')
        })
    })

    sapply(phase, function(n) {
            u <- data[, paste('U', n, 'Scaled', sep='')]
            i <- data[, paste('I', n, 'Scaled', sep='')]
            li <- phase_shift(time=data$Time,
                              u=u, i=i,
                              u.threshold=3, i.threshold=.01,
                              downsample_threshold=threshold[3])
            if (is.null(li$time)) return(NULL)

            pos1 <- seq(-pi, pi, by=pi/6)
            pos <- seq(-pi, pi, by=pi/2)
            tickmark <- expression(-~~pi, '', 0, '', pi)
            plot(li$time, li$phi,
                 main=paste('Phase - L', n, sep=''),
                 xlab='Time (s)',
                 ylab='',
                 ylim=c(-pi * 1.05, pi * 1.05),
                 yaxt='none',
                 col='seagreen', type=type,
                 panel.first=c(abline(v=marker.lost, lty=3)))
            axis(2, at=pos1, label=F, tck=-.03)
            axis(2, at=pos, label=tickmark, las=2)
            if (! is.null(marker.oe))
                axis(1, at=marker.oe, label=F, tck=-.04, col.ticks='deeppink')
            abline(h=pos, lwd=0.2)
    })
}

# This is a wrapper function, using a data frame to call outlier and extremer,
# then return ol and oe list.
# @return a 4-layer list of <ol|ex>/<U|I>/L<n>/<time|value>
#
ui_oe.calc <- function (data, range=c(-Inf, Inf), phase, lost_time) {
    data <- subset(data, Time >= range[1])
    data <- subset(data, Time <= range[2])

    type <- c('U', 'I')
    phase_name <- paste('L', phase, sep='')
    ex_threshold <- c(VOLTAGE * sqrt(2) * 1.05, IMAX * sqrt(2) * 1.05)

    ol <- lapply(1:length(type), function(m) {
        li <- lapply(phase, function(n) {
            value <- data[, paste(type[m], n, 'Scaled', sep='')]

            print(paste('calculate outlier on phase ',
                        n, ' for ', type[m], sep=''))
            li <- outlier(data$Time, value,
                          skip_time=lost_time)
            li$time <- li$time[! is.na(li$time)]
            li$value <- li$value[! is.na(li$value)]

            # when in null values, keep the list in the same form
            if (length(li$time) == 0)
                return(list(time=c(), value=c()))
            else
                return(li)
        })
        names(li) <- phase_name
        li
    })
    names(ol) <- type

    ex <- lapply(1:length(type), function(m) {
        li <- lapply(phase, function(n) {
            value <- data[, paste(type[m], n, 'Scaled', sep='')]

            print(paste('calculate extremers on phase ',
                        n, ' for ', type[m], sep=''))
            li <- extremer(data$Time, value, threshold=ex_threshold[m])
            li$time <- li$time[! is.na(li$time)]
            if (length(li$time) == 0) li$time <- c()
            li$value <- li$value[! is.na(li$value)]
            if (length(li$value) == 0) li$value <- c()
            return(li)
        })
        names(li) <- phase_name
        return(li)
    })
    names(ex) <- type
    list(ol=ol, ex=ex)
}

# Plot U/I outliers and extremers
# @ol outliers, a 3-layer list of <U|I>/L<n>/<time|value>
# @ex extremers, a 3-layer list of <U|I>/L<n>/<time|value>
#
plot.ui_oe <- function(ol, ex, phase, time_scale=NULL) {
    # the min/max U/I need firstly looked up from the data
    # then adjusted with some reasonable default scale.
    #
    oe <- list(ol=ol, ex=ex)
    mm <- lapply(names(oe), function(ol_or_ex) {
        li <- oe[[ol_or_ex]]
        mm <- lapply(names(li), function(u_or_i) {
            li <- li[[u_or_i]]
            ph <- names(li)[phase]
            mm <- do.call(cbind, lapply(ph,
                        function(p) {
                            c(min(li[[p]]$value), max(li[[p]]$value))
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

    par(bg='cornsilk', mfrow=c(2,length(phase)))

    y_scale <- list(U=c(umin, umax), I=c(imin, imax))
    q <- c('U', 'I')
    colors <- c('orange', 'blue')

    sapply(c('L1', 'L2', 'L3')[phase], function(n) {
        # m is index of c('U', 'I')
        sapply(1:length(q), function(m) {
            plot(ol[[q[m]]][[n]]$time, ol[[q[m]]][[n]]$value,
                 main=paste(q[m],
                            ' outliers - L',
                            paste(phase, collapse=','), sep=''),
                 xlab='Time (s)',
                 ylabl='',
                 xlim=time_scale,
                 ylim=y_scale[[q[m]]],
                 col=colors[m], q='p')
            points(ex[[q[m]]][[n]]$time, ex[[q[m]]][[n]]$value, pch=8, col='red')
            abline(h=0, lty=3)
        })
    })
    NULL
}

save_plot <- function(plot, name, dir='.',
                      width=480, height=640, png=F, svg=T) {
    if (svg) {
        svglite(paste(dir, '/', name, '.svg', sep=''),
            width=width/SYS_DPI, height=height/SYS_DPI)
        plot()
        dev.off()
    }
    if (png) {
        png(paste(dir, '/', name, '.png', sep=''),
            width=width, height=height, unit='px')
        plot()
        dev.off()
    }
}

# Create time series of u and i, which have a phase shift.
# @param phi the phase difference betwee voltage and current, defined as phase of
#   voltage minus phase of current. we suppose voltage phase is zero.
# @sr sampling rate
# @return a list of ts, u, i 
#
create_ac <- function(phi=0, sr=6.4e3, duration=.04, f.0=50, v.rms=230, i.rms=50, plot=F) {
    w <- 2*pi*f.0

    ts <- seq(0, duration, 1/sr)
    u <- v.rms*sqrt(2)*cos(w*ts)
    i <- i.rms*sqrt(2)*cos(w*ts - phi)

    if (plot) {
        plot(ts, u, type='l', col='darkorange1', xlab='time', ylab='u/i')
        lines(ts, i, col='blue2')
        abline(h=0, lty=3)

        abline(v=0, lty=3)
        abline(v=phi/w, lty=3)
    }

    return(list(ts=ts, u=u, i=i))
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
