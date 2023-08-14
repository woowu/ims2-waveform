#!/usr/bin/Rscript
library(dplyr)

data <- read.csv('data/ims-waveform.csv')
data <- data %>% filter(name == 'u1' | name == 'i1')
p <- ggplot(data, aes(x=time, y=value, color=name)) +
    geom_line() +
    scale_color_manual(labels = c('i1', 'u1'), values = c('red', 'blue')) +
    labs(x = 'Time (ms)', y = 'Amplitude', color = '')
ggsave('waveform.pdf', p)
ggsave('waveform.png', p)
