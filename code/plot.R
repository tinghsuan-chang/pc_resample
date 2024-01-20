library(tidyverse)
library(ggplot2)
library(patchwork)
library(reshape2)
library(ggpattern)

# complete ordering ------------------------------------------------------
comp <- readRDS(file = "df/comp.RDS")

# coverage vs M
p1 <- comp %>%
  filter(nb_max == 7, nu == 0.025, n == 500, c == 0.1) %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = M, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(25, 50, 100)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.55, 1)) +
  theme_bw()

# coverage vs n
p2 <- comp %>%
  filter(nb_max == 7, nu == 0.025, M == 50, c == 0.1) %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = n, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(250, 500, 1000)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.55, 1)) +
  theme_bw()

# CI length vs M
p3 <- comp %>%
  filter(nb_max == 7, nu == 0.025, n == 500, c == 0.1) %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = M, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(25, 50, 100)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.1, 0.45)) +
  ylab("CI length") +
  theme_bw()

# CI length vs n
p4 <- comp %>%
  filter(nb_max == 7, nu == 0.025, M == 50, c == 0.1) %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = n, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(250, 500, 1000)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.1, 0.45)) +
  ylab("CI length") +
  theme_bw()

# coverage vs c*
p5 <- comp %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 50, c %in% c((1:10)/10)) %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.55, 1)) +
  xlab("c*") +
  theme_bw()

# CI length vs c*
p6 <- comp %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 50, c %in% c((1:10)/10)) %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.1, 0.45)) +
  ylab("CI width") +
  xlab("c*") +
  theme_bw() 

# partial ordering ------------------------------------------------------
part <- readRDS(file = "df/part.RDS")

# coverage vs M 
p7 <- part %>%
  filter(nb_max == 7, nu == 0.025, n == 500, c == 0.1) %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = M, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(25, 50, 100)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.8, 1)) +
  theme_bw()

# coverage vs n
p8 <- part %>%
  filter(nb_max == 7, nu == 0.025, M == 50, c == 0.1) %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = n, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(250, 500, 1000)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.8, 1)) +
  theme_bw()

# CI length vs M
p9 <- part %>%
  filter(nb_max == 7, nu == 0.025, n == 500, c == 0.1) %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = M, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(25, 50, 100)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.1, 0.4)) +
  ylab("CI length") +
  theme_bw()

# CI length vs n
p10 <- part %>%
  filter(nb_max == 7, nu == 0.025, M == 50, c == 0.1) %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = n, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(250, 500, 1000)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.1, 0.4)) +
  ylab("CI length") +
  theme_bw()

# coverage vs c*
p11 <- part %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 50, c %in% c((1:10)/10)) %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.8, 1)) +
  xlab("c*") +
  theme_bw()

# CI length vs c*
p12 <- part %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 50, c %in% c((1:10)/10)) %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.15, 0.55)) +
  ylab("CI length") +
  xlab("c*") +
  theme_bw() 

# keep/M vs c*
p13 <- part %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 50, c %in% c((1:10)/10)) %>%
  ggplot(aes(x = c, y = keep_M*100)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0)) +
  ylab("% kept graphs") +
  xlab("c*") +
  theme_bw() 


