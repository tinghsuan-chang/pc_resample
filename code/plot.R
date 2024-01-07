library(tidyverse)
library(ggplot2)
library(patchwork)
library(reshape2)
library(ggpattern)

# complete ordering ------------------------------------------------------
comp <- readRDS(file = "df/comp.RDS")

# coverage vs M
p1 <- comp %>%
  filter(nb_max == 7, nu == 0.025, n == 500, c == 0.2) %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = M, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(25, 50, 100)) +
  scale_color_manual(labels = c("resample", "true", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.55, 1)) +
  theme_bw()

# coverage vs n
p2 <- comp %>%
  filter(nb_max == 7, nu == 0.025, M == 50, c == 0.2) %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = n, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(250, 500, 1000)) +
  scale_color_manual(labels = c("resample", "true", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.55, 1)) +
  theme_bw()

# CI length vs M
p3 <- comp %>%
  filter(nb_max == 7, nu == 0.025, n == 500, c == 0.2) %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = M, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(25, 50, 100)) +
  scale_color_manual(labels = c("resample", "true", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.1, 0.45)) +
  ylab("CI length") +
  theme_bw()

# CI length vs n
p4 <- comp %>%
  filter(nb_max == 7, nu == 0.025, M == 50, c == 0.2) %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = n, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(250, 500, 1000)) +
  scale_color_manual(labels = c("resample", "true", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.1, 0.45)) +
  ylab("CI length") +
  theme_bw()

# coverage vs c*
p5 <- comp %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 50, c %in% c((1:20)/10)) %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0.5, 1.0, 1.5, 2.0)) +
  scale_color_manual(labels = c("resample", "true", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.55, 1)) +
  xlab("c*") +
  theme_bw()

# CI length vs c*
p6 <- comp %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 50, c %in% c((1:20)/10)) %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0.5, 1.0, 1.5, 2.0)) +
  scale_color_manual(labels = c("resample", "true", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.1, 0.45)) +
  ylab("CI width") +
  xlab("c*") +
  theme_bw() 

# pct kept graphs vs c*
p7 <- comp %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 50, c %in% c((1:20)/10)) %>%
  ggplot(aes(x = c, y = kept*100)) +
  geom_line() +
  geom_point() +
  ylab("% kept graphs") +
  xlab("c*") +
  theme_bw() 

# partial ordering ------------------------------------------------------
part <- readRDS(file = "df/part.RDS")

# coverage vs M 
p8 <- part %>%
  filter(nb_max == 7, nu == 0.025, n == 500, c == 0.1) %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = M, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(25, 50, 100)) +
  scale_color_manual(labels = c("resample", "true", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.8, 1)) +
  theme_bw()

# coverage vs n
p9 <- part %>%
  filter(nb_max == 7, nu == 0.025, M == 50, c == 0.1) %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = n, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(250, 500, 1000)) +
  scale_color_manual(labels = c("resample", "true", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.8, 1)) +
  theme_bw()

# CI length vs M
p10 <- part %>%
  filter(nb_max == 7, nu == 0.025, n == 500, c == 0.1) %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = M, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(25, 50, 100)) +
  scale_color_manual(labels = c("resample", "true", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.1, 0.4)) +
  ylab("CI length") +
  theme_bw()

# CI length vs n
p11 <- part %>%
  filter(nb_max == 7, nu == 0.025, M == 50, c == 0.1) %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = n, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(250, 500, 1000)) +
  scale_color_manual(labels = c("resample", "true", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.1, 0.4)) +
  ylab("CI length") +
  theme_bw()

# coverage vs c*
p12 <- part %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 50, c %in% c(seq(from = 0.5, to = 7, by = 0.5)/10)) %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(from = 0.1, to = 0.7, by = 0.1)) +
  scale_color_manual(labels = c("resample", "true", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.8, 1)) +
  xlab("c*") +
  theme_bw()

# CI length vs c*
p13 <- part %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 50, c %in% c(seq(from = 0.5, to = 7, by = 0.5)/10)) %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(from = 0.1, to = 0.7, by = 0.1)) +
  scale_color_manual(labels = c("resample", "true", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.15, 0.3)) +
  ylab("CI length") +
  xlab("c*") +
  theme_bw() 

# valid/M and keep/M vs c*
p14 <- part %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 50, c %in% c(seq(from = 0.5, to = 7, by = 0.5)/10)) %>%
  dplyr::select(c, valid_M, keep_M) %>%
  gather(key = "var", value = "value", -c) %>%
  ggplot(aes(x = c, y = value*100)) +
  geom_line(aes(color = var)) +
  geom_point(aes(color = var)) +
  ylab("% graphs") +
  xlab("c*") +
  scale_color_manual("", labels = c("kept", "valid"), values = c("darkred", "steelblue")) +
  theme_bw() 

# other plots ------------------------------------------------------

# set.seed(123123) --> complete order, coverage vs c*
comp2 <- readRDS(file = "df/comp2.RDS")
p15 <- comp2 %>%
  filter(n == 500, M == 50, c %in% c((1:20)/10)) %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0.5, 1.0, 1.5, 2.0)) +
  scale_color_manual(labels = c("resample", "true", "naive"), values = c("red", "gray", "blue")) +
  xlab("c*") +
  theme_bw()

# set.seed(123123) --> complete order, CI length vs c*
p16 <- comp2 %>%
  filter(n == 500, M == 50, c %in% c((1:20)/10)) %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0.5, 1.0, 1.5, 2.0)) +
  scale_color_manual(labels = c("resample", "true", "naive"), values = c("red", "gray", "blue")) +
  ylab("CI width") +
  xlab(expression(c['*'])) +
  theme_bw() 

# set.seed(123123) --> complete order, pct kept graphs vs c*
p17 <- comp2 %>%
  filter(n == 500, M == 50, c %in% c((1:20)/10)) %>%
  ggplot(aes(x = c, y = kept*100)) +
  geom_line() +
  geom_point() +
  ylab("% kept graphs") +
  xlab(expression(c['*'])) +
  theme_bw() 

# complete order, plot CI (n=500, M=50, c=0.5)
load(file = "sim_out/comp_n500_M50_c05.RDS")
CI_l <- sapply(sim[1:250], f <- function(l) {getElement(l, "CI")[1]})
CI_u <- sapply(sim[1:250], f <- function(l) {getElement(l, "CI")[2]})
naive.CI_l <- sapply(sim[1:250], f <- function(l) {getElement(l, "naive.CI")[1]})
naive.CI_u <- sapply(sim[1:250], f <- function(l) {getElement(l, "naive.CI")[2]})
df <- tibble(ii = 1:length(CI_l), CI_l = CI_l, CI_u = CI_u, naive.CI_l = naive.CI_l, naive.CI_u = naive.CI_u)
p18 <- df %>%
  ggplot() +
  geom_errorbar(aes(x = ii, ymin = CI_l, ymax = CI_u), colour = "orange") +
  #geom_errorbar(aes(x = ii, ymin = naive.CI_l, ymax = naive.CI_u), colour = "blue", alpha = 0.4) +
  geom_hline(yintercept = -0.3222485, linetype = "dashed", color = "red") +
  ylim(c(-0.8, 0)) +
  labs(title = "c* = 0.5", x = "Iteration",
       caption = "(cover = 87%, avg. width = 0.278)") +
  theme_bw() 

# complete order, plot CI (n=500, M=50, c=2)
load(file = "sim_out/comp_n500_M50_c2.RDS")
CI_l <- sapply(sim[1:250], f <- function(l) {getElement(l, "CI")[1]})
CI_u <- sapply(sim[1:250], f <- function(l) {getElement(l, "CI")[2]})
naive.CI_l <- sapply(sim[1:250], f <- function(l) {getElement(l, "naive.CI")[1]})
naive.CI_u <- sapply(sim[1:250], f <- function(l) {getElement(l, "naive.CI")[2]})
df <- tibble(ii = 1:length(CI_l), CI_l = CI_l, CI_u = CI_u, naive.CI_l = naive.CI_l, naive.CI_u = naive.CI_u)
p19 <- df %>%
  ggplot() +
  geom_errorbar(aes(x = ii, ymin = CI_l, ymax = CI_u), colour = "orange") +
  #geom_errorbar(aes(x = ii, ymin = naive.CI_l, ymax = naive.CI_u), colour = "blue", alpha = 0.4) +
  geom_hline(yintercept = -0.3222485, linetype = "dashed", color = "red") +
  ylim(c(-0.8, 0)) +
  labs(title = "c* = 2", x = "Iteration",
       caption = "(cover = 98%, avg. width = 0.275)") +
  theme_bw() 

# partial order, valid/keep when c* = 1/12
load(file = "sim_out/part_n500_M50_c008.RDS")
part_n500_M50_c12 <- sim
valid <- sapply(part_n500_M50_c12[1:100], f <- function(l) {getElement(l, "n_valid_m")})
keep <- sapply(part_n500_M50_c12[1:100], f <- function(l) {getElement(l, "n_keep_m")})
df <- tibble(ii = 1:length(valid), valid = valid, keep = keep)
p20 <- ggplot(melt(df, id = "ii"), aes(x = ii, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual("", values = c("valid" = "azure3", "keep" = "azure4")) +
  ylim(c(0, 25)) +
  labs(title = "c* = 1/12", x = "Iteration", y = "Number of graphs") +
  theme_bw()

# partial order, valid/keep when c* = 0.2
load(file = "sim_out/part_n500_M50_c02.RDS")
part_n500_M50_c5 <- sim
valid <- sapply(part_n500_M50_c5[1:100], f <- function(l) {getElement(l, "n_valid_m")})
keep <- sapply(part_n500_M50_c5[1:100], f <- function(l) {getElement(l, "n_keep_m")})
df <- tibble(ii = 1:length(valid), valid = valid, keep = keep)
p21 <- ggplot(melt(df, id = "ii"), aes(x = ii, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual("", values = c("valid" = "azure3", "keep" = "azure4")) +
  ylim(c(0, 25)) +
  labs(title = "c* = 1/5", x = "Iteration", y = "Number of graphs") +
  theme_bw()

