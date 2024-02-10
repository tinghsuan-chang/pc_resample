library(tidyverse)
library(ggplot2)
library(patchwork)
library(reshape2)
library(ggpattern)

part <- readRDS(file = "df/part.RDS")

# avg.nb = 7, M = 50
# coverage vs c* 
p1_1 <- part %>%
  filter(n == 500, M == 50, c %in% c(c(1,2,4,6,8)/100, (1:5)/10), is.na(seed), G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = coverage, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.01, 0.1, 0.3, 0.5)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.5, 1)) +
  xlab("c*") +
  theme_bw()

# avg.nb = 7, M = 50
# CI length vs c* 
p1_2 <- part %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 50, c %in% c(c(1,2,4,6,8)/100, (1:5)/10), is.na(seed), G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = CI_width, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.01, 0.1, 0.3, 0.5)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.15, 0.55)) +
  ylab("CI length") +
  xlab("c*") +
  theme_bw() 

# avg.nb = 7, M = 50
# keep/M vs c* 
p1_3 <- part %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 50, c %in% c(c(1,2,4,6,8)/100, (1:5)/10), is.na(seed), G == "n.nodes = 10, avg.nb = 7") %>%
  ggplot(aes(x = c, y = keep_M*100)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0.01, 0.1, 0.3, 0.5)) +
  ylim(c(0, 30)) +
  ylab("% kept graphs") +
  xlab("c*") +
  theme_bw() 

# avg.nb = 7, M = 100
# coverage vs c* 
p2_1 <- part %>%
  filter(n == 500, M == 100, c %in% c(c(1,2,4,6,8)/100, (1:5)/10), is.na(seed), G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = coverage, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.01, 0.1, 0.3, 0.5)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.5, 1)) +
  xlab("c*") +
  theme_bw()

# avg.nb = 7, M = 100
# CI length vs c* 
p2_2 <- part %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 100, c %in% c(c(1,2,4,6,8)/100, (1:5)/10), is.na(seed), G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = CI_width, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.01, 0.1, 0.3, 0.5)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.15, 0.55)) +
  ylab("CI length") +
  xlab("c*") +
  theme_bw() 

# avg.nb = 7, M = 100
# keep/M vs c* 
p2_3 <- part %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 100, c %in% c(c(1,2,4,6,8)/100, (1:5)/10), is.na(seed), G == "n.nodes = 10, avg.nb = 7") %>%
  ggplot(aes(x = c, y = keep_M*100)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0.01, 0.1, 0.3, 0.5)) +
  ylim(c(0, 30)) +
  ylab("% kept graphs") +
  xlab("c*") +
  theme_bw() 

# avg.nb = 4, M = 50
# coverage vs c* 
p3_1 <- part %>%
  filter(n == 500, M == 50, c %in% c(c(1,2,4,6,8)/100, (1:5)/10), is.na(seed), G == "n.nodes = 10, avg.nb = 4") %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = coverage, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.01, 0.1, 0.3, 0.5)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.5, 1)) +
  xlab("c*") +
  theme_bw()

# avg.nb = 4, M = 50
# CI length vs c* 
p3_2 <- part %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 50, c %in% c(c(1,2,4,6,8)/100, (1:5)/10), is.na(seed), G == "n.nodes = 10, avg.nb = 4") %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = CI_width, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.01, 0.1, 0.3, 0.5)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.15, 0.55)) +
  ylab("CI length") +
  xlab("c*") +
  theme_bw() 

# avg.nb = 4, M = 50
# keep/M vs c* 
p3_3 <- part %>%
  filter(nb_max == 7, nu == 0.025, n == 500, M == 50, c %in% c(c(1,2,4,6,8)/100, (1:5)/10), is.na(seed), G == "n.nodes = 10, avg.nb = 4") %>%
  ggplot(aes(x = c, y = keep_M*100)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0.01, 0.1, 0.3, 0.5)) +
  ylim(c(0, 30)) +
  ylab("% kept graphs") +
  xlab("c*") +
  theme_bw() 

# avg.nb = 7
# coverage vs M 
p4_1 <- part %>%
  filter(n == 500, c == 0.02, is.na(seed), G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = M, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(25, 50, 100)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.5, 1)) +
  theme_bw()

# avg.nb = 7
# coverage vs n
p4_2 <- part %>%
  filter(M == 50, c == 0.02, is.na(seed), G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover), factor_key = TRUE) %>%
  ggplot(aes(x = n, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(250, 500, 1000)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.5, 1)) +
  theme_bw()

# avg.nb = 7
# CI length vs M
p4_3 <- part %>%
  filter(n == 500, c == 0.02, is.na(seed), G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = M, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(25, 50, 100)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.1, 0.5)) +
  ylab("CI length") +
  theme_bw()

# avg.nb = 7
# CI length vs n
p4_4 <- part %>%
  filter(M == 50, c == 0.02, is.na(seed), G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, CI_width, c(width, tru.width, naive.width), factor_key = TRUE) %>%
  ggplot(aes(x = n, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(250, 500, 1000)) +
  scale_color_manual(labels = c("resample", "oracle", "naive"), values = c("red", "gray", "blue")) +
  ylim(c(0.1, 0.5)) +
  ylab("CI length") +
  theme_bw()
