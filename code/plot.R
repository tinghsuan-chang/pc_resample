library(tidyverse)
library(ggplot2)
library(patchwork)
library(reshape2)
library(ggpattern)

df <- readRDS(file = "df/df.RDS")

# Figure 1(a): avg.nb = 7, M = 50, coverage vs c*
fig_1a <- df %>%
  filter(n == 500, M == 50, G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover.01, naive.cover.05), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = coverage, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.01, 0.1, 0.3, 0.5)) +
  scale_color_manual(labels = c("resample", "oracle", expression(paste("naive (", alpha, "=0.01)")), expression(paste("naive (", alpha, "=0.05)"))), 
                     values = c("coral2", "azure4", "cornflowerblue", "aquamarine4")) +
  ylim(c(0.5, 1)) +
  xlab("c*") +
  theme_bw() +
  ggtitle("(a)")

# Figure 1(b): avg.nb = 7, M = 50, CI length vs c* 
fig_1b <- df %>%
  filter(n == 500, M == 50, G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, CI_width, c(width, tru.width, naive.width.01, naive.width.05), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = CI_width, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.01, 0.1, 0.3, 0.5)) +
  scale_color_manual(labels = c("resample", "oracle", expression(paste("naive (", alpha, "=0.01)")), expression(paste("naive (", alpha, "=0.05)"))), 
                     values = c("coral2", "azure4", "cornflowerblue", "aquamarine4")) +
  ylim(c(0.15, 0.5)) +
  ylab("CI length") +
  xlab("c*") +
  theme_bw() +
  ggtitle("(b)")

# Fig 1(c): avg.nb = 7, M = 50, keep/M vs c* 
fig_1c <- df %>%
  filter(n == 500, M == 50, G == "n.nodes = 10, avg.nb = 7") %>%
  ggplot(aes(x = c, y = keep_M*100)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0.01, 0.1, 0.3, 0.5)) +
  ylim(c(0, 30)) +
  ylab("% kept graphs") +
  xlab("c*") +
  theme_bw() +
  ggtitle("(c)")

f1 <- ((fig_1a + theme(legend.position = "none")) + fig_1b + theme(legend.title = element_blank())) /
  fig_1c 

# Fig 2(a): avg.nb = 7, M = 100, coverage vs c* 
fig_2a <- df %>%
  filter(n == 500, M == 100, G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover.01, naive.cover.05), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = coverage, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.01, 0.1, 0.3, 0.5)) +
  scale_color_manual(labels = c("resample", "oracle", expression(paste("naive (", alpha, "=0.01)")), expression(paste("naive (", alpha, "=0.05)"))), 
                     values = c("coral2", "azure4", "cornflowerblue", "aquamarine4")) +
  ylim(c(0.5, 1)) +
  xlab("c*") +
  theme_bw() +
  ggtitle("(a)")

# Fig 2(b): avg.nb = 7, M = 100, CI length vs c* 
fig_2b <- df %>%
  filter(n == 500, M == 100, G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, CI_width, c(width, tru.width, naive.width.01, naive.width.05), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = CI_width, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.01, 0.1, 0.3, 0.5)) +
  scale_color_manual(labels = c("resample", "oracle", expression(paste("naive (", alpha, "=0.01)")), expression(paste("naive (", alpha, "=0.05)"))), 
                     values = c("coral2", "azure4", "cornflowerblue", "aquamarine4")) +
  ylim(c(0.15, 0.5)) +
  ylab("CI length") +
  xlab("c*") +
  theme_bw() +
  ggtitle("(b)")

# Fig 2(c): avg.nb = 7, M = 100, keep/M vs c* 
fig_2c <- df %>%
  filter(n == 500, M == 100, G == "n.nodes = 10, avg.nb = 7") %>%
  ggplot(aes(x = c, y = keep_M*100)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0.01, 0.1, 0.3, 0.5)) +
  ylim(c(0, 30)) +
  ylab("% kept graphs") +
  xlab("c*") +
  theme_bw() +
  ggtitle("(c)")

f2 <- ((fig_2a + theme(legend.position = "none")) + fig_2b + theme(legend.title = element_blank())) /
  fig_2c 

# Fig 3(a): avg.nb = 7, c* = 0.02, coverage vs M 
fig_3a <- df %>%
  filter(n == 500, c == 0.02, G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover.01, naive.cover.05), factor_key = TRUE) %>%
  ggplot(aes(x = M, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(25, 50, 100)) +
  scale_color_manual(labels = c("resample", "oracle", expression(paste("naive (", alpha, "=0.01)")), expression(paste("naive (", alpha, "=0.05)"))), 
                     values = c("coral2", "azure4", "cornflowerblue", "aquamarine4")) +
  ylim(c(0.45, 1)) +
  theme_bw() +
  ggtitle("(a)")

# Fig 3(b): avg.nb = 7, c* = 0.02, coverage vs n
fig_3b <- df %>%
  filter(M == 50, c == 0.02, G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover.01, naive.cover.05), factor_key = TRUE) %>%
  ggplot(aes(x = n, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(250, 500, 1000)) +
  scale_color_manual(labels = c("resample", "oracle", expression(paste("naive (", alpha, "=0.01)")), expression(paste("naive (", alpha, "=0.05)"))), 
                     values = c("coral2", "azure4", "cornflowerblue", "aquamarine4")) +
  ylim(c(0.45, 1)) +
  theme_bw() +
  ggtitle("(b)")

# Fig 3(c): avg.nb = 7, c* = 0.02, CI length vs M
fig_3c <- df %>%
  filter(n == 500, c == 0.02, G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, CI_width, c(width, tru.width, naive.width.01, naive.width.05), factor_key = TRUE) %>%
  ggplot(aes(x = M, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(25, 50, 100)) +
  scale_color_manual(labels = c("resample", "oracle", expression(paste("naive (", alpha, "=0.01)")), expression(paste("naive (", alpha, "=0.05)"))), 
                     values = c("coral2", "azure4", "cornflowerblue", "aquamarine4")) +
  ylim(c(0.1, 0.5)) +
  ylab("CI length") +
  theme_bw() +
  ggtitle("(c)")

# Fig 3(d): avg.nb = 7, c* = 0.02, CI length vs n
fig_3d <- df %>%
  filter(M == 50, c == 0.02, G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, CI_width, c(width, tru.width, naive.width.01, naive.width.05), factor_key = TRUE) %>%
  ggplot(aes(x = n, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(250, 500, 1000)) +
  scale_color_manual(labels = c("resample", "oracle", expression(paste("naive (", alpha, "=0.01)")), expression(paste("naive (", alpha, "=0.05)"))), 
                     values = c("coral2", "azure4", "cornflowerblue", "aquamarine4")) +
  ylim(c(0.1, 0.5)) +
  ylab("CI length") +
  theme_bw() +
  ggtitle("(d)")

f3 <- (fig_3a + theme(legend.position = "none")| fig_3b + theme(legend.position = "none")) /
  (fig_3c + theme(legend.position = "none")| fig_3d + theme(legend.title = element_blank()))

