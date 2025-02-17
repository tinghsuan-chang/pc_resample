library(tidyverse)
library(ggplot2)
library(patchwork)
library(reshape2)
library(ggpattern)

df <- readRDS(file = "data/df_final.RDS")

# Fig 1(a): avg.nb = 7, M = 50, coverage vs c*
fig_1a <- df %>%
  filter(n == 500, M == 50, G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover.01, naive.cover.05), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = coverage, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.006, 0.01, 0.02, 0.03, 0.04)) +
  scale_color_manual(labels = c("resample", "oracle", bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.01)"))))), bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.05)")))))), 
                     values = c("#D55E00", "#999999", "#0072B2", "#009E73")) +
  ylim(c(0.4, 1)) +
  xlab("c*") +
  theme_bw() +
  ggtitle("(a)")

# Fig 1(b): avg.nb = 7, M = 50, CI length vs c* 
fig_1b <- df %>%
  filter(n == 500, M == 50, G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, CI_width, c(width, tru.width, naive.width.01, naive.width.05), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = CI_width, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.006, 0.01, 0.02, 0.03, 0.04)) +
  scale_color_manual(labels = c("resample", "oracle", bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.01)"))))), bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.05)")))))), 
                     values = c("#D55E00", "#999999", "#0072B2", "#009E73")) +
  ylim(c(0.15, 0.60)) +
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
  scale_x_continuous(breaks = c(0.006, 0.008, 0.01, 0.02, 0.03, 0.04)) +
  ylim(c(0, 15)) +
  ylab("% kept graphs") +
  xlab("c*") +
  theme_bw() +
  ggtitle("(c)")

f1 <- ((fig_1a + theme(legend.position = "none")) + fig_1b + theme(legend.title = element_blank())) /
  fig_1c 
ggsave("figures/f1.png", f1, width = 9, height = 7, units = "in")

# Fig 2(a): avg.nb = 7, M = 100, coverage vs c* 
fig_2a <- df %>%
  filter(n == 500, M == 100, G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover.01, naive.cover.05), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = coverage, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.006, 0.01, 0.02, 0.03, 0.04)) +
  scale_color_manual(labels = c("resample", "oracle", bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.01)"))))), bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.05)")))))), 
                     values = c("#D55E00", "#999999", "#0072B2", "#009E73")) +
  ylim(c(0.4, 1)) +
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
  scale_x_continuous(breaks = c(0.006, 0.01, 0.02, 0.03, 0.04)) +
  scale_color_manual(labels = c("resample", "oracle", bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.01)"))))), bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.05)")))))), 
                     values = c("#D55E00", "#999999", "#0072B2", "#009E73")) +
  ylim(c(0.15, 0.60)) +
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
  scale_x_continuous(breaks = c(0.006, 0.008, 0.01, 0.02, 0.03, 0.04)) +
  ylim(c(0, 15)) +
  ylab("% kept graphs") +
  xlab("c*") +
  theme_bw() +
  ggtitle("(c)")

f2 <- ((fig_2a + theme(legend.position = "none")) + fig_2b + theme(legend.title = element_blank())) /
  fig_2c 
ggsave("figures/f2.png", f2, width = 9, height = 7, units = "in")

# Fig 3(a): avg.nb = 7, c* = 0.01, coverage vs M 
fig_3a <- df %>%
  filter(n == 500, c == 0.01, G == "n.nodes = 10, avg.nb = 7") %>%
  filter(M %in% c(50, 250, 500, 1000)) %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover.01, naive.cover.05), factor_key = TRUE) %>%
  ggplot(aes(x = M, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(50, 250, 500, 1000)) +
  scale_color_manual(labels = c("resample", "oracle", bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.01)"))))), bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.05)")))))), 
                     values = c("#D55E00", "#999999", "#0072B2", "#009E73")) +
  ylim(c(0.35, 1)) +
  theme_bw() +
  ggtitle("(a)")

# Fig 3(b): avg.nb = 7, c* = 0.01, coverage vs n
fig_3b <- df %>%
  filter(M == 50, c == 0.01, G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover.01, naive.cover.05), factor_key = TRUE) %>%
  ggplot(aes(x = n, y = coverage, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(250, 500, 1000)) +
  scale_color_manual(labels = c("resample", "oracle", bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.01)"))))), bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.05)")))))), 
                     values = c("#D55E00", "#999999", "#0072B2", "#009E73")) +
  ylim(c(0.35, 1)) +
  theme_bw() +
  ggtitle("(b)")

# Fig 3(c): avg.nb = 7, c* = 0.01, CI length vs M
fig_3c <- df %>%
  filter(n == 500, c == 0.01, G == "n.nodes = 10, avg.nb = 7") %>%
  filter(M %in% c(50, 250, 500, 1000)) %>%
  gather(method, CI_width, c(width, tru.width, naive.width.01, naive.width.05), factor_key = TRUE) %>%
  ggplot(aes(x = M, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(50, 250, 500, 1000)) +
  scale_color_manual(labels = c("resample", "oracle", bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.01)"))))), bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.05)")))))),
                     values = c("#D55E00", "#999999", "#0072B2", "#009E73")) +
  ylim(c(0.1, 0.65)) +
  ylab("CI length") +
  theme_bw() +
  ggtitle("(c)")

# Fig 3(d): avg.nb = 7, c* = 0.01, CI length vs n
fig_3d <- df %>%
  filter(M == 50, c == 0.01, G == "n.nodes = 10, avg.nb = 7") %>%
  gather(method, CI_width, c(width, tru.width, naive.width.01, naive.width.05), factor_key = TRUE) %>%
  ggplot(aes(x = n, y = CI_width, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(250, 500, 1000)) +
  scale_color_manual(labels = c("resample", "oracle", bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.01)"))))), bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.05)")))))),
                     values = c("#D55E00", "#999999", "#0072B2", "#009E73")) +
  ylim(c(0.1, 0.65)) +
  ylab("CI length") +
  theme_bw() +
  ggtitle("(d)")

f3 <- (fig_3a + theme(legend.position = "none")| fig_3b + theme(legend.position = "none")) /
  (fig_3c + theme(legend.position = "none")| fig_3d + theme(legend.title = element_blank()))
ggsave("figures/f3.png", f3, width = 9, height = 7, units = "in")

# Supp fig 1(a): avg.nb = 4, M = 50, coverage vs c*
supp_fig_1a <- df %>%
  filter(n == 500, M == 50, G == "n.nodes = 10, avg.nb = 4") %>%
  gather(method, coverage, c(cover, tru.cover, naive.cover.01, naive.cover.05), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = coverage, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.006, 0.01, 0.02, 0.03, 0.04)) +
  scale_color_manual(labels = c("resample", "oracle", bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.01)"))))), bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.05)")))))),
                     values = c("#D55E00", "#999999", "#0072B2", "#009E73")) +
  ylim(c(0.4, 1)) +
  xlab("c*") +
  theme_bw() +
  ggtitle("(a)")

# Supp fig 1(b): avg.nb = 4, M = 50, CI length vs c* 
supp_fig_1b <- df %>%
  filter(n == 500, M == 50, G == "n.nodes = 10, avg.nb = 4") %>%
  gather(method, CI_width, c(width, tru.width, naive.width.01, naive.width.05), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = CI_width, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.006, 0.01, 0.02, 0.03, 0.04)) +
  scale_color_manual(labels = c("resample", "oracle", bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.01)"))))), bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.05)")))))),
                     values = c("#D55E00", "#999999", "#0072B2", "#009E73")) +
  ylim(c(0.15, 0.6)) +
  ylab("CI length") +
  xlab("c*") +
  theme_bw() +
  ggtitle("(b)")

# Supp fig 1(c): avg.nb = 4, M = 50, keep/M vs c* 
supp_fig_1c <- df %>%
  filter(n == 500, M == 50, G == "n.nodes = 10, avg.nb = 4") %>%
  ggplot(aes(x = c, y = keep_M*100)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0.006, 0.008, 0.01, 0.02, 0.03, 0.04)) +
  ylim(c(0, 15)) +
  ylab("% kept graphs") +
  xlab("c*") +
  theme_bw() +
  ggtitle("(c)")

supp_f1 <- ((supp_fig_1a + theme(legend.position = "none")) + supp_fig_1b + theme(legend.title = element_blank())) /
  supp_fig_1c 
ggsave("figures/supp_sparse.png", supp_f1, width = 9, height = 7, units = "in")


#### ADD FIG -- TRUNCATED RESAMPLING ##############################
df_trunc <- readRDS(file = "data/df_trunc.RDS")
df_trunc <- df_trunc %>%
  select(c(cover, width, keep_M)) %>%
  rename(trunc.cover = cover,
         trunc.width = width,
         trunc.keep_M = keep_M)
df_trunc <- cbind(df[1:8,], df_trunc) 

# Supp fig 2(a): avg.nb = 7, M = 50, coverage vs c*
supp_fig_2a <- df_trunc %>%
  gather(method, coverage, c(cover, trunc.cover, tru.cover, naive.cover.01, naive.cover.05), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = coverage, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.006, 0.01, 0.02, 0.03, 0.04)) +
  scale_color_manual(labels = c("resample", bquote(atop(" ", atop(textstyle("truncated"), textstyle("resample")))), "oracle", bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.01)"))))), bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.05)")))))), 
                     values = c("#D55E00", "#E69F00", "#999999", "#0072B2", "#009E73")) +
  ylim(c(0.4, 1)) +
  xlab("c*") +
  theme_bw() +
  ggtitle("(a)")

# Supp fig 2(b): avg.nb = 7, M = 50, CI length vs c* 
supp_fig_2b <- df_trunc %>%
  gather(method, CI_width, c(width, trunc.width, tru.width, naive.width.01, naive.width.05), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = CI_width, color = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(0.006, 0.01, 0.02, 0.03, 0.04)) +
  scale_color_manual(labels = c("resample", bquote(atop(" ", atop(textstyle("truncated"), textstyle("resample")))), "oracle", bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.01)"))))), bquote(atop(" ", atop(textstyle("naive"), textstyle(paste("(",alpha,"=0.05)")))))), 
                     values = c("#D55E00", "#E69F00", "#999999", "#0072B2", "#009E73")) +
  ylim(c(0.15, 0.60)) +
  ylab("CI length") +
  xlab("c*") +
  theme_bw() +
  ggtitle("(b)")

# Supp fig 2(c): avg.nb = 7, M = 50, keep/M vs c* 
supp_fig_2c <- df_trunc %>%
  gather(method, keep, c(keep_M, trunc.keep_M), factor_key = TRUE) %>%
  ggplot(aes(x = c, y = keep*100, color = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0.006, 0.008, 0.01, 0.02, 0.03, 0.04)) +
  scale_color_manual(labels = c("resample", bquote(atop(" ", atop(textstyle("truncated"), textstyle("resample"))))), 
                     values = c("#293352", "#52854C")) +
  ylim(c(0, 15)) +
  ylab("% kept graphs") +
  xlab("c*") +
  theme_bw() +
  ggtitle("(c)")

supp_f2 <- ((supp_fig_2a + theme(legend.position = "none")) + supp_fig_2b + theme(legend.title = element_blank())) /
  (supp_fig_2c + theme(legend.title = element_blank()))
ggsave("figures/supp_trunc.png", supp_f2, width = 9, height = 7, units = "in")

#### ADD FIG -- BOOTSTRAP ##################################
df_boot <- readRDS(file = "data/df_boot.RDS")

# Supp fig 3(a): coverage vs M
supp_fig_3a <- df_boot %>%
  ggplot(aes(x = boot_M, y = cover)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(100, 500, 750, 1000)) +
  xlab("M") +
  theme_bw() +
  ggtitle("(a)")

# Supp fig 3(b): CI length vs M
supp_fig_3b <- df_boot %>%
  ggplot(aes(x = boot_M, y = width)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = c(100, 500, 750, 1000)) +
  ylab("CI length") +
  xlab("M") +
  theme_bw() +
  ggtitle("(b)")

supp_f3 <- supp_fig_3a + supp_fig_3b
ggsave("figures/supp_boot.png", supp_f3, width = 7, height = 4, units = "in")
