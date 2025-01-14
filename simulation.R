## This is an execution file for the Monte Carlo simulations in the paper.
## Load "function" file first, and then execute following code to perform the analyses.

source(".../function.R")

## Common settings
cl<-makeCluster(detectCores()-1, type="PSOCK")
seed<-5
dist<-"norm"
K<-3000
n_trt<-100
n_star<-100
theta_trt<-0.4
theta_cnt<-seq(0, 0.4, 0.01)
B<-1000
alpha<-0.025
power<-0.8
sigma<-1
delta_null<-0
M<-10000
burnin<-2000

scenario_sim_list<-list(
  list(scenario=1, theta_ext=c(0, 0), n_s=c(c(50, 100, 100))),
  list(scenario=2, theta_ext=c(0, 0.4), n_s=c(c(50, 100, 100))),
  list(scenario=3, theta_ext=c(0.4, 0.4), n_s=c(c(50, 100, 100))),
  list(scenario=4, theta_ext=c(0, 0, 0, 0), n_s=c(c(50, 100, 100, 100, 100))),
  list(scenario=5, theta_ext=c(0, 0.1, 0.2, 0.3), n_s=c(c(50, 100, 100, 100, 100))),
  list(scenario=6, theta_ext=c(0.4, 0.4, 0.4, 0.4), n_s=c(c(50, 100, 100, 100, 100)))
)

## ACWE
method<-"ACWE"
remark<-paste0("B=",B)
Sys.time()
system.time(
  result_sim_ACWE<-map_dfr(scenario_sim_list, ~sim_ACWE_all(.x$scenario, .x$theta_ext, .x$n_s))
)
stopCluster(cl)

## no-pooling
method<-"no-pooling"
remark<-"-"
Sys.time()
system.time(
  result_sim_no_pooling<-map_dfr(scenario_sim_list, ~sim_no_pooling_all(.x$scenario, .x$theta_ext, .x$n_s))
)

## full-pooling
method<-"full-pooling"
remark<-"-"
Sys.time()
system.time(
  result_sim_full_pooling<-map_dfr(scenario_sim_list, ~sim_full_pooling_all(.x$scenario, .x$theta_ext, .x$n_s))
)

## MAP
method<-"MAP"
robust<-F
w_robust<-NULL
sd_tau<-0.1
n_component<-1
remark<-paste0("sd_tau=", sd_tau, ", ",
               "n_component=", n_component, ", ",
               "robust=", robust)
Sys.time()
system.time(
  result_sim_MAP<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, .x$theta_ext, .x$n_s))
)

## R-MAP
method<-"R-MAP"
robust<-T
w_robust<-0.5
remark<-paste0("sd_tau=", sd_tau, ", ",
               "n_component=", n_component, ", ",
               "robust=", robust, ", ",
               "w_robust=", w_robust)
Sys.time()
system.time(
  result_sim_RMAP<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, .x$theta_ext, .x$n_s))
)

## MPP
method<-"MPP"
a_omega<-b_omega<-1
eta<-10000
remark<-paste0("a_omega=", a_omega, ", ",
               "b_omega=", b_omega, ", ",
               "eta=", eta)
Sys.time()
system.time(
  result_sim_MPP<-map_dfr(scenario_sim_list, ~sim_MPP_all(.x$scenario, .x$theta_ext, .x$n_s))
)

result_sim_all<-bind_rows(result_sim_ACWE,
                          result_sim_no_pooling,
                          result_sim_full_pooling,
                          result_sim_MAP,
                          result_sim_RMAP,
                          result_sim_MPP)
result_sim_all<-tibble(result_sim_all,
                       ESS_actual=sigma^2/((result_sim_all$SE^2) - (sigma^2/n_trt)))

## plot
size_axistitle<-18
size_axistext<-15
size_legend<-18
size_point<-0.5
label_method<-c("ACWE", "MAP", "no-pooling", "R-MAP", "full-pooling","MPP")

## Rejection probability
plot_reject_1<-
  result_sim_all %>% filter(scenario==1) %>% ggplot(aes(x=theta_cnt, y=reject,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, 0.2)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  geom_hline(yintercept=power, linetype=3) + 
  geom_hline(yintercept=alpha, linetype=3) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * ") = (0, 0)" )) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_reject_2<-
  result_sim_all %>% filter(scenario==2) %>% ggplot(aes(x=theta_cnt, y=reject,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, 0.2)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  geom_hline(yintercept=power, linetype=3) + 
  geom_hline(yintercept=alpha, linetype=3) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * ") = (0, 0.4)")) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_reject_3<-
  result_sim_all %>% filter(scenario==3) %>% ggplot(aes(x=theta_cnt, y=reject,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, 0.2)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  geom_hline(yintercept=power, linetype=3) + 
  geom_hline(yintercept=alpha, linetype=3) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * ") = (0.4, 0.4)")) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_reject_4<-
  result_sim_all %>% filter(scenario==4) %>% ggplot(aes(x=theta_cnt, y=reject,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, 0.2)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  geom_hline(yintercept=power, linetype=3) + 
  geom_hline(yintercept=alpha, linetype=3) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * "," ~  theta^(3) * "," ~ theta^(4) * ") = (0, 0, 0, 0)" )) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_reject_5<-
  result_sim_all %>% filter(scenario==5) %>% ggplot(aes(x=theta_cnt, y=reject,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, 0.2)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  geom_hline(yintercept=power, linetype=3) + 
  geom_hline(yintercept=alpha, linetype=3) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * "," ~  theta^(3) * "," ~ theta^(4) * ") = (0, 0.1, 0.2, 0.3)")) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_reject_6<-
  result_sim_all %>% filter(scenario==6) %>% ggplot(aes(x=theta_cnt, y=reject,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, 0.2)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  geom_hline(yintercept=power, linetype=3) + 
  geom_hline(yintercept=alpha, linetype=3) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * "," ~  theta^(3) * "," ~ theta^(4) * ") = (0.4, 0.4, 0.4, 0.4)")) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_reject_all<-ggarrange(plot_reject_1, plot_reject_4,
                           plot_reject_2, plot_reject_5,
                           plot_reject_3, plot_reject_6,
                           labels=c("#1", "#4",
                                    "#2", "#5",
                                    "#3", "#6"),
                           common.legend=T, legend="bottom", ncol=2, nrow=3)

## Bias
plot_bias_1<-
  result_sim_all %>% filter(scenario==1) %>% ggplot(aes(x=theta_cnt, y=bias,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(-0.4,0.4), breaks=seq(-0.4, 0.4, 0.2)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * ") = (0, 0)" )) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_bias_2<-
  result_sim_all %>% filter(scenario==2) %>% ggplot(aes(x=theta_cnt, y=bias,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(-0.4,0.4), breaks=seq(-0.4, 0.4, 0.2)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * ") = (0, 0.4)" )) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_bias_3<-
  result_sim_all %>% filter(scenario==3) %>% ggplot(aes(x=theta_cnt, y=bias,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(-0.4,0.4), breaks=seq(-0.4, 0.4, 0.2)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * ") = (0.4, 0.4)" )) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_bias_4<-
  result_sim_all %>% filter(scenario==4) %>% ggplot(aes(x=theta_cnt, y=bias,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(-0.4,0.4), breaks=seq(-0.4, 0.4, 0.2)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * "," ~  theta^(3) * "," ~ theta^(4) * ") = (0, 0, 0, 0)" )) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_bias_5<-
  result_sim_all %>% filter(scenario==5) %>% ggplot(aes(x=theta_cnt, y=bias,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(-0.4,0.4), breaks=seq(-0.4, 0.4, 0.2)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * "," ~  theta^(3) * "," ~ theta^(4) * ") = (0, 0.1, 0.2, 0.3)" )) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_bias_6<-
  result_sim_all %>% filter(scenario==6) %>% ggplot(aes(x=theta_cnt, y=bias,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(-0.4,0.4), breaks=seq(-0.4, 0.4, 0.2)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * "," ~  theta^(3) * "," ~ theta^(4) * ") = (0.4, 0.4, 0.4, 0.4)" )) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_bias_all<-ggarrange(plot_bias_1, plot_bias_4, 
                         plot_bias_2, plot_bias_5,
                         plot_bias_3, plot_bias_6,
                         labels=c("#1", "#4",
                                  "#2", "#5",
                                  "#3", "#6"),
                         common.legend=T, legend="bottom", ncol=2, nrow=3)

## RMSE
plot_RMSE_1<-
  result_sim_all %>% filter(scenario==1) %>% ggplot(aes(x=theta_cnt, y=RMSE,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,0.4), breaks=seq(0, 0.4, 0.1)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * ") = (0, 0)" )) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_RMSE_2<-
  result_sim_all %>% filter(scenario==2) %>% ggplot(aes(x=theta_cnt, y=RMSE,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,0.4), breaks=seq(0, 0.4, 0.1)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * ") = (0, 0.4)" )) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_RMSE_3<-
  result_sim_all %>% filter(scenario==3) %>% ggplot(aes(x=theta_cnt, y=RMSE,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,0.4), breaks=seq(0, 0.4, 0.1)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * ") = (0.4, 0.4)" )) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_RMSE_4<-
  result_sim_all %>% filter(scenario==4) %>% ggplot(aes(x=theta_cnt, y=RMSE,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,0.4), breaks=seq(0, 0.4, 0.1)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * "," ~  theta^(3) * "," ~ theta^(4) * ") = (0, 0, 0, 0)" )) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_RMSE_5<-
  result_sim_all %>% filter(scenario==5) %>% ggplot(aes(x=theta_cnt, y=RMSE,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,0.4), breaks=seq(0, 0.4, 0.1)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * "," ~  theta^(3) * "," ~ theta^(4) * ") = (0, 0.1, 0.2, 0.3)" )) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_RMSE_6<-
  result_sim_all %>% filter(scenario==6) %>% ggplot(aes(x=theta_cnt, y=RMSE,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,0.4), breaks=seq(0, 0.4, 0.1)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * "," ~  theta^(3) * "," ~ theta^(4) * ") = (0.4, 0.4, 0.4, 0.4)" )) +
  xlab(expression(theta^(0))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_RMSE_all<-ggarrange(plot_RMSE_1, plot_RMSE_4, 
                         plot_RMSE_2, plot_RMSE_5,
                         plot_RMSE_3, plot_RMSE_6,
                         labels=c("#1", "#4",
                                  "#2", "#5",
                                  "#3", "#6"),
                         common.legend=T, legend="bottom", ncol=2, nrow=3)

## ESS
plot_ESS_1<-
  result_sim_all %>% filter(scenario==1) %>% ggplot(aes(x=theta_cnt, y=ESS_actual,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,500), breaks=seq(0,500,100)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * ") = (0, 0)" )) +
  xlab(expression(theta^(0))) + ylab("ESS") + 
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_ESS_2<-
  result_sim_all %>% filter(scenario==2) %>% ggplot(aes(x=theta_cnt, y=ESS_actual,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,500), breaks=seq(0,500,100)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * ") = (0, 0.4)")) +
  xlab(expression(theta^(0))) + ylab("ESS") + 
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_ESS_3<-
  result_sim_all %>% filter(scenario==3) %>% ggplot(aes(x=theta_cnt, y=ESS_actual,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,500), breaks=seq(0,500,100)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * ") = (0.4, 0.4)")) +
  xlab(expression(theta^(0))) + ylab("ESS") + 
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_ESS_4<-
  result_sim_all %>% filter(scenario==4) %>% ggplot(aes(x=theta_cnt, y=ESS_actual,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,500), breaks=seq(0,500,100)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * "," ~  theta^(3) * "," ~ theta^(4) * ") = (0, 0, 0, 0)" )) +
  xlab(expression(theta^(0))) + ylab("ESS") + 
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_ESS_5<-
  result_sim_all %>% filter(scenario==5) %>% ggplot(aes(x=theta_cnt, y=ESS_actual,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,500), breaks=seq(0,500,100)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * "," ~  theta^(3) * "," ~ theta^(4) * ") = (0, 0.1, 0.2, 0.3)")) +
  xlab(expression(theta^(0))) + ylab("ESS") + 
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_ESS_6<-
  result_sim_all %>% filter(scenario==6) %>% ggplot(aes(x=theta_cnt, y=ESS_actual,
                                                        color=factor(method, levels=label_method),
                                                        linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,500), breaks=seq(0,500,100)) +
  xlim(c(min(theta_cnt), max(theta_cnt))) +
  ggtitle(expression("(" * theta^(1) * "," ~ theta^(2) * "," ~  theta^(3) * "," ~ theta^(4) * ") = (0.4, 0.4, 0.4, 0.4)")) +
  xlab(expression(theta^(0))) + ylab("ESS") + 
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_ESS_all<-ggarrange(plot_ESS_1, plot_ESS_4,
                        plot_ESS_2, plot_ESS_5,
                        plot_ESS_3, plot_ESS_6,
                        labels=c("#1", "#4",
                                 "#2", "#5",
                                 "#3", "#6"),
                        common.legend=T, legend="bottom", ncol=2, nrow=3)
