## This is an execution file for the application in the paper.
## Load "function" file first, and then execute following code to perform the analyses.

source(".../functions.R")

## Common settings
n_trt<-39
mean_trt<--29.2
n_s<-c(20, 74, 166, 328, 20, 25, 58)
mean_s<-c(-63.1, -51, -49, -36, -47, -90, -54)
v_s<-rep(88^2, 7)
n_star<-40
sigma<-88
sd_tau<-0.1
a_omega<-b_omega<-1
eta<-10000

alpha<-0.025
B<-10000
delta_null<-0
M<-10000
burnin<-2000
seed<-5

## ACWE
set.seed(seed)
result_app_ACWE<-ACWE_2arm(n_trt=n_trt, mean_trt=mean_trt, n_s=n_s, mean_s=mean_s, v_s=v_s,
                           n_star=n_star, sigma=sigma, alpha=alpha, B=B)

## no-pooling
result_app_no_pooling<-no_pooling(n_trt=n_trt, mean_trt=mean_trt,
                                  n_cnt=n_s[1], mean_cnt=mean_s[1],
                                  sigma=sigma, alpha=alpha)

## full-pooling
result_app_full_pooling<-full_pooling(n_trt=n_trt, mean_trt=mean_trt,
                                      n_s=n_s, mean_s=mean_s,
                                      sigma=sigma, alpha=alpha)

## MAP
set.seed(seed)
result_app_MAP<-MAP(n_trt=n_trt, mean_trt=mean_trt, n_s=n_s, mean_s=mean_s, sigma=sigma,
                    alpha=alpha, sd_tau=sd_tau, n_component=1, robust=F, w_robust=NULL)

## R-MAP
set.seed(seed)
result_app_RMAP<-MAP(n_trt=n_trt, mean_trt=mean_trt, n_s=n_s, mean_s=mean_s, sigma=sigma,
                     alpha=alpha, sd_tau=sd_tau, n_component=1, robust=T, w_robust=0.5)

## MPP
set.seed(seed)
result_app_MPP<-MPP_2arm(n_trt=n_trt, mean_trt=mean_trt, n_s=n_s, mean_s=mean_s, v_s=v_s,
                         a_omega=a_omega, b_omega=b_omega, eta=eta, sigma=sigma, alpha=alpha)

result_app_all<-bind_rows(result_app_no_pooling[1:3],
                          result_app_full_pooling[1:3],
                          result_app_ACWE[1:3],
                          result_app_MAP[1:3],
                          result_app_RMAP[1:3],
                          result_app_MPP[1:3])

result_app_all<-result_app_all %>%
  dplyr::mutate(group=(row_number()+1) %/% 2) %>%
  dplyr::group_by(group) %>%
  dplyr::summarize(delta_estimated=first(delta_estimated),
                   SE=first(SE),
                   CI_lower=first(CI),
                   CI_upper=last(CI)) %>% select(-group)

label_method<-c("no-pooling", "full-pooling","ACWE", "MAP", "R-MAP", "MPP")
result_app_all<-tibble(method=label_method,
                       result_app_all)
result_app_all$method<-result_app_all$method %>% factor(levels=rev(label_method))

## plot
size_axistext<-15

plot_app<-ggplot(result_app_all, aes(x=delta_estimated, y=method)) +
  geom_point(size=2, color="black") +
  geom_errorbarh(aes(xmin=CI_lower, xmax=CI_upper), height=0.3, color="black") + 
  geom_vline(xintercept=0, linetype="dashed", color="black") + 
  geom_text(aes(x=delta_estimated+1,
                label=paste0(round(delta_estimated, 1), 
                             " (", round(CI_lower, 1), ", ",
                             round(CI_upper, 1), ")")),
            vjust=2, size=4, hjust=0.3) +
  xlab("Between-Group Difference in Mean ΔCDAI") + 
  theme(axis.title.y=element_blank(),
        axis.title.x=element_text(size=15),
        axis.text.x=element_text(size=size_axistext),
        axis.text.y=element_text(size=size_axistext)) +
  scale_x_continuous(breaks=seq(-20,80,10))
