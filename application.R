## This is an execution file for the application in the paper.
## Load "function" file first, and then execute following code to perform the analyses.

source(".../function.R")

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
B_CI<-5000

# ACWE
set.seed(seed)
result_app_ACWE<-ACWE_2arm(n_trt=n_trt, mean_trt=mean_trt, n_s=n_s, mean_s=mean_s, v_s=v_s,
                           n_star=n_star, sigma=sigma, alpha=alpha, B=B)
set.seed(seed)
phi<-ACWE(n_s=n_s, mean_s=mean_s, v_s=v_s,
          n_star=n_star, sigma=sigma, alpha=alpha, B=B)$correction_factor

sum(n_s*result_app_ACWE$weight)^2/sum(n_s*(result_app_ACWE$weight)^2)
n_star*phi

set.seed(seed)
system.time(
  result_app_ACWE_CI<-ACWE_w_CI(n_s=n_s, mean_s=mean_s, v_s=v_s, n_star=n_star, sigma=sigma, alpha=alpha, B=B, B_CI=B_CI)
)

# no-borrowing
result_app_no_borrowing<-no_borrowing(n_trt=n_trt, mean_trt=mean_trt,
                                  n_cnt=n_s[1], mean_cnt=mean_s[1],
                                  sigma=sigma, alpha=alpha)

# full-borrowing
result_app_full_borrowing<-full_borrowing(n_trt=n_trt, mean_trt=mean_trt,
                                      n_s=n_s, mean_s=mean_s,
                                      sigma=sigma, alpha=alpha)

# RMAP
set.seed(seed)
result_app_RMAP<-MAP(n_trt=n_trt, mean_trt=mean_trt, n_s=n_s, mean_s=mean_s, sigma=sigma,
                     alpha=alpha, sd_tau=sd_tau, n_component=1, robust=T, w_robust=0.5)

# MPP
set.seed(seed)
result_app_MPP<-MPP_2arm(n_trt=n_trt, mean_trt=mean_trt, n_s=n_s, mean_s=mean_s, v_s=v_s,
                         a_omega=a_omega, b_omega=b_omega, eta=eta, sigma=sigma, alpha=alpha)

set.seed(seed)
result_app_MPP_CI<-MPP(n_s=n_s, mean_s=mean_s, v_s=v_s, a_omega=a_omega, b_omega=b_omega, eta=eta, sigma=sigma)$post_mat[,-1]

# EBPP
set.seed(seed)
result_app_EBPP<-EBPP_2arm(n_trt=n_trt, mean_trt=mean_trt, n_s=n_s, mean_s=mean_s, v_s=v_s,
                           eta=eta, sigma=sigma, alpha=alpha)
set.seed(seed)
result_app_EBPP_CI<-EBPP_w_CI(n_s=n_s, mean_s=mean_s, v_s=v_s, sigma=sigma, eta=eta, alpha=alpha, B_CI=B_CI)

result_app_all<-bind_rows(result_app_no_borrowing[1:3],
                          result_app_full_borrowing[1:3],
                          result_app_ACWE[1:3],
                          result_app_MPP[1:3],
                          result_app_EBPP[1:3],
                          result_app_RMAP[1:3])

result_app_all<-result_app_all %>%
  mutate(group=(row_number()+1) %/% 2) %>%
  group_by(group) %>%
  summarize(delta_estimated=first(delta_estimated),
            SE=first(SE),
            CI_lower=first(CI),
            CI_upper=last(CI)) %>% select(-group)

label_method<-c("no-borrowing", "full-borrowing","ACWE", "MPP", "EBPP", "R-MAP")
result_app_all<-tibble(method=label_method,
                       result_app_all)
result_app_all$method<-result_app_all$method %>% factor(levels=rev(label_method))

tmp<-cbind(t(result_app_ACWE_CI[,-1]),
           apply(result_app_MPP_CI, 2, function(x)quantile(x, probs=alpha, na.rm=T)),
           apply(result_app_MPP_CI, 2, function(x)quantile(x, probs=1-alpha, na.rm=T)),
           t(result_app_EBPP_CI))

CI<-tibble(ACWE_w_LCL=tmp[,1],
           ACWE_w_UCL=tmp[,2],
           MPP_w_LCL=tmp[,3],
           MPP_w_UCL=tmp[,4],
           EBPP_w_LCL=tmp[,5],
           EBPP_w_UCL=tmp[,6])

table_app<-tibble(study=c("H (2006)", "S (2007a)", "S (2007b)", "S (2001a)",
                          "W (2004)", "S (2001b)"),
                  n=n_s[-1],
                  mean_median=mean_s[-1],
                  std=((mean_s[1]-mean_s[-1])/sigma),
                  ACWE_w=(result_app_ACWE$weight[-1]),
                  ACWE_nw=n*ACWE_w,
                  MPP_w=(result_app_MPP$omega_estimated),
                  MPP_nw=n*MPP_w,
                  EBPP_w=(result_app_EBPP$omega_estimated),
                  EBPP_nw=n*EBPP_w,
                  
                  ACWE_w_LCL=CI$ACWE_w_LCL,
                  ACWE_w_UCL=CI$ACWE_w_UCL,
                  MPP_w_LCL=CI$MPP_w_LCL,
                  MPP_w_UCL=CI$MPP_w_UCL,
                  EBPP_w_LCL=CI$EBPP_w_LCL,
                  EBPP_w_UCL=CI$EBPP_w_UCL) %>% 
  mutate(study=factor(study,
                      levels=c("S (2007a)", "S (2007b)", "S (2001a)", "S (2001b)",
                               "H (2006)", "W (2004)"))) %>% arrange(study)

#################################################################
# plot
size_axistitle<-18
size_axistext<-15
size_legend<-18
size_point<-0.5
dpi<-800

# delta
plot_app<-ggplot(result_app_all, aes(x=delta_estimated, y=method)) +
  geom_point(size=2, color="black") +
  geom_errorbar(aes(xmin=CI_lower, xmax=CI_upper), width=0.3, color="black") + 
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
  scale_x_continuous(breaks=seq(-20, 80, 10))

# CI
for_plot_app_CI<-table_app %>% 
  select(study, ACWE_w, ACWE_w_LCL, ACWE_w_UCL,
         MPP_w, MPP_w_LCL, MPP_w_UCL,
         EBPP_w, EBPP_w_LCL, EBPP_w_UCL) %>% 
  pivot_longer(cols=matches("^(ACWE|MPP|EBPP)_w(_LCL|_UCL)?$"),
               names_to="name", values_to="value") %>% 
  mutate(method=str_extract(name, "^(ACWE|MPP|EBPP)"),
         stat=case_when(str_detect(name, "_LCL$") ~ "lower",
                        str_detect(name, "_UCL$") ~ "upper",
                        T ~ "estimate")) %>% select(-name) %>%
  pivot_wider(names_from=stat, values_from=value) %>%
  mutate(method=factor(method, levels=c("ACWE", "MPP", "EBPP"))) %>%
  select(study, method, estimate, lower, upper)

plot_app_CI_1<-
  ggplot(for_plot_app_CI, aes(x=study, y=estimate, color=method)) +
  geom_point(position=position_dodge(width=0.6), size=1) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width=0.3, position=position_dodge(width=0.6)) +
  coord_cartesian(ylim=c(0, 1)) +
  labs(x="Study", y="Estimate", color="Method") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        legend.title=element_blank())

plot_app_CI_2<-
  ggplot(for_plot_app_CI, aes(x=study, y=estimate, color=method)) +
  geom_point(position=position_dodge(width=0.6), size=1) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width=0.3, position=position_dodge(width=0.6)) +
  coord_cartesian(ylim=c(0.005, 0.02)) +
  labs(x="Study", y="Estimate", color="Method") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        legend.title=element_blank())

plot_app_CI_all<-ggarrange(plot_app_CI_1, plot_app_CI_2,
                           common.legend=T, legend="bottom", ncol=2, nrow=1)
