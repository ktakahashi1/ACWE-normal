## This is an execution file for the numerical examples in the paper.
## Load "function" file first, and then execute following code to perform the analyses.

source(".../function.R")

## Common settings
dist<-"norm"
n_star<-100
v_s<-rep(1,3)
sigma<-1
alpha<-0.025
B<-100000
M<-100000
burnin<-2000
eta<-10000
seed<-1

size_axistitle<-18
size_axistext<-15
size_legend<-18

## Scenario 1
n_s<-c(50,100,100)
mean_s<-cbind(0, seq(-2,2,0.01), 0)

## ACWE
method<-"ACWE"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(mean_s), ncol=ncol(mean_s))
phi<-rep(NA, nrow(mean_s))
Sys.time()
t<-system.time(
  for (i in 1:nrow(mean_s)) {
    result_ex_ACWE<-ACWE(n_s=n_s, mean_s=mean_s[i,], v_s=v_s,
                         n_star=n_star, sigma=sigma, alpha=alpha, B=B)
    weight[i,]<-result_ex_ACWE$weight
    phi[i]<-result_ex_ACWE$correction_factor
  }
)[[3]]
for_plot_ex_1_ACWE<-tibble(mean_1=mean_s[,2], w1=weight[,2], w2=weight[,3], wn1=w1*n_s[1+1], wn2=w2*n_s[1+2], phi=phi)
for_plot_ex_1_ACWE<-for_plot_ex_1_ACWE %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2", "phi"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_1_ACWE$legend<-factor(for_plot_ex_1_ACWE$legend, levels=c("w1", "w2", "wn1", "wn2", "phi"))

## MPP
method<-"MPP"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(mean_s), ncol=ncol(mean_s)-1)
Sys.time()
t<-system.time(
  for (i in 1:nrow(mean_s)) {
    result_ex_MPP<-MPP(n_s=n_s, mean_s=mean_s[i,], v_s=v_s,
                       a_omega=1, b_omega=1, eta=eta, sigma=sigma)
    weight[i,]<-result_ex_MPP$omega
  }
)[[3]]
for_plot_ex_1_MPP<-tibble(mean_1=mean_s[,2], w1=weight[,1], w2=weight[,2], wn1=w1*n_s[1+1], wn2=w2*n_s[1+2])
for_plot_ex_1_MPP<-for_plot_ex_1_MPP %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_1_MPP$legend<-factor(for_plot_ex_1_MPP$legend, levels=c("w1", "w2", "wn1", "wn2"))

## EBPP
method<-"EBPP"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(mean_s), ncol=ncol(mean_s)-1)
Sys.time()
t<-system.time(
  for (i in 1:nrow(mean_s)) {
    result_ex_EBPP<-EBPP(n_s=n_s, mean_s=mean_s[i,], v_s=v_s, eta=eta, sigma=sigma)
    weight[i,]<-result_ex_EBPP$omega
  }
)[[3]]
for_plot_ex_1_EBPP<-tibble(mean_1=mean_s[,2], w1=weight[,1], w2=weight[,2], wn1=w1*n_s[1+1], wn2=w2*n_s[1+2])
for_plot_ex_1_EBPP<-for_plot_ex_1_EBPP %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_1_EBPP$legend<-factor(for_plot_ex_1_EBPP$legend, levels=c("w1", "w2", "wn1", "wn2"))

for_plot_ex_1_all<-bind_rows(for_plot_ex_1_ACWE, for_plot_ex_1_MPP, for_plot_ex_1_EBPP)
for_plot_ex_1_all<-tibble(x_axis=colnames(for_plot_ex_1_all)[1], for_plot_ex_1_all) %>% memisc::rename(mean_1=x_value)

## Scenario 2
n_s<-c(50,100,100)
mean_s<-cbind(0, seq(-2,2,0.01), 0.2)

## ACWE
method<-"ACWE"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(mean_s), ncol=ncol(mean_s))
phi<-rep(NA, nrow(mean_s))
Sys.time()
t<-system.time(
  for (i in 1:nrow(mean_s)) {
    result_ex_ACWE<-ACWE(n_s=n_s, mean_s=mean_s[i,], v_s=v_s,
                         n_star=n_star, sigma=sigma, alpha=alpha, B=B)
    weight[i,]<-result_ex_ACWE$weight
    phi[i]<-result_ex_ACWE$correction_factor
  }
)[[3]]
for_plot_ex_2_ACWE<-tibble(mean_1=mean_s[,2], w1=weight[,2], w2=weight[,3], wn1=w1*n_s[1+1], wn2=w2*n_s[1+2], phi=phi)
for_plot_ex_2_ACWE<-for_plot_ex_2_ACWE %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2", "phi"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_2_ACWE$legend<-factor(for_plot_ex_2_ACWE$legend, levels=c("w1", "w2", "wn1", "wn2", "phi"))

## MPP
method<-"MPP"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(mean_s), ncol=ncol(mean_s)-1)
Sys.time()
t<-system.time(
  for (i in 1:nrow(mean_s)) {
    result_ex_MPP<-MPP(n_s=n_s, mean_s=mean_s[i,], v_s=v_s,
                       a_omega=1, b_omega=1, eta=eta, sigma=sigma)
    weight[i,]<-result_ex_MPP$omega
  }
)[[3]]
for_plot_ex_2_MPP<-tibble(mean_1=mean_s[,2], w1=weight[,1], w2=weight[,2], wn1=w1*n_s[1+1], wn2=w2*n_s[1+2])
for_plot_ex_2_MPP<-for_plot_ex_2_MPP %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_2_MPP$legend<-factor(for_plot_ex_2_MPP$legend, levels=c("w1", "w2", "wn1", "wn2"))

## EBPP
method<-"EBPP"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(mean_s), ncol=ncol(mean_s)-1)
Sys.time()
t<-system.time(
  for (i in 1:nrow(mean_s)) {
    result_ex_EBPP<-EBPP(n_s=n_s, mean_s=mean_s[i,], v_s=v_s, eta=eta, sigma=sigma)
    weight[i,]<-result_ex_EBPP$omega
  }
)[[3]]
for_plot_ex_2_EBPP<-tibble(mean_1=mean_s[,2], w1=weight[,1], w2=weight[,2], wn1=w1*n_s[1+1], wn2=w2*n_s[1+2])
for_plot_ex_2_EBPP<-for_plot_ex_2_EBPP %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_2_EBPP$legend<-factor(for_plot_ex_2_EBPP$legend, levels=c("w1", "w2", "wn1", "wn2"))

for_plot_ex_2_all<-bind_rows(for_plot_ex_2_ACWE, for_plot_ex_2_MPP, for_plot_ex_2_EBPP)
for_plot_ex_2_all<-tibble(x_axis=colnames(for_plot_ex_2_all)[1], for_plot_ex_2_all) %>% memisc::rename(mean_1=x_value)

## Scenario 3
n_s<-c(50,100,100)
mean_s<-cbind(0, seq(-2,2,0.01), 0.4)

## ACWE
method<-"ACWE"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(mean_s), ncol=ncol(mean_s))
phi<-rep(NA, nrow(mean_s))
Sys.time()
t<-system.time(
  for (i in 1:nrow(mean_s)) {
    result_ex_ACWE<-ACWE(n_s=n_s, mean_s=mean_s[i,], v_s=v_s,
                         n_star=n_star, sigma=sigma, alpha=alpha, B=B)
    weight[i,]<-result_ex_ACWE$weight
    phi[i]<-result_ex_ACWE$correction_factor
  }
)[[3]]
for_plot_ex_3_ACWE<-tibble(mean_1=mean_s[,2], w1=weight[,2], w2=weight[,3], wn1=w1*n_s[1+1], wn2=w2*n_s[1+2], phi=phi)
for_plot_ex_3_ACWE<-for_plot_ex_3_ACWE %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2", "phi"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_3_ACWE$legend<-factor(for_plot_ex_3_ACWE$legend, levels=c("w1", "w2", "wn1", "wn2", "phi"))

## MPP
method<-"MPP"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(mean_s), ncol=ncol(mean_s)-1)
Sys.time()
t<-system.time(
  for (i in 1:nrow(mean_s)) {
    result_ex_MPP<-MPP(n_s=n_s, mean_s=mean_s[i,], v_s=v_s,
                       a_omega=1, b_omega=1, eta=eta, sigma=sigma)
    weight[i,]<-result_ex_MPP$omega
  }
)[[3]]
for_plot_ex_3_MPP<-tibble(mean_1=mean_s[,2], w1=weight[,1], w2=weight[,2], wn1=w1*n_s[1+1], wn2=w2*n_s[1+2])
for_plot_ex_3_MPP<-for_plot_ex_3_MPP %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_3_MPP$legend<-factor(for_plot_ex_3_MPP$legend, levels=c("w1", "w2", "wn1", "wn2"))

## EBPP
method<-"EBPP"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(mean_s), ncol=ncol(mean_s)-1)
Sys.time()
t<-system.time(
  for (i in 1:nrow(mean_s)) {
    result_ex_EBPP<-EBPP(n_s=n_s, mean_s=mean_s[i,], v_s=v_s, eta=eta, sigma=sigma)
    weight[i,]<-result_ex_EBPP$omega
  }
)[[3]]
for_plot_ex_3_EBPP<-tibble(mean_1=mean_s[,2], w1=weight[,1], w2=weight[,2], wn1=w1*n_s[1+1], wn2=w2*n_s[1+2])
for_plot_ex_3_EBPP<-for_plot_ex_3_EBPP %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_3_EBPP$legend<-factor(for_plot_ex_3_EBPP$legend, levels=c("w1", "w2", "wn1", "wn2"))

for_plot_ex_3_all<-bind_rows(for_plot_ex_3_ACWE, for_plot_ex_3_MPP, for_plot_ex_3_EBPP)
for_plot_ex_3_all<-tibble(x_axis=colnames(for_plot_ex_3_all)[1], for_plot_ex_3_all) %>% memisc::rename(mean_1=x_value)

## Scenario 4
n_s<-cbind(50, seq(1,100,1), 100)
mean_s<-rep(0,3)

## ACWE
method<-"ACWE"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(n_s), ncol=ncol(n_s))
phi<-rep(NA, nrow(n_s))
Sys.time()
t<-system.time(
  for (i in 1:nrow(n_s)) {
    result_ex_ACWE<-ACWE(n_s=n_s[i,], mean_s=mean_s, v_s=v_s,
                         n_star=n_star, sigma=sigma, alpha=alpha, B=B)
    weight[i,]<-result_ex_ACWE$weight
    phi[i]<-result_ex_ACWE$correction_factor
  }
)[[3]]
for_plot_ex_4_ACWE<-tibble(n1=n_s[,2], w1=weight[,2], w2=weight[,3], wn1=w1*n1, wn2=w2*n_s[1,3], phi=phi)
for_plot_ex_4_ACWE<-for_plot_ex_4_ACWE %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2", "phi"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_4_ACWE$legend<-factor(for_plot_ex_4_ACWE$legend, levels=c("w1", "w2", "wn1", "wn2", "phi"))

## MPP
method<-"MPP"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(n_s), ncol=ncol(n_s)-1)
Sys.time()
t<-system.time(
  for (i in 1:nrow(n_s)) {
    result_ex_MPP<-MPP(n_s=n_s[i,], mean_s=mean_s, v_s=v_s,
                       a_omega=1, b_omega=1, eta=eta, sigma=sigma)
    weight[i,]<-result_ex_MPP$omega
  }
)[[3]]
for_plot_ex_4_MPP<-tibble(n1=n_s[,2], w1=weight[,1], w2=weight[,2], wn1=w1*n1, wn2=w2*n_s[1,3])
for_plot_ex_4_MPP<-for_plot_ex_4_MPP %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_4_MPP$legend<-factor(for_plot_ex_4_MPP$legend, levels=c("w1", "w2", "wn1", "wn2"))

## EBPP
method<-"EBPP"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(n_s), ncol=ncol(n_s)-1)
Sys.time()
t<-system.time(
  for (i in 1:nrow(n_s)) {
    result_ex_EBPP<-EBPP(n_s=n_s[i,], mean_s=mean_s, v_s=v_s, eta=eta, sigma=sigma)
    weight[i,]<-result_ex_EBPP$omega
  }
)[[3]]
for_plot_ex_4_EBPP<-tibble(n1=n_s[,2], w1=weight[,1], w2=weight[,2], wn1=w1*n1, wn2=w2*n_s[1,3])
for_plot_ex_4_EBPP<-for_plot_ex_4_EBPP %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_4_EBPP$legend<-factor(for_plot_ex_4_EBPP$legend, levels=c("w1", "w2", "wn1", "wn2"))

for_plot_ex_4_all<-bind_rows(for_plot_ex_4_ACWE, for_plot_ex_4_MPP, for_plot_ex_4_EBPP)
for_plot_ex_4_all<-tibble(x_axis=colnames(for_plot_ex_4_all)[1], for_plot_ex_4_all) %>% memisc::rename(n1=x_value)

## Scenario 5
n_s<-cbind(50, seq(1,100,1), 100)
mean_s<-c(0, 0, 0.2)

## ACWE
method<-"ACWE"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(n_s), ncol=ncol(n_s))
phi<-rep(NA, nrow(n_s))
Sys.time()
t<-system.time(
  for (i in 1:nrow(n_s)) {
    result_ex_ACWE<-ACWE(n_s=n_s[i,], mean_s=mean_s, v_s=v_s,
                         n_star=n_star, sigma=sigma, alpha=alpha, B=B)
    weight[i,]<-result_ex_ACWE$weight
    phi[i]<-result_ex_ACWE$correction_factor
  }
)[[3]]
for_plot_ex_5_ACWE<-tibble(n1=n_s[,2], w1=weight[,2], w2=weight[,3], wn1=w1*n1, wn2=w2*n_s[1,3], phi=phi)
for_plot_ex_5_ACWE<-for_plot_ex_5_ACWE %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2", "phi"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_5_ACWE$legend<-factor(for_plot_ex_5_ACWE$legend, levels=c("w1", "w2", "wn1", "wn2", "phi"))

## MPP
method<-"MPP"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(n_s), ncol=ncol(n_s)-1)
Sys.time()
t<-system.time(
  for (i in 1:nrow(n_s)) {
    result_ex_MPP<-MPP(n_s=n_s[i,], mean_s=mean_s, v_s=v_s,
                       a_omega=1, b_omega=1, eta=eta, sigma=sigma)
    weight[i,]<-result_ex_MPP$omega
  }
)[[3]]
for_plot_ex_5_MPP<-tibble(n1=n_s[,2], w1=weight[,1], w2=weight[,2], wn1=w1*n1, wn2=w2*n_s[1,3])
for_plot_ex_5_MPP<-for_plot_ex_5_MPP %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_5_MPP$legend<-factor(for_plot_ex_5_MPP$legend, levels=c("w1", "w2", "wn1", "wn2"))

## EBPP
method<-"EBPP"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(n_s), ncol=ncol(n_s)-1)
Sys.time()
t<-system.time(
  for (i in 1:nrow(n_s)) {
    result_ex_EBPP<-EBPP(n_s=n_s[i,], mean_s=mean_s, v_s=v_s, eta=eta, sigma=sigma)
    weight[i,]<-result_ex_EBPP$omega
  }
)[[3]]
for_plot_ex_5_EBPP<-tibble(n1=n_s[,2], w1=weight[,1], w2=weight[,2], wn1=w1*n1, wn2=w2*n_s[1,3])
for_plot_ex_5_EBPP<-for_plot_ex_5_EBPP %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_5_EBPP$legend<-factor(for_plot_ex_5_EBPP$legend, levels=c("w1", "w2", "wn1", "wn2"))

for_plot_ex_5_all<-bind_rows(for_plot_ex_5_ACWE, for_plot_ex_5_MPP, for_plot_ex_5_EBPP)
for_plot_ex_5_all<-tibble(x_axis=colnames(for_plot_ex_5_all)[1], for_plot_ex_5_all) %>% memisc::rename(n1=x_value)


## Scenario 6
n_s<-cbind(50, seq(1,100,1), 100)
mean_s<-c(0, 0, 0.4)

## ACWE
method<-"ACWE"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(n_s), ncol=ncol(n_s))
phi<-rep(NA, nrow(n_s))
Sys.time()
t<-system.time(
  for (i in 1:nrow(n_s)) {
    result_ex_ACWE<-ACWE(n_s=n_s[i,], mean_s=mean_s, v_s=v_s,
                         n_star=n_star, sigma=sigma, alpha=alpha, B=B)
    weight[i,]<-result_ex_ACWE$weight
    phi[i]<-result_ex_ACWE$correction_factor
  }
)[[3]]
for_plot_ex_6_ACWE<-tibble(n1=n_s[,2], w1=weight[,2], w2=weight[,3], wn1=w1*n1, wn2=w2*n_s[1,3], phi=phi)
for_plot_ex_6_ACWE<-for_plot_ex_6_ACWE %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2", "phi"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_6_ACWE$legend<-factor(for_plot_ex_6_ACWE$legend, levels=c("w1", "w2", "wn1", "wn2", "phi"))

## MPP
method<-"MPP"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(n_s), ncol=ncol(n_s)-1)
Sys.time()
t<-system.time(
  for (i in 1:nrow(n_s)) {
    result_ex_MPP<-MPP(n_s=n_s[i,], mean_s=mean_s, v_s=v_s,
                       a_omega=1, b_omega=1, eta=eta, sigma=sigma)
    weight[i,]<-result_ex_MPP$omega
  }
)[[3]]
for_plot_ex_6_MPP<-tibble(n1=n_s[,2], w1=weight[,1], w2=weight[,2], wn1=w1*n1, wn2=w2*n_s[1,3])
for_plot_ex_6_MPP<-for_plot_ex_6_MPP %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_6_MPP$legend<-factor(for_plot_ex_6_MPP$legend, levels=c("w1", "w2", "wn1", "wn2"))

## EBPP
method<-"EBPP"
set.seed(seed)
weight<-matrix(NA, nrow=nrow(n_s), ncol=ncol(n_s)-1)
Sys.time()
t<-system.time(
  for (i in 1:nrow(n_s)) {
    result_ex_EBPP<-EBPP(n_s=n_s[i,], mean_s=mean_s, v_s=v_s, eta=eta, sigma=sigma)
    weight[i,]<-result_ex_EBPP$omega
  }
)[[3]]
for_plot_ex_6_EBPP<-tibble(n1=n_s[,2], w1=weight[,1], w2=weight[,2], wn1=w1*n1, wn2=w2*n_s[1,3])
for_plot_ex_6_EBPP<-for_plot_ex_6_EBPP %>% pivot_longer(cols=c("w1", "w2", "wn1", "wn2"), names_to="legend", values_to="value") %>% mutate(method=method)
for_plot_ex_6_EBPP$legend<-factor(for_plot_ex_6_EBPP$legend, levels=c("w1", "w2", "wn1", "wn2"))

for_plot_ex_6_all<-bind_rows(for_plot_ex_6_ACWE, for_plot_ex_6_MPP, for_plot_ex_6_EBPP)
for_plot_ex_6_all<-tibble(x_axis=colnames(for_plot_ex_6_all)[1], for_plot_ex_6_all) %>% memisc::rename(n1=x_value)

#################################################################
# Combine and create plots
# Weight
title_1<-expression("(" * bar(italic(x))^(2) * "," ~ italic(n)[1] * "," ~  italic(n)[2] * ") = (0, 100, 100)" )
title_2<-expression("(" * bar(italic(x))^(2) * "," ~ italic(n)[1] * "," ~  italic(n)[2] * ") = (0.2, 100, 100)" )
title_3<-expression("(" * bar(italic(x))^(2) * "," ~ italic(n)[1] * "," ~  italic(n)[2] * ") = (0.4, 100, 100)" )
title_4<-expression("(" * bar(italic(x))^(1) * "," ~ bar(italic(x))^(2) * "," ~  italic(n)[2] * ") = (0, 0, 100)" )
title_5<-expression("(" * bar(italic(x))^(1) * "," ~ bar(italic(x))^(2) * "," ~  italic(n)[2] * ") = (0, 0.2, 100)" )
title_6<-expression("(" * bar(italic(x))^(1) * "," ~ bar(italic(x))^(2) * "," ~  italic(n)[2] * ") = (0, 0.4, 100)" )

plot_ex_w_1<-create_plot_ex_all(for_plot_ex_1_all, "w", title_1)
plot_ex_w_2<-create_plot_ex_all(for_plot_ex_2_all, "w", title_2)
plot_ex_w_3<-create_plot_ex_all(for_plot_ex_3_all, "w", title_3)
plot_ex_w_4<-create_plot_ex_all(for_plot_ex_4_all, "w", title_4)
plot_ex_w_5<-create_plot_ex_all(for_plot_ex_5_all, "w", title_5)
plot_ex_w_6<-create_plot_ex_all(for_plot_ex_6_all, "w", title_6)

plot_ex_w_all<-ggarrange(plot_ex_w_1,
                         plot_ex_w_4,
                         plot_ex_w_2,
                         plot_ex_w_5,
                         plot_ex_w_3,
                         plot_ex_w_6,
                         labels=c("#1", "#4",
                                  "#2", "#5",
                                  "#3", "#6"),
                         common.legend=T, legend="bottom", ncol=2, nrow=3)

# Weighted sample size
plot_ex_wn_1<-create_plot_ex_all(for_plot_ex_1_all, "wn", title_1)
plot_ex_wn_2<-create_plot_ex_all(for_plot_ex_2_all, "wn", title_2)
plot_ex_wn_3<-create_plot_ex_all(for_plot_ex_3_all, "wn", title_3)
plot_ex_wn_4<-create_plot_ex_all(for_plot_ex_4_all, "wn", title_4)
plot_ex_wn_5<-create_plot_ex_all(for_plot_ex_5_all, "wn", title_5)
plot_ex_wn_6<-create_plot_ex_all(for_plot_ex_6_all, "wn", title_6)

plot_ex_wn_all<-ggarrange(plot_ex_wn_1,
                          plot_ex_wn_4,
                          plot_ex_wn_2,
                          plot_ex_wn_5,
                          plot_ex_wn_3,
                          plot_ex_wn_6,
                          labels=c("#1", "#4",
                                   "#2", "#5",
                                   "#3", "#6"),
                          common.legend=T, legend="bottom", ncol=2, nrow=3)

# Phi
plot_ex_phi_1<-for_plot_ex_1_all %>%
  filter(method=="ACWE", legend %in% c("w1", "w2", "phi")) %>%
  ggplot(aes(x=x_value, color=legend)) +
  geom_line(aes(y=value), linewidth=1) +
  scale_color_manual(values=c("w1"="#F8766D", "w2"="#00BA38", "phi"="#619CFF"),
                     labels=c(w1=expression(hat(italic(w))^(1)),
                              w2=expression(hat(italic(w))^(2)),
                              phi=expression(italic(phi)))) +
  ggtitle(title_1) + ylab("Estimates") + xlab(expression(bar(italic(x))^(1))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom") + scale_y_continuous(limits=c(0,3))

plot_ex_phi_2<-for_plot_ex_2_all %>%
  filter(method=="ACWE", legend %in% c("w1", "w2", "phi")) %>%
  ggplot(aes(x=x_value, color=legend)) +
  geom_line(aes(y=value), linewidth=1) +
  scale_color_manual(values=c("w1"="#F8766D", "w2"="#00BA38", "phi"="#619CFF"),
                     labels=c(w1=expression(hat(italic(w))^(1)),
                              w2=expression(hat(italic(w))^(2)),
                              phi=expression(italic(phi)))) +
  ggtitle(title_2) + ylab("Estimates") + xlab(expression(bar(italic(x))^(1))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom") + scale_y_continuous(limits=c(0,3))

plot_ex_phi_3<-for_plot_ex_3_all %>%
  filter(method=="ACWE", legend %in% c("w1", "w2", "phi")) %>%
  ggplot(aes(x=x_value, color=legend)) +
  geom_line(aes(y=value), linewidth=1) +
  scale_color_manual(values=c("w1"="#F8766D", "w2"="#00BA38", "phi"="#619CFF"),
                     labels=c(w1=expression(hat(italic(w))^(1)),
                              w2=expression(hat(italic(w))^(2)),
                              phi=expression(italic(phi)))) +
  ggtitle(title_3) + ylab("Estimates") + xlab(expression(bar(italic(x))^(1))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom") + scale_y_continuous(limits=c(0,3))

plot_ex_phi_4<-for_plot_ex_4_all %>%
  filter(method=="ACWE", legend %in% c("w1", "w2", "phi")) %>%
  ggplot(aes(x=x_value, color=legend)) +
  geom_line(aes(y=value), linewidth=1) +
  scale_color_manual(values=c("w1"="#F8766D", "w2"="#00BA38", "phi"="#619CFF"),
                     labels=c(w1=expression(hat(italic(w))^(1)),
                              w2=expression(hat(italic(w))^(2)),
                              phi=expression(italic(phi)))) +
  ggtitle(title_4) + ylab("Estimates") + xlab(expression(italic(n)[1])) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom") + scale_y_continuous(limits=c(0,1.3))

plot_ex_phi_5<-for_plot_ex_5_all %>%
  filter(method=="ACWE", legend %in% c("w1", "w2", "phi")) %>%
  ggplot(aes(x=x_value, color=legend)) +
  geom_line(aes(y=value), linewidth=1) +
  scale_color_manual(values=c("w1"="#F8766D", "w2"="#00BA38", "phi"="#619CFF"),
                     labels=c(w1=expression(hat(italic(w))^(1)),
                              w2=expression(hat(italic(w))^(2)),
                              phi=expression(italic(phi)))) +
  ggtitle(title_5) + ylab("Estimates") + xlab(expression(italic(n)[1])) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom") + scale_y_continuous(limits=c(0,1.3))

plot_ex_phi_6<-for_plot_ex_6_all %>%
  filter(method=="ACWE", legend %in% c("w1", "w2", "phi")) %>%
  ggplot(aes(x=x_value, color=legend)) +
  geom_line(aes(y=value), linewidth=1) +
  scale_color_manual(values=c("w1"="#F8766D", "w2"="#00BA38", "phi"="#619CFF"),
                     labels=c(w1=expression(hat(italic(w))^(1)),
                              w2=expression(hat(italic(w))^(2)),
                              phi=expression(italic(phi)))) +
  ggtitle(title_6) + ylab("Estimates") + xlab(expression(italic(n)[1])) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom") + scale_y_continuous(limits=c(0,1.3))

plot_ex_phi_all<-ggarrange(plot_ex_phi_1,
                           plot_ex_phi_4,
                           plot_ex_phi_2,
                           plot_ex_phi_5,
                           plot_ex_phi_3,
                           plot_ex_phi_6,
                           labels=c("#1", "#4",
                                    "#2", "#5",
                                    "#3", "#6"),
                           common.legend=T, legend="bottom", ncol=2, nrow=3)
