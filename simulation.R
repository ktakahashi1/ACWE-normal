## This is an execution file for the Monte Carlo simulations in the paper.
## Load "function" file first, and then execute following code to perform the analyses.

source(".../function.R")

## Common settings
dist<-"norm"
n_trt<-100
n_star<-100
theta_trt<-0.4
theta_cnt<-c(theta_trt, 0)
mean_ext_1<-seq(theta_cnt[2], theta_cnt[1], 0.1)

B<-1000
alpha<-0.025
power<-0.8
sigma<-1
M<-10000
burnin<-2000
eta<-10000

###########################################################################
## Main simulations
###########################################################################
scenario_sim_list<-list(
  list(scenario=1, mean_ext=c(NA, 0), n_s=c(50, 100, 100)),
  list(scenario=2, mean_ext=c(NA, 0.2), n_s=c(50, 100, 100)),
  list(scenario=3, mean_ext=c(NA, 0.4), n_s=c(50, 100, 100))
)

## ACWE
method<-"ACWE"
remark<-paste0("B=",B)
K<-50000
seed<-1
Sys.time()
t<-system.time(result_sim_ACWE_1<-map_dfr(scenario_sim_list, ~sim_ACWE_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_ACWE_1<-tibble(result_sim_ACWE_1, hour=t/3600)

seed<-2
Sys.time()
t<-system.time(result_sim_ACWE_2<-map_dfr(scenario_sim_list, ~sim_ACWE_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_ACWE_2<-tibble(result_sim_ACWE_2, hour=t/3600)

result_sim_ACWE<-bind_rows(result_sim_ACWE_1, result_sim_ACWE_2)
result_sim_ACWE<-clean_sim_result(result_sim_ACWE)

## no-borrowing
method<-"no-borrowing"
remark<-"-"
K<-100000
seed<-1
Sys.time()
t<-system.time(result_sim_no_borrowing<-map_dfr(scenario_sim_list, ~sim_no_borrowing_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_no_borrowing<-tibble(result_sim_no_borrowing, hour=t/3600)

## full-borrowing
method<-"full-borrowing"
remark<-"-"
K<-100000
seed<-1
Sys.time()
t<-system.time(result_sim_full_borrowing<-map_dfr(scenario_sim_list, ~sim_full_borrowing_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_full_borrowing<-tibble(result_sim_full_borrowing, hour=t/3600)

## R-MAP
method<-"R-MAP"
robust<-T
w_robust<-0.5
sd_tau<-0.1
n_component<-1
remark<-paste0("sd_tau=", sd_tau, ", ",
               "n_component=", n_component, ", ",
               "robust=", robust)
K<-1000
seed<-1
Sys.time()
t<-system.time(result_sim_RMAP_1<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_1<-tibble(result_sim_RMAP_1, hour=t/3600)

seed<-2
Sys.time()
t<-system.time(result_sim_RMAP_2<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_2<-tibble(result_sim_RMAP_2, hour=t/3600)

seed<-3
Sys.time()
t<-system.time(result_sim_RMAP_3<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_3<-tibble(result_sim_RMAP_3, hour=t/3600)

K<-2000
seed<-4
Sys.time()
t<-system.time(result_sim_RMAP_4<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_4<-tibble(result_sim_RMAP_4, hour=t/3600)

seed<-5
Sys.time()
t<-system.time(result_sim_RMAP_5<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_5<-tibble(result_sim_RMAP_5, hour=t/3600)

seed<-6
Sys.time()
t<-system.time(result_sim_RMAP_6<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_6<-tibble(result_sim_RMAP_6, hour=t/3600)

seed<-7
Sys.time()
t<-system.time(result_sim_RMAP_7<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_7<-tibble(result_sim_RMAP_7, hour=t/3600)

seed<-8
Sys.time()
t<-system.time(result_sim_RMAP_8<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_8<-tibble(result_sim_RMAP_8, hour=t/3600)

seed<-9
Sys.time()
t<-system.time(result_sim_RMAP_9<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_9<-tibble(result_sim_RMAP_9, hour=t/3600)

seed<-10
Sys.time()
t<-system.time(result_sim_RMAP_10<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_10<-tibble(result_sim_RMAP_10, hour=t/3600)

seed<-11
Sys.time()
t<-system.time(result_sim_RMAP_11<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_11<-tibble(result_sim_RMAP_11, hour=t/3600)

seed<-12
Sys.time()
t<-system.time(result_sim_RMAP_12<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_12<-tibble(result_sim_RMAP_12, hour=t/3600)

seed<-13
Sys.time()
t<-system.time(result_sim_RMAP_13<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_13<-tibble(result_sim_RMAP_13, hour=t/3600)

K<-1000
seed<-14
Sys.time()
t<-system.time(result_sim_RMAP_14<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_14<-tibble(result_sim_RMAP_14, hour=t/3600)

seed<-15
Sys.time()
t<-system.time(result_sim_RMAP_15<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_15<-tibble(result_sim_RMAP_15, hour=t/3600)

seed<-16
Sys.time()
t<-system.time(result_sim_RMAP_16<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_16<-tibble(result_sim_RMAP_16, hour=t/3600)

seed<-17
Sys.time()
t<-system.time(result_sim_RMAP_17<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_17<-tibble(result_sim_RMAP_17, hour=t/3600)

seed<-18
Sys.time()
t<-system.time(result_sim_RMAP_18<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_18<-tibble(result_sim_RMAP_18, hour=t/3600)

seed<-19
Sys.time()
t<-system.time(result_sim_RMAP_19<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_19<-tibble(result_sim_RMAP_19, hour=t/3600)

seed<-20
Sys.time()
t<-system.time(result_sim_RMAP_20<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_20<-tibble(result_sim_RMAP_20, hour=t/3600)

seed<-21
Sys.time()
t<-system.time(result_sim_RMAP_21<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_21<-tibble(result_sim_RMAP_21, hour=t/3600)

seed<-22
Sys.time()
t<-system.time(result_sim_RMAP_22<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_22<-tibble(result_sim_RMAP_22, hour=t/3600)

seed<-23
Sys.time()
t<-system.time(result_sim_RMAP_23<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_23<-tibble(result_sim_RMAP_23, hour=t/3600)

seed<-24
Sys.time()
t<-system.time(result_sim_RMAP_24<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_24<-tibble(result_sim_RMAP_24, hour=t/3600)

seed<-25
Sys.time()
t<-system.time(result_sim_RMAP_25<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_25<-tibble(result_sim_RMAP_25, hour=t/3600)

seed<-26
Sys.time()
t<-system.time(result_sim_RMAP_26<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_26<-tibble(result_sim_RMAP_26, hour=t/3600)

seed<-27
Sys.time()
t<-system.time(result_sim_RMAP_27<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_27<-tibble(result_sim_RMAP_27, hour=t/3600)

seed<-28
Sys.time()
t<-system.time(result_sim_RMAP_28<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_28<-tibble(result_sim_RMAP_28, hour=t/3600)

seed<-29
Sys.time()
t<-system.time(result_sim_RMAP_29<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_29<-tibble(result_sim_RMAP_29, hour=t/3600)

seed<-30
Sys.time()
t<-system.time(result_sim_RMAP_30<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_30<-tibble(result_sim_RMAP_30, hour=t/3600)

seed<-31
Sys.time()
t<-system.time(result_sim_RMAP_31<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_31<-tibble(result_sim_RMAP_31, hour=t/3600)

seed<-32
Sys.time()
t<-system.time(result_sim_RMAP_32<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_32<-tibble(result_sim_RMAP_32, hour=t/3600)

seed<-33
Sys.time()
t<-system.time(result_sim_RMAP_33<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_33<-tibble(result_sim_RMAP_33, hour=t/3600)

seed<-34
Sys.time()
t<-system.time(result_sim_RMAP_34<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_34<-tibble(result_sim_RMAP_34, hour=t/3600)

seed<-35
Sys.time()
t<-system.time(result_sim_RMAP_35<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_35<-tibble(result_sim_RMAP_35, hour=t/3600)

seed<-36
Sys.time()
t<-system.time(result_sim_RMAP_36<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_36<-tibble(result_sim_RMAP_36, hour=t/3600)

seed<-37
Sys.time()
t<-system.time(result_sim_RMAP_37<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_37<-tibble(result_sim_RMAP_37, hour=t/3600)

seed<-38
Sys.time()
t<-system.time(result_sim_RMAP_38<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_38<-tibble(result_sim_RMAP_38, hour=t/3600)

seed<-39
Sys.time()
t<-system.time(result_sim_RMAP_39<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_39<-tibble(result_sim_RMAP_39, hour=t/3600)

seed<-40
Sys.time()
t<-system.time(result_sim_RMAP_40<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_40<-tibble(result_sim_RMAP_40, hour=t/3600)

seed<-41
Sys.time()
t<-system.time(result_sim_RMAP_41<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_41<-tibble(result_sim_RMAP_41, hour=t/3600)

seed<-42
Sys.time()
t<-system.time(result_sim_RMAP_42<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_42<-tibble(result_sim_RMAP_42, hour=t/3600)

seed<-43
Sys.time()
t<-system.time(result_sim_RMAP_43<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_43<-tibble(result_sim_RMAP_43, hour=t/3600)

seed<-44
Sys.time()
t<-system.time(result_sim_RMAP_44<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_44<-tibble(result_sim_RMAP_44, hour=t/3600)

seed<-45
Sys.time()
t<-system.time(result_sim_RMAP_45<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_45<-tibble(result_sim_RMAP_45, hour=t/3600)

seed<-46
Sys.time()
t<-system.time(result_sim_RMAP_46<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_46<-tibble(result_sim_RMAP_46, hour=t/3600)

seed<-47
Sys.time()
t<-system.time(result_sim_RMAP_47<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_47<-tibble(result_sim_RMAP_47, hour=t/3600)

seed<-48
Sys.time()
t<-system.time(result_sim_RMAP_48<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_48<-tibble(result_sim_RMAP_48, hour=t/3600)

seed<-49
Sys.time()
t<-system.time(result_sim_RMAP_49<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_49<-tibble(result_sim_RMAP_49, hour=t/3600)

seed<-50
Sys.time()
t<-system.time(result_sim_RMAP_50<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_50<-tibble(result_sim_RMAP_50, hour=t/3600)

seed<-51
Sys.time()
t<-system.time(result_sim_RMAP_51<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_51<-tibble(result_sim_RMAP_51, hour=t/3600)

seed<-52
Sys.time()
t<-system.time(result_sim_RMAP_52<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_52<-tibble(result_sim_RMAP_52, hour=t/3600)

seed<-53
Sys.time()
t<-system.time(result_sim_RMAP_53<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_53<-tibble(result_sim_RMAP_53, hour=t/3600)

seed<-54
Sys.time()
t<-system.time(result_sim_RMAP_54<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_54<-tibble(result_sim_RMAP_54, hour=t/3600)

seed<-55
Sys.time()
t<-system.time(result_sim_RMAP_55<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_55<-tibble(result_sim_RMAP_55, hour=t/3600)

seed<-56
Sys.time()
t<-system.time(result_sim_RMAP_56<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_56<-tibble(result_sim_RMAP_56, hour=t/3600)

seed<-57
Sys.time()
t<-system.time(result_sim_RMAP_57<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_57<-tibble(result_sim_RMAP_57, hour=t/3600)

seed<-58
Sys.time()
t<-system.time(result_sim_RMAP_58<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_58<-tibble(result_sim_RMAP_58, hour=t/3600)

seed<-59
Sys.time()
t<-system.time(result_sim_RMAP_59<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_59<-tibble(result_sim_RMAP_59, hour=t/3600)

seed<-60
Sys.time()
t<-system.time(result_sim_RMAP_60<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_60<-tibble(result_sim_RMAP_60, hour=t/3600)

seed<-61
Sys.time()
t<-system.time(result_sim_RMAP_61<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_61<-tibble(result_sim_RMAP_61, hour=t/3600)

seed<-62
Sys.time()
t<-system.time(result_sim_RMAP_62<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_62<-tibble(result_sim_RMAP_62, hour=t/3600)

seed<-63
Sys.time()
t<-system.time(result_sim_RMAP_63<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_63<-tibble(result_sim_RMAP_63, hour=t/3600)

seed<-64
Sys.time()
t<-system.time(result_sim_RMAP_64<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_64<-tibble(result_sim_RMAP_64, hour=t/3600)

seed<-65
Sys.time()
t<-system.time(result_sim_RMAP_65<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_65<-tibble(result_sim_RMAP_65, hour=t/3600)

seed<-66
Sys.time()
t<-system.time(result_sim_RMAP_66<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_66<-tibble(result_sim_RMAP_66, hour=t/3600)

seed<-67
Sys.time()
t<-system.time(result_sim_RMAP_67<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_67<-tibble(result_sim_RMAP_67, hour=t/3600)

seed<-68
Sys.time()
t<-system.time(result_sim_RMAP_68<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_68<-tibble(result_sim_RMAP_68, hour=t/3600)

seed<-69
Sys.time()
t<-system.time(result_sim_RMAP_69<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_69<-tibble(result_sim_RMAP_69, hour=t/3600)

seed<-70
Sys.time()
t<-system.time(result_sim_RMAP_70<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_70<-tibble(result_sim_RMAP_70, hour=t/3600)

seed<-71
Sys.time()
t<-system.time(result_sim_RMAP_71<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_71<-tibble(result_sim_RMAP_71, hour=t/3600)

seed<-72
Sys.time()
t<-system.time(result_sim_RMAP_72<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_72<-tibble(result_sim_RMAP_72, hour=t/3600)

seed<-73
Sys.time()
t<-system.time(result_sim_RMAP_73<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_73<-tibble(result_sim_RMAP_73, hour=t/3600)

seed<-74
Sys.time()
t<-system.time(result_sim_RMAP_74<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_74<-tibble(result_sim_RMAP_74, hour=t/3600)

seed<-75
Sys.time()
t<-system.time(result_sim_RMAP_75<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_75<-tibble(result_sim_RMAP_75, hour=t/3600)

seed<-76
Sys.time()
t<-system.time(result_sim_RMAP_76<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_76<-tibble(result_sim_RMAP_76, hour=t/3600)

seed<-77
Sys.time()
t<-system.time(result_sim_RMAP_77<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_77<-tibble(result_sim_RMAP_77, hour=t/3600)

seed<-78
Sys.time()
t<-system.time(result_sim_RMAP_78<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_78<-tibble(result_sim_RMAP_78, hour=t/3600)

seed<-79
Sys.time()
t<-system.time(result_sim_RMAP_79<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_79<-tibble(result_sim_RMAP_79, hour=t/3600)

seed<-80
Sys.time()
t<-system.time(result_sim_RMAP_80<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_80<-tibble(result_sim_RMAP_80, hour=t/3600)

seed<-81
Sys.time()
t<-system.time(result_sim_RMAP_81<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_81<-tibble(result_sim_RMAP_81, hour=t/3600)

seed<-82
Sys.time()
t<-system.time(result_sim_RMAP_82<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_82<-tibble(result_sim_RMAP_82, hour=t/3600)

seed<-83
Sys.time()
t<-system.time(result_sim_RMAP_83<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_83<-tibble(result_sim_RMAP_83, hour=t/3600)

seed<-84
Sys.time()
t<-system.time(result_sim_RMAP_84<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_84<-tibble(result_sim_RMAP_84, hour=t/3600)

seed<-85
Sys.time()
t<-system.time(result_sim_RMAP_85<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_85<-tibble(result_sim_RMAP_85, hour=t/3600)

seed<-86
Sys.time()
t<-system.time(result_sim_RMAP_86<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_86<-tibble(result_sim_RMAP_86, hour=t/3600)

seed<-87
Sys.time()
t<-system.time(result_sim_RMAP_87<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_87<-tibble(result_sim_RMAP_87, hour=t/3600)

seed<-88
Sys.time()
t<-system.time(result_sim_RMAP_88<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_88<-tibble(result_sim_RMAP_88, hour=t/3600)

seed<-89
Sys.time()
t<-system.time(result_sim_RMAP_89<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_89<-tibble(result_sim_RMAP_89, hour=t/3600)

seed<-90
Sys.time()
t<-system.time(result_sim_RMAP_90<-map_dfr(scenario_sim_list, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_90<-tibble(result_sim_RMAP_90, hour=t/3600)

result_sim_RMAP<-bind_rows(mget(paste0("result_sim_RMAP_", 1:90)))
result_sim_RMAP<-clean_sim_result(result_sim_RMAP)

## MPP
method<-"MPP"
a_omega<-b_omega<-1
remark<-paste0("a_omega=", a_omega, ", ",
               "b_omega=", b_omega, ", ",
               "eta=", eta)
K<-10000
seed<-1
Sys.time()
t<-system.time(result_sim_MPP_1<-map_dfr(scenario_sim_list, ~sim_MPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_MPP_1<-tibble(result_sim_MPP_1, hour=t/3600)

seed<-2
Sys.time()
t<-system.time(result_sim_MPP_2<-map_dfr(scenario_sim_list, ~sim_MPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_MPP_2<-tibble(result_sim_MPP_2, hour=t/3600)

seed<-3
Sys.time()
t<-system.time(result_sim_MPP_3<-map_dfr(scenario_sim_list, ~sim_MPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_MPP_3<-tibble(result_sim_MPP_3, hour=t/3600)

seed<-4
Sys.time()
t<-system.time(result_sim_MPP_4<-map_dfr(scenario_sim_list, ~sim_MPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_MPP_4<-tibble(result_sim_MPP_4, hour=t/3600)

seed<-5
Sys.time()
t<-system.time(result_sim_MPP_5<-map_dfr(scenario_sim_list, ~sim_MPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_MPP_5<-tibble(result_sim_MPP_5, hour=t/3600)

seed<-6
Sys.time()
t<-system.time(result_sim_MPP_6<-map_dfr(scenario_sim_list, ~sim_MPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_MPP_6<-tibble(result_sim_MPP_6, hour=t/3600)

seed<-7
Sys.time()
t<-system.time(result_sim_MPP_7<-map_dfr(scenario_sim_list, ~sim_MPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_MPP_7<-tibble(result_sim_MPP_7, hour=t/3600)

seed<-8
Sys.time()
t<-system.time(result_sim_MPP_8<-map_dfr(scenario_sim_list, ~sim_MPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_MPP_8<-tibble(result_sim_MPP_8, hour=t/3600)

seed<-9
Sys.time()
t<-system.time(result_sim_MPP_9<-map_dfr(scenario_sim_list, ~sim_MPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_MPP_9<-tibble(result_sim_MPP_9, hour=t/3600)

seed<-10
Sys.time()
t<-system.time(result_sim_MPP_10<-map_dfr(scenario_sim_list, ~sim_MPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_MPP_10<-tibble(result_sim_MPP_10, hour=t/3600)

result_sim_MPP<-bind_rows(mget(paste0("result_sim_MPP_", 1:10)))
result_sim_MPP<-clean_sim_result(result_sim_MPP)

## EBPP
method<-"EBPP"
remark<-paste0("eta=", eta)
K<-10000
seed<-1
Sys.time()
t<-system.time(result_sim_EBPP_1<-map_dfr(scenario_sim_list, ~sim_EBPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_EBPP_1<-tibble(result_sim_EBPP_1, hour=t/3600)

seed<-2
Sys.time()
t<-system.time(result_sim_EBPP_2<-map_dfr(scenario_sim_list, ~sim_EBPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_EBPP_2<-tibble(result_sim_EBPP_2, hour=t/3600)

seed<-3
Sys.time()
t<-system.time(result_sim_EBPP_3<-map_dfr(scenario_sim_list, ~sim_EBPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_EBPP_3<-tibble(result_sim_EBPP_3, hour=t/3600)

seed<-4
Sys.time()
t<-system.time(result_sim_EBPP_4<-map_dfr(scenario_sim_list, ~sim_EBPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_EBPP_4<-tibble(result_sim_EBPP_4, hour=t/3600)

seed<-5
Sys.time()
t<-system.time(result_sim_EBPP_5<-map_dfr(scenario_sim_list, ~sim_EBPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_EBPP_5<-tibble(result_sim_EBPP_5, hour=t/3600)

seed<-6
Sys.time()
t<-system.time(result_sim_EBPP_6<-map_dfr(scenario_sim_list, ~sim_EBPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_EBPP_6<-tibble(result_sim_EBPP_6, hour=t/3600)

seed<-7
Sys.time()
t<-system.time(result_sim_EBPP_7<-map_dfr(scenario_sim_list, ~sim_EBPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_EBPP_7<-tibble(result_sim_EBPP_7, hour=t/3600)

seed<-8
Sys.time()
t<-system.time(result_sim_EBPP_8<-map_dfr(scenario_sim_list, ~sim_EBPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_EBPP_8<-tibble(result_sim_EBPP_8, hour=t/3600)

seed<-9
Sys.time()
t<-system.time(result_sim_EBPP_9<-map_dfr(scenario_sim_list, ~sim_EBPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_EBPP_9<-tibble(result_sim_EBPP_9, hour=t/3600)

seed<-10
Sys.time()
t<-system.time(result_sim_EBPP_10<-map_dfr(scenario_sim_list, ~sim_EBPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_EBPP_10<-tibble(result_sim_EBPP_10, hour=t/3600)

result_sim_EBPP<-bind_rows(mget(paste0("result_sim_EBPP_", 1:10)))
result_sim_EBPP<-clean_sim_result(result_sim_EBPP)

result_sim_all<-bind_rows(result_sim_ACWE,
                          result_sim_no_borrowing,
                          result_sim_full_borrowing,
                          result_sim_RMAP,
                          result_sim_MPP,
                          result_sim_EBPP)

#################################################################
## plot
size_axistitle<-18
size_axistext<-15
size_legend<-18
size_point<-0.5
label_method<-c("ACWE", "MPP", "no-borrowing", "EBPP", "full-borrowing", "R-MAP")
dpi<-800

# Power
plot_power_1<-
  result_sim_all %>% filter(scenario==1, theta_cnt==0) %>% 
  ggplot(aes(x=mean_ext_1, y=reject_rate, color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, 0.2)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  geom_hline(yintercept=power, linetype=3) +
  ggtitle(expression(bar(italic(x))^(2)* "= 0" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("Power") + 
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_power_2<-
  result_sim_all %>% filter(scenario==2, theta_cnt==0) %>% 
  ggplot(aes(x=mean_ext_1, y=reject_rate, color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, 0.2)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  geom_hline(yintercept=power, linetype=3) +
  ggtitle(expression(bar(italic(x))^(2)* "= 0.2" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("Power") + 
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_power_3<-
  result_sim_all %>% filter(scenario==3, theta_cnt==0) %>% 
  ggplot(aes(x=mean_ext_1, y=reject_rate, color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, 0.2)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  geom_hline(yintercept=power, linetype=3) +
  ggtitle(expression(bar(italic(x))^(2)* "= 0.4" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("Power") + 
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

# TIER
plot_TIER_1<-
  result_sim_all %>% filter(scenario==1, theta_cnt==0.4) %>% 
  ggplot(aes(x=mean_ext_1, y=reject_rate, color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, 0.2)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  geom_hline(yintercept=alpha, linetype=3) +
  ggtitle(expression(bar(italic(x))^(2)* "= 0" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("Type I error rate") + 
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_TIER_2<-
  result_sim_all %>% filter(scenario==2, theta_cnt==0.4) %>% 
  ggplot(aes(x=mean_ext_1, y=reject_rate, color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, 0.2)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  geom_hline(yintercept=alpha, linetype=3) +
  ggtitle(expression(bar(italic(x))^(2)* "= 0.2" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("Type I error rate") + 
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_TIER_3<-
  result_sim_all %>% filter(scenario==3, theta_cnt==0.4) %>% 
  ggplot(aes(x=mean_ext_1, y=reject_rate, color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, 0.2)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  geom_hline(yintercept=alpha, linetype=3) +
  ggtitle(expression(bar(italic(x))^(2)* "= 0.4" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("Type I error rate") + 
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_power_TIER_all<-ggarrange(plot_power_1, plot_TIER_1,
                               plot_power_2, plot_TIER_2,
                               plot_power_3, plot_TIER_3,
                               labels=c("#1", "#1",
                                        "#2", "#2",
                                        "#3", "#3"),
                               common.legend=T, legend="bottom", ncol=2, nrow=3)

# Contour plot
tmp<-c("ACWE", "no-borrowing", "full-borrowing", "MPP", "EBPP", "R-MAP")
tmp_power<-result_sim_all %>% filter(theta_cnt==0) %>% select(scenario, method, mean_ext_1, reject_rate) %>% rename(power=reject_rate)
tmp_TIER<-result_sim_all %>% filter(theta_cnt==0.4) %>% select(scenario, method, mean_ext_1, reject_rate) %>% rename(TIER=reject_rate)
for_contourplot<-bind_cols(tmp_power, tmp_TIER %>% select(TIER)) %>% 
  mutate(mean_ext_2=if_else(scenario==1, 0,
                            if_else(scenario==2, 0.2, 0.4)),
         method=factor(method,levels=tmp))

contourplot_power<-
  ggplot(for_contourplot,
         aes(x=mean_ext_1, y=mean_ext_2, z=power)) +
  geom_contour_filled(aes(fill=after_stat(level))) +
  facet_wrap(~method) +
  scale_fill_viridis_d(name="") +
  ggtitle("Power") +
  xlab(expression(bar(italic(x))^(1))) +
  ylab(expression(bar(italic(x))^(2))) +
  theme_minimal(base_size=14) +
  theme(plot.title=element_text(hjust=0.5, size=15))

contourplot_TIER<-
  ggplot(for_contourplot,
         aes(x=mean_ext_1, y=mean_ext_2, z=TIER)) +
  geom_contour_filled(aes(fill=after_stat(level))) +
  facet_wrap(~method) +
  scale_fill_viridis_d(name="") +
  ggtitle("Type I error rate") +
  xlab(expression(bar(italic(x))^(1))) +
  ylab(expression(bar(italic(x))^(2))) +
  theme_minimal(base_size=14) +
  theme(plot.title=element_text(hjust=0.5, size=15))

plot_ct_all<-ggarrange(contourplot_power, contourplot_TIER,
                       common.legend=T, legend="bottom", ncol=2, nrow=1)

# Bias
plot_bias_alt_1<-result_sim_all %>% filter(scenario==1, theta_cnt==0) %>% 
  ggplot(aes(x=mean_ext_1, y=bias,
             color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(-0.4, 0.4), breaks=seq(-0.4, 0.4, 0.2)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  ggtitle(expression("(" * italic(theta)^(0) *  "," ~ bar(italic(x))^(2) *") = (0, 0)" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("Bias") +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_bias_alt_2<-result_sim_all %>% filter(scenario==2, theta_cnt==0) %>% 
  ggplot(aes(x=mean_ext_1, y=bias,
             color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(-0.4, 0.4), breaks=seq(-0.4, 0.4, 0.2)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  ggtitle(expression("(" * italic(theta)^(0) *  "," ~ bar(italic(x))^(2) *") = (0, 0.2)" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("Bias") +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_bias_alt_3<-result_sim_all %>% filter(scenario==3, theta_cnt==0) %>% 
  ggplot(aes(x=mean_ext_1, y=bias,
             color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(-0.4, 0.4), breaks=seq(-0.4, 0.4, 0.2)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  ggtitle(expression("(" * italic(theta)^(0) *  "," ~ bar(italic(x))^(2) *") = (0, 0.4)" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("Bias") +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_bias_null_1<-result_sim_all %>% filter(scenario==1, theta_cnt==0.4) %>% 
  ggplot(aes(x=mean_ext_1, y=bias,
             color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(-0.4, 0.4), breaks=seq(-0.4, 0.4, 0.2)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  ggtitle(expression("(" * italic(theta)^(0) *  "," ~ bar(italic(x))^(2) *") = (0.4, 0)" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("Bias") +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_bias_null_2<-result_sim_all %>% filter(scenario==2, theta_cnt==0.4) %>% 
  ggplot(aes(x=mean_ext_1, y=bias,
             color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(-0.4, 0.4), breaks=seq(-0.4, 0.4, 0.2)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  ggtitle(expression("(" * italic(theta)^(0) *  "," ~ bar(italic(x))^(2) *") = (0.4, 0.2)" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("Bias") +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_bias_null_3<-result_sim_all %>% filter(scenario==3, theta_cnt==0.4) %>% 
  ggplot(aes(x=mean_ext_1, y=bias,
             color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(-0.4, 0.4), breaks=seq(-0.4, 0.4, 0.2)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  ggtitle(expression("(" * italic(theta)^(0) *  "," ~ bar(italic(x))^(2) *") = (0.4, 0.4)" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("Bias") +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_bias_all<-ggarrange(plot_bias_alt_1, plot_bias_null_1,
                         plot_bias_alt_2, plot_bias_null_2,
                         plot_bias_alt_3, plot_bias_null_3,
                         labels=c("#1", "#1",
                                  "#2", "#2",
                                  "#3", "#3"),
                         common.legend=T, legend="bottom", ncol=2, nrow=3)

# ESS
plot_ESS_alt_1<-result_sim_all %>% filter(scenario==1, theta_cnt==0) %>% 
  ggplot(aes(x=mean_ext_1, y=ESS,
             color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0, 250), breaks=seq(0, 250, 50)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  ggtitle(expression("(" * italic(theta)^(0) *  "," ~ bar(italic(x))^(2) *") = (0, 0)" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("ESS") +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_ESS_alt_2<-result_sim_all %>% filter(scenario==2, theta_cnt==0) %>% 
  ggplot(aes(x=mean_ext_1, y=ESS,
             color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0, 250), breaks=seq(0, 250, 50)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  ggtitle(expression("(" * italic(theta)^(0) *  "," ~ bar(italic(x))^(2) *") = (0, 0.2)" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("ESS") +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_ESS_alt_3<-result_sim_all %>% filter(scenario==3, theta_cnt==0) %>% 
  ggplot(aes(x=mean_ext_1, y=ESS,
             color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0, 250), breaks=seq(0, 250, 50)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  ggtitle(expression("(" * italic(theta)^(0) *  "," ~ bar(italic(x))^(2) *") = (0, 0.4)" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("ESS") +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_ESS_null_1<-result_sim_all %>% filter(scenario==1, theta_cnt==0.4) %>% 
  ggplot(aes(x=mean_ext_1, y=ESS,
             color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0, 250), breaks=seq(0, 250, 50)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  ggtitle(expression("(" * italic(theta)^(0) *  "," ~ bar(italic(x))^(2) *") = (0.4, 0)" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("ESS") +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_ESS_null_2<-result_sim_all %>% filter(scenario==2, theta_cnt==0.4) %>% 
  ggplot(aes(x=mean_ext_1, y=ESS,
             color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0, 250), breaks=seq(0, 250, 50)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  ggtitle(expression("(" * italic(theta)^(0) *  "," ~ bar(italic(x))^(2) *") = (0.4, 0.2)" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("ESS") +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_ESS_null_3<-result_sim_all %>% filter(scenario==3, theta_cnt==0.4) %>% 
  ggplot(aes(x=mean_ext_1, y=ESS,
             color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0, 250), breaks=seq(0, 250, 50)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  ggtitle(expression("(" * italic(theta)^(0) *  "," ~ bar(italic(x))^(2) *") = (0.4, 0.4)" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("ESS") +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_ESS_all<-ggarrange(plot_ESS_alt_1, plot_ESS_null_1,
                        plot_ESS_alt_2, plot_ESS_null_2,
                        plot_ESS_alt_3, plot_ESS_null_3,
                        labels=c("#1", "#1",
                                 "#2", "#2",
                                 "#3", "#3"),
                        common.legend=T, legend="bottom", ncol=2, nrow=3)

###########################################################################
## Additional simulations
###########################################################################
scenario_sim_list_ad<-list(
  list(scenario=1, mean_ext=c(NA, 0, 0, 0), n_s=c(50, 100, 100, 100, 100)),
  list(scenario=2, mean_ext=c(NA, 0.4, 0.4, 0.4), n_s=c(50, 100, 100, 100, 100))
)

## ACWE
method<-"ACWE"
remark<-paste0("B=",B)
K<-5000
seed<-1
Sys.time()
t<-system.time(result_sim_ACWE_ad_1<-map_dfr(scenario_sim_list_ad, ~sim_ACWE_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_ACWE_ad_1<-tibble(result_sim_ACWE_ad_1, hour=t/3600)

seed<-2
Sys.time()
t<-system.time(result_sim_ACWE_ad_2<-map_dfr(scenario_sim_list_ad, ~sim_ACWE_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_ACWE_ad_2<-tibble(result_sim_ACWE_ad_2, hour=t/3600)

result_sim_ACWE_ad<-bind_rows(result_sim_ACWE_ad_1, result_sim_ACWE_ad_2)
result_sim_ACWE_ad<-clean_sim_result(result_sim_ACWE_ad)

## no-borrowing
method<-"no-borrowing"
remark<-"-"
K<-10000
seed<-1
Sys.time()
t<-system.time(result_sim_no_borrowing_ad<-map_dfr(scenario_sim_list_ad, ~sim_no_borrowing_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_no_borrowing_ad<-tibble(result_sim_no_borrowing_ad, hour=t/3600)

## full-borrowing
method<-"full-borrowing"
remark<-"-"
K<-10000
seed<-1
Sys.time()
t<-system.time(result_sim_full_borrowing_ad<-map_dfr(scenario_sim_list_ad, ~sim_full_borrowing_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_full_borrowing_ad<-tibble(result_sim_full_borrowing_ad, hour=t/3600)

## MPP
method<-"MPP"
a_omega<-b_omega<-1
remark<-paste0("a_omega=", a_omega, ", ",
               "b_omega=", b_omega, ", ",
               "eta=", eta)
K<-2500
seed<-1
Sys.time()
t<-system.time(result_sim_MPP_ad_1<-map_dfr(scenario_sim_list_ad, ~sim_MPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_MPP_ad_1<-tibble(result_sim_MPP_ad_1, hour=t/3600)

seed<-2
Sys.time()
t<-system.time(result_sim_MPP_ad_2<-map_dfr(scenario_sim_list_ad, ~sim_MPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_MPP_ad_2<-tibble(result_sim_MPP_ad_2, hour=t/3600)

seed<-3
Sys.time()
t<-system.time(result_sim_MPP_ad_3<-map_dfr(scenario_sim_list_ad, ~sim_MPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_MPP_ad_3<-tibble(result_sim_MPP_ad_3, hour=t/3600)

seed<-4
Sys.time()
t<-system.time(result_sim_MPP_ad_4<-map_dfr(scenario_sim_list_ad, ~sim_MPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_MPP_ad_4<-tibble(result_sim_MPP_ad_4, hour=t/3600)

result_sim_MPP_ad<-bind_rows(mget(paste0("result_sim_MPP_ad_", 1:4)))
result_sim_MPP_ad<-clean_sim_result(result_sim_MPP_ad)

## EBPP
method<-"EBPP"
remark<-"-"
K<-10000
seed<-1
Sys.time()
t<-system.time(result_sim_EBPP_ad<-map_dfr(scenario_sim_list_ad, ~sim_EBPP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_EBPP_ad<-tibble(result_sim_EBPP_ad, hour=t/3600)

## R-MAP
method<-"R-MAP"
robust<-T
w_robust<-0.5
sd_tau<-0.1
n_component<-1
remark<-paste0("sd_tau=", sd_tau, ", ",
               "n_component=", n_component, ", ",
               "robust=", robust)
K<-2500
seed<-1
Sys.time()
t<-system.time(result_sim_RMAP_ad_1<-map_dfr(scenario_sim_list_ad, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_ad_1<-tibble(result_sim_RMAP_ad_1, hour=t/3600)

seed<-2
Sys.time()
t<-system.time(result_sim_RMAP_ad_2<-map_dfr(scenario_sim_list_ad, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_ad_2<-tibble(result_sim_RMAP_ad_2, hour=t/3600)

seed<-3
Sys.time()
t<-system.time(result_sim_RMAP_ad_3<-map_dfr(scenario_sim_list_ad, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_ad_3<-tibble(result_sim_RMAP_ad_3, hour=t/3600)

seed<-4
Sys.time()
t<-system.time(result_sim_RMAP_ad_4<-map_dfr(scenario_sim_list_ad, ~sim_MAP_all(.x$scenario, mean_ext_1, .x$mean_ext, .x$n_s)))[[3]]
result_sim_RMAP_ad_4<-tibble(result_sim_RMAP_ad_4, hour=t/3600)

result_sim_RMAP_ad<-bind_rows(mget(paste0("result_sim_RMAP_ad_", 1:4)))
result_sim_RMAP_ad<-clean_sim_result(result_sim_RMAP_ad)

result_sim_ACWE_ad<-clean_sim_result(result_sim_ACWE_ad)
result_sim_RMAP_ad<-clean_sim_result(result_sim_RMAP_ad)
result_sim_MPP_ad<-clean_sim_result(result_sim_MPP_ad)
result_sim_EBPP_ad<-clean_sim_result(result_sim_EBPP_ad)

result_sim_ad_all<-bind_rows(result_sim_ACWE_ad,
                             result_sim_no_borrowing_ad,
                             result_sim_full_borrowing_ad,
                             result_sim_RMAP_ad,
                             result_sim_MPP_ad,
                             result_sim_EBPP_ad)

#################################################################
## plot
# Power
plot_power_ad_1<-
  result_sim_ad_all %>% filter(scenario==1, theta_cnt==0) %>% 
  ggplot(aes(x=mean_ext_1, y=reject_rate, color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, 0.2)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  geom_hline(yintercept=power, linetype=3) +
  ggtitle(expression("(" * bar(italic(x))^(2) *  "," ~ bar(italic(x))^(3) * "," * ~ bar(italic(x))^(4) *") = (0, 0, 0)" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("Power") + 
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_power_ad_2<-
  result_sim_ad_all %>% filter(scenario==2, theta_cnt==0) %>% 
  ggplot(aes(x=mean_ext_1, y=reject_rate, color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, 0.2)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  geom_hline(yintercept=power, linetype=3) +
  ggtitle(expression("(" * bar(italic(x))^(2) *  "," ~ bar(italic(x))^(3) * "," * ~ bar(italic(x))^(4) *") = (0.4, 0.4, 0.4)" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("Power") + 
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

# TIER
plot_TIER_ad_1<-
  result_sim_ad_all %>% filter(scenario==1, theta_cnt==0.4) %>% 
  ggplot(aes(x=mean_ext_1, y=reject_rate, color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, 0.2)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  geom_hline(yintercept=alpha, linetype=3) +
  ggtitle(expression("(" * bar(italic(x))^(2) *  "," ~ bar(italic(x))^(3) * "," * ~ bar(italic(x))^(4) *") = (0, 0, 0)" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("Type I error rate") + 
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_TIER_ad_2<-
  result_sim_ad_all %>% filter(scenario==2, theta_cnt==0.4) %>% 
  ggplot(aes(x=mean_ext_1, y=reject_rate, color=factor(method, levels=label_method),
             linetype=factor(method, levels=label_method))) +
  geom_point(size=size_point) + geom_line() +
  scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, 0.2)) +
  xlim(c(min(mean_ext_1), max(mean_ext_1))) +
  geom_hline(yintercept=alpha, linetype=3) +
  ggtitle(expression("(" * bar(italic(x))^(2) *  "," ~ bar(italic(x))^(3) * "," * ~ bar(italic(x))^(4) *") = (0.4, 0.4, 0.4)" )) +
  xlab(expression(bar(italic(x))^(1))) +
  ylab("Type I error rate") + 
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom")

plot_power_TIER_ad_all<-ggarrange(plot_power_ad_1, plot_TIER_ad_1,
                                  plot_power_ad_2, plot_TIER_ad_2,
                                  labels=c("#A1", "#A1",
                                           "#A2", "#A2"),
                                  common.legend=T, legend="bottom", ncol=2, nrow=2)