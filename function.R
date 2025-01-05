## This is a file that contains all required functions.
rm(list=ls())
gc(reset=T)
options(digits=5)
options(scipen=999)

library(tidyverse)
library(MASS)
library(parallel)
library(nloptr)
library(conflicted)
library(ggpubr)
library(RBesT)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
theme_set(theme_classic())

estimation<-function(x,x_true) {
  RMSE<-mean((x-x_true)^2) %>% sqrt()
  bias<-mean(x)-x_true
  SE<-var(x) %>% sqrt()
  return(list(RMSE=RMSE, bias=bias, SE=SE))
}

compute_mean_s<-function(data) {
  aggregate(data[,2], by=list(data[,1]), function(x) mean(x))[,2]
}

compute_v_s<-function(data) {
  aggregate(data[,2], by=list(data[,1]), function(x) var(x))[,2]
}

compute_theta_star<-function(n_s, w_s, mean_s) {
  sum(n_s*w_s*mean_s) / sum(n_s*w_s)
}

compute_v_theta_star<-function(n_s, w_s, v_s) {
  sum(n_s*w_s^2*v_s)/sum(n_s*w_s)^2
}

wlogL<-function(n_s, mean_s, v_s, w_s, theta, sigma) {
  SSE_s<-v_s*(n_s-1)
  wlogL_components<-w_s * (-(n_s/2)*log(2*pi*sigma^2) - 
                             (n_s*(mean_s-theta)^2 + SSE_s) / (2*sigma^2) )
  wlogL<-sum(wlogL_components)
  return(wlogL)
}

moment_mixdist<-function(w, mean, var) {
  mix_mean<-sum(w*mean)
  mix_var<-sum(w*(var+mean^2))-mix_mean^2
  return(list(mix_mean=mix_mean,
              mix_var=mix_var))
}

constraint<-function(params, n_s, mean_s, v_s, n_star, sigma, phi) {
  S<-length(n_s)-1
  w<-params[1:S]
  w0<-1
  w_s<-c(w0, w)
  
  numerator<-sum(n_s*w_s)^2
  denominator<-sum(n_s*w_s^2)
  
  return(numerator/denominator - n_star*phi)
}

objective_function<-function(params, n_s, mean_s, v_s, n_star, sigma, phi) {
  S<-length(n_s)-1
  w<-params[1:S]
  w0<-1
  w_s<-c(w0, w)
  
  numerator<-sum(n_s*w_s*mean_s)
  denominator<-sum(n_s*w_s)
  theta_star<-numerator/denominator
  
  log_likelihood<-wlogL(n_s=n_s, mean_s=mean_s, v_s=v_s, w_s=w_s,
                        theta=theta_star, sigma=sigma)
  
  return(-log_likelihood)
}

compute_w_uniform<-function(n_s, n_star) {
  equation<-function(w) {
    numerator<-(n_s[1] + sum(n_s[-1]*w))^2
    denominator<-n_s[1] + sum(n_s[-1]*w^2)
    return(numerator/denominator - n_star)
  }
  
  result<-uniroot(equation, c(0,1))
  return(result$root)
}

estimate_theta_star<-function(n_s, mean_s, v_s, n_star, sigma, phi) {
  S<-length(n_s)-1
  w_init<-compute_w_uniform(n_s=n_s, n_star=n_star)
  w_init<-rep(w_init,S)
  lambda_init<-0
  init<-c(w_init, lambda_init)
  
  lower<-c(rep(0,S), -Inf)
  upper<-c(rep(1,S), Inf)
  
  result<-nloptr(x0=init, eval_f=objective_function, eval_g_eq=constraint,
                 lb=lower, ub=upper, opts=list("algorithm"="NLOPT_LN_COBYLA",
                                               "xtol_rel"=1.0e-8,
                                               "maxeval"=1000),
                 n_s=n_s, mean_s=mean_s, v_s=v_s, n_star=n_star, sigma=sigma, phi=phi)
  w_s<-c(1,result$solution[1:S])
  theta_star_estimated<-compute_theta_star(n_s, w_s, mean_s) 
  
  return(list(theta_star_estimated=theta_star_estimated,
              w_s=w_s))
}

ACWE<-function(n_s, mean_s, v_s, n_star, sigma, alpha, B) {
  S<-length(n_s)-1
  
  # Step 1
  Bsample<-mvrnorm(n=B, mu=mean_s, Sigma=diag((sigma^2)/n_s))
  Bsample<-split(Bsample, seq(nrow(Bsample)))
  phi<-1
  clusterExport(cl,
                varlist=c("n_s", "v_s", "n_star", "sigma", "phi",
                          "estimate_theta_star", "compute_w_uniform", "wlogL",
                          "nloptr", "objective_function", "constraint",
                          "compute_theta_star"),
                envir=environment())
  result<-parLapply(Bsample,
                    function(row) estimate_theta_star(mean_s=row, n_s=n_s, v_s=v_s, n_star=n_star, sigma=sigma, phi=1),
                    cl=cl)
  theta_star_boot<-map_dbl(result, "theta_star_estimated") %>% unname()
  v_theta_star_boot<-var(theta_star_boot)
  ESS_unadjusted<-sigma^2/v_theta_star_boot
  correction_factor<-n_star/ESS_unadjusted
  
  # Step 2
  result<-estimate_theta_star(n_s=n_s, mean_s=mean_s, v_s=v_s, n_star=n_star,
                              sigma=sigma, phi=correction_factor)
  
  theta_star_estimated<-result$theta_star_estimated
  w_s<-result$w_s
  v_theta_star<-sigma^2/n_star
  ESS<-sigma^2/v_theta_star
  LCL<-theta_star_estimated - qnorm(alpha, lower.tail=F)*sqrt(v_theta_star)
  UCL<-theta_star_estimated + qnorm(alpha, lower.tail=F)*sqrt(v_theta_star)
  
  return(list(theta_star_estimated=theta_star_estimated,
              SE=sqrt(v_theta_star),
              CI=c(LCL, UCL),
              weight=w_s,
              ESS=ESS,
              ESS_unadjusted=ESS_unadjusted,
              correction_factor=correction_factor))
}

ACWE_2arm<-function(n_trt, mean_trt, n_s, mean_s, v_s, n_star, sigma, alpha, B) {
  result_cnt<-ACWE(n_s=n_s, mean_s=mean_s, v_s=v_s, n_star=n_star,
                   sigma=sigma, alpha=alpha, B=B)
  delta_estimated<-mean_trt - result_cnt$theta_star_estimated
  v_theta_trt<-sigma^2/n_trt
  v_delta<-v_theta_trt + (result_cnt$SE)^2
  
  LCL<-delta_estimated - qnorm(alpha, lower.tail=F)*sqrt(v_delta)
  UCL<-delta_estimated + qnorm(alpha, lower.tail=F)*sqrt(v_delta)
  
  return(list(delta_estimated=delta_estimated,
              SE=sqrt(v_delta),
              CI=c(LCL, UCL),
              weight=result_cnt$weight,
              ESS=result_cnt$ESS))
}

no_pooling<-function(n_trt, mean_trt, n_cnt, mean_cnt, sigma, alpha) {
  
  v_theta_trt<-sigma^2/n_trt
  v_theta_cnt<-sigma^2/n_cnt
  
  delta_estimated<-mean_trt-mean_cnt
  v_delta<-v_theta_trt+v_theta_cnt
  
  LCL<-delta_estimated - qnorm(alpha, lower.tail=F)*sqrt(v_delta)
  UCL<-delta_estimated + qnorm(alpha, lower.tail=F)*sqrt(v_delta)
  
  return(list(delta_estimated=delta_estimated,
              SE=sqrt(v_delta),
              CI=c(LCL, UCL)))
}

full_pooling<-function(n_trt, mean_trt, n_s, mean_s, sigma, alpha) {
  
  mean_cnt<-weighted.mean(x=mean_s, w=n_s)
  v_theta_trt<-sigma^2/n_trt
  v_theta_cnt<-sigma^2/sum(n_s)
  
  delta_estimated<-mean_trt-mean_cnt
  v_delta<-v_theta_trt+v_theta_cnt
  
  LCL<-delta_estimated - qnorm(alpha, lower.tail=F)*sqrt(v_delta)
  UCL<-delta_estimated + qnorm(alpha, lower.tail=F)*sqrt(v_delta)
  
  return(list(delta_estimated=delta_estimated,
              SE=sqrt(v_delta),
              CI=c(LCL, UCL)))
}

MAP<-function(n_trt, mean_trt, n_s, mean_s, sigma, 
              alpha, sd_tau, n_component, robust, w_robust=NULL) {
  options(RBesT.MC.control=list(adapt_delta=0.999))
  
  ext_data<-tibble(s=1:(length(n_s)-1),
                   n=n_s[-1], mean=mean_s[-1], se=sqrt(sigma^2/n))
  
  map_mcmc<-gMAP(cbind(mean, se)~1 | s, weights=n, data=ext_data,
                 family=gaussian,
                 beta.prior=cbind(0,10000),
                 tau.dist="HalfNormal", tau.prior=cbind(0, sd_tau),
                 iter=M, warmup=burnin, thin=2, chains=1, cores=parallel::detectCores()-1)
  map_approx<-mixfit(map_mcmc, Nc=n_component)
  
  if (robust==T) {
    map_approx<-robustify(map_approx, weight=w_robust, mean=map_approx[2], sigma=sigma)
  }
  
  post_cnt<-postmix(map_approx, m=mean_s[1], se=sqrt(sigma^2/n_s[1]))
  
  prior_trt<-mixnorm(c(1,0,10000), param="ms")
  post_trt<-postmix(prior_trt, m=mean_trt, se=sqrt(sigma^2/n_trt))
  
  moment_theta0_cnt<-moment_mixdist(w=post_cnt[1,], mean=post_cnt[2,], var=post_cnt[3,]^2)
  v_theta0_cnt<-moment_theta0_cnt$mix_var
  v_theta0_cnt_ind<-sigma^2/n_s[1]
  v_theta_trt<-post_trt[3]^2
  
  delta_estimated<-post_trt[2] - moment_theta0_cnt$mix_mean
  v_delta<-v_theta_trt + v_theta0_cnt
  
  ESS<-n_s[1]*(v_theta0_cnt_ind)/(v_theta0_cnt)
  
  LCL<-qmixdiff(post_trt, post_cnt, p=alpha, lower.tail=T)
  UCL<-qmixdiff(post_trt, post_cnt, p=1-alpha)
  
  return(list(delta_estimated=delta_estimated,
              SE=sqrt(v_delta),
              CI=c(LCL, UCL),
              ESS=ESS))
}

log_g_omega<-function(n_ext, mean_ext, v_ext, omega, sigma, eta) {
  A<-(1/sigma^2)*sum(omega*n_ext) + 1/eta^2
  B<-(1/sigma^2)*sum(omega*n_ext*mean_ext)
  C<-(1/(2*sigma^2))*sum(omega*(n_ext*mean_ext^2 + (n_ext-1)*v_ext))
  log_g_omega<-sum(-(omega*n_ext/2)*log(2*pi*sigma^2)) - (1/2)*log(A*eta^2)+(B^2)/(2*A)-C
  return(log_g_omega=log_g_omega)
}

MPP<-function(n_s, mean_s, v_s, a_omega, b_omega, eta, sigma) {
  
  S<-length(n_s)-1
  post_mat<-matrix(NA, nrow=M+burnin, ncol=1+S)
  
  post_mat[1,1]<-weighted.mean(x=mean_s, w=n_s)
  post_mat[1,2:(S+1)]<-0.5
  
  for (i in 1:(nrow(post_mat)-1)) {
    theta_cur<-post_mat[i,1]
    omega_cur<-post_mat[i,2:(S+1)]
    
    ## Proposal distributions
    theta_tmp<-rnorm(1, mean=theta_cur, sd=sigma*0.1)
    omega_tmp<-rbeta(S, shape1=1, shape2=1)
    
    log_prior_cur<-wlogL(n_s=n_s[-1], mean_s=mean_s[-1], v_s=v_s[-1], 
                         w_s=omega_cur, theta=theta_cur, sigma=sigma) +
      sum(dbeta(x=omega_cur, shape1=a_omega, shape2=b_omega, log=T)) + 
      dnorm(x=theta_cur, mean=0, sd=eta, log=T) -
      log_g_omega(n_ext=n_s[-1], mean_ext=mean_s[-1], v_ext=v_s[-1],
                  omega=omega_cur, sigma=sigma, eta=eta)
    
    
    
    log_post_cur<-log_prior_cur + wlogL(n_s=n_s[1], mean_s=mean_s[1], v_s=v_s[1], 
                                        w_s=1, theta=theta_cur, sigma=sigma)
    
    log_prior_new<-wlogL(n_s=n_s[-1], mean_s=mean_s[-1], v_s=v_s[-1],
                         w_s=omega_tmp, theta=theta_tmp, sigma=sigma) +
      sum(dbeta(x=omega_tmp, shape1=a_omega, shape2=b_omega, log=T)) + 
      dnorm(x=theta_tmp, mean=0, sd=eta, log=T) -
      log_g_omega(n_ext=n_s[-1], mean_ext=mean_s[-1], v_ext=v_s[-1],
                  omega=omega_tmp, sigma=sigma, eta=eta)
    
    
    
    log_post_new<-log_prior_new + wlogL(n_s=n_s[1], mean_s=mean_s[1], v_s=v_s[1], 
                                        w_s=1, theta=theta_tmp, sigma=sigma)
    
    p_accept<-min(c(1, exp(log_post_new-log_post_cur)))
    if (runif(1,0,1) < p_accept) {
      post_mat[(i+1),1]<-theta_tmp
      post_mat[(i+1),2:(S+1)]<-omega_tmp
    } else {
      post_mat[(i+1),1]<-theta_cur
      post_mat[(i+1),2:(S+1)]<-omega_cur
    }
  }
  post_mat<-post_mat[(burnin+1):(M+burnin),]
  
  v_theta<-post_mat[,1] %>% var()
  v_theta_ind<-(sigma^2)/n_s[1]
  ESS<-n_s[1]*v_theta_ind/v_theta
  return(list(post_mat=post_mat, ESS=ESS))
}

MPP_2arm<-function(n_trt, mean_trt, n_s, mean_s, v_s, a_omega, b_omega, eta, sigma, alpha) {
  
  S<-length(n_s)-1
  MCMC_cnt<-MPP(n_s=n_s, mean_s=mean_s, v_s=v_s, a_omega=a_omega, b_omega=b_omega, eta=eta, sigma=sigma)
  theta_cnt_post<-MCMC_cnt$post_mat[,1]
  
  delta_post<-rnorm(M, mean_trt, sqrt(sigma^2/n_trt)) - theta_cnt_post
  delta_estimated<-delta_post %>% mean()
  SE<-delta_post %>% sd()
  LCL<-delta_post %>% quantile(alpha) %>% as.numeric()
  UCL<-delta_post %>% quantile(1-alpha) %>% as.numeric()
  omega_estimated<-apply(MCMC_cnt$post_mat[,2:(S+1)], MARGIN=2, FUN=mean)
  
  return(list(delta_estimated=delta_estimated,
              SE=SE,
              CI=c(LCL, UCL),
              omega_estimated=omega_estimated,
              ESS=MCMC_cnt$ESS))
}

sim_ACWE<-function(K, n_trt, n_s, n_star,
                   theta_trt, theta_cnt, theta_ext, sigma, alpha, B) {
  set.seed(seed)
  S<-length(n_s)-1
  delta_estimated<-rep(NA,K)
  z<-rep(NA,K)
  reject<-rep(NA,K)
  LCL<-rep(NA,K)
  UCL<-rep(NA,K)
  coverage<-rep(NA,K)
  ESS_all<-rep(NA,K)
  weight_mat<-matrix(rep(NA, (1+S)*K), ncol=(1+S))
  
  for (k in 1:K) {
    cur_data_trt<-cbind(rep(0, n_trt), rnorm(n=n_trt, mean=theta_trt, sd=sigma))
    cur_data_cnt<-cbind(rep(0, n_s[1]), rnorm(n=n_s[1], mean=theta_cnt, sd=sigma))
    
    ext_data_list<-lapply(1:S, function(s) {
      cbind(rep(s, n_s[s+1]), rnorm(n=n_s[s+1], mean=theta_ext[s], sd=sigma))
    })
    names(ext_data_list)<-paste("ext_data", 1:S, sep="")
    list2env(ext_data_list, envir=.GlobalEnv)
    ext_data<-do.call(rbind, ext_data_list)
    data_cnt<-rbind(cur_data_cnt, ext_data)
    
    mean_s<-compute_mean_s(data_cnt)
    v_s<-compute_v_s(data_cnt)
    mean_trt<-mean(cur_data_trt[,2])
    
    result<-ACWE_2arm(n_trt=n_trt, mean_trt=mean_trt, n_s=n_s, mean_s=mean_s, v_s=v_s,
                      n_star=n_star, sigma=sigma, alpha=alpha, B=B)
    
    delta_estimated[k]<-result$delta_estimated
    ESS_all[k]<-result$ESS
    
    LCL[k]<-result$CI[1]
    UCL[k]<-result$CI[2]
    weight_mat[k,]<-result$weight
    
    ## Test
    z[k]<-delta_estimated[k]/result$SE
    if (pnorm(z[k], lower.tail=F)<=alpha) {
      reject[k]<-1
    } else {
      reject[k]<-0
    }
    
    ## Coverage
    if (LCL[k]<=(theta_trt-theta_cnt) & (theta_trt-theta_cnt)<=UCL[k]) {
      coverage[k]<-1
    } else {
      coverage[k]<-0
    }
  }
  
  ## Estimation
  result_estimation<-estimation(delta_estimated, (theta_trt-theta_cnt))
  
  return(list(delta_estimated=mean(delta_estimated),
              reject_rate=mean(reject),
              RMSE=result_estimation$RMSE,
              bias=result_estimation$bias,
              SE=result_estimation$SE,
              coverage=mean(coverage),
              ESS=mean(ESS_all),
              weight_mean=apply(weight_mat, MARGIN=2, FUN=mean),
              
              delta_estimated_all=delta_estimated,
              weight_all=weight_mat,
              ESS_all=ESS_all,
              
              z=z)
  )
}

sim_no_pooling<-function(K, n_trt, n_cnt, theta_trt, theta_cnt, sigma, alpha) {
  set.seed(seed)
  delta_estimated<-rep(NA,K)
  z<-rep(NA,K)
  reject<-rep(NA,K)
  LCL<-rep(NA,K)
  UCL<-rep(NA,K)
  coverage<-rep(NA,K)
  
  for (k in 1:K) {
    cur_data_trt<-cbind(rep(0,n_trt), rnorm(n=n_trt, mean=theta_trt, sd=sigma))
    cur_data_cnt<-cbind(rep(0,n_cnt), rnorm(n=n_cnt, mean=theta_cnt, sd=sigma))
    
    result<-no_pooling(n_trt=n_trt, mean_trt=mean(cur_data_trt[,2]),
                       n_cnt=n_cnt, mean_cnt=mean(cur_data_cnt[,2]),
                       sigma=sigma, alpha=alpha)
    delta_estimated[k]<-result$delta_estimated
    
    LCL[k]<-result$CI[1]
    UCL[k]<-result$CI[2]
    
    ## Test
    z[k]<-delta_estimated[k]/result$SE
    if (pnorm(z[k], lower.tail=F)<=alpha) {
      reject[k]<-1
    } else {
      reject[k]<-0
    }
    
    ## Coverage
    if (LCL[k]<=(theta_trt-theta_cnt) & (theta_trt-theta_cnt)<=UCL[k]) {
      coverage[k]<-1
    } else {
      coverage[k]<-0
    }
  }
  
  ## Estimation
  result_estimation<-estimation(delta_estimated, (theta_trt-theta_cnt))
  
  
  return(list(delta_estimated=mean(delta_estimated),
              reject_rate=mean(reject),
              RMSE=result_estimation$RMSE,
              bias=result_estimation$bias,
              SE=result_estimation$SE,
              coverage=mean(coverage),
              
              delta_estimated_all=delta_estimated,
              z=z)
  )
}

sim_full_pooling<-function(K, n_trt, n_s, theta_trt, theta_cnt, theta_ext,
                           sigma, alpha) {
  set.seed(seed)
  S<-length(n_s)-1
  delta_estimated<-rep(NA,K)
  z<-rep(NA,K)
  reject<-rep(NA,K)
  LCL<-rep(NA,K)
  UCL<-rep(NA,K)
  coverage<-rep(NA,K)
  
  for (k in 1:K) {
    cur_data_trt<-cbind(rep(0,n_trt), rnorm(n=n_trt, mean=theta_trt, sd=sigma))
    cur_data_cnt<-cbind(rep(0,n_s[1]), rnorm(n=n_s[1], mean=theta_cnt, sd=sigma))
    
    ext_data_list<-lapply(1:S, function(s) {
      cbind(rep(s, n_s[s+1]), rnorm(n=n_s[s+1], mean=theta_ext[s], sd=sigma))
    })
    names(ext_data_list)<-paste("ext_data", 1:S, sep="")
    list2env(ext_data_list, envir=.GlobalEnv)
    ext_data<-do.call(rbind, ext_data_list)
    data_cnt<-rbind(cur_data_cnt, ext_data)
    
    mean_s<-compute_mean_s(data_cnt)
    v_s<-compute_v_s(data_cnt)
    
    result<-full_pooling(n_trt=n_trt,
                         mean_trt=mean(cur_data_trt[,2]),
                         n_s=n_s, mean_s=mean_s, sigma=sigma, alpha=alpha)
    delta_estimated[k]<-result$delta_estimated
    
    LCL[k]<-result$CI[1]
    UCL[k]<-result$CI[2]
    
    ## Test
    z[k]<-delta_estimated[k]/result$SE
    if (pnorm(z[k], lower.tail=F)<=alpha) {
      reject[k]<-1
    } else {
      reject[k]<-0
    }
    
    ## Coverage
    if (LCL[k]<=(theta_trt-theta_cnt) & (theta_trt-theta_cnt)<=UCL[k]) {
      coverage[k]<-1
    } else {
      coverage[k]<-0
    }
  }
  
  ## Estimation
  result_estimation<-estimation(delta_estimated, (theta_trt-theta_cnt))
  
  return(list(delta_estimated=mean(delta_estimated),
              reject_rate=mean(reject),
              RMSE=result_estimation$RMSE,
              bias=result_estimation$bias,
              SE=result_estimation$SE,
              coverage=mean(coverage),
              
              delta_estimated_all=delta_estimated,
              z=z)
  )
}

sim_MAP<-function(K, n_trt, n_s, theta_trt, theta_cnt, theta_ext, 
                  sigma, alpha, delta_null, sd_tau, n_component, robust, w_robust=NULL) {
  set.seed(seed)
  S<-length(n_s)-1
  delta_estimated<-rep(NA,K)
  reject<-rep(NA,K)
  LCL<-rep(NA,K)
  UCL<-rep(NA,K)
  coverage<-rep(NA,K)
  ESS<-rep(NA,K)
  
  for (k in 1:K) {
    cur_data_trt<-cbind(rep(0,n_trt), rnorm(n=n_trt, mean=theta_trt, sd=sigma))
    cur_data_cnt<-cbind(rep(0,n_s[1]), rnorm(n=n_s[1], mean=theta_cnt, sd=sigma))
    
    ext_data_list<-lapply(1:S, function(s) {
      cbind(rep(s, n_s[s+1]), rnorm(n=n_s[s+1], mean=theta_ext[s], sd=sigma))
    })
    names(ext_data_list)<-paste("ext_data", 1:S, sep="")
    list2env(ext_data_list, envir=.GlobalEnv)
    ext_data<-do.call(rbind, ext_data_list)
    data_cnt<-rbind(cur_data_cnt, ext_data)
    
    mean_s<-compute_mean_s(data_cnt)
    v_s<-compute_v_s(data_cnt)
    
    result<-MAP(n_trt=n_trt, mean_trt=mean(cur_data_trt[,2]), n_s=n_s,
                mean_s=mean_s, sigma=sigma, alpha=alpha, sd_tau=sd_tau,
                n_component=n_component, robust=robust, w_robust=w_robust)
    delta_estimated[k]<-result$delta_estimated
    
    LCL[k]<-result$CI[1]
    UCL[k]<-result$CI[2]
    
    ESS[k]<-result$ESS
    
    ## Test
    if(delta_null<=LCL[k]) {
      reject[k]<-1
    } else {
      reject[k]<-0
    }
    
    ## Coverage
    if (LCL[k]<=(theta_trt-theta_cnt) & (theta_trt-theta_cnt)<=UCL[k]) {
      coverage[k]<-1
    } else {
      coverage[k]<-0
    }
  }
  
  ## Estimation
  result_estimation<-estimation(delta_estimated, (theta_trt-theta_cnt))
  
  return(list(delta_estimated=mean(delta_estimated),
              reject_rate=mean(reject),
              RMSE=result_estimation$RMSE,
              bias=result_estimation$bias,
              SE=result_estimation$SE,
              coverage=mean(coverage),
              ESS=mean(ESS),
              
              delta_estimated_all=delta_estimated,
              ESS_all=ESS)
  )
}

sim_MPP<-function(K, n_trt, n_s, theta_trt, theta_cnt, theta_ext, 
                  sigma, alpha, delta_null, a_omega, b_omega, eta) {
  set.seed(seed)
  S<-length(n_s)-1
  delta_estimated<-rep(NA,K)
  reject<-rep(NA,K)
  LCL<-rep(NA,K)
  UCL<-rep(NA,K)
  coverage<-rep(NA,K)
  ESS<-rep(NA,K)
  weight_mat<-matrix(NA, nrow=K, ncol=S)
  
  for (k in 1:K) {
    cur_data_trt<-cbind(rep(0,n_trt), rnorm(n=n_trt, mean=theta_trt, sd=sigma))
    cur_data_cnt<-cbind(rep(0,n_s[1]), rnorm(n=n_s[1], mean=theta_cnt, sd=sigma))
    
    ext_data_list<-lapply(1:S, function(s) {
      cbind(rep(s, n_s[s+1]), rnorm(n=n_s[s+1], mean=theta_ext[s], sd=sigma))
    })
    names(ext_data_list)<-paste("ext_data", 1:S, sep="")
    list2env(ext_data_list, envir=.GlobalEnv)
    ext_data<-do.call(rbind, ext_data_list)
    data_cnt<-rbind(cur_data_cnt, ext_data)
    
    mean_s<-compute_mean_s(data_cnt)
    v_s<-compute_v_s(data_cnt)
    mean_trt<-mean(cur_data_trt[,2])
    v_trt<-var(cur_data_trt[,2])
    
    result<-MPP_2arm(n_trt=n_trt, mean_trt=mean_trt, n_s=n_s, mean_s=mean_s, v_s=v_s,
                     a_omega=a_omega, b_omega=b_omega, eta=eta, sigma=sigma, alpha=alpha)
    delta_estimated[k]<-result$delta_estimated
    
    LCL[k]<-result$CI[1]
    UCL[k]<-result$CI[2]
    weight_mat[k,]<-result$omega_estimated
    
    ESS[k]<-result$ESS
    
    ## Test
    if(delta_null<=LCL[k]) {
      reject[k]<-1
    } else {
      reject[k]<-0
    }
    
    ## Coverage
    if (LCL[k]<=(theta_trt-theta_cnt) & (theta_trt-theta_cnt)<=UCL[k]) {
      coverage[k]<-1
    } else {
      coverage[k]<-0
    }
  }
  
  ## Estimation
  result_estimation<-estimation(delta_estimated, (theta_trt-theta_cnt))
  
  return(list(delta_estimated=mean(delta_estimated),
              reject_rate=mean(reject),
              RMSE=result_estimation$RMSE,
              bias=result_estimation$bias,
              SE=result_estimation$SE,
              coverage=mean(coverage),
              ESS=mean(ESS),
              weight_mean=apply(weight_mat, MARGIN=2, FUN=mean),
              
              delta_estimated_all=delta_estimated,
              weight_all=weight_mat)
  )
}

sim_ACWE_all<-function(scenario, theta_ext, n_s) {
  theta_ext_str<-paste(theta_ext, collapse=", ")
  theta_ext_str<-sprintf("(%s)", theta_ext_str)
  n_ext_str<-paste(n_s[-1], collapse=", ")
  n_ext_str<-sprintf("(%s)", n_ext_str)
  
  master_sim<-tibble(
    scenario=rep(scenario, length(theta_cnt)),
    method=rep(method ,length(theta_cnt)),
    dist=rep(dist, length(theta_cnt)),
    
    K=rep(K, length(theta_cnt)),
    n_trt=rep(n_trt, length(theta_cnt)),
    n_cur_cnt=rep(n_s[1], length(theta_cnt)),
    n_ext=rep(n_ext_str, length(theta_cnt)),
    
    theta_trt=rep(theta_trt, length(theta_cnt)),
    theta_cnt=theta_cnt,
    theta_ext=rep(theta_ext_str, length(theta_cnt)),
    
    n_star=rep(n_star, length(theta_cnt)),
    
    alpha=rep(alpha, length(theta_cnt)),
    sigma=rep(sigma, length(theta_cnt)),
    remark=rep(remark, length(theta_cnt)),
    seed=rep(seed, length(theta_cnt)),
    date=rep(Sys.Date(), length(theta_cnt))
  )
  
  result_sim<-mapply(sim_ACWE, theta_cnt=master_sim$theta_cnt,
                     MoreArgs=list(K=master_sim$K[1],
                                   n_trt=master_sim$n_trt[1],
                                   n_s=n_s,
                                   n_star=master_sim$n_star[1],
                                   
                                   theta_trt=master_sim$theta_trt[1],
                                   theta_ext=theta_ext,
                                   sigma=master_sim$sigma[1],
                                   
                                   alpha=master_sim$alpha[1],
                                   B=B))
  tmp<-lapply(result_sim[8,], round, 3) %>% as.character()
  
  result_sim_cleaned<-tibble(delta_estimated=result_sim[1,] %>% unlist(use.names=F),
                             reject=result_sim[2,] %>% unlist(use.names=F),
                             RMSE=result_sim[3,] %>% unlist(use.names=F),
                             bias=result_sim[4,] %>% unlist(use.names=F),
                             SE=result_sim[5,] %>% unlist(use.names=F),
                             coverage=result_sim[6,] %>% unlist(use.names=F),
                             ESS=result_sim[7,] %>% unlist(use.names=F),
                             weight=substr(tmp, 2, nchar(tmp)) %>% unlist(use.names=F))
  
  result_sim_cleaned<-bind_cols(master_sim, result_sim_cleaned)
  result_sim_cleaned<-result_sim_cleaned %>% select(scenario, method, K, n_trt, n_cur_cnt, n_star, n_ext, dist, theta_trt, theta_cnt, theta_ext, delta_estimated, reject, RMSE, bias, SE, coverage, ESS, weight, alpha, remark, seed, date)
}

sim_no_pooling_all<-function(scenario, theta_ext, n_s) {
  theta_ext_str<-paste(theta_ext, collapse=", ")
  theta_ext_str<-sprintf("(%s)", theta_ext_str)
  n_ext_str<-paste(n_s[-1], collapse=", ")
  n_ext_str<-sprintf("(%s)", n_ext_str)
  
  master_sim<-tibble(
    scenario=rep(scenario, length(theta_cnt)),
    method=rep(method ,length(theta_cnt)),
    dist=rep(dist, length(theta_cnt)),
    
    K=rep(K, length(theta_cnt)),
    n_trt=rep(n_trt, length(theta_cnt)),
    n_cur_cnt=rep(n_s[1], length(theta_cnt)),
    n_ext=rep(n_ext_str, length(theta_cnt)),
    
    theta_trt=rep(theta_trt, length(theta_cnt)),
    theta_cnt=theta_cnt,
    theta_ext=rep(theta_ext_str, length(theta_cnt)),
    
    n_star=rep(n_star, length(theta_cnt)),
    
    alpha=rep(alpha, length(theta_cnt)),
    sigma=rep(sigma, length(theta_cnt)),
    remark=rep(remark, length(theta_cnt)),
    seed=rep(seed, length(theta_cnt)),
    date=rep(Sys.Date(), length(theta_cnt))
  )
  
  result_sim<-parallel::mcmapply(sim_no_pooling, theta_cnt=master_sim$theta_cnt,
                                 MoreArgs=list(K=master_sim$K[1],
                                               n_trt=master_sim$n_trt[1],
                                               n_cnt=master_sim$n_cur_cnt[1],
                                               
                                               theta_trt=master_sim$theta_trt[1],
                                               sigma=master_sim$sigma[1],
                                               alpha=master_sim$alpha[1]),
                                 mc.cores=parallel::detectCores()-1, mc.preschedule=T)
  
  result_sim_cleaned<-tibble(delta_estimated=result_sim[1,] %>% unlist(use.names=F),
                             reject=result_sim[2,] %>% unlist(use.names=F),
                             RMSE=result_sim[3,] %>% unlist(use.names=F),
                             bias=result_sim[4,] %>% unlist(use.names=F),
                             SE=result_sim[5,] %>% unlist(use.names=F),
                             coverage=result_sim[6,] %>% unlist(use.names=F),
                             ESS=master_sim$n_cur_cnt[1],
                             weight="-")
  
  result_sim_cleaned<-bind_cols(master_sim, result_sim_cleaned)
  result_sim_cleaned<-result_sim_cleaned %>% select(scenario, method, K, n_trt, n_cur_cnt, n_star, n_ext, dist, theta_trt, theta_cnt, theta_ext, delta_estimated, reject, RMSE, bias, SE, coverage, ESS, weight, alpha, remark, seed, date)
}

sim_full_pooling_all<-function(scenario, theta_ext, n_s) {
  theta_ext_str<-paste(theta_ext, collapse=", ")
  theta_ext_str<-sprintf("(%s)", theta_ext_str)
  n_ext_str<-paste(n_s[-1], collapse=", ")
  n_ext_str<-sprintf("(%s)", n_ext_str)
  
  master_sim<-tibble(
    scenario=rep(scenario, length(theta_cnt)),
    method=rep(method ,length(theta_cnt)),
    dist=rep(dist, length(theta_cnt)),
    
    K=rep(K, length(theta_cnt)),
    n_trt=rep(n_trt, length(theta_cnt)),
    n_cur_cnt=rep(n_s[1], length(theta_cnt)),
    n_ext=rep(n_ext_str, length(theta_cnt)),
    
    theta_trt=rep(theta_trt, length(theta_cnt)),
    theta_cnt=theta_cnt,
    theta_ext=rep(theta_ext_str, length(theta_cnt)),
    
    n_star=rep(n_star, length(theta_cnt)),
    
    alpha=rep(alpha, length(theta_cnt)),
    sigma=rep(sigma, length(theta_cnt)),
    remark=rep(remark, length(theta_cnt)),
    seed=rep(seed, length(theta_cnt)),
    date=rep(Sys.Date(), length(theta_cnt))
  )
  
  result_sim<-parallel::mcmapply(sim_full_pooling, theta_cnt=master_sim$theta_cnt,
                                 MoreArgs=list(K=master_sim$K[1],
                                               n_trt=master_sim$n_trt[1],
                                               n_s=n_s,
                                               
                                               theta_trt=master_sim$theta_trt[1],
                                               theta_ext=theta_ext,
                                               
                                               sigma=master_sim$sigma[1],
                                               alpha=master_sim$alpha[1]),
                                 mc.cores=parallel::detectCores()-1, mc.preschedule=T)
  
  result_sim_cleaned<-tibble(delta_estimated=result_sim[1,] %>% unlist(use.names=F),
                             reject=result_sim[2,] %>% unlist(use.names=F),
                             RMSE=result_sim[3,] %>% unlist(use.names=F),
                             bias=result_sim[4,] %>% unlist(use.names=F),
                             SE=result_sim[5,] %>% unlist(use.names=F),
                             coverage=result_sim[6,] %>% unlist(use.names=F),
                             ESS=sum(n_s),
                             weight="-")
  
  result_sim_cleaned<-bind_cols(master_sim, result_sim_cleaned)
  result_sim_cleaned<-result_sim_cleaned %>% select(scenario, method, K, n_trt, n_cur_cnt, n_star, n_ext, dist, theta_trt, theta_cnt, theta_ext, delta_estimated, reject, RMSE, bias, SE, coverage, ESS, weight, alpha, remark, seed, date)
}

sim_MAP_all<-function(scenario, theta_ext, n_s) {
  theta_ext_str<-paste(theta_ext, collapse=", ")
  theta_ext_str<-sprintf("(%s)", theta_ext_str)
  n_ext_str<-paste(n_s[-1], collapse=", ")
  n_ext_str<-sprintf("(%s)", n_ext_str)
  
  master_sim<-tibble(
    scenario=rep(scenario, length(theta_cnt)),
    method=rep(method ,length(theta_cnt)),
    dist=rep(dist, length(theta_cnt)),
    
    K=rep(K, length(theta_cnt)),
    n_trt=rep(n_trt, length(theta_cnt)),
    n_cur_cnt=rep(n_s[1], length(theta_cnt)),
    n_ext=rep(n_ext_str, length(theta_cnt)),
    
    theta_trt=rep(theta_trt, length(theta_cnt)),
    theta_cnt=theta_cnt,
    theta_ext=rep(theta_ext_str, length(theta_cnt)),
    
    n_star=rep(n_star, length(theta_cnt)),
    
    alpha=rep(alpha, length(theta_cnt)),
    sigma=rep(sigma, length(theta_cnt)),
    remark=rep(remark, length(theta_cnt)),
    seed=rep(seed, length(theta_cnt)),
    date=rep(Sys.Date(), length(theta_cnt))
  )
  
  result_sim<-parallel::mcmapply(sim_MAP, theta_cnt=master_sim$theta_cnt,
                                 MoreArgs=list(K=master_sim$K[1],
                                               n_trt=master_sim$n_trt[1],
                                               n_s=n_s,
                                               
                                               theta_trt=master_sim$theta_trt[1],
                                               theta_ext=theta_ext,
                                               
                                               sigma=master_sim$sigma[1],
                                               alpha=master_sim$alpha[1],
                                               
                                               delta_null=delta_null,
                                               sd_tau=sd_tau,
                                               n_component=n_component,
                                               robust=robust,
                                               w_robust=w_robust),
                                 mc.cores=parallel::detectCores()-1, mc.preschedule=F)
  
  result_sim_cleaned<-tibble(delta_estimated=result_sim[1,] %>% unlist(use.names=F),
                             reject=result_sim[2,] %>% unlist(use.names=F),
                             RMSE=result_sim[3,] %>% unlist(use.names=F),
                             bias=result_sim[4,] %>% unlist(use.names=F),
                             SE=result_sim[5,] %>% unlist(use.names=F),
                             coverage=result_sim[6,] %>% unlist(use.names=F),
                             ESS=result_sim[7,] %>% unlist(use.names=F),
                             weight="-")
  
  result_sim_cleaned<-bind_cols(master_sim, result_sim_cleaned)
  result_sim_cleaned<-result_sim_cleaned %>% select(scenario, method, K, n_trt, n_cur_cnt, n_star, n_ext, dist, theta_trt, theta_cnt, theta_ext, delta_estimated, reject, RMSE, bias, SE, coverage, ESS, weight, alpha, remark, seed, date)
}

sim_MPP_all<-function(scenario, theta_ext, n_s) {
  theta_ext_str<-paste(theta_ext, collapse=", ")
  theta_ext_str<-sprintf("(%s)", theta_ext_str)
  n_ext_str<-paste(n_s[-1], collapse=", ")
  n_ext_str<-sprintf("(%s)", n_ext_str)
  
  master_sim<-tibble(
    scenario=rep(scenario, length(theta_cnt)),
    method=rep(method ,length(theta_cnt)),
    dist=rep(dist, length(theta_cnt)),
    
    K=rep(K, length(theta_cnt)),
    n_trt=rep(n_trt, length(theta_cnt)),
    n_cur_cnt=rep(n_s[1], length(theta_cnt)),
    n_ext=rep(n_ext_str, length(theta_cnt)),
    
    theta_trt=rep(theta_trt, length(theta_cnt)),
    theta_cnt=theta_cnt,
    theta_ext=rep(theta_ext_str, length(theta_cnt)),
    
    n_star=rep(n_star, length(theta_cnt)),
    
    alpha=rep(alpha, length(theta_cnt)),
    sigma=rep(sigma, length(theta_cnt)),
    remark=rep(remark, length(theta_cnt)),
    seed=rep(seed, length(theta_cnt)),
    date=rep(Sys.Date(), length(theta_cnt))
  )
  
  result_sim<-mapply(sim_MPP, theta_cnt=master_sim$theta_cnt,
                     MoreArgs=list(K=master_sim$K[1],
                                   n_trt=master_sim$n_trt[1],
                                   n_s=n_s,
                                   theta_trt=master_sim$theta_trt[1],
                                   theta_ext=theta_ext,
                                   sigma=master_sim$sigma[1],
                                   alpha=master_sim$alpha[1],
                                   delta_null=delta_null,
                                   a_omega=a_omega,
                                   b_omega=b_omega,
                                   eta=eta))
  tmp<-lapply(result_sim[8,], round, 3) %>% as.character()
  
  result_sim_cleaned<-tibble(delta_estimated=result_sim[1,] %>% unlist(use.names=F),
                             reject=result_sim[2,] %>% unlist(use.names=F),
                             RMSE=result_sim[3,] %>% unlist(use.names=F),
                             bias=result_sim[4,] %>% unlist(use.names=F),
                             SE=result_sim[5,] %>% unlist(use.names=F),
                             coverage=result_sim[6,] %>% unlist(use.names=F),
                             ESS=result_sim[7,] %>% unlist(use.names=F),
                             weight=substr(tmp, 2, nchar(tmp)) %>% unlist(use.names=F))
  
  result_sim_cleaned<-bind_cols(master_sim, result_sim_cleaned)
  result_sim_cleaned<-result_sim_cleaned %>% select(scenario, method, K, n_trt, n_cur_cnt, n_star, n_ext, dist, theta_trt, theta_cnt, theta_ext, delta_estimated, reject, RMSE, bias, SE, coverage, ESS, weight, alpha, remark, seed, date)
}
