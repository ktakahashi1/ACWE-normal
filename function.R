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
cl<-makeCluster(detectCores()-1, type="PSOCK")

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

compute_mcESS<-function(v_delta, sigma, n_trt) {
  v_theta_cnt<-v_delta - (sigma^2/n_trt)
  mcESS<-sigma^2/v_theta_cnt
  return(mcESS=mcESS)
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

ACWE_w_CI<-function(n_s, mean_s, v_s, n_star, sigma, alpha, B, B_CI) {
  w_mat<-matrix(ncol=length(n_s), nrow=B_CI)
  CI<-matrix(ncol=length(n_s), nrow=2)
  mean_s_B<-mvrnorm(B_CI, mean_s, Sigma=diag(sigma/n_s, ncol=length(n_s), nrow=length(n_s)))
  
  for (b in 1:B_CI) {
    tmp<-ACWE(n_s=n_s, mean_s=mean_s_B[b,], v_s=v_s, n_star=n_star, sigma=sigma, alpha=alpha, B=B)
    w_mat[b,]<-tmp$weight
  }
  CI[1,]<-apply(w_mat, 2, function(x)quantile(x, probs=alpha, na.rm=T))
  CI[2,]<-apply(w_mat, 2, function(x)quantile(x, probs=1-alpha, na.rm=T))
  return(CI)
}

no_borrowing<-function(n_trt, mean_trt, n_cnt, mean_cnt, sigma, alpha) {
  
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

full_borrowing<-function(n_trt, mean_trt, n_s, mean_s, sigma, alpha) {
  
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
                 iter=M, warmup=burnin, thin=1, chains=2, cores=parallel::detectCores()-1)
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
    
    # Proposal distributions
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
  omega_estimated<-apply(post_mat[,2:(S+1)], MARGIN=2, FUN=mean)
  
  v_theta<-post_mat[,1] %>% var()
  v_theta_ind<-(sigma^2)/n_s[1]
  ESS<-n_s[1]*v_theta_ind/v_theta
  return(list(post_mat=post_mat, omega=omega_estimated, ESS=ESS))
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

objective_function_EBPP<-function(params, n_s, mean_s, v_s, sigma, eta) {
  S<-length(mean_s)-1
  n_ext<-n_s[-1]
  mean_ext<-mean_s[-1]
  v_ext<-v_s[-1]
  omega<-params[1:S]
  
  A<-(1/sigma^2)*sum(omega*n_ext) + 1/eta^2
  B<-(1/sigma^2)*sum(omega*n_ext*mean_ext)
  C<-(1/(2*sigma^2))*sum(omega*(n_ext*mean_ext^2 + (n_ext-1)*v_ext))
  
  A_plus<-(1/sigma^2)*(n_s[1] + sum(omega*n_ext)) + 1/eta^2
  B_plus<-(1/sigma^2)*(n_s[1]*mean_s[1] + sum(omega*n_ext*mean_ext))
  C_plus<-(1/(2*sigma^2))*(n_s[1]*mean_s[1]^2 + (n_s[1]-1)*v_s[1] + 
                             sum(omega*(n_ext*mean_ext^2 + (n_ext-1)*v_ext)))
  
  log_p_omega<-0.5*log(A) - 0.5*log(A_plus) + 0.5*((B_plus^2/A_plus) - (B^2/A))
  
  return(-log_p_omega)
}

estimate_omega_EBPP<-function(n_s, mean_s, v_s, sigma, eta) {
  S<-length(mean_s)-1
  result<-nloptr(x0=rep(0.5,S), eval_f=objective_function_EBPP,
                 lb=rep(0,S), ub=rep(1,S),
                 opts=list(algorithm="NLOPT_LN_BOBYQA", maxeval=1000,
                           xtol_rel=1e-8, ftol_rel=1e-8),
                 n_s=n_s, mean_s=mean_s, v_s=v_s, sigma=sigma, eta=eta)
  
  list(omega_estimated=result$solution,
       log_marginal_L=-(result$objective),
       status=result$status,
       message=result$message)
}

EBPP<-function(n_s, mean_s, v_s, sigma, eta) {
  S<-length(n_s)-1
  omega<-estimate_omega_EBPP(n_s=n_s, mean_s=mean_s, v_s=v_s, sigma=sigma, eta=eta)[[1]] %>% unlist()
  
  post<-rep(NA, M+burnin)
  post[1]<-weighted.mean(x=mean_s, w=n_s)
  
  accept<-rep(NA, (M+burnin-1))
  
  for (i in 1:(length(post)-1)) {
    theta_cur<-post[i]
    
    # Proposal distributions
    theta_tmp<-rnorm(1, mean=theta_cur, sd=sigma*0.1)
    
    log_prior_cur<-wlogL(n_s=n_s[-1], mean_s=mean_s[-1], v_s=v_s[-1], 
                         w_s=omega, theta=theta_cur, sigma=sigma) +
      dnorm(x=theta_cur, mean=0, sd=eta, log=T)
    
    log_post_cur<-log_prior_cur + wlogL(n_s=n_s[1], mean_s=mean_s[1], v_s=v_s[1], 
                                        w_s=1, theta=theta_cur, sigma=sigma)
    
    log_prior_new<-wlogL(n_s=n_s[-1], mean_s=mean_s[-1], v_s=v_s[-1], 
                         w_s=omega, theta=theta_tmp, sigma=sigma) +
      dnorm(x=theta_tmp, mean=0, sd=eta, log=T)
    
    log_post_new<-log_prior_new + wlogL(n_s=n_s[1], mean_s=mean_s[1], v_s=v_s[1],
                                        w_s=1, theta=theta_tmp, sigma=sigma)
    
    p_accept<-min(c(1, exp(log_post_new-log_post_cur)))
    if (runif(1,0,1) < p_accept) {
      post[(i+1)]<-theta_tmp
      accept[i]<-1
    } else {
      post[(i+1)]<-theta_cur
      accept[i]<-0
    }
  }
  post<-post[(burnin+1):(M+burnin)]
  
  v_theta<-post %>% var()
  v_theta_ind<-(sigma^2)/n_s[1]
  ESS<-n_s[1]*(v_theta_ind/v_theta)
  accept_ratio<-mean(accept)
  
  return(list(post=post, omega=omega, ESS=ESS, accept_ratio=accept_ratio))
}

EBPP_2arm<-function(n_trt, mean_trt, n_s, mean_s, v_s, eta, sigma, alpha) {
  S<-length(n_s)-1
  MCMC_cnt<-EBPP(n_s=n_s, mean_s=mean_s, v_s=v_s, eta=eta, sigma=sigma)
  theta_cnt_post<-MCMC_cnt$post
  
  delta_post<-rnorm(M, mean_trt, sqrt(sigma^2/n_trt)) - theta_cnt_post
  delta_estimated<-delta_post %>% mean()
  SE<-delta_post %>% sd()
  LCL<-delta_post %>% quantile(alpha) %>% as.numeric()
  UCL<-delta_post %>% quantile(1-alpha) %>% as.numeric()
  omega_estimated<-MCMC_cnt$omega
  
  return(list(delta_estimated=delta_estimated,
              SE=SE,
              CI=c(LCL, UCL),
              omega_estimated=omega_estimated,
              ESS=MCMC_cnt$ESS))
}

EBPP_w_CI<-function(n_s, mean_s, v_s, sigma, eta, alpha, B_CI) {
  w_mat<-matrix(ncol=(length(n_s)-1), nrow=B_CI)
  CI<-matrix(ncol=(length(n_s)-1), nrow=2)
  mean_s_B<-mvrnorm(B_CI, mean_s, Sigma=diag(sigma/n_s, ncol=length(n_s), nrow=length(n_s)))
  
  for (b in 1:B_CI) {
    tmp<-estimate_omega_EBPP(n_s=n_s, mean_s=mean_s_B[b,], v_s=v_s, sigma=sigma, eta=eta)
    w_mat[b,]<-as.numeric(tmp$omega_estimated)
  }
  CI[1,]<-apply(w_mat, 2, function(x)quantile(x, probs=alpha, na.rm=T))
  CI[2,]<-apply(w_mat, 2, function(x)quantile(x, probs=1-alpha, na.rm=T))
  return(CI)
}

sim_ACWE<-function(K, n_trt, n_s, n_star, theta_trt, theta_cnt, mean_ext, sigma, alpha, B) {
  
  S<-length(n_s)-1
  ext_data_list<-lapply(1:S, function(s) {
    mat<-cbind(rep(s, n_s[s+1]), rnorm(n=n_s[s+1]))
    mat[,2]<-as.numeric(scale(mat[,2])) + mean_ext[s]
    mat
  })
  names(ext_data_list)<-paste("ext_data", 1:S, sep="")
  list2env(ext_data_list, envir=.GlobalEnv)
  ext_data<-do.call(rbind, ext_data_list)
  
  conduct_internal_procedure<-function(theta_cnt) {
    set.seed(seed)
    delta_estimated<-rep(NA,K)
    z<-rep(NA,K)
    z_no_borrowing<-rep(NA,K)
    LCL<-rep(NA,K)
    UCL<-rep(NA,K)
    weight_mat<-matrix(rep(NA, (1+S)*K), ncol=(1+S))
    ESS<-rep(NA,K)
    
    for (k in 1:K) {
      cur_data_trt<-cbind(rep(0, n_trt), rnorm(n=n_trt, mean=theta_trt, sd=sigma))
      cur_data_cnt<-cbind(rep(0, n_s[1]), rnorm(n=n_s[1], mean=theta_cnt, sd=sigma))
      
      data_cnt<-rbind(cur_data_cnt, ext_data)
      
      mean_s<-compute_mean_s(data_cnt)
      v_s<-compute_v_s(data_cnt)
      mean_trt<-mean(cur_data_trt[,2])
      
      result<-ACWE_2arm(n_trt=n_trt, mean_trt=mean_trt, n_s=n_s, mean_s=mean_s, v_s=v_s,
                        n_star=n_star, sigma=sigma, alpha=alpha, B=B)
      
      delta_estimated[k]<-result$delta_estimated
      
      LCL[k]<-result$CI[1]
      UCL[k]<-result$CI[2]
      weight_mat[k,]<-result$weight
      ESS[k]<-result$ESS
      
      z[k]<-delta_estimated[k]/result$SE
      z_no_borrowing[k]<-(mean_trt - mean_s[1])/sqrt((sigma^2/n_trt) + (sigma^2/n_s[1]))
    }
    
    # Test
    reject_rate<-mean(pnorm(z, lower.tail=F)<=alpha)
    if (theta_trt==theta_cnt) {
      hypothesis<-"null" 
    } else {
      hypothesis<-"alternative"
    }
    
    # Estimation
    result_estimation<-estimation(delta_estimated, (theta_trt-theta_cnt))
    mcESS<-compute_mcESS(v_delta=result_estimation$SE^2, sigma=sigma, n_trt=n_trt)
    
    # Result
    result_all<-list(delta_estimated=mean(delta_estimated),
                     reject_rate=reject_rate,
                     hypothesis=hypothesis,
                     RMSE=result_estimation$RMSE,
                     bias=result_estimation$bias,
                     SE=result_estimation$SE,
                     mcESS=mcESS,
                     ESS=mean(ESS),
                     weight_mean=apply(weight_mat, MARGIN=2, FUN=mean),
                     
                     ESS_all=ESS,
                     delta_estimated_all=delta_estimated,
                     weight_all=weight_mat,
                     z=z)
    return(result_all)
  }
  
  result_null<-conduct_internal_procedure(theta_cnt=theta_cnt[1])
  alpha_no_borrowing<-result_null$reject_rate
  
  result_alt<-conduct_internal_procedure(theta_cnt=theta_cnt[2])
  
  return(list(result_null=result_null, result_alt=result_alt))
}

sim_no_borrowing<-function(K, n_trt, n_cnt, theta_trt, theta_cnt, sigma, alpha) {
  conduct_internal_procedure<-function(theta_cnt) {
    set.seed(seed)
    delta_estimated<-rep(NA,K)
    z<-rep(NA,K)
    LCL<-rep(NA,K)
    UCL<-rep(NA,K)

    for (k in 1:K) {
      cur_data_trt<-cbind(rep(0, n_trt), rnorm(n=n_trt, mean=theta_trt, sd=sigma))
      cur_data_cnt<-cbind(rep(0, n_cnt), rnorm(n=n_cnt, mean=theta_cnt, sd=sigma))
      
      result<-no_borrowing(n_trt=n_trt, mean_trt=mean(cur_data_trt[,2]),
                           n_cnt=n_cnt, mean_cnt=mean(cur_data_cnt[,2]), sigma=sigma, alpha=alpha)
      delta_estimated[k]<-result$delta_estimated
      
      LCL[k]<-result$CI[1]
      UCL[k]<-result$CI[2]
      
      z[k]<-delta_estimated[k]/result$SE
    }
    
    # Test
    reject_rate<-mean(pnorm(z, lower.tail=F)<=alpha)
    if (theta_trt==theta_cnt) {
      hypothesis<-"null"
    } else {
      hypothesis<-"alternative"
    }
    
    # Estimation
    result_estimation<-estimation(delta_estimated, (theta_trt-theta_cnt))
    mcESS<-compute_mcESS(v_delta=result_estimation$SE^2, sigma=sigma, n_trt=n_trt)
    
    # Result
    result_all<-list(delta_estimated=mean(delta_estimated),
                     reject_rate=reject_rate,
                     hypothesis=hypothesis,
                     RMSE=result_estimation$RMSE,
                     bias=result_estimation$bias,
                     SE=result_estimation$SE,
                     mcESS=mcESS,
                     
                     delta_estimated_all=delta_estimated,
                     z=z)
    return(result_all)
  }
  
  result_null<-conduct_internal_procedure(theta_cnt=theta_cnt[1])
  result_alt<-conduct_internal_procedure(theta_cnt=theta_cnt[2])
  
  return(list(result_null=result_null, result_alt=result_alt))
}

sim_full_borrowing<-function(K, n_trt, n_s, theta_trt, theta_cnt, mean_ext, sigma, alpha) {
  
  S<-length(n_s)-1
  ext_data_list<-lapply(1:S, function(s) {
    mat<-cbind(rep(s, n_s[s+1]), rnorm(n=n_s[s+1]))
    mat[,2]<-as.numeric(scale(mat[,2])) + mean_ext[s]
    mat
  })
  names(ext_data_list)<-paste("ext_data", 1:S, sep="")
  list2env(ext_data_list, envir=.GlobalEnv)
  ext_data<-do.call(rbind, ext_data_list)
  
  conduct_internal_procedure<-function(theta_cnt) {
    set.seed(seed)
    delta_estimated<-rep(NA,K)
    z<-rep(NA,K)
    z_no_borrowing<-rep(NA,K)
    LCL<-rep(NA,K)
    UCL<-rep(NA,K)
    
    for (k in 1:K) {
      cur_data_trt<-cbind(rep(0, n_trt), rnorm(n=n_trt, mean=theta_trt, sd=sigma))
      cur_data_cnt<-cbind(rep(0, n_s[1]), rnorm(n=n_s[1], mean=theta_cnt, sd=sigma))
      
      data_cnt<-rbind(cur_data_cnt, ext_data)
      
      mean_s<-compute_mean_s(data_cnt)
      v_s<-compute_v_s(data_cnt)
      mean_trt<-mean(cur_data_trt[,2])
      
      result<-full_borrowing(n_trt=n_trt, mean_trt=mean_trt, n_s=n_s, mean_s=mean_s, sigma=sigma, alpha=alpha)
      delta_estimated[k]<-result$delta_estimated
      LCL[k]<-result$CI[1]
      UCL[k]<-result$CI[2]
      
      z[k]<-delta_estimated[k]/result$SE
      z_no_borrowing[k]<-(mean_trt - mean_s[1])/sqrt((sigma^2/n_trt) + (sigma^2/n_s[1]))
    }
    
    # Test
    reject_rate<-mean(pnorm(z, lower.tail=F)<=alpha)
    if (theta_trt==theta_cnt) {
      hypothesis<-"null"
    } else {
      hypothesis<-"alternative"
    }
    
    # Estimation
    result_estimation<-estimation(delta_estimated, (theta_trt-theta_cnt))
    mcESS<-compute_mcESS(v_delta=result_estimation$SE^2, sigma=sigma, n_trt=n_trt)
    
    # Result
    result_all<-list(delta_estimated=mean(delta_estimated),
                     reject_rate=reject_rate,
                     hypothesis=hypothesis,
                     RMSE=result_estimation$RMSE,
                     bias=result_estimation$bias,
                     SE=result_estimation$SE,
                     mcESS=mcESS,

                     delta_estimated_all=delta_estimated,
                     z=z)
    return(result_all)
  }
  
  result_null<-conduct_internal_procedure(theta_cnt=theta_cnt[1])
  alpha_no_borrowing<-result_null$reject_rate
  
  result_alt<-conduct_internal_procedure(theta_cnt=theta_cnt[2])
  
  return(list(result_null=result_null, result_alt=result_alt))
}

sim_MAP<-function(K, n_trt, n_s, theta_trt, theta_cnt, mean_ext, 
                  sigma, alpha, sd_tau, n_component, robust, w_robust=NULL) {
  S<-length(n_s)-1
  delta_null<-theta_trt-theta_cnt[1]
  ext_data_list<-lapply(1:S, function(s) {
    mat<-cbind(rep(s, n_s[s+1]), rnorm(n=n_s[s+1]))
    mat[,2]<-as.numeric(scale(mat[,2])) + mean_ext[s]
    mat
  })
  names(ext_data_list)<-paste("ext_data", 1:S, sep="")
  list2env(ext_data_list, envir=.GlobalEnv)
  ext_data<-do.call(rbind, ext_data_list)
  
  conduct_internal_procedure<-function(theta_cnt) {
    set.seed(seed)
    delta_estimated<-rep(NA,K)
    z_no_borrowing<-rep(NA,K)
    LCL<-rep(NA,K)
    UCL<-rep(NA,K)
    ESS<-rep(NA,K)
    
    for (k in 1:K) {
      cur_data_trt<-cbind(rep(0, n_trt), rnorm(n=n_trt, mean=theta_trt, sd=sigma))
      cur_data_cnt<-cbind(rep(0, n_s[1]), rnorm(n=n_s[1], mean=theta_cnt, sd=sigma))
      
      data_cnt<-rbind(cur_data_cnt, ext_data)
      
      mean_s<-compute_mean_s(data_cnt)
      v_s<-compute_v_s(data_cnt)
      mean_trt<-mean(cur_data_trt[,2])
      
      result<-MAP(n_trt=n_trt, mean_trt=mean_trt, n_s=n_s, mean_s=mean_s, sigma=sigma, alpha=alpha,
                  sd_tau=sd_tau, n_component=n_component, robust=robust, w_robust=w_robust)
      
      delta_estimated[k]<-result$delta_estimated
      
      LCL[k]<-result$CI[1]
      UCL[k]<-result$CI[2]
      ESS[k]<-result$ESS
      
      z_no_borrowing[k]<-(mean_trt - mean_s[1])/sqrt((sigma^2/n_trt) + (sigma^2/n_s[1]))
    }
    
    # Test
    reject_rate<-mean(delta_null<=LCL)
    if (theta_trt==theta_cnt) {
      hypothesis<-"null"
    } else {
      hypothesis<-"alternative"
    }
    
    # Estimation
    result_estimation<-estimation(delta_estimated, (theta_trt-theta_cnt))
    mcESS<-compute_mcESS(v_delta=result_estimation$SE^2, sigma=sigma, n_trt=n_trt)
    
    # Result
    result_all<-list(delta_estimated=mean(delta_estimated),
                     reject_rate=reject_rate,
                     hypothesis=hypothesis,
                     RMSE=result_estimation$RMSE,
                     bias=result_estimation$bias,
                     SE=result_estimation$SE,
                     mcESS=mcESS,
                     ESS=mean(ESS),
                     
                     delta_estimated_all=delta_estimated,
                     ESS_all=ESS)
    return(result_all)
  }
  
  result_null<-conduct_internal_procedure(theta_cnt=theta_cnt[1])
  alpha_no_borrowing<-result_null$reject_rate
  
  result_alt<-conduct_internal_procedure(theta_cnt=theta_cnt[2])
  
  return(list(result_null=result_null, result_alt=result_alt))
}

sim_MPP<-function(K, n_trt, n_s, theta_trt, theta_cnt, mean_ext, sigma, alpha, a_omega, b_omega, eta) {
  S<-length(n_s)-1
  delta_null<-theta_trt-theta_cnt[1]
  ext_data_list<-lapply(1:S, function(s) {
    mat<-cbind(rep(s, n_s[s+1]), rnorm(n=n_s[s+1]))
    mat[,2]<-as.numeric(scale(mat[,2])) + mean_ext[s]
    mat
  })
  names(ext_data_list)<-paste("ext_data", 1:S, sep="")
  list2env(ext_data_list, envir=.GlobalEnv)
  ext_data<-do.call(rbind, ext_data_list)
  
  conduct_internal_procedure<-function(theta_cnt) {
    set.seed(seed)
    delta_estimated<-rep(NA,K)
    z_no_borrowing<-rep(NA,K)
    LCL<-rep(NA,K)
    UCL<-rep(NA,K)
    weight_mat<-matrix(NA, nrow=K, ncol=S)
    ESS<-rep(NA,K)
    
    for (k in 1:K) {
      cur_data_trt<-cbind(rep(0, n_trt), rnorm(n=n_trt, mean=theta_trt, sd=sigma))
      cur_data_cnt<-cbind(rep(0, n_s[1]), rnorm(n=n_s[1], mean=theta_cnt, sd=sigma))
      
      data_cnt<-rbind(cur_data_cnt, ext_data)
      
      mean_s<-compute_mean_s(data_cnt)
      v_s<-compute_v_s(data_cnt)
      mean_trt<-mean(cur_data_trt[,2])
      
      result<-MPP_2arm(n_trt=n_trt, mean_trt=mean_trt, n_s=n_s, mean_s=mean_s, v_s=v_s,
                       a_omega=a_omega, b_omega=b_omega, eta=eta, sigma=sigma, alpha=alpha)
      delta_estimated[k]<-result$delta_estimated
      
      LCL[k]<-result$CI[1]
      UCL[k]<-result$CI[2]
      weight_mat[k,]<-result$omega_estimated
      
      z_no_borrowing[k]<-(mean_trt - mean_s[1])/sqrt((sigma^2/n_trt) + (sigma^2/n_s[1]))
      ESS[k]<-result$ESS
    }
    
    # Test
    reject_rate<-mean(delta_null<=LCL)
    if (theta_trt==theta_cnt) {
      hypothesis<-"null"
    } else {
      hypothesis<-"alternative"
    }
    
    # Estimation
    result_estimation<-estimation(delta_estimated, (theta_trt-theta_cnt))
    mcESS<-compute_mcESS(v_delta=result_estimation$SE^2, sigma=sigma, n_trt=n_trt)
    
    # Result
    result_all<-list(delta_estimated=mean(delta_estimated),
                     reject_rate=reject_rate,
                     hypothesis=hypothesis,
                     RMSE=result_estimation$RMSE,
                     bias=result_estimation$bias,
                     SE=result_estimation$SE,
                     mcESS=mcESS,
                     ESS=mean(ESS),
                     weight_mean=apply(weight_mat, MARGIN=2, FUN=mean),
                     
                     delta_estimated_all=delta_estimated,
                     ESS_all=ESS,
                     weight_all=weight_mat)
    return(result_all)
  }
  
  result_null<-conduct_internal_procedure(theta_cnt=theta_cnt[1])
  alpha_no_borrowing<-result_null$reject_rate
  
  result_alt<-conduct_internal_procedure(theta_cnt=theta_cnt[2])
  
  return(list(result_null=result_null, result_alt=result_alt))
}

sim_EBPP<-function(K, n_trt, n_s, theta_trt, theta_cnt, mean_ext, sigma, alpha, eta) {
  S<-length(n_s)-1
  delta_null<-theta_trt-theta_cnt[1]
  ext_data_list<-lapply(1:S, function(s) {
    mat<-cbind(rep(s, n_s[s+1]), rnorm(n=n_s[s+1]))
    mat[,2]<-as.numeric(scale(mat[,2])) + mean_ext[s]
    mat
  })
  names(ext_data_list)<-paste("ext_data", 1:S, sep="")
  list2env(ext_data_list, envir=.GlobalEnv)
  ext_data<-do.call(rbind, ext_data_list)
  
  conduct_internal_procedure<-function(theta_cnt) {
    set.seed(seed)
    delta_estimated<-rep(NA,K)
    z_no_borrowing<-rep(NA,K)
    LCL<-rep(NA,K)
    UCL<-rep(NA,K)
    weight_mat<-matrix(NA, nrow=K, ncol=S)
    ESS<-rep(NA,K)
    
    for (k in 1:K) {
      cur_data_trt<-cbind(rep(0, n_trt), rnorm(n=n_trt, mean=theta_trt, sd=sigma))
      cur_data_cnt<-cbind(rep(0, n_s[1]), rnorm(n=n_s[1], mean=theta_cnt, sd=sigma))
      
      data_cnt<-rbind(cur_data_cnt, ext_data)
      
      mean_s<-compute_mean_s(data_cnt)
      v_s<-compute_v_s(data_cnt)
      mean_trt<-mean(cur_data_trt[,2])
      
      result<-EBPP_2arm(n_trt=n_trt, mean_trt=mean_trt, n_s=n_s, mean_s=mean_s, v_s=v_s,
                       eta=eta, sigma=sigma, alpha=alpha)
      delta_estimated[k]<-result$delta_estimated
      
      LCL[k]<-result$CI[1]
      UCL[k]<-result$CI[2]
      weight_mat[k,]<-result$omega_estimated
      
      z_no_borrowing[k]<-(mean_trt - mean_s[1])/sqrt((sigma^2/n_trt) + (sigma^2/n_s[1]))
      ESS[k]<-result$ESS
    }
    
    # Test
    reject_rate<-mean(delta_null<=LCL)
    if (theta_trt==theta_cnt) {
      hypothesis<-"null"
    } else {
      hypothesis<-"alternative"
    }
    
    # Estimation
    result_estimation<-estimation(delta_estimated, (theta_trt-theta_cnt))
    mcESS<-compute_mcESS(v_delta=result_estimation$SE^2, sigma=sigma, n_trt=n_trt)
    
    # Result
    result_all<-list(delta_estimated=mean(delta_estimated),
                     reject_rate=reject_rate,
                     hypothesis=hypothesis,
                     RMSE=result_estimation$RMSE,
                     bias=result_estimation$bias,
                     SE=result_estimation$SE,
                     mcESS=mcESS,
                     ESS=mean(ESS),
                     weight_mean=apply(weight_mat, MARGIN=2, FUN=mean),
                     
                     delta_estimated_all=delta_estimated,
                     ESS_all=ESS,
                     weight_all=weight_mat)
    return(result_all)
  }
  
  result_null<-conduct_internal_procedure(theta_cnt=theta_cnt[1])
  alpha_no_borrowing<-result_null$reject_rate
  
  result_alt<-conduct_internal_procedure(theta_cnt=theta_cnt[2])
  
  return(list(result_null=result_null, result_alt=result_alt))
}

sim_ACWE_all<-function(scenario, mean_ext_1, mean_ext, n_s) {
  map_dfr(mean_ext_1, function(m1) {
    mean_ext_i<-mean_ext
    mean_ext_i[1]<-m1
    
    result<-sim_ACWE(K=K, n_trt=n_trt, n_s=n_s, n_star=n_star, theta_trt=theta_trt,
                     theta_cnt=theta_cnt, mean_ext=mean_ext_i, sigma=sigma, alpha=alpha, B=B)
    
    conduct_internal_procedure<-function(x) {
      weight<-paste0("(", paste(formatC(x$weight_mean, format="f", digits=4), collapse=", "), ")")
      tibble(scenario=scenario,
             method=method,
             K=K,
             n_trt=n_trt,
             n_s=sprintf("(%s)", paste(n_s, collapse=", ")),
             n_star=n_star,
             theta_trt=theta_trt,
             theta_cnt=ifelse(x$hypothesis=="null", theta_cnt[1], theta_cnt[2]),
             mean_ext=sprintf("(%s)", paste(mean_ext_i, collapse=", ")),
             mean_ext_1=m1,
             delta_estimated=x$delta_estimated,
             reject_rate=x$reject_rate,
             hypothesis=x$hypothesis,
             RMSE=x$RMSE,
             bias=x$bias,
             SE=x$SE,
             mcESS=x$mcESS,
             ESS=x$ESS,
             weight=weight,
             alpha=alpha,
             remark=remark,
             seed=seed,
             date=Sys.Date())
    }
    bind_rows(conduct_internal_procedure(result$result_null),
              conduct_internal_procedure(result$result_alt))
  }) %>% arrange(theta_cnt)
}

sim_no_borrowing_all<-function(scenario, mean_ext_1, mean_ext, n_s) {
  map_dfr(mean_ext_1, function(m1) {
    mean_ext_i<-mean_ext
    mean_ext_i[1]<-m1
    
    result<-sim_no_borrowing(K=K, n_trt=n_trt, n_cnt=n_s[1], theta_trt=theta_trt,
                             theta_cnt=theta_cnt, sigma=sigma, alpha=alpha)
    
    conduct_internal_procedure<-function(x) {
      ESS<-n_s[1]
      tibble(scenario=scenario,
             method=method,
             K=K,
             n_trt=n_trt,
             n_s=sprintf("(%s)", paste(n_s, collapse=", ")),
             n_star=NA,
             theta_trt=theta_trt,
             theta_cnt=ifelse(x$hypothesis=="null", theta_cnt[1], theta_cnt[2]),
             mean_ext=sprintf("(%s)", paste(mean_ext_i, collapse=", ")),
             mean_ext_1=m1,
             delta_estimated=x$delta_estimated,
             reject_rate=x$reject_rate,
             hypothesis=x$hypothesis,
             RMSE=x$RMSE,
             bias=x$bias,
             SE=x$SE,
             mcESS=x$mcESS,
             ESS=ESS,
             weight=NA,
             alpha=alpha,
             remark=remark,
             seed=seed,
             date=Sys.Date())
    }
    bind_rows(conduct_internal_procedure(result$result_null),
              conduct_internal_procedure(result$result_alt))
  }) %>% arrange(theta_cnt)
}

sim_full_borrowing_all<-function(scenario, mean_ext_1, mean_ext, n_s) {
  map_dfr(mean_ext_1, function(m1) {
    mean_ext_i<-mean_ext
    mean_ext_i[1]<-m1
    
    result<-sim_full_borrowing(K=K, n_trt=n_trt, n_s=n_s, theta_trt=theta_trt, theta_cnt=theta_cnt,
                               mean_ext=mean_ext_i, sigma=sigma, alpha=alpha)
    
    conduct_internal_procedure<-function(x) {
      ESS<-sum(n_s)
      tibble(scenario=scenario,
             method=method,
             K=K,
             n_trt=n_trt,
             n_s=sprintf("(%s)", paste(n_s, collapse=", ")),
             n_star=NA,
             theta_trt=theta_trt,
             theta_cnt=ifelse(x$hypothesis=="null", theta_cnt[1], theta_cnt[2]),
             mean_ext=sprintf("(%s)", paste(mean_ext_i, collapse=", ")),
             mean_ext_1=m1,
             delta_estimated=x$delta_estimated,
             reject_rate=x$reject_rate,
             hypothesis=x$hypothesis,
             RMSE=x$RMSE,
             bias=x$bias,
             SE=x$SE,
             mcESS=x$mcESS,
             ESS=ESS,
             weight=NA,
             alpha=alpha,
             remark=remark,
             seed=seed,
             date=Sys.Date())
    }
    bind_rows(conduct_internal_procedure(result$result_null),
              conduct_internal_procedure(result$result_alt))
  }) %>% arrange(theta_cnt)
}

sim_MAP_all<-function(scenario, mean_ext_1, mean_ext, n_s) {
  map_dfr(mean_ext_1, function(m1) {
    mean_ext_i<-mean_ext
    mean_ext_i[1]<-m1
    
    result<-sim_MAP(K=K, n_trt=n_trt, n_s=n_s, theta_trt=theta_trt, theta_cnt=theta_cnt,
                    mean_ext=mean_ext_i, sigma=sigma, alpha=alpha, sd_tau=sd_tau,
                    n_component=n_component, robust=robust, w_robust=w_robust)
    
    conduct_internal_procedure<-function(x) {
      weight<-NA
      tibble(scenario=scenario,
             method=method,
             K=K,
             n_trt=n_trt,
             n_s=sprintf("(%s)", paste(n_s, collapse=", ")),
             n_star=NA,
             theta_trt=theta_trt,
             theta_cnt=ifelse(x$hypothesis=="null", theta_cnt[1], theta_cnt[2]),
             mean_ext=sprintf("(%s)", paste(mean_ext_i, collapse=", ")),
             mean_ext_1=m1,
             delta_estimated=x$delta_estimated,
             reject_rate=x$reject_rate,
             hypothesis=x$hypothesis,
             RMSE=x$RMSE,
             bias=x$bias,
             SE=x$SE,
             mcESS=x$mcESS,
             ESS=x$ESS,
             weight=weight,
             alpha=alpha,
             remark=remark,
             seed=seed,
             date=Sys.Date())
    }
    bind_rows(conduct_internal_procedure(result$result_null),
              conduct_internal_procedure(result$result_alt))
  }) %>% arrange(theta_cnt)
}

sim_MPP_all<-function(scenario, mean_ext_1, mean_ext, n_s) {
  map_dfr(mean_ext_1, function(m1) {
    mean_ext_i<-mean_ext
    mean_ext_i[1]<-m1
    
    result<-sim_MPP(K=K, n_trt=n_trt, n_s=n_s, theta_trt=theta_trt, theta_cnt=theta_cnt,
                    mean_ext=mean_ext_i, sigma=sigma, alpha=alpha, a_omega=a_omega,
                    b_omega=b_omega, eta=eta)
    
    conduct_internal_procedure<-function(x) {
      weight<-paste0("(", paste(formatC(x$weight_mean, format="f", digits=4), collapse=", "), ")")
      tibble(scenario=scenario,
             method=method,
             K=K,
             n_trt=n_trt,
             n_s=sprintf("(%s)", paste(n_s, collapse=", ")),
             n_star=NA,
             theta_trt=theta_trt,
             theta_cnt=ifelse(x$hypothesis=="null", theta_cnt[1], theta_cnt[2]),
             mean_ext=sprintf("(%s)", paste(mean_ext_i, collapse=", ")),
             mean_ext_1=m1,
             delta_estimated=x$delta_estimated,
             reject_rate=x$reject_rate,
             hypothesis=x$hypothesis,
             RMSE=x$RMSE,
             bias=x$bias,
             SE=x$SE,
             mcESS=x$mcESS,
             ESS=x$ESS,
             weight=weight,
             alpha=alpha,
             remark=remark,
             seed=seed,
             date=Sys.Date())
    }
    bind_rows(conduct_internal_procedure(result$result_null),
              conduct_internal_procedure(result$result_alt))
  }) %>% arrange(theta_cnt)
}

sim_EBPP_all<-function(scenario, mean_ext_1, mean_ext, n_s) {
  map_dfr(mean_ext_1, function(m1) {
    mean_ext_i<-mean_ext
    mean_ext_i[1]<-m1
    
    result<-sim_EBPP(K=K, n_trt=n_trt, n_s=n_s, theta_trt=theta_trt, theta_cnt=theta_cnt,
                    mean_ext=mean_ext_i, sigma=sigma, alpha=alpha, eta=eta)
    
    conduct_internal_procedure<-function(x) {
      weight<-paste0("(", paste(formatC(x$weight_mean, format="f", digits=4), collapse=", "), ")")
      tibble(scenario=scenario,
             method=method,
             K=K,
             n_trt=n_trt,
             n_s=sprintf("(%s)", paste(n_s, collapse=", ")),
             n_star=NA,
             theta_trt=theta_trt,
             theta_cnt=ifelse(x$hypothesis=="null", theta_cnt[1], theta_cnt[2]),
             mean_ext=sprintf("(%s)", paste(mean_ext_i, collapse=", ")),
             mean_ext_1=m1,
             delta_estimated=x$delta_estimated,
             reject_rate=x$reject_rate,
             hypothesis=x$hypothesis,
             RMSE=x$RMSE,
             bias=x$bias,
             SE=x$SE,
             mcESS=x$mcESS,
             ESS=x$ESS,
             weight=weight,
             alpha=alpha,
             remark=remark,
             seed=seed,
             date=Sys.Date())
    }
    bind_rows(conduct_internal_procedure(result$result_null),
              conduct_internal_procedure(result$result_alt))
  }) %>% arrange(theta_cnt)
}

clean_sim_result<-function(result) {
  
  n_subset<-result$seed %>% unique() %>% length()
  result<-result %>% mutate(SE2=SE^2)
  
  group_key<-c("scenario", "mean_ext_1", "theta_cnt")
  metrics<-c("delta_estimated", "reject_rate",
             "bias", "SE2", "ESS")
  
  mean_or_na<-function(x) {
    x<-as.numeric(x)
    x<-x[!is.na(x)]
    if(length(x)==0) return(NA_real_)
    mean(x)
  }
  
  only_if_unique<-function(x) {
    ux<-unique(x)
    ux<-ux[!is.na(ux)]
    if(length(ux)==1) return(ux)
    return(x[NA_integer_][1])
  }
  
  carry_cols<-setdiff(names(result), c(metrics, group_key, ".mc_mean"))
  
  summary_tbl<-result %>%
    group_by(across(all_of(group_key))) %>%
    summarise(
      across(all_of(metrics), ~mean_or_na(.x)),
      across(all_of(carry_cols), ~only_if_unique(.x)),
      .groups="drop") %>% select(all_of(names(result)), everything()) %>%
    mutate(SE=sqrt(SE2), K=n_subset*K, RMSE=sqrt(SE2+bias^2),
           mcESS=compute_mcESS(SE2, sigma, n_trt)) %>%
    select(-SE2) %>% arrange(scenario, theta_cnt)
}

create_plot_ex_all<-function(for_plot_ex, type, title) {
  # type=="w": weight
  # type=="wn": weighted sample size
  if (type=="w") {
    for_plot_ex<-for_plot_ex %>% filter(legend %in% c("w1", "w2"))
    y_max<-1
  } else if (type=="wn") {
    for_plot_ex<-for_plot_ex %>% filter(legend %in% c("wn1", "wn2"))
    y_max<-100
  }
  
  leg_method_plot<-for_plot_ex %>% 
    ggplot(aes(x=x_value, y=value, linetype=method, alpha=method, size=method)) +
    geom_line(linewidth=1) +
    scale_linetype_manual(values=c(ACWE="solid", MPP="dashed", EBPP="solid"),
                          limits=c("ACWE", "MPP", "EBPP"), name=NULL) +
    scale_alpha_manual(values=c(ACWE=1, MPP=1, EBPP=0.2),
                       limits=c("ACWE", "MPP", "EBPP"), name=NULL) +
    scale_size_manual(values=c(ACWE=1.2, MPP=1.0, EBPP=0.8),
                      limits=c("ACWE", "MPP", "EBPP"), name=NULL) +
    theme_void() +
    theme(legend.position="right",
          legend.title=element_blank(),
          legend.text=element_text(size=size_legend-3),
          legend.background=element_rect(fill="white", color="black", size=0.8, linetype="solid"),
          legend.box.margin=margin(2, 6, 2, 6),
          legend.margin=margin(2, 6, 2, 6),
          legend.key.height=unit(0.9, "lines"),
          legend.key.width=unit(2.6, "lines")) +
    guides(linetype=guide_legend(override.aes=list(linewidth=1.1)))
  
  gt<-ggplot_gtable(ggplot_build(leg_method_plot))
  leg_method_grob<-gt$grobs[[which(sapply(gt$grobs, function(x) x$name)=="guide-box")]]
  
  xr<-range(for_plot_ex$x_value, na.rm=T)
  
  if (for_plot_ex[1,1]=="mean_1") {
    x_label<-expression(bar(italic(x))^{(1)})
  }  else if (for_plot_ex[1,1]=="n1") {
    x_label<-expression(italic(n)[1])
  }
  
  if (type=="w") {
    p<-for_plot_ex %>% 
      ggplot(aes(x=x_value, y=value, color=legend,
                 linetype=method, alpha=method, size=method)) +
      geom_line(linewidth=1) +
      scale_color_manual(values=c("w1"="#F8766D", "w2"="#00BA38"),
                         breaks=c("w1", "w2"),
                         labels=c(w1=expression(hat(italic(w))^(1)),
                                  w2=expression(hat(italic(w))^(2)))) +
      scale_linetype_manual(values=c(ACWE="solid", MPP="dashed", EBPP="solid"),
                            limits=c("ACWE", "MPP", "EBPP"), name="Method") +
      scale_alpha_manual(values=c(ACWE=1, MPP=1, EBPP=0.2),
                         limits=c("ACWE", "MPP", "EBPP"), name="Method") +
      scale_size_manual(values=c(ACWE=1.2, MPP=1.0, EBPP=0.8),
                        limits=c("ACWE", "MPP", "EBPP"), name="Method") +
      ggtitle(title) +
      ylab("Estimates") +
      xlab(x_label) +
      theme(plot.title=element_text(hjust=0.5, size=15),
            axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=size_axistitle),
            axis.text.x=element_text(size=size_axistext),
            axis.text.y=element_text(size=size_axistext),
            legend.title=element_blank(),
            legend.text=element_text(size=size_legend),
            legend.position="bottom") +
      scale_y_continuous(limits=c(0, y_max)) +
      
      guides(linetype="none", alpha="none", size="none",
             color=guide_legend(keywidth=2.6)) +
      coord_cartesian(clip="off") +
      
      annotation_custom(grob=leg_method_grob,
                        xmin=xr[2]-0.22*diff(xr), xmax=xr[2]-0.4,
                        ymin=0.74, ymax=0.98)
  } else if (type=="wn") {
    p<-for_plot_ex %>% 
      ggplot(aes(x=x_value, y=value, color=legend,
                 linetype=method, alpha=method, size=method)) +
      geom_line(linewidth=1) +
      scale_color_manual(values=c("wn1"="#F8766D", "wn2"="#00BA38"),
                         breaks=c("wn1", "wn2"),
                         labels=c(wn1=expression(hat(italic(w))^{(1)}*italic(n)[1]),
                                  wn2=expression(hat(italic(w))^{(2)}*italic(n)[2]))) +
      scale_linetype_manual(values=c(ACWE="solid", MPP="dashed", EBPP="solid"),
                            limits=c("ACWE", "MPP", "EBPP"), name="Method") +
      scale_alpha_manual(values=c(ACWE=1, MPP=1, EBPP=0.3),
                         limits=c("ACWE", "MPP", "EBPP"), name="Method") +
      scale_size_manual(values=c(ACWE=1.2, MPP=1.0, EBPP=0.8),
                        limits=c("ACWE", "MPP", "EBPP"), name="Method") +
      ggtitle(title) +
      ylab("Estimates") +
      xlab(x_label) +
      theme(plot.title=element_text(hjust=0.5, size=15),
            axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=size_axistitle),
            axis.text.x=element_text(size=size_axistext),
            axis.text.y=element_text(size=size_axistext),
            legend.title=element_blank(),
            legend.text=element_text(size=size_legend),
            legend.position="bottom") +
      scale_y_continuous(limits=c(0, y_max)) +
      
      guides(linetype="none", alpha="none", size="none",
             color=guide_legend(keywidth=2.6)) +
      coord_cartesian(clip="off") +
      
      annotation_custom(grob=leg_method_grob,
                        xmin=xr[2]-0.22*diff(xr), xmax=xr[2]-0.4,
                        ymin=0.74*100, ymax=0.98*100)
  }
}
