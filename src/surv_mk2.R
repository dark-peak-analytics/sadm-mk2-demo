
#library(survminer)
#library(survival)
#library(flexsurv)


source("./src/predict.flexsurvreg.R")

# load functions

# simulate survial data 
genWeibullSurvDat = function(
  cons = rnorm(1,0.00446820,0.00044682),
  gamma = rnorm(1,0.06082920,0.00608292),
  RR = 0.75,         # rr of int
  age_range = 0:100, # simulate over time 
  censor_age = 35,   # censor everone at
  n = 100,           # base sample n
  n_t = 100,         # int sample n
  seed = NULL){
  
  set.seed(seed)
  
  
  
# base  
  surv_probs = exp(-cons*(gamma^-1)*(exp(gamma*age_range)-1))
  
# treatment
  gamma = gamma * (RR)
  surv_probs_t = exp(-cons*(gamma^-1)*(exp(gamma*age_range)-1))
  
# survival time density function
  z = z_t = c(0)
  for(i in 2:length(surv_probs)){
    z = c(z,surv_probs[i-1]-surv_probs[i])
    z_t = c(z_t,surv_probs_t[i-1]-surv_probs_t[i])
  }
  
  # draws
  test_data = sample(x = age_range,size = n,replace = T,prob = z)
  test_data_t = sample(x = age_range,size = n_t,replace = T,prob = z_t)
  
  # survival df  
  surv_df = data.frame(treatment = c(rep(1,n),rep(2,n_t)),
                       survival_time = c(test_data,test_data_t),
                       event = T)
  surv_df$event[surv_df$survival_time>censor_age] = 0
  surv_df$survival_time[surv_df$survival_time>censor_age] = censor_age
  
  return(surv_df)
}
  

fitSurvDists = function(
  surv_df,    # surv data with cols: 'survival_time' and 'event' 
  dists = c("weibullph","gompertz","lognormal","llogis","exp"), # fit these models to the surv data
  times = 1:50,     # predict survival for these times
  linetype = NULL
  ){
  
  require(survival)
  require(flexsurv)
  require(purrr)
  
  res_list = c()
  surv_obj = Surv(time = surv_df$survival_time,event =surv_df$event,type = "right")
  for(d in dists){
    
    temp = flexsurvreg(formula = surv_obj~as.factor(treatment),data=surv_df, dist = d)
    
    for(intervention in unique(surv_df$treatment)){
      
      pred = predict.flexsurvreg(temp,times = times,newdata = data.frame(treatment = rep(intervention,length(times) )))
      res_list = rbind(res_list, cbind("time" = times, "est"= pred,treatment  = intervention, "dist" = d))
      
    }
    
    
    
  }
  res_list = data.frame(res_list)
  res_list$time = as.numeric(res_list$time)
  res_list$est = as.numeric(res_list$est)
  res_list$treatment = as.factor(res_list$treatment)
  
  if(is.null(linetype)){
    res_list$linetype = as.numeric(as.factor(res_list$dist))
  } else {
    stop("cols param not yet implemented!")
  }
  
  return(res_list)
}
 

 ######

# AT SERVER START
# generate survival data and fit models
# set.seed(2020)
# surv_df = genWeibullSurvDat(RR = 0.75,age_range = 0:100,censor_age = 100,n = 2000,n_t = 2000)
# surv_m_fitted = fitSurvDists(surv_df,times = 1:120)
# surv_obj_1 = Surv(time = surv_df$survival_time,event =surv_df$event,type = "right")
# surv_fit_1 = survfit(surv_obj_1~surv_df$treatment)
# plot_df <- surv_summary(surv_fit_1, data = surv_df)


# ggplot() +
#     geom_step(data = plot_df, aes(x = time, y = surv, col = treatment), size = 1) +
#     geom_line(data = surv_m_fitted, aes(x = time, y = est, col = as.factor(treatment), linetype = dist), alpha = 0.7, size = 1) +
#     NULL

######### user selects something

# age_range = 1:101
# dist = "llogis"
# psa_iterations = 1000

# # take user input and fit model
# selected_model = flexsurvreg(formula = surv_obj_1 ~ as.factor(treatment), data = surv_df, dist = dist)

# p_HS_SOC = lapply(1:psa_iterations, function(x) {
#     pred_SOC = predict.uncertain.flexsurvreg(selected_model, type = "hazard", times = age_range, newdata = data.frame(treatment = rep(1, length(age_range))))
# })
# p_HS_SOC = matrix(unlist(p_HS_SOC),ncol=psa_iterations, byrow=T)

# p_HS_TRT = lapply(1:psa_iterations, function(x) {
#     pred_TRT = predict.uncertain.flexsurvreg(selected_model, type = "hazard", times = age_range, newdata = data.frame(treatment = rep(2, length(age_range))))
# })
# p_HS_TRT = matrix(unlist(p_HS_TRT),ncol=psa_iterations, byrow=T)
