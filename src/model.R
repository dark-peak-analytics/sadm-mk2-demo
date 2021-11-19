# markov model

# function to create transition matrix
transMat = function(p_HS_SOC,p_HS_TRT,p_HD,rr_SD){

    p_SD = p_HD * rr_SD
    if(p_SD>1){p_SD=1} # ensure row sum = 1
    if(p_HS_SOC + p_HD >1){p_HS_SOC = 1-p_HD} # ensure row sum = 1
    if(p_HS_TRT + p_HD >1){p_HS_TRT = 1-p_HD} # ensure row sum = 1


    m_SOC = matrix(
        c(
            # Healthy               Sick    Dead        TREAT MATRIX ELEMENTS
            1 - (p_HS_SOC + p_HD), p_HS_SOC, p_HD, 0, 0, 0,
            0                    , 1 - p_SD, p_SD, 0, 0, 0,
            0                    , 0       , 1   , 0, 0, 0
        ),
        nrow = 3,
        byrow = T
    )
    m_TRT = matrix(
        c(
            # SOC MATRIX      Healthy               Sick       Dead        SOC MATRIX ELEMENTS
            0, 0, 0,        1 - (p_HS_TRT + p_HD), p_HS_TRT , p_HD,
            0, 0, 0,        0                    , 1 - p_SD , p_SD,
            0, 0, 0,        0                    , 0        , 1
        ),
        nrow = 3,
        byrow = T
    )

    rbind(m_SOC,m_TRT)
}

# create/adjust transition matrices for tim dependent risks
make4dTransMat = function(p_HS_SOC,p_HS_TRT,p_HD,rr_SD,horizon,psa_iterations){

d4_trans_mat = array(
    data = NA,
    dim = c(6, 6, horizon, psa_iterations),
    dimnames = list(
        c("healthy_SOC","sick_SOC","dead_SOC","healthy_TRT","sick_TRT","dead_TRT"),
        c("healthy_SOC","sick_SOC","dead_SOC","healthy_TRT","sick_TRT","dead_TRT"),
        1:horizon,
        1:psa_iterations
    )
    )


for(i in 1:psa_iterations){
    temp_args = cbind(p_HS_SOC = p_HS_SOC[,i],p_HS_TRT = p_HS_TRT[,i],p_HD = p_HD,rr_SD=rr_SD[i])
    d4_trans_mat[, , , i] = sapply(1:nrow(temp_args), function(x) {
        transMat(
            temp_args[x,1],
            temp_args[x,2],
            temp_args[x,3],
            temp_args[x,4]
        )
    })
}
return(d4_trans_mat)
}

# comp discounted sum for markov trace
discountedSum = function(x, disc_rate = 0.035, time = NULL){
    if(is.null(time)){
        time = 1:length(x)
    }
    res = x * 1 / (1 + disc_rate)^time
    res = sum(res)
    return(res)
}

# transMat(0.9,0.5,5)


# psa_iterations = 10
# horizon = length(surv_comb)
# disc_rate = 0.035


# random param utility drawer

drawHelper = function(dist,v1,v2,psa_it){

    v1 = as.numeric(v1)
    v2 = as.numeric(v2)
    switch(dist,

        "random uniform" = {
            runif(psa_it, v1, v2)
        },

        "normal" = {
            rnorm(psa_it, v1, v2)
        },
        "log normal" = {
            rlnorm(psa_it, v1, v2)
        },
        "beta" = {
            rbeta(psa_it, v1, v2)
        },
        "fixed" = {
            rep(v1, psa_it)
        },
        "gamma" = {
            rgamma(psa_it, shape = v1, scale = v2)
        }
    )
}

# draw params

drawParams = function(
    psa_iterations = 10,
    horizon_start = 50,
    horizon_end = 90,
    surv_comb,
    p_HS_SOC,
    p_HS_TRT,
    mean_rr_log = 0,
    sd_rr_log = 0.1,
    c_H_SOC,
    c_TRT,
    c_S,
    u_H,
    u_S) {

    horizon = horizon_end - horizon_start +1

    p_HD = 1 - surv_comb$surv_comb[surv_comb$age %in% horizon_start:horizon_end]
    rr_SD = exp(rlnorm(psa_iterations, mean_rr_log, sd_rr_log))

    c_H_SOC = drawHelper(c_H_SOC[1], c_H_SOC[2], c_H_SOC[3], psa_iterations)
    c_TRT = drawHelper(c_TRT[1], c_TRT[2], c_TRT[3], psa_iterations)
    c_H_TRT = c_H_SOC + c_TRT
    c_S = drawHelper(c_S[1], c_S[2], c_S[3], psa_iterations)
    c_D = 0

    # u_H = u_H # runif(psa_iterations, 0.75, 1)
    u_H = drawHelper(u_H[1], u_H[2], u_H[3], psa_iterations)
    u_S = drawHelper(u_S[1], u_S[2], u_S[3], psa_iterations)
    u_D = 0

    params = list(
        "psa_iterations" = psa_iterations,
        "horizon" = horizon,
        "trans_mat" = make4dTransMat(p_HS_SOC, p_HS_TRT, p_HD, rr_SD, horizon, psa_iterations),
        "c_HSD" = cbind(c_H_SOC, c_S, c_D, c_H_TRT, c_S, c_D),
        "u_HSD" = cbind(u_H, u_S, u_D, u_H, u_S, u_D),
        "c_TRT" = c_TRT
    )

    return(params)
}



runMarkov = function(
    psa_iterations,     # number of PSA iterations to run
    horizon_start,      # age at which the model starts
    horizon_end,        # age at which the model stops
    surv_comb_df,       # gen pop survival data frame
    selected_model,     # selected flexsurvreg model fit for H to S transistion probs
    mean_rr_log = 0,    # mean log RR of S->D compared to H->D
    sd_rr_log = 0.1,    # sd lod RR
    c_H_SOC,            # cost of H
    c_TRT,              # additional cost of H for TRT group
    c_S,                # cost of S (no TRT group any more)
    u_H,                # utility of H
    u_S){               # utility of S


    print("Gen time dependent trans_mats")
    age_range = horizon_start:horizon_end

    p_HS_SOC = lapply(1:psa_iterations, function(x) {
        pred_SOC = predict.uncertain.flexsurvreg(selected_model, type = "hazard", times = age_range, newdata = data.frame(treatment = rep(1, length(age_range))))
    })
    p_HS_SOC = matrix(unlist(p_HS_SOC), ncol = psa_iterations, byrow = T)

    p_HS_TRT = lapply(1:psa_iterations, function(x) {
        pred_TRT = predict.uncertain.flexsurvreg(selected_model, type = "hazard", times = age_range, newdata = data.frame(treatment = rep(2, length(age_range))))
    })
    p_HS_TRT = matrix(unlist(p_HS_TRT), ncol = psa_iterations, byrow = T)

    print("DRAW PARAMS")

    params = drawParams(
        psa_iterations = psa_iterations,
        horizon_start = horizon_start,
        horizon_end = horizon_end,
        surv_comb = surv_comb_df,
        p_HS_SOC = p_HS_SOC,
        p_HS_TRT=p_HS_TRT,
        mean_rr_log = mean_rr_log,
        sd_rr_log = sd_rr_log,
        c_H_SOC = c_H_SOC,
        c_TRT = c_TRT,
        c_S = c_S,
        u_H = u_H,
        u_S = u_S
        )

    print("RUN LOOP")
    # loop setup
    qalys_res = costs_res = undic_time = disc_time = matrix(NA,nrow=params$psa_iterations,ncol = 6)

    ## psa loop
    for(i in 1:params$psa_iterations){

        markov_trace = matrix(data = NA, nrow = params$horizon, ncol = 6)
        markov_trace[1,] = c(1,0,0,1,0,0)

        makrkov_trace = darkpeak::ArmaTDMarkovLoop(markov_trace, params$trans_mat[, , , i])

        undic_time[i,] = colSums(makrkov_trace)
        disc_time[i,] = apply(makrkov_trace, 2, discountedSum)
        qalys_res[i,] = disc_time[i,] * params$u_HSD[i, ]
        costs_res[i,] = disc_time[i,] * params$c_HSD[i, ]
    }


    costs = cbind(rowSums(costs_res[, 1:3]), rowSums(costs_res[, 4:6]))
    qalys = cbind(rowSums(qalys_res[, 1:3]), rowSums(qalys_res[, 4:6]))
    colnames(costs) = colnames(qalys) = c("Standard Care", "Supimab")

    # icer = (sum(costs_res[, 4:6]) - sum(costs_res[, 1:3])) / (sum(qalys_res[, 4:6]) - sum(qalys_res[, 1:3]))
    # print("icer")
    # print(icer)

    print("RETURN RESULTS")

    res_list = list(
        "costs" = costs,
        "qalys" = qalys,
        "undic_time" = undic_time,
        "disc_time" = disc_time,
        "qalys_by_state" = qalys_res,
        "costs_by_state" = costs_res,
        "c_TRT" = params$c_TRT
    )

    return(res_list)
    }




####### TO RUN/TEST/DEBUG MARKOV MODEL OUTSIDE SHINY --------------------

# #  NOT RUN -
# # set.seed(2020)

# # first: load functions above !

# # load essential libraries
#     library(survminer)
#     library(survival)
#     library(flexsurv)
#     library(purrr)

# # source some utility function
#     source("./predict.flexsurvreg.R")
#     source("./surv_mk2.R")

#     # load ONS SURVIVAL DATA utility functions
#     loadONS = function(str = "df_ons.csv") {
#     surv_probs = read.csv(str)
#     surv_probs = surv_probs[surv_probs$age > 49, ]
#     surv_probs = surv_probs[, -1]
#     # add 91-100 age
#     df_temp = surv_probs[nrow(surv_probs):(nrow(surv_probs) - 1), ]
#     df_temp = df_temp[rep(1:2, each = 10), ]
#     df_temp$age = rep(91:100, 2)
#     surv_probs = rbind(surv_probs, df_temp)
#     surv_probs = surv_probs[order(surv_probs$age), ]
#     return(surv_probs)
#     }

#     # combine death rates of males and females
#     survCombinator = function(surv_f,surv_m,prop_female){
#         deaths_comb = (surv_f$deaths * prop_female) + (surv_m$deaths * (1 - prop_female))
#         pop_comb = (surv_f$pop * prop_female) + (surv_m$pop * (1 - prop_female))
#         mort_comb = deaths_comb / (pop_comb + deaths_comb / 2)
#         surv_comb = 1 - mort_comb
#         surv_c = data.frame(age = 50:100, surv_cum = cumprod(surv_comb), surv_comb = surv_comb)
#         return(surv_c)
#     }

# # generate and load neccessary data
#     # load ONS data for general pop. to get mortality rate
#     surv_probs = loadONS()
#     surv_m = surv_probs[surv_probs$sex == "Male", ]
#     surv_m$surv_cum = cumprod(surv_m$surv_rx)
#     surv_f = surv_probs[surv_probs$sex == "Female",]
#     surv_f$surv_cum = cumprod(surv_f$surv_rx)

#     # gen weibull data for fitting treatment effect survival model, for p(H -> S)
#     surv_df = genWeibullSurvDat(RR = 0.75,age_range = 0:100,censor_age = 36,n = 2000,n_t = 2000)
#     surv_obj_1 <- Surv(time = surv_df$survival_time,event =surv_df$event,type = "right")


# # MODEL SETUP
#     prop_female = 0.62          # proportion female, to weight general pop. mortality rate
#     horizon_start = 50          # at what age to start (>= 50)
#     horizon_end = 90            # at what age to stop (<= 100)
#     age_range = horizon_start:horizon_end   # age range to draw time dependent trans matrices, could potentially adjut cycle length
#     surv_comb_df = survCombinator(surv_f,surv_m,prop_female) # general pop mortality rate
#     dist = "weibullph"           # flex surv reg model fit
#     psa_iterations = 1000 # n psa iterations (up to 10,000 is abolsutely fine)
#     # flex surv fit to estimate treatment effectiveness
#     selected_model = flexsurvreg(formula = surv_obj_1 ~ as.factor(treatment), data = surv_df, dist = dist)
#     mean_rr_log = 0.49      # adjust mortality rate in state sick wiht relative risk
#     sd_rr_log = 0.19        # log sd of mean RR
#     # distribution and parameters for costs and utilities
#     c_H_SOC = c("log normal", 6, 0.1)
#     c_TRT = c("fixed", 1250, 0)
#     c_S = c("gamma", 44, 125)
#     u_H = c("beta", 10, 2)
#     u_S = c("beta", 6, 3)


# # RUN MODEL
#     res = runMarkov(
#         psa_iterations, # number of PSA iterations to run
#         horizon_start, # age at which the model starts
#         horizon_end, # age at which the model stops
#         surv_comb_df, # gen pop survival data frame
#         selected_model, # selected flexsurvreg model fit for H to S transistion probs
#         mean_rr_log = 0, # mean log RR of S->D compared to H->D
#         sd_rr_log = 0.1, # sd lod RR
#         c_H_SOC, # cost of H
#         c_TRT, # additional cost of H for TRT group
#         c_S, # cost of S (no TRT group any more)
#         u_H, # utility of H
#         u_S # utility sick
#     )

# # raw results
# head(res$qalys) # total qalys per strategy
# head(res$qalys_res) # qalys per state per iteration
# head(res$costs) # aggregate strategy costs per iteration
# head(res$costs_res) # cost per state per iteration
# head(res$undic_time) # undiscounted total time spend in any state per iteration
# head(res$disc_time) # disocunted total time spend in any state per iteration
# rowSums(head(res$undic_time))


# # reporting
# source("./cep.R")
# source("./makeCEAC.R")
# source("./icer_tbl.R")
# library(ggplot2)
# library(scales)
# library(DT)
# makeCEAC(res$costs, res$qalys, treatment = colnames(res$qalys), col = c("red", "blue"))
# makeCEPlane(res$costs, res$qalys, comparitor = colnames(res$qalys)[1], colnames(res$qalys)[2], col = c("red", "blue"))
# createICERtable(res$costs, res$qalys)

