# a lean shiny app for a simple markov model
# by Paul Schneider
# and Robert Smith
# contact: p.schneider@sheffield.ac.uk

rm(list = ls())

# contains all the libraries
miceadds::source.all("utils")


# setup
set.seed(2020)

tabClose <<- function(tab_id){
                    column(
                    offset = 1, width = 10, align = "center",
                    HTML("&nbsp"),
                    br(),
                    closeTabBtn(paste0("close_tab",tab_id)),
                    br(), br(),
                    HTML("&nbsp")
                )
                }

# source modules and functions
miceadds::source.all("src")

# user interface parts
miceadds::source.all("UI_parts")



# gen weibull data
surv_df = genWeibullSurvDat(RR = 0.75,age_range = 0:100,censor_age = 36,n = 2000,n_t = 2000)
surv_m_fitted <- fitSurvDists(surv_df,times = 1:120)
surv_obj_1 <- Surv(time = surv_df$survival_time,event =surv_df$event,type = "right")
surv_fit_1 <- survfit(surv_obj_1~surv_df$treatment)
plot_df <- surv_summary(surv_fit_1, data = surv_df)

# load ONS data
surv_probs = loadONS()
surv_m = surv_probs[surv_probs$sex == "Male", ]
surv_m$surv_cum = cumprod(surv_m$surv_rx)
surv_f = surv_probs[surv_probs$sex == "Female",]
surv_f$surv_cum  = cumprod(surv_f$surv_rx)

# some stlying
font_add_google("Inter", "Inter")
showtext_auto()
options(scipen = 99) # no sci notations


ui <- fluidPage(

    # preamble
    tags$head(tags$script(HTML(JS.logify))),
    tags$head(tags$script(HTML(JS.onload))),
    tags$head(tags$script(src = "enter_button.js")),

    includeCSS("www/custom.css"),
    theme = shinytheme("flatly"),
    useShinydashboard(), #
    useShinyjs(), # shinyjs set on
    # shiny alert when clicking model run without completing all steps
    useShinyalert(),
    # noty for sending message after model is done
    use_noty(maxVisible = 2),
    # loading spinner with css hack
    add_busy_spinner(
        timeout = 0,
        spin = "semipolar",
        position = "top-right",
        margins = c(25,"260px"),
        color="#68D3BF"),

    # MAIN PAGE ------------------------
    fluidRow(
        column(
            offset = 1, width = 10,
            div(class = "clear_h", "A lean shiny app for a simple markov model -",span("beta 1.0",style = "color: #68D3BF")),
            hr(),
            br(),
            column(
                width = 3,
                div(
                    class = "nav_menu",


                    # NAVIGATION UI MODULES ----------------------------------
                    # div("Navigation", class = "nav_head"),
                    # hr(),

                    # Base survival button - ticked when done
                    survival_ui(tab_id = 1),

                    # Sick survival button - ticked when done
                    sickRR_ui(tab_id = 2),

                    # Supimab effect button - ticked when done
                    trt_effect_ui(tab_id = 3, surv_m_fitted = surv_m_fitted),

                    # Costs & Utils button - ticked when done
                    cu_ui(tab_id = 4),

                    # Setup button - ticked when done
                    setup_ui(tab_id = 5),

                    # run button
                    actionBttn(inputId = "run_model", 
                               label = "Run model", 
                               icon = icon("rocket"), 
                               block = T, 
                               color = "success", 
                               style = "material-flat"),

                    hr(),


                    div(
                        # About the app modal
                        actionLink(inputId = "show", 
                                   label =  "About the tool", 
                                   icon = icon(name ="info-circle","fa-0.5x")),
                        

                        # Feedback
                        div(
                            icon("comment-alt"),
                            HTML('<a href="mailto:p.schneider@sheffield.ac.uk">Feedback?</a>'),
                            style = "padding-top:5px;"    
                        ),
                        class = "nav_foot")


                     )
            ),

            column(
                width = 9,
                div(
                    class = "main_panel",
                    # main user interface
                    main_ui()
                )
            )
        )
    ),

    # fixed author batch in bottom left corner
    makeFooter()
)




server <- function(input, output, session){

# keep track of which tasks are opened and closed
tabs <- paste0("tab",1:5)
task_counter = reactiveValues()
lapply(tabs, function(x) task_counter[[x]]<-0)
tab_open = reactiveValues()
lapply(tabs, function(x) tab_open[[x]]<-F)

# when 'about tool' button is pressed it shows the 'about' modal.
observeEvent(input$show, {
    showModal(
        aboutModal
        ) # end show modal
})


# Navigation logic
observeEvent(lapply(c(tabs,paste0("close_",tabs),"run_model"), function(x) input[[x]]), ignoreInit = T, ignoreNULL = T, {

    # this algorithm tracks which tabs are currently open,
    # which have been opened, which needs closing, and if
    # all tabs have been opened and closed at least once,
    # it highlights the model run button

    tabs_open <- unlist(lapply(tabs, function(x) tab_open[[x]]))
    btn_open_states <- as.numeric(unlist(lapply(paste0(tabs), function(x) input[[x]])))
    prev_open <- as.numeric(unlist(isolate(lapply(tabs, function(x) task_counter[[x]]))))
    open_diff <- abs(btn_open_states - prev_open)

    if (sum(tabs_open) > 0) {

        tab_needs_closing <- tabs_open > 0
        hideDropMenu(paste0(tabs[tab_needs_closing], "_dropmenu"))
        tab_open[[tabs[tab_needs_closing]]] <- F
        btnTaskCompleted(tabs[tab_needs_closing], "#72C1EE", session)

        if (sum(prev_open < 1) < 1) {
            btnLookActived("run_model") # make run button look active
        }

    }

    if(sum(open_diff)>0){
        task_counter[[tabs[open_diff != 0]]] <- task_counter[[tabs[open_diff != 0]]] + 1
        tab_open[[tabs[open_diff != 0]]] <- T
    }
})


# tab 1 -----------------------------
# mixed pop survival df
surv_comb_df <- reactive({
    res = survCombinator(surv_f, surv_m, input$prop_female / 100)
    res
})

# gen pop survival plot
output$gen_pop_survival <- renderPlot({
    makeGenPopSurvPlot(
        surv_f = surv_f,
        surv_m = surv_m,
        surv_c = surv_comb_df(),
        set_min = input$set_horizon[[1]],
        set_max = input$set_horizon[[2]]
    )
})


# interpret psa it
psa_iterations <- reactiveVal(1000)

observeEvent(input$psa_its,
             ignoreInit = T,
             ignoreNULL = T,{

   # convert psa iterations to actual numbers.                          
   nums <- c("0" = 10, "1" = 100, "2" = 500, "3" = 1000,
             "4" = 2500, "5" = 5000, "6" = 10000)
   
   it <- nums[paste(input$psa_its)]

    psa_iterations(it)

    })



# TAB 2 -------------------------------------------------

# which survival fit is selected?
selected_model <- reactive({NULL})

selected_model <- eventReactive(input$select_surv_fit, ignoreNULL = F, {
    age_range = input$set_horizon[[1]]:input$set_horizon[[2]]
    dist = input$select_surv_fit
    psa_iterations <- psa_iterations()
    # take user input and fit model
    selected_model = flexsurvreg(formula = surv_obj_1 ~ as.factor(treatment), data = surv_df, dist = dist)
    return(selected_model)
})

# treatment effect surv plot
output$surv_fit_plot <- renderPlot({
    makeTrtSurvFitPlot(surv_m_fitted, plot_df, input)
})

# treatment surv model table
output$surv_fit_model <- function(){
    x <- makeTrtSurvModelTxt(selected_model(), input$select_surv_fit)
    x
}

# TAB 3 --------------------------------------------------------

output$rr_hist <- renderPlot({
    rrHistMaker(input$rr_mean_log, input$rr_sd_log, rr_range = c(0,3))
})



# TAB 4: COST & UTILS -----------------------------------------

dist_vars <- c("c_tbl_supimab","c_tbl_h", "c_tbl_s","u_tbl_h","u_tbl_s")

p_dists = reactiveValues(
    "c_tbl_supimab" = "fixed",
    "c_tbl_h" = "log normal",
    "c_tbl_s" = "gamma",
    "u_tbl_h" = "beta",
    "u_tbl_s" = "beta"
)


observeEvent(lapply(dist_vars, function(x) input[[x]]), {

    for (var in dist_vars) {
        # loop through all distribution input selectors
        dist_was = p_dists[[var]]
        dist_is = input[[var]]
        if(dist_is == ""){
            # hack to make table render with default set here on server side
            updateSelectInput(session,var,selected = dist_was)
        }
        var_changed = (dist_was != dist_is)

        # if one has changed or not been set, update table and default values
        if (var_changed) {

            # if dist is changed to/from fixed, dis/enable second input parameter
            if (dist_was == "fixed") {
                enable(id = paste0(var, "_v2"))
            }
            if (dist_is == "fixed") {
                disable(id = paste0(var, "_v2"))
            }

            tbl_labels = tblLabeller(dist_is)          # retrieve labels (e.g. mean, sd for rnorm)
            v_id = paste0(var, c("_v1", "_v2"))        # ids of textinputs
            v_init_val = dist_values()[[var]][[dist_is]] # default values for val 1+2

            # set text inputs to default
            updateNumericInput(session, v_id[1], label = tbl_labels[1], value = v_init_val[1])
            updateNumericInput(session, v_id[2], label = tbl_labels[2], value = v_init_val[2])

            p_dists[[var]] <- dist_is                   # update reactiveVals
        }
    }
})

# update plot if one of the values is changes
output$c_tbl_supimab_plot <- renderPlot(tblPlotter(input$c_tbl_supimab, input$c_tbl_supimab_v1, input$c_tbl_supimab_v2))
output$c_tbl_h_plot       <- renderPlot(tblPlotter(input$c_tbl_h, input$c_tbl_h_v1, input$c_tbl_h_v2))
output$c_tbl_s_plot       <- renderPlot(tblPlotter(input$c_tbl_s, input$c_tbl_s_v1, input$c_tbl_s_v2))
output$u_tbl_h_plot       <- renderPlot(tblPlotter(input$u_tbl_h, input$u_tbl_h_v1, input$u_tbl_h_v2))
output$u_tbl_s_plot       <- renderPlot(tblPlotter(input$u_tbl_s, input$u_tbl_s_v1, input$u_tbl_s_v2))






# RUN MODEL  ------------------------------------------------------------------

run_anyway_yes = reactiveVal(F)
observeEvent(input$run_anyway, ignoreNULL = T,{
    # if people dont go through all steps, they can run the model anyway:
    # to avoid code redundancy, we control this with a reactive value: run_anyway_yes()
    # this also ensures that the confirmation only has to be given once
    run_anyway_yes(T)
})
# alternatively, you can hit enter on the shiny alert
onclick("run_anyway_alt", {
    run_anyway_yes(T)
    btnLookActived("run_model")
    disable("run_anyway")
    closeAlert()
})

model_res <- reactive({NULL})
model_ran <- reactiveVal(F)
t_start <- reactiveVal()

# run model function #=====

model_res <- eventReactive(list(input$run_model,
                                run_anyway_yes()),
                           ignoreNULL = T,
                           ignoreInit = T, {

    prev_open <- as.numeric(unlist(isolate(lapply(tabs, function(x) task_counter[[x]]))))

    if (sum(prev_open<1)>0 & run_anyway_yes() == F) {
        # if all tasks completed AND run_anyway_yes is F, ask for confirmation
        confirmRunModal()
        # invalidateLater(1000)

    } else {
        # i.e. if either all tasks are completed OR run_anyway_yes is T, run model
        # deactive all btns while the model is running
        disableBackgroundBtns(isolate(task_counter))

    # SHOW A SPINNING WHEEL WAITING SCREEN  <<--------- TO DO!
    t_start(Sys.time())
    
    res = runMarkov(
        # input rr_mean and sd!
        psa_iterations = psa_iterations(),
        horizon_start = input$set_horizon[[1]],
        horizon_end = input$set_horizon[[2]],
        surv_comb_df = surv_comb_df(),
        selected_model = selected_model(),
        mean_rr_log = input$rr_mean_log,
        sd_rr_log = input$rr_sd_log,
        c_H_SOC = c(input$c_tbl_h,input$c_tbl_h_v1,input$c_tbl_h_v2),   # cost of H dist and params
        c_TRT   = c(input$c_tbl_supimab,input$c_tbl_supimab_v1,input$c_tbl_supimab_v2),     # additional cost of H for TRT group
        c_S     = c(input$c_tbl_s,input$c_tbl_s_v1,input$c_tbl_s_v2),                                        # cost of S (no TRT group any more)
        u_H     = c(input$u_tbl_h,input$u_tbl_h_v1,input$u_tbl_h_v2),                                       # utility of H
        u_S     = c(input$u_tbl_s,input$u_tbl_s_v1,input$u_tbl_s_v2)
    )

    model_ran(T)

    # active all btns again
    lapply(tabs,enable)
    enable("run_model")
    time_elapsed = Sys.time() - t_start()
    units = attributes(time_elapsed)$units
    str_elapsed = HTML(paste0(
        "<b>Model run finished.</b><br>",
        "PSA iterations: ", formatC(psa_iterations(),big.mark = ",",format="f",digits = 0), "<br>",
        "Elapsed time: ", round(time_elapsed, 2), " ", units
    ))
    noty(text = str_elapsed, type = "alert",layout = "topRight",theme = "metroui",session = session,timeout = 5000)
    return(res)
    }
})


# make model run
observeEvent(
    model_res(),
    {
    print("Trigger model execution")
})



# ---- MAIN PANEL -------------------------------------------------------
# ce-plane ====

output$cep_plane <- renderPlot({
    
    cep_plane = makeCEPlane(
        model_res()$costs, 
        model_res()$qalys,
        comparitor = colnames(model_res()$costs)[1],
        treatment = colnames(model_res()$costs)[2],
        thresh = input$wtp, 
        show_ellipse = input$ellipse,
        colors = c("transparent", input$cep_col)
    )
    
    #cep_planePPT <<- cep_plane
    
    return(cep_plane)
})

# icer tbl
output$icer_tbl <- renderDataTable({
    
    icer_tbl = createICERtable(model_res()$costs, 
                               model_res()$qalys,
                               ci = input$ci95switch)
    
    icer_tbl
})

# ceac
output$ceac <- renderPlot({
    
    ceac = makeCEAC(
        model_res()$costs, 
        model_res()$qalys,
        treatment = colnames(model_res()$costs),
        col = c("cyan", input$cep_col)
    )
    
    return(ceac)
})


# price optimality =====
opt_state_is = reactiveVal(0)
observeEvent(input$optimise_price,ignoreNULL = F,{
    if(is.null(input$optimise_price)){
        price_model_res <- NULL
    } else {
        if (input$optimise_price == opt_state_is()) {
        price_model_res <- NULL
    } else {
        opt_state_is(input$optimise_price)
        disableBackgroundBtns(isolate(task_counter)) # disables all btns
        po_it = 500
        price_model_res = runMarkov(
            psa_iterations = po_it, # diff than normal markov
            horizon_start = input$set_horizon[[1]],
            horizon_end = input$set_horizon[[2]],
            surv_comb_df = surv_comb_df(),
            selected_model = selected_model(),
            mean_rr_log = input$rr_mean_log,
            sd_rr_log = input$rr_sd_log,
            c_H_SOC = c(input$c_tbl_h, input$c_tbl_h_v1, input$c_tbl_h_v2),
            c_TRT = c("random uniform", input$supi_price_range), # diff than normal markov
            c_S = c(input$c_tbl_s, input$c_tbl_s_v1, input$c_tbl_s_v2), # cost of S (no TRT group any more)
            u_H = c(input$u_tbl_h, input$u_tbl_h_v1, input$u_tbl_h_v2), # utility of H
            u_S = c(input$u_tbl_s, input$u_tbl_s_v1, input$u_tbl_s_v2)
        )
        # enable all btns
        lapply(tabs, enable)
        enable("run_model")
    }
    }

    output$price_optim_plot <- renderPlot({
        opt_state_is()
        type = ifelse(input$optim_prob, 2, 1)
        price_optim_plot <- priceOptim(
            costs = price_model_res$costs,
            qalys = price_model_res$qalys,
            price = price_model_res$c_TRT,
            thresh = input$price_thresh,
            range_x = input$supi_price_range,
            col = "orange", type = type
        )
        price_optim_plot
    })

})


# Stability plot =====

output$stability <- renderPlot({

    stabilityPlot = makeStabilityplot(
        total_costs = model_res()$costs,
        total_qalys =  model_res()$qalys,
        line_col = input$cep_col
    )

    return(stabilityPlot)
})




# main panel =====
output$main_panel <- renderUI({

    mainPanelCreator(model_ran())

})



# Download PowerPoint file #======

output$downloadPowerPoint <- downloadHandler(
    
    filename = function() { paste0('darkpeak_SickSicker_slides.pptx')},
    
    content = function(file) {
        
        ICERTablePPT <- autofit(
            flextable(
                createICERtablePPT(model_res()$costs, 
                                          model_res()$qalys,
                                          ci = input$ci95switch))
            )
        
        
        price_model_res = runMarkov(
            psa_iterations = 500, # diff than normal markov
            horizon_start = input$set_horizon[[1]],
            horizon_end = input$set_horizon[[2]],
            surv_comb_df = surv_comb_df(),
            selected_model = selected_model(),
            mean_rr_log = input$rr_mean_log,
            sd_rr_log = input$rr_sd_log,
            c_H_SOC = c(input$c_tbl_h, input$c_tbl_h_v1, input$c_tbl_h_v2),
            c_TRT = c("random uniform", input$supi_price_range), # diff than normal markov
            c_S = c(input$c_tbl_s, input$c_tbl_s_v1, input$c_tbl_s_v2), # cost of S (no TRT group any more)
            u_H = c(input$u_tbl_h, input$u_tbl_h_v1, input$u_tbl_h_v2), # utility of H
            u_S = c(input$u_tbl_s, input$u_tbl_s_v1, input$u_tbl_s_v2)
        )
        
        
        
        price_optim_plot <- priceOptim(
            costs = price_model_res$costs,
            qalys = price_model_res$qalys,
            price = price_model_res$c_TRT,
            thresh = input$price_thresh,
            range_x = input$supi_price_range,
            col = "orange", 
            type = ifelse(input$optim_prob, 2, 1)
        )

        
        placeholderPlot <- ggplot()+ theme_classic()
        
        # make temporary file
        file_pptx <- tempfile(fileext = ".pptx")
        
        # run function to create custom slides
        makePowerpoint(title_list = as.list(c("ICER Table",
                                              "Cost Effectiveness Plane", 
                                              "Cost Effectiveness Acceptability Curve",
                                              "Optimal Pricing", 
                                              "Stability Plot")),
                       
                       content_list = list(ICERTablePPT,
                                           makeCEPlane(
                                               model_res()$costs, 
                                               model_res()$qalys,
                                               comparitor = colnames(model_res()$costs)[1],
                                               treatment = colnames(model_res()$costs)[2],
                                               thresh = input$wtp, 
                                               show_ellipse = input$ellipse,
                                               colors = c("transparent", input$cep_col)),
                                           makeCEAC(
                                               model_res()$costs, 
                                               model_res()$qalys,
                                               treatment = colnames(model_res()$costs),
                                               col = c("cyan", input$cep_col)), 
                                           price_optim_plot, 
                                           stabilityPlot = makeStabilityplot(
                                               total_costs = model_res()$costs,
                                               total_qalys =  model_res()$qalys,
                                               line_col = input$cep_col)
                                           ),
                       
                       template_path = "template/template_pres.pptx",
                       
                       target_file = file_pptx)
        
        # rename file to choice name
        file.rename(from = file_pptx, to = file )
        
        }
)

# download CSV handler
output$downloadCSV <- downloadHandler(
    
    filename = function() { paste0('darkpeak_SickSicker_PSAruns.csv')},
    
    content = function(file) {
        
        file_doc <- tempfile(fileext = ".csv")
        
        tempCSV <- cbind(model_res()$costs, model_res()$qalys) 
        colnames(tempCSV) <- c("SOC.costs", "Supimab.costs", "SOC.qalys", "Supimab.qalys")
      
        write.csv(x = tempCSV, file = file_doc)
        
        file.rename(from = file_doc, to = file )
        
    })

output$downloadWordDoc <- downloadHandler(
    
    filename = function() { paste0('darkpeak_SickSicker_report.docx')},
    
    content = function(file) {
        
        file_doc <- tempfile(fileext = ".docx")
        
        ICERTableWord <- autofit(
            flextable(
                createICERtablePPT(model_res()$costs, 
                                   model_res()$qalys,
                                   ci = input$ci95switch))
            )
        
        price_model_res = runMarkov(
            psa_iterations = 500, # diff than normal markov
            horizon_start = input$set_horizon[[1]],
            horizon_end = input$set_horizon[[2]],
            surv_comb_df = surv_comb_df(),
            selected_model = selected_model(),
            mean_rr_log = input$rr_mean_log,
            sd_rr_log = input$rr_sd_log,
            c_H_SOC = c(input$c_tbl_h, input$c_tbl_h_v1, input$c_tbl_h_v2),
            c_TRT = c("random uniform", input$supi_price_range), # diff than normal markov
            c_S = c(input$c_tbl_s, input$c_tbl_s_v1, input$c_tbl_s_v2), # cost of S (no TRT group any more)
            u_H = c(input$u_tbl_h, input$u_tbl_h_v1, input$u_tbl_h_v2), # utility of H
            u_S = c(input$u_tbl_s, input$u_tbl_s_v1, input$u_tbl_s_v2)
        )
        
        
        
        price_optim_plot <- priceOptim(
            costs = price_model_res$costs,
            qalys = price_model_res$qalys,
            price = price_model_res$c_TRT,
            thresh = input$price_thresh,
            range_x = input$supi_price_range,
            col = "orange", 
            type = ifelse(input$optim_prob, 2, 1)
        )
        
        # create placeholder plot
        placeholderPlot <- ggplot()+ theme_classic()
        
        makeWordDoc(figures = list(#ICERTableWord,
                                   makeCEPlane(
                                       model_res()$costs, 
                                       model_res()$qalys,
                                       comparitor = colnames(model_res()$costs)[1],
                                       treatment = colnames(model_res()$costs)[2],
                                       thresh = input$wtp, 
                                       show_ellipse = input$ellipse,
                                       colors = c("transparent", input$cep_col)),
                                   makeCEAC(
                                       model_res()$costs, 
                                       model_res()$qalys,
                                       treatment = colnames(model_res()$costs),
                                       col = c("cyan", input$cep_col)), 
                                   price_optim_plot, 
                                   stabilityPlot = makeStabilityplot(
                                       total_costs = model_res()$costs,
                                       total_qalys =  model_res()$qalys,
                                       line_col = input$cep_col)),
                    
                    figure_text <-  list(#"ICER Table",
                              "Cost Effectiveness Plane", 
                              "Cost Effectiveness Acceptability Curve",
                              "Optimal Pricing", 
                              "Stability Plot"),
                    
                    template_path = "template/word_template.docx",
                    app_user_name = "hi",
                    target_file = file_doc
        )
        
        file.rename(from = file_doc, to = file )
    }
)




}

shinyApp(ui,server)




