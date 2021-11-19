# trt effect module



trt_effect_ui <- function(surv_m_fitted,title = "Supimab effect",tab_id = 3) {

    dropMenu(

        # title
        actionBttn(paste0("tab",tab_id), icon = icon("square"), title,style = "jelly",color = "success"),

        # some styling
        arrow = T,
        placement = "right-start",
        maxWidth = "2000px",
        hideOnClick = T,
        options = dropMenuOptions(duration = c(275, 250), animation = "fade", flip = FALSE),

        # content
        fluidRow(
            column(
                offset = 1, width = 10,
                div(title, style = "font-weight:300; font-size:20pt; margin-top:25px; margin-bottom:10px"),
                div(
                    style = "font-size:130%",
                    tags$ul(
                        tags$li("Effectiveness of Supimab compared to standard of care"),
                        tags$li("Time to progression is from a study with limited follow-up"),
                        tags$li("The data has to be extrapolated - select a model from below")
                    )
                ),

                hr(),

                column(
                    offset = 0, width = 4, align = "center",
                    selectInput(
                        inputId = "select_surv_fit",
                        label = "Which fit to use in the model?",
                        choices = unique(surv_m_fitted$dist),
                        selected = NULL,
                        multiple = F
                    )
                ),
                # select survival model for markov
                column(
                    width = 8, align = "center",
                    checkboxGroupButtons(
                        inputId = "show_surv_fits",
                        label = "Show model",
                        choices = unique(surv_m_fitted$dist),
                        status = "info",
                        selected = unique(surv_m_fitted$dist)[c(1,3)],
                        # multiple = T
                    )
                )
            ),
            # flexsurv model plot
                column(offset = 1,width = 7,
                    align = "center",
                    plotOutput("surv_fit_plot",
                    height = "400px",width = "100%")),
            
            # chosen flexsurv model res table
                column(
                    width = 4,
                    style = "white-space: nowrap; overflow:hidden",align="left",
                    # align = "center",
                    tableOutput("surv_fit_model"),
                    br(),br(),
                    HTML("<img src=trt_surv_legend.png width = '50%'>")
                ),

                # close
                column(offset = 1, width = 10,
                    align = "center",
                    HTML("&nbsp"),
                hr(),
                    closeTabBtn(paste0("close_tab",tab_id)),
                    br(),
                HTML("&nbsp")
                )

            )
        )

}


makeTrtSurvModelTxt = function(selected_model,dist){
    require(knitr)
    require(kableExtra)
    
    # model_call = Reduce(paste, deparse(selected_model$call)) #not used
    flexsurv_reg_res = round(selected_model$res[, c(1, 4)], 3)
    rownames(flexsurv_reg_res)[nrow(flexsurv_reg_res)] = "Supimab"
    colnames(flexsurv_reg_res) = c("est","SE")

    kable(
        caption = "",
        flexsurv_reg_res,
        booktabs = T,
        format = "html"
    ) %>%
      add_header_above(c("flexsurvreg model results" = 3)) %>% 
          kable_styling(bootstrap_options = c("hover", "condensed","basic"), full_width = F, position = "left") %>%
          row_spec(1, extra_css = "border-top: 1px solid #DDDDDD") %>%
          row_spec(nrow(flexsurv_reg_res), extra_css = "border-bottom: 1px solid #DDDDDD") %>%
      kableExtra::footnote(
              general_title = "fit: ", general = paste0(dist,"; AIC: ",round(selected_model$AIC,2)),
              title_format = c("italic", "underline"), footnote_as_chunk = T
          ) # %>%
            #   add_footnote(model_call,notation="none")


            
}


makeTrtSurvFitPlot <- function(surv_m_fitted,plot_df, input) {
    selected_survival_models <- input$show_surv_fits
    line_size_magnifier <- input$select_surv_fit

    sub_df <- subset(surv_m_fitted, surv_m_fitted$dist %in% selected_survival_models)
    transparents_df <- subset(surv_m_fitted, !(surv_m_fitted$dist %in% selected_survival_models))

    surv_plot <- ggplot() +
        geom_step(data = plot_df, aes(x = time, y = surv, col = treatment), size = 1.3) +
        geom_line(data = sub_df, aes(x = time, y = est, col = as.factor(treatment), linetype = dist), alpha = 0.7, size = 1)

    # add transparent lines of deselected model to keep legend consistent
    if (length(transparents_df$dist) > 0) {
        surv_plot <- surv_plot +
            geom_line(data = transparents_df, aes(x = time, y = est, col = "", linetype = dist), col = "transparent")
    }


    # increase lines of model selected for markov
    if (line_size_magnifier %in% selected_survival_models) {
        surv_plot <- surv_plot +
            geom_line(
                data = subset(surv_m_fitted, surv_m_fitted$dist %in% line_size_magnifier),
                aes(x = time, y = est, col = as.factor(treatment), linetype = dist), size = 1.5,
            )
    }


    linetype_order <- unique(c(sub_df$linetype, transparents_df$linetype))
    linetype_order <- linetype_order[order(linetype_order)]

    surv_plot <- surv_plot +
        ylab("Survival") +
        scale_x_continuous(name = "Months", breaks = seq(0, 72, 6)) +
        xlab("Months") +
        coord_cartesian(xlim = c(0, 72), ylim = c(0, 1)) +
        scale_linetype_manual(name = "", values = unique(linetype_order)) +
        scale_color_manual(
            name = "", 
            values = c("cadetblue", "darkorange", "transparent"), 
            labels = c("SOC", "Supimab")
            ) +
        theme_minimal() +
        theme(
            legend.position = "none",
            # legend.text = element_text(size = rel(1.5)),
            # legend.key.width = unit(1, "cm"),
            # legend.text = element_text(size = 15),
            axis.text.x = element_text(size = 15),
            axis.title.x = element_text(size = 15, face = "bold",vjust = -1),
            axis.text.y = element_text(size = 15),
            axis.title.y = element_text(size = 15, face = "bold")
        ) #+
        # guides(linetype = guide_legend(override.aes = list(size = 1.)))



    return(surv_plot)
}



