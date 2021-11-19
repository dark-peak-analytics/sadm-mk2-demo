# survival module



# # # load ONS SURVIVAL DATA -------
loadONS = function(str = "data/df_ons.csv") {
  surv_probs = read.csv(str)
  surv_probs = surv_probs[surv_probs$age > 49, ]
  surv_probs = surv_probs[, -1]
  # add 91-100 age
  df_temp = surv_probs[nrow(surv_probs):(nrow(surv_probs) - 1), ]
  df_temp = df_temp[rep(1:2, each = 10), ]
  df_temp$age = rep(91:100, 2)
  surv_probs = rbind(surv_probs, df_temp)
  surv_probs = surv_probs[order(surv_probs$age), ]
  return(surv_probs)
}



# gen pop survival ui  -----
survival_ui <- function(title = "Base survival",tab_id = 1){
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
                    div(title,style = "font-weight:300; font-size:20pt; margin-top:25px; margin-bottom:10px")
                ),
                column(
                offset = 1, width = 8,
                div(
                    style = "font-size:120%",
                    p("We use mortality rates from the ONS to model age-dependent baseline mortality rates."),
                    column(11,style="padding-left:0",
                        p("In the base scenario, the cohort is 65% female, but you can change 
                        this using the slider on the right. Mortality rates are adjusted accordingly"),
                        p("You can also set the age at which the cohort
                    enters (currently 50), and at what age the model stops (currently 90), 
                    using the two handles at the bottom."),
                    br()
                    ),
                    column(1,icon("arrow-right"),br(),br(),icon("arrow-down"),style="font-size:200%; white-space: nowrap;")
                )
            ),
            column(3, align = "left",
                noUiSliderInput(
                    inputId = "prop_female",
                    label = "% Female",
                    orientation = "vertical",direction = "rtl",
                    min = 0, max = 100, value = 64,step = 1,
                    height = "120px", width = "50%",
                    color = "#70A338",
                    pips = list(
                        mode = "values",
                        values = seq(0, 100, 25),
                        density = 5
                    ),
                    format = wNumbFormat(decimals = 0,suffix = "%")
                )
            ),
            # column(offset = 1,width = 10,
                # hr(),
                # div(# style = "font-size:120%",br()),
                column(offset = 1,width = 10,align="center",
                div(style = "border: 1px solid black",
                plotOutput("gen_pop_survival",height = "350px",width = "95%"),
                div(
                    style = "margin-left:90px; margin-right:45px;",
                    noUiSliderInput(
                        inputId = "set_horizon",
                        label = "Select the age range to investigate in the model",
                        min = 50, max = 100,
                        connect = c(T,T,T),
                        color = c("#70708a"),
                        value = c(50, 90),
                        margin = 5,
                        pips = list(
                            mode = "values",
                            values = seq(50,100,5),
                            density = 5
                        ),
                        format = wNumbFormat(decimals = 0),
                        width = "95%",
                        step = 1
                    )
                ),
                # span("Set model horizon: start and end age",style="font-weight:300; font-size:15pt"),
                br(),HTML("&nbsp")
                )),

                # close
                column(offset = 1,width = 10,align="center",
                HTML("&nbsp"),
                br(),br(),
                closeTabBtn(paste0("close_tab",tab_id)),
                    br(),br(),
                    HTML("&nbsp")
                )
        )
        )


}



# function to combine surv_m and surv_f with prop_female -----
survCombinator = function(surv_f,surv_m,prop_female){
    deaths_comb = (surv_f$deaths * prop_female) + (surv_m$deaths * (1 - prop_female))
    pop_comb = (surv_f$pop * prop_female) + (surv_m$pop * (1 - prop_female))
    mort_comb = deaths_comb / (pop_comb + deaths_comb / 2)
    surv_comb = 1 - mort_comb
    surv_c = data.frame(age = 50:100, surv_cum = cumprod(surv_comb), surv_comb = surv_comb)
    return(surv_c)
}

#### function to create gen pop survival plot 
makeGenPopSurvPlot = function(set_min,set_max,surv_f,surv_m,surv_c) {

  ggplot() +
    # median survival line
    geom_hline(yintercept = 0.5, alpha = 0.7, col = "gray", size = 1.2) +

    # survival curves
    geom_line(data = surv_m, aes(x = age, y = surv_cum, col = "Male"), size = 1.5, alpha = 1) +
    geom_line(data = surv_f, aes(x = age, y = surv_cum, col = "Female"), size = 1.5, alpha = 1) +
    geom_line(data = surv_c, aes(x = age, y = surv_cum, col = "Mix, used in model"), size = 1.5, alpha = 1) +

    # gray out areas that arent considere din the model
    geom_vline(xintercept = set_min, alpha = 0.5, col = "gray") +
    geom_vline(xintercept = set_max, alpha = 0.5, col = "gray") +
    
        geom_rect(aes(xmin = 40, xmax = set_min, ymin = -0.2, ymax = 1.2), fill = "gray", alpha = 0.2) +
        geom_rect(aes(xmin = set_max, xmax = 110, ymin = -0.2, ymax = 1.2), fill = "gray", alpha = 0.2) +

        # set clors for survivalcirvey
        scale_color_manual(
              name = "",
              values = c("goldenrod","#2FA4E7", "#70A338"),
              guide = guide_legend(keywidth = 3,override.aes = list(size=4))
          ) +

        # labels 
        ylab("Proportion alive") +
        xlab("Age") +

        # margins
        scale_x_continuous(breaks = seq(0, 100, 10)) +
        coord_cartesian(xlim = c(50, 100), ylim = c(0, 1)) +

        # styling
          theme_minimal() +
            theme(
              legend.position = "top",
              legend.text = element_text(size = 15),
              # axis.text.x = element_text(size = 15),
              # axis.title.x = element_text(size = 15, face = "bold"),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_text(size = 15),
              axis.title.y = element_text(size = 15, face = "bold")
            )

    }


