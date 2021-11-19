# sick_RR module

sickRR_ui <- function(title = "Sick survival",tab_id = 2){
        
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
                    offset = 1,
                    width = 10,
                    br(),
                    div(title, style = "font-weight:300; font-size:20pt; margin-top:25px; margin-bottom:10px"),
                    br(),
                    div(style="font-size:120%; font-weight:450","p(sick -> dead) = p(healthy -> dead) * RR"),
                    br(),
                    div(style="font-size:120%; margin-bottom:10px;",
                        p("People in the sick state have a higher mortality rate than in the healthy state."),
                        p("The transisition probability from sick to dead is computed as the healthy (base) survival rate, times a relative risk (RR).")
                        ),
                    br(),br(),
                    column(6,
                    br(),
                    tags$table(id = "tbl_rr", 
                    tags$th(id = "tbl_rr_h",colspan = "7", "You can set the mean RR and the uncertainty around it - a log normal distribution is used."),
                        tags$tr(
                            width = "100%",
                            tags$td(width = "5%", tags$div(style = "font-size:14px;", " ")),
                            tags$td(width = "40%", div(style = "font-size:14px;", "Mean log(RR):")),
                            tags$td(width = "10%", numericInput(inputId = "rr_mean_log",min = NA, step = 0.01,max = 2, value = 0.49,label ="",width = "90px")),
                            tags$td(width = "5%", tags$div(style = "font-size:14px;", " ")),
                            tags$td(width = "15%", tags$div(style = "font-size:14px;", "SD:")),
                            tags$td(width = "10%", numericInput(inputId = "rr_sd_log",step = 0.01, min = 0, max = 2, value = 0.19,label ="",width = "90px")),
                            tags$td(width = "5%", tags$div(style = "font-size:14px;", " "))
                        ))),
                        column(6,plotOutput("rr_hist",height = "200px")),
                        column(12,hr())
                        
                ),
                
                # close
                tabClose(tab_id)
            )
        )
    
}




# server: hist rr
rrHistMaker = function(mean,sd,rr_range = c(0,3)){
        rr_out <- function(x) {
            dlnorm(x, mean, sd)
        }
        ggplot(NULL, aes(x = 3)) +
            geom_area(stat = "function", fun = rr_out, col = "#6684A2", fill = "#6684A2", xlim = rr_range, alpha = 0.65) +
            geom_vline(xintercept = exp(mean), col = "#6684A2", alpha = 0.8) +
            coord_cartesian(ylim = c(0, 2)) +
            theme_minimal() +
            xlab("Distribution: RR of death in Sick State") +
            ylab("") +
            theme(axis.text.y = element_blank())
    }
