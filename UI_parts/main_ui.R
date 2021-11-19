# main ui

require(colourpicker)

main_ui <- function() {
    fluidRow(
        column(
            width = 12,
            # div(class="clear_h","Some text"),
            br(),
            uiOutput("main_panel")
        )
    )
}


mainPanelCreator = function(model_ran){

if (model_ran == TRUE) {
            ui_out = fluidRow(
                br(),

                # BOX --------------------
                tabBox(
                    title = tagList(icon("dice-d20"), "Results summary "," "),
                    id = "tabbox",
                    width = "90%",
                    # height = "700px",
                    
                    # ICER TABLE --------------------
                    tabPanel(
                        title = "ICER Table",
                        fluidRow(
                            column(align="center",
                                offset = 0, width = 12,
                                dataTableOutput("icer_tbl",width ="80%")
                            ),
                            column(
                                offset = 8, width =3 ,style = "margin-top:-30px",

                                materialSwitch(inputId = "ci95switch", label = "Show 95% CI", status = "primary",value = T)
                            )
                        )
                    ),

                    # CE-PLANE --------------------
                    tabPanel(
                        title = "CE-Plane",
                        fluidRow(
                            column(offset = 0,12,align= "center",style= "z-index: 2;",
                            splitLayout(
                                cellWidths = c("10%","25%","25%","10%","30%"),
                                cellArgs = list(style = "margin-top: -40px; margin-bottom: -20px;"),
                                # set custom threshold
                                div(HTML("Threshold:"), style="padding-top:35px;"),
                                div(title = "WTP Threshold for CE-Plane",
                                    numericInputIcon(
                                        inputId = "wtp",label =  "", 
                                        help_text = "Threshold", 
                                        min = 0, max = 100000, value = 20000, 
                                        step = 1000, icon = list(NULL, icon("pound-sign"),".00"), 
                                        width = "180px")
                                ),
                                
                                # show/hide ellipse
                                div(
                                    materialSwitch(
                                        inputId = "ellipse", 
                                        label = "Show ellipse", 
                                        status = "primary",
                                        value = F
                                        ), 
                                    style="padding-top:35px; text-align:left"
                                ),
                                
                                # set colour 
                                div(HTML("Set colour:"), style="padding-top:35px;"),
                                div(colourInput("cep_col", NULL, "#eb8d00"), style="padding-top:25px;")
                                ),
                                hr(),
                                plotOutput("cep_plane",height = "450px",width = "90%")
                            )
                        )
                    ),

                    # CEA CURVE --------------------
                    tabPanel(
                        title = "CEA-Curve",
                        fluidRow(
                            column(
                                12,
                                align = "center",
                                hr(),
                                plotOutput("ceac", height = "450px", width = "90%")
                            )
                        )
                    ),



                    # PRICE OPTIMALITY --------------------
                    tabPanel(
                        title = "Optimal pricing",
                        fluidRow(
                            column(
                                width = 3, align = "center",
                                sliderInput(
                                    "supi_price_range", "Price Range", 
                                    min = 0, max = 5000, value = c(0, 3000), 
                                    step = 100, pre = "\u00A3"
                                    )
                            ),
                            column(
                                width = 3, align = "center",
                                numericInputIcon(
                                        inputId = "price_thresh",label =  "Threshold", 
                                        help_text = "Threshold", 
                                        min = 0, max = 100000, value = 20000, 
                                        step = 1000, icon = list(NULL, icon("pound-sign"),".00"), 
                                        width = "100%")
                                ),
                                column(3,style = "padding-left:20px;",
                                div(
                                    HTML("<b>Show probability</b>")),
                                    div(
                                        materialSwitch(inputId = "optim_prob", label = "", status = "primary",value = T),
                                        style = "padding-top:10px; padding-left:20px;"
                                    )
                                ),
                            column(
                                width = 3, align = "left",style = "padding-top:30px;",
                                    actionBttn(inputId = "optimise_price",label = HTML(paste0("Run analyis",tags$sup("*"))), icon = icon("chart-line"),style = "jelly",color = "success"),
                            ),
                            column(
                                width = 12,
                                align="center",
                                hr(),
                                withSpinner(
                                    plotOutput(
                                        "price_optim_plot", width = "100%", height = "400px"),
                                        hide.ui = FALSE, type = 5, color = "#68D3BF", size = 1
                                    ),
                                div(
                                    style ="
                                    text-align:left; 
                                    line-height: 14px; 
                                    font-size: 14px; 
                                    padding-top:10px;
                                    padding-left:20px;
                                    padding-right:40px;",
                                    HTML(paste0(tags$sup("*"),"
                                        The model is re-run with varying Supimab prices.
                                        The results are then used to fit statistical models 
                                        (logistic and non-parametric loess regression) to efficiently estimate
                                        the expected INB and the probability of Supimab being cost-effective
                                        for any given price level."))
                                )))), # end tab panel


                    # Stability Plot --------------------
                    tabPanel(
                        title = "Stability Plot",
                        fluidRow(
                            column(
                                12,
                                align = "center",
                                # hr(),
                                plotOutput("stability", height = "450px", width = "90%")
                                ))), # end tab panel
                    
                    tabPanel(
                        title = "Download Options",
                        # downloads
                        fluidRow(
                            column(offset = 1,
                                width = 10,
                                align = "center",
                                h3("Download options"),
                                
                                p("Click on the download button below to download a slidepack containing 
                                   all of the outputs shown in the app. This should make reporting much easier to update.
                                   Since this involves running the pricing analysis this may take up to 1 minute to run."),
                                
                                column(width = 4,
                                       downloadButton(outputId = 'downloadPowerPoint',
                                                      label =  'Slide Deck')),
                                column(width = 4,
                                downloadButton(outputId = 'downloadWordDoc',
                                               label =  'Word Document')),
                                column(width = 4,
                                downloadButton(outputId = 'downloadCSV',
                                               label =  'CSV of PSA runs')),
                                
                                br())),
                        
                        fluidRow(" ")
                    
                        ) # end tab-box
                    ) # end fluid row
            )
 return(ui_out)
            
    } else {
        ui_out = fluidRow(
            column(
                offset = 0, width = 12,
                style = "font-size: 110%; font-weight:350; max-width: 800px;",
                        p(
                        "This is an Shiny web application for a time-dependent Markov Model. 
                        The purpose of the app is to demonstrate how Shiny can be used create 
                        clean, accessible, and intuitive user interfaces for powerful health 
                        economic models built in R."
                        ),
                        br(),

                        p(HTML(paste0(
                        "<b>To use the app, please complete the the five steps in the menu on the 
                        left hand side, and then click 'Run model'</b>"
                        ))),
                        br(),

                        p(HTML(
                        "The underlying model is a 3-state transition model, with states: '<b>H</b>ealthy', 
                        '<b>S</b>ick' and '<b>D</b>ead'. It is used to inform a decision about a new, 
                        promising drug, <b><i>Supimab</i></b>, which reduces the probability 
                        of becoming Sick. Almost all parameters are uncertain and are sampled 
                        from various probability distributions."
                        )),

                        p("All transistion probabilities are time-dependent:"),
                        tags$ul(
                            tags$li("
                            p(H -> D) is the general population's mortality rate"
                            ),
                            tags$li("
                            To get p(S -> D), we adjust p(H -> D), using some relative risk"
                            ),
                            tags$li("p(H -> S) is extrapolated from fictitious Supimab trial data"
                            )
                        ),
                        br(), 
                        
                HTML("<figure>
                <img src = 'trans_mat.png' width='100%' >
                <figcaption>Simple Markov Model Structure: p_HD and p_SD is time dependent, and p_HS is obtained by survival analysis from trial data, RR_S is input by the app user as a relative risk with a distribution to incorporate parameter uncertainty.</figcaption>
                </figure>
                "),
                br()
            )
        )
        return(ui_out)
    }


}