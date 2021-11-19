# confirmRunModal

# this is a shiny alter that shows when you try to run the model
# without going through all the setup up steps
# you can cancel or run the model anyway with the default setup

confirmRunModal = function(){

    # change to shiny alert!
     shinyalert(
        title = "",
        text = 

            fluidRow(
                column(
                    offset = 2, width = 8,
                    div(class="clear_h","You didn't go through all setup steps."),
                    br(),
                    span("You can run the model with the default setup, or go back and complete the steps now.", style = "font-size:130%"),
                    br(),br(),br(),
                    span("What do you want to do?", style = "font-size:130%; font-weight: 450;"),
                    br(),br(),
                ),
            
                column(
                    width = 12,
                    align = "center",
                    actionBttn("close_modal", "Go back", color = "default", style = "jelly"),
                    div(style = "width:50px;display:inline-block;"),
                    # run_anyway_alt is use din .js file to react to key stroke (enter)
                    span(id = "run_anyway_alt",actionBttn("run_anyway", "Run anyway", color = "success", style = "jelly", icon = icon("rocket"))),
                    br(),br()
                )),

        size = "m",
        closeOnEsc = T,
        closeOnClickOutside = T,
        html = TRUE,
        type = "warning",
        showConfirmButton = FALSE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
        )
   }