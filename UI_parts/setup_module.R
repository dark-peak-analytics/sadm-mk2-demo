

# model setup

setup_ui <- function(title = "Setup",tab_id = 5){
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
                div("Setup", style = "font-weight:300; font-size:20pt; margin-top:25px; margin-bottom:10px"),
                
                # NOTE: the slider input values are adjusted by the js logify function below!
                sliderInput("psa_its", "How many PSA iterations",
                    min = 0, max = 6, 
                    value = 3,  # = 1,000
                    step = 1
                ),
                tabClose(tab_id)
            )
        )
    )
}


JS.logify <-
"
// function to logify a sliderInput
function logifySlider (sliderId, sci = false) {
    // regular number style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return (['Ultra fast (n=10)','100','500','1,000','2,500','5,000','Accurate (n=10,000)'][num]); }
    })
}"
# call logifySlider for each relevant sliderInput
JS.onload <-
"
// execute upon document loading
$(document).ready(function() {
  // wait a few ms to allow other scripts to execute
  setTimeout(function() {
    // include call for each slider
    logifySlider('psa_its')
  }, 5)})
"