makeFooter = function(){
    div(
        class = "copyright",
        column(2, HTML("<img src= dpa2.png width='80px'>"), style = "margin:0;padding:0;", align = "right"),
        column(10,
            style = "vertical-align: center;", align = "left",
            HTML("A shiny app by:<br> Paul Schneider & Rob Smith"), br(),
            HTML('<a target="_blank" href="https://www.darkpeakanalytics.com/">Dark Peak Analytics</a>')
        )
    )
}