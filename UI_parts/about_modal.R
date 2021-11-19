aboutModal <- modalDialog(
  id = "about_us_modal",
  fluidRow(
    column(
      offset = 1,
      width=10,
      br(),

      h3("About this app"),
      br(),

      h4("Disclaimer"),
      p(HTML(paste0("Please note that this is a beta version. Not everything may be running smoothly,
      and occasionally the app may crash. However, we have included quite a few features, and more are coming.
      In the meantime, please <a href='mailto:p.schneider@sheffield.ac.uk'>contact us</a> with any requests or queries."
      ))),
      br(),

      h4("Purpose"),
      p("
        This web-app was created as a simple example of a lean, user-friendly interface
        for a health economic decision model. The app, as well as the underlying model, were
        entirely developed in R and R shiny. The primary focus is to make cost-effectiveness models
        built in R more transparent and accessible to decision makers and other stakeholders.
        They can use the interface to run the model for different parameter inputs in real time,
        testing different scenarios and assumptions, without the need to learn a programming language."
      ),
      br(),

      h4("How it works"),
      p("There are two components to this app:"),
      tags$ol(
        tags$li("
          A cost-effectiveness model: the model is an ordinary, time-dependent
          3-state Markov model. It is custom-built for this project and uses
          'Rcpp' to call C++ functions, to speed up the loops within the model.
          This makes makes the model run approximately 3-5 times faster than base R."
        ),
        br(),
        tags$li("
          A user-interface: the web app is developed in R shiny and makes use of many additional
          packages (and some html/css/js tricks) to improve the functionality. It allows the user
          to control the Markov model and to specify various input paramters for it."
        )
      ),
      br(),

      p(HTML("



      <i><b>Paul Schneider</b></i> and <i><b>Robert Smith</b></i><br> December 2020<br>"
      )),
    ) # column close
  ),  # row close

  style = "font-weight: 350; line-height: 22px;",
  size="l",
  easyClose = T,
  footer = div(style = "text-align:center", modalButton("Dismiss")
  )
)
