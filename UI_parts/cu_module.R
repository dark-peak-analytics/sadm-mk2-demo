# costs & utils


cu_ui <- function(title = "Costs & utils", tab_id = 4) {
    dropMenu(
        # title
        actionBttn(paste0("tab", tab_id), icon = icon("square"), title, style = "jelly", color = "success"),

        # some styling
        arrow = T,
        placement = "right-start",
        maxWidth = "2000px",
        hideOnClick = F,
        options = dropMenuOptions(duration = c(275, 250), animation = "fade", flip = FALSE),

        # content

        fluidRow(
            column(
                offset = 1, width = 10,
                div(title, style = "font-weight:300; font-size:20pt; margin-top:25px; margin-bottom:10px"),
                div(
                    style = "font-size:120%",
                    p("Set state costs and utilities input parameters"),
                    p("")
                )
            )
            ),
            fluidRow(
                column(
                    offset = 1,width = 10,

                tags$table(
                    id = "c_tbl",
                    width = "100%",
                    # style = "vertical-align:center; font-size:110%;",
                    tags$th(width = "15%","Parameter",style="text-align: left; padding-left:10px;"),
                    tags$th(width = "15%","Distribution"),
                    tags$th(width = "15%","Parameter 1"),
                    tags$th(width = "15%","Parameter 2"),
                    tags$th(width = "40%","Distribution"),

                    rowMaker(id = "c_tbl_supimab", var_name = HTML(paste(tags$b("Supimab")," costs"))),
                    rowMaker(id = "c_tbl_h", var_name = HTML(paste("Costs",tags$b(" Healthy")))),
                    rowMaker(id = "c_tbl_s", var_name = HTML(paste("Costs",tags$b(" Sick")))),
                    rowMaker(id = "u_tbl_h", var_name = HTML(paste("Utilities",tags$b(" Healthy")))),
                    rowMaker(id = "u_tbl_s", var_name = HTML(paste("Utilities",tags$b(" Sick")))),

                )
                ),
                     # close
                column(offset = 1,width = 10,align="center",
                HTML("&nbsp"),
                br(),
                closeTabBtn(paste0("close_tab",tab_id)),
                    br(),br(),
                    HTML("&nbsp")
                )
                    )

    )
}




# utility function to create rows for cu table
rowMaker = function(var_name = "State cost healthy",id = "c_tbl_h"){
    
    all_dists = c("normal", "random uniform", "gamma", "log normal","beta", "fixed")
    
    new_row = tags$tr(
        width = "100%",
        tags$td(width = "15%", div(var_name),style="font-size:110%; vertical-align:center; text-align:left;  padding-left:10px; text-shadow: 1px 1px 1px rgba(136, 45, 45, 0.1); border-radius:20px;"),
        tags$td(
            width = "15%", 
            selectizeInput(
                inputId = id, label = "",
                choices = all_dists,
                width = "100%",
                options = list(
                    # firce no default
                    # placeholder = 'Select \ndistribution',
                    onInitialize = I('function() { this.setValue(""); }')
                )
            ),
                style="text-align:center; padding-top:10px;"),
        tags$td(width = "15%", div(textInput(paste0(id,"_v1"),NULL,width = "100%"),style="text-align:center;"), style="text-align:center;"),
        tags$td(width = "15%", textInput(paste0(id,"_v2"),NULL,width = "100%"), style="text-align:center;"),
        tags$td(width = "40%",  plotOutput(paste0(id,"_plot"),height = "120px"))
    )
    
    return(new_row)
}



# function to define default parameters for different vars and distributions 
#  used to populate textinput at init
dist_values = function() {
    list(
        "c_tbl_supimab" = list(
            "random uniform" = c(90, 110),
            "normal" = c(90, 110),
            "gamma" = c(200, 20),
            "log normal" = c(4.6, 0.1),
            "beta" = c(1, 1),
            "fixed" = c(1500, 0)
        ),
        "c_tbl_h" = list(
            "random uniform" = c(90, 110),
            "normal" = c(90, 110),
            "gamma" = c(200, 20),
            "log normal" = c(6, 0.1),
            "beta" = c(1, 1),
            "fixed" = c(100, 0)
        ),
        "c_tbl_s" = list(
            "random uniform" = c(90, 110),
            "normal" = c(90, 110),
            "gamma" = c(44, 125),
            "log normal" = c(4.6, 0.1),
            "beta" = c(1, 1),
            "fixed" = c(100, 0)
        ),
        "u_tbl_h" = list(
            "random uniform" = c(90, 110),
            "normal" = c(90, 110),
            "gamma" = c(200, 20),
            "log normal" = c(4.6, 0.1),
            "beta" = c(10, 2),
            "fixed" = c(100, 0)
        ),
        "u_tbl_s" = list(
            "random uniform" = c(90, 110),
            "normal" = c(90, 110),
            "gamma" = c(200, 20),
            "log normal" = c(4.6, 0.1),
            "beta" = c(6, 3),
            "fixed" = c(100, 0)
        )
    )
}

# depending on the distribution, a different function is used to generate the 
# data set for the distribution plot
plotSwitcher = function(dist,vals){

     switch(dist,

         "random uniform" = {
                 plot_x = seq(vals[1], vals[2], length.out = 100)
                 plot_y = dunif(plot_x, vals[1], vals[2])
                 plot_range_x = c(vals[1] - ((vals[1] + vals[2]) / 10), vals[2] + ((vals[1] + vals[2]) / 10))
                 plot_range_y = c(0, max(plot_y) + max(plot_y) / 10)
         },

         "normal" = {
                 plot_x = qnorm(seq(0.01, 0.99, length.out = 100), vals[1], vals[2])
                 plot_y = dnorm(plot_x, vals[1], vals[2])
                 plot_range_x = range(plot_x)
                 plot_range_y = c(0, max(plot_y) + max(plot_y)/10)
         },
         "log normal" = {
                 plot_x = qlnorm(seq(0.01, 0.99, length.out = 100), vals[1], vals[2])
                 plot_y = dlnorm(plot_x, vals[1], vals[2])
                 plot_range_x = range(plot_x)
                 plot_range_y = c(0, max(plot_y) + max(plot_y)/10)
         },
         "beta" = {
                 plot_x = qbeta(seq(0.01, 0.99, length.out = 100), vals[1], vals[2])
                 plot_y = dbeta(plot_x, vals[1], vals[2])
                 plot_range_x = c(0,1)
                 plot_range_y = c(0, max(plot_y) + max(plot_y)/10)
         },
         "fixed" = {
                 return(NULL)
         },
         "gamma" = {
                 plot_x = qgamma(p = seq(0.01, 0.99, length.out = 100), shape = vals[1], scale = vals[2])
                 plot_y = dgamma(plot_x, shape = vals[1], scale = vals[2])
                 plot_range_x = range(plot_x)
                 plot_range_y = c(0, max(plot_y) + max(plot_y)/10)
         }
     )
     res = list(
         plot_x = plot_x,
         plot_y = plot_y,
         plot_range_x = plot_range_x,
         plot_range_y = plot_range_y
     )

        return(res)
        }

# takes a list with data from plotSwitcher() and generate a ggplot
tblPlotter = function(dist,val1, val2){

        vals = as.numeric(c(val1, val2))
        if (NA %in% vals | -1 %in% sign(vals)) {
            # if there is a NA value
            tbl_plot = ggplot() +
                theme_void() +
                geom_label(aes(x = 1,y = 1,label = "Non valid input"),size = 10,col="#641e1e") 
            return(tbl_plot)
        }

        if(dist =="fixed"){
            tbl_plot = ggplot() +
                theme_void() +
                geom_label(aes(x = 1,y = 1,label = val1),size = 20) +
                ggtitle("Fixed value")
            return(tbl_plot)
        }
        # else
        x = plotSwitcher(dist, vals)

        tbl_plot = ggplot() +
            geom_density(
                aes(x = x$plot_x, y = x$plot_y),
                stat = "identity", fill = "cadetblue",
                col = "cadetblue4", alpha = 0.5
                ) +
            coord_cartesian(
                xlim = x$plot_range_x,
                ylim = x$plot_range_y
                ) +
                xlab("Value") +
                    scale_y_continuous(name = "Density", labels = NULL) +
                    theme_minimal() +
                    theme(
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank()
                    )


        return(tbl_plot)
    }

# utility function to retrieve the labels for parameters 1+2
tblLabeller = function(dist){
    switch(dist,
        "random uniform" = c("Min", "Max"),
        "normal" = c("Mean", "SD"),
        "gamma" = c("Shape", "Scale"),
        "log normal" = c("meanlog", "sdlog"),
        "beta" = c("shape1", "shape2"),
        "fixed" = c("value", "")
    )

}

