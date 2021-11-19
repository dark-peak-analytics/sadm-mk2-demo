# price optimality





priceOptim <- function(qalys,costs,price,thresh = input$price_thresh,range_x = NULL,type = 1, col = "orange"){

    if (is.null(qalys)) {
        null_plot = ggplot() +
            theme_void() +
            geom_label(aes(x = 1, y = 1, label = "Click button to run analysis"), size = 6, col = "#641e1e")
        return(null_plot)
    }


    nb_soc = qalys[,1] * thresh - costs[,1]
    nb_supi = qalys[,2] * thresh - costs[,2]
    inb = nb_supi - nb_soc

    if (is.null(range_x)) {
        range_x = range(price)
        pred_x = as.numeric(range_x[1]:range_x[2])
    } else {
        pred_x = as.numeric(range_x[1]:range_x[2])
    }




    if(type == 1){
        plo = ggplot(NULL, aes(x = price, y = round(inb/1000))) +
                geom_hline(yintercept = 0, linetype="dashed") +
                geom_point(size = 0.7,alpha =.7, col = col) +
                geom_smooth(fill="steelblue2") +
                theme_minimal() +
                scale_y_continuous(label = unit_format(prefix = "\u00A3",suffix = "k")) +
                scale_x_continuous(label = label_comma(prefix = "\u00A3")) +
                ylab("Expected INB") +
                xlab("Supimab price")

    }

    if (type == 2) {
        fitlog = glm(inb > 0 ~ price, family = "binomial")
        preds = predict(fitlog, newdata = data.frame(price = pred_x), type = "response", se = T)
        p.pred = preds$fit
        p.upper = p.pred + 2 * preds$se.fit
        p.lower = p.pred - 2 * preds$se.fit

        plo = ggplot() +
            geom_point(aes(x=price, y= as.numeric(inb>0)), col = col, size=0.7, alpha=0.7)+
            geom_hline(yintercept = 0.5, linetype = "dashed", col = "gray") +
            geom_ribbon(aes(x=pred_x, ymin=p.lower, ymax = p.upper), fill="steelblue2", alpha=0.3) +
            geom_line(aes(x = pred_x, y = p.pred)) +
            theme_minimal() +
            scale_x_continuous(label = label_comma(prefix = "\u00A3")) +
            #ylim(0, 1) +
            ylab("Probability Supimab cost-effective") +
            xlab("Supimab price")
    }

    plo = plo + theme(
                    legend.text = element_text(size = 14),
                    legend.title = element_text(size = 14),
                    axis.text.x = element_text(size = 14),
                    axis.title.x = element_text(size = 14),
                    axis.title.y = element_text(size = 14),
                    axis.text.y = element_text(size = 14)
                )

    return(plo)

}