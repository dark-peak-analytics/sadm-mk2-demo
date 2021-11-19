makeStabilityplot <-
  function(total_costs = darkpeak::example_TC[, 1:2],
           total_qalys = darkpeak::example_TQ[, 1:2],
           line_col = "blue") {
    
    # TESTS ------
    
    # test the number of rows are equal in cost and qaly matrices
    testthat::expect_equal(nrow(total_costs), nrow(total_qalys))
    
    # test the number of columns are equal in cost and qaly matrices.
    testthat::expect_equal(ncol(total_costs), ncol(total_qalys))
    
    # CALC INC COSTS/QALYS ---------
    df = data.frame(v_inc_costs = total_costs[, 2] - total_costs[, 1],
                    v_inc_qalys = total_qalys[, 2] - total_qalys[, 1])
    
    # CALC CUMULATIVE ICER -------
    df$cumICER = cumsum(df$v_inc_costs) / cumsum(df$v_inc_qalys)
    df$n = 1:nrow(df)
    
    # PLOT ----------
    ggplot(data = df,
           mapping = aes(x = n,
                         y = cumICER)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 10, vjust = -5),
        axis.title.y = element_text(size = 10, vjust = 5),
        plot.margin = unit(c(2, 2, 2, 2), "cm")
      ) + # centre plot title
      labs(title = "ICER stability throughout the PSA") +
      xlab(label = "PSA iteration") +
      scale_y_continuous(labels = label_comma(pre = "\u00A3"), name = "Mean ICER at the i'th iteration") +
      geom_line(col = line_col) +  # add lineplot
      coord_cartesian(ylim = range(df$cumICER[10:length(df$cumICER)])) +
      theme(
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = element_text(size = 14)
      )
    
  }
