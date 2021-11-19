# icertable
createICERtable <- function(total_costs = example_TC, total_qalys = example_TQ,
                            ref_index = 1, ci = T) {
  n_col <- ncol(total_costs)
  c_f <- function(x, d = 2, ci = T) {
    x <- c(mean(x), quantile(x, probs = c(0.025, 0.975)))
    x <- formatC(x,
      digits = d, big.mark = ",", format = "f",
      drop0trailing = F
    )
    if (ci) {
      x <- paste0(x[1], " (", x[2], "; ", x[3], ")")
    }
    else {
      x <- paste0(x[1])
    }
    x[grepl("0 \\(", substr(x, 1, 3))] <- ""
    x[grepl("0[.]0 \\(", substr(x, 1, 5))] <- ""
    x[grepl("0[.]00 \\(", substr(x, 1, 6))] <- ""
    x[grepl("0[.]000 \\(", substr(x, 1, 7))] <- ""
    x
  }
  mean_C <- apply(total_costs, 2, c_f, 0, ci)
  mean_Q <- apply(total_qalys, 2, c_f, 3, ci)

  inc.C <- cbind(total_costs[, 1:n_col] - total_costs[, ref_index])
  inc.C <- apply(inc.C, 2, c_f, 0, ci)

  inc.Q <- cbind(total_qalys[, 1:n_col] - total_qalys[, ref_index])
  inc.Q <- apply(inc.Q, 2, c_f, 3, ci)

  NB20 <- total_qalys * 20000 - total_costs
  INB20 <- NB20[, ] - NB20[, 1]
  INB20 <- apply(INB20, 2, c_f, 0, ci)

  NB30 <- total_qalys * 30000 - total_costs
  INB30 <- NB30[, ] - NB30[, 1]
  INB30 <- apply(INB30, 2, c_f, 0, ci)

  icer <- (mean(total_costs[,ref_index] - total_costs[,-ref_index])) / (mean(total_qalys[,ref_index] - total_qalys[,-ref_index]))
  icer <- c("",formatC(icer,format = "f", big.mark = ",", digits = 0))

  res_table <- as.data.frame(t(
    data.frame(
      mean_C, inc.C, mean_Q,inc.Q, icer,INB20, INB30,
    stringsAsFactors = F
  )))
  rownames(res_table) <- c(
    "Costs", "Incremental Costs", "QALYs",
    "Incremental QALYs","ICER", "INB 20,000 GBP", "INB 30,000 GBP"
  )
  res_table$temp <- c(
    "Costs (GBP)", "Costs (GBP)", 
    "QALYs","QALYs", 
    "Incremental Cost-Effectiveness Ratio",
    "Incremental Net Benefit (GBP)", "Incremental Net Benefit (GBP)"
  )
  
  DT::datatable(
    data = res_table, 
    class = "compact row-border",
    options = list(
      order = F,
      colReorder = TRUE, dom = "tB", buttons = c(
      "copy","csv", "excel", "pdf", "print"
    ), columnDefs = list(
      list(width = "170px",targets = 0), 
      list(width = "270px",targets = 1), 
      list(width = "270px",targets = 2), 
      list(visible = FALSE, targets = n_col +
      1)), rowGroup = list(dataSrc = n_col + 1)), 
      extensions = c("Buttons", "ColReorder", "RowGroup"),
    rownames = c(
      "Total Costs", "Incremental Costs", 
      "Total QALYs", "Incremental QALYs",
      "ICER",
      "20,000 GBP", "30,000 GBP"
    )
  ) %>% 
  formatStyle(0:2,valueColumns = 0,"border-top" = styleEqual(c("Total QALYs","Total Costs","20,000 GBP","ICER"), c('0.5px groove black','0.5px groove black','0.5px groove black','0.5px groove black')))

}





createICERtablePPT <- function(total_costs = example_TC, 
                               total_qalys = example_TQ,
                               ref_index = 1, 
                               ci = T) {
  
  n_col <- ncol(total_costs)
  
  c_f <- function(x, d = 2, ci = T) {
    
    x <- c(mean(x), quantile(x, probs = c(0.025, 0.975)))
    x <- formatC(x,
                 digits = d, big.mark = ",", format = "f",
                 drop0trailing = F
    )
    
    if (ci) {
      
      x <- paste0(x[1], " (", x[2], "; ", x[3], ")")
      
    }
    else {
      x <- paste0(x[1])
    }
    
    x[grepl("0 \\(", substr(x, 1, 3))] <- ""
    x[grepl("0[.]0 \\(", substr(x, 1, 5))] <- ""
    x[grepl("0[.]00 \\(", substr(x, 1, 6))] <- ""
    x[grepl("0[.]000 \\(", substr(x, 1, 7))] <- ""
    x
  }
  
  mean_C <- apply(total_costs, 2, c_f, 0, ci)
  mean_Q <- apply(total_qalys, 2, c_f, 3, ci)
  
  inc.C <- cbind(total_costs[, 1:n_col] - total_costs[, ref_index])
  inc.C <- apply(inc.C, 2, c_f, 0, ci)
  
  inc.Q <- cbind(total_qalys[, 1:n_col] - total_qalys[, ref_index])
  inc.Q <- apply(inc.Q, 2, c_f, 3, ci)
  
  NB20 <- total_qalys * 20000 - total_costs
  INB20 <- NB20[, ] - NB20[, 1]
  INB20 <- apply(INB20, 2, c_f, 0, ci)
  
  NB30 <- total_qalys * 30000 - total_costs
  INB30 <- NB30[, ] - NB30[, 1]
  INB30 <- apply(INB30, 2, c_f, 0, ci)
  
  icer <- (mean(total_costs[,ref_index] - total_costs[,-ref_index])) / (mean(total_qalys[,ref_index] - total_qalys[,-ref_index]))
  icer <- c("",formatC(icer,format = "f", big.mark = ",", digits = 0))
  
  res_table <- as.data.frame(t(
    data.frame(
      mean_C, inc.C, mean_Q,inc.Q, icer,INB20, INB30,
      stringsAsFactors = F
    )))
  
  rownames(res_table) <- c(
    "Costs (GBP)", 
    "Incremental Costs (GBP)", 
    "QALYs",
    "Incremental QALYs",
    "ICER (GBP)", 
    "INB 20,000 GBP", 
    "INB 30,000 GBP"
  )
  
  res_table <-cbind(rownames(res_table), data.frame(res_table, row.names=NULL))
  
  return(res_table)
  
}