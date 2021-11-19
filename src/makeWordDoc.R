#library(officer)
#library(magrittr)

makeWordDoc <- 
  function(template_path,
           app_user_name,
           figures,
           figure_text,
           target_file){
    
    DOC <- read_docx(template_path) %>%
      
      body_add_break() %>% 
      
      body_add_par(value = "Simple Model Results", 
                   style = "Title") %>%
      
      body_add_par(value =  "", 
                   style = "Normal") %>% 
      
      body_add_par(value = paste0("This analysis was run from the Lean Shiny App by ",
                                  app_user_name,
                                  " on ",
                                  Sys.Date())) %>%
      
      body_add_break()
    
    body_end_section_continuous(DOC)
    
    
    for(f in 1:length(figures)){
      
      body_add_par(x = DOC, 
                   value = paste0("Figure ", f),
                   style = "Normal")
      
      body_add_par(x= DOC,
                   value =  "", 
                   style = "Normal")
      
      body_add_par(x = DOC, 
                   value = paste0(figure_text[[f]]),
                   style = "Normal")
      
      body_add_par(x= DOC,
                   value =  "", 
                   style = "Normal")
      
      body_add_gg(x = DOC,
                  width = 7, 
                  height = 4,
                  value = figures[[f]])
      
      body_add_break(x = DOC)
      
    } 
    
    body_end_section_landscape(DOC)
    
    body_add_par(x = DOC, 
                 value = "Parameters", 
                 style = "Normal") %>% 
      
      body_add_par(value =  "", 
                   style = "Normal") %>% 
      
      body_add_par(value =  "The following parameter values were used by the user to generate the graphs above.", 
                   style = "Normal") %>%
      
      #body_add_table(paramTable) %>%  
      
      print(target = target_file)
    
  }

#list_of_plots <- 
#lapply(X = 1:5, FUN = function(x){
#  ggplot(data = iris, 
#         aes(Sepal.Length, Petal.Length)) + 
#           geom_point(col = x)
#         
#         })
#
#list_of_figure_text <- lapply(X = 1:5, 
#                              FUN = function(x){
#  
#                                paste0("This particular plot relates to ", x)
#                                
#                                })
#
#
#
#makeWordDoc(template_path = "C:/Users/Robert/Desktop/template.docx",
#            app_user_name = "",
#            paramTable = head(mtcars),
#            figures = list_of_plots,
#            figure_text = list_of_figure_text,
#            output_file_path = "C:/Users/Robert/Desktop")


#body_add_par("Table of Contents", style = "heading 1") %>% 
#body_add_toc(level = 2) %>% 
#body_add_par("Table of figures", style = "heading 1") %>% 
#body_add_toc(style = "Image Caption") %>% 
#body_add_par("Table of tables", style = "heading 1") %>% 
#body_add_toc(style = "Table Caption")



