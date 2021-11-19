# UTIL FUNCTIONS
makePPT_Footer <- function(x) {
  ph_with(x = x,
          value = "Dark Peak Analytics",
          location = ph_location_type(type = "ftr"))
}

time_stamp <-  function(x) {
  ph_with(x = x,
          value = format(Sys.Date()),
          location = ph_location_type(type = "dt"))
}

slide_number <-  function(x, num) {
  ph_with(x = x,
          value = paste(num), 
          location = ph_location_type(type = "sldNum"))
}

make_title <- function(x, string ){
  ph_with(x = x, 
          value = string , 
          location = ph_location_type(type = "title"))
}


#placeholderPlot <- 
#  ggplot(data = mtcars, 
#       aes(x = mpg, y = cyl))+
#  geom_point()
#
#placeholderTable <-  
#  mtcars %>% head %>% flextable %>% autofit
#



#=================#
# CREATE NEW FILE #
#=================#

makePowerpoint <- function(title_list,
                           content_list,
                           template_path,
                           target_file){
  
  # load template powerpoint
  my_pres <- read_pptx(template_path)
  
  
  # Loop through some slide content
  for(s in 1:length(content_list)){
    
    # add a slide
    my_pres <- add_slide(x = my_pres)
    
    # set title list_of_titles[[s]]
    my_pres <- make_title(x = my_pres, 
                          string = paste0(title_list[[s]]))
    # add content
    my_pres <- ph_with(x = my_pres, 
                       value = content_list[[s]], 
                       location = ph_location_type(type = "body"))
    }
    
  # add footer, slide numbers and time stamp of slide creation.
  n_slides <- length(my_pres)
  
  for (i_slide in 2:n_slides) {
    
    my_pres <- on_slide(index = i_slide, 
                        makePPT_Footer(my_pres))
    my_pres <- on_slide(index = i_slide, 
                        time_stamp(x = my_pres))
    my_pres <- on_slide(index = i_slide, 
                        slide_number(my_pres, num = i_slide))
    
  }
  
  # create the file in the target file ...
  print(x = my_pres, 
        target = target_file)
  
}

#
#makePowerpoint(title_list = as.list(paste0("title", 1:4)),
#               content_list = list(placeholderPlot, placeholderPlot, placeholderPlot, placeholderPlot),
#               template_path = "template/template_pres.pptx",
#               target_file = "template/test.pptx")