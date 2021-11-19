# utility functions


# btnTaskCompleted
# when a task is completed, i.e. tab closed,
# this funciton changes the icon, background col, and hover animation
btnTaskCompleted <- function(id, color, session) {
    runjs(paste0('document.getElementById("', id, '").setAttribute("class","action-button bttn bttn-material-flat shiny-bound-input");'))
    runjs(paste0('document.getElementById("', id, '").style.background="', color, '";'))
    updateActionButton(session, id, icon = icon("check-square"))
}

btnLookActived <- function(id, color = "#548a17",session) {
    runjs(paste0('document.getElementById("', id, '").setAttribute("class","action-button bttn bttn-jelly bttn-md bttn-success bttn-block bttn-no-outline shiny-bound-input");'))
    runjs(paste0('document.getElementById("', id, '").style.background="', color, '";'))
    # updateActionButton(session, id, icon = icon("check-square"))

}

# closeTabBtn
    # a simple funciton to create close buttons for tabs
    # NEEDS <<- asignment! bc function is used on sever side within functions
closeTabBtn <<- function(id){
                    actionBttn(id, "Confirm", color = "success", block = F, style = "jelly",size ="lg")
                }

# disableBackgroundBtns
    # when a tab is open, this function disables all other buttons
disableBackgroundBtns = function(task_counter){
        # disables all tabs when you open one
        # prevents user form opening multiple tabs
        other_btns = names(task_counter)
        lapply(other_btns, disable)
        disable("run_model") # you have to close the tab first
}

