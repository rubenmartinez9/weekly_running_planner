# functions to save and load sessions

# saving/loading workouts ----

save_workout <- function(workout, name){

  if(!file.exists('output/saved_workouts.RDS')) {
    saveRDS(list(), file = 'output/saved_workouts.RDS')
  }
  saved_workouts <- readRDS("output/saved_workouts.RDS")
  if(name %in% names(saved_workouts)){
    answer = readline('name already in use, do you want to override workout? (y/n): \n')
    if(answer == 'y'){
      saved_workouts[[name]] <- NULL 
    } else {
      cli::cli_alert_danger("workout was not saved")
      return(F)
    }
    
  }
  
  saved_workouts <- append(saved_workouts, list(workout %>% as_tibble()) %>% set_names(name))
  saveRDS(saved_workouts, file = 'output/saved_workouts.RDS')
  cli::cli_alert_success("workout was saved")
  return(T)
}


load_workout <- function(name = NULL){
  saved_workouts <- readRDS("output/saved_workouts.RDS")
  
  if(is.null(name)){
    name = names(saved_workouts)
  }
  
  if(!all(name %in% names(saved_workouts))){
    cli::cli_alert_danger("some workout does not exist. Call {.code names(load_workout())} to see the existing ones")
    return(list())
  }

  res = saved_workouts %>% 
    keep(names(.) %in% name) %>% 
    map(as_workout)
  
  return(res)
}

# load_workout() %>% map(as_tibble)