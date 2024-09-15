# functions to create a single workout session

# basic funs ----
translate_target <- function(target){
  # Take some unit of target (e.g. Z1/Z2/Z3...) and convert it to the relevant unit for Garmin. May need additional args like max HR
  target
}

translate_duration <- function(duration){
  # Take some unit of duration (time, distance) and convert it to the relevant unit for Garmin
  duration
}

addstep_generic <- function(duration, target){
  res = 
    list(
      duration = translate_duration(duration),
      target = translate_target(target)
    )
  
  return(res)
}

addstep_specific <- function(step, duration, target){
  valid_steps = c("warmup", "run", "recovery", "cooldown")
  if(!(step %in% valid_steps)) cli::cli_abort("step must be one of: {valid_steps}")
  
  res =
    list(
      c(
        step = step,
        addstep_generic(duration, target)
      )
    )
  return(res)
}

combinestep <- function(...){
  c(...)
}

repeatstep <- function(step, times){
  rep(step, times)
}
