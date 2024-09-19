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

# session creation ----

as_workout <- function(workout){
  #perform standard checks, add additional info, etc.
  if(is_tibble(workout)){
    workout = workout %>% 
      as.list() %>%
      list_transpose()
  }
  
  class(workout) <- c("workout", class(workout))
  workout
}

as_tibble.workout <- function(workout){
  workout %>%
    map(as_tibble) %>% 
    list_rbind()
}

summary.workout <- function(workout){
  workout %>%
    as_tibble() %>% 
    group_by(step) %>% 
    summarise(
      duration = sum(duration),
      target = mean(target),
      n_steps = n()
      #TODO add more relevant stats
      # time in different zones, etc
      
    )
}

plot.workout <- function(workout){
  data = workout %>%
    as_tibble() %>% 
    mutate(
      time0 = 0 + lag(cumsum(duration), default = 0),
      time1 = time0 + duration
    ) 
  data %>% 
    ggplot() +
    geom_rect(
      aes(xmin = time0, xmax = time1, 
          ymin = 0, ymax = target,
          fill = step)
    ) +
    geom_path(
      data = data %>% 
        pivot_longer(c(time0, time1)),
      aes(value, target)) +
    coord_cartesian(ylim = c(0, NA)) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) + 
    labs(x = 'duration', y = 'target')
}

addstep_generic <- function(duration, target){
  res = 
    list(
      duration = translate_duration(duration),
      target = translate_target(target)
    )
  
  return(res)
}

addstep_specific <- function(step, duration, target, times = 1){
  valid_steps = c("warmup", "run", "recovery", "cooldown")
  if(!all((step %in% valid_steps))) cli::cli_abort("step must be one of: {valid_steps}")

  res =
    list(
      list(step = factor(step, valid_steps)) %>% 
        modifyList(addstep_generic(duration, target))
    )
  
  #vectorized
  if(length(step)>1){
    res = res %>% 
      map(list_transpose) %>% 
      unlist(recursive = F)
  }
  
  #repeat step
  if(times>1){
    res = repeatstep(res, times = times)
  }
  
  return(res)
}

combinestep <- function(...){
  c(...)
}

repeatstep <- function(..., times){
  rep(combinestep(...), times)
}

create_workout <- function(...){
  combinestep(...) %>% 
    as_workout()
}

# create_workout(
#   addstep_specific('warmup', 3, 3),
#   addstep_specific('recovery', 3, 3),
#   addstep_specific(c('run', 'recovery'), c(1,4), c(1,4), times = 3),
#   addstep_specific('recovery', 3,3),
#   addstep_specific(c('run', 'recovery'), c(1,4), c(1,4), times = 2),
#   addstep_specific('cooldown', 2,2)
# )

# plan creation ----

as_plan <- function(plan){
  #perform standard checks, add additional info, etc.
  
  #names check
  weekdays = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  names(plan) <- tolower(names(plan)) %>% map_chr(~weekdays[pmatch(.x, tolower(weekdays))])
  
  #add empty row for empty days
  
  class(plan) <- c("plan", class(plan))
  plan
}

as_tibble.plan <- function(plan){
  
  plan %>% 
    map2(., seq_along(.), ~.x %>% map(as_tibble) %>% list_rbind %>% mutate(session_n = .y)) %>% 
    list_rbind(names_to = "day") %>% 
    group_by(day) %>% 
    mutate(session_n = session_n - max(session_n) + n_distinct(session_n)) %>% 
    mutate(day = factor(day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
    ungroup()
}

summary.plan <- function(plan){
  plan %>%
    as_tibble() %>% 
    group_by(day) %>% 
    summarise(
      n_sessions = n_distinct(session_n),
      duration = sum(duration),
      target = mean(target)
      #TODO add more relevant stats
      # time in different zones, etc
      
    )
}

plot.plan <- function(plan){
  data = plan %>% 
    as_tibble() %>% 
    complete(day, session_n) %>% 
    group_by(day, session_n) %>% 
    mutate(
      time0 = 0 + lag(cumsum(duration), default = 0),
      time1 = time0 + duration
    ) 
  data %>% 
    ggplot() +
    geom_rect(
      aes(xmin = time0, xmax = time1, 
          ymin = 0, ymax = target,
          fill = step)
    ) +
    geom_path(
      data = data %>% 
        pivot_longer(c(time0, time1)),
      aes(value, target)) +
    coord_cartesian(ylim = c(0, NA)) +
    facet_grid(day~paste('session', session_n)) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = NA, color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) + 
    labs(x = 'duration', y = 'target')
}

create_plan <- function(...){
  res = list(...)
  
  res %>% 
    as_plan()
}

# plan = create_plan(monday = workout, tuesday = workout, friday = workout, sat = workout)