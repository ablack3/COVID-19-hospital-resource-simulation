library(tidyverse)
# library(queuecomputer)





# how many new hospital beds will we need tomorrow?


# parameters
mild_case_detection_probability <- .8
infection_rate <- .3
simulation_length <- 30
total_resources_available <- 10
resources_in_use_at_start <- 7


# global data structures holding system state
spreading_queue <- c(ceiling(runif(1, min = 6, max = 8)))
hospital_resource_queue <- integer() # infinite queue
hospital_resource <- ceiling(runif(resources_in_use_at_start, 3, 8)) # length must be <= total_resources_available

# endpoints
endpoint_mild_case_quarantined <- 0
endpoint_discharged_safely <- 0
endpoint_died <- 0

# execution traces
trace_num_waiting <- integer(simulation_length)
trace_endpoint_died <- integer(simulation_length)







# modifys the spreading queue and returns the number of people leaving the queue
update_spreading_queue <- function(){
  stopifnot(length(spreading_queue) > 0)
  # how many new cases will be infected today?
  new_infections <- sum(rpois(length(spreading_queue), infection_rate))
  num_symptomatic <- sum(spreading_queue == 0)
  # reduce time remaining by one and remove zeros
  spreading_queue <- subset(spreading_queue - 1, spreading_queue > 0)
  # add new infections to spreading_queue
  spreading_queue <<- c(spreading_queue, rep(6, new_infections))
  return(num_symptomatic)
}




# takes the number of new symptomatic cases today 
# updates the number detected based on detection probabilities
# returns the number need hospital resources
update_num_detected <- function(num_symptomatic){
  
  case_severity <- sample(c("mild", "severe"), num_symptomatic, prob = c(.9, .1), replace = T)
  
  # severe cases are always detected. mild cases are detected with probability = "mild_case_detection_probability"
  endpoint_mild_case_quarantined <<- endpoint_mild_case_quarantined + sum(case_severity == "mild")
  n_detected <<- n_detected + sum(case_severity == "severe") + rbinom(1, size = sum(case_severity == "mild"), prob = mild_case_detection_probability)
  
  
  return(sum(case_severity == "severe")) # number who need hospital resources
}


update_hospital_resource_queue <- function(num_severe_cases){
  # discharge patients from hospital (some die and some survive)
  num_to_discharge <- sum(hospital_resource == 0)
  outcome <- sample(c("died", "survived"), num_to_discharge, replace = T, prob = c(.3, .7))
  endpoint_discharged_safely <<- endpoint_discharged_safely + sum(outcome == "survived")
  endpoint_died <<- endpoint_died + sum(outcome == "died")
  hospital_resource <<- subset(hospital_resource - 1, hospital_resource > 0)
  
  # discharge patient from the queue (some die and some survive)
  num_to_discharge <- sum(hospital_resource_queue == 0)
  outcome <- sample(c("died", "survived"), num_to_discharge, replace = T, prob = c(.5, .5))
  endpoint_discharged_safely <<- endpoint_discharged_safely + sum(outcome == "survived")
  endpoint_died <<- endpoint_died + sum(outcome == "died")
  hospital_resource_queue <<- subset(hospital_resource_queue - 1, hospital_resource_queue > 0)
  
  
  # move patients from queue to hosptial
  resources_available <- total_resources_available - length(hospital_resource)
  if(length(hospital_resource_queue) > 0 & resources_available > 0){
    num_to_move <- min(resources_available, length(hospital_resource_queue))
    hospital_resource <<- c(hospital_resource, hospital_resource_queue[1:num_to_move])
    hospital_resource_queue <<- hospital_resource_queue[-1:-num_to_move]
  }
  
  # how long will the newly diagnosed people need hospital resources?
  utilization_times <- ceiling(runif(num_severe_cases, 3, 8))
  
  # move new patients to hospital
  resources_available <- total_resources_available - length(hospital_resource)
  if(length(utilization_times) > 0 & resources_available > 0){
    num_to_move <- min(resources_available, length(utilization_times))
    hospital_resource <<- c(hospital_resource, utilization_times[1:num_to_move])
    utilization_times <- utilization_times[-1:-num_to_move]
  }
  
  # move remaining new cases to the queue
  if(length(utilization_times) > 0){
    hospital_resource_queue <<- c(hospital_resource_queue, utilization_times)
  }
  
  trace_num_waiting[i] <<- length(hospital_resource_queue) 
  trace_endpoint_died[i] <<- sum(outcome == "died")
}
  
# run the simulation
set.seed(123)
for(i in 1:simulation_length){
    update_spreading_queue() %>% 
      update_num_detected() %>% 
      update_hospital_resource_queue()
}


plot(1:simulation_length, trace_num_waiting, type = 'l')
plot(1:simulation_length, trace_endpoint_died, type = 'l')
