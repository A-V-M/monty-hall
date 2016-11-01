
monty_hall_sim <- function(n_doors,n_opened,opts){
#TO-DO 
# A) ADD INFORMATION GAINED FROM OPENING DOORS
# B) ADD OPTION TO CHOOSE DOORS BASED ON RANDOM SWITCH, PROBABILISTIC SWITCH OR ALWAYS SWITCH
# n_doors <- 3
# 
# n_opened <- 1
# 
# opts <-1
# 

doors <- seq(1,n_doors)

prize <- sample(doors,1)

chosen_door <- sample(doors,1)

doors_available <- doors [! doors %in% c(prize,chosen_door)]

probs <- vector("numeric",n_doors)

probs[doors] <- 1/n_doors

if (length(doors_available) > 1 )
{

doors_opened <- sample(doors_available,n_opened)
} else
  
{doors_opened <- doors_available}

  

doors_left <- doors[! doors %in% doors_opened]

entropy_car <- -sum(probs*log2(probs))



# P1 is the conditional probability of the prize being behind the initial chosen door
# p(prize|door closed)

P1 <- 1 / n_doors

# P2 is the conditional probability of the prize NOT being behind the initial chosen door
# p(~car|door closed)

P2 <- (1-P1)/(n_doors - n_opened -1)

probs[! doors %in% chosen_door] <- P2
probs[doors_opened] <- 0
entropy_after <- -sum(probs*log2(probs), NA, na.rm = TRUE)

switch(opts,{},
{
probs[c(chosen_door, doors_opened)] <- 0
},

{

probs[doors] <- 1/length(doors_left)

probs[doors_opened] <- 0

entropy_after <- -sum(probs*log2(probs), NA, na.rm = TRUE)

})

MI <- entropy_car - entropy_after

machine_guess <- sample(doors,1,replace=FALSE,probs)

if (chosen_door != machine_guess) {switched = 1} else {switched = 0}

if (prize == machine_guess) {correct = 1} else {correct = 0}

sim_results <- data.frame(prize,chosen_door,machine_guess,switched,correct,MI/entropy_car)

return(sim_results)

}

