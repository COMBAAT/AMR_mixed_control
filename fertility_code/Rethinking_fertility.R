#Cooment added 7 June 2026
prob <- 0.1
n <- 9999999
fertility <- c("L")
for (i in 1:n) {
  last_state <- tail(fertility, 1)
  if (last_state == "L") {next_state = "PN"}
  if (last_state == "PN") {next_state = ifelse(runif(1) < prob, "F", "L")}
  if (last_state == "F") {next_state = "PN"}
  
  if (next_state == "F") {
    fertility[i] <- "PF"
  }
  fertility[i+1] <- next_state
}
my_table <- table(fertility) / length(fertility)
my_table

# From first principles, these are the relative probabilities of being in the L, P or F states
rL = 1
rP = 1 / (1 - prob)
rF =  prob / (1 - prob) 

denom <- 2 / (1 - prob)

#Actual probabilities of being in the L, P or F states
pL <- (1 - prob) / 2
pP <- 1 / 2
pF <- prob / 2
vec <- c(pF, pL, pP)


#Now adjust to rename the pregnancy when it is followed by fallow period
pPN <- pP - pF
pPF <- pF

vec2 <- c(pF, pL, pPF, pPN)
vec2
my_table

vec2 / my_table

# Now work out how much time in each compartment
DL <- 100
DPN <- 100
DF <- 100
DPF <- 100 # but will be variable

#
denom2 <- DF * pF + DL * pL + DPF * pPF + DPN * pPN
prop_time_fallow <- DF * pF / denom2
prop_time_lactating <- DL * pL / denom2
prop_time_normal_pregnancy <- DPN * pPN / denom2
prop_time_short_pregnancy <- DPF * pPF / denom2

vec3 <- c(prop_time_fallow, prop_time_lactating, prop_time_normal_pregnancy, prop_time_short_pregnancy)
vec3
sum(vec3)



##############################################################################
days_infected <- 15
vec_L <- rep("L", 30)
vec_P <- rep("P", 20)
vec_combined <- c(vec_L, vec_P)
vec_long <- rep(vec_combined, 10000)
vec_long

ans <- c()
start_state <- c()
for (i in 1:10000) {
  start_inf_position <- sample(1:length(vec_long), 1)
  start_state <- c(start_state, vec_long[start_inf_position])
  end_inf_position <- start_inf_position + (days_infected-1)
  chunk <- vec_long[start_inf_position:end_inf_position]
  chunk
  days_lactating_and_infected <- sum(chunk == "L")
  days_lactating_and_infected
  ans <- c(ans, days_lactating_and_infected)
}
ans
start_state
sum(ans[ans > 0]) / sum(start_state == "L")

