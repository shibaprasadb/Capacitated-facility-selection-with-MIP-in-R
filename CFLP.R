#Load the Libraries
library(geosphere)
library(ompr)
library(ompr.roi)
library(ROI.plugin.symphony)
library(tidyverse)

n<- 500 #number of customers
m<- 20 #number of facility centers

set.seed(1234)

fixedcost <- round(runif(m, min=5000, max=10000))

warehouse_locations <- data.frame(
  cen_id=c(1:m),
  cen_y=runif(m, 22.4, 22.6),
  cen_x= runif(m, 88.3, 88.48)
)

customer_locations <- data.frame(
  id=c(1:n),
  y=runif(n, 22.27, 22.99),
  x= runif(n, 88.12, 88.95)
)

#Randomly generate demand and capacity
capacity <- round(runif(m, 1000, 4000))
demand <- round(runif(n, 5, 50))

#The function to compute transport cost
transportcost <- function(i, j) {
  customer <- customer_locations[i, ]
  warehouse <- warehouse_locations[j, ]
  (distm(c(customer$x, customer$y), c(warehouse$x, warehouse$y), fun = distHaversine)/1000)*20
}


model <- MIPModel() %>%
  # 1 iff i gets assigned to SC j
  add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%
  
  # 1 if SC j is built
  add_variable(y[j], j = 1:m, type = "binary") %>%
  
  # Objective function
  set_objective(sum_expr(transportcost(i, j) * x[i, j], i = 1:n, j = 1:m) + 
                  sum_expr(fixedcost[j] * y[j], j = 1:m), "min") %>%
  
  #Demand of customers shouldn't exceed total facility capacities
  add_constraint(sum_expr(demand[i] * x[i, j], i = 1:n) <= capacity[j] * y[j], j = 1:m) %>%
  
  # every customer needs to be assigned to a SC
  add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n) %>% 
  
  # if a customer is assigned to a SC, then this SC must be built
  add_constraint(x[i,j] <= y[j], i = 1:n, j = 1:m)
model

#Solving the model with keeping a gap limit of 5%
result <- solve_model(model, with_ROI(solver = "symphony",
                                      verbosity=-1, gap_limit=5))
result %>% 
  get_solution(x[i,j]) %>%
  filter(value ==1) %>%  
  select(i, j) %>%
  rename(id=i, cen_id=j) %>%
  inner_join(warehouse_locations, by='cen_id') %>%
  inner_join(customer_locations, by='id')-> facility_allotment

#Plotting the results
ggplot()+
  geom_point(data=facility_allotment, aes(x=x, y=y, col=as.factor(cen_id)))+
  geom_point(data = facility_allotment %>%
               distinct(cen_id, .keep_all = T), aes(x=cen_x, y=cen_y), shape=17, size=5)+
  theme_bw()+
  theme(legend.position = 'none')+
  ggtitle('Capacitated Facility Location Problem')



