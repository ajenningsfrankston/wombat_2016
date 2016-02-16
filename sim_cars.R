#
#
# cars on a linear grid, with a return path (simple multi-class commuter)
#
# columns: location, 
#          direction (up/down)
#          loading/unloading
#          type (1,2,4)
#          no passengers
#          p1
#          p2
#          p3
#          p4  (arrival times)
#


trace <- FALSE

no_cars <- 50 
path_length <- 1000 
no_types <- 3

sample_size = 1000


c1_times = c()
c2_times = c()
c3_times = c()

types <- c(1,2,4)

cars <- matrix(nrow=no_cars, ncol= 9)
#
start_loc <- path_length - 1
loc <- start_loc

for (i in 1:no_cars) {
  loc <- loc -1 
  cars[i,1] <- loc
  cars[i,2] <- -1
  cars[i,3] <- 0
# types 1 to 3 uniformly distributed
  cars[i,4] <- as.integer(1+2.99*runif(1)) 
 
  cars[i,5:9] <- 0

} 
# stops at 250m intervals on a 5km line
# 
# columns: no 1, no 4, no 8
#
#
no_stops <- 20 
max_queue <- 100

stop_count <- matrix(0,nrow=20,ncol=4)

for (i in 1:no_stops){ 
  stop_count[i,1] <- 250*i/5 # stop location in time steps (5m of movement at 60km/hr) - 250m intervals
}

class_queue = array(0,dim=c(3,20,max_queue))

t <- 0 
epoch <- 10000
enough_samples <- FALSE

while (( t < epoch) &&(!enough_samples))
{
  
t <- t + 1

#
# assume peak passenger is at 8am, with sigma of 45 mins (normal distribution)
#
  
mflow = (1000/10)/(3*3*20*60)

#
# time since start of session (seconds): 

z = (t/3 - (60*60))/(45*60*60)
p = dnorm(z) 

lambda = p*mflow

# probability of a passenger appearing roughly proportional to lambda. 

for (i in 1:no_stops) {
  for (j in 1:3) {
    if (runif(1) < lambda/3) {
      if (trace) { cat("passenger waiting at stop ",i,"type",j,"time",t,"\n")}
      stop_count[i,j+1] <- stop_count[i,j+1] + 1
    
      class_queue[j,i,stop_count[i,j+1]] <- t
  
      
    }
  }
}


for (i in 1:no_cars) {

# at a stop and going backward ? 
  
  if (((cars[i,1] %% 50) == 0) & (cars[i,2] < 0)) 
  {
# if this stop has the longest queue for car's type then turn around 
    
    type = cars[i,4]
    stop_no = cars[i,1]/50
    mloc = which.max(stop_count[,type+1])
    if (mloc == stop_no) {
      cars[i,2] = 1
    }
  }
  
# at a stop and going forward? 
  
  if (((cars[i,1] %% 50) == 0) & (cars[i,2] > 0)) 
  {
    
    if (cars[i,3] > 0 )   # already  loading/unloading
    {
      cars[i,3] <- cars[i,3] - 1
    }
    else
    {
      stop_no = cars[i,1]/50
    
      type = cars[i,4]
      no_passengers = cars[i,5]
      at_station <- (stop_no == 20)
      
      if ((stop_count[stop_no,type+1] > 0) & (! at_station))  # passenger waiting 
      {
        if (cars[i,5] < types[cars[i,4]])   # not full
        { 
          if (trace) { cat("pickup at stop ",stop_no,"car no",i,"type",type,"\n") }
 
# time is counted from when the passenger starts to queue at the stop...
#
          qloc <- stop_count[stop_no,type+1]
          pickup_time <- class_queue[type,stop_no,qloc]
          
# shift the queued passengers left
#
         
          len = length(class_queue[type,stop_no,])
          for (k in 1:(len-1)) {
            class_queue[type,stop_no,k] <- class_queue[type,stop_no,k+1]
          }
          class_queue[len] <- 0
          stop_count[stop_no,type+1] <- stop_count[stop_no,type+1] - 1
         
          
          cars[i,no_passengers+6] <- pickup_time 
          cars[i,5] <- cars[i,5] + 1 
          cars[i,3] <- 4  } # countdown to loading/unloading 
     
        
      }
      
      if(at_station)
      {
        type = cars[i,4]
        no_passengers = cars[i,5]
        for (j in 1:no_passengers)
        {
          if(type ==1){
            c1_times <- append(c1_times,(t-cars[i,j+5])) 
          } else
          if(type ==2){
            c2_times <- append(c2_times,(t-cars[i,j+5])) 
          } else
          if(type ==3){
            c3_times <- append(c3_times,(t-cars[i,j+5])) 
          }
          sample_count = max(length(c1_times),length(c2_times),length(c3_times))
          if (sample_count > sample_size)
          { enough_samples <- TRUE }
        }
       
        cars[i,5:9] <- 0   # drop passengers
        if (trace) {cat("car no",i,"dropped passengers \n")}
        cars[i,2] <- -1
        
      }
      
    }
  }
  
  # at start and decreasing? turn around
  
  if ((cars[i,2] < 0) & (cars[i,1] <= 1))
  { 
    cars[i,2] <- 1 
    cars[i,1] <- 1
  }
  
  # at end and increasing? turn around 
  
  if ((cars[i,2] > 0) & (cars[i,1] >= path_length))
  { 
    cars[i,2] <- -1 
    cars[i,1] <- path_length
  }
  
  if (cars[i,2] < 0)
  {
    cars[i,1] <- cars[i,1] - 1 
  }
  
  cars[i,1] <- cars[i,1] + cars[i,2]
 
  }

}

c1_times <- c1_times/180
c2_times <- c2_times/180
c4_times <- c3_times/180

par(mfrow = c(3,1))
hist(c1_times,xlim=c(0,30))
hist(c2_times,xlim=c(0,30))
hist(c4_times,xlim=c(0,30))




