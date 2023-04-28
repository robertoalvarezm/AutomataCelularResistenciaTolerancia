library(viridis)
######################################

# 1.  Si en la celda central  
#     en su vecindad hay ,al menos, una planta tolerante 
#     existe una probabilidad p que el
#     escarabajo se mueva hacia esa planta tolerante

# 2.  Si en las vecindades de la celda central tolerante
#     sólo hay plantas resistentes, 
#     se pueden presentar dos posibles escenarios:
#       a) Que el escarabajo no se mueva de la planta tolerante (estado inicial)
#       b) Que el escarabajo se mueva a cualquier planta resistente con la misma 
#     probabilidad, dado que la planta tolerante (estado inicial) tiene un 
#     porcentaje de daño alto

#######################################



#######################################

### Regresión

######################################
seeds_tol<-function(consumo){
  
  return(rnorm(1,476.043,54.878) + (rnorm(1,-4.784,1.055) )*consumo)
}

seed_res<-function(consumo){
  
  return(3)
}
############################3

# Set the size of the grid
grid.size <- 100



# Create a matrix to represent the grid
grid <- matrix(rep(0,grid.size*grid.size),nrow=grid.size, ncol=grid.size)

# Select gradient of resistant: tolerant
res<-0.5
tol<-1-res

initial_configuration<-function(r,tam,rejilla){
for(i in 1:tam){
  for(j in 1:tam){
    x<-runif(1)
    #print(x)
    #print(i)
    #print(j)
    if(x < r ){
      rejilla[i,j]<-1
    }else{
      rejilla[i,j]<-2
    }
  }
}
  return(rejilla)
}

grid<-initial_configuration(res,grid.size,grid) 
# Set the initial values of the grid
# 1: Resistant
# 2: Tolerant
# 3: Resistant  +  Insect
# 4: Tolerant +    Insect

# First, an Insect in the middle 

if(grid[grid.size/2,grid.size/2]==1){
  grid[grid.size/2,grid.size/2] <- 3  
}else{
  grid[grid.size/2,grid.size/2] <- 4
}
#Initial Insects 
x<-sample(1:100,20)
for( i in 1:9){
  if(grid[x[i],x[i+1]] == 1){
    grid[x[i],x[i+1]] <- 3  
  }else{
    grid[x[i],x[i+1]] <- 4
  }
  
}

farben<-function(g){
  kleuren<-matrix(rep(0,grid.size*grid.size),nrow=grid.size, ncol=grid.size)
  for(i in 1:grid.size)
    for(j in 1:grid.size){
      if(g[i,j]==1){
        kleuren[i,j]="#440154FF"
      }else if(g[i,j]==2){
        kleuren[i,j]="#31688EFF"
      }else if(g[i,j]==3){
        kleuren[i,j]="#35B779FF"
      }else{
        kleuren[i,j]="#FDE725FF"
      }
    }
  return(kleuren)
}

colors<-farben(grid)


pheatmap::pheatmap(grid,color=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"),cluster_rows = FALSE,
                   cluster_cols = FALSE,
                   legend_labels = c("R","T","R+I","T+I"),
                   legend_breaks = c(1,2,3,4)
                   )


# Automata's Rules


############

# 1. If there is a tolerant plant surrounding a cell with an insect, 
# there is a probability that the insect moves toward that plant at the next step
# 2. 
# 3. The level of damage is indicated by the number of time steps 
# 4. If the level of damage
############

# Set the probability of an individual becoming eaten by an insect (Rule 1)
infection.probability <- 0.25

# Set the probability of an infected (Rule 2)
infection.probability_2 <- 0.20

# Threshold danio
umbral_danio<-30

# Set the maximum number of iterations
max.iterations <- 200

#Contabiliza el número de :
# 1: Resistant
# 2: Tolerant
# 3: Resistant  +  Insect
# 4: Tolerant +    Insect
# Para cada tiempo t

out<-data.frame(time=1:max.iterations,
                Resistants=rep(0,max.iterations),
                Tolerants=rep(0,max.iterations),
                Insect_Resistants=rep(0,max.iterations),
                Insect_Tolerants=rep(0,max.iterations))

danio<-matrix(rep(0,grid.size*grid.size),nrow=grid.size, ncol=grid.size)

new.grid<-grid
# Run the cellular automata
for(iteration in 1:max.iterations){
  # Create a new matrix to store the updated grid
  #new.grid <- matrix(rep(1,grid.size*grid.size), nrow=grid.size, ncol=grid.size)

  # Loop over each cell in the grid
  for (i in 1:grid.size) {
    for (j in 1:grid.size) {
      # RULE 1
      # If the cell is TOLERANT or RESITANT and currently with 
      # an insect
        if (grid[i,j] == 4 | grid[i,j]==3) {
          #The level of damage increases one unit
          #danio[i,j]<-(danio[i,j] + 1)/max.iterations
          # Check the surrounding cells to see if any are TOLERANT 
          #tolerant.neighbors <- 0
          if (i > 1 && grid[i-1,j] == 2) {if(runif(1) < infection.probability) new.grid[i-1,j] = 4}
          if (i < grid.size && grid[i+1,j] == 2) {if (runif(1) < infection.probability) new.grid[i+1,j] = 4}
          if (j > 1 && grid[i,j-1] == 2) {if (runif(1) < infection.probability)  new.grid[i,j-1] = 4}
          if (j < grid.size && grid[i,j+1] == 2) {if (runif(1) < infection.probability) new.grid[i,j+1] = 4}
        
          }
      # If the cell is TOLERANT and currently with an insect
      if (grid[i,j] == 4 & danio[i,j] > umbral_danio) {
        # Check the surrounding cells to see if any are TOLERANT
        #danio[i,j]<-(danio[i,j] +1)/max.iterations
        if (i > 1 && grid[i-1,j] == 1) {if (runif(1) <infection.probability_2) new.grid[i-1,j] = 3}
        if (i < grid.size && grid[i+1,j] == 1) {if (runif(1) < infection.probability_2) new.grid[i+1,j] = 3}
        if (j > 1 && grid[i,j-1] == 1) {if (runif(1) < infection.probability_2)  new.grid[i,j-1] = 3}
        if (j < grid.size && grid[i,j+1] == 1) {if (runif(1) < infection.probability_2) new.grid[i,j+1] = 3}

      }
    }
  }
  
  # Update the grid and danio with the new values
   for(i in 1:grid.size)
     for(j in 1:grid.size){
       if(grid[i,j]==3 | grid[i,j]==4){
         danio[i,j]<-danio[i,j] + 100/max.iterations
       }
     }
   grid <- new.grid
   #print(table(grid)[[2]])
   if(any(grid==3)){
   out[iteration,2]<-table(grid)[[1]]
   out[iteration,3]<- table(grid)[[2]]
   out[iteration,4] <-table(grid)[[3]]
   out[iteration,5] <-table(grid)[[4]]}else{
   out[iteration,2]<-table(grid)[[1]]
   out[iteration,3]<- table(grid)[[2]]
   out[iteration,4] <-0  
   }
   
   
}
pheatmap::pheatmap(grid,color=c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"),cluster_rows = FALSE,
                   cluster_cols = FALSE,
                   legend_labels = c("R","T","R+I","T+I"),
                   legend_breaks = c(1,2,3,4)
)




library(ggplot2)
library(tidyr)

df_long <- gather(out, key = "variable", value = "value", -time)

ggplot(df_long, aes(x = time, y = value, color = variable)) +
  geom_line() +
  labs(title = "",
       x = "Tiempo",
       y = "Número de plantas") +
  theme_minimal()


boxplot(danio)

cuentas_final<-table(grid)
write.csv(cuentas_final,"~/Desktop/Cuentas_final_res0.1.csv")
write.csv(danio,"~/Desktop/Danio_final_res0.1.csv")
write.csv(grid,"~/Desktop/Configuracion_final_res0.1.csv")


