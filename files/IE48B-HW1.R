setwd("C:/Users/Pýnar YILDIRIM/Desktop/ie48b/hw1")
getwd()
rm(list=ls())
library(scatterplot3d)
library(data.table)
library(ggplot2)

#Data of each dimension is read:

x = read.table("C:/Users/Pýnar YILDIRIM/Desktop/ie48b/hw1/uWaveGestureLibrary_X_TRAIN")
y = read.table("C:/Users/Pýnar YILDIRIM/Desktop/ie48b/hw1/uWaveGestureLibrary_Y_TRAIN")
z = read.table("C:/Users/Pýnar YILDIRIM/Desktop/ie48b/hw1/uWaveGestureLibrary_Z_TRAIN")


#X values are seperated according to the classes:
  
head(x)

print("Class 1 x")
class1_x = x[x$V1==1,]
head(class1_x)
class1_x = class1_x[,-1]

print("Class 2 x")
class2_x = x[x$V1==2,]
head(class2_x)
class2_x = class2_x[,-1]

print("Class 3 x")
class3_x = x[x$V1==3,]
head(class3_x)
class3_x = class3_x[,-1]


print("Class 4 x")
class4_x = x[x$V1==4,]
head(class4_x)
class4_x = class4_x[,-1]



print("Class 5 x")
class5_x = x[x$V1==5,]
head(class5_x)
class5_x = class5_x[,-1]



print("Class 6 x")
class6_x = x[x$V1==6,]
head(class6_x)
class6_x = class6_x[,-1]


print("Class 7 x")
class7_x = x[x$V1==7,]
head(class7_x)
class7_x = class7_x[,-1]



print("Class 8 x")
class8_x = x[x$V1==8,]
head(class8_x)
class8_x = class8_x[,-1]





#Y values are seperated according to the classes:
  
head(y)


print("Class 1 y")
class1_y = y[y$V1==1,]
head(class1_y)
class1_y = class1_y[,-1]


print("Class 2 y")
class2_y = y[y$V1==2,]
head(class2_y)
class2_y = class2_y[,-1]

print("Class 3 y")
class3_y = y[y$V1==3,]
head(class3_y)
class3_y = class3_y[,-1]


print("Class 4 y")
class4_y = y[y$V1==4,]
head(class4_y)
class4_y = class4_y[,-1]



print("Class 5 y")
class5_y = y[y$V1==5,]
head(class5_y)
class5_y = class5_y[,-1]



print("Class 6 y")
class6_y = y[y$V1==6,]
head(class6_y)
class6_y = class6_y[,-1]


print("Class 7 y")
class7_y = y[y$V1==7,]
head(class7_y)
class7_y = class7_y[,-1]



print("Class 8 y")
class8_y = y[y$V1==8,]
head(class8_y)
class8_y= class8_y[,-1]


#Z values are seperated according to the classes:
  
head(z)


print("Class 1 z")
class1_z = z[z$V1==1,]
head(class1_z)
class1_z = class1_z[,-1]


print("Class 2 z")
class2_z = z[z$V1==2,]
head(class2_z)
class2_z = class2_z[,-1]

print("Class 3 z")
class3_z = z[z$V1==3,]
head(class3_z)
class3_z = class3_z[,-1]


print("Class 4 z")
class4_z = z[z$V1==4,]
head(class4_z)
class4_z = class4_z[,-1]


print("Class 5 z")
class5_z = z[z$V1==5,]
head(class5_z)
class5_z = class5_z[,-1]



print("Class 6 z")
class6_z= z[z$V1==6,]
head(class6_z)
class6_z = class6_z[,-1]


print("Class 7 z")
class7_z = z[z$V1==7,]
head(class7_z)
class7_z = class7_z[,-1]



print("Class 8 z")
class8_z = z[z$V1==8,]
head(class8_z)
class8_z= class8_z[,-1]


#1)

#Velocities are calculated as the cumulative sum of accelerations and similarly locations at each time are calculated as the cumulative sum of velocities.

#3D PLOT for CLASS 1

#CLASS 1 PLOTTING

t_class1_x=as.data.frame(t(class1_x[,]))
velocity_class1_x=cumsum(t_class1_x)
loc_class1_x=cumsum(velocity_class1_x)

t_class1_y=as.data.frame(t(class1_y[,]))
velocity_class1_y=cumsum(t_class1_y)
loc_class1_y=cumsum(velocity_class1_y)

t_class1_z=as.data.frame(t(class1_z[,]))
velocity_class1_z=cumsum(t_class1_z)
loc_class1_z=cumsum(velocity_class1_z)

scatterplot3d(  x=loc_class1_x[,1], y=loc_class1_y[,1], z=loc_class1_z[,1], main="3D Scatter Plot for Gesture Class1",
                xlab = "X", ylab= "Y", zlab="Z")


#3D PLOT for CLASS 2

#CLASS 2 PLOTTING

t_class2_x=as.data.frame(t(class2_x[,]))
velocity_class2_x=cumsum(t_class2_x)
loc_class2_x=cumsum(velocity_class2_x)

t_class2_y=as.data.frame(t(class2_y[,]))
velocity_class2_y=cumsum(t_class2_y)
loc_class2_y=cumsum(velocity_class2_y)

t_class2_z=as.data.frame(t(class2_z[,]))
velocity_class2_z=cumsum(t_class2_z)
loc_class2_z=cumsum(velocity_class2_z)

scatterplot3d(x=loc_class2_x[,2], y=loc_class2_y[,2], z=loc_class2_z[,2],main="3D Scatter Plot for Gesture Class2",
              xlab = "X", ylab= "Y", zlab="Z")

#3D PLOT for CLASS 3

#CLASS 3 PLOTTING

t_class3_x=as.data.frame(t(class3_x[,]))
velocity_class3_x=cumsum(t_class3_x)
loc_class3_x=cumsum(velocity_class3_x)

t_class3_y=as.data.frame(t(class3_y[,]))
velocity_class3_y=cumsum(t_class3_y)
loc_class3_y=cumsum(velocity_class3_y)

t_class3_z=as.data.frame(t(class3_z[,]))
velocity_class3_z=cumsum(t_class3_z)
loc_class3_z=cumsum(velocity_class3_z)

scatterplot3d(x=loc_class3_x[,3], y=loc_class3_y[,3], z=loc_class3_z[,3],main="3D Scatter Plot for Gesture Class3",
              xlab = "X", ylab= "Y", zlab="Z")


#3D PLOT for CLASS 4

#CLASS 4 PLOTTING

t_class4_x=as.data.frame(t(class4_x[,]))
velocity_class4_x=cumsum(t_class4_x)
loc_class4_x=cumsum(velocity_class4_x)

t_class4_y=as.data.frame(t(class4_y[,]))
velocity_class4_y=cumsum(t_class4_y)
loc_class4_y=cumsum(velocity_class4_y)

t_class4_z=as.data.frame(t(class4_z[,]))
velocity_class4_z=cumsum(t_class4_z)
loc_class4_z=cumsum(velocity_class4_z)

scatterplot3d(x=loc_class4_x[,3], y=loc_class4_y[,3], z=loc_class4_z[,3],main="3D Scatter Plot for Gesture Class4",
              xlab = "X", ylab= "Y", zlab="Z")


#3D PLOT for CLASS 5

#CLASS 5 PLOTTING

t_class5_x=as.data.frame(t(class5_x[,]))
velocity_class5_x=cumsum(t_class5_x)
loc_class5_x=cumsum(velocity_class5_x)

t_class5_y=as.data.frame(t(class5_y[,]))
velocity_class5_y=cumsum(t_class5_y)
loc_class5_y=cumsum(velocity_class5_y)

t_class5_z=as.data.frame(t(class5_z[,]))
velocity_class5_z=cumsum(t_class5_z)
loc_class5_z=cumsum(velocity_class5_z)

scatterplot3d(x=loc_class5_x[,14], y=loc_class5_y[,14], z=loc_class5_z[,14],main="3D Scatter Plot for Gesture Class5",
              xlab = "X", ylab= "Y", zlab="Z")


#3D PLOT for CLASS 6

#CLASS 6 PLOTTING

t_class6_x=as.data.frame(t(class6_x[,]))
velocity_class6_x=cumsum(t_class6_x)
loc_class6_x=cumsum(velocity_class6_x)

t_class6_y=as.data.frame(t(class6_y[,]))
velocity_class6_y=cumsum(t_class6_y)
loc_class6_y=cumsum(velocity_class6_y)

t_class6_z=as.data.frame(t(class6_z[,]))
velocity_class6_z=cumsum(t_class6_z)
loc_class6_z=cumsum(velocity_class6_z)

scatterplot3d(x=loc_class6_x[,6], y=loc_class6_y[,6], z=loc_class6_z[,6],main="3D Scatter Plot for Gesture Class6",
              xlab = "X", ylab= "Y", zlab="Z")


#3D PLOT for CLASS 7

#CLASS 7 PLOTTING

t_class7_x=as.data.frame(t(class7_x[,]))
velocity_class7_x=cumsum(t_class7_x)
loc_class7_x=cumsum(velocity_class7_x)

t_class7_y=as.data.frame(t(class7_y[,]))
velocity_class7_y=cumsum(t_class7_y)
loc_class7_y=cumsum(velocity_class7_y)

t_class7_z=as.data.frame(t(class7_z[,]))
velocity_class7_z=cumsum(t_class7_z)
loc_class7_z=cumsum(velocity_class7_z)

scatterplot3d(x=loc_class7_x[,7], y=loc_class7_y[,7], z=loc_class7_z[,7],main="3D Scatter Plot for Gesture Class7",
              xlab = "X", ylab= "Y", zlab="Z")


#3D PLOT for CLASS 8

#CLASS 8 PLOTTING

t_class8_x=as.data.frame(t(class8_x[,]))
velocity_class8_x=cumsum(t_class8_x)
loc_class8_x=cumsum(velocity_class8_x)

t_class8_y=as.data.frame(t(class8_y[,]))
velocity_class8_y=cumsum(t_class8_y)
loc_class8_y=cumsum(velocity_class8_y)

t_class8_z=as.data.frame(t(class8_z[,]))
velocity_class8_z=cumsum(t_class8_z)
loc_class8_z=cumsum(velocity_class8_z)

scatterplot3d(x=loc_class8_x[,8], y=loc_class8_y[,8], z=loc_class8_z[,8],main="3D Scatter Plot for Gesture Class8",
              xlab = "X", ylab= "Y", zlab="Z")


#An instance of each class is plotted on the 3D space as above. Most of them are recognizable except some incorrect sliding moves in the beginnings and ends. Instances of Class 3 and Class 4 are odd. 

#2)

#To obtain a simpler representation, I combined X,Y and Z coordinates and calculated euclidean distance from the origin to each point of each instance for all classes. 


distances_class1 = data.frame()

for( i in 1:nrow(class1_x)){
  for (j in 1:ncol(class1_z)){
    distances_class1[i,j] = sqrt(sum(class1_x[i,j]**2 + class1_y[i,j]**2 + class1_z[i,j]**2))
  }
}


library(ggplot2)

ggplot()+geom_line(aes(x=1:ncol(distances_class1[1,]), y=as.numeric(distances_class1[1,])))+labs(title="Distances to Origin 1st Instance Class1", y="Distance", x="Point")

hist(as.numeric(distances_class1[1,]), main="Histogram of 1st Instance Class1", xlab="Distance to Origin")



distances_class2 = data.frame()

for( i in 1:nrow(class2_x)){
  for (j in 1:ncol(class2_z)){
    distances_class2[i,j] = sqrt(sum(class2_x[i,j]**2 + class2_y[i,j]**2 + class2_z[i,j]**2))
  }
}

ggplot()+geom_line(aes(x=1:ncol(distances_class2[2,]), y=as.numeric(distances_class2[2,])))+labs(title="Distances to Origin 2nd Instance Class2", y="Distance", x="Point")


hist(as.numeric(distances_class2[2,]), main="Histogram of 2nd Instance Class2", xlab="Distance to Origin")


distances_class3 = data.frame()

for( i in 1:nrow(class3_x)){
  for (j in 1:ncol(class3_z)){
    distances_class3[i,j] = sqrt(sum(class3_x[i,j]**2 + class3_y[i,j]**2 + class3_z[i,j]**2))
  }
}


ggplot()+geom_line(aes(x=1:ncol(distances_class3[3,]), y=as.numeric(distances_class3[3,])))+labs(title="Distances to Origin 3rd Instance Class3", y="Distance", x="Point")

hist(as.numeric(distances_class3[3,]), main="Histogram of 3rd Instance Class3", xlab="Distance to Origin")


distances_class4 = data.frame()

for( i in 1:nrow(class4_x)){
  for (j in 1:ncol(class4_z)){
    distances_class4[i,j] = sqrt(sum(class4_x[i,j]**2 + class4_y[i,j]**2 + class4_z[i,j]**2))
  }
}


ggplot()+geom_line(aes(x=1:ncol(distances_class4[3,]), y=as.numeric(distances_class4[3,])))+labs(title="Distances to Origin 3rd Instance Class4", y="Distance", x="Point")


hist(as.numeric(distances_class4[3,]), main="Histogram of 3rd Instance Class4", xlab="Distance to Origin")



distances_class5 = data.frame()

for( i in 1:nrow(class5_x)){
  for (j in 1:ncol(class5_z)){
    distances_class5[i,j] = sqrt(sum(class5_x[i,j]**2 + class5_y[i,j]**2 + class5_z[i,j]**2))
  }
}


ggplot()+geom_line(aes(x=1:ncol(distances_class5[14,]), y=as.numeric(distances_class5[14,])))+labs(title="Distances to Origin 14th Instance Class5", y="Distance", x="Point")

hist(as.numeric(distances_class5[14,]), main="Histogram of 14th Instance Class5", xlab="Distance to Origin")


distances_class6 = data.frame()

for( i in 1:nrow(class6_x)){
  for (j in 1:ncol(class6_z)){
    distances_class6[i,j] = sqrt(sum(class6_x[i,j]**2 + class6_y[i,j]**2 + class6_z[i,j]**2))
  }
}


ggplot()+geom_line(aes(x=1:ncol(distances_class6[6,]), y=as.numeric(distances_class6[6,])))+labs(title="Distances to Origin 6th Instance Class6", y="Distance", x="Point")


hist(as.numeric(distances_class6[6,]), main="Histogram of 6th Instance Class6", xlab="Distance to Origin")



distances_class7 = data.frame()

for( i in 1:nrow(class7_x)){
  for (j in 1:ncol(class7_z)){
    distances_class7[i,j] = sqrt(sum(class7_x[i,j]**2 + class7_y[i,j]**2 + class7_z[i,j]**2))
  }
}


ggplot()+geom_line(aes(x=1:ncol(distances_class7[7,]), y=as.numeric(distances_class7[7,])))+labs(title="Distances to Origin 7th Instance Class7", y="Distance", x="Point")


hist(as.numeric(distances_class7[7,]), main="Histogram of 7th Instance Class7", xlab="Distance to Origin")



distances_class8 = data.frame()

for( i in 1:nrow(class8_x)){
  for (j in 1:ncol(class8_z)){
    distances_class8[i,j] = sqrt(sum(class8_x[i,j]**2 + class8_y[i,j]**2 + class8_z[i,j]**2))
  }
}


ggplot()+geom_line(aes(x=1:ncol(distances_class8[8,]), y=as.numeric(distances_class8[8,])))+labs(title="Distances to Origin 8th Instance Class8", y="Distance", x="Point")


hist(as.numeric(distances_class8[8,]), main="Histogram of 8th Instance Class8", xlab="Distance to Origin")


#I used SAX representation as a piecewise model to represent distance information.

library(TSrepr)
library(data.table)

sax_segment_length=5
sax_alphabet_size=10

alphabet=list("a","b","c","d","e","f","g","h","i","j")


sax_rep_class1=repr_sax(distances_class1[1,], q = sax_segment_length, a = sax_alphabet_size)
sax_rep_class1
dummy_class1=c(1:(length(sax_rep_class1)-1))*sax_segment_length 
dummy_class1=c(dummy_class1,length(distances_class1[1,]))  
dt_sax_1=data.table(time=dummy_class1,sax_rep_char=sax_rep_class1)
dt_sax_1

alpha_1 = data.frame(alphabet)
k=1
for (i in alphabet){
  print(c(i,'count for class 1 instance 1', print(sum(dt_sax_1==i))))
  alpha_1[2,k] = sum(dt_sax_1==i)
  k=k+1
}
alpha_1
ggplot()+geom_col(aes(x=(as.character(alpha_1[1,])),y=as.numeric(alpha_1[2,])))+labs(title="SOX Representation Class 1 Instance 1", 
                                                                                     x="Letter", y="Frequency")

sax_rep_class2=repr_sax(distances_class2[2,], q = sax_segment_length, a = sax_alphabet_size)
sax_rep_class2
dummy_class2=c(1:(length(sax_rep_class2)-1))*sax_segment_length 
dummy_class2=c(dummy_class2,length(distances_class2[2,]))  
dt_sax_2=data.table(time=dummy_class2,sax_rep_char=sax_rep_class2)
dt_sax_2


sax_rep_class3=repr_sax(distances_class3[3,], q = sax_segment_length, a = sax_alphabet_size)
sax_rep_class3
dummy_class3=c(1:(length(sax_rep_class3)-1))*sax_segment_length 
dummy_class3=c(dummy_class3,length(distances_class3[3,]))  
dt_sax_3=data.table(time=dummy_class3,sax_rep_char=sax_rep_class3)
dt_sax_3


sax_rep_class4=repr_sax(distances_class4[3,], q = sax_segment_length, a = sax_alphabet_size)
sax_rep_class4
dummy_class4=c(1:(length(sax_rep_class4)-1))*sax_segment_length 
dummy_class4=c(dummy_class4,length(distances_class4[3,]))  
dt_sax_4=data.table(time=dummy_class4,sax_rep_char=sax_rep_class4)
dt_sax_4


sax_rep_class5=repr_sax(distances_class5[14,], q = sax_segment_length, a = sax_alphabet_size)
sax_rep_class5
dummy_class5=c(1:(length(sax_rep_class5)-1))*sax_segment_length 
dummy_class5=c(dummy_class5,length(distances_class5[14,]))  
dt_sax_5=data.table(time=dummy_class5,sax_rep_char=sax_rep_class5)
dt_sax_5

sax_rep_class6=repr_sax(distances_class6[6,], q = sax_segment_length, a = sax_alphabet_size)
sax_rep_class6
dummy_class6=c(1:(length(sax_rep_class6)-1))*sax_segment_length 
dummy_class6=c(dummy_class6,length(distances_class6[6,]))  
dt_sax_6=data.table(time=dummy_class6,sax_rep_char=sax_rep_class6)
dt_sax_6


sax_rep_class7=repr_sax(distances_class7[7,], q = sax_segment_length, a = sax_alphabet_size)
sax_rep_class7
dummy_class7=c(1:(length(sax_rep_class7)-1))*sax_segment_length 
dummy_class7=c(dummy_class7,length(distances_class7[7,]))  
dt_sax_7=data.table(time=dummy_class7,sax_rep_char=sax_rep_class7)
dt_sax_7



sax_rep_class8=repr_sax(distances_class8[8,], q = sax_segment_length, a = sax_alphabet_size)
sax_rep_class8
dummy_class8=c(1:(length(sax_rep_class8)-1))*sax_segment_length 
dummy_class8=c(dummy_class8,length(distances_class8[8,]))  
dt_sax_8=data.table(time=dummy_class8,sax_rep_char=sax_rep_class8)
dt_sax_8

                                                                                  
