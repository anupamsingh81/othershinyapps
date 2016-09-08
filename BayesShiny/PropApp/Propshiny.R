n1 = 52 # men
y1 = 9  # left-handed men
n2 = 480 # women
y2 = 400  # left-handed women
a= 10
b=10
c=1
d =1
compval = 0.3

# SIMULATION
I = 10000 # simulations
theta1 = rbeta(I, y1+a, (n1-y1)+b)  
theta2 = rbeta(I, y2+c, (n2-y2)+d)
diff = theta1-theta2-compval  # simulated diffs

# OUTPUT
quantiles = quantile(diff,c(0.005,0.025,0.5,0.975,0.995))
print(quantiles,digits=2)

print("Probability higher % men than women lefties:")
print(mean(theta1>theta2))


# VISUALIZATION
#png(file="bayesContingency.png")
plot(density(diff),
     xlab="theta1 - theta2",
     ylab="p(theta1 - theta2 | y, n)",
     main="Posterior Simulation of Male - Female Lefties",
     ylim=c(0,8),
     frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
abline(v=quantiles[2], col="blue")
abline(v=quantiles[4], col="blue")
#dev.off()