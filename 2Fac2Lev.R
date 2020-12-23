\begin{lstlisting}
# 2^2 Factorial Design
# Filename 2Fac2Lev.R

df2 = read.csv("Router-2x2.csv", 
header = T)    # Read the data into a data frame  
df2[1] <- NULL # and ignore the header (first row)

# Concatenate the data rows in df2 into a single vector r
r = c(t(as.matrix(df2))) # response data 


# Assign new variables for the treatment levels and number of observations
f1 = c("40 rpm", 
"90 rpm")         # 1st factor levels 

f2 = c("1/16 in", 
"1/8 in")         # 2nd factor levels

k1 = length(f1)          # number of 1st factors 
k2 = length(f2)          # number of 2nd factors

n = 4                    # observations per treatment


# Create vectors that correspond to the ith treatment level of the response data r 
# element-by-element with the gl function (generate factor levels). 

A = gl(k1,    # number of reps
1, 
n*k1*k2,        # integer length of result
factor(f1))     # factor labels

B = gl(k2, 
n*k1, 
n*k1*k2, 
factor(f2)) 

av = aov(r ~ A * B)  # Execute the ANOVA and include interaction 

cat("\014") # Clear the console

# Print output to console
out <- summary(av)
cat("ANOVA results:\n\n")
print(out)

# Diagnostic plots
par(las=1)              #horizontal y-axis values
interaction.plot(A,
B,
r,
main="Interaction Plot",
sub="Cutting Speed vs. Bit Size")

results<-lm(r~A*B)   # Store linear model in results

# Plot residuals from linear model
par(mfrow=c(1,2),        # side-by-side, horizontal y-axis values
las=1)        
plot(results,            # residuals plot
which=1)
plot(results,            # normal Q-Q plot of residuals
which=2)

\end{lstlisting}