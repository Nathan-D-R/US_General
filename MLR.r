# R Script File
# We are interested in the relationship between the following variables:
# Y = X.Donated.From.Big.Oil
# X = NUCLEAR, COAL, NATURAL.GAS, PETROLEUM, HYDRO, GEOTHERMAL, SOLAR, WIND, and BIOMASS

# Particularly with Coal, because that is predominantly what is being lobbied for by the "Big Oil" companies in this data.

# Hypothesis:
# H0: There is a negative relationship between X.Donated.From.Big.Oil and Coal.
# H1: There is a positive relationship between X.Donated.From.Big.Oil and Coal.

# PDF Config
pdf("MLR.pdf", width = 8.5, height = 9)

# Libraries
library(corrplot) # Correlation Matrix

# Import Data
data <- read.csv("US_General.csv", header = TRUE, sep = ",")

# Rename Y to be less politically charged
Oil.Lobbying <- data$X.Donated.From.Big.Oil

# Adjust Y for Population and GDP
Y <- cbind(Oil.Lobbying,
           Oil.Lobbying / data$Population,
           Oil.Lobbying / data$State.GDP.Q4.2021,
           Oil.Lobbying / (data$State.GDP.Q4.2021 / data$Population))

# Add names to Y for readability
ynames <- cbind("Oil Lobbying", "Oil Lobbying / Population", "Oil Lobbying / State GDP", "Oil Lobbying / State.GDP.Q4.2021 / Population")

# For loop to iterate through each Y for Multiple Linear Regression, Correlation Matrix, and Scatterplots
for (i in 1:ncol(Y)) {

# Create a data frame with Y and X
        data <- data.frame(Y[,i],
                data$NUCLEAR,
                data$COAL,
                data$NATURAL.GAS,
                data$PETROLEUM,
                data$HYDRO,
                data$GEOTHERMAL,
                data$SOLAR,
                data$WIND,
                data$BIOMASS)

# Rename the Columns to be more readable
names(data) <- c("Y",
                 "NUCLEAR",
                 "COAL",
                 "NATURAL.GAS",
                 "PETROLEUM",
                 "HYDRO",
                 "GEOTHERMAL",
                 "SOLAR",
                 "WIND",
                 "BIOMASS")

# Perform Multiple Linear Regression
model <- lm(Y ~ ., data = data)

# Print the Model Summary and name of Y
print(paste("Y=", ynames[,i]))
print(summary(model))

# Correlation Matrix
cor <- cor(data)

# Output the Correlation Matrix with red and green colors

# Method = Circle
corrplot(cor,
         method = "circle",
         col = c("red", "green"),
         mar = c(0, 0, 2, 0),
         title = paste("Y=", ynames[,i]))

# Method = Number
corrplot(cor,
         method = "number",
         col = c("red", "green"),
         mar = c(0, 0, 2, 0),
         title = paste("Y=", ynames[,i]))

# For loop to make a scatterplot for each X variable
for (j in 2:ncol(data)) {

# Output Scatterplot
plot(data[, j],
     data[, 1],
     xlab = names(data)[j],
     ylab = names(data)[1],
     main = paste(names(data)[1], "vs", names(data)[j]))

# Output Regression Line with Scatterplots
abline(lm(data[, 1] ~ data[, j]), col = "red")
}
}

# Print warnings, if any
warnings()
