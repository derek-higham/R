# Factor Analysis

# Load the correlation matrix into R
cor.values <- c(1.000,0.210,0.370,-0.32,0.000,-0.31,-0.26,0.090,-0.38,
                0.210,1.000,0.090,-0.29,0.120,-0.30,-0.14,0.010,-0.39,
                0.370,0.090,1.000,-0.31,-0.04,-0.30,-0.11,0.120,-0.39,
                -0.32,-0.29,-0.31,1.00,-0.16,0.25,-0.13,-0.14,0.900,
                0.00,0.120,-0.04,-0.16,1.000,-0.20,-0.03,-0.08,-0.38,
                -0.31,-0.30,-0.30,0.25,-0.20,1.000,-0.24,-0.16,0.180,
                -0.26,-0.14,-0.11,-0.13,-0.03,-0.24,1.000,-0.20,0.040,
                0.090,0.010,0.120,-0.14,-0.08,-0.16,-0.20,1.000,-0.24,
                -0.38,-0.39,-0.39,0.900,-0.38,0.180,0.040,-0.24,1.000
)
cor.values

# How do we put these correlation values into a correlation matrix?
help(matrix)
cor.matrix <- matrix(cor.values,nrow=9,ncol=9,byrow=TRUE)

# Check that object is a matrix object
is.matrix(cor.matrix)

# Check that matrix is symmetric
# This check helps check for typos
isSymmetric(cor.matrix)

# Stoetzel estimated a 3 factor model in his paper
# We will now estimate a 3 factor model with a VARIMAX roatation using 
# maximum likelihood factor analysis
f.1 <- factanal(covmat=cor.matrix, n.obs=1442, factors=3, rotation='varimax')
names(f.1)
f.1

# Fit factor models for k=1 through 6
# Do any of these models represent the correct number of factors 
# based on the inference results?
fact.1 <- factanal(covmat=cor.matrix, n.obs=1442, factors=1, rotation='varimax')
names(fact.1)
fact.1

fact.2 <- factanal(covmat=cor.matrix, n.obs=1442, factors=2, rotation='varimax')
names(fact.2)
fact.2

fact.3 <- factanal(covmat=cor.matrix, n.obs=1442, factors=3, rotation='varimax')
names(fact.3)
fact.3

fact.4 <- factanal(covmat=cor.matrix, n.obs=1442, factors=4, rotation='varimax')
names(fact.4)
fact.4

fact.5 <- factanal(covmat=cor.matrix, n.obs=1442, factors=5, rotation='varimax')
names(fact.5)
fact.5

fact.6 <- factanal(covmat=cor.matrix, n.obs=1442, factors=6, rotation='varimax')
names(fact.6)
fact.6

# VARIMAX factor rotation is an example of an ornthogonal factor rotation.
# There is oblique factor rotations
# Example of oblique factor rotation is PROMAX rotation
# Fit a 3 factor model with a PROMAX rotation using max likelihood factor analysis
g.1 <- factanal(covmat=cor.matrix, nobs=1442, factors=3, rotation='promax')
g.1

# Use the factor loadings and the specific variances to approximate the correlation matrix.
# measure the fit of these approaximations using the MAE of the residual matrix
# which factor model better approximates the correlation matrix?
gamma.f1 <- f.1$loadings;
approx.f1 <- gamma.f1%*%t(gamma.f1) + diag(f.1$uniqueness);
mae.f1 <- mean(abs(approx.f1-cor.matrix))

gamma.g1 <- g.1$loadings;
approx.g1 <- gamma.g1%*%t(gamma.g1) + diag(g.1$uniqueness);
mae.g1 <- mean(abs(approx.g1-cor.matrix))

mae.f1
mae.g1