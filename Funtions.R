# Functions to convert from Celsius degree-days to Fahrenheit degree-days 
# and vice versa.

# From C to F
CDD_FDD <- function(x) {
  (9/5) * x
}

# From F to C
FDD_CDD <- function(x) {
  (5/9) * x
}