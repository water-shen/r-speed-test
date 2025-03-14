f <- function(x) {
    return(tan(exp(sin(127 * x - 42 * pi))))
}

g1 <- function(x) {
    return(127 * x - 42 * pi)
}

g2 <- function(x) {
    return(sin(x))
}

g3 <- function(x) {
    return(exp(x))
}

g4 <- function(x) {
    return(tan(x))
}

N <- 1e7

result1 <- rep(0, N)
print(system.time({
    for (i in 1:N) {
        result1[i] <- f(i)
    }
}))

result2 <- rep(0, N)
print(system.time({
    for (i in 1:N) {
        result2[i] <- g4(g3(g2(g1(i))))
    }
}))

print(all(result1 == result2))


# Output:
#    user  system elapsed 
#    2.83    0.02    2.87
#    user  system elapsed 
#    6.00    0.11    6.14