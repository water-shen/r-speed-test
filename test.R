f <- function(x) {
    return(x)
}

g1 <- function(x) {
    return(x)
}

g2 <- function(x) {
    return(x)
}

g3 <- function(x) {
    return(x)
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
        result2[i] <- g3(g2(g1(i)))
    }
}))

print(all(result1 == result2))


# Output:
#    user  system elapsed 
#    1.31    0.00    1.34
#    user  system elapsed 
#    3.42    0.05    3.53