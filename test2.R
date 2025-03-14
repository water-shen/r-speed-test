f <- function(x) {
    return(((x + 1)^2 + 3)^4)
}

g1 <- function(x) {
    return(x + 1)
}

g2 <- function(x) {
    return(x^2)
}

g3 <- function(x) {
    return(x + 3)
}

g4 <- function(x) {
    return(x^4)
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
#    1.84    0.02    1.88
#    user  system elapsed 
#    5.00    0.02    5.08