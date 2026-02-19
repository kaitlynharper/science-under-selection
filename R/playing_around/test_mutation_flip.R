# Without selection and only mutation: How fast does it approach the equilibrium of 50%?

n <- 100000
mutation_rate = 0.1 # probability that a new agent's replication_probability flips (0->1 or 1->0)

agents <- rep(0, n)  # initialize all at state "0"
evol <- c()

for (i in 1:1000) {
  to_flip_index <- sample(1:n, size=floor(n*mutation_rate), replace=FALSE)
  agents[to_flip_index] <- 1-agents[to_flip_index]
  avg_repl_prob <- mean(agents)
  evol <- c(evol, avg_repl_prob)
}

300/35

evol[1:9]