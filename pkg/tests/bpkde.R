library(bpkde)

datasets <- data(package = "bpkde")$results[, 3]

set.seed(0)
for(d in datasets) {
  data(list = d)
  mat <- get(d)
  print(d)
  print(bpkde(mat))
}


