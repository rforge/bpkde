library(bpkde)

datasets <- data(package = "bpkde")$results[, 3]
referencePackages <- c("KernSmooth")
packages <- c("bpkde", referencePackages)

if(require(referencePackages, character.only = TRUE)) {
  linbin2Ds <- list()
  bin.est <- list()
  for(p in packages)
    linbin2Ds[[p]] <- eval(parse(text = paste(p, "linbin2D", sep = ":::")))

  for(d in datasets) {
    data(list = d)
    mat <- get(d)
    x <- seq(min(mat[, 1]) - 1, max(mat[, 1]) + 1, length = 100)
    y <- seq(min(mat[, 2]) - 1, max(mat[, 2]) + 1, length = 100)
    bin.est[[1]] <- linbin2Ds[[1]](mat, x, y)
    bin.est[[2]] <- linbin2Ds[[2]](mat, x, y)
    print(d)
    stopifnot(all.equal(bin.est[[1]], bin.est[[2]]))
  }
}


