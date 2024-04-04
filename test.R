set.seed(12345)
pop <- rnorm(1000)
(mp <- mean(pop))

ll <- 200
allmeanconf <- function(pop, sz, l, m, level = 0.95) {
  p <- NULL
  low <- NULL
  high <- NULL
  for (i in 1:l) {
    # Lấy mẫu l lần
    tst <- t.test(sample(pop, size = sz), conf.level = level)
    t1 <- tst$conf.int[1]
    t2 <- tst$conf.int[2]
    low <- c(low, t1)
    high <- c(high, t2)
    if (t1 <= m & m <= t2)
      p <- c(p, 'Yes')
    else
      p <- c(p, 'No')
  }
  list(p = p, low = low, high = high)
}

r11 <-
  allmeanconf(
    pop,
    sz = 10,
    l = 11,
    m = mp,
    level = 0.75
  )
table(r11$p)

r12 <- allmeanconf(
  pop,
  sz = 60,
  l = ll,
  m = mp,
  level = 0.75
)
table(r12$p)

r21 <- allmeanconf(
  pop,
  sz = 10,
  l = ll,
  m = mp,
  level = 0.95
)
table(r21$p)

r22 <- allmeanconf(
  pop,
  sz = 60,
  l = ll,
  m = mp,
  level = 0.95
)
table(r22$p)

plotmeanconf <- function(pop, rr, l, m) {
  low <- rr$low
  high <- rr$high
  tt <- table(rr$p)
  pb <- tt['Yes'] / sum(tt)
  plot(
    y = pop,
    x = rep(-0.5, length(pop)),
    pch = 20,
    cex = 0.5,
    xlab = 'test',
    xlim = c(-0.5, l),
    ylab = "confidence interval",
    col = 'blue',
    main = paste0('prob=', pb)
  )
  for (i in 1:l)
    if (low[i] <= m & m <= high[i])
      lines(y = c(low[i], high[i]), x = c(i, i))
  else
    lines(y = c(low[i], high[i]),
          x = c(i, i),
          col = 'red')
  abline(h = m, col = 'blue')
}

par(mar = c(4, 4, 1, 1))
plotmeanconf(pop, r11, ll, mp)
plotmeanconf(pop, r12, ll, mp)

par(mar = c(4, 4, 1, 1))
plotmeanconf(pop, r21, ll, mp)
plotmeanconf(pop, r22, ll, mp)

