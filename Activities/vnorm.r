vnorm = function(x,p=2) {
  if (p == 'I' )
    return(max(abs(x)))
  else
    return(sum(abs(x)^p)^(1/p))
}

vnorm(c(3,-2, 2, 3, 1, 4, 1, 2,3),p=2)

