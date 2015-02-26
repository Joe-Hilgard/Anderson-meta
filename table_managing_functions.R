# NOTE: using atanh to get back to Correlation will introduce
  # some bias. I can't recall inverse of z-transform right now.
  # And we're working with the z-scores anyway.

require(plyr) #should I move to dplyr? need to learn it first

stringMerge = function(strings) {
  if (length(unique(strings)) == 1) return(strings[1]) 
  else return(paste(unique(strings), collapse="/"))
}

r2ztrans = function(r) .5*log((1+r)/(1-r))
#z2rtrans = function(z) 2*exp(1+z) / exp(1-z)

combine.rows=function(dat, filter, do) {
  temp = dat[filter,]
  if (is.null(temp)) break #return("Error")
  tempOut = temp[1,]
  # do the math stuff
  if (do=="sum") {
    tempOut$Sample.size = sum(temp$Sample.size)
    # Set std.err to 1/sqrt(N-3).
    # I realize the se.r / (1-r^2) equation may be more appropriate but idk how to
    tempOut$Std.Err = sqrt(1/(tempOut$Sample.size-3)) 
    tempOut$Fisher.s.Z = weighted.mean(temp$Fisher.s.Z, w=temp$Sample.size)
    tempOut$Correlation = atanh(tempOut$Fisher.s.Z)
  }
  if (do=="average") {
    tempOut$Fisher.s.Z = weighted.mean(temp$Fisher.s.Z, w=temp$Sample.size)
    tempOut$Correlation = atanh(tempOut$Fisher.s.Z)
    # Std.Err should not change if Ns are equal and se=1/sqrt(N-3)
    # Still, let's average SEs together, for lack of a better aggregation, & just in case.
    # This may bias the SE a little high? 
    tempOut$Std.Err = mean(temp$Std.Err)
  }
  # do the string stuff
  stringCols = names(dat)[!names(dat) %in% c("Sample.size", "Correlation", "Cor.Joe",
                                             "Fisher.s.Z", "Z.Joe", "Std.Err", "Std.Err.Joe",
                                             "t", "df", "p", "p.value", "Std.Err.r", "Study", 
                                             "Full.Reference", "Citation")]
  for (i in stringCols) tempOut[,i] = stringMerge(temp[,i])
  # Export all the rows, minus the ones that were aggregated, plus their aggregation.
  fullOut = rbind(dat[!(filter),], tempOut)
  return(fullOut)
}

getTooMany = function(dat, filter) {
  frame = dat[filter,]
  temp = table(frame$Full.Reference, frame$Outcome, frame$Study)
  count = adply(temp, c(1,2))
  if (dim(count)[2] > 3) rowsums = apply(count[,3:dim(count)[2]], 1, sum) else rowsums = count[,3]
  count = count[rowsums>1,]
  testfunc = function(x) if(sum(x>1)>0) return(TRUE) else return(FALSE)
  toomany = apply(count[,3:dim(count)[2], drop=F], 1, testfunc)
  sum(toomany)
  #View(count[toomany,])
  #View(dat[dat$Full.Reference %in% count[toomany, "X1"],])
  return(frame[frame$Full.Reference %in% count[toomany, "X1"],])
}

# Apply filter before calling getTooMany2,
# Supply list for index e.g. list(dat$Outcome, dat$Study)
# Now just returns a vector to be used as you will
getTooMany2 = function(dataset, INDEX) {
  temp = table(INDEX)
  count = adply(temp, c(1,2))
  if (dim(count)[2] > 3) rowsums = apply(count[,3:dim(count)[2]], 1, sum) else rowsums = count[,3]
  #count = count[rowsums>1,]
  testfunc = function(x) if(sum(x>1)>0) return(TRUE) else return(FALSE)
  toomany = apply(count[,3:dim(count)[2], drop=F], 1, testfunc)
  sum(toomany)
  # oh my god this is ugly
  return(INDEX[[1]][INDEX[[1]] %in% count[toomany, "INDEX.1"]])
}
# # take it for a spin
# brady = dat[grep("Mathews", dat$Full.Reference),]
# brady = brady[brady$Outcome=="AggBeh",]
# brady
# brady = combine.rows(brady, brady$X %in% c(98:99), "sum")
# brady
# 
# # and averaging?
