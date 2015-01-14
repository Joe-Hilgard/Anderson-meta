require(plyr)

stringMerge = function(strings) {
  if (length(unique(strings)) == 1) return(strings[1]) 
  else return(paste(unique(strings), collapse="/"))
}

combine.rows=function(dat, filter, do) {
  temp = dat[filter,]
  if (is.null(temp)) break #return("Error")
  tempOut = temp[1,]
  # do the math stuff
  if (do=="sum") {
    tempOut$Sample.size = sum(temp$Sample.size)
    tempOut$Std.Err = sqrt(1/tempOut$Sample.size)
    tempOut$Fisher.s.Z = weighted.mean(temp$Fisher.s.Z, w=temp$Sample.size)
    tempOut$Correlation = atanh(tempOut$Fisher.s.Z)
  }
  if (do=="average") {
    tempOut$Fisher.s.Z = weighted.mean(temp$Fisher.s.Z, w=temp$Sample.size)
    tempOut$Correlation = atanh(tempOut$Fisher.s.Z)
  }
  # do the string stuff
  stringCols = names(dat)[!names(dat) %in% c("Sample.size", "Correlation", "Cor.Joe",
                                             "Fisher.s.Z", "Z.Joe", "Std.Err", "Std.Err.Joe",
                                             "t", "df", "p", "p.value")]
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
