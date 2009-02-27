spreadout<-function(x,mindist) {
 if(sum(!is.na(x)) < 2) return(x)
 xorder<-order(x)
 # get the diffs between the ordered series of x
 diffo<-diff(x[xorder])
 diffo<-diffo[!is.na(diffo)]
 lendiffo<-length(diffo)
 middif<-lendiffo%/%2
 if(middif == 0) middif<-1
 # are there any diffs less than mindist
 if(any(diffo < mindist)) {
  # start from the middle, work out
  for(xoind in middif:1) {
   if(diffo[xoind] < mindist) x[xorder[xoind]]<-x[xorder[xoind+1]]-mindist
   # check that this hasn't jumped over the next point
   if(xoind > 1 && x[xorder[xoind-1]] > x[xorder[xoind]]) {
    x[xorder[xoind-1]]<-x[xorder[xoind]]-mindist/10
    # recalculate the diffs or it will miss this
    diffo<-diff(x[xorder])
   }
  }
  for(xoind in middif:lendiffo) {
   if(diffo[xoind] < mindist) x[xorder[xoind+1]]<-x[xorder[xoind]]+mindist
   if(xoind < lendiffo && x[xorder[xoind+2]] < x[xorder[xoind+1]]) {
    x[xorder[xoind+2]]<-x[xorder[xoind+1]]+mindist/10
    diffo<-diff(x[xorder])
   }
  }
 }
 return(x)
}
