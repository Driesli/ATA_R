##----------------------------------------------------------------
## setConstraint.R
##----------------------------------------------------------------

## noItemOverlap.R is used as a source file in the simulation studies for the manuscript
## entitled: "Multidimensional Linear Test Assembly Using Mixed Integer Linear 
## Programming". 
## This file creates a funcion that writes a sparse matrix that can be used in the 
## writeMILP function (sourse-file writeMILP.R)
## The resulting sparse matrix is a combination of (1) the matrix A 
## (dim = c(nrSets*nrForms, nrItems * nrForms + 1)) with weights for all the MILP-
## variables (i.e, the left hand side of the constraints), (2) a vector (length nrSets*nrForms)
## with the sign of the constraints represented by a number (-1 = leq, 0 = eq, 1 = geq),
## and (3) a vector d (length nrSets*nrForms) with the right hand side of the constraints.

setConstraint <- function(sets, nForms, nItems, sign, nPerSet){
   if(length(sets) != length(nPerSet)){stop("Number of sets in the pool should be equal to the number of sets in each form.")
   } else {
      M <- nForms*nItems
      nSets <- length(sets)
      rowIndex <- as.vector(c(sapply(1:nForms, function(f){
         as.vector(sapply(1:nSets, function(set){
            rep(set + nSets * (f-1), length(sets[[set]]))
         }))
      }), 1:(nSets*nForms), 1:(nSets*nForms)))
      
      colIndex <- as.vector(c(sapply(1:nForms, function(f){
         as.vector(sapply(1:nSets, function(set){
            sets[[set]] + nItems * (f-1)
         }))
      }), rep(M + 2, nSets*nForms), rep(M + 3, nSets*nForms)))
      
      values <- as.vector(c(sapply(1:nForms, function(f){
         as.vector(sapply(1:nSets, function(set){
            rep(1, length(sets[[set]]))
         }))
      }), rep(sign, nSets*nForms), rep(nPerSet, times = nForms)))
      
      sparseMatrix(i = rowIndex, 
                   j = colIndex, 
                   x = values)
   }
}
