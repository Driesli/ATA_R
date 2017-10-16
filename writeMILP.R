##----------------------------------------------------------------
## writeMILP.R
##----------------------------------------------------------------

## writeMILP.R is used as a source file in the simulation studies for the manuscript
## entitled: "Multidimensional Linear Test Assembly Using Mixed Integer Linear 
## Programming". 
## This file creates a funcion that writes the MILP model so that it can be used
## by gurobi. That is a list is created that can be used as argument in the gurobi()
## function call.

source("nrItInForm.R")
source("noItemOverlap.R")
source("setConstraint.R")
source("targetConstraint.R")

writeMILP <- function(pars,
                      settings,
                      thetaPoints, 
                      additionalConstraints, 
                      values.target,
                      values.TCC,
                      do.combined = TRUE){
   
   F <- settings$ATA$F
   I <- settings$pool$I 
   n.A <- F * I + 1           # number or MILP variables (binary + one real for objective)
   n.Ad <- n.A + 2            # number of columns in A.d matrix (number of MILP varibales + two)
   
   # create Ad matrix, a matrix that contains al the MILP model constraints
   # Ad is a combination of: 
   #  - A (the left hand side of the constraints)
   #  - the sign (of the constraints)
   #  - d (the right hand side of the constraints)
   
   # constrain number of items per form
   Ad.nrItInForm <- nrItInFormConstraint(nForms = F, nItems = I, sign = 0, nrItems = settings$ATA$n) 
   
   # no item overlap constraint
   Ad.noItemOverlap  <- noItemOverlapConstraint(nForms = F, nItems = I, sign = -1)
   
   # function to creat indexes for item type and item content constraints
   
      
   
   # item content constraints
   if(is.null(settings$ATA$Nc)){Ad.c <- NULL} else {
      Ad.c <- setConstraint(sets = settings$pool$Vc, nForms = F, nItems = I, sign = 0, nPerSet = settings$ATA$Nc)
   }
   
   # item format constraints
   if(is.null(settings$ATA$Nf)){Ad.f <- NULL} else {
      Ad.f <- setConstraint(sets = settings$pool$Vf, nForms = F, nItems = I, sign = 0, nPerSet = settings$ATA$Nf)
   }
   
   # constraints related to target
   Ad.target <- targetConstraint(nForms = F, nItems = I, itemValues = values.target$c.A, 
                                 targetValues = values.target$c.d, thetaPoints = thetaPoints)
   
   # constraints related to TCC
   if(do.combined){
      b_width = if(pars$dimensions == 3){0.5} else {0.25} # set bandwidth TCC for combined target
      
      Ad.TCC <- targetConstraint(nForms = F, nItems = I, itemValues = values.TCC$c.A, 
                                 targetValues = values.TCC$c.d, thetaPoints = thetaPoints,
                                 relative = FALSE, bWidth = b_width)
   } else {Ad.TCC <- NULL}
   
   
   # combine all constraints
   allConstraints <- c(Ad.nrItInForm, Ad.noItemOverlap, Ad.f, Ad.c, Ad.target, Ad.TCC)
   Ad <- do.call(rbind, allConstraints)
   
   # sense of the constraints 
   sense <- as.vector(Ad[,(n.A+1)])
   sense[which(sense==-1)] <- "<="
   sense[which(sense==0)] <- "="
   sense[which(sense==1)] <- ">="
   
   MILP <- list(A = Ad[,1:n.A],
                rhs = as.vector(Ad[,(n.Ad)]),
                sense = sense,
                obj = c(rep(0, I*F), 1),
                modelsense = "min",
                vtype = c(rep("B", I*F), "C"))
   
}

