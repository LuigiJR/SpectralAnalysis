###  Apply the Pegram scale-and-shift to preserve wet area
ScaleAndShift = function(R_orig,R_pert) {
   #  -  R_orig  : original analysis
   #  -  R_pert  : perturbed analysis 

	NR = dim(R_orig)[1]
	NC = NR

	small_c = min(R_orig)
	
	WAR0 = sum(R_orig > small_c)/(NR*NC)  # wet area ratio
        MM0  = mean(R_orig[R_orig> small_c]) # marginal mean and sd.
        SD0  = sd(R_orig[R_orig > small_c])
        TT0  = as.numeric(quantile(R_pert,probs=1 - WAR0))
    #  --
        MMT = mean(R_pert[R_pert > TT0])
        SDT = sd(R_pert[R_pert > TT0])
    #
        R_ens = (R_pert - MMT)/SDT * SD0 + MM0
        T_star = (TT0 - MMT)/SDT * SD0 + MM0
        R_ens[R_ens < T_star] = small_c

	
   return(R_ens)

}
