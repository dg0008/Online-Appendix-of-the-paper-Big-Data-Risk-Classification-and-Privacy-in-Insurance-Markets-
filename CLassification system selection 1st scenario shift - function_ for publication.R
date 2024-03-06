#OPTIMAL RISK CLASSIFICATION SYSTEM

#install.packages("xlsx")
#library("xlsx")
####################################################################################
#Input characteristics of the underlying population
#number of subpopulations:
S=70

##########################################
#create empty dataframe for storing results
results_df <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(results_df) <- c('alpha','prob_misclass','opt_nr_of_classes_w_misclass','opt_nr_of_classes_wo_misclass','max_net_profit_w_missclass','max_net_profit_wo_misclass')


#For subpopulation 1 to S, a vector giving the actuarial cost in each subpopulation 
#from the highest risk to the lower risk subpopulation
Pa_s=matrix(seq(2800, 40, length=S), S,1)

#Classification costs k(I_m-1)=100(I_m-1)
cost=matrix(seq(from=0,by=1000,length=S), S,1)

#for(misclass_rate in seq(0,0.4,by=0.05)){                 
##########################################
  # for(alpha in seq(0.95,1,by=0.005)) 
  profit_function <- function(misclass_rate, alpha) {
   
  
  #For subpopulation 1 to S, a vector giving the number of agents in that population (column vector)
  N_s=1*matrix(rep(100, S), S,1)
  ;
  #For subpopulation 1 to S, a vector giving the highest reservation price (ph perspective) in each subpopulation 
  #from the highest risk to the lower risk subpopulation.
  Pr_s=alpha*matrix(seq(3500, 50, length=S), S,1)
  ;
    ####################################################################################
    # Characteristics of the classification system
    
    #for a classification system with l classes, in 1 to S, ranked from 1 to k 
    #(where k takes values 1 to l) from high to low risk.
    #Number of subpopulations in each risk class, 
    sub_per_class=matrix(0, S, S)
    ;
    #starting from high risk classes (first column) to low risk column l, 
    #1st line classification with 1 class, last with S classes
    for(l in 1:S) {if(S/l==floor(S/l)) 
    {sub_per_class[l,1:l]=c(rep(S/l,l))} 
      else {sub_per_class[l,1:(S-l*floor(S/l))]<-c(rep(floor(S/l)+1,S-l*floor(S/l)));
      sub_per_class[l,(S-l*floor(S/l)+1):l]<-c(rep(floor(S/l),l-S+l*floor(S/l)))} };
    
    
   
    ####################################################################################
    # Calculating optimal profit per class and profit including misclassifications
    
    
    #create data frame with 0 rows and 19 columns to store intermediate calculations
    df <- data.frame(matrix(ncol = 19, nrow = 0))
    ;
    #provide column names. col 1-7 frozen, no change in number
    #l-class system, k- the class within the system, o subsegment / populatio within class sub_per_class[l,1:k] subsets
    colnames(df) <- c('l', 'k', 'o',"Int_low_bound","Int_upp_bound","Upper_bound_price","Low_bound_price","Opt_price_in_section","Opt_quantity_in_section"
                      , "p_q_segment","tc_seg","Opt_pofit_1_in_seg","Opt_pofit_1_in_class","Opt_price_in_class","Q_missc_to_higher","Q_missc_to_low","Extra_profit_Qh","Extra_profit_Ql","Profit_with_misclass")
    
    ;
    #filling the dataframe
    
    for(l in 1:S) {
      pij=matrix(rep(misclass_rate,l), l,1) # Misclassification vector: fixed
      #pij=matrix(rep(0,S),1,S)
      ;
      for(k in 1:l) { 
        #Misclassification ratios
        #within a classification system is the same between adjecant classes. 
        
        for(o in (sum(sub_per_class[l,1:k-1])+1):(sum(sub_per_class[l,1:k]))) {
          df[nrow(df) + 1,1:7] = c(l, k,o,sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o])-
                                     sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o]/Pr_s[(sum(sub_per_class[l,1:k-1])+1):o])*Pr_s[o],if(o==sum(sub_per_class[l,1:k])) {
                                       sum(N_s[(sum(sub_per_class[l,1:k-1])+1):(sum(sub_per_class[l,1:k]))])} else {sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o])-
                                           sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o]/Pr_s[(sum(sub_per_class[l,1:k-1])+1):o])*Pr_s[o+1]},Pr_s[o],if(o<sum(sub_per_class[l,1:k])) {Pr_s[o+1]} else {0})}
        ; 
        #"Opt_quantity_in_section"
        for(o in (sum(sub_per_class[l,1:k-1])+1):(sum(sub_per_class[l,1:k]))) {
          df$Opt_quantity_in_section[df$l==l & df$k==k & df$o==o] = as.numeric(try(uniroot(function(x) sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o])/sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o]/Pr_s[(sum(sub_per_class[l,1:k-1])+1):o])
                                                                                           -2*x/sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o]/Pr_s[(sum(sub_per_class[l,1:k-1])+1):o])-
                                                                                             ((sum(Pa_s[(sum(sub_per_class[l,1:k-1])+1):o]*(1/sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o]/Pr_s[(sum(sub_per_class[l,1:k-1])+1):o]))/(Pr_s[(sum(sub_per_class[l,1:k-1])+1):o]/N_s[(sum(sub_per_class[l,1:k-1])+1):o])))+
                                                                                                (sum(Pa_s[(sum(sub_per_class[l,1:k-1])+1):o]*(sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o])/sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o]/Pr_s[(sum(sub_per_class[l,1:k-1])+1):o])-Pr_s[(sum(sub_per_class[l,1:k-1])+1):o])/(Pr_s[(sum(sub_per_class[l,1:k-1])+1):o]/N_s[(sum(sub_per_class[l,1:k-1])+1):o])))/x),
                                                                                           lower=max(df$Int_low_bound[df$l==l & df$k==k & df$o==o],0.0001),upper=df$Int_upp_bound[df$l==l & df$k==k & df$o==o])$root, silent=T))}
        ;
        #;"Opt_price_in_section" 
        for(o in (sum(sub_per_class[l,1:k-1])+1):(sum(sub_per_class[l,1:k]))) {
          if(is.na(df$Opt_quantity_in_section[df$l==l & df$k==k & df$o==o])) {
            df$Opt_price_in_section[df$l==l & df$k==k & df$o==o]=0} else {
              df$Opt_price_in_section[df$l==l & df$k==k & df$o==o]=(
                sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o])/
                  (sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o]/Pr_s[(sum(sub_per_class[l,1:k-1])+1):o])))-
                df$Opt_quantity_in_section[df$l==l & df$k==k & df$o==o]/sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o]/Pr_s[(sum(sub_per_class[l,1:k-1])+1):o])}}
        ;
        # "p_q_segment"- pxq in a segment
        for(o in (sum(sub_per_class[l,1:k-1])+1):(sum(sub_per_class[l,1:k]))) {
          if(is.na(df$Opt_quantity_in_section[df$l==l & df$k==k & df$o==o])) 
          {df$p_q_segment[df$l==l & df$k==k & df$o==o]=0} else {
            df$p_q_segment[df$l==l & df$k==k & df$o==o]=df$Opt_quantity_in_section[df$l==l & df$k==k & df$o==o]*df$Opt_price_in_section[df$l==l & df$k==k & df$o==o]}}
        ;
        # tc_seg - total cost in a segment
        for(o in (sum(sub_per_class[l,1:k-1])+1):(sum(sub_per_class[l,1:k]))){
          if(is.na(df$Opt_quantity_in_section[df$l==l & df$k==k & df$o==o])) 
          {df$tc_seg[df$l==l & df$k==k & df$o==o]=0} else {
            df$tc_seg[df$l==l & df$k==k & df$o==o]=sum((N_s[(sum(sub_per_class[l,1:k-1])+1):o]-N_s[(sum(sub_per_class[l,1:k-1])+1):o]*df$Opt_price_in_section[df$l==l & df$k==k & df$o==o]/Pr_s[(sum(sub_per_class[l,1:k-1])+1):o])*Pa_s[(sum(sub_per_class[l,1:k-1])+1):o])}}
      }
      ;
      for(k in 1:l) {
        # "Opt_pofit_1_in_segment"  - optimal profit in a segment, within a class
        for(o in (sum(sub_per_class[l,1:k-1])+1):(sum(sub_per_class[l,1:k]))) {if(is.na(df$Opt_quantity_in_section[df$l==l & df$k==k & df$o==o])) {
          df$Opt_pofit_1_in_seg[df$l==l & df$k==k & df$o==o]= 0} else {
            df$Opt_pofit_1_in_seg[df$l==l & df$k==k & df$o==o]=df$p_q_segment[df$l==l & df$k==k & df$o==o]-
              df$tc_seg[df$l==l & df$k==k & df$o==o]}} 
        ;
        # "Opt_pofit_1_in_class" - optimal profit in a class, highest among segments, disregarding misclassification
        for(o in (sum(sub_per_class[l,1:k-1])+1):(sum(sub_per_class[l,1:k]))) {
          if(df$Opt_pofit_1_in_seg[df$l==l & df$k==k & df$o==o]<max(df$Opt_pofit_1_in_seg[df$l==l & df$k==k])) {
            df$Opt_pofit_1_in_class[df$l==l & df$k==k & df$o==o]=0} else {df$Opt_pofit_1_in_class[df$l==l & df$k==k & df$o==o]=df$Opt_pofit_1_in_seg[df$l==l & df$k==k & df$o==o]}}
        ;
        # "Opt_price_in_class" - offered price that generates the highest profit in class, disregarding misclassification
        for(o in (sum(sub_per_class[l,1:k-1])+1):(sum(sub_per_class[l,1:k]))) {
          if(df$Opt_pofit_1_in_class[df$l==l & df$k==k & df$o==o]==0) {df$Opt_price_in_class[df$l==l & 
                                                                                               df$k==k & df$o==o]=0} else {df$Opt_price_in_class[df$l==l & df$k==k & df$o==o]=df$Opt_price_in_section[df$l==l & df$k==k & df$o==o]}}
      }
      ; 
      #accounting for misclassification
      #beak down by k to new loop, to ensure that k is populated when using price of preceding class/ next class
      for(k in 1:l) {
        # "Q_missc_to_higher", number of agents in a class that are offered the optimal price of the preceding class, if positive (classes are ranked from higher risk)
        # in preceding class 
        for(o in (sum(sub_per_class[l,1:k-1])+1):(sum(sub_per_class[l,1:k]))) {if(k==1) {df$Q_missc_to_higher[df$l==l & df$k==k & df$o==o]=0} else
          if(df$Upper_bound_price[df$l==l & df$k==k & df$o==o]>=max(df$Opt_price_in_class[df$l==l & df$k==k-1]) &
             df$Low_bound_price[df$l==l & df$k==k & df$o==o]<max(df$Opt_price_in_class[df$l==l & df$k==k-1])) {
            df$Q_missc_to_higher[df$l==l & df$k==k & df$o==o]=max(0,sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o])-
                                                                    max(df$Opt_price_in_class[df$l==l & df$k==k-1])*sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o]/Pr_s[(sum(sub_per_class[l,1:k-1])+1):o]))
          } else {df$Q_missc_to_higher[df$l==l & df$k==k & df$o==o]=0}}
        ;
        #"Q_missc_to_low" number of agents in a class that are offered the optimal price of the adjacent lower risk class (cant be negative)
        for(o in (sum(sub_per_class[l,1:k-1])+1):(sum(sub_per_class[l,1:k]))) {if(k==l) {df$Q_missc_to_low[df$l==l & df$k==k & df$o==o]=0} else
          if(df$Upper_bound_price[df$l==l & df$k==k & df$o==o]>=max(df$Opt_price_in_class[df$l==l & df$k==k+1]) &
             df$Low_bound_price[df$l==l & df$k==k & df$o==o]<max(df$Opt_price_in_class[df$l==l & df$k==k+1])) {
            df$Q_missc_to_low[df$l==l & df$k==k & df$o==o]=max(0,sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o])-
                                                                 max(df$Opt_price_in_class[df$l==l & df$k==k+1])*sum(N_s[(sum(sub_per_class[l,1:k-1])+1):o]/Pr_s[(sum(sub_per_class[l,1:k-1])+1):o]))
          } else {df$Q_missc_to_low[df$l==l & df$k==k & df$o==o]=0}}
        ;
        #"Extra_profit_Qh" - extra profit created by misclassifying agents to a higher risk class
        for(o in (sum(sub_per_class[l,1:k-1])+1):(sum(sub_per_class[l,1:k]))) {
          if(df$Q_missc_to_higher[df$l==l & df$k==k & df$o==o]==0) {
            df$Extra_profit_Qh[df$l==l & df$k==k & df$o==o]=0} else {
              df$Extra_profit_Qh[df$l==l & df$k==k & df$o==o]=df$Q_missc_to_higher[df$l==l & df$k==k & df$o==o]*max(df$Opt_price_in_class[df$l==l & df$k==k-1])-
                sum((N_s[(sum(sub_per_class[l,1:k-1])+1):o]-N_s[(sum(sub_per_class[l,1:k-1])+1):o]*max(df$Opt_price_in_class[df$l==l & df$k==k-1])/Pr_s[(sum(sub_per_class[l,1:k-1])+1):o])*Pa_s[(sum(sub_per_class[l,1:k-1])+1):o])}}
        ;
        #"Extra_profit_Ql" extra profit created by misclassifying agents to a lower risk class (can be negative)
        for(o in (sum(sub_per_class[l,1:k-1])+1):(sum(sub_per_class[l,1:k]))) {
          if(df$Q_missc_to_low[df$l==l & df$k==k & df$o==o]==0) {
            df$Extra_profit_Ql[df$l==l & df$k==k & df$o==o]=0} else {
              df$Extra_profit_Ql[df$l==l & df$k==k & df$o==o]=df$Q_missc_to_low[df$l==l & df$k==k & df$o==o]*max(df$Opt_price_in_class[df$l==l & df$k==k+1])-
                sum((N_s[(sum(sub_per_class[l,1:k-1])+1):o]-N_s[(sum(sub_per_class[l,1:k-1])+1):o]*max(df$Opt_price_in_class[df$l==l & df$k==k+1])/Pr_s[(sum(sub_per_class[l,1:k-1])+1):o])*Pa_s[(sum(sub_per_class[l,1:k-1])+1):o])}}
        ;
        #"Profit_with_misclass" - total profit accounting for misclassification. Summing up profit generated by the percentage that is correctly classified and the share of those classified to lower and higher risk classes
        #here we will write it in the same segment as the optimal profit without misclassification and leave rows of other segments empty
        for(o in (sum(sub_per_class[l,1:k-1])+1):(sum(sub_per_class[l,1:k]))) {if(df$Opt_pofit_1_in_class[df$l==l & df$k==k & df$o==o]==0) {
          df$Profit_with_misclass[df$l==l & df$k==k & df$o==o]=0} else if(l==1) {df$Profit_with_misclass[df$l==l & df$k==k & df$o==o]=df$Opt_pofit_1_in_class[df$l==l & df$k==k & df$o==o]
          } else if(k==1 | k==l) {
            df$Profit_with_misclass[df$l==l & df$k==k & df$o==o]=(1-pij[l])*df$Opt_pofit_1_in_class[df$l==l & df$k==k & df$o==o]+
              pij[l]*max(df$Extra_profit_Qh[df$l==l & df$k==k])+pij[l]*max(df$Extra_profit_Ql[df$l==l & df$k==k])} else {
                df$Profit_with_misclass[df$l==l & df$k==k & df$o==o]=(1-2*pij[l])*df$Opt_pofit_1_in_class[df$l==l & df$k==k & df$o==o]+
                  pij[l]*max(df$Extra_profit_Qh[df$l==l & df$k==k]) +pij[l]*max(df$Extra_profit_Ql[df$l==l & df$k==k])}
        }
      }
      
    }
    ;
    
    
    #export dataframe to excel for debug
    
    #write.xlsx(df, file = paste0("C:..."1st_Scenario_detailed_for_alpha",alpha,"_",S,"_pij_",pij[1],".xlsx"));
    
    ####################################################################################
    #"Profit_per_classification_system"
    #write this in separate matrix
    #"Net_Profit_per_classification_system & write.xlsx
    profit<-as.matrix(rep(0,2*S),2,S);
    dim(profit)<-c(S,2);
    colnames(profit) <- c("Profit_wo_misclass", "Profit_with_misclass");
    for(l in 1:S) {profit[l,1]<-sum(df$Opt_pofit_1_in_class[df$l==l])};
    for(l in 1:S) {profit[l,2]<-sum(df$Profit_with_misclass[df$l==l])};
    
    #as above
    #Classification costs k(I_m-1)=3000(I_m-1)
    #cost=matrix(seq(from=0,by=100,length=S), S,1)
    
    Results<-data.frame(cbind(cost,profit,profit[,1]-cost,profit[,2]-cost));
    colnames(Results) <- c("cost","Profit_wo_misclass", "Profit_with_misclass","net_profit_wo_misclass", "net_profit_with_misclass")
    #fill in results table results_df 
    # c('alpha','prob_misclass','opt_nr_of_classes_w_misclass','opt_nr_of_classes_wo_misclass','max_net_profit_w_missclass','max_net_profit_wo_misclass')
    
    r3=which.max(Results$net_profit_with_misclass)
    r4=which.max(Results$net_profit_wo_misclass)
    r5=Results$net_profit_with_misclass[r3]
    r6=Results$net_profit_wo_misclass[r4]
    results_df[nrow(results_df) + 1,] = c(alpha,pij[1],r3,r4,r5,r6)
    
  
    #
    return(c(r5,r3))
    
  }
#}
#export results to excel for debug
#write.xlsx(results_df, file = paste0("C:..."1st_Scenario_Profit_per_class_....xlsx"))


alpha_misclass<- data.frame(matrix(ncol = 4, nrow = 0))
colnames(alpha_misclass) <- c('prob_misclas','alpha','nr risk classes_d','nr risk classes_n')
f2<-function(misclass_rate1, alpha1) {profit_function(misclass_rate1,1)[1]-profit_function(0,alpha1)[1]}
for(misclass_rate1 in seq(0,0.3,by=0.025)){f3<-function(alpha1) {f2(misclass_rate1,alpha1)[1]}
alpha_misclass[nrow(alpha_misclass) + 1,1]<-misclass_rate1
alpha_misclass[nrow(alpha_misclass),2]<-as.numeric(try(uniroot(f3,c(0,1))$root, silent=T))
alpha_misclass[nrow(alpha_misclass),3]<-profit_function(misclass_rate1,1)[2]
alpha_misclass[nrow(alpha_misclass),4]<-as.numeric(try(profit_function(0,alpha_misclass[nrow(alpha_misclass),2])[2]))
}

#write.xlsx(alpha_misclass, file = paste0("C:/...",cost[2],".xlsx"))

