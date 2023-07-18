library(brms)


model1_sharing=read.csv("~/model1_sharing.csv", header=TRUE)
model1_grooming=read.csv("~/model1_grooming.csv", header=TRUE)
model1_coalition=read.csv("~/model1_coalition.csv", header=TRUE)


model2_sharing=read.csv("~/model2_sharing.csv", header=TRUE)
model2_grooming=read.csv("~/model2_grooming.csv", header=TRUE)
model2_coalition=read.csv("~/model2_coalition.csv", header=TRUE)


model3a_sharing=read.csv("~/model3a_sharing.csv", header=TRUE)
model3b_sharing=read.csv("~/model3b_sharing.csv", header=TRUE)
model3a_grooming=read.csv("~/model3a_grooming.csv", header=TRUE)
model3b_grooming=read.csv("~/model3b_grooming.csv", header=TRUE)



#####first model - individuals with high in-group cooperative tendencies are more likely to cooperate with out-group members
##Cooperative_tendencies standardized to a mean of 0 and SD of 1
##Observation_effort - is log transfromed

###food sharing
m1.share=brm(Response~Cooperative_tendencies+Sex+Group + offset(Observation_effort) + (1 | individual), 
		data=model1_sharing, family = poisson, control = list(adapt_delta = 0.99),
		prior = c(prior(normal(0, 2.5), class = Intercept),
                prior(normal(0, 2.5), class = b)))

#how much of the variation in the response is explained 
bayes_R2(m1.share)


###grooming
m1.groom=brm(Response~Cooperative_tendencies+Sex+Group + offset(Observation_effort) + (1 | individual), 
		data=model1_grooming, family = poisson, control = list(adapt_delta = 0.99),
		prior = c(prior(normal(0, 2.5), class = Intercept),
                prior(normal(0, 2.5), class = b)))

#how much of the variation in the response is explained 
bayes_R2(m1.groom)

###coalition
m1.coalition=brm(Response~Cooperative_tendencies+Sex+Group + offset(Observation_effort) + (1 | individual), 
		data=model1_coalition, family = poisson, control = list(adapt_delta = 0.99),
		prior = c(prior(normal(0, 2.5), class = Intercept),
                prior(normal(0, 2.5), class = b)))

#how much of the variation in the response is explained 
bayes_R2(m1.coalition)


#####second model - between-group partners with high joint cooperative scare are more likely to cooperate with one another 

##Joint_cooperation_score, Grooming_score, Coalition_score are standardized to a mean of 0 and SD of 1
##Observation_effort - is log transfromed

##food sharing
m2.share=brm(Response~Joint_cooperation_score+Grooming+Coalition+Sex_combination+offset(Observation_effort)+(1|Dyad)+(1|Individual1)+(1|Individual2), 
		data=model2_sharing, family = poisson, control = list(adapt_delta = 0.99),
		prior = c(prior(normal(0, 2.5), class = Intercept),
        prior(normal(0, 2.5), class = b)))


#how much of the variation in the response is explained 
bayes_R2(m2.share)


##grooming
m2.groom=brm(Response~Joint_cooperation_score+Food_sharing+Coalition+Sex_combination+offset(Observation_effort)+(1|Dyad)+(1|Individual1)+(1|Individual2), 
		data=model2_grooming, family = poisson, control = list(adapt_delta = 0.99),
		prior = c(prior(normal(0, 2.5), class = Intercept),
        prior(normal(0, 2.5), class = b)))


#how much of the variation in the response is explained 
bayes_R2(m2.groom)


##coalition
m2.coalition=brm(Response~Joint_cooperation_score+Grooming+Food_sharing+Sex_combination+offset(Observation_effort)+(1|Dyad)+(1|Individual1)+(1|Individual2), 
		data=model2_coalition, family = poisson, control = list(adapt_delta = 0.99),
		prior = c(prior(normal(0, 2.5), class = Intercept),
        prior(normal(0, 2.5), class = b)))


#how much of the variation in the response is explained 
bayes_R2(m2.coalition)




#####third model - more generous individuals also receive food from more partners within (a) and between (b) groups 

#In_group.out_degree, In_group.in_degree, Out_group.out_degree,	Out_group.in_degree are standardized to a mean of 0 and SD of 1

##within group
##food sharing
m3a.s <- 
	data = model3a_sharing, 
      In_group.out_degree ~ 0 + In_group.in_degree,
      prior = c(prior(normal(0, 1), class = b),
                prior(normal(0, 1), class = sigma)),
      chains = 4, cores = 4, 
      seed = 1)

##grooming
m3a.g <- 
	data = model3a_grooming, 
      In_group.out_degree ~ 0 + In_group.in_degree,
      prior = c(prior(normal(0, 1), class = b),
                prior(normal(0, 1), class = sigma)),
      chains = 4, cores = 4, 
      seed = 1)


##between groups
##food sharing
m3b.s <- 
	data = model3b_sharing, 
      Out_group.out_degree ~ 0 + Out_group.in_degree,
      prior = c(prior(normal(0, 1), class = b),
                prior(normal(0, 1), class = sigma)),
      chains = 4, cores = 4, 
      seed = 1)
	  
##grooming
m3b.g <- 
	data = model3b_grooming, 
      Out_group.out_degree ~ 0 + Out_group.in_degree,
      prior = c(prior(normal(0, 1), class = b),
                prior(normal(0, 1), class = sigma)),
      chains = 4, cores = 4, 
      seed = 1)


