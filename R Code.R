library(brms)


model1=read.csv("~/model1.csv", header=TRUE)
model2=read.csv("~/model2.csv", header=TRUE)
model3a=read.csv("~/model3a.csv", header=TRUE)
model3b=read.csv("~/model3b.csv", header=TRUE)


#####first model - individuals with high in-group cooperative tendencies share more food with out groups 

##Cooperative_tendencies standardized to a mean of 0 and SD of 1
##Observation_effort - is log transfromed

m1.brm=brm(Response~Cooperative_tendencies+Sex+Group + offset(Observation_effort) + (1 | individual), 
		data=model1, family = poisson, control = list(adapt_delta = 0.99),
		prior = c(prior(normal(0, 2.5), class = Intercept),
                prior(normal(0, 2.5), class = b)))

#how much of the variation in the response is explained 
bayes_R2(m1.brm)



#####second model - between-group partners with high joint cooperative scare are more likely to share food with one another 

##Joint_cooperation_score, Grooming_score, Coalition_score are standardized to a mean of 0 and SD of 1
##Observation_effort - is log transfromed


m2.brm=brm(Response~Joint_cooperation_score+Grooming_score+Coalition_score+Sex_combination+offset(Observation_effort)+(1|Dyad)+(1|Individual1)+(1|Individual2), 
		data=model2, family = poisson, control = list(adapt_delta = 0.99),
		prior = c(prior(normal(0, 2.5), class = Intercept),
        prior(normal(0, 2.5), class = b)))


#how much of the variation in the response is explained 
bayes_R2(m2.brm)




#####third model - more generous individuals also receive food from more partners within (a) and between (b) groups 

#In_group.out_degree, In_group.in_degree, Out_group.out_degree,	Out_group.in_degree are standardized to a mean of 0 and SD of 1

##within group
m3a <- 
	data = model3a, 
      In_group.out_degree ~ 0 + In_group.in_degree,
      prior = c(prior(normal(0, 1), class = b),
                prior(normal(0, 1), class = sigma)),
      chains = 4, cores = 4, 
      seed = 1)


##between groups
m3b <- 
	data = model3b, 
      Out_group.out_degree ~ 0 + Out_group.in_degree,
      prior = c(prior(normal(0, 1), class = b),
                prior(normal(0, 1), class = sigma)),
      chains = 4, cores = 4, 
      seed = 1)


