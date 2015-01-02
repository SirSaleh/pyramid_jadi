# Hi dear jadi; I hope I win :)
#To run this codes just copy it in R console! you will see the result
# Note: you can change this initial values to calculate results for different values ie different populations
#initialize 
ir_population <- 70000000;
Population_name = "Iranians"
current_month <- 0;
member_count <- 1;
entered_size_in_month <- {1};
generations_subset <- {};
benefitman_least_months <- 4;
benefitman_least_subs <- 6; 
##########

#functions
legal_enterers <- function(ent_size_array,cu_month){ # Calculate integer value for number of legal members to enter new members
num <- 0;
	for (i in c((cu_month-1):(cu_month-benefitman_least_months))){
		if (i >= 1){
			if (is.numeric(ent_size_array[i])){
				num = num + ent_size_array[i]; 
			}
			else{
				num=1;
			}
		}else{
			break;
		}
	}
	return (num);
}
benefs <- function(ent_size_array){
	benefs = 0;
	index = c(1:length(ent_size_array))
	for (i in 1:(length(ent_size_array)-benefitman_least_months)){ 
		suitable_months=0;
		subsets=0;
		if (i+benefitman_least_months < length(ent_size_array)){ #I check if we are at the end of pyramid or not!
			for (j in (i+1):length(ent_size_array)){
								
				subsets=subsets+2^(j-i);
				suitable_months= suitable_months+1;
				if (subsets>=benefitman_least_subs && suitable_months>=benefitman_least_months){is.perfect=TRUE;break;}
							
			}
		} else if (i+benefitman_least_months == length(ent_size_array)){
			for (j in (i+1):length(ent_size_array)){
				if (j!=length(ent_size_array)){
					subsets=subsets+2^(j-i);
					suitable_months= suitable_months+1;
				}else{
					if (subsets>=benefitman_least_subs && suitable_months>=benefitman_least_months){is.perfect=TRUE;break;}
				}
			}
			
		}
		if (is.perfect){benefs= benefs+ent_size_array[i];}
	}
	return (benefs);
}



## Our main operation
while (member_count < ir_population && member_count + legal_enterers(entered_size_in_month,current_month+1) * 2 <ir_population){
	current_month = current_month + 1;

	if (current_month!=1){
		
	 	member_count <- member_count + legal_enterers(entered_size_in_month,current_month) * 2;
		entered_size_in_month[current_month] = legal_enterers(entered_size_in_month,current_month)*2
	}else{
		member_count = 1;
		entered_size_in_month[current_month] = 1;
	}
}

if (member_count < ir_population){
	current_month <- current_month + 1;
	entered_size_in_month[current_month] <- ir_population-sum(entered_size_in_month);
	member_count = sum(entered_size_in_month);
}

# Show results 
cat ("*****\nIt lasts",current_month,"months to all ",Population_name,"be in this **cking system and just ",benefs(entered_size_in_month)," People (about",(benefs(entered_size_in_month)/ir_population)*100,"%) are Happy :)\n******");
#end
