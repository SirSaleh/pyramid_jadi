# I hope I win :)
# initialize
ir_population <- 70000000;
current_month <- 0;
member_count <- 1;
entered_size_in_month <- {1};
generations_subset <- {};
benefitman_least_months <- 4;
benefitman_least_subs <- 6; 


#functions
legal_enterers <- function(ent_size_array,cu_month){
print (paste("===============",ent_size_array));
num <- 0;
	for (i in c((cu_month-1):(cu_month-6))){
		if (i >= 1){
			#print (paste("Yes: i= ",i,"curm",cu_month,"arr",ent_size_array,"legs= ",num));
			if (is.numeric(ent_size_array[i])){
				num = num + ent_size_array[i]; 
			}
			else{
				num=1;
			}
		}else{
			print (paste("No: i= ",i,"curm",cu_month));
			break;
		}
	}
	print (num)
	#print (paste(ent_size_array,"===",cu_month," legalinterees---",num));
	return (num);
}
benefs <- function(ent_size_array){
	perfect_idx = perfect.subset.gerations(ent_size_array);
	benefs = 0;
	index = c(1:length(ent_size_array))
	#ent_size_array = ent_size_array[index<=(length(ent_size_array)-benefitman_least_months)]; #obtimizing
	#TODO Whe are in this loops
	for (i in 1:length(ent_size_array)){ #I set it to 2 because first generation is just 1 guy.
		suitable_months=0;
		subsets=0;
		for (j in (i+1):length(ent_size_array)){
			is.perfect = FALSE;
			#if each i element has true result of perfectness and "" in aggregate, all of it's people's generation are benficiary. else need to deterine how much of it
			if (perfect_idx[j]){
				subsets=subsets+2^(j-i)
				suitable_months= suitable_months+1;
				if (subsets>=benefitman_least_subs && suitable_months>=benefitman_least_months){is.perfect=TRUE;break;}
			}else{
				#TODO: Here we arrive to end of the pyramid
				
			}
			
		}
		if (is.perfect){benefs= benefs+ent_size_array[i];}
	}
	return (benefs);
}
perfect.subset.gerations <- function(ent_size_array){
	#TODO _check_perfectness
	perfects = {TRUE};
	for (i in 2:length(ent_size_array)){
		if (ent_size_array[i+1]==legal_enterers(ent_size_array,i)){
			perfects[i]=TRUE;
		}
		else{
			perfects[i]=FALSE;
		}
	}
	return(perfects)
}


## Our main operation
while (member_count < ir_population && member_count + legal_enterers(entered_size_in_month,current_month+1) * 2 <ir_population){
	#print (paste(current_month,"===",member_count))
	current_month = current_month + 1;
	 #???? ### Key: listen legal_enterers do nothing with current entered size
	print (paste("here1 current month is",current_month))
	if (current_month!=1){
		print (paste("not first, current month is",current_month,"membercount ",member_count,"legals: --->",legal_enterers(entered_size_in_month,current_month),"en in month: ", entered_size_in_month))
	 	member_count <- member_count + legal_enterers(entered_size_in_month,current_month) * 2;
		entered_size_in_month[current_month] = legal_enterers(entered_size_in_month,current_month)*2
	}else{
		member_count = 1;
		entered_size_in_month[current_month] = 1;
		print (paste("res=",entered_size_in_month))
	}
}

if (member_count < ir_population){
	current_month <- current_month + 1;
	entered_size_in_month[current_month] <- ir_population-sum(entered_size_in_month);
	member_count = sum(entered_size_in_month);
}

# Show results 
print (paste("It lasts",current_month," months to all iranians be in this **cking system."));
