library(rlist)
library(stringr)

##############################
# Define useful functions
##############################

# Combine each element of a set with each element of a different set via union and intersetion
combine = function(component1, component2){
  set <- list()
  for(a in 1:length(component1)){
  for (b in 1:length(component2)){
    if(identical(component1,component2) & a>=b){ #filtering one of the orders and unions/intersections of a set with itself
      next
    }
    i = unlist(component1[a], use.names=FALSE)
    j = unlist(component2[b], use.names=FALSE)
    oldnames <- names(set)
    flav.comb1 = union(i,j)
    flav.comb2 = intersect(i,j)
    set = list.append(set, flav.comb1, flav.comb2)
    names(set) <- c(oldnames, paste("union", names(component1[a]), names(component2[b])), paste("intersect", names(component1[a]), names(component2[b])))
  }
}
 return(set)
}


# Filter function: to ease further computations, we further remove (i) feature combinations which result in an empty set; (ii) from all possible combinations of two features which result in the same list of descriptions, we preserve only one
# We care not to remove combinations containing seplus.
filter = function(set){
  set = set[set != "character(0)"]
  unwanted = c()
  for(i in 1:length(set)){
    for(j in 1:length(set)){
      if(!setequal(unlist(set[i], use.names = FALSE), unlist(set[j], use.names = FALSE))){
        next
      }
      if(i>=j){
        next
      }
      if(grepl("seplus", names(set[j]))&!grepl("seplus", names(set[i]))){ #do not eliminate feature combinations that contain seplus
        next
      }
      unwanted = c(unwanted, j)
    }
  }
  set = set[-unwanted]
  return(set)
}

# Generate all descriptions up to a certain depth (depth = number of features in the descriptions) starting with build_blocks whose depth is 1
generate = function(depth, build_blocks){
  start = 2
  while (start < depth + 1){
    nam <- paste0("build_blocks", start)
    assign(nam, c())
  for(x in 1:start){
    for(y in 1:start){
      if(x+y == start & x <= y){
        current = combine(eval(parse(text = paste0("build_blocks", x))), eval(parse(text = paste0("build_blocks", y))))
        assign(paste0("build_blocks", start), c(eval(parse(text = paste0("build_blocks", start))), current))
      }
    }
  }
   assign(paste0("build_blocks", start), filter(eval(parse(text = paste0("build_blocks", start)))))
   start = start+1
  }
  all = c()
  for(i in 1:depth){
    all = c(all, eval(parse(text = paste0("build_blocks", i))))
  }
  return(all)
}

##############################
# Features, ie. build_blocks1
##############################
kplus = c("sk")
kminus = c("su", "ns", "npi", "fci", "nq")
specplus = c("sk", "su")
specminus = c("ns", "npi", "fci", "nq")
seplus = c("npi", "fci", "nq")
seminus = c("sk", "su", "ns")
rplus = c("npi", "nq")
rminus = c("fci")
negplus = c("nq")
negminus = c("sk", "su", "ns", "npi", "fci")

build_blocks1 = list(kplus, kminus, specplus, specminus, seplus, seminus, rplus, rminus, negplus, negminus)
names(build_blocks1) <- c("kplus", "kminus", "specplus", "specminus", "seplus", "seminus", "rplus", "rminus", "negplus", "negminus")

##############################
# Generate all descriptions
##############################
descriptions = generate(6, build_blocks1)
#With up to 6 features, we capture all 63 logically possible flavor combinations. 

##############################
# Selecting the descriptions with minimun complexity
##############################
#Compute complexity
df <- data.frame(names(descriptions), I(descriptions))
df$complexity = str_count(df$names.descriptions., "kplus") + str_count(df$names.descriptions., "kminus") + str_count(df$names.descriptions., "specplus") + str_count(df$names.descriptions., "specminus") + str_count(df$names.descriptions., "seplus") + str_count(df$names.descriptions., "seminus") + str_count(df$names.descriptions., "rplus") + str_count(df$names.descriptions., "rminus") + str_count(df$names.descriptions., "negplus") + str_count(df$names.descriptions., "negminus") 

#According to Haspelmath, you can only have rplus or rminus feature in the intersection with se+ feature. So we filter those that don't satisfy this.
#Note that this command removes most but not all the cases which are not allowed, but we verify later that none of those is selected as the best description.

df <- subset(df, !((grepl("rplus",df$names.descriptions.)|grepl("rminus",df$names.descriptions.))&!(grepl("seplus",df$names.descriptions.)&grepl("intersect",df$names.descriptions.))))

#Arranging nicely the meaning column
df$meaning <- ifelse(grepl("sk", df$descriptions),"sk-","X-")
df$meaning <- ifelse(grepl("su", df$descriptions),paste0(df$meaning,"su-"),paste0(df$meaning,"X-"))
df$meaning <- ifelse(grepl("ns", df$descriptions),paste0(df$meaning,"ns-"),paste0(df$meaning,"X-"))
df$meaning <- ifelse(grepl("npi", df$descriptions),paste0(df$meaning,"npi-"),paste0(df$meaning,"X-"))
df$meaning <- ifelse(grepl("fci", df$descriptions),paste0(df$meaning,"fci-"),paste0(df$meaning,"X-"))
df$meaning <- ifelse(grepl("nq", df$descriptions),paste0(df$meaning,"nq"),paste0(df$meaning,"X"))

#Create the data frame that stores for each meaning combination its (minimum) complexity
minimum.df = df[FALSE,]
for(i in unique(df$meaning)){
  temp = subset(df, meaning == i)
  minimum = min(temp$complexity)
  temp.min = subset(temp, complexity == minimum)
  minimum.df = rbind(minimum.df, temp.min[1,])
}

write.csv2(minimum.df,'minimum-desc-indef.csv')