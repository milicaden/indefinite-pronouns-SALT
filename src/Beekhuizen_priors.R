Folder = "../data/"
# Import full corpus from Beekhuizen study
data.full = read.csv(paste0(Folder, 'beekhuizen_full_set.csv'), sep = ',')
data.full.sub = droplevels(data.full[data.full$source != 'excluded' & data.full$type %in% c('body', 'thing') & data.full$annotation != 'UF',])

###################
###Translating this to flavor frequencies based on English indefinite pronouns for person
###################
data.full.sub1 = subset(data.full.sub, type == "body")
data.full.sub1$flavor = data.full.sub1$annotation
data.full.sub1$flavor <- as.character(data.full.sub1$flavor)

# If 'nobody', flavor is NQ (there is no instances of no-one in the corpus)
data.full.sub1$flavor = ifelse(data.full.sub1$en == "nobody", "NQ", data.full.sub1$flavor)
# If 'anybody/anyone' and functions question, indirect negation, direct negation, flavor is NPI
data.full.sub1$flavor = ifelse((data.full.sub1$annotation == "QU"|data.full.sub1$annotation == "IN"|data.full.sub1$annotation == "DN")&(data.full.sub1$en == "anybody"|data.full.sub1$en == "anyone"), "NPI", data.full.sub1$flavor)
# If 'anybody/anyone' and functions conditional or comparative, flavor is half of the time NPI and half of the time FCI
data.full.sub1$flavor = ifelse((data.full.sub1$annotation == "CP"|data.full.sub1$annotation == "CD")&(data.full.sub1$en == "anybody"|data.full.sub1$en == "anyone"), "NPI_FCI", data.full.sub1$flavor)
# If 'somebody/someone' and functions question, indirect negation, conditional, direct negation or comparative, flavor is NS
data.full.sub1$flavor = ifelse((data.full.sub1$annotation == "QU"|data.full.sub1$annotation == "IN"|data.full.sub1$annotation == "CD"|data.full.sub1$annotation == "DN"|data.full.sub1$annotation == "CP")&(data.full.sub1$en == "somebody"|data.full.sub1$en == "someone"), "NS", data.full.sub1$flavor)

# Otherwise, flavor == function
flavors = prop.table(xtabs(~ data.full.sub1$flavor))

#Attribute half of NPI_FCI to FC, and the other half to NPI
flavors["FCI"] = flavors["FC"] + flavors["NPI_FCI"]/2
flavors["NPI"] = flavors["NPI"] + flavors["NPI_FCI"]/2
flavors["SK"] = flavors["SP"]/2
flavors["SU"] = flavors["SP"]/2

flavors = data.frame(rbind(flavors))
flavors["NPI_FCI"] = NULL
flavors["SP"] = NULL
flavors["FC"] = NULL
names(flavors)<- tolower(names(flavors))
write.csv(flavors, paste0(Folder, 'Beekhuizen_priors.csv'), row.names=FALSE)




