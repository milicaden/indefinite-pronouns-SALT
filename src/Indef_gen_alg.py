from __future__ import division
import math
import pandas as pd
import numpy as np
import random
import pygmo
from copy import copy

######################################
# Define relevant functions
######################################

# Probability of a flavor based on Beekhuizen's corpus
def probaf(state):
    return beekhuizen_priors[state].iloc[0]

# Probability of a flavor given an item
def probafgi(state, word, df):
    temp = df[df['ITEM']== word]
    if temp['numoffl'].values[0] == 0:
        return 0
    else:
        return temp[state+"flavor"].values[0]/temp['numoffl'].values[0]

# Probability of an item given a flavor
def probaigf(state, word, df):
    temp = df[df['ITEM']== word]
    if temp['numof'+state].values[0] == 0:
        return 0 #this step to assign probability of each item given some flavor x to be 0 if no item can express that flavor in the language
    else:
        return temp[state+"flavor"].values[0]/temp['numof'+state].values[0]


# Utility of a language (asumming u(s,s')=1 if s=s', 0 otherwise).
def utility(language, df):
    temp = df[df['LANG'] == language]
    u = 0
    for i in temp['ITEM']:
        for k in ["sk", "su", "ns", "npi", "fci", "nq"]:
            u += probaf(k)*probafgi(k,i,df)*probaigf(k,i,df)
    return u

# Communicative cost of a language (perhaps this should be smth else)
def cost(language, df):
    if utility(language, df) == 0:
        return np.nan
    return 1/utility(language, df)

# Complexity of a language in terms of Haspelmath's features
def featurecomplexity(language, df):
    temp = df[df['LANG'] == language]
    u = 0
    for i in temp['ITEM']:
        temp2 = temp[temp['ITEM'] == i]
        currentitemcomplexity = temp2['complexity'].values[0]
        u += currentitemcomplexity
    return u

# Compute cost and complexity of languages in a data frame
def costcomplexity(data_frame):
    languages = np.unique(data_frame['LANG'])
    complexityoflanguages = []
    for i in languages:
        k = featurecomplexity(i, data_frame)
        complexityoflanguages.append(k)

    costoflanguages = []
    for i in languages:
        k = cost(i, data_frame)
        costoflanguages.append(k)

    all = pd.DataFrame(list(zip(languages, complexityoflanguages, costoflanguages)),
                       columns=['LANG', 'complexityoflanguages', 'costoflanguages'])

    all.dropna()
    return all


#A function that takes a data frame with raw data, and computes all the measures necessary to later evaluate cost&complexity
def prep(temp):
    # prepare the data frame to compute cost and complexity
    # store flavors expressible by an item in 'meaning' column
    temp['meaning'] = np.where(temp['skflavor'] == 1, 'sk-', 'X-')
    temp['meaning'] = np.where(temp['suflavor'] == 1, temp['meaning'] + 'su-', temp['meaning'] + 'X-')
    temp['meaning'] = np.where(temp['nsflavor'] == 1, temp['meaning'] + 'ns-', temp['meaning'] + 'X-')
    temp['meaning'] = np.where(temp['npiflavor'] == 1, temp['meaning'] + 'npi-', temp['meaning'] + 'X-')
    temp['meaning'] = np.where(temp['fciflavor'] == 1, temp['meaning'] + 'fci-', temp['meaning'] + 'X-')
    temp['meaning'] = np.where(temp['nqflavor'] == 1, temp['meaning'] + 'nq', temp['meaning'] + 'X')

    # get feature content of an item based on which flavors it can express
    temp = temp.merge(mindesc, on=["meaning"])

    # compute how many flavors each item can express
    temp['numoffl'] = temp['skflavor'] + temp['suflavor'] + temp['nsflavor'] + temp['npiflavor'] + temp['fciflavor'] + temp['nqflavor']

    # compute for each language how many items can express each of the flavors
    temp2 = temp.groupby('LANG').agg(
        {'skflavor': 'sum', 'suflavor': 'sum', 'nsflavor': 'sum', 'npiflavor': 'sum', 'fciflavor': 'sum',
         'nqflavor': 'sum'}).rename(
        columns={'skflavor': 'numofsk', 'suflavor': 'numofsu', 'nsflavor': 'numofns', 'npiflavor': 'numofnpi',
                 'fciflavor': 'numoffci', 'nqflavor': 'numofnq'})

    temp = temp.merge(temp2, on=["LANG"])
    return temp

# Remove takes a language df, and removes one row (one item) from the language if l has more than 1 item
def remove(language):
    if len(language.index) > 1: #only remove if the language has more than one word
        index = random.randint(0,len(language.index)-1)
        removed = language.drop(language.index[index])
    else:
        removed = language
    removed.reset_index(inplace=True, drop = True)
    return removed

# Add an item to the language (the function imports a data file with all logically possible items)
def add(language):
    items = copy(allitems) #allitems is a data file with all logically possible items
    index = random.randint(0, len(items.index) - 1)
    item = copy(items.iloc[[index]])
    item['LANG'] = language['LANG'].iloc[0]
    count = 0
    for s in language.ITEM.unique():
        while 'item' + str(count) in s:
            count += 1 #we do this count to assign the new name to an item
    item['ITEM'] = language['LANG'].iloc[0]+'-item' + str(count)
    temp = pd.concat([language, item])
    temp.reset_index(inplace=True, drop = True)
    return temp

# Interchange an item in the language
def interchange(language):
    return remove(add(language))

# Define what it means to mutate a language
def mutate(language):
    possible_mutations = [interchange]
    if len(language.index) < lang_size:
         possible_mutations.append(add)
    if len(language.index) > 1:
         possible_mutations.append(remove)
    mutation = random.choice(possible_mutations)
    return mutation(language)

# Derive 'amount' of languages from 'language_list' via mutations
def sample_mutated(languages_list, amount, generation, max_mutations):
     amount -= len(languages_list.LANG.unique()) #this is so that the mutations are added up to the original languages so that their sum = amount
     amount_per_language = int(math.floor(amount / len(languages_list.LANG.unique())))
     amount_random = amount % len(languages_list.LANG.unique())

     mutated_languages = pd.DataFrame(columns=list(allitems))


     for language in languages_list.LANG.unique():
         for i in range(amount_per_language):
             num_mutations = random.randint(1, max_mutations)
             mutated_language = copy(languages_list[languages_list['LANG'] == language])
             for j in range(num_mutations):
                mutated_language = mutate(mutated_language)
                mutated_language.reset_index(inplace=True, drop=True)

             mutated_language['LANG'] = language + 'gen' +str(generation) + 'mutoff' + str(i)
             mutated_language['ITEM'] = [sub.replace(language, language + 'gen' +str(generation) + 'mutoff' + str(i)) for sub in mutated_language['ITEM']]
             mutated_languages = pd.concat([mutated_languages, mutated_language])
             mutated_languages.reset_index(inplace=True, drop=True)

     for i in range(amount_random):
         language = random.choice(languages_list.LANG.unique())
         mutated_language = mutate(languages_list[languages_list['LANG'] == language])
         mutated_language['LANG'] = language + 'gen' + str(generation) + 'rcoff' + str(i)
         mutated_language['ITEM'] = [sub.replace(language, language + 'gen' +str(generation) + 'rcoff' + str(i)) for sub in mutated_language['ITEM']]
         mutated_languages = pd.concat([mutated_languages, mutated_language])
         mutated_languages.reset_index(inplace=True, drop=True)

     mutated_languages = pd.concat([mutated_languages, languages_list])
     mutated_languages.reset_index(inplace=True, drop=True)

     return mutated_languages

######################################
# Evolutionary algorithm
######################################

if __name__ == "__main__":

    # Read relevant files
    Folder = "../data/"
    beekhuizen_priors = pd.read_csv(Folder + "Beekhuizen_priors.csv")#file with prior probability distribution over flavors extracted from Beekhuizen et al.'s annotated corpus
    mindesc = pd.read_csv(Folder + "minimum-desc-indef.csv", delimiter = ";") #file with complexity information for each item (based on their shortest descriptions)
    allitems =  pd.read_csv(Folder + "allitems.csv") #file with 63 logically possible items (the one that can't convey any of the flavors is removed)
    allitems.reset_index(inplace=True, drop=True)
    lang_size = 10 #max number of words in languages

    # Generate generation 0 that should have 2000 languages
    languages = pd.DataFrame(columns=list(allitems))
    for i in range(2000):
        j = random.randint(1,lang_size) #number of words
        for a in range(j):
            index = random.randint(0, len(allitems.index) - 1)
            item = copy(allitems.iloc[[index]])
            item[['LANG']] = 'lang'+str(i)
            item[['ITEM']] = 'lang'+str(i)+'-item'+str(a)
            item.reset_index(inplace=True, drop = True)
            languages = pd.concat([languages, item])
            languages.reset_index(inplace=True, drop = True)


    # Run the evolutionary algorithm for 100 generations, each with 2000 languages
    num_of_gen = 100
    sample_size = 2000
    max_mut = 3

    for i in range(1,num_of_gen+1):
        temp = copy(languages)
        temp = prep(temp)
        tempcc = costcomplexity(temp)
        tempcc['new_col'] = list(zip(tempcc.costoflanguages, tempcc.complexityoflanguages))
        dominating_indices = pygmo.non_dominated_front_2d(tempcc['new_col'])
        dominating_languages_list = [tempcc.LANG[k] for k in dominating_indices] #this was bound by upper i somehow!!!
        dominating_languages_df = languages[languages['LANG'].isin(dominating_languages_list)]
        languages = sample_mutated(dominating_languages_df, sample_size, i, max_mut)

    languages = prep(languages)
    costcomlang = costcomplexity(languages)
    costcomlang.to_csv(r''+Folder+'finalgencostcom.csv', index=False)

    # Find the dominant languages from the final generation + languages of Experiment1
    allfinal = pd.read_csv(Folder + "all_complexity_cost_exp1.csv")# file with languages of Experiment 1
    allfinaluntyped = allfinal.drop(columns = ['type','syn_index'])
    domselect = pd.concat([costcomlang, allfinaluntyped])
    domselect.reset_index(inplace=True, drop=True)

    domselect['new_col'] = list(zip(domselect.costoflanguages, domselect.complexityoflanguages))
    dominating_indices = pygmo.non_dominated_front_2d(domselect['new_col'])
    dominating_languages_list = [domselect.LANG[i] for i in dominating_indices]
    domselect = domselect[domselect['LANG'].isin(dominating_languages_list)]
    domselect.to_csv(r''+Folder+'pareto_dominant.csv', index=False)