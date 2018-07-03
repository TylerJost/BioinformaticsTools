# -*- coding: utf-8 -*-
"""
Created on Tue Jul  3 11:10:51 2018

@author: jost
"""
# Generate all codon combinations from a given protein sequence. 
# WARNGING: This can get really large, really fast. So be warned. I have a
# pretty beefy computer and it can get bogged down since there are so many 
# possible combinations.

from itertools import product

#==============================================================================
# Functions
#==============================================================================

# Take a stripped down codon table and turn it into a dictionary. Keys are the 
# protein codes, values are the codons (in a list)
def prot2Codon(fileName):
    codonDict = {}
    # Read in the CSV table and split it by new lines
    codonTableCSV = open(fileName,'r')
    ct = codonTableCSV.read()
    ct = ct[0:len(ct)-1]
    ct = ct.split('\n')
    # For every row, split it by its commas
    for row in ct:
        rowList = row.split(',')
    # For every third item, take the item two away from it
        for item in range(0,len(rowList),3):
            code = rowList[item+1]
            codon = rowList[item]
            # Add the codon to the appropriate protein key
            if code in codonDict:
                codonDict[code].append(codon)
            else:
                codonDict[code] = [codon]
    codonTableCSV.close()
    return codonDict

# Generate all codon combinations
def genCodonCombo(seq,codonDict):
    # Make a list of lists containing the possible combinations
    codonList = []
    for protein in seq:
        codonList.append(codonDict[protein])
    
    # Use the module product to generate all combinations
    codonCombo = list(product(*codonList))
    # Join these together because otherwise they outuput as separated tuples
    codonComboCombine = []
    for combo in codonCombo:
        codonComboCombine.append(''.join(combo))
    return codonComboCombine

#==============================================================================
# Input/Code
#==============================================================================

# Sequence
seq = 'HSYIN'
# Make codon tables
standardCodons = prot2Codon('standardCodonTable.csv')
# Make list of codon combinations
codonCombos = genCodonCombo(seq,standardCodons)

print(codonCombos)
