'''
Programming Project: Frequent Itemset Mining Algorithms
Author: Chunlei Zhou
2019/10/10 version 1.0
Due: 2019/10/17
Algorithms used: 
	(1) Apriori [AS94b]
Use the UCI Adult Census Dataset
http://archive.ics.uci.edu/ml/datasets/Adult
'''

# Apriori [AS94b]
import pandas as pd
from collections import defaultdict
import time

def find_frequent_1_itemset(D, min_sup):
    item_count = defaultdict(int)
    for transaction in D:
        for item in transaction:
            item_count[item] += 1
    L1 = []
    for item, count in item_count.items():
        if count >= min_sup:
            L1.append({item})
    return L1

def has_frequent_subset(c, Lk_1):
    for t in c:
        k_1_subset = c - set([t])
        if k_1_subset not in Lk_1:
            return True
    return False


def apriori_gen(Lk_1, k):
    Ck = []
    for l1 in Lk_1:
        for l2 in Lk_1:
            c = l1.union(l2)
            if len(c) == k:
                if not has_frequent_subset(c, Lk_1) and c not in Ck:
                    Ck.append(c)
    return Ck

def apriori(D, min_sup):
    L = []
    L0 = []
    k = 2
    L1 = find_frequent_1_itemset(D, min_sup)
    L.append(L0)
    L.append(L1)
    while len(L[k-1]) != 0:
        Lk = []
        Ck = apriori_gen(L[k-1], k)
        c_count = defaultdict(int)
        for c in range(len(Ck)):
            for t in D:
                if Ck[c].issubset(t):
                    c_count[c] += 1
        for itemset, count in c_count.items():
            if count >= min_sup:
                Lk.append(Ck[itemset])
        L.append(Lk)
        k += 1

    L.remove(L0)
    return L

if __name__ == "__main__":
    '''
    dataset = pd.read_csv('/Users/zhouchunlei/Desktop/DATA SCIENCE/Data Mining/Homework/Project Apriori/adult.data.csv')
    df = dataset.transpose()
    Sk = []
    for column in df.columns:
        Sk.append(set((df[column])))
    '''
    start_time = time.time()
    with open('/Users/zhouchunlei/Desktop/DATA SCIENCE/Data Mining/Homework/Project Apriori/adult.data.csv') as file:
        ds = file.readlines()
    transactions = []
    for i in range(len(ds)):
        transactions.append(ds[i].strip().split(','))
    Sk = []
    for transaction in transactions:
        Sk.append(set(transaction))
    min_sup = len(transactions) * 0.5
    L = apriori(Sk, min_sup)
    print("The executing time of Apriori algorithm is %s seconds." % (time.time() - start_time))
    print(L)
    print('Implementing Apriori Algorithm, longest itemset with support more than 1000 has a length of', len(L)-1)