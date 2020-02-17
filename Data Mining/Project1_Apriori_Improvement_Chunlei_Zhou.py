'''
Programming Project: Frequent Itemset Mining Algorithms
Author: Chunlei Zhou
2019/10/12 version 1.0
Due: 2019/10/17
Algorithms used:
	(3) Improvement of (1)
	A.Savasere,E.Omiecinski,andS.Navathe.
	Anefficientalgorithmformining association rules in large databases. VLDB'95
	'Partition Algorithm' (Chosen randomly from the given papers.)
Use the UCI Adult Census Dataset
http://archive.ics.uci.edu/ml/datasets/Adult
'''

# Partition-based Apriori Algorithm
from collections import defaultdict
import Project1_Apriori_Chunlei_Zhou
import time

def partition_dataset(D, N, length_for_each_partition):
    P = []
    i = 1
    while len(P) < N:
        if i != N:
            P.append(D[(i - 1) * length_for_each_partition:i * length_for_each_partition])
        else:
            P.append(D[(i - 1) * length_for_each_partition:])
        i += 1
    return P

def Phase_I(P, min_sup):
    L = []
    total_transaction = 0
    for i in range(len(P)):
        total_transaction += len(P[i])
    for partition in P:
        local_min_sup = min_sup * len(partition)/total_transaction
        Lp = Project1_Apriori_Chunlei_Zhou.apriori(partition, local_min_sup)
        L.append(Lp)
    print("The executing time of Phase I is %s seconds." % (time.time() - start_time))
    return L

def Merge_Phase(L, P):
    CkG = []
    k = 0
    for j in range(len(P)):
        while k < len(L[j]) and len(L[j][k]) != 0:
            CkG.append(L[j][k])
            k += 1
    print("The executing time of merge phase is %s seconds." % (time.time() - start_time))
    return CkG

def Phase_II(P, CkG):
    measure_support = defaultdict(int)
    for partition in P:
        for ck in CkG:
            for freq_k_itemset in ck:
                for transactions in partition:
                    if freq_k_itemset.issubset(set(transactions)):
                        measure_support[str(freq_k_itemset)] += 1
    print("The executing time of Phase II is %s seconds." % (time.time() - start_time))
    return measure_support.items()

if __name__ == "__main__":
    start_time = time.time()
    with open('/Users/zhouchunlei/Desktop/DATA SCIENCE/Data Mining/Homework/Project Apriori/adult.data.csv') as file:
        ds = file.readlines()
    transactions = []
    for i in range(len(ds)):
        transactions.append(ds[i].strip().split(','))
    length_for_each_partition = 31
    N = int(len(transactions)/length_for_each_partition)+1
    P = partition_dataset(transactions, N, length_for_each_partition)
    min_sup = len(transactions)*0.5
    phase_1 = Phase_I(P, min_sup)
    all_candidate = Merge_Phase(phase_1, P)
    phase_2 = Phase_II(P, all_candidate)
    Frequent_Itemset_List = []
    for itemset, support in phase_2:
        if support >= min_sup and itemset not in Frequent_Itemset_List:
            Frequent_Itemset_List.append(itemset)
    print("The executing time of the improved Apriori algorithm is %s seconds." % (time.time() - start_time))
    print('Frequent Itemset List:')
    print(Frequent_Itemset_List)
    # print(len(Frequent_Itemset_List))

'''
The size of page in main memory is 4096 bytes.
The size of the dataset it 4.3MB.
Thus, the number of partitions should be at least 1043 and the partition size should be at most 31
It seems that the partition size is small enough, so phase I speed up significantly.
However, phase II is not fast enough, which makes the performance of the algorithm... not good...
It should because tooooo many for loops are used and the whole dataset is huge.
'''