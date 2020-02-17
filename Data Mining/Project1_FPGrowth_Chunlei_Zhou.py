'''
Programming Project: Frequent Itemset Mining Algorithms
Author: Chunlei Zhou
2019/10/11 version 1.0
Due: 2019/10/17
Algorithms used:
	(2) FP-growth [HPY00]
Use the UCI Adult Census Dataset
http://archive.ics.uci.edu/ml/datasets/Adult
The algorithm contains two parts.
    First part: build the tree;
    Second part: mine through the tree.
'''

# FP-growth [HPY00]
import operator
import time

class Conditional_FP_Tree:
    def __init__(self, item, support, parentNode):
        self.name = item
        self.count = support
        self.nodeLink = None
        self.parent = parentNode
        self.children = {}

    def support_count(self, support):
        self.count += support
'''
    def display(self, depth = 1):
        print(' '*depth, self.name, ' ', self.count)
        for child in self.children.values():
            child.display(depth + 1)
'''

'''
def find_frequent_items(D, min_sup:int):
    item_count = defaultdict(int)
    for row in D:
        for item in row:
            item_count[item] += 1
    items = dict()
    for item, count in item_count.items():
        if count >= min_sup:
            items.update({item: count})
    F = sorted(items.items(), key=operator.itemgetter(1), reverse=True)
    return F
'''

def create_FP_Tree(D, min_sup:int):
    # To create FP Tree, create headertable first.
    headertable = {}
    for row in D:
        for item in row:
            headertable[item] = headertable.get(item, 0) + D[row]
    # The headertable include item name and support count.
    remove = []
    for key in headertable.keys():
        if headertable[key] < min_sup:
            remove.append(key)
    for item in remove:
        headertable.pop(item)
    # If item is not frequent, then remove from headertable.
    ### frequent_itemset = find_frequent_items(D, min_sup)
    frequent_itemset = sorted(headertable.items(), key=operator.itemgetter(1), reverse=True)
    if len(frequent_itemset) != 0:
        for item in headertable:
            headertable[item] = [headertable[item], None]
            # create node link.
            # Now headertable contains itemname, support count, and node link for each item.
        FPTree = Conditional_FP_Tree('Null', 1, None)
        # root node: item = Null, support = 1, Parent = None
        for transaction, support in D.items():
            treegrowth = {}
            for item in transaction:
                if item in set(headertable.keys()):
                    treegrowth[item] = headertable[item][0]
                    # only need frequency here
                    # treegrowth contains all unique frequent items in each transaction in turn
            if len(treegrowth) > 0:
                sortgrowth = sorted(treegrowth.items(), key=operator.itemgetter(1), reverse=True)
                # To build the tree, sort by support count. Highest comes first.
                nextitemset = []
                for item in sortgrowth:
                    nextitemset.append(item[0])
                # THIS IS SOMETHING NEW TO ME: orderedItems = [v[0] for v in st]
                # MORE EFFICIENT CODING THIS WAY.
                updateFPTree(nextitemset, FPTree, headertable, support)
                # Update tree for each transaction in turn to construct the whole tree.
        return FPTree, headertable
    else:
        return None, None

'''
============ Insert Tree =============
'''

def updateFPTree(itemset, tobeupdateTree, headertable, support):
    # Update tree for item in the chosen itemset one by one.
    # Finish by recursion
    if itemset[0] in tobeupdateTree.children:
        # If the item is already one child of the current tree, then update the support count.
        tobeupdateTree.children[itemset[0]].support_count(support)
    else:
        # Call to build a new branch of the conditional tree and update the headertable.
        tobeupdateTree.children[itemset[0]] = Conditional_FP_Tree(itemset[0], support, tobeupdateTree)
        if headertable[itemset[0]][1] == None:
            # If there is no nodeLink, then create one.
            headertable[itemset[0]][1] = tobeupdateTree.children[itemset[0]]
        else:
            # If there is a nodeLink, then update it.
            updatenodeLink(headertable[itemset[0]][1], tobeupdateTree.children[itemset[0]])
    # Recursion
    if len(itemset) > 1:
        new_itemset = itemset[1::]
        updateFPTree(new_itemset, tobeupdateTree.children[itemset[0]], headertable, support)

def updatenodeLink(new_node, target_node):
    # Based on Figure 6.7, all linked nodes are as nodes in linked list.
    # When there is a nodeLink, then find the end node and add the new link.
    while new_node.nodeLink != None:
        new_node = new_node.nodeLink
    new_node.nodeLink = target_node

'''
============== Top-down finished. Now starts bottom-up. ==============
'''

def bottom_up_Tree(leafnode, path):
    # Recursion again
    if leafnode.parent != None:
        # We have not reached the root node yet.
        path.append(leafnode.name) # grow up
        bottom_up_Tree(leafnode.parent, path)
    return path

def find_path(node):
    # Recursion again...
    conditional_pattern_base = {}
    while node != None:
        path = []
        updatepath = bottom_up_Tree(node, path)
        if len(updatepath) > 1:
            conditional_pattern_base[frozenset(updatepath[1:])] = node.count
            # node is the leafnode for this certain path,
            # so its support count should be the minimum support count of all nodes in the path
            # Use the minimun support count of all nodes in the path as the support count of the path.
            # conditional pattern base stored all paths and their support counts.
        node = node.nodeLink
        # Start from the bottom and move to the parent to find all paths.
    return conditional_pattern_base

'''
============== Mine Frequent Patterns ==============
'''

def FP_Growth(headertable, prefix_path, min_sup:int, frequent_itemset):
    # beta = [alpha[0] for alpha in sorted(headertable.items(), key=operator.itemgetter(1))] why bug?
    beta = [alpha[0] for alpha in sorted(headertable.items(), key=lambda p: p[1][0])]
    # Start from the bottom frequent_1_itemset: leafnode
    # Use each one of the frequent_1_item as the leafnode, updating path with len(path) increase by 1 each time.
    for base_node in beta:
        newfreqitemset = prefix_path.copy()
        newfreqitemset.add(base_node)
        frequent_itemset.append((newfreqitemset, headertable[base_node][0]))
        conditional_pattern_base = find_path(headertable[base_node][1])
        conditional_FPTree, conditional_headertable = create_FP_Tree(conditional_pattern_base, min_sup)
        if conditional_headertable != None:
            # As long as there is frequent itemset, recurse.
            FP_Growth(conditional_headertable, newfreqitemset, min_sup, frequent_itemset)
    return frequent_itemset

######################################################################################################################

if __name__ == "__main__":
    start_time = time.time()
    with open('/Users/zhouchunlei/Desktop/DATA SCIENCE/Data Mining/Homework/Project Apriori/adult.data.csv') as file:
        ds = file.readlines()
    transactions = []
    for i in range(len(ds)):
        transactions.append(ds[i].strip().split(','))
    Dataset = {}
    for transaction in transactions:
        Dataset[frozenset(transaction)] = 1
    Frequent_Itemset = []
    Prefix_Path = set([])
    min_sup = len(transactions) * 0.5
    FPTree, HeaderTable = create_FP_Tree(Dataset, min_sup)
    FrequentItemset = FP_Growth(HeaderTable, Prefix_Path, min_sup, Frequent_Itemset)
    print(FrequentItemset)
    # print(len(FrequentItemset))
    print("The executing time of the FP-Growth algorithm is %s seconds." % (time.time() - start_time))