#######################################
# Title: Sorting Problems
# Author: Sergio Andrade
# Description: Implementing sorting
# algorithms such as insertion-sort,
# merge-sort and others.
#######################################

# Insertion sort 
a = [3, 1, 7, 2, 4, 3]
n = len(a)

def insertion_sort(a) :
    for j in range(1,n,1):
        print(j)
        i   = j-1
        key = a[j]
        while (i+1>0 and a[i]>key):
            a[i+1] = a[i]
            i = i-1
        a[i+1] = key
        print(a)
    return a
    
insertion_sort(a)
    
# Merge
import math
p=1
q=math.floor((n+1)/2)
r=n

n_1 = q-p+1
n_2 = r-q

for i in range(1,n_1,1):
    L[i] = a[p+i-1]

for j in range(1,n_2,1):
    L[j] = a[q+j]
   
i =1
j =1
for k in range(p,r,1):
    if L[i] <= R[j]:
        a[k] = L[i]
        i=i+1
    else :
        A[k]=R[j]
        j=j+1











# Merge-sort