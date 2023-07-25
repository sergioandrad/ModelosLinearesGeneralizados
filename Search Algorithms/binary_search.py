# -*- coding: utf-8 -*-
"""
Binary Search
Algorithms 4th edition - Robert Sedgewick and Kevin Wayne
Created on Fri Jun 23 10:07:24 2023
@author: sergio andrade
"""

# Argument vector
vec = [1,2,3,9,12,20,31]
# Element we'd like to check if it belongs to vec and 
# retrieve its index 
x   = vec[5]

# Function that bisects the vector until we find x, otherwise
# it returns -1 to signal that x do not belong to the vector
def rank(x, vec):
    lo_b = 0
    hi_b = len(vec)-1
    while lo_b <= hi_b:
        mid = int(lo_b + (hi_b - lo_b)/2)
        if x < vec[mid]: 
            hi_b = mid-1
        elif x > vec[mid]:
            lo_b=mid+1
        else:
            return mid
    return -1

# Testing the binary search function
x = 3
resp = rank(x=x,vec=vec)
x == vec[resp]

# This way is faster, efficiently computes floor using mod operator
def search(self, nums: List[int], target: int) -> int:
    import math
    L = 0
    R = len(nums) - 1
    while (L <= R):
        LR = L + R
        m = (LR - (LR % 2)) // 2
        if nums[m] < target:
            L = m + 1
        elif nums[m] > target:
            R = m - 1
        else:
            return (m)
    return (-1)