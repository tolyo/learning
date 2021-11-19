# https://en.wikipedia.org/wiki/Heap%27s_algorithm

def permutations(k, A):
    if k == 1:
        print(A)
    else:
        for i in range(k):
            permutations(k-1, A)
            if k % 2 == 0:
                swap = A[i]
                A[i] = A[k-1]
                A[k-1] = swap
            else:
                swap = A[0]
                A[0] = A[k-1]
                A[k-1] = swap  


print(permutations(len(a), a))