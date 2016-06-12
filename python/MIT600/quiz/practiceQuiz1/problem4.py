def addVectors(v1, v2):
    """ assumes v1 and v2 are lists of ints. Returns a list containing
    the pointwise sum of the elements in v1 and v2. E.g.,
    addVectors([4,5], [1,2,3]) returns [5,7,3],and
    addVectors([], []) returns []. Does not modify inputs."""

    sumlist = []
    shortList = None
    longList = None

    if len(v1) >= len(v2):
        shortList = v1
        longList = v2
    else:
        longList = v2
        shortList = v1

    for val in longList:
        if longList.index(val) >= len(shortList):
            sumlist.append(val)
        else:
            sumlist.append(val + shortList[longList.index(val)])

    return sumlist

print addVectors([4,5], [1,2,3])
print addVectors([], [])