def bubble(list):
    swaped = False
    length = len(list)
    for i, val in enumerate(list):
        if (i+1 < length):
            if (list[i+1] < val):
                list[i] = list[i+1]
                list[i+1] = val
                swaped = True
    if not swaped:
        return list
    else:
        return bubble(list)



print(bubble([3,9,4,5,1,8,5,2]))