def get_height_inc(tower, index, changes, left):
    if index == len(tower):
        return changes
    if index == 0:
        return min(
            get_height_inc(tower, index + 1, changes, True), 
            get_height_inc(tower, index + 1, changes, False)
        )
    curr_height = tower[index]
    prev_height = tower[index - 1]
    diff = curr_height - prev_height + 1

    if left:
        changes += diff
    else:
        changes += (diff * index)
    min_changes_children = min(
        get_height_inc(tower, index + 1, changes, True), 
        get_height_inc(tower, index + 1, changes, False)
    )
    return min(changes, min_changes_children)


def sum_height_changes(tower):
    return get_height_inc(tower, 0, 0, True)

print(sum_height_changes([1, 4, 3, 2, 1]))
# print(sum_height_changes([2, 5, 6, 9, 8]))


# [1, 4, 3, 2, 1]
# [5, 4, 3, 2, 1] -> 4


# [2, 5, 6, 9, 8]
# [4, 5, 6, 7, 8] -> 4
