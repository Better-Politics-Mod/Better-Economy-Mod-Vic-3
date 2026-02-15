# """

# for i,good in good_list:
#     tot = 0
#     for each building:
#         if good_produced in building:
#             tot+=building[good_i_produced]
#             for j,good in good_list:
#                 if good consumed in building:
#                     m[i,j] += good_consumed

#     for j,good in good_list:
#         m[i,j] = m[i,j]/tot


# """

with open("better-economy-mod/common/goods/bem_00_goods.txt", "r") as f:
    # goods are gonna be REPLACE:{good} = ...
    # we just want the {good} part
    import re
    goods = []
    for line in f.readlines():
        if "REPLACE:" in line:
            good = re.search(r"REPLACE:(\w+)", line).group(1)
            goods.append(good)

with open("out.txt", "w") as f:
    for i, good in enumerate(goods):
        f.write(f"bem_calculate_consumption_of_good_subp = {{ i = {i} gi = {good} }}\n")
    for i, good in enumerate(goods):
        f.write(f"bem_add_to_matrix_if_consumed_subp = {{ i = $i$ gi = $gi$ j = {i} gj = {good} }}\n")