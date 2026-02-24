import re

def read_goods(filepath):
    with open(filepath, "r") as f:
        return [
            re.search(r"REPLACE:(\w+)", line).group(1)
            for line in f
            if "REPLACE:" in line
        ]

def write_subp_block(f, effect_name, params, goods):
    f.write(f"{effect_name} = {{\n")
    for i, good in enumerate(goods):
        param_str = " ".join(
            f"{k} = {v.format(i=i, good=good)}"
            for k, v in params
        )
        f.write(f"  {effect_name}_subp = {{ {param_str} }}\n")
    f.write("}\n")

def write_matrix_sum_block(f, axis, param_name, goods):
    effect_name = f"bem_calculate_{axis}_sum"
    f.write(f"{effect_name} = {{\n")
    for i, good in enumerate(goods):
        f.write(
            f"  set_variable = {{ name = bem_production_matrix_sum_{axis}_{i} value = 0 }}\n"
            f"  bem_calculate_{axis}_sum_subp = {{ {param_name} = {i} }}\n"
        )
    f.write("}\n")

def write_elem_subp_subp_block(f, prefix, subname, k_var, upper_triangle=False, divide_by_diagonal=False):
    """
    Writes the innermost subp_subp block, iterating over k.
    k_var:            the variable name to compare k against ('$i$' or '$j$')
    upper_triangle:   if True, also guards j iterations to only the upper triangle (i <= j)
    divide_by_diagonal: if True, divides the result by matrix[j][j] after summing
    """
    # set variable to 0

    f.write(f"{prefix}_subp_subp = {{\n")
    f.write(
        f"  set_variable = {{ name = {subname}_$i$_$j$ value = 0 }}\n"
    )
    for k, _ in enumerate(goods):
        f.write(
            f"  if = {{ limit = {{ {k_var} > {k} }}\n"
            f"    {subname}_subp_subp_subp = {{ i = $i$ j = $j$ k = {k} }}\n"
            f"  }}\n"
        )
    if divide_by_diagonal:
        f.write(
            "  change_variable = {\n"
            f"    name = {subname}_$i$_$j$\n"
            "    divide = var:bem_production_matrix_$j$_$j$\n"
            "  }\n"
        )
    f.write("}\n")
def write_forward_sub_subp_subp_block(f, goods):
    """
    Iterates k from 0 to i-1 (k < i): y_i -= L[i][k] * y_k
    Lower triangle only, no division (L[i][i] = 1).
    """
    f.write("bem_forward_sub_subp_subp = {\n")
    for k, _ in enumerate(goods):
        f.write(
            f"  if = {{ limit = {{ $i$ > {k} }}\n"
            f"    bem_forward_sub_subp_subp_subp = {{ i = $i$ k = {k} }}\n"
            f"  }}\n"
        )
    f.write("}\n")

def write_forward_sub_subp_block(f, goods):
    """
    Iterates i from 0 to n-1: initializes y_i = v_i then accumulates k < i terms.
    """
    f.write("bem_forward_sub_subp = {\n")
    for i, _ in enumerate(goods):
        f.write(
            f"  set_variable = {{ name = bem_y_{i} value = var:bem_v_{i} }}\n"
            f"  bem_forward_sub_subp_subp = {{ i = {i} }}\n"
        )
    f.write("}\n")

def write_back_sub_subp_subp_block(f, goods):
    """
    Iterates k from i+1 to n-1 (k > i): x_i -= U[i][k] * x_k
    Upper triangle only.
    """
    f.write("bem_back_sub_subp_subp = {\n")
    for k, _ in enumerate(goods):
        f.write(
            f"  if = {{ limit = {{ $i$ < {k} }}\n"
            f"    bem_back_sub_subp_subp_subp = {{ i = $i$ k = {k} }}\n"
            f"  }}\n"
        )
    f.write("}\n")

def write_back_sub_subp_block(f, goods):
    """
    Iterates i from n-1 down to 0: initializes x_i = y_i, accumulates k > i terms,
    then divides by U[i][i] and stores into result_i.
    Reversed so that x_k values are ready when needed.
    """
    f.write("bem_back_sub_subp = {\n")
    for i, g in reversed(list(enumerate(goods))):
        f.write(
            f"  set_variable = {{ name = bem_x_{i} value = var:bem_y_{i} }}\n"
            f"  bem_back_sub_subp_subp = {{ i = {i} }}\n"
            f"  bem_back_sub_divide_subp = {{ i = {i} gi = {g} }}\n"
        )
    f.write("}\n")
def write_elem_subp_block(f, prefix, goods, upper_triangle=False):
    """
    Writes the subp block iterating over j for fixed i.
    upper_triangle: if True, wraps each j in an i <= j guard.
    """
    f.write(f"{prefix}_subp = {{\n")
    for j, _ in enumerate(goods):
        if upper_triangle:
            f.write(
                f"  if = {{ limit = {{ $i$ <= {j} }}\n"
                f"    {prefix}_subp_subp = {{ i = $i$ j = {j} }}\n"
                f"  }}\n"
            )
        else:
            f.write(f"  {prefix}_subp_subp = {{ i = $i$ j = {j} }}\n")
    f.write("}\n")

def generate_effects(goods, output_path):
    with open(output_path, "w") as f:
        write_subp_block(f, "bem_clear_subp", [("i", "{i}"), ("j", "$j$")], goods)
        write_subp_block(f, "bem_clear", [("j", "{i}")], goods)
        write_subp_block(f, "bem_calculate_consumption_of_good",
            [("i", "{i}"), ("gi", "{good}")], goods)

        write_subp_block(f, "bem_add_to_matrix_if_consumed",
            [("i", "$i$"), ("gi", "$gi$"), ("j", "{i}"), ("gj", "{good}")], goods)


        write_elem_subp_subp_block(f, "bem_calculate_L_elem", "bem_tr_matrix", k_var="$j$",
            divide_by_diagonal=True)
        write_elem_subp_block(f, "bem_calculate_L_elem", goods, upper_triangle=False)
        write_subp_block(f, "bem_calculate_L_elem", [("i", "{i}")], goods)

        write_elem_subp_subp_block(f, "bem_calculate_U_elem", "bem_tr_matrix", k_var="$i$",
            divide_by_diagonal=False)
        write_elem_subp_block(f, "bem_calculate_U_elem", goods, upper_triangle=True)
        write_subp_block(f, "bem_calculate_U_elem", [("i", "{i}")], goods)

        # LU solve: forward then back substitution
        write_forward_sub_subp_subp_block(f, goods)
        write_forward_sub_subp_block(f, goods)
        write_back_sub_subp_subp_block(f, goods)
        write_back_sub_subp_block(f, goods)
goods = read_goods("better-economy-mod/common/goods/bem_00_goods.txt")
generate_effects(goods, "better-economy-mod/common/scripted_effects/bem_calculations_autogen.txt")