def generate_bem_buildagen_subp(building, max_n=2000, output_file=None):
    lines = []
    lines.append("bem_buildagen_subp = {")
    lines.append(f"    if = {{ limit = {{ scope:target_building = bt:building_{building} }}")

    for n in range(1, max_n + 1):
        lines.append("    bem_buildagen_subsubp = {")
        lines.append(f"        n = {n}")
        lines.append(f"        building = {building}")
        lines.append("    }")

    lines.append("}")
    lines.append("}")

    output = "\n".join(lines)

    if output_file:
        with open(output_file, "w") as f:
            f.write(output)
    else:
        print(output)

# Example usage
generate_bem_buildagen_subp("$building$", 2000, "builda-builda-gen.txt")