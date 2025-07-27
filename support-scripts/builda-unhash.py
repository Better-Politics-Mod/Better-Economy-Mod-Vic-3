# read through every file and find `building_(.*?) =`
import os
import re

path = os.environ['HOME'] + "/.local/share/Steam/steamapps/common/Victoria 3/game/common/buildings"


def find_buildings(path):
    buildings = []
    for root, dirs, files in os.walk(path):
        for file in files:
            if file.endswith(".txt"):
                with open(os.path.join(root, file), 'r', encoding='utf-8') as f:
                    content = f.read()
                    matches = re.findall(r'building_(\w+)', content)
                    buildings.extend(matches)
    return set(buildings)

# go through the list
# and write
# if = { limit = { scope:target_building = { is_building_type = building_(.*?) } } 
#  create_building = {
#    building = building_(.*?)
#    level = scope:level
#  }
# }

def write_building_script(buildings):
    with open('builda-unhash.txt', 'w', encoding='utf-8') as f:
        for i, building in enumerate(sorted(buildings)):
            f.write(f"""
    if = {{ limit = {{ scope:bkey = {i} }}
        bt:building_{building} = {{
            save_scope_as = building_tp
        }}
    }}
    """)


if __name__ == "__main__":
    buildings = find_buildings(path)
    write_building_script(buildings)
    print(f"Generated script for {len(buildings)} buildings.")