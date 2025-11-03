#! /usr/bin/env nix-shell
#! nix-shell -i nu -p nushell perl --pure

use pdxParser.nu *

def tap [out] {
  let input = $in
  print $out
  $input
}

let state_regions: list = ls `~/.local/share/Steam/steamapps/common/Victoria 3/game/map_data/state_regions` | get name | each {path expand} | where {|path| ($path | path basename) != '99_seas.txt'} | reduce -f [] {|path| append (open $path | from pdxscript | get key)}
let sr_splown = $state_regions | each {|it| [$"($it)_findist_levels" $"($it)_manor_levels"] } | flatten
#let tags = open `~/.local/share/Steam/steamapps/common/Victoria 3/game/common/country_definitions/00_countries.txt`
#| parse -r '^\s*(\w+)\s*=\s*(\w+)\s*$' | get capture1 | zip ($in | each {$'"($in)"'}) | uniq | reduce -f (open `~/.local/share/Steam/steamapps/common/Victoria 3/game/common/country_definitions/00_countries.txt`) {|it| str replace -a $' ($it.0)' $' ($it.1)'}
#| lines | where {|it| $it !~ 'color'} | str join "\n" | from pdxscript | where {|r| $r.value.value | any {$in == '"recognized"'} } | get key
let buildings: list = ls `~/.local/share/Steam/steamapps/common/Victoria 3/game/common/buildings` | get name | each {path expand} | where {str ends-with '.txt'} | where {|path| ($path | path basename) != '08_monuments.txt'} | reduce -f [] {|path| append (open $path | from pdxscript | get key)}

let entrypoint = $buildings | reduce -f 'bem_neobuildagen = {' {|building| $in + $"\n\tbem_neobuildagen_subp_depth0 = { building = ($building) }"} | $in + "\n}"
let depth0 = 1..2000 | reduce -f "bem_neobuildagen_subp_depth0 = {\n\tif = { limit = { scope:target_building = bt:building_$building$ }" {|it| $in + $"\n\t\tbem_neobuildagen_subp_depth1 = {\n\t\t\t\tSTATE_SVEALAND_findist_levels = ($it)\n\t\t\t\tbuilding = $building$\n\t\t}"} | $in + "\n\t}\n}"
#let create_building_block = $state_regions | reduce -f "create_building={\n\tbuilding=$building$\n\tadd_ownership={" {|state_region| $in + $"\n\t\tbuilding={\n\t\t\ttype=\"building_financial_district\"\n\t\t\tcountry=scope:($state_region)_largest_owner\n\t\t\tlevels=$($state_region)_findist_levels$\n\t\t\tregion=\"($state_region)\"\n\t\t}\n\t\tbuilding={\n\t\t\ttype=\"building_manor_house\"\n\t\t\tcountry=scope:($state_region)_largest_owner\n\t\t\tlevels=$($state_region)_manor_levels$\n\t\t\tregion=\"($state_region)\"\n\t\t}"} | $in + "\n\t}\n}"
$sr_splown | skip 1 | par-each {|sr| 
let idx = $sr_splown | enumerate | where item == ($sr) | get index | get 0
1..2000 | reduce -f $"bem_neobuildagen_subp_depth($idx) = {\n\tif = { limit = { ($sr_splown | get ($idx - 2)) = $($sr_splown | get ($idx - 2))$ }" {|it| $in + $"\n\t\tbem_neobuildagen_subp_depth($idx + 1) = {\n\t\t\t\t($sr) = ($it)\n\t\t\t\tbuilding = $building$\n\t\t\t\t($sr_splown | slice ..($idx - 1) | each {|i| $'($i) = $($i)$'} | str join "\n\t\t\t\t")\n\t\t}"} | $in + "\n\t}\n}\n" | tap $idx | str replace -a (char --unicode FEFF) '' | save -a ./test3.txt} 