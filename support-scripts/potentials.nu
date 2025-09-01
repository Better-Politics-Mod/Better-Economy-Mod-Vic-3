#! /usr/bin/env nix-shell
#! nix-shell -i nu -p nushell perl --pure

#/(?<key>[^=\s]+)\s*=\s*(?<value>\{(?:[^{}]+|(?&value))*\})\s*/g
#/(?<key>[^=\s]+)\s*=\s*(?<value>"(?:[^"\\]|\\.)*"|[+-]?\d+|\{(?:[^{}"]+|"(?:[^"\\]|\\.)*"|(?&value))*\})\s*/g
#/(?<key>[^=\s]+)\s*=\s*(?<value>\{(?:"(?:[^"\\]|\\.)*"|[^{}"]+|(?&value))*\}|"(?:[^"\\]|\\.)*"|[+-]?\d+)\s*/g
    
def 'from pdxscript' []: string -> table {    
    lines
    | each { split row '#' | get 0 }
    | str join "\n" 
    | str replace -ar '\s+' '' 
    | ^perl -e 'local $/; print join "\n", (<STDIN> =~ /(?<key>[^=\s]+)\s*=\s*(?<value>\{(?:"(?:[^"\\]|\\.)*"|[^{}"]+|(?&value))*\}|"(?:[^"\\]|\\.)*"|[+-]?(?:\d+(?:\.\d*)?|\.\d+))\s*/g);'
    | split row "\n"
    | chunks 2 
    | each {|i| { key: ($i | get 0 | default null), value: ($i | get 1 | default null) } }
    | each {|r|
        if $r.value =~ '(?s)^\s*\{\s*.*?=\s*.*\}\s*$' {
            $r.value | str substring 1..-1 | from pdxscript | { key: $r.key, value: $in } #| each { |row| $row | values | {$in.0: $in.1}} | reduce {|it| merge $it } | { key: $r.key, value: $in }
        } else if $r.value =~ '(?s)^\s*\{\s*[^=]*\}\s*$' {
            $r.value | split row '"' | where {|it| $it != "" } | slice 1..-2 | each {|| $'"($in)"'} | { key: $r.key, value: $in }
        } else if $r.value =~ '^".*?"$' {
            $r #$r.value | str substring 1..-2 | { key: $r.key, value: $in }
        } else if $r.value =~ '-?\d+' {
            $r.value | into int | { key: $r.key, value: $in }
        } else {
            $r
        }
    }
}

def 'to pdxscript' []: table -> string {
    each {|r|
        if ($r.value | describe) == 'list<string>' {
            $r | $"($in.key) = {\n($in.value | each {$"\t($in)"} | str join "\n")\n}"
        } else if ($r.value | describe) == 'string' or ($r.value | describe) == 'int' {
            $r | $"($in.key) = ($in.value)"
        } else if ($r.value | describe | str starts-with table) or ($r.value | describe) == 'list<any>' {
            $r | $"($in.key) = {\n($in.value | to pdxscript | lines | each {$"\t($in)"} | str join "\n")\n}"
        }
    }
    | str join "\n"
}

def make_discoverable [bg discovered_mult undiscovered_mult]: table -> table {
    update cells -c [value] {|c|
    if ($c.key | any {$in == capped_resources}) {
        if ($c | where key == capped_resources | get value.0 | any {$in.key == $bg}) {
            let o = ($c | where key == capped_resources | get value.0 | where key == $bg | get value.0)
            $c | append {key: resource, value: [[key value]; [type $'"($bg)"'] [discovered_amount ($o * $discovered_mult)] [undiscovered_amount ($o * $undiscovered_mult)]]}
            | update value {|r| if $r.key == capped_resources {$r.value | where key != $bg} else {$r.value}}
        } else {$c}
    } else {$c}
    | if ($in | where key == capped_resources | get value.0?) == [] {$in | where key != capped_resources} else {$in}
    }
}

def multiply_potentials [bg mult]: table -> table {
    update cells -c [value] {
    each {|r|
        if $r.key == capped_resources {
            {key: $r.key value: ($r.value | each {
                if $in.key == $bg {{key: $in.key, value: ($in.value * $mult)}} else {$in}
            })}
        } else {$r}
    }
}
}

mkdir ../better-economy-mod/map_data/state_regions
ls `~/.local/share/Steam/steamapps/common/Victoria 3/game/map_data/state_regions` | get name | each {path expand}
| each {|path| 
    open $path
    | parse -r '(\{[^{}=]*\})' | get capture0 
    | zip ($in | each {split row -r '\s+' | slice 1..-2 | each {if not ($in | str contains '"') {$'"($in)"'} else {}} | insert 0 '{' | append '}' | str join ' '}) 
    | reduce -f (open $path) {|it| str replace -a $it.0 $it.1} 
    | from pdxscript
    | make_discoverable bg_coal_mining 1 3
    | make_discoverable bg_iron_mining 1 3
    | make_discoverable bg_lead_mining 1 3
    | make_discoverable bg_sulfur_mining 1 3
    | multiply_potentials bg_logging 4
    | to pdxscript
    | save -f ('../better-economy-mod/map_data/state_regions/' + $'($path | path basename)')
    print $'($path | path basename)'
}
