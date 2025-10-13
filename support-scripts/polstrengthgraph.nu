#! /usr/bin/env nix-shell
#! nix-shell -i nu -p nushell perl --pure

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
            $r.value | into float | { key: $r.key, value: $in }
        } else {
            $r
        }
    }
}

open `~/.local/share/Steam/steamapps/common/Victoria 3/game/common/buy_packages/00_buy_packages.txt` | from pdxscript | each {$in.value.value.0} | prepend 0
| series line -s 3 | chart 2d | draw svg | save -f out.svg