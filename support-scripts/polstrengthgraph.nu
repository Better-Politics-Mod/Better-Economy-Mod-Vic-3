#! /usr/bin/env nix-shell
#! nix-shell -i nu -p nushell perl --pure

use pdxParser.nu *

open `~/.local/share/Steam/steamapps/common/Victoria 3/game/common/buy_packages/00_buy_packages.txt` | from pdxscript | each {$in.value.value.0} | prepend 0
| series line -s 3 | chart 2d | draw svg | save -f out.svg