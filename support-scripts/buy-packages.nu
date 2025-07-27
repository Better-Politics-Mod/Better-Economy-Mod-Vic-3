#!/usr/bin/env nu
def polstrength [wealth] { 
    if $wealth <= 10 {
        $wealth * 0.03
    } else if $wealth <= 25 {
        ($wealth - 10) ** 1 * 0.48 + 0.3
    } else if $wealth <= 45 {
        ($wealth - 15) ** 1.5 + 8
    } else { 
        ($wealth - 45) ** 2 * 2 + 70
    }
}

def total_expenditures [wealth] {
    1.085 ** (50 + 1.25 * $wealth) + 75
} 

def weight_basic_food [wealth] {
    if $wealth > 0 and $wealth <= 18 {
        5 * $wealth + 90
    } else if $wealth > 18 and $wealth <= 25 {
        180
    } else if $wealth > 24 and $wealth <= 29 {
        -5 * $wealth + 305
    } else {
        0
    }
}

def weight_crude_items [wealth] {
    if $wealth > 4 and $wealth <= 14 {
        -1.5 * (($wealth - 9) ** 2) + 50
    } else {
        0
    }
}

def weight_simple_clothing [wealth] {
    if ($wealth > 0 and $wealth <= 14) {
        -0.33 * (($wealth - 8) ** 2) + 50
    } else {
        0
    }
}

def popneed_heating [wealth] {
    if ($wealth > 0 and $wealth < 9) {
        $wealth + 17
    } else if ($wealth >= 9) {
        26
    } else {
        0
    }
}

def weight_household_items [wealth] {
    if ($wealth > 10 and $wealth < 30) {
        12 * $wealth - 113
    } else if ($wealth >= 30 and $wealth < 45) {
        247
    } else {
        0
    }
}

def weight_standard_clothing [wealth] {
    if ($wealth > 10 and $wealth < 24) {
        12 * $wealth - 113
    } else if ($wealth >= 24 and $wealth < 39) {
        175
    } else {
        0
    }
}

def weight_services [wealth] {
    if ($wealth > 10 and $wealth < 30) {
        12 * $wealth - 113
    } else if ($wealth >= 30 and $wealth < 45) {
        247
    } else {
        0
    }
}

def popneed_intoxicants [wealth] {
    if ($wealth > 0 and $wealth < 30) {
        7 * $wealth
    } else if ($wealth >= 30) {
        210
    } else {
        0
    }
}

def weight_luxury_drinks [wealth] {
    if ($wealth >= 15) {
        (1.145 ** $wealth) + 20 / 2
    } else {
        0
    }
}

def weight_free_movement [wealth] {
    if ($wealth >= 10) {
        1.145 ** $wealth / 4
    } else {
        0
    }
}

def weight_communication [wealth] {
    if ($wealth >= 20) {
        (1.145 ** $wealth) / 3 + 20
    } else {
        0
    }
}

def weight_luxury_food [wealth] {
    if ($wealth >= 20) {
        (1.145 ** $wealth) + 20 / 2
    } else {
        0
    }
}

def weight_luxury_items [wealth] {
    if ($wealth >= 15) {
        (1.165 ** $wealth) + 20 / 3
    } else {
        0
    }
}

def weight_leisure [wealth] {
    if ($wealth >= 20) {
        (1.165 ** $wealth) + 20 / 3
    } else {
        0
    }
}

def weight_financial_services [wealth] {
    if ($wealth >= 30) {
        (1.25 ** ($wealth - 20)) + 50
    } else {
        0
    }
}

def weight_sum [wealth] {(
    (weight_basic_food $wealth) + 
    (weight_crude_items $wealth) +
    (weight_simple_clothing $wealth) +
    #(weight_heating $wealth) +
    (weight_household_items $wealth) +
    (weight_standard_clothing $wealth) +
    (weight_services $wealth) +
    #(weight_intoxicants $wealth) +
    (weight_luxury_drinks $wealth) +
    (weight_free_movement $wealth) +
    (weight_communication $wealth) +
    (weight_luxury_food $wealth) +
    (weight_luxury_items $wealth) +
    (weight_leisure $wealth) +
    (weight_financial_services $wealth)
)}

def real_expenditures [weight wealth] {
    ($weight / (weight_sum $wealth)) * (total_expenditures $wealth) | math round
}

def buy_package [weight wealth] {
    $"(real_expenditures $weight $wealth) # (((real_expenditures $weight $wealth) / (total_expenditures $wealth)) * 100 | math round -p 2)%"
}

def goods [wealth] {
    {
    popneed_basic_food: (buy_package (weight_basic_food $wealth) $wealth)
    popneed_crude_items: (buy_package (weight_crude_items $wealth) $wealth)
    popneed_simple_clothing: (buy_package (weight_simple_clothing $wealth) $wealth)
    popneed_heating: $"(popneed_heating $wealth) # unweighted"
    popneed_household_items: (buy_package (weight_household_items $wealth) $wealth)
    popneed_standard_clothing: (buy_package (weight_standard_clothing $wealth) $wealth)
    popneed_services: (buy_package (weight_services $wealth) $wealth)
    popneed_intoxicants: $"(popneed_intoxicants $wealth) # unweighted"
    popneed_luxury_drinks: (buy_package (weight_luxury_drinks $wealth) $wealth)
    popneed_free_movement: (buy_package (weight_free_movement $wealth) $wealth)
    popneed_communication: (buy_package (weight_communication $wealth) $wealth)
    popneed_luxury_food: (buy_package (weight_luxury_food $wealth) $wealth)
    popneed_luxury_items: (buy_package (weight_luxury_items $wealth) $wealth)
    popneed_leisure: (buy_package (weight_leisure $wealth) $wealth)
    popneed_financial_services: (buy_package (weight_financial_services $wealth) $wealth)
    }
}

def 'to pdxscript' []: record -> string {
  to json -t 1 
| str replace -a "\"" "" 
| str replace -a "," "" 
| str replace -a ":" " =" 
| str substring 2..-2 
| lines 
| each {|| str substring 1..} 
| str join "\n"
}

1..99 | reduce --fold {} {|i| insert $"wealth_($i)" {political_strength: (polstrength $i), goods: (goods $i)}}
| to pdxscript | save -f "../better-economy-mod/common/buy_packages/zz_bem_buy_packages.txt"
