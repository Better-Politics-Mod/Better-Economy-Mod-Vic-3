#!/usr/bin/env python3
"""
Party Approval Rating Calculator

This script parses regional faction data and calculates weighted party approval ratings.
Each region contains factions with:
- Strength values
- Percentage support within the region
- Influence values for different parties (USP, PFJP, NFP, CPS)

The approval rating for each party is calculated by:
1. Weighting each faction's party influence by its percentage support in the region
2. Weighting each region's contribution by its percentage of total population
"""

import re
from typing import Dict, List, Tuple


def parse_input(text: str) -> Dict:
    """
    Parse the input text into a structured format.
    
    Returns:
        Dict containing regions with their factions and influence values
    """
    data = {}
    current_region = None
    current_faction = None
    
    lines = text.strip().split('\n')
    
    for line in lines:
        line = line.strip()
        if not line:
            continue
            
        # Region header: ## Region Name (City) [percentage%]
        region_match = re.match(r'^##\s+(.+?)\s+\((.+?)\)\s*$', line)
        if region_match:
            region_name = region_match.group(1).strip()
            city = region_match.group(2).strip()
            current_region = region_name
            data[current_region] = {
                'city': city,
                'percentage': None,
                'factions': {}
            }
            continue
        
        # Region percentage: [X%]
        percentage_match = re.match(r'^\[(\d+)%\]$', line)
        if percentage_match and current_region:
            data[current_region]['percentage'] = int(percentage_match.group(1))
            continue
        
        # Faction header: ### Faction Name
        faction_match = re.match(r'^###\s+(.+)$', line)
        if faction_match and current_region:
            faction_name = faction_match.group(1).strip()
            current_faction = faction_name
            data[current_region]['factions'][current_faction] = {
                'strength': None,
                'percentage': None,
                'influence': {}
            }
            continue
        
        # Strength and percentage: Strength X, Y%
        strength_match = re.match(r'^Strength\s+(\d+),\s+(\d+)%$', line)
        if strength_match and current_region and current_faction:
            data[current_region]['factions'][current_faction]['strength'] = int(strength_match.group(1))
            data[current_region]['factions'][current_faction]['percentage'] = int(strength_match.group(2))
            continue
        
        # Influence values: USP X, PFJP Y, NFP Z, CPS W
        influence_match = re.match(r'^USP\s+(\d+),\s+PFJP\s+(\d+),\s+NFP\s+(\d+),\s+CPS\s+(\d+)$', line)
        if influence_match and current_region and current_faction:
            data[current_region]['factions'][current_faction]['influence'] = {
                'USP': int(influence_match.group(1)),
                'PFJP': int(influence_match.group(2)),
                'NFP': int(influence_match.group(3)),
                'CPS': int(influence_match.group(4))
            }
            continue
    
    return data


def calculate_party_approval(data: Dict) -> Dict[str, float]:
    """
    Calculate weighted approval ratings for each party.
    
    Formula:
    For each party:
        approval = sum over all regions (
            region_percentage * sum over all factions in region (
                faction_percentage * party_influence_in_faction
            ) / 100
        ) / 100
    
    Returns:
        Dict mapping party names to their approval ratings
    """
    parties = ['USP', 'PFJP', 'NFP', 'CPS']
    approval_ratings = {party: 0.0 for party in parties}
    
    for region_name, region_data in data.items():
        region_percentage = region_data['percentage']
        
        if region_percentage is None:
            print(f"Warning: Region '{region_name}' has no percentage, skipping")
            continue
        
        # Calculate regional contribution for each party
        for party in parties:
            regional_influence = 0.0
            
            for faction_name, faction_data in region_data['factions'].items():
                faction_percentage = faction_data['percentage']
                party_influence = faction_data['influence'].get(party, 0)
                
                if faction_percentage is not None:
                    # Weight by faction's support within the region
                    regional_influence += (faction_percentage / 100.0) * party_influence
            
            # Weight by region's percentage of total population
            approval_ratings[party] += (region_percentage / 100.0) * regional_influence
    
    return approval_ratings


def print_results(approval_ratings: Dict[str, float]):
    """Print the approval ratings in a formatted way."""
    print("\n" + "="*50)
    print("PARTY APPROVAL RATINGS")
    print("="*50)
    
    # Sort by approval rating (descending)
    sorted_parties = sorted(approval_ratings.items(), key=lambda x: x[1], reverse=True)
    
    for party, rating in sorted_parties:
        print(f"{party:10s}: {rating:6.2f}")
    
    print("="*50 + "\n")


def main():
    """Main function to read input and calculate approval ratings."""
    # Read input from file or stdin
    import sys
    
    if len(sys.argv) > 1:
        # Read from file
        with open(sys.argv[1], 'r') as f:
            input_text = f.read()
    else:
        # Read from stdin
        print("Enter the regional data (press Ctrl+D when done):")
        input_text = sys.stdin.read()
    
    # Parse and calculate
    data = parse_input(input_text)
    approval_ratings = calculate_party_approval(data)
    print_results(approval_ratings)
    
    return approval_ratings


if __name__ == "__main__":
    main()