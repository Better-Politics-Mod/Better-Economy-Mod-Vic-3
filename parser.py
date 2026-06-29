
###################
# PARADOX PARSER  #
###################
class ParadoxParser:
    """Recursive parser for Paradox Interactive game script files."""
    
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.length = len(text)
    
    def parse(self):
        """Parse the entire document and return the tree structure."""
        return self.parse_object()
    
    def current_char(self):
        """Get current character without advancing position."""
        if self.pos < self.length:
            return self.text[self.pos]
        return None
    
    def peek_char(self, offset=1):
        """Peek ahead at character."""
        if self.pos + offset < self.length:
            return self.text[self.pos + offset]
        return None
    
    def advance(self):
        """Move to next character."""
        self.pos += 1
    
    def skip_whitespace(self):
        """Skip whitespace and comments."""
        while self.pos < self.length:
            char = self.current_char()
            
            # Skip whitespace
            if char in ' \t\n\r':
                self.advance()
            # Skip line comments
            elif char == '#':
                while self.current_char() and self.current_char() != '\n':
                    self.advance()
            else:
                break
    
    def parse_string(self):
        """Parse a quoted string."""
        quote_char = self.current_char()
        self.advance()  # Skip opening quote
        
        result = []
        while self.current_char() and self.current_char() != quote_char:
            if self.current_char() == '\\':
                self.advance()
                if self.current_char():
                    result.append(self.current_char())
                    self.advance()
            else:
                result.append(self.current_char())
                self.advance()
        
        if self.current_char() == quote_char:
            self.advance()  # Skip closing quote
        
        return ''.join(result)
    
    def parse_identifier(self):
        """Parse an unquoted identifier or value."""
        result = []
        
        while self.current_char():
            char = self.current_char()
            
            # Stop at structural characters or whitespace
            if char in '{}=#\n\r\t ':
                break
            
            result.append(char)
            self.advance()
        
        return ''.join(result)
    
    def parse_comparison_or_value(self):
        """Parse a comparison expression (a > b) or a simple value."""
        # Save position in case we need to backtrack
        start_pos = self.pos
        
        # Parse first part
        first = self.parse_identifier()
        
        self.skip_whitespace()
        
        # Check if followed by comparison operator
        char = self.current_char()
        if char in '<>':
            operator = char
            self.advance()
            
            # Check for compound operators (<=, >=, etc.)
            if self.current_char() == '=':
                operator += self.current_char()
                self.advance()
            
            self.skip_whitespace()
            
            # Parse right side
            second = self.parse_identifier()
            
            # Return as a comparison expression string
            return f"{first}{operator}{second}"
        
        # Not a comparison, just return the first part
        return first
    
    def parse_value(self):
        """Parse a single value (string, number, identifier, or object)."""
        self.skip_whitespace()
        
        char = self.current_char()
        
        if char is None:
            return None
        
        # Quoted string
        if char in '"\'':
            return self.parse_string()
        
        # Nested object
        if char == '{':
            return self.parse_object()
        
        # Unquoted identifier, number, or comparison expression
        return self.parse_comparison_or_value()
    
    def parse_object(self):
        """Parse an object (dictionary or list)."""
        self.skip_whitespace()
        
        items = []  # Track all items to detect structure
        
        # Handle opening brace if present
        if self.current_char() == '{':
            self.advance()
        
        while self.current_char():
            self.skip_whitespace()
            
            # Check for closing brace
            if self.current_char() == '}':
                self.advance()
                break
            
            # End of input
            if self.current_char() is None:
                break
            
            # Parse key (might be a comparison expression)
            key = self.parse_comparison_or_value()
            
            if key is None:
                break
            
            self.skip_whitespace()
            
            # Check for assignment operator (only =)
            char = self.current_char()
            
            if char == '=':
                operator = char
                self.advance()
                
                # Check for compound operators (==)
                if self.current_char() == '=':
                    operator += self.current_char()
                    self.advance()
                
                self.skip_whitespace()
                
                # Parse the value after the operator
                value = self.parse_value()
                
                # Store as key-value pair
                items.append({'key': key, 'value': value, 'has_operator': True})
            
            else:
                # No operator - just a standalone key (or comparison expression)
                items.append({'key': key, 'value': True, 'has_operator': False})
        
        # Now decide structure based on items
        if not items:
            return {}
        
        # Check if all items have operators (key=value pairs)
        all_have_operators = all(item['has_operator'] for item in items)
        none_have_operators = all(not item['has_operator'] for item in items)
        
        # Get all unique keys
        keys = [item['key'] for item in items]
        unique_keys = set(keys)
        has_duplicates = len(keys) != len(unique_keys)
        
        if all_have_operators and not has_duplicates:
            # All key=value pairs with unique keys -> simple dict
            result = {}
            for item in items:
                result[item['key']] = item['value']
            return result
        
        elif all_have_operators and has_duplicates:
            # Multiple key=value pairs, some keys repeated -> list of dicts
            result = []
            for item in items:
                result.append({item['key']: item['value']})
            return result
        
        elif none_have_operators:
            # All standalone keys -> dict with True values
            result = {}
            for item in items:
                if item['key'] in result:
                    # Convert to list for duplicates
                    if not isinstance(result[item['key']], list):
                        result[item['key']] = [result[item['key']]]
                    result[item['key']].append(True)
                else:
                    result[item['key']] = True
            return result
        
        else:
            # Mixed: some with operators, some without
            # Keep as dict but handle duplicates
            result = {}
            for item in items:
                if item['key'] in result:
                    # Convert to list if duplicate key
                    if not isinstance(result[item['key']], list):
                        result[item['key']] = [result[item['key']]]
                    result[item['key']].append(item['value'])
                else:
                    result[item['key']] = item['value']
            return result


with open('toanalyze.v3', 'r') as file:
    text = file.read()
    res = ParadoxParser(text).parse()

import json
with open('output.json', 'w') as file:
    json.dump(res, file, indent=4)