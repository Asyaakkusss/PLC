import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SpartieScanner {
    private String source;

    private int start = 0;
    private int current = 0;
    private int line = 1;

    private static final Map<String, TokenType> keywords = new HashMap<>();
    static {
        keywords.put("if", TokenType.IF);
        keywords.put("else", TokenType.ELSE);
        keywords.put("for", TokenType.FOR);
        keywords.put("while", TokenType.WHILE);
        keywords.put("true", TokenType.TRUE);
        keywords.put("false", TokenType.FALSE);
        keywords.put("fun", TokenType.FUN);
        keywords.put("return", TokenType.RETURN);
        keywords.put("var", TokenType.VAR);
        keywords.put("print", TokenType.PRINT);
        keywords.put("null", TokenType.NULL);
    }

    public SpartieScanner(String source) {
        this.source = source;
    }

    public List<Token> scan() {
        List<Token> tokens = new ArrayList<>();

        Token token = null;
        while (!isAtEnd() && (token = getNextToken()) != null) {
            if (token.type != TokenType.IGNORE) tokens.add(token);
        }

        return tokens;
    }

    private Token getNextToken() {
        Token token = null;

        // Try to get each type of token, starting with a simple token, and getting a little more complex
        token = getSingleCharacterToken();
        if (token == null) token = getComparisonToken();
        if (token == null) token = getDivideOrComment();
        if (token == null) token = getStringToken();
        if (token == null) token = getNumericToken();
        if (token == null) token = getIdentifierOrReservedWord();
        if (token == null) {
            error(line, String.format("Unexpected character '%c' at %d", source.charAt(current), current));
        }

        return token;
    }

    // TODO: Complete implementation
    private Token getSingleCharacterToken() {
        // Hint: Examine the character, if you can get a token, return it, otherwise return null
        // Hint: Be careful with the divide, we have to know if it is a single character

        char nextCharacter = source.charAt(current);
        String text = String.valueOf(nextCharacter); // stores current char as string -- used when declaring new token

        // Hint: Start of not knowing what the token is, if we can determine it, return it, otherwise, return null
        TokenType type = TokenType.UNDEFINED;

            switch (nextCharacter) {
                case '(':
                    type = TokenType.LEFT_PARENT; 
                    break;
                case ')':
                    type = TokenType.RIGHT_PARENT; 
                    break;
                case '{':
                    type = TokenType.LEFT_BRACE; 
                    break;
                case '}':
                    type = TokenType.RIGHT_BRACE; 
                    break;
                case ';':
                    type = TokenType.SEMICOLON; 
                    break;
                case ',':
                    type = TokenType.COMMA; 
                    break;
                case '+':
                    type = TokenType.ADD; 
                    break;
                case '-':
                    type = TokenType.SUBTRACT; 
                    break;
                case '*':
                    type = TokenType.MULTIPLY; 
                    break;
                case '&':
                    type = TokenType.AND; break;
                case '|':
                    type = TokenType.OR; break;
                default: 
                    return null; // other tokens (=, /, ! for example) will be handled elsewhere
                }

        if (type != null)
        {
            current++
            return new Token(type, text, line)
        }

        return null;
    }

    // TODO: test code 
    private Token getComparisonToken() {
        // Hint: Examine the character for a comparison but check the next character (as long as one is available)
        // For example: < or <=
        char nextCharacter = source.charAt(current);

        TokenType tok = null; 

        switch(nextCharacter) {
            case '<':
            {

                if ((current + 1) < source.length() && source.charAt(current + 1) == '=') {
                    tok = TokenType.LESS_EQUAL; 
                    current = current + 2; 
                    return new Token(tok, "<=", line);
                }
                else {
                    tok = TokenType.LESS_THAN;
                    current++;
                    return new Token(tok, "<", line);
                }
            }
            case '>':
            {
                if ((current + 1) < source.length() && source.charAt(current + 1) == '=') {
                    tok = TokenType.GREATER_EQUAL; 
                    current = current + 2; 
                    return new Token(tok, ">=", line);
                }
                else {
                    tok = TokenType.GREATER_THAN;
                    current++;
                    return new Token(tok, "<", line);
                }
            }
            case '=': // ASSIGN vs EQUIVALENT
                if ((current + 1) < source.length() && source.charAt(current + 1) == '=') {
                    current += 2;
                    return new Token(TokenType.EQUIVALENT, "==", line);
                } 
                else {
                    current++;
                    return new Token(TokenType.ASSIGN, "=", line);
                }
            case '!': // NOT_EQUAL vs NOT
                if ((current + 1) < source.length() && source.charAt(current + 1) == '=') {
                    current += 2;
                    return new Token(TokenType.NOT_EQUAL, "!=", line);
                } 
                else {
                    current++;
                    return new Token(TokenType.NOT, "!", line);
                }
        }

        return null;
    }

    // TODO: test code
    private Token getDivideOrComment() {
        // Hint: Examine the character for a comparison but check the next character (as long as one is available)
        char nextCharacter = source.charAt(current);
        TokenType tok = null; 

        if (nextCharacter == '/') {

            if (current + 1 < source.length() && source.charAt(current + 1) == '/') {
                current = current + 2;

                while (current < source.length() && source.charAt(current) != '\n') {
                    current++;
                }
                tok = TokenType.IGNORE; 
                return new Token(tok, "//", line);
            } 
            
            else {
                current++;
                tok = TokenType.DIVIDE; 
                return new Token(tok, "/", line);
            }
        }
        return null;
    }

    // TODO: Complete implementation
    private Token getStringToken() {
        // Hint: Check if you have a double quote, then keep reading until you hit another double quote
        // But, if you do not hit another double quote, you should report an error
        char nextCharacter = source.charAt(current);
        String string = null; 
        int end; 
        int start; 
        TokenType tok = null; 
        
        if (nextCharacter == '"') {

            current++; 
            start = current; 
            while (current < source.length() && source.charAt(current) != '\n') {
                if (source.charAt(current) == '"') {
                    end = current; 
                    string = source.substring(start, end); 
                    tok = TokenType.STRING; 
                    current++; 
                    return new Token(tok, string, line); 
                }
                current++; 
                        
            }
            error(line, "Unterminated String");

        }

        return null;
    }

    // TODO: test code 
    private Token getNumericToken() {
        // Hint: Follow similar idea of String, but in this case if it is a digit
        // You should only allow one period in your scanner
        char nextCharacter = source.charAt(current); 

        if (isDigit(nextCharacter)) {
            int start = current;
    
            while (current < source.length() && isDigit(source.charAt(current))) {
                current++;
            }
    
            if (current < source.length() && source.charAt(current) == '.') {

                if ((current + 1) < source.length() && isDigit(source.charAt(current + 1))) {
                    current++; 
                    while (current < source.length() && isDigit(source.charAt(current))) {
                        current++;
                    }
                }
            }
    
            String number = source.substring(start, current);
            TokenType tok = TokenType.NUMBER;
            return new Token(tok, number, line);
        }
    
        return null;
    }

    // TODO: Complete implementation
    private Token getIdentifierOrReservedWord() {
        // Hint: Assume first it is an identifier and once you capture it, then check if it is a reserved word.
        char nextCharacter = source.charAt(current); 
        TokenType tok = TokenType.IDENTIFIER; 

        if (isAlpha(nextCharacter)) {
            int start = current; 
            int end; 

            while (current < source.length() && (isAlpha(source.charAt(current)))) {
                current++;
            }
            end = current; 
            String string = source.substring(start, end); 

            tok = keywords.get(string);
            
            if (tok == null) {
                tok = TokenType.IDENTIFIER; 
            }

            return new Token(tok, string, line); 
    
        }

        return null;
    }
    
    // Helper Methods
    private boolean isDigit(char character) {
        return character >= '0' && character <= '9';
    }

    private boolean isAlpha(char character) {
        return character >= 'a' && character <= 'z' ||
                character >= 'A' && character <= 'Z';
    }

    // This will check if a character is what you expect, if so, it will advance
    // Useful for checking <= or //
    private boolean examine(char expected) {
        if (isAtEnd()) return false;
        if (source.charAt(current + 1) != expected) return false;

        // Otherwise, it matches it, so advance
        return true;
    }

    private boolean isAtEnd() {
        return current >= source.length();
    }

    // Error handling
    private void error(int line, String message) {
        System.err.printf("Error occurred on line %d : %s\n", line, message);
        System.exit(ErrorCode.INTERPRET_ERROR);
    }
}
