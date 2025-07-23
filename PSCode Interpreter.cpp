#include <iostream> // enables output of strings to screen
#include <fstream> // enables writing to files
#include <stdio.h>
#include <iomanip> //manipulates floating point data to enable printing
#include <string>
#include <vector>
#include <unordered_map>
#include <cctype>
#include <limits> // for INT_MAX, INT_MIN, FLT_MAX
#include <exception>
#include <stack>
#include<type_traits> //used for generics and type comparisons
#include <sstream> // used to add strings together
using namespace std; //This makes outputting to the console require less lines
using std::string; //Include these lines to add strings

enum class OpType { ADD, SUB, DIV, MULTIPLY, Other }; //Create an enum called OpType that handles different operations
enum class EqualityOperator { LT, GT, EQ, NEQ, LTET, GTET };
enum class DataType { Int, Float, Char, String, Boolean, UnAssigned };
enum class ExceptionType { Lexical, Syntactical, Runtime };

enum class StatementType { BEGIN, END, Output, AssignmentStatement, VariableExpression, IFStatement, ForLoop, WhileLoop, Undefined };
enum class Token { BEGIN, END, IF, WHILE, FOR, ENDIF, THEN, TO, ELSE, ELSEIF, ENDWHILE, NEXT, DISPLAY, SET, AS, ADD, SUB, DIV, MULTIPLY, Int, Float, Char, String, Boolean, OpenBrace, ClosedBrace, Variable, Literal, EqualityOp, AND, OR };

struct TokenInstance {
    Token type = Token::Literal;
    string lexeme = "";
    TokenInstance(Token t = Token::Literal, string l = "") : type(t), lexeme(l) {} //initialise values
};

struct Numeric {
    DataType type;
    int intValue;
    float floatValue;
    Numeric(DataType t = DataType::Int, int intVal = 0, float floatVal = 0) : type(t), intValue(intVal), floatValue(floatVal) {} //Initialise values
};

class Exception {
public:
    ExceptionType exceptionType;
    string message = "Something went wrong";
    // Static callback for code invalidation
    static void (*invalidateCallback)();

    Exception(ExceptionType type, string msg) {
        this->exceptionType = type;
        this->message = msg;
        raise();
    }

    string getExceptionName() {
        switch (exceptionType) {
        case ExceptionType::Lexical:
            return "A lexical error occurred.";
            break;
        case ExceptionType::Syntactical:
            return "A syntactical error occurred.";
            break;
        default: //Runtime exception
            return "A runtime exception occurred.";
            break;
        }
    }

    void raise() {
        cout << getExceptionName() << endl;
        cout << message << endl;
        if (invalidateCallback) invalidateCallback();
    }
};

class Integer {
private:
    int errVal = -69;

public:
    Integer() {}

    bool validInt(string str) {
        try {
            int intVal = stoi(str);
            return true;
        }
        catch (const std::invalid_argument&) {
            return false;
        }
    }

    int stringToInt(string str) {
        bool isValid = validInt(str);
        if (isValid) {
            return stoi(str);
        }
        else {
            Exception ex = Exception(ExceptionType::Runtime, "Cannot convert the token, " + str + ", to an Integer");
            return this->errVal;
        }
    }
};

class FloatingPoint {
private:
    float errVal = -69.0;

public:
    FloatingPoint() {}

    bool validFloat(string str) {
        try {
            float floatVal = stof(str);
            return true;
        }
        catch (const std::invalid_argument&) {
            return false;
        }
    }

    float stringToFloat(string str) {
        bool isValid = validFloat(str);
        if (isValid) {
            return stof(str);
        }
        else {
            Exception ex = Exception(ExceptionType::Runtime, "Cannot convert the token, " + str + ", to a Floating Point number");
            return this->errVal;
        }
    }
};

class Character {
private:
    char def = '~';
public:
    Character() {}

    bool validChar(string str) {
        //cout << "reading character: " << str << " with a length of " << str.length() << endl; // debugging output statement
        //Given a value is read from a file, determine whether or not it is a string. In C++, the single-quote is '/''
        if (str.length() != 3) return false;
        char singleQuote = '\'';
        return str[0] == singleQuote && str[2] == singleQuote;
    }
    char stringToChar(string str) {
        bool isValid = validChar(str);
        if (isValid) {
            return str[1];
        }
        else {
            Exception ex = Exception(ExceptionType::Runtime, "Cannot convert the token, " + str + ", to a Character.");
            return this->def;
        }
    }
};

class StringLiteral {
private:
    string def = "";
public:
    StringLiteral() {}

    bool validString(string str) {
        //Given a value is read from a file, determine whether or not it is a string. In C++, the double-quote is /" /"
        if (str.length() < 2) return false;
        char startChar = str[0];
        char endChar = str[str.length() - 1];
        char doubleQuote = '"';
        return startChar == doubleQuote && endChar == doubleQuote;
    }

    string getStringValue(string str) {
        bool isValid = validString(str);
        if (isValid) {
            try {
                return str.substr(1, str.length() - 2);
            }
            catch (const std::out_of_range&) { //in case of empty string 
                return this->def;
            }
        }
        else {
            Exception ex = Exception(ExceptionType::Runtime, "Cannot convert the token, " + str + ", to a string");
            return this->def;
        }
    }
};

class BooleanValue {
private:
    string def = "FALSE";
public:
    BooleanValue() {}

    bool validBoolean(string str) {
        return str == "TRUE" || str == "FALSE";
    }

    string getBooleanValue(string str) {
        bool isValid = validBoolean(str);
        if (isValid) return str;
        Exception ex = Exception(ExceptionType::Runtime, "Cannot convert the token, " + str + ", to a boolean");
        return this->def;
    }
};

class Stack { //Where variables will be stored

private:
    //Where variable values are stored, accessed, and altered
    unordered_map<string, string> stringVariables;
    unordered_map<string, int> intVariables;
    unordered_map<string, float> floatVariables;
    unordered_map<string, char> charVariables;
    unordered_map<string, string> booleanVariables;

    //Where the type of each variable is stored
    unordered_map<string, DataType> nameToType;

    void raiseRuntimeException(string msg) {
        Exception ex = Exception(ExceptionType::Runtime, msg);
    }

public:

    Stack() {}

    string getTypeName(DataType type) {
        switch (type) {
        case DataType::Int:
            return "Int";
            break;
        case DataType::Float:
            return "Float";
            break;
        case DataType::String:
            return "String";
            break;
        case DataType::Char:
            return "Char";
            break;
        case DataType::Boolean:
            return "Boolean";
            break;
        default:
            return "Undefined";
            break;
        }
    }

    void addNewVariable(string name, DataType type) {

        if (nameToType.find(name) != nameToType.end()) { //variable already exists
            raiseRuntimeException("Variable " + name + " has already been declared");
        }
        else {
            //Add variable to unordered_maps
            //cout << "Variable " + name + " has type: " + getTypeName(type) << endl; // debugging output statement
            switch (type) {
            case DataType::String:
                stringVariables[name] = "";
                nameToType[name] = type;
                break;
            case DataType::Int:
                intVariables[name] = 0;
                nameToType[name] = type;
                break;
            case DataType::Float:
                floatVariables[name] = 0;
                nameToType[name] = type;
                break;
            case DataType::Char:
                charVariables[name] = ' ';
                nameToType[name] = type;
                break;
            case DataType::Boolean:
                booleanVariables[name] = "FALSE";
                nameToType[name] = type;
                break;
            default:
                raiseRuntimeException("The variable initialised has not got a valid type.");
                break;
            }
        }
    }

    //Type-specific values. These methods have the precondition that you have already checked to see if a variable exists and is of the specified type

    string getStringVariable(string name) {
        //precondition: the variable is expected to be a string
        auto it = nameToType.find(name);
        if (it == nameToType.end()) {
            raiseRuntimeException("The variable, " + name + ", doesn't exist.");
            return "";
        }
        return stringVariables[name];
    }

    int getIntegerVariable(string name) {
        //precondition: the variable is expected to be an integer
        auto it = nameToType.find(name);
        if (it == nameToType.end()) {
            raiseRuntimeException("The variable, " + name + ", doesn't exist.");
            return 0;
        }
        return intVariables[name];
    }

    float getFloatVariable(string name) {
        //precondition: the variable is expected to be a floating point number
        auto it = nameToType.find(name);
        if (it == nameToType.end()) {
            raiseRuntimeException("The variable, " + name + ", doesn't exist.");
            return 0;
        }
        return floatVariables[name];
    }

    char getCharVariable(string name) {
        //precondition: the variable is expected to be an integer
        char value = ' ';
        auto it = nameToType.find(name);
        if (it == nameToType.end()) {
            raiseRuntimeException("The variable, " + name + ", doesn't exist.");
        }
        else {
            DataType type = it->second;
            if (type != DataType::Char) {
                raiseRuntimeException("Expected a character variable/literal. Instead, the " + getTypeName(type) + " " + name + " was given.");
            }
            else {
                value = charVariables[name]; // converting from ASCII to char
            }
        }
        return value;
    }

    string getBooleanVariable(string name) {
        auto it = nameToType.find(name);
        if (it == nameToType.end()) {
            raiseRuntimeException("The variable, " + name + ", doesn't exist.");
            return "FALSE";
        }
        return booleanVariables[name];
    }

    void updateVariableValue(string name, string newValue) {
        cout << "The new value of " << name << " = " << newValue << endl; // debugging output statement
        auto it = nameToType.find(name);

        if (it == nameToType.end()) {
            //If the variable doesn't exist, raise an exception
            raiseRuntimeException("The variable, " + name + ", doesn't exist.");
        }
        else {
            DataType type = it->second;

            switch (type) {
            case DataType::String:
                stringVariables[name] = newValue;
                break;
            case DataType::Int: {
                Integer Int;
                if (Int.validInt(newValue)) {
                    intVariables[name] = Int.stringToInt(newValue);
                }
                else {
                    raiseRuntimeException("The value, " + newValue + ", is not a valid integer.");
                }
                break;
            }
            case DataType::Float: {
                FloatingPoint Float;
                if (Float.validFloat(newValue)) {
                    floatVariables[name] = Float.stringToFloat(newValue);
                }
                else {
                    raiseRuntimeException("The value, " + newValue + ", is not a valid floating point number.");
                }
                break;
            }
            case DataType::Char: {
                Character Char;
                charVariables[name] = Char.stringToChar(newValue);
                break;
            }
            case DataType::Boolean: {
                BooleanValue Boolean;
                if (Boolean.validBoolean(newValue)) {
                    booleanVariables[name] = newValue;
                }
                else {
                    raiseRuntimeException("The value, " + newValue + ", is not a valid boolean.");
                }
                break;
            }
            }
        }
    }

    string getValueAsString(string name) {
        string res = "";
        auto it = nameToType.find(name);
        bool variableExists = it != nameToType.end();
        if (!variableExists) {
            raiseRuntimeException("The variable, " + name + ", has not been defined.");
            return res;
        }
        DataType type = it->second;
        switch (type) {
        case DataType::Int:
            res = to_string(getIntegerVariable(name));
            break;
        case DataType::Float:
            res = to_string(getFloatVariable(name));
            break;
        case DataType::Char:
            res = string(1, getCharVariable(name));
            break;
        case DataType::String:
            res = getStringVariable(name);
            break;
        default:
            res = getBooleanVariable(name);
            break;
        }
        return res;
    }

    DataType getType(string name) {
        DataType type = DataType::UnAssigned;
        auto it = nameToType.find(name);
        if (it != nameToType.end()) {
            type = it->second;
        }
        else {
            raiseRuntimeException("Variable, " + name + ", has not been declared.");
        }
        return type;
    }

};

class Lexer {
private:
    string line;
    bool validToken = true;
    unordered_map<string, Token> tokenMap = { //The tokenMap handles terminal values
        {"BEGIN", Token::BEGIN},
        {"END", Token::END},
        {"IF", Token::IF},
        {"WHILE", Token::WHILE},
        {"FOR", Token::FOR},
        {"END IF", Token::ENDIF},
        {"THEN", Token::THEN},
        {"TO", Token::TO},
        {"ELSE", Token::ELSE},
        {"ELSE IF", Token::ELSEIF},
        {"END WHILE", Token::ENDWHILE},
        {"NEXT", Token::NEXT},
        {"DISPLAY", Token::DISPLAY},
        {"SET", Token::SET},
        {"AS", Token::AS},
        //arithmetic operands
        {"+", Token::ADD},
        {"-", Token::SUB},
        {"/", Token::DIV},
        {"*", Token::MULTIPLY},
        //equality operands
        {"=", Token::EqualityOp},
        {">", Token::EqualityOp},
        {">=", Token::EqualityOp},
        {"<", Token::EqualityOp},
        {"<=", Token::EqualityOp},
        {"<>", Token::EqualityOp},
        //data types and other
        {"int", Token::Int},
        {"float", Token::Float},
        {"char", Token::Char},
        {"string", Token::String},
        {"bool", Token::Boolean},
        {"(", Token::OpenBrace},
        {")", Token::ClosedBrace},
        {"AND", Token::AND},
        {"OR", Token::OR},
        //TRUE and FALSE terminal values
        {"TRUE", Token::Literal},
        {"FALSE", Token::Literal}
    };
public:
    Lexer() {}

    void setLine(string line) {
        this->line = line;
    }

    bool tokensValid() const {
        return this->validToken;
    }

    bool isDigit(char c) {
        //Determines whether or not a character is a digit using std library method
        return isdigit(c);
    }

    bool isLetter(char c) {
        int asc = int(c);
        //if lowercase or capital letter (based on ascii table value), return true
        return (asc >= 97 && asc <= 122) || (asc >= 65 && asc <= 90);
    }

    bool isSpecialChar(char c) {
        //define special characters as -> `~!@#$%^&*()-+={}[]|;:"/?.>,<
        bool special = false;
        unordered_map<char, bool> specialChars = { {'`', true}, {'~', true}, { '!', true }, {'@', true}, {'#', true}, {'$', true}, {'%', true}, {'^', true}, {'&', true}, {'*', true}, {'(', true}, {')', true}, {'-', true}, {'+', true}, {'=', true}, {'{', true}, {'}', true}, {'[', true}, {']', true}, {'\'', true},  {'|', true}, {';', true}, {':', true}, {'\'', true},  {'"', true}, {'/', true}, {'?', true}, {'.', true}, {'>', true}, {',', true}, {'<', true} };
        auto it = specialChars.find(c);
        if (it != specialChars.end()) {
            special = it->second;
        }
        return special;
    }

    bool isValidVariable(string element) {
        bool valid = true;
        size_t len = element.length();
        if (len > 0 && valid) {
            valid = isLetter(element[0]);
            int i = 1;

            while (i < element.length() && valid) {
                valid = !isSpecialChar(element[i]);
                i++;
            }
        }
        return valid;
    }

    bool isLiteral(string element) {
        Integer Int;
        bool isLiteral = false;

        isLiteral = Int.validInt(element);
        if (isLiteral) {
            return true;
        }
        FloatingPoint Float;
        isLiteral = Float.validFloat(element);
        if (isLiteral) {
            return true;
        }
        Character Char;
        isLiteral = Char.validChar(element);
        if (isLiteral) {
            return true;
        }
        StringLiteral String;
        isLiteral = String.validString(element);
        if (isLiteral) {
            return true;
        }
        return false;
    }

    Token getToken(const string& element) {
        auto it = tokenMap.find(element);
        if (it != tokenMap.end()) {
            return it->second;
        }
        if (isValidVariable(element)) {
            return Token::Variable;
        }
        if (isLiteral(element)) {
            return Token::Literal;
        }
        raiseException("An unexpected token was found: " + element);
        return Token::Literal; // Default fallback
    }

    string removeNewline(string line) {
        string chopped = line;
        if (!line.empty()) {//if the line isn't an empty string
            if (line.back() == '\n' || line.back() == '\r') { //if the last character is a newline
                size_t len = line.length();
                chopped.erase(len - 1); //remove last character
            }
        }
        return chopped;
    }

    bool isSpace_orEnd(char c, int idx, size_t lastIdx) { //determines if a character is the last in a string, or is a space character
        if (c == ' ') {
            return true;
        }
        if (idx == lastIdx) {
            return true;
        }
        return false;
    }

    vector<string> chopIntoLexemes(string line) {
        vector<string> lexemes;
        size_t i = 0;
        size_t n = line.length();
        while (i < n) {
            // Skip spaces
            while (i < n && isspace(line[i])) i++;
            //if (i >= n) break;

            // Handle quoted strings
            if (line[i] == '"') {
                size_t start = i++;
                while (i < n && line[i] != '"') i++;
                if (i < n) i++; // Include closing quote
                lexemes.push_back(line.substr(start, i - start));
            }
            // Handle quoted characters
            else if (line[i] == '\'') {
                size_t start = i++;
                while (i < n && line[i] != '\'') i++;
                i += 1;
                lexemes.push_back(line.substr(start, i - start));
            }
            // Handle multi-char operators
            else if (i + 1 < n && (line.substr(i, 2) == ">=" || line.substr(i, 2) == "<=" || line.substr(i, 2) == "<>")) {
                lexemes.push_back(line.substr(i, 2));
                i += 2;
            }
            // Handle numbers (including negative and decimal)
            else if ((line[i] == '-' && i + 1 < n && isdigit(line[i + 1])) || isdigit(line[i])) {
                size_t start = i;
                if (line[i] == '-') i++; // skip minus
                while (i < n && (isdigit(line[i]) || line[i] == '.')) i++;
                lexemes.push_back(line.substr(start, i - start));
            }
            // Handle single-char tokens (operators, punctuation)
            else if (ispunct(line[i])) {
                lexemes.push_back(string(1, line[i]));
                i++;
            }
            // Handle identifiers (variables, keywords)
            else {
                size_t start = i;
                while (i < n && !isspace(line[i]) && !ispunct(line[i])) i++;
                lexemes.push_back(line.substr(start, i - start));
            }
        }
        return lexemes;
    }


    vector<string> revisedLexemes(const vector<string>& lexemes) {
        vector<string> revised;
        size_t i = 0;
        while (i < lexemes.size()) {
            if (i + 1 < lexemes.size()) {
                string combined = lexemes[i] + " " + lexemes[i + 1];
                if (combined == "END IF" || combined == "ELSE IF" || combined == "END WHILE") {
                    revised.push_back(combined);
                    i += 2;
                    continue;
                }
            }
            revised.push_back(lexemes[i]);
            i++;
        }
        return revised;
    }

    void printLexemes(vector<string> lexemes) {
        string msg = "";

        for (string lexeme : lexemes) {
            msg = msg + lexeme + "\n";
        }

        msg += "\n";
        cout << msg;
    }

    vector<TokenInstance> tokenizeLine() {
        vector<TokenInstance> tokens;
        vector<string> lexemes;
        //cout << "Tokenizing" << endl;
        line = removeNewline(line); //remove newline characters
        //cout << "Without newline: " + line << endl;
        lexemes = chopIntoLexemes(line); //break line into substrings based on spaces
        //cout << "Chopped into lexemes...\n";
        //cout << "Revised lexemes...\n";
        lexemes = revisedLexemes(lexemes);//edit those substrings for tokens like ELSEIF, ENDIF, ENDWHILE, ect
        //printLexemes(lexemes); // debugging output statement

        Token t = Token::Literal;

        for (auto& lexeme : lexemes) { //convert each substring into TokenInstance and add new TokenInstances to the tokens vector
            t = getToken(lexeme);
            tokens.push_back({ t, lexeme });
        }

        return tokens;
    }

    void raiseException(string msg) {
        this->validToken = false;
        Exception ex = Exception(ExceptionType::Lexical, msg);
    }

};

class Statement { //Every statement inherits from this
    //TDLR
public:
    virtual ~Statement() {}
    virtual void printValues() {}
    virtual void execute() {} //generic -> runs statement
    virtual void raiseRuntimeException(string msg) {
        Exception ex(ExceptionType::Runtime, msg);
    }

    Statement() {}
};

class Expression : public Statement {
    //The virtual keyword implements polymorphism and inheritance easily. It states that a method or attribute can be overridden.
public:
    virtual ~Expression() {}
    virtual Numeric evaluate() const = 0;
    //The const terminator is put after the method signature to tell the compiler that this method doesn't modify the state of the object.
    //The override terminator ensures a class correctly overrides a virtual method from a base class.
};

class Literal :public Expression { //This is a leaf node that can represent numbers, and be combined with BinaryOperation to hold expressions
    Numeric value;
public:
    //The return values provided by the Literal class vary based on whether or not a floating point value or integer is required
    Literal(int val) : value(DataType::Int, val, 0.0f) {}
    Literal(float val) : value(DataType::Float, 0, val) {}
    //This type of initialisation is called an initialiser list. It is a memory efficient and fast way of initialising class members.
    Numeric evaluate() const override {
        return value;
    }
};

class BinaryOperation : public Expression {
    OpType op;
    Expression* left;
    Expression* right;
    //Defining leaf-nodes to store Value structs

public:
    BinaryOperation(OpType op, Expression* left, Expression* right) : op(op), left(left), right(right) {}
    //Initialising virtual constructor

    void checkOverflow(OpType op, Numeric l, Numeric r) {
        //Check overly large and small numbers
        if (l.type == DataType::Int) {
            switch (op) {
            case OpType::ADD:
                if ((r.intValue > 0 && l.intValue > numeric_limits<int>::max() - r.intValue) || (r.intValue < 0 && l.intValue < numeric_limits<int>::min() - r.intValue)) {
                    const_cast<BinaryOperation*>(this)->raiseRuntimeException("Integer overflow on addition.");
                }
                break;
            case OpType::SUB:
                if ((r.intValue < 0 && l.intValue > numeric_limits<int>::max() + r.intValue) || (r.intValue > 0 && l.intValue < numeric_limits<int>::min() + r.intValue)) {
                    const_cast<BinaryOperation*>(this)->raiseRuntimeException("Integer overflow on subtraction.");
                }
                break;
            case OpType::MULTIPLY: { // check all 4 case in which overflow can happen with multiplication
                bool invalid = false;
                if (l.intValue > 0) {
                    if (r.intValue > 0) {
                        if (l.intValue > numeric_limits<int>::max() / r.intValue) {
                            invalid = true;
                        }
                    }
                    else {
                        if (r.intValue < numeric_limits<int>::min() / l.intValue) {
                            invalid = true;
                        }
                    }
                }
                else if (l.intValue < 0) {
                    if (r.intValue > 0) {
                        if (l.intValue < numeric_limits<int>::min() / r.intValue) {
                            invalid = true;
                        }
                    }
                    else {
                        if (l.intValue != 0 && r.intValue < numeric_limits<int>::max() / l.intValue) invalid = true;
                    }
                }
                if (invalid) {
                    const_cast<BinaryOperation*>(this)->raiseRuntimeException("Integer overflow on multiplication.");
                }
                break;
            }
            default: //OpType::DIV
                if (r.intValue == 0) {
                    const_cast<BinaryOperation*>(this)->raiseRuntimeException("Integer division by zero error.");
                }
                //Integer division overflow happens in only one case, when INT_MIN / -1, because of the 32-bit integer system
                else if (l.intValue == numeric_limits<int>::min() && r.intValue == -1) {
                    const_cast<BinaryOperation*>(this)->raiseRuntimeException("Integer overflow on division (INT_MIN / -1).");
                }
                break;
            }
        }
        else { // DataType::Float
            switch (op) {
            case OpType::ADD:
                if ((r.floatValue > 0 && l.floatValue > numeric_limits<float>::max() - r.floatValue) || (r.floatValue < 0 && l.floatValue < numeric_limits<float>::min() - r.floatValue)) {
                    const_cast<BinaryOperation*>(this)->raiseRuntimeException("Floating point overflow on addition.");
                }
                break;
            case OpType::SUB:
                if ((r.floatValue  < 0 && l.floatValue  > numeric_limits<float>::max() + r.floatValue) || (r.floatValue > 0 && l.floatValue < numeric_limits<float>::min() + r.floatValue)) {
                    const_cast<BinaryOperation*>(this)->raiseRuntimeException("Floating point overflow on subtraction.");
                }
                break;
            case OpType::MULTIPLY: { // check all 4 case in which overflow can happen with multiplication
                bool invalid = false;
                if (l.floatValue > 0) {
                    if (r.floatValue > 0) {
                        if (l.floatValue > numeric_limits<float>::max() / r.floatValue) {
                            invalid = true;
                        }
                    }
                    else {
                        if (r.floatValue < -numeric_limits<float>::max() / l.floatValue) {
                            invalid = true;
                        }
                    }
                }
                else if (l.floatValue < 0) {
                    if (r.floatValue > 0) {
                        if (l.floatValue < -numeric_limits<float>::max() / r.floatValue) {
                            invalid = true;
                        }
                    }
                    else {
                        if (l.floatValue != 0 && r.floatValue < numeric_limits<float>::max() / l.floatValue) {
                            invalid = true;
                        }
                    }
                }
                if (invalid) {
                    const_cast<BinaryOperation*>(this)->raiseRuntimeException("Floating point overflow on multiplication.");
                }
                break;
            }
            default: // OpType::DIV

                if (r.floatValue == 0.0f) {
                    const_cast<BinaryOperation*>(this)->raiseRuntimeException("Floating point division by zero error.");
                }
                // Check for overflow using limits
                else {
                    float result = l.floatValue / r.floatValue;
                    if (result > numeric_limits<float>::max() || result < -numeric_limits<float>::max()) {
                        const_cast<BinaryOperation*>(this)->raiseRuntimeException("Floating point overflow on division.");
                    }
                }
                break;
            }
        }
    }

    Numeric evaluate() const override { //override keyword shows overriding class method by child class
        Numeric l = left->evaluate();
        Numeric r = right->evaluate();
        DataType resType = l.type;
        int intRes = 0;
        float floatRes = 0;
        if (l.type != r.type) {
            //Use const_cast to call the non-const raiseRuntimeException method
            const_cast<BinaryOperation*>(this)->raiseRuntimeException("Type mismatch in binary operation.");
            return Numeric(resType, intRes, floatRes);
        }
        const_cast<BinaryOperation*>(this)->checkOverflow(op, l, r);
        switch (op) {
        case OpType::ADD:
            if (resType == DataType::Int) {
                intRes = l.intValue + r.intValue;
            }
            else {
                floatRes = l.floatValue + r.floatValue;
            }
            break;
        case OpType::SUB:
            if (resType == DataType::Int) {
                intRes = l.intValue - r.intValue;
            }
            else {
                floatRes = l.floatValue - r.floatValue;
            }
            break;
        case OpType::DIV:
            if (l.type == DataType::Int) {
                intRes = l.intValue / r.intValue;
            }
            //DataType::Float
            else {
                floatRes = l.floatValue / r.floatValue;
            }

            break;
        case OpType::MULTIPLY:
            if (l.type == DataType::Int) {
                intRes = l.intValue * r.intValue;
            }
            else {
                floatRes = l.floatValue * r.floatValue;
            }
            break;
        }
        return Numeric(resType, intRes, floatRes);
    }
};
Stack VariablesStack; // Global variable used for storing variable data

class Assignment : public Statement {
private:
    DataType type;
    string name;
public:
    Assignment(DataType newType, string newName) {
        this->type = newType;
        this->name = newName;
    }
    Assignment() {
        this->type = DataType::UnAssigned;
        this->name = "unknown variable";
    }
    void alterName(string name) { //N.B. THIS SHOULD ONLY BE CALLED BY THE PARSER
        this->name = name;
    }
    void alterType(DataType type) {//N.B. THIS SHOULD BE ONLY CALLED BY THE PARSER
        this->type = type;
    }
    void execute() override {
        VariablesStack.addNewVariable(this->name, this->type);
        //cout << "New variable added, " + name; // debugging output statement
    }
};

class OutputStatement : public Statement {
private:
    vector<TokenInstance> outputValues;
public:
    void addVariable(TokenInstance value) {
        this->outputValues.push_back(value);
    }

    size_t numVariables() {
        return outputValues.size();
    }

    string getValue(TokenInstance t) {
        string res = "";
        if (t.type == Token::Literal) {
            return t.lexeme;
        }
        string name = t.lexeme;
        res = VariablesStack.getValueAsString(name);
        return res;
    }

    void execute() override {
        for (const auto& token : outputValues) {
            //for (TokenInstance token: outputValues) {
                //cout << "valid";
            cout << getValue(token);
        }
        cout << endl;
    }
};

class VariableExpression : public Statement { //putting data into declared variables
private:
    TokenInstance lhsVar = { Token::Literal, "" };
    vector<TokenInstance> tokens;
    Expression* expr = nullptr;
    DataType expectedType = DataType::Int;
    bool needToEvaluate = true;

    int getPrecedence(OpType op) {
        if (op == OpType::MULTIPLY || op == OpType::DIV) {
            return 2;
        }
        else if (op == OpType::ADD || op == OpType::SUB) {
            return 1;
        }
        else {
            return 0;
        }
    }

    // Helper: Convert TokenInstance to OpType (already exists in Parser, but add here for self-containment)
    OpType getOpType(TokenInstance t) {
        switch (t.type) {
        case Token::ADD: return OpType::ADD;
        case Token::SUB: return OpType::SUB;
        case Token::DIV: return OpType::DIV;
        case Token::MULTIPLY: return OpType::MULTIPLY;
        default: return OpType::Other;
        }
    }
    //Helper: get operand values
    bool isOperand(TokenInstance token) {
        return token.type == Token::ADD || token.type == Token::SUB || token.type == Token::MULTIPLY || token.type == Token::DIV;
    }

    //Helper: get value of variable/lexeme
    int getIntValue(TokenInstance token) {
        //return stoi(token.lexeme); // For now, only handle integer literals
        Integer Int;
        if (token.type == Token::Literal) {
            return  Int.stringToInt(token.lexeme);
        }
        //assume type = Token::Variable
        DataType type = VariablesStack.getType(token.lexeme);
        if (type == DataType::Int) {
            cout << "The variable " + token.lexeme + " has a value of " << VariablesStack.getIntegerVariable(token.lexeme) << endl; // debugging output statement
            return VariablesStack.getIntegerVariable(token.lexeme);
        }

        raiseRuntimeException("Type mismatch for token, " + token.lexeme + ". An integer variable was expected.");
        return 0;
    }

    float getFloatValue(TokenInstance token) {
        FloatingPoint Float;
        if (token.type == Token::Literal) {
            return Float.stringToFloat(token.lexeme);
        }
        DataType type = VariablesStack.getType(token.lexeme);
        if (type == DataType::Float) {
            cout << "The variable " + token.lexeme + " has a value of " << VariablesStack.getFloatVariable(token.lexeme) << endl; // debugging output statement
            return VariablesStack.getFloatVariable(token.lexeme);
        }
        raiseRuntimeException("Type mismatch for token, " + token.lexeme + ". A floating point variable was expected." + " Instead recieved variable of type " + VariablesStack.getTypeName(type) + ". ");
        return 0;
    }

    int getValue(TokenInstance token) {
        //This gets all values for Variables/Literals that aren't floating point or string. The values are coerced to integers
        if (expectedType == DataType::Int) {
            return getIntValue(token);
        }
        else if (expectedType == DataType::Char) {
            char c = ' ';
            if (token.type == Token::Literal) {
                Character Char;
                c = Char.stringToChar(token.lexeme);
            }
            else if (token.type == Token::Variable) {
                c = VariablesStack.getCharVariable(token.lexeme);
            }
            return (int)c; //convert back to integer
        }
        else if (expectedType == DataType::Boolean) {
            string boolString = "FALSE";
            if (token.type == Token::Literal) {
                BooleanValue Boolean;
                boolString = Boolean.getBooleanValue(token.lexeme);
            }
            else if (token.type == Token::Variable) {
                boolString = VariablesStack.getBooleanVariable(token.lexeme);
            }
            if (boolString == "TRUE") {
                return 1;
            }
            return 0;
        }
        return 0;
    }

    Expression* buildASTFromPostfix(vector<TokenInstance> postfix) {
        stack<Expression*> exprStack;
        OpType type = OpType::Other;
        for (auto& token : postfix) {
            if (token.type == Token::Literal || token.type == Token::Variable) {
                if (expectedType == DataType::Int) {
                    int val = getIntValue(token);
                    exprStack.push(new Literal(val));
                }
                else if (expectedType == DataType::Float) {
                    float val = getFloatValue(token);
                    exprStack.push(new Literal(val));
                }
            }
            else if (token.type == Token::ADD || token.type == Token::SUB || token.type == Token::MULTIPLY || token.type == Token::DIV) {
                if (exprStack.size() < 2) return nullptr; // ADD DIVISION BY ZERO AND OVERLY LARGE NUMBER ERROR CHECKING
                Expression* right = exprStack.top();
                exprStack.pop();
                Expression* left = exprStack.top();
                exprStack.pop();
                type = getOpType(token);
                exprStack.push(new BinaryOperation(type, left, right));
            }
        }
        if (exprStack.size() == 1) return exprStack.top();
        return nullptr;
    }

    vector<TokenInstance> infixToPostfix(vector<TokenInstance> tokens) {
        //This makes it easier to evaluate mathematical expressions by rewriting the expression so that the order of operations is explicit
        vector<TokenInstance> output;
        stack<TokenInstance> opStack;

        for (auto& token : tokens) {
            //It simplifies the expression by adding the variables and literals to the output vector, adding operations to the stack, and removing braces
            if (token.type == Token::Literal || token.type == Token::Variable) {
                output.push_back(token);
            }
            else if (token.type == Token::OpenBrace) {
                opStack.push(token);
            }
            else if (token.type == Token::ClosedBrace) {
                while (!opStack.empty() && opStack.top().type != Token::OpenBrace) {
                    output.push_back(opStack.top());
                    opStack.pop();
                }
                if (!opStack.empty() && opStack.top().type == Token::OpenBrace) {
                    opStack.pop();
                }
            }
            else if (isOperand(token)) {
                OpType currOp = getOpType(token);
                while (!opStack.empty() && isOperand(opStack.top())) {
                    //This shuffles the order of tokens to ensure the operands are in the correct order
                    OpType topOp = getOpType(opStack.top());
                    if (getPrecedence(topOp) >= getPrecedence(currOp)) {
                        output.push_back(opStack.top());
                        opStack.pop();
                    }
                    else {
                        break;
                    }
                }
                opStack.push(token);
            }
        }
        while (!opStack.empty()) {
            output.push_back(opStack.top());
            opStack.pop();
        }
        printPostfix(output); // debugging output statement
        return output;
    }

    void printPostfix(vector<TokenInstance> tokens) {
        for (auto& token : tokens) {
            cout << token.lexeme + " ";
        }
        cout << endl;
    }

    Expression* buildAST_NonNumeric(vector<TokenInstance> postfix) {
        stack<Expression*> exprStack;
        int val = 0;
        for (auto& token : postfix) {
            if (token.type == Token::Literal || token.type == Token::Variable) {
                val = getValue(token);
                exprStack.push(new Literal(val));
            }
            else if (token.type == Token::ADD || token.type == Token::SUB || token.type == Token::MULTIPLY || token.type == Token::DIV) {
                if (exprStack.size() < 2) return nullptr; // ADD DIVISION BY ZERO AND OVERLY LARGE NUMBER ERROR CHECKING
                Expression* right = exprStack.top();
                exprStack.pop();
                Expression* left = exprStack.top();
                exprStack.pop();
                exprStack.push(new BinaryOperation(getOpType(token), left, right));
            }
        }
        if (exprStack.size() == 1) return exprStack.top();
        return nullptr;
    }

    Numeric mathematicalExpression() {
        Numeric res;
        vector<TokenInstance> postFix = infixToPostfix(tokens); //Simplify tokens and order of execution based on BODMAS
        Expression* expr = buildASTFromPostfix(postFix); //Generate expression based on tokens
        if (expr) {
            res = expr->evaluate(); //Execute expression if it is valid
        }
        delete expr;
        return res;
    }

    char characterExpression() {
        Numeric result;
        vector<TokenInstance> postFix = infixToPostfix(tokens);
        Expression* expr = buildAST_NonNumeric(postFix);
        if (expr) {
            result = expr->evaluate();
        }
        char res = result.intValue;
        return res;
    }

    string booleanExpression() {
        string res = "FALSE";
        Numeric result;
        vector<TokenInstance> postFix = infixToPostfix(tokens);
        Expression* expr = buildAST_NonNumeric(postFix);
        if (expr) {
            result = expr->evaluate();
        }
        if (result.intValue > 0) {
            res = "TRUE";
        }
        else {
            res = "FALSE";
        }
        return res;
    }

    vector<TokenInstance> removeADDTokens() {
        vector<TokenInstance> newTokens;
        if (tokens.size() > 1) {
            size_t i = 0;
            Token t;
            while (i < tokens.size()) {
                t = tokens[i].type;
                if (t != Token::ADD && t != Token::OpenBrace && t != Token::ClosedBrace) { // skip ADD tokens and braces
                    newTokens.push_back(tokens[i]);
                }
                i++;
            }
        }
        else {
            newTokens.push_back(tokens[0]);
        }
        return newTokens;
    }

    string stringExpression(vector<TokenInstance> adjustedTokens) {
        string res = "";
        bool validExpr = true;
        string msg = "";
        size_t idx = 0;
        TokenInstance t;
        DataType type = DataType::String;
        string currentString = "";
        stringstream stream;
        StringLiteral String;

        while (idx < adjustedTokens.size() && validExpr) {
            currentString = "";
            t = adjustedTokens[idx];
            if (t.type == Token::Literal) {
                currentString = String.getStringValue(t.lexeme);
            }

            else if (t.type == Token::Variable) {
                type = VariablesStack.getType(t.lexeme);
                if (type != DataType::String) {
                    validExpr = false;
                    msg = "The variable, " + t.lexeme + ", is not a string variable and does not belong in a string expression.";
                }
                else {
                    currentString = VariablesStack.getStringVariable(t.lexeme);
                    cout << "The variable " + t.lexeme + " has a value of " << currentString << endl; // debugging output statement
                }
            }

            else if (t.type == Token::DIV || t.type == Token::MULTIPLY || t.type == Token::SUB) {
                validExpr = false;
                msg = "The operator, " + t.lexeme + ", was not expected in a string expression.";
            }

            if (currentString != "") { // adding strings together
                res = res + currentString;
            }
            idx++;
        }
        if (!validExpr) {
            raiseRuntimeException(msg);
        }
        return res;
    }

    void shorthandAssignment() {
        //This is called when a variable is set to a single variable / value
        TokenInstance rhs = tokens[0];
        string value;
        string expectedTypeName = VariablesStack.getTypeName(expectedType);
        string varName = lhsVar.lexeme;
        bool validExpr = true;
        StringLiteral String;
        Character Char;
        if (rhs.type == Token::Variable) {
            value = VariablesStack.getValueAsString(rhs.lexeme);
        }
        else if (rhs.type == Token::Literal) {
            if (expectedType == DataType::String) {
                value = String.getStringValue(rhs.lexeme);
            }
            else {
                value = rhs.lexeme;
            }
        }
        else {
            validExpr = false;
            raiseRuntimeException("Invalid value for type " + expectedTypeName + ". Expected a " + expectedTypeName + " but got " + rhs.lexeme + " instead.");
        }
        if (validExpr) {
            VariablesStack.updateVariableValue(varName, value);
        }
    }

public:
    VariableExpression(vector<TokenInstance> tokens, TokenInstance lhsVar, bool needToEvaluate) {
        this->tokens = tokens;
        this->lhsVar = lhsVar;
        this->needToEvaluate = needToEvaluate;

        this->expr = nullptr;
        this->expectedType = DataType::Int;
    }


    void execute() override {
        //getting the variable expression type
        string lhsVarName = lhsVar.lexeme;
        DataType lhsVarType = VariablesStack.getType(lhsVarName);
        this->expectedType = lhsVarType;

        //Declaring variables
        Numeric numericRes;
        vector<TokenInstance> adjustedTokens;
        string stringRes;
        char charRes = ' ';
        string boolRes = "FALSE";

        cout << "For " + lhsVarName + " the need to evaluate is " << needToEvaluate << "." << endl;
        if (!needToEvaluate) {
            shorthandAssignment();
        }
        else {

            switch (lhsVarType) {
            case DataType::Int: //For integers and booleans, parse as a mathematical expression
                numericRes = mathematicalExpression();
                stringRes = to_string(numericRes.intValue);
                break;
            case DataType::Float:
                numericRes = mathematicalExpression();
                stringRes = to_string(numericRes.floatValue);
                break;
            case DataType::String: //For strings, prase as a string expression
                adjustedTokens = removeADDTokens();
                stringRes = stringExpression(adjustedTokens);
                break;
            case DataType::Char: {
                //For characters, the ascii values are added, subtracted, multiplied, and divided
                charRes = characterExpression();
                stringRes = charRes;// convert the given value back to 'character' by adding quotation marks
                stringstream stream;
                stream << "'" << charRes << "'";
                stringRes = stream.str();
                break;
            }
            default: //boolean variables
                //For booleans, mathematical operations are performed as usual. If the result is non-zero, it is coerced back to TRUE, and if the result is zero, it is coerced to FALSE.
                boolRes = booleanExpression();
                stringRes = boolRes;
                break;
            }
            VariablesStack.updateVariableValue(lhsVarName, stringRes);
        }
    }

    ~VariableExpression() {
        delete expr;
        expr = nullptr;
    }

};

class Condition {
private:
    TokenInstance lhs;
    TokenInstance rhs;
    EqualityOperator op = EqualityOperator::EQ;

    bool evaluateConditions_withIntegers(int lhsVal, int rhsVal) {
        bool valid = false;
        switch (this->op) {
        case EqualityOperator::LT:
            valid = true ? lhsVal < rhsVal : false;
            break;
        case EqualityOperator::LTET:
            valid = true ? lhsVal <= rhsVal : false;
            break;
        case EqualityOperator::GT:
            valid = true ? lhsVal > rhsVal : false;
            break;
        case EqualityOperator::GTET:
            valid = true ? lhsVal > rhsVal : false;
            break;
        }
        return valid;
    }

    bool evaluateConditions_withFloats(float lhsVal, float rhsVal) {
        bool valid = false;
        switch (this->op) {
        case EqualityOperator::LT:
            valid = true ? lhsVal < rhsVal : false;
            break;
        case EqualityOperator::LTET:
            valid = true ? lhsVal <= rhsVal : false;
            break;
        case EqualityOperator::GT:
            valid = true ? lhsVal > rhsVal : false;
            break;
        case EqualityOperator::GTET:
            valid = true ? lhsVal > rhsVal : false;
            break;
        }
        return valid;
    }

public:
    Condition(TokenInstance lhs, TokenInstance rhs, EqualityOperator op) {
        this->lhs = lhs;
        this->rhs = rhs;
        this->op = op;
    }

    string getValue(TokenInstance t) {
        if (t.type == Token::Literal) {
            return t.lexeme;
        }
        //If it is a variable
        return VariablesStack.getValueAsString(t.lexeme);
    }

    bool evaluateCondition() {
        bool result = false;
        string lhsValue = getValue(this->lhs);
        string rhsValue = getValue(this->rhs);
        //get integer and float values for lhs and rhs

        bool valid = false;
        switch (this->op) {
        case EqualityOperator::EQ:
            result = true ? lhsValue == rhsValue : false;
            break;
        case EqualityOperator::NEQ:
            result = true ? lhsValue != rhsValue : false;
            break;
        default:
            Integer I = Integer();
            FloatingPoint F = FloatingPoint();
            //check LT, GT, LTET, GTET conditions using float or integer values, depending on whether or not they are valid
            if (I.validInt(lhsValue) && I.validInt(rhsValue)) {
                int lhs_IntVal = I.stringToInt(lhsValue);
                int rhs_IntVal = I.stringToInt(rhsValue);
                valid = this->evaluateConditions_withIntegers(lhs_IntVal, rhs_IntVal);
            }
            else if (F.validFloat(lhsValue) && F.validFloat(rhsValue)) {
                float lhs_FloatVal = F.stringToFloat(lhsValue);
                float rhs_FloatVal = F.stringToFloat(rhsValue);
                valid = this->evaluateConditions_withFloats(lhs_FloatVal, rhs_FloatVal);
            }
            else {
                raiseException("Improper values in above condition.");
            }
        }

        return result;
    }
    void raiseException(string exceptionMsg) {
        Exception ex = Exception(ExceptionType::Runtime, exceptionMsg);
    }
};

class ConditionalStatement : public Statement {
private:
    vector<Condition> conditionals;
    vector<TokenInstance> linkingStatements;
public:
    ConditionalStatement() {}

    ConditionalStatement(vector<Condition> conditionals, vector<TokenInstance> linkingStatements) {
        this->conditionals = conditionals;
        this->linkingStatements = linkingStatements;
    }
    bool evaluateConditionalStatement() {
        bool result = conditionals[0].evaluateCondition();

        if (this->conditionals.size() == 1) {
            return result;
        }
        int conditionIdx = 1;
        Token link;
        int linkIdx;
        bool lhs = false;
        bool rhs = false;

        while (conditionIdx < this->conditionals.size()) {
            linkIdx = conditionIdx - 1;
            link = this->linkingStatements[linkIdx].type;
            lhs = this->conditionals[conditionIdx - 1].evaluateCondition();
            rhs = this->conditionals[conditionIdx].evaluateCondition();

            if (link == Token::AND) {
                result = lhs && rhs;
            }
            else { //link = Token::OR
                result = lhs || rhs;
            }
            conditionIdx++;
        }
        return result;
    }
};

class Branch {
private:
    ConditionalStatement conditionStatement;
    vector<Statement*> statements;
public:
    Branch() {}

    ~Branch() { //destructor
        for (auto stat : this->statements) delete stat;
    }

    Branch(ConditionalStatement conditionStatement, vector<Statement*> statements) {
        this->conditionStatement = conditionStatement;
        this->statements = statements;
    }
    bool evaluateCondition() {
        return conditionStatement.evaluateConditionalStatement();
    }
    void executeStatements() {
        for (auto& stat : statements) {
            stat->execute();
        }
    }
    void executeBranch() {
        if (evaluateCondition()) {
            executeStatements();
        }
    }
};

struct BranchNode {
    //ConditionalStatement condition; // For IF and ELSE IF; ignored for ELSE
    Branch* current = nullptr;
    BranchNode* nested = nullptr;
    BranchNode* nextBranch = nullptr;
    bool isElseBranch = false;          // True if this is an ELSE branch

    BranchNode() = default;
    ~BranchNode() { //destructor
        /*for (auto stat : nested->statements) delete child;
        for (auto stmt : statements) delete stmt;*/
        delete nested;
        delete nextBranch;
        delete current;
    }
};


class IFStatement : public Statement {
private:
    BranchNode* root; // Root node for this IF statement

    void executeBranches(BranchNode* currentBranchNode) {
        bool finished = false;
        bool res = false;
        while (!finished) {
            Branch* branch = currentBranchNode->current;
            BranchNode* nestedNode = currentBranchNode->nested;
            if (currentBranchNode->isElseBranch == false) {
                res = branch->evaluateCondition();
                if (res) {
                    if (nestedNode != nullptr) {
                        executeBranches(nestedNode);
                    }
                    branch->executeStatements();
                }
            }
            else {
                finished = true;
                res = branch->evaluateCondition();
                if (res) {
                    branch->executeStatements();
                }
            }

            if (currentBranchNode->nextBranch == nullptr) {
                finished = true;
            }
            else {
                currentBranchNode = currentBranchNode->nextBranch;
            }
        }

    }

public:
    IFStatement(BranchNode* rootNode = nullptr) : root(rootNode) {}

    ~IFStatement() override {
        delete root;
    }

    void execute() override {
        executeBranches(root);
    }
};


class WhileLoop : public Statement {
private:
    Branch* branch;
    int repetitionCount = 0;

    void executeWhileLoop() {
        bool cond = false;
        cond = branch->evaluateCondition();
        int maxValue = 32768;
        while (cond) {

            branch->executeStatements();
            cond = branch->evaluateCondition();
            repetitionCount++;
            if (repetitionCount >= maxValue) { //smallest maximum integer value
                raiseRuntimeException("Maximum number of repetitions exceeded. You may have created an infinite loop here.");
                break;
            }
        }
    }

public:

    WhileLoop() : branch(nullptr), repetitionCount(0) {} //default constructor initialising attributes

    WhileLoop(Branch* branch) {
        this->branch = branch;
        this->repetitionCount = 0;
    }

    ~WhileLoop() override {
        delete branch;
        branch = nullptr;
    }

    void execute() override {
        if (branch != nullptr) {
            executeWhileLoop();
        }
    }

};

class ForLoop : public Statement {
private:
    TokenInstance startValue = { Token::Literal, "0" };
    TokenInstance endValue = { Token::Literal, "0" };
    vector<Statement*> statements;
    int numRepetitions = 0;
    int repetitionCount = 0;

    int getValue(string val) {
        int value = 0;
        Integer Int;
        if (Int.validInt(val)) {
            value = Int.stringToInt(val);
        }
        else {
            raiseRuntimeException("The value, " + val + " is not a valid integer. An integer value was expected.");
        }
        return value;
    }

    void getRepetitions() {
        int start = getValue(startValue.lexeme);
        int end = getValue(endValue.lexeme);
        if (start > end) {
            this->numRepetitions = start - end + 1;
        }
        else {
            this->numRepetitions = end - start + 1;
        }
    }

    void resetCount() {
        repetitionCount = 0;
    }

    void executeStatements() {
        for (auto& stat : statements) {
            stat->execute();
        }
    }

public:

    ForLoop(vector<Statement*> statements, TokenInstance start, TokenInstance end) {
        this->statements = statements;
        this->startValue = start;
        this->endValue = end;
        this->numRepetitions = 0;
        this->repetitionCount = 0;
    }

    ~ForLoop() override {
        for (auto stat : this->statements) delete stat;
    }

    void execute() override {
        resetCount();
        getRepetitions();
        int maxValue = 32768;
        while (repetitionCount <= numRepetitions) {
            executeStatements();
            repetitionCount++;
            if (repetitionCount >= maxValue) {
                raiseRuntimeException("Maximum number of repetitions exceeded. You may have created an infinite loop here.");
                break;
            }
        }
    }
};

class Interpreter; // Forward declaration
extern Interpreter interpreter; // Declare the global variable

class SourceReader {
private:
    string inputFilename = "";
    string outputFilename = "";
    int lineNumber = 0;
    ifstream fileReader;
    string currentLine;

public:
    SourceReader() {

    }
    void alterFileNames(string inputFileName, string outputFileName) {
        this->inputFilename = inputFileName;
        this->outputFilename = outputFileName;
        fileReader.open(this->inputFilename);
        if (!fileReader.is_open()) {
            throw std::runtime_error("Could not open the source file, " + this->inputFilename + ".");
        }
    }

    ~SourceReader() {
        fileReader.close();
    }

    void writeOutput(string outputLine) {
        ofstream fileWriter;
        try {
            fileWriter.open(outputFilename);
            fileWriter.exceptions(std::ofstream::failbit | std::ofstream::badbit); // this enables exceptions on bad file reading
            fileWriter << outputLine << endl;
            fileWriter.close();
        }
        catch (const std::ofstream::failure&) {
            cout << "An error occurred when writing to the file" << endl;
        }
    }

    bool nextLine() {
        //updates the current line and line number
        if (getline(fileReader, currentLine)) {
            lineNumber++;
            return true;
        }
        return false;
    }

    string getLine() {
        return currentLine;
    }

    int getLineNumber() {
        return lineNumber;
    }
};

SourceReader* sourceReader;//Forward declaration

class Interpreter {

private:
    bool validCode = true;
    vector<string>lines; // only used in testing
    int currentLineNum = 0;
    string currentLine;

public:
    Interpreter() {}

    int getLineNum() {
        return this->currentLineNum;
    }

    void setLines(vector<string> lines) {
        this->lines = lines;
    }

    void setCurrentLine(string currentLine) {
        this->currentLine = currentLine;
    }

    void resetLineNum() {
        this->currentLineNum = 0;
    }

    void invalidateCode() {
        this->validCode = false;
    }

    bool isValidCode() {
        return this->validCode;
    }

    vector<TokenInstance> getNewTokens() { // ONLY USED IN TESTING
        vector<TokenInstance> tokens;
        Lexer lexer;
        if (this->currentLineNum < this->lines.size()) {
            //cout << "Tokenizing line " << currentLineNum << ": "; // debugging output statement
            lexer.setLine(this->lines[currentLineNum]);
            tokens = lexer.tokenizeLine();
            //printTokens(tokens); // debugging output statement
            currentLineNum++;
        }
        return tokens;
    }

    vector<TokenInstance> advanceTokens() { //USED IN THE REAL SOLUTION
        vector<TokenInstance> tokens;
        Lexer lexer;
        lexer.setLine(currentLine);
        if (sourceReader->nextLine()) {
            string line = sourceReader->getLine();
            lexer.setLine(line);
            tokens = lexer.tokenizeLine();
        }
        return tokens;
    }

    bool notFinalLine() {
        return this->currentLineNum < this->lines.size();
    }

    string getTokenString(Token t) {
        string tokenStr = "BEGIN";
        switch (t) {
        case Token::ADD:
            tokenStr = "ADD";
            break;
        case Token::AND:
            tokenStr = "AND";
            break;
        case Token::AS:
            tokenStr = "AS";
            break;
        case Token::Boolean:
            tokenStr = "Boolean";
            break;
        case Token::Char:
            tokenStr = "Char";
            break;
        case Token::ClosedBrace:
            tokenStr = "ClosedBrace";
            break;
        case Token::DISPLAY:
            tokenStr = "DISPLAY";
            break;
        case Token::DIV:
            tokenStr = "DIV";
            break;
        case Token::ELSE:
            tokenStr = "ElSE";
            break;
        case Token::ELSEIF:
            tokenStr = "ELSEIF";
            break;
        case Token::END:
            tokenStr = "END";
            break;
        case Token::ENDIF:
            tokenStr = "ENDIF";
            break;
        case Token::ENDWHILE:
            tokenStr = "ENDWHILE";
            break;
        case Token::EqualityOp:
            tokenStr = "EqualityOp";
            break;
        case Token::Float:
            tokenStr = "Float";
            break;
        case Token::FOR:
            tokenStr = "FOR";
            break;
        case Token::IF:
            tokenStr = "IF";
            break;
        case Token::Int:
            tokenStr = "Int";
            break;
        case Token::Literal:
            tokenStr = "Literal";
            break;
        case Token::MULTIPLY:
            tokenStr = "MULTIPLY";
            break;
        case Token::OpenBrace:
            tokenStr = "OpenBrace";
            break;
        case Token::OR:
            tokenStr = "OR";
            break;
        case Token::SET:
            tokenStr = "SET";
            break;
        case Token::String:
            tokenStr = "String";
            break;
        case Token::SUB:
            tokenStr = "SUB";
            break;
        case Token::THEN:
            tokenStr = "THEN";
            break;
        case Token::TO:
            tokenStr = "TO";
            break;
        case Token::Variable:
            tokenStr = "Variable";
            break;
        case Token::NEXT:
            tokenStr = "NEXT";
            break;
        default:
            tokenStr = "WHILE";
            break;
        }
        return tokenStr;
    }

    void printTokens(vector<TokenInstance>tokens) {
        string tokenStr = "BEGIN";
        for (TokenInstance token : tokens) {
            tokenStr = getTokenString(token.type);
            cout << tokenStr + " ";
        }
        cout << endl;
    }

    void printLexemes(vector<TokenInstance> tokens) {
        string lexeme = "BEGIN";
        for (TokenInstance token : tokens) {
            lexeme = token.lexeme;
            cout << lexeme + " ";
        }
        cout << endl;
    }

};

class Parser {
private:
    StatementType statType = StatementType::Undefined;
    Statement* statement; //defining with pointer prevents loss of derived class data
    bool validSyntax = true;
    bool isFinalLine = false;
    ExceptionType exType = ExceptionType::Syntactical;
    vector<TokenInstance> tokens; // Use a dynamic vector instead of a fixed-size array to store the tokens 
    //Tracking number of blank lines skipped
    int blankLineSkips = 0; //track number of times a blank line is skipped
    const int maxBlankLineSkips = 1000;

public:
    Parser() : statement(nullptr) {}

    Parser(vector<TokenInstance>generatedTokens) {
        this->statement = nullptr;
        this->tokens = generatedTokens;
    }

    ~Parser() { //destructor
        delete statement;
    }

    void setNewTokens(vector<TokenInstance>generatedTokens) {
        this->tokens = generatedTokens;
    }

    void generateNewTokens() { // ONLY USED IN TESTING
        //cout << "generating new tokens : line " << interpreter.getLineNum() << endl;
        this->tokens = interpreter.getNewTokens();
        //interpreter.printTokens(this->tokens);
    }

    void advanceTokens() {
        this->tokens = interpreter.advanceTokens();
    }

    void resetLineSkips() {
        blankLineSkips = 0;
    }

    void recordLineSkip() {
        blankLineSkips++;
    }

    bool syntaxValid() {
        return this->validSyntax;
    }

    bool maxSkipsReached() {
        return blankLineSkips > maxBlankLineSkips;
    }

    void checkFinalLine() {
        this->isFinalLine = !interpreter.notFinalLine();
    }

    void parseExpression() {
        Token t = tokens[0].type;
        switch (t) {
        case Token::SET:
            this->statType = StatementType::AssignmentStatement;
            assignmentStatement();
            break;
        case Token::Variable:
            this->statType = StatementType::VariableExpression;
            variableExpression();
            break;
        case Token::DISPLAY:
            this->statType = StatementType::Output;
            outputStatement();
            break;
        case Token::IF: {// note, using curly brackets is essential in cases where variables are being declared that have constructors/destructors
            this->statType = StatementType::IFStatement;
            BranchNode* node = parseIfStatement();
            IFStatement* ifStat = new IFStatement(node);
            this->statement = ifStat;
            break;
        }
        case Token::FOR: {
            this->statType = StatementType::ForLoop;
            ForLoop* forLoop = parseForLoop();
            if (!forLoop) {
                //If parsing failed, do not create a ForLoop object
                this->statement = nullptr;
                break;
            }
            this->statement = forLoop;
            break;
        }
        case Token::WHILE: {
            this->statType = StatementType::WhileLoop;
            Branch* whileBranch = parseWhileLoop();
            if (!whileBranch) {
                // If parsing failed, do not create a WhileLoop statement
                this->statement = nullptr;
                break;
            }
            WhileLoop* whileLoopStatement = new WhileLoop(whileBranch);
            this->statement = whileLoopStatement;
            break;
        }
        case Token::ENDIF:
            //This is only valid if we are inside an IF statement. Outside, it's extra or misplaced.
            raiseException("Extra END IF found outside of an IF statement");
            break;
        case Token::ENDWHILE:
            //This is only valid if we are inside a WHILE loop. Outside, it's extra or misplaced.
            raiseException("Extra END WHILE found outside of a WHILE loop.");
            break;
        case Token::NEXT:
            //This is only valid if we are inside a FOR loop. Outside, it's extra or misplaced.
            raiseException("Extra NEXT found outside of a FOR loop.");
            break;
        default:
            break;
            /*if (t != Token::BEGIN && t != Token::END) {
                this->exType = ExceptionType::Lexical;
                raiseException("Invalid token");
            }
            break;*/
        }
    }

    Statement* getExecutableStatement() {
        return this->statement;
    }

    DataType getType(TokenInstance t) {
        DataType dType = DataType::UnAssigned;
        switch (t.type) {
        case Token::Int:
            dType = DataType::Int;
            break;
        case Token::Float:
            dType = DataType::Float;
            break;
        case Token::Char:
            dType = DataType::Char;
            break;
        case Token::String:
            dType = DataType::String;
            break;
        case Token::Boolean:
            dType = DataType::Boolean;
            break;
        default:
            raiseException(ExceptionType::Lexical, "Invalid data type.");
            break;
        }
        return dType;
    }

    OpType getOpType(TokenInstance t) {
        OpType op = OpType::Other;
        switch (t.type) {
        case Token::ADD:
            op = OpType::ADD;
            break;
        case Token::SUB:
            op = OpType::SUB;
            break;
        case Token::DIV:
            op = OpType::DIV;
            break;
        case Token::MULTIPLY:
            op = OpType::MULTIPLY;
            break;
        default:
            raiseException(ExceptionType::Lexical, "Invalid arithmetic operand.");
            break;
        }
        return op;
    }

    EqualityOperator getEqualityOperator(TokenInstance t) {
        EqualityOperator op = EqualityOperator::EQ;
        string val = t.lexeme;

        if (val == "<>") {
            op = EqualityOperator::NEQ;
        }
        else if (val == "<") {
            op = EqualityOperator::LT;
        }
        else if (val == "<=") {
            op = EqualityOperator::LTET;
        }
        else if (val == ">") {
            op = EqualityOperator::GT;
        }
        else if (val == ">=") {
            op = EqualityOperator::GTET;
        }
        else {
            if (val != "=") {
                raiseException("Invalid equality operator in above condition.");
            }
        }
        return op;
    }
    OpType getArithmeticOperator(TokenInstance t) {
        OpType op = OpType::Other;

        switch (t.type) {
        case Token::ADD:
            op = OpType::ADD;
            break;
        case Token::SUB:
            op = OpType::SUB;
            break;
        case Token::DIV:
            op = OpType::DIV;
            break;
        case Token::MULTIPLY:
            op = OpType::MULTIPLY;
            break;
        default:
            raiseException("Invalid arithmetic operator.");
        }
        return op;
    }

    bool isOperand(Token t) {
        return t == Token::ADD || t == Token::SUB || t == Token::DIV || t == Token::MULTIPLY;
    }

    bool isVariableORValue(Token type) {
        return type == Token::Variable || type == Token::Literal;
    }

    void raiseException(ExceptionType exType, string message) {
        this->validSyntax = false;
        Exception ex = Exception(exType, message);
    }

    void raiseException(string message) {
        this->validSyntax = false;
        Exception ex = Exception(this->exType, message);
    }

    void assignmentStatement() {
        //precondition: tokens[0] == SET
        //EBNF: SET <variable> AS <datatype>
        string exceptionMsg = "Invalid assignment statement.";
        if (tokens.size() != 4 || tokens[2].type != Token::AS) { //check correct structure
            raiseException(exceptionMsg);
        }
        else {
            DataType dType = getType(tokens[3]);
            TokenInstance var = tokens[1];
            //check correct tokens
            if (dType != DataType::UnAssigned && var.type == Token::Variable) {
                this->statement = new Assignment(dType, var.lexeme);
            }
        }
    }

    void outputStatement() {
        //outputs to console
        //EBNF: DISPLAY <variable> | <value> {ADD <variable> | <value>}, noting that Token::Literal defines a <value> and Token::Variable defines a <variable>
        //precondition: tokens[0] == DISPLAY
        bool valid = true;
        bool expectedValue = true; //handles whether or not an ADD statement is expected
        Token type = Token::Literal;
        string exceptionMsg = "Invalid output statement.";
        size_t i = 1;
        OutputStatement* output = new OutputStatement();
        while (i < tokens.size() && valid) {
            type = tokens[i].type;
            if (expectedValue && isVariableORValue(type)) {
                output->addVariable(tokens[i]);
                expectedValue = false;
            }
            else if (!expectedValue && type == Token::ADD) {
                expectedValue = true;
            }
            //error checking: improper token found
            else {
                valid = false;
            }
            i++;
        }
        //error checking: empty DISPLAY statement
        /*if (output.numVariables() == 0) {
            valid = false;
        }*/
        //error checking: expected Literal|Variable
        if (expectedValue) {
            valid = false;
        }

        if (!valid) {
            raiseException(exceptionMsg);
        }
        else {
            this->statement = output;
        }

    }

    bool isCondition(vector<TokenInstance>conditionTokens) {
        //assume conditionTokens.size() == 3
        bool condition = false;
        if (conditionTokens.size() != 3) {
            //cout << "invalid length = " << conditionTokens.size() << endl;
            return false;
        }
        return isVariableORValue(conditionTokens[0].type) && checkEqualityOperand(conditionTokens[1]) && isVariableORValue(conditionTokens[2].type);
    }

    Condition getCondition(vector<TokenInstance> conditionTokens) {
        EqualityOperator op = getEqualityOperator(conditionTokens[1]);
        Condition c = Condition(conditionTokens[0], conditionTokens[2], op);
        return c;
    }

    vector<Condition> getConditions(vector<TokenInstance> tokens) {
        cout << "getConditions: tokens = ";//debugging output statement
        interpreter.printLexemes(tokens); //debugging output statement
        vector<Condition> conditions;
        size_t i = 0;
        while (i + 2 < tokens.size()) {
            vector<TokenInstance> conditionTokens(tokens.begin() + i, tokens.begin() + i + 3);
            //cout << "Checking conditionTokens: "; //debugging output statement
            //interpreter.printTokens(conditionTokens);//debugging output statement
            if (!isCondition(conditionTokens)) {
                raiseException("Invalid condition statement.");
                break;
            }
            conditions.push_back(getCondition(conditionTokens));
            i += 3;
            // Skip linker if present
            if (i < tokens.size() && isConditionLinker(tokens[i])) {
                i += 1;
            }
        }
        return conditions;
    }

    bool isConditionLinker(TokenInstance t) {
        return t.type == Token::OR || t.type == Token::AND;
    }

    bool isBrace(Token t) {
        return t == Token::OpenBrace || t == Token::ClosedBrace;
    }

    bool isOperator(TokenInstance t) {
        return t.type == Token::ADD || t.type == Token::SUB || t.type == Token::MULTIPLY || t.type == Token::DIV;
    }

    string getBranchName(Token t) {
        string name = "IF";
        if (t == Token::ELSEIF) {
            name = "ElSE IF";
        }
        else if (t == Token::ELSE) {
            name = "ELSE";
        }
        return name;
    }

    vector<TokenInstance> getConditionLinkers(vector<TokenInstance> tokens) {
        //cout << "getConditionLinkers: tokens = ";//debugging output statement
        //interpreter.printTokens(tokens);//debugging output statement
        vector<TokenInstance> conditionLinkers;
        size_t i = 3;
        while (i < tokens.size()) {
            cout << "Checking for linker at i=" << i << ": [" << tokens[i].lexeme << "]" << endl; //debugging output statement
            if (isConditionLinker(tokens[i])) {
                conditionLinkers.push_back(tokens[i]);
                i += 4; // Skip linker and next condition (3 tokens)
            }
            else if (i == tokens.size()) {
                //End of tokens, no linker expected
                break;
            }
            else {
                // If not a linker where expected, raise exception
                raiseException("Expected either an AND or an OR operator.");
                break;
            }
        }
        return conditionLinkers;
    }

    ConditionalStatement parseConditionalStatement(vector<TokenInstance> tokens) {
        //conditional statement used in iteration and branching
        //precondition: <condition> expected
        //postcondition: <condition> valid or invalid

        /*The EBNF is:
        <variable>|<value> <equality operator> <OTHER variable><value>{AND | OR <variable>|<value> <equality operator> <OTHER variable>|<value>}
        Which simplifies to:
        <condition> {AND|OR <condition>}
        Where <condition> is:
        <variable>|<value><equality operator><variable|value>
        */
        vector<Condition> conditions = getConditions(tokens);
        vector<TokenInstance>conditionLinkers = getConditionLinkers(tokens);
        ConditionalStatement conditionStatement(conditions, conditionLinkers);
        //cout <<"parseConditionalStatement: tokens = "; //debugging output statement
        //interpreter.printTokens(tokens); //debugging output statement
        //IF EVERYTHING WORKS OUT, CREATE THE ConditionalStatement() object
        return conditionStatement;
    }

    bool checkEqualityOperand(TokenInstance t) {
        bool valid = false;
        Token type = t.type;
        string value = t.lexeme;
        if (type == Token::EqualityOp) {
            if (value == "=" || value == "<>" || value == ">" || value == ">=" || value == "<" || value == "<=") {
                valid = true;
            }
        }
        return valid;
    }

    Branch* parseWHILEBranch(const vector<TokenInstance>& headerTokens) {
        checkFinalLine();
        if (tokens.empty() && isFinalLine) {
            return nullptr;
        }

        cout << "parseWHILEBranch: headerTokens = ";
        interpreter.printLexemes(headerTokens);
        ConditionalStatement cond;
        size_t start = 1;
        size_t end = headerTokens.size();
        vector<TokenInstance> conditionTokens(headerTokens.begin() + start, headerTokens.begin() + end);
        cond = parseConditionalStatement(conditionTokens);
        vector<Statement*> statements;
        bool done = false;
        Token t = Token::Literal;

        // Advance to first line of the loop body
        //generateNewTokens();
        advanceTokens();
        checkFinalLine();

        resetLineSkips();
        while (!done && !isFinalLine && validSyntax && !maxSkipsReached()) {
            if (tokens.empty()) {
                //generateNewTokens();
                advanceTokens();
                checkFinalLine();
                recordLineSkip();
                //check for final line if an empty line has been found to prevent an infinite loop from occurring
                if (tokens.empty()) {
                    if (isFinalLine) break;
                    else continue;
                }
            }
            t = tokens[0].type;
            if (t == Token::IF) {
                BranchNode* nestedIfNode = parseIfStatement();
                if (!nestedIfNode) {
                    return nullptr;
                }
                IFStatement* nestedIfStatement = new IFStatement(nestedIfNode);
                statements.push_back(nestedIfStatement);
                statement = nullptr;
                //generateNewTokens();
                advanceTokens();
            }
            else if (t == Token::WHILE) {
                Branch* nestedWhile = parseWhileLoop();
                if (!nestedWhile) {
                    return nullptr;
                }
                WhileLoop* whileLoopStatement = new WhileLoop(nestedWhile);
                statements.push_back(whileLoopStatement);
                statement = nullptr;
                //generateNewTokens();
                advanceTokens();
            }
            else if (t == Token::ENDWHILE) {
                done = true;
                break; // Do NOT call generateNewTokens() here!
            }
            else {
                parseExpression();
                if (statement) statements.push_back(statement);
                statement = nullptr;
                //generateNewTokens();
                advanceTokens();
            }
            cout << "Printing next line: ";
            interpreter.printLexemes(tokens);
            cout << "which is tokenized to become: ";
            interpreter.printTokens(tokens);
            checkFinalLine();
        }

        if (!done) {
            if (!tokens.empty() && tokens[0].type == Token::ENDWHILE) {
                done = true;
            }
            else { //if we are at the end of the file, or in any other case, it's a missing END WHILE
                raiseException("Missing an END WHILE for the above WHILE loop.");
                return nullptr;
            }
        }
        //Only advance if END WHILE is found
        if (!done) {
            raiseException("Missing an END WHILE for the above WHILE loop.");
            return nullptr;
        }

        if (!validSyntax) return nullptr;
        return new Branch(cond, statements);
    }


    Branch* parseIFBranch(bool isElseBranch, const vector<TokenInstance>& headerTokens, BranchNode* parentNode = nullptr) {
        //cout << "parseBranch: next token is " << interpreter.getTokenString(tokens[0].type) << " (" << tokens[0].lexeme << ")" << endl; //debugging output statement
        ConditionalStatement cond;
        if (!isElseBranch) {
            size_t start = 1;
            size_t end = headerTokens.size();
            if (headerTokens.back().type == Token::THEN) {
                end--;
                vector<TokenInstance> condTokens(headerTokens.begin() + start, headerTokens.begin() + end);
                cond = parseConditionalStatement(condTokens);
            }
            else {
                raiseException("Expected a THEN keyword.");
            }
        }
        vector<Statement*> statements;
        bool done = false;
        resetLineSkips();
        while (!done && !isFinalLine && validSyntax && !maxSkipsReached()) {
            if (tokens.empty()) {
                //generateNewTokens();
                advanceTokens();
                recordLineSkip();
                if (tokens.empty()) continue;
            }
            //cout << "parseIfStatement: lookahead token is " << interpreter.getTokenString(tokens[0].type) << " (" << tokens[0].lexeme << ")" << endl; //debugging output statement
            Token t = tokens[0].type;
            if (t == Token::IF) {
                BranchNode* nestedIfNode = parseIfStatement();
                IFStatement* nestedIfStatement = new IFStatement(nestedIfNode);
                statements.push_back(nestedIfStatement);
                statement = nullptr;
                //generateNewTokens();
                advanceTokens();
            }
            else if (t == Token::ELSEIF || t == Token::ELSE || t == Token::ENDIF) {
                done = true;
            }
            else {
                parseExpression();
                if (statement) statements.push_back(statement);
                statement = nullptr;
                //generateNewTokens();
                advanceTokens();
            }
            checkFinalLine();
        }

        return new Branch(cond, statements);
    }


    BranchNode* parseIfStatement() {
        //cout << "parseIfStatement: lookahead token is " << interpreter.getTokenString(tokens[0].type) << " (" << tokens[0].lexeme << ")" << endl; //debugging output statement
        bool isElse = tokens[0].type == Token::ELSE;
        vector<TokenInstance> headerTokens = tokens;
        //generateNewTokens(); // <-- Advance to the first line of the branch body!
        advanceTokens();
        BranchNode* node = new BranchNode();
        node->isElseBranch = isElse;
        node->current = parseIFBranch(isElse, headerTokens, node);

        // Now look ahead for ELSE IF, ELSE, or END IF
        BranchNode* lastNode = node;
        bool done = false;
        while (!isFinalLine && !done && validSyntax) {
            if (tokens.empty()) {
                //generateNewTokens();
                advanceTokens();
                if (tokens.empty()) continue;
            }
            //cout << "parseIfStatement: lookahead token is " << interpreter.getTokenString(tokens[0].type) << " (" << tokens[0].lexeme << ")" << endl; //debugging output statement
            Token t = tokens[0].type;

            if (t == Token::ELSEIF || t == Token::ELSE) {
                lastNode->nextBranch = parseIfStatement();
                done = true;
            }
            else if (t == Token::ENDIF) {
                done = true;
            }
            else {
                string name = getBranchName(t);
                raiseException("Unexpected token found after " + name + " branch.");
            }
            // Only advance tokens if you expect to process the next branch
            if (!done) {
                //generateNewTokens();
                advanceTokens();
            }
            checkFinalLine();
        }

        if (!done && isFinalLine) {
            if (!tokens.empty() && tokens[0].type == Token::ENDIF) {
                done = true;
            }
            else {
                raiseException("Missing an END IF for the above IF statement.");
            }
        }

        return node;
    }

    vector<TokenInstance> parseForStatement(vector<TokenInstance> headerTokens) {
        //Expect headerTokens = FOR <integer variable> = <start number>|<variable> TO <end number>|<variable>
        vector<TokenInstance> startEndTokens;
        bool validForStat = false;
        if (tokens.size() == 6) {
            if (headerTokens[1].type == Token::Variable && headerTokens[4].type == Token::TO && headerTokens[2].lexeme == "=") { //checking initial variable and terminals in FOR statement (if the first token is known to be a FOR token)
                if (isVariableORValue(headerTokens[3].type) && isVariableORValue(headerTokens[5].type)) { //checking the two non-terminals in the FOR statement
                    validForStat = true;
                }
            }
        }

        if (validForStat) {
            startEndTokens.push_back(headerTokens[3]);
            startEndTokens.push_back(headerTokens[5]);
        }
        else {
            raiseException("Invalid tokens for FOR loop statement.");

        }
        return startEndTokens;
    }

    ForLoop* parseForLoop() {
        //for-loop
        /*EBNF: FOR <integer variable> = <start number>|<variable> TO <end number>|<variable>
                      {<statements>(may include more loops or if-statements)}
                NEXT
        */
        //precondition: tokens[0] == FOR
        //postcondition: NEXT expected
        vector<Statement*> statements;
        TokenInstance startValue = { Token::Literal, "0" };
        TokenInstance endValue = { Token::Literal, "0" };

        vector<TokenInstance>headerTokens = tokens;

        vector<TokenInstance> startEndTokens = parseForStatement(headerTokens);
        if (startEndTokens.size() == 2) {
            startValue = startEndTokens[0];
            endValue = startEndTokens[1];
        }
        //generateNewTokens();
        advanceTokens();
        resetLineSkips();
        bool done = false;
        Token t = Token::Literal;

        while (!isFinalLine && !done && validSyntax && !maxSkipsReached()) {
            if (tokens.empty()) {
                //generateNewTokens();
                advanceTokens();
                if (tokens.empty()) continue;
            }
            t = tokens[0].type;
            checkFinalLine();
            if (t == Token::FOR) {
                ForLoop* forLoop = parseForLoop();
                statements.push_back(forLoop);
                this->statement = nullptr;
                //generateNewTokens();
                advanceTokens();
            }
            else if (t == Token::NEXT) {
                done = true;
                //cout << "Found NEXT token, breaking out of FOR loop body.\n"; //debugging output statement
            }
            else {
                parseExpression();
                statements.push_back(this->statement);
                this->statement = nullptr;
                //generateNewTokens();
                advanceTokens();
            }
        }
        //cout << "After FOR loop body, done = " << done << endl; //debugging output statement
        if (!done) {
            raiseException("Missing a NEXT statement for the above FOR loop.");
            return nullptr;
        }
        return new ForLoop(statements, startValue, endValue);
    }

    Branch* parseWhileLoop() {
        checkFinalLine(); //defensive guard to prevent infinite recursion
        if (tokens.empty() && isFinalLine) {
            return nullptr;
        }
        //while-loop
        //EBNF: WHILE <condition> 
        //precondition: tokens[0] == WHILE
        //postcondition: ENDWHILE expected
        vector<TokenInstance> headerTokens = tokens;
        Branch* branch = parseWHILEBranch(headerTokens);
        return branch;
    }

    void variableExpression() {
        //put values into variables
        //use mathematicalExpression() for type int and float
        //precondition: tokens[0].type == <variable>
        //EBNF: <variable> = <variable>|<value> {<operator> <variable>|<value>}
        //The actual complexities of type concatenation will be handled by the Runtime Analyser
        size_t size = tokens.size();
        bool validExpression = true;
        string msg = "Invalid variable expression.";
        if (size < 3) {
            validExpression = false;
        }
        else if (tokens[1].lexeme != "=") {
            validExpression = false;
            msg = "Invalid equality operator.";
        }
        vector<TokenInstance> exprTokens;
        size_t idx = 2;
        bool valueExpected = true;
        bool opExpected = false;
        Token type;
        bool needToEvaluate = size > 3;
        int braceDepth = 0;
        //cout << "there are " << size << " tokens in the expression " << endl; // debugging output statement
        while (idx < size && validExpression) {
            type = tokens[idx].type;

            if (valueExpected) {
                if (isVariableORValue(type)) {
                    exprTokens.push_back(tokens[idx]);
                    valueExpected = false;
                    opExpected = true;
                }
                else if (type == Token::OpenBrace) {
                    exprTokens.push_back(tokens[idx]);
                    braceDepth++;
                    // A new expression starts inside the bracket
                }
                else {
                    validExpression = false;
                    msg = "A variable, value, or opening brace '(' is expected.";
                }
            }
            else if (opExpected) {
                if (isOperand(type)) {
                    exprTokens.push_back(tokens[idx]);
                    opExpected = false;
                    valueExpected = true;
                }
                else if (type == Token::ClosedBrace) {
                    if (braceDepth == 0) {
                        validExpression = false;
                        msg = "Unmatched closing parenthesis ')'.";
                    }
                    else {
                        exprTokens.push_back(tokens[idx]);
                        braceDepth--;
                        // we still expect an operator or closing parenthesis next
                    }
                }
                else {
                    validExpression = false;
                    msg = "An operator or closing brace ')' is expected.";
                }
            }
            else {
                validExpression = false;
            }
            idx++;
        }

        if (braceDepth != 0) {
            msg = "Unmatched parentheses in the above expression. Was expecting a ')'.";
            validExpression = false;
        }

        if (validExpression) {
            TokenInstance lhsVar = tokens[0];
            this->statement = new VariableExpression(exprTokens, lhsVar, needToEvaluate);
        }
        else {
            if (braceDepth == 0) {
                if (valueExpected) {
                    msg = "A variable or value is expected.";
                }

                else if (opExpected) {
                    msg = "An arithmetic operator is expected.";
                }
            }
            raiseException(msg);
            this->statement = nullptr;
        }
    }

};

class Test {
public:

    void manualArithmetic() { // passes (works)
        //The goal here is to create a basic AST for arithmetic operations so we can learn about the process of building an abstract syntax tree or AST.
        //The role of creating the AST will eventually be done by a parser.

        //Build AST for nested expression below
        Expression* expr = new BinaryOperation(
            OpType::MULTIPLY,
            new BinaryOperation(
                OpType::DIV,
                new BinaryOperation(
                    OpType::ADD,
                    new Literal(10),
                    new Literal(2)
                ),
                new Literal(6)
            ),
            new Literal(5)
        );
        cout << "Result of 5 * ( (10 + 2) / 6 ) = " << expr->evaluate().intValue;
        delete expr;
        /*In C++ memory is manually allocated. Therefore, objects must be deleted once they go out of scope to avoid memory leaks. Memory leaks are errors that occur
        when a program manually allocates memory but fails to free memory when it is no longer needed. Preventing memory leaks improves performance, prevents
        unpredictable behaviour, and avoids memory clashes.*/
    }

    void outputStatement_valid() { // passes (works)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::DISPLAY, "DISPLAY"},
            TokenInstance {Token::Literal, "name: "},
            TokenInstance {Token::ADD, "+"},
            TokenInstance {Token::Variable, "name"}
        };

        Parser p(tokens);
        p.outputStatement();
    }

    void outputStatement_invalidADD() { //passes (error detected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::DISPLAY, "DISPLAY"},
            TokenInstance {Token::Literal, "name: "},
            TokenInstance {Token::ADD, "+"},
        };
        Parser p(tokens);
        p.outputStatement();
    }

    void outputStatement_invalidRandomToken() { //passes (error detected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::DISPLAY, "DISPLAY"},
            TokenInstance {Token::Literal, "name: "},
            TokenInstance {Token::DIV, "/"},
            TokenInstance {Token::Variable, "name"}
        };
        Parser p(tokens);
        p.outputStatement();
    }
    void outputStatement_invalidLength() { //passes (error detected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::DISPLAY, "DISPLAY"}
        };
        Parser p(tokens);
        p.outputStatement();
    }

    void assignmentStatement_valid() { // passes (works)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::SET, "SET"},
            TokenInstance {Token::Variable, "var"},
            TokenInstance {Token::AS, "AS"},
            TokenInstance {Token::Int, "int"}
        };
        Parser p(tokens);
        p.assignmentStatement();
    }

    void assignmentStatement_invalidDatatype() { // passes (error detected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::SET, "SET"},
            TokenInstance {Token::Variable, "var"},
            TokenInstance {Token::AS, "AS"},
            TokenInstance {Token::Variable, "Double"} //we assume here that the tokenizer interperets "Double" as a variable because there is no Double datatype in our language
        };
        Parser p(tokens);
        p.assignmentStatement();
    }

    void assignmentStatement_invalidLength() { // passes (error detected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::SET, "SET"},
            TokenInstance {Token::Variable, "var"},
        };
        Parser p(tokens);
        p.assignmentStatement();
    }

    void assignmentStatement_invalidRandomTokens() { // passes (error detected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::SET, "SET"},
            TokenInstance {Token::Variable, "var"},
            TokenInstance {Token::AS, "AS"},
            TokenInstance {Token::Int, "int"},
            TokenInstance {Token::EqualityOp, "="},
            TokenInstance {Token::Literal, "69"}
            //SET var AS Integer = 69
            //This is an invalid expression in the language since defining variables and assigning values are two different things
        };
        Parser p(tokens);
        p.assignmentStatement();
    }

    void conditionalStatement_validShort() { // passes (works)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::Variable, "var1"},
            TokenInstance {Token::EqualityOp, ">"},
            TokenInstance {Token::Literal, "5"}
        };
        Parser p(tokens);
        ConditionalStatement c = p.parseConditionalStatement(tokens);
    }
    void conditionalStatement_validLonger() { // passes (works)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::Variable, "var1"},
            TokenInstance {Token::EqualityOp, ">"},
            TokenInstance {Token::Literal, "5"},
            TokenInstance {Token::AND, "AND"},
            TokenInstance {Token::Variable, "var1"},
            TokenInstance {Token::EqualityOp, "<"},
            TokenInstance {Token::Variable, "var2"},
            TokenInstance {Token::OR, "OR"},
            TokenInstance {Token::Variable, "var2"},
            TokenInstance {Token::EqualityOp, "="},
            TokenInstance {Token::Literal, "-1"}
        };
        Parser p(tokens);
        ConditionalStatement c = p.parseConditionalStatement(tokens);
    }

    void conditionalStatement_invalidToken() { // passes (error detected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::Variable, "var1"},
            TokenInstance {Token::EqualityOp, ">"},
            TokenInstance {Token::Literal, "5"},
            TokenInstance {Token::ADD, "+"},
            TokenInstance {Token::Variable, "var1"},
            TokenInstance {Token::EqualityOp, "<"},
            TokenInstance {Token::Variable, "var2"},
        };
        Parser p(tokens);
        ConditionalStatement c = p.parseConditionalStatement(tokens);
    }
    void conditionalStatement_invalid_expectedCondition() { // passes (error detected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::Variable, "var1"},
            TokenInstance {Token::EqualityOp, ">"},
            TokenInstance {Token::Literal, "5"},
            TokenInstance {Token::AND, "AND"},
            TokenInstance {Token::Variable, "var1"},
            TokenInstance {Token::EqualityOp, "<"},
            TokenInstance {Token::Variable, "var2"},
            TokenInstance {Token::OR, "OR"}
        };
        Parser p(tokens);
        ConditionalStatement c = p.parseConditionalStatement(tokens);
    }

    void conditionalStatement_invalid_expectedLinker() { // passes (error detected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::Variable, "var1"},
            TokenInstance {Token::EqualityOp, ">"},
            TokenInstance {Token::Literal, "5"},
            TokenInstance {Token::AND, "AND"},
            TokenInstance {Token::Variable, "var1"},
            TokenInstance {Token::EqualityOp, "<"},
            TokenInstance {Token::Variable, "var2"},
            TokenInstance {Token::Variable, "var2"},
            TokenInstance {Token::EqualityOp, "="},
            TokenInstance {Token::Literal, "-1"}
        };
        Parser p(tokens);
        ConditionalStatement c = p.parseConditionalStatement(tokens);
    }

    void variableExpression_validShort() { // passes (works)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::Variable, "var"},
            TokenInstance {Token::EqualityOp, "="},
            TokenInstance {Token::Literal, "0"}
        };
        Parser p(tokens);
        p.variableExpression();
    }

    void variableExpression_validLonger() { // passes (works)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::Variable, "var"},
            TokenInstance {Token::EqualityOp, "="},
            TokenInstance {Token::Variable, "x"},
            TokenInstance {Token::ADD, "+"},
            TokenInstance {Token::Variable, "y"},
            TokenInstance {Token::DIV, "/"},
            TokenInstance {Token::Literal, "1.5"}
        };
        Parser p(tokens);
        p.variableExpression();
    }

    void variableExpression_invalidLength() { // passes (error detected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::Variable, "var"},
            TokenInstance {Token::Literal, "="}
        };
        Parser p(tokens);
        p.variableExpression();
    }

    void variableExpression_invalidEqualityOp() { // passes (error detected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::Variable, "var"},
            TokenInstance {Token::EqualityOp, ">"},
            TokenInstance {Token::Literal, "0"}
        };
        Parser p(tokens);
        p.variableExpression();
    }

    void variableExpression_invalid_expectedOperand() { // passes (error detected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::Variable, "var"},
            TokenInstance {Token::EqualityOp, "="},
            TokenInstance {Token::Variable, "x"},
            TokenInstance {Token::ADD, "+"},
            TokenInstance {Token::Variable, "y"},
            TokenInstance {Token::Literal, "1.5"}
        };
        Parser p(tokens);
        p.variableExpression();
    }

    void variableExpression_invalid_expectedValue() { // passes (error detected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::Variable, "var"},
            TokenInstance {Token::EqualityOp, "="},
            TokenInstance {Token::Variable, "x"},
            TokenInstance {Token::ADD, "+"},
            TokenInstance {Token::DIV, "/"},
            TokenInstance {Token::Literal, "1.5"}
        };
        Parser p(tokens);
        p.variableExpression();
    }

    void variableExpression_invalidOperand() { // passes (error detected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::Variable, "var"},
            TokenInstance {Token::EqualityOp, "="},
            TokenInstance {Token::Variable, "x"},
            TokenInstance {Token::EqualityOp, "="},
            TokenInstance {Token::Literal, "1.5"}
        };
        Parser p(tokens);
        p.variableExpression();
    }

    void testInterpreter(vector<string> lines) {
        interpreter.setLines(lines);
        interpreter.resetLineNum();
        vector<TokenInstance> tokens = interpreter.getNewTokens();
        //cout << "testInterpreter: first tokens = "; //debugging output statement
        //interpreter.printLexemes(tokens); //debugging output statement
        Parser p(tokens);

        // Parse all lines/statements, not just the first
        while (p.syntaxValid() && !tokens.empty() && interpreter.isValidCode()) {
            interpreter.printTokens(tokens);
            p.parseExpression();
            if (p.syntaxValid()) {
                Statement* statement = p.getExecutableStatement();
                if (statement) {
                    statement->execute();
                }
                delete statement;
                tokens = interpreter.getNewTokens();
                p.setNewTokens(tokens);
            }
            else {
                break;
            }
        }

    }

    void ifStatement_valid_Short() { // passes (works)
        vector<string> lines = {
            "IF var > 1 THEN",
                "var = var + 1",
            "END IF" };
        testInterpreter(lines);
    }
    void ifStatement_valid_Longer() { // passes (works)
        vector<string> lines = {
            "IF var > 1 THEN",
                "var = var + 1",
            "ELSE IF var > 0 AND var < 1 THEN",
                "var = var - 1",
            "ELSE",
                "var = 0",
            "END IF" };
        testInterpreter(lines);
    }

    void ifStatement_valid_Longer_MissingStatement() { // passes (works)
        vector<string> lines = {
            "IF var > 1 THEN",
                "var = var + 1",
            "ELSE IF var > 0 AND var < 1 THEN",
            "ELSE",
            "END IF" };
        testInterpreter(lines);
    }

    void ifStatement_valid_Longer_Nested() { // passes (works)
        vector<string> lines = {
            "IF var > 1 THEN",
                "var = var + 1",
            "ELSE IF var > 0 AND var < 1 THEN",
                "IF x = var THEN",
                    "var = x - 1",
                "END IF",
            "ELSE",
                "var = 0",
            "END IF" };
        testInterpreter(lines);
    }

    void ifStatement_invalid_Short_InvalidEqualityOperand() { // passes (error detected)
        vector<string> lines = {
            "IF var is 1 THEN",
                "var = var + 1",
            "END IF" };
        testInterpreter(lines);
    }

    void ifStatement_invalid_Longer_InvalidEqualityOperand() { // passes (error detected)
        vector<string> lines = {
            "IF var > 1 THEN",
                "var = var + 1",
            "ELSE IF var > 0 AND var != 1 THEN",
                "var = var - 1",
            "ELSE",
                "var = 0",
            "END IF" };
        testInterpreter(lines);
    }

    void ifStatement_invalid_Longer_Nested_InvalidEqualityOperand() { // passes (error detected)
        vector<string> lines = {
            "IF var > 1 THEN",
                "var = var + 1",
            "ELSE IF var > 0 AND var < 1 THEN",
                "IF x != var THEN",
                    "var = x - 1",
                "END IF",
            "ELSE",
                "var = 0",
            "END IF" };
        testInterpreter(lines);
    }

    void ifStatement_invalid_Short_MissingENDIF() { // passes (error detected)
        vector<string> lines = {
            "IF var is 1 THEN",
                "var = var + 1" };
        testInterpreter(lines);
    }

    void ifStatement_invalid_Longer_MissingENDIF() { // passes (error detected)
        vector<string> lines = {
            "IF var > 1 THEN",
                "var = var + 1",
            "ELSE IF var > 0 AND var < 1 THEN",
                "var = var - 1",
            "ELSE",
                "var = 0" };
        testInterpreter(lines);
    }

    void ifStatement_invalid_Nested_MissingENDIF() { // passes (error detected)
        vector<string> lines = {
            "IF var > 1 THEN",
                "var = var + 1",
            "ELSE IF var > 0 AND var < 1 THEN",
                "IF x = var THEN",
                    "var = x - 1",
            "ELSE",
                "var = 0",
            "END IF" };
        testInterpreter(lines);
    }

    void ifStatement_invalid_Short_MissingTHEN() { // passes (error detected)
        vector<string> lines = {
            "IF var is 1 THEN",
                "var = var + 1",
            "END IF" };
        testInterpreter(lines);
    }

    void ifStatement_invalid_Longer_MissingTHEN() { // passes (error detected)
        vector<string> lines = {
            "IF var > 1 THEN",
                "var = var + 1",
            "ELSE IF var > 0 AND var < 1",
                "var = var - 1",
            "ELSE",
                "var = 0",
            "END IF" };
        testInterpreter(lines);
    }

    void ifStatement_invalid_Nested_MissingTHEN() { // passes (error detected)
        vector<string> lines = {
            "IF var > 1 THEN",
                "var = var + 1",
            "ELSE IF var > 0 AND var < 1 THEN",
                "IF x = var",
                    "var = x - 1",
                "END IF",
            "ELSE",
                "var = 0",
            "END IF" };
        testInterpreter(lines);
    }
    //N.B. Change these conditions to include negative numbers
    void whileLoop_valid_Short() { // passes (works as expected)
        vector<string> lines = {
        "WHILE x < 0",
            "x = x - 1",
            "y = y + 2",
        "END WHILE"
        };
        testInterpreter(lines);
    }

    void whileLoop_valid_Long() { // passes (works as expected)
        vector<string> lines = {
        "WHILE x > 0",
            "x = x - 1",
            "IF var > 1 AND var < 2 THEN",
                "var = var + 1",
            "ELSE IF var > 0 AND var < 1 THEN",
                "var = var - 1",
            "ELSE",
                "var = 0",
            "END IF",
        "END WHILE" };
        testInterpreter(lines);
    }

    void whileLoop_valid_MissingStatements() { // passes (works as expected)
        vector<string> lines = {
        "WHILE x = 1",
        "END WHILE" };
        testInterpreter(lines);
    }

    void whileLoop_valid_nestedLoop() { // passes (works as expected)
        vector<string> lines = {
        "WHILE x > 1",
           "x = x - 1",
           "IF var > 1 THEN",
               "var = var - 1",
           "END IF",
           "WHILE var < 10",
                "var = var + 2",
           "END WHILE",
        "END WHILE" };
        testInterpreter(lines);
    }

    void whileLoop_valid_BlankLines() { // passes (works as expected)
        vector<string> lines = {
            "WHILE x < 10",
            "",
            "",
            "END WHILE"
        };
        testInterpreter(lines);
    }

    void whileLoop_invalid_ExtraENDWHILE() { // passes (error detected)
        vector<string> lines = {
            "WHILE x > 0",
            "x = x - 1",
            "END WHILE",
            "END WHILE"
        };
        testInterpreter(lines);
    }

    void whileLoop_invalid_Nested_ExtraENDWHILE() { // passes (error detected)
        vector<string> lines = {
        "WHILE x > 1",
           "x = x - 1",
           "IF var > 1 THEN",
               "var = var - 1",
           "END IF",
           "WHILE var < 10",
                "var = var + 2",
           "END WHILE",
        "END WHILE",
        "END WHILE" };
        testInterpreter(lines);
    }

    void whileLoop_invalid_InvalidEqualityOperator() { // passes (error detected)
        vector<string> lines = {
        "WHILE x != -1",
            "x = x - 1",
            "IF var > 1 THEN",
                "var = var + 1",
            "ELSE IF var > 0 AND var < 1",
                "var = var - 1",
            "ELSE",
                "var = 0",
            "END IF",
        "END WHILE" };
        testInterpreter(lines);
    }

    void whileLoop_invalid_InvalidRandomToken() { // passes (error detected)
        vector<string> lines = {
        "WHILE x 1 = 10",
            "x = x - 1",
            "IF var > 1 THEN",
                "var = var + 1",
            "ELSE IF var > 0 AND var < 1",
                "var = var - 1",
            "ELSE",
                "var = 0",
            "END IF",
        "END WHILE" };
        testInterpreter(lines);
    }

    void whileLoop_invalid_MissingENDWHILE() { // passes (error detected)
        vector<string> lines = {
        "WHILE x > 1",
            "x = x - 1",
            "IF var > 10 THEN",
                "var = var + 1",
            "ELSE IF var > 5 AND var < 10 THEN",
                "var = var - 1",
            "ELSE",
                "var = x",
            "END IF"
        };
        testInterpreter(lines);
    }

    void whileLoop_invalid_Nested_MissingENDWHILE() { // passes (error detected)
        vector<string> lines = {
        "WHILE x > 1",
            "x = x - 1",
            "IF var > 10 THEN",
            "var = var + 1",
            "ELSE IF var > 5 AND var < 10 THEN",
            "var = var - 1",
            "ELSE",
            "var = x",
            "END IF",
            "WHILE var > 0",
                "var = x - 2",
                "y = var * 2",
            "END WHILE"
        };
        testInterpreter(lines);
    }

    void forLoop_valid_Short() { // passes (works as expected)
        vector<string> lines = {
        "FOR i = 1 TO 5",
        "x = x * i",
        "y = 1 / x",
        "NEXT"
        };
        testInterpreter(lines);
    }

    void forLoop_valid_Long() { // passes (works as expected)
        vector<string> lines = {
        "FOR i = a TO 5",
            "x = x * i",
            "IF x > a THEN",
            "x = x - y",
            "ELSE",
            "a = a - 1",
            "END IF",
            "y = x",
            "NEXT"
        };
        testInterpreter(lines);
    }

    void forLoop_valid_Nested() { // passes (works as expected)
        vector<string> lines = {
        "FOR i = a TO b",
            "x = x * i",
            "WHILE x < 100",
                "IF x > a THEN",
                    "x = x - y",
                "END IF",
            "END WHILE",
            "FOR c = 1 TO 10",
                "x = x + c",
            "NEXT",
            "y = x",
        "NEXT"
        };
        testInterpreter(lines);
    }

    void forLoop_valid_MissingStatements() { // passes (works as expected)
        vector<string> lines = {
        "FOR x = 1 TO length",
            "a = length * breadth",
            "FOR y = 1 TO 10",
            "NEXT",
        "NEXT"
        };
        testInterpreter(lines);
    }

    void forLoop_valid_BlankLines() { // passes (works as expected)
        vector<string> lines = {
            "FOR y = 10 TO 1",
            " ",
            "  ",
            "",
            "NEXT",
        };
        testInterpreter(lines);
    }

    void forLoop_invalid_Short_lowercaseFOR() { // passes (error detected)
        vector<string> lines = {
        "for i = 1 TO 100",
        "x = x + i",
        "y = 1 / x",
        "NEXT"
        };
        testInterpreter(lines);
    }

    void forLoop_invalid_Nested_lowercaseFOR() { // passes (error detected)
        vector<string> lines = {
        "FOR x = 1 TO length",
            "a = length * breadth",
            "for y = 1 TO 10",
            "NEXT",
        "NEXT"
        };
        testInterpreter(lines);
    }

    void forLoop_invalid_Short_invalidForStatement() { // passes (error detected)
        vector<string> lines = {
        "FOR i = 1 TO",
        "x = x * i",
        "y = 1 / x",
        "NEXT"
        };
        testInterpreter(lines);
    }

    void forLoop_invalid_Long_invalidForStatement() { // passes (error detected)
        vector<string> lines = {
        "FOR 1 = a TO b",
            "x = x * i",
            "WHILE x < 100"
                "IF x > a THEN",
                    "x = x - y",
                "END IF",
            "END WHILE",
        "NEXT"
        };
        testInterpreter(lines);
    }

    void forLoop_invalid_Nested_invalidForStatement() { // passes (error detected)
        vector<string> lines = {
        "FOR i = a TO b",
            "WHILE x > 0",
                "x = x - y",
            "END WHILE",
            "FOR c <= 10",
                "x = x + c",
            "NEXT",
            "y = x",
        "NEXT"
        };
        testInterpreter(lines);
    }

    void forLoop_invalid_Short_InvalidRandomToken() { // passes (error detected)
        vector<string> lines = {
        "FOR c = a TO b + 1",
            "x = x * i",
        "NEXT"
        };
        testInterpreter(lines);
    }

    void forLoop_invalid_Nested_InvalidRandomToken() { // passes (error detected)
        vector<string> lines = {
        "FOR i = a TO b",
            "FOR c = TO 10",
                "x = x + c",
            "NEXT",
            "END IF",
            "y = x",
        "NEXT"
        };
        testInterpreter(lines);
    }

    void forLoop_invalid_Short_MissingNEXT() { // passes (error detected)
        vector<string> lines = {
        "FOR c = a TO b",
            "x = x * i",
            "y = x + y / c"
        };
        testInterpreter(lines);
    }

    void forLoop_invalid_Nested_MissingNEXT() { // passes (error detected)
        vector<string> lines = {
            "FOR i = a TO b",
                "WHILE x > 0",
                    "x = x - y",
                    "FOR c <= 10",
                        "x = x + c",
                        "y = x",
                "END WHILE",
            "NEXT"
        };
        testInterpreter(lines);
    }

    void forLoop_invalid_Short_ExtraNEXT() { // passes (error detected)
        vector<string> lines = {
        "FOR c = a TO b",
            "x = x * i",
        "NEXT",
        "NEXT"
        };
        testInterpreter(lines);
    }

    void forLoop_invalid_Nested_ExtraNEXT() { // passes (error detected)
        vector<string> lines = {
            "FOR i = a TO b",
                "FOR c = b TO 10",
                        "x = x + c",
                "NEXT",
                "NEXT",
                "NEXT",
                "y = x",
            "NEXT"
        };
        testInterpreter(lines);
    }

    void mathematicalExpression_valid_Short() { // passes (works as expected)
        vector<string> lines = {
        "SET x AS int",
        "SET y AS int",
        "x = 10",
        "y = 20",
        "x = x + y",
        "DISPLAY x" };
        testInterpreter(lines);
    }

    void mathematicalExpression_valid_MultipleOperands() { // passes (works as expected)
        vector<string> lines = {
        "SET a AS float",
        "SET b AS float",
        "SET c AS float",
        "a = 1.5",
        "b = -2",
        "a = b * 7.2 - a / b + 7.2",
        "DISPLAY a" };
        testInterpreter(lines);
    }

    void mathematicalExpression_valid_BODMAS() { // passes (works as expected)
        vector<string> lines = {
        "SET a AS int",
        "SET b AS int",
        "SET c AS int",
        "a = 15",
        "b = -2",
        "c = 7",
        "a = (b - c) * a - a / (b + c)",
        "DISPLAY a" };
        testInterpreter(lines);
    }

    void mathematicalExpression_invalid_undeclaredVariable() { // passes (error detected)
        vector<string> lines = {
        "SET x AS int",
        "SET y AS int",
        "x = 10",
        "y = 20",
        "z = x + y" };
        testInterpreter(lines);
    }

    void mathematicalExpression_invalid_typeError() { // passes (error detected)
        vector<string> lines = {
        "SET a AS float",
        "SET b AS float",
        "SET c AS int",
        "a = 1.5",
        "b = -2",
        "c = 7",
        "a = b * c - a / b + c" };
        testInterpreter(lines);
    }

    void mathematicalExpression_invalid_invalidBrackets() { // passes (error detected)
        vector<string> lines = {
         "SET a AS int",
         "SET b AS int",
         "SET c AS int",
         "a = 15",
         "b = -2",
         "c = 7",
         "a = (b - c) * a - a / (b + c" };
        testInterpreter(lines);
    }

    void stringExpression_valid_Short() { // passes (works as expected)
        vector<string> lines = {
        "SET x AS string",
        "SET y AS string",
        "x = \"Hell\"",
        "y = \"o\"",
        "x = x + y + \" World\"",
        "DISPLAY x" };
        testInterpreter(lines);
    }

    void stringExpression_valid_Braces() { // passes (works as expected)
        vector<string> lines = {
        "SET smth AS string",
        "SET msg AS string",
        "smth = \"startin\"",
        "msg = \"somthin\"",
        "msg = smth + ( \" \" + msg)",
        "DISPLAY msg" };
        testInterpreter(lines);
    }

    void stringExpression_invalid_undeclaredVariable() { // passes (error detected)
        vector<string> lines = {
        "SET x AS string",
        "x = \"diddy\"",
        "diddy = x + \"party\"" };
        testInterpreter(lines);
    }

    void stringExpression_invalid_typeError() { // passes (error detected)
        vector<string> lines = {
        "SET a AS string",
        "SET b AS string",
        "SET c AS int",
        "a = \"score\"",
        "b = \" = \"",
        "c = 10",
        "a = a + ( b + c )" };
        testInterpreter(lines);
    }

    void stringExpression_invalid_invalidOperands() { // passes (error detected)
        vector<string> lines = {
                "SET diddy AS string",
                "diddy = \" diddy \"",
                "SET aintNoParty AS string",
                "aintNoParty = \"there aint no party\"",
                "SET res AS string",
                "res = aintNoParty + \" like a \" / diddy * \" part\" - \"y\"" };
        testInterpreter(lines);
    }

    void stringExpression_invalid_invalidRandomToken() { // passes (error detected)
        vector<string> lines = {
                "SET diddy AS string",
                "diddy = \" diddy \"",
                "SET aintNoParty AS string",
                "aintNoParty = \"there aint no party\"",
                "SET res AS string",
                "res = aintNoParty + \" like a \" + diddy + \" part\" +% \"y\"" };
        testInterpreter(lines);
    }

    void characterExpression_valid_Short() { // passes (works as expected)
        vector<string> lines = {
        "SET a AS char",
        "SET B AS char",
        "SET C AS char",
        "a = 'A'",
        "B = '1'",
        "C = a + B",
        "DISPLAY C" };
        testInterpreter(lines);
    }

    void characterExpression_valid_BODMAS() { // passes (works as expected)
        vector<string> lines = {
        "SET A AS char",
        "SET B AS char",
        "SET C AS char",
        "A = 'a'",
        "B = '1'",
        "C = ' '",
        "C = C + ( (A*B) / '~') - C",
        "DISPLAY C" };
        testInterpreter(lines);
    }

    void characterExpression_invalid_undeclaredVariable() { // passes (error detected)
        vector<string> lines = {
        "SET A AS char",
        "SET B AS char",
        "SET C AS char",
        "A = 'a'",
        "B = '1'",
        "C = ' '",
        "B = C - ( (A+b) * A ) / 'C'" };
        testInterpreter(lines);
    }

    void characterExpression_invalid_invalidCharacter() { // passes (error detected)
        vector<string> lines = {
        "SET A AS char",
        "SET B AS char",
        "SET C AS char",
        "A = 'a'",
        "B = '1'",
        "C = '27'",
        "B = C - ( (A+B) * 2 ) / 3" };
        testInterpreter(lines);
    }

    void characterExpression_invalid_typeError() { // passes (error detected)
        vector<string> lines = {
        "SET A AS char",
        "SET B AS char",
        "SET C AS string",
        "A = 'a'",
        "B = '1'",
        "C = \"27\"",
        "B = C + ( ( A + B ) * '2' ) / '3'" };
        testInterpreter(lines);
    }


    void booleanExpression_valid_Short() { // passes (works as expected)
        vector<string> lines = {
        "SET b AS bool",
        "b = TRUE",
        "b = b - TRUE - TRUE",
        "DISPLAY b" };
        testInterpreter(lines);
    }

    void booleanExpression_valid_BODMAS() { // passes (works as expected)
        vector<string> lines = {
        "SET b AS bool",
        "SET found AS bool",
        "SET c AS bool",
        "b = TRUE",
        "found = FALSE",
        "c = ( b - found ) * b + c",
        "DISPLAY c" };
        testInterpreter(lines);
    }

    void booleanExpression_invalid_divisionByZero() { // passes (error detected)
        vector<string> lines = {
        "SET b AS bool",
        "SET found AS bool",
        "SET c AS bool",
        "b = TRUE",
        "found = FALSE",
        "c = ( found - b ) * b / c" };
        testInterpreter(lines);
    }

    void booleanExpression_invalid_undeclaredVariable() { // passes (error detected)
        vector<string> lines = {
        "SET b AS bool",
        "SET found AS bool",
        "SET C AS bool",
        "b = TRUE",
        "found = FALSE",
        "C = ( b - found ) * b + c" };
        testInterpreter(lines);
    }

    void boolenExpression_invalid_typeError() { // passes (error detected)
        vector<string> lines = {
        "SET b AS bool",
        "b = TRUE",
        "b = b - TRUE - TRUE",
        "b = 1" };
        testInterpreter(lines);
    }

};

void (*Exception::invalidateCallback)() = nullptr;

Interpreter interpreter; // Definition of the global 

void invalidateCode() {
    interpreter.invalidateCode();
    exit(EXIT_FAILURE); //stops code from running any further when an exception is encountered
    //This method is only to be called when an Exception occurs
}

string getFileName(string msg) {
    string fileName = "";
    cout << msg;
    cin >> fileName;
    return fileName;
}

void runInterpreter() {
    //Runs the code for the entire solution
    string inputFileName = getFileName("Enter the name of the source code txt file. ");
    string outputFileName = getFileName("Enter the name of the txt file to which you would like to display any outputs. ");
    sourceReader->alterFileNames(inputFileName, outputFileName);

    Parser parser;
    Lexer lexer;
    vector<TokenInstance> tokens;

    interpreter.resetLineNum();
    tokens = interpreter.advanceTokens();
    //cout << "testInterpreter: first tokens = "; //debugging output statement
    //interpreter.printLexemes(tokens); //debugging output statement
    parser.setNewTokens(tokens);

    // Parse all lines/statements, not just the first
    while (parser.syntaxValid() && !tokens.empty() && interpreter.isValidCode()) {
        interpreter.printTokens(tokens);
        parser.parseExpression();
        if (parser.syntaxValid()) {
            Statement* statement = parser.getExecutableStatement();
            if (statement) {
                statement->execute();
            }
            delete statement;
            tokens = interpreter.getNewTokens();
            parser.setNewTokens(tokens);
        }
        else {
            break;
        }
    }

    delete(sourceReader);
}

int main() {
    Exception::invalidateCallback = &invalidateCode;
    Test test;
    test.booleanExpression_valid_Short();
    return 0;
}
