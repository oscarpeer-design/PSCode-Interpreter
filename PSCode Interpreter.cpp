#include <iostream> // enables output of strings to screen
#include <stdio.h>
#include <iomanip> //manipulates floating point data to enable printing
#include <string>
#include <vector>
#include <unordered_map>
#include <cctype>
#include <exception>
//#include <variant>
using namespace std; //This makes outputting to the console require less lines
using std::string; //Include these lines to add strings

enum class OpType { ADD, SUB, DIV, MULTIPLY, Other }; //Create an enum called OpType that handles different operations
enum class EqualityOperator { LT, GT, EQ, NEQ, LTET, GTET };
enum class DataType { Int, Float, Char, String, Boolean, UnAssigned };
enum class ExceptionType { Lexical, Syntactical, Runtime };

enum class StatementType { BEGIN, END, Output, AssignmentStatement, VariableExpression, IFStatement, ForLoop, WhileLoop, Undefined };
enum class Token { BEGIN, END, IF, WHILE, FOR, ENDIF, THEN, TO, ELSE, ELSEIF, ENDWHILE, DISPLAY, SET, AS, ADD, SUB, DIV, MULTIPLY, Int, Float, Char, String, Boolean, OpenBrace, ClosedBrace, Variable, Literal, EqualityOp, AND, OR };

struct TokenInstance {
    Token type = Token::Literal;
    string lexeme = "";
    TokenInstance(Token t = Token::Literal, string l = "") : type(t), lexeme(l) {} //initialise values
};

class Exception {
public:
    ExceptionType exceptionType;
    string message = "Something went wrong";

    Exception(ExceptionType type, string msg) {
        this->exceptionType = type;
        this->message = msg;
        cout << message << endl;
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
        catch (const std::invalid_argument& e) {
            return false;
        }
    }

    int stringToInt(string str) {
        bool isValid = validInt(str);
        if (isValid) {
            return stoi(str);
        }
        else {
            Exception ex = Exception(ExceptionType::Runtime, "Cannot convert the string, " + str + ", to an Integer");
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
        catch (const std::invalid_argument& e) {
            return false;
        }
    }

    float stringToFloat(string str) {
        bool isValid = validFloat(str);
        if (isValid) {
            return stof(str);
        }
        else {
            Exception ex = Exception(ExceptionType::Runtime, "Cannot convert the string, " + str + ", to a Floating Point number");
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
        return str.length() == 1;
    }
    char stringToChar(string str) {
        bool isValid = validChar(str);
        if (isValid) {
            return str[0];
        }
        else {
            Exception ex = Exception(ExceptionType::Runtime, "Cannot convert the string, " + str + ", to a Character.");
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
            catch (const std::out_of_range& e) { //in case of empty string 
                return this->def;
            }
        }
        else {
            Exception ex = Exception(ExceptionType::Runtime, "Cannot convert the token, " + str + ", to a string");
            return this->def;
        }
    }

};

class Lexer {
private:
    string line;
    bool validToken = true;
    unordered_map<string, Token> tokenMap = {
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
        {"OR", Token::OR}
    };
public:
    Lexer() {}

    void setLine(string line) {
        this->line = line;
    }

    bool tokensValid() {
        return this->validToken;
    }

    bool isDigit(char c) {
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
        if (len > 0) {
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
        Integer Int = Integer();
        bool isInt = Int.validInt(element);
        if (isInt) {
            return true;
        }
        FloatingPoint Float = FloatingPoint();
        bool isFloat = Float.validFloat(element);
        if (isFloat) {
            return true;
        }
        Character Char = Character();
        bool isChar = Char.validChar(element);
        if (isChar) {
            return true;
        }
        StringLiteral String = StringLiteral();
        bool isString = String.validString(element);
        if (isString) {
            return true;
        }
        return false;
    }

    Token getToken(const string& element) {
        auto it = tokenMap.find(element);
        if (it != tokenMap.end()) {
            return it->second;
        }
        if (isLiteral(element)) {
            return Token::Literal;
        }
        if (isValidVariable(element)) {
            return Token::Variable;
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
        size_t i = 0, n = line.length();
        while (i < n) {
            // Skip spaces
            while (i < n && isspace(line[i])) i++;
            if (i >= n) break;

            // Handle quoted strings
            if (line[i] == '"') {
                size_t start = i++;
                while (i < n && line[i] != '"') i++;
                if (i < n) i++; // Include closing quote
                lexemes.push_back(line.substr(start, i - start));
            }
            // Handle multi-char operators (e.g., >=, <=, <>, END IF)
            else if (i + 1 < n && (line.substr(i, 2) == ">=" || line.substr(i, 2) == "<=" || line.substr(i, 2) == "<>")) {
                lexemes.push_back(line.substr(i, 2));
                i += 2;
            }
            // Handle single-char tokens
            else if (ispunct(line[i])) {
                lexemes.push_back(string(1, line[i]));
                i++;
            }
            // Handle words (identifiers, keywords)
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
        string line = this->line;
        //cout << "Tokenizing" << endl;
        line = removeNewline(line); //remove newline characters
        //cout << "Without newline: " + line << endl;
        lexemes = chopIntoLexemes(line); //break line into substrings based on spaces
        //cout << "Chopped into lexemes...\n";
        //printLexemes(lexemes);
        //cout << "Revised lexemes...\n";
        lexemes = revisedLexemes(lexemes);//edit those substrings for tokens like ELSEIF, ENDIF, ENDWHILE, ect
        //printLexemes(lexemes);

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
    Statement() {}
};

class Expression : public Statement {
    //The virtual keyword implements polymorphism and inheritance easily. It states that a method or attribute can be overridden.
public:
    virtual ~Expression() {}
    virtual int evaluate() const = 0;
    //The const terminator is put after the method signature to tell the compiler that this method doesn't modify the state of the object.
    //The override terminator ensures a class correctly overrides a virtual method from a base class.
};

class Literal :public Expression { //This is a leaf node that can represent numbers, and be combined with BinaryOperation to hold expressions
    int value;
public:
    Literal(int val) : value(val) {}
    //This type of initialisation is called an initialiser list. It is a memory efficient and fast way of initialising class members.
    int evaluate() const override {
        return value;
    }
};

class BinaryOperation : public Expression {
    OpType op;
    Expression* left;
    Expression* right;
    //Defining leaf-nodes to store numbers
public:
    BinaryOperation(OpType op, Expression* left, Expression* right) : op(op), left(left), right(right) {}
    //Initialising virtual constructor
    int evaluate() const override { //override keyword shows overriding class method by child class
        int evaluation = 0;
        int l = left->evaluate();
        int r = right->evaluate();
        switch (op) {
        case OpType::ADD:
            evaluation = l + r;
            break;
        case OpType::SUB:
            evaluation = l - r;
            break;
        case OpType::DIV:
            if (r == 0) {
                evaluation = 0;
            }
            else {
                evaluation = l / r;
            }
            break;
        case OpType::MULTIPLY:
            evaluation = l * r;
            break;
        }
        return evaluation;
    }
};
//template <typename T> //create generic type template for variables
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
    void printValues() override {
        for (const auto& token : outputValues) {
            //for (TokenInstance token: outputValues) {
                //cout << "valid";
            cout << token.lexeme;
        }
        cout << endl;
    }
};

class VariableExpression : public Statement { //putting data into declared variables
private:
    vector<TokenInstance> values;
    vector<OpType> operations;
public:
    VariableExpression(vector<TokenInstance> values, vector<OpType> operations) {
        this->values = values;
        this->operations = operations;
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

    bool evaluateCondition() {
        bool result = false;
        string lhsValue = this->lhs.lexeme;
        string rhsValue = this->rhs.lexeme;
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
    Branch* current;
    BranchNode* nested;
    BranchNode* nextBranch;
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


class Loop : public BranchNode {

};

class WhileLoop : public Loop {

};

class ForLoop : public Statement {
private:
    vector<Statement*> statements;
    int numReptitions = 0;

    void getRepetitions() { //TDLR
        this->numReptitions = 0;
    }
public:
    void execute() override {

    }
};

class Interpreter; // Forward declaration
extern Interpreter interpreter; // Declare the global variable

class FileHandler {
protected:
    string inputFilename;
    string outputFilename;

public:

    FileHandler() {}

    FileHandler(string inputFilename, string outputFilename) {
        this->inputFilename = inputFilename;
        this->outputFilename = outputFilename;
    }

    void writeOutput(string outputLine) {

    }

    string readLine() {
        string line = "";
        return line;
    }
};

class Interpreter : public FileHandler {

private:
    bool validCode = true;
    vector<string>lines;
    int currentLineNum = 0;
public:
    Interpreter() {}

    Interpreter(string inputFilename, string outputFilename) {
        this->inputFilename = inputFilename;
        this->outputFilename = outputFilename;
    }

    void setLines(vector<string> lines) {
        this->lines = lines;
    }

    vector<TokenInstance> getNewTokens() {
        vector<TokenInstance> tokens;
        Lexer lexer;
        if (this->currentLineNum < this->lines.size()) {
            lexer.setLine(this->lines[currentLineNum]);
            tokens = lexer.tokenizeLine();
            currentLineNum++;
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

};

class Parser {
private:
    StatementType statType = StatementType::Undefined;
    Statement* statement; //defining with pointer prevents loss of derived class data
    bool validSyntax = true;
    bool isFinalLine = false;
    ExceptionType exType = ExceptionType::Syntactical;
    vector<TokenInstance> tokens; // Use a dynamic vector instead of a fixed-size array to store the tokens 
    //size_t numTokens = tokens.size;

public:
    Parser() : statement(nullptr) {}

    Parser(vector<TokenInstance>generatedTokens) {
        //this->numTokens = generatedTokens.size();
        this->statement = nullptr;
        this->tokens = generatedTokens;
    }

    ~Parser() { //destructor
        delete statement;
    }

    void setNewTokens(vector<TokenInstance>generatedTokens) {
        //this->numTokens = generatedTokens.size();
        this->tokens = generatedTokens;
    }

    void generateNewTokens() {
        this->tokens = interpreter.getNewTokens();
        //interpreter.printTokens(this->tokens);
    }

    bool syntaxValid() {
        return this->validSyntax;
    }

    void checkFinalLine() {
        this->isFinalLine = !interpreter.notFinalLine();
    }

    void parseExpression() {
        Token t = this->tokens[0].type;
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
        case Token::FOR:
            this->statType = StatementType::ForLoop;
            forLoop();
            break;
        case Token::WHILE:
            this->statType = StatementType::WhileLoop;
            whileLoop();
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
        if (this->tokens.size() != 4 || this->tokens[2].type != Token::AS) { //check correct structure
            raiseException(exceptionMsg);
        }
        else {
            DataType dType = getType(this->tokens[3]);
            TokenInstance var = this->tokens[1];
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
        while (i < this->tokens.size() && valid) {
            type = this->tokens[i].type;
            if (expectedValue && isVariableORValue(type)) {
                output->addVariable(this->tokens[i]);
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
            output->printValues();
        }

    }

    bool isCondition(vector<TokenInstance>conditionTokens) {
        //assume conditionTokens.size() == 3
        bool condition = false;
        if (conditionTokens.size() != 3) {
            //cout << "invalid length = " << conditionTokens.size() << endl;
            return condition;
        }
        if (isVariableORValue(conditionTokens[0].type) && checkEqualityOperand(conditionTokens[1]) && isVariableORValue(conditionTokens[2].type)) {
            //cout << "valid syntax structure" << endl;
            if (conditionTokens[0].lexeme != conditionTokens[2].lexeme) {
                condition = true;
            }
        }
        return condition;
    }

    Condition getCondition(vector<TokenInstance> conditionTokens) {
        EqualityOperator op = getEqualityOperator(conditionTokens[1]);
        Condition c = Condition(conditionTokens[0], conditionTokens[2], op);
        return c;
    }

    vector<Condition> getConditions(vector<TokenInstance>tokens) {
        vector<Condition> conditions;
        //This takes a conditional statement and extracts the conditions where a condition is <variable>|<value> <equality operator> <OTHER variable><value>
        vector<TokenInstance>conditionTokens;
        //bool expectedCondition = true;
        bool validCondition = true;
        string msg = "Invalid condition statement.";
        size_t size = tokens.size();
        size_t j;
        size_t i = 2;
        while (i < size && validCondition) {
            j = i - 2;
            conditionTokens.assign(tokens.begin() + j, tokens.begin() + i + 1);
            //print out conditionTokens:
            //cout << "Token at " << i << " is " << tokens[i].lexeme << endl;
            //cout << conditionTokens[0].lexeme << " " << conditionTokens[1].lexeme << " " << conditionTokens[2].lexeme << endl;
            validCondition = isCondition(conditionTokens);

            if (validCondition) {
                ;
                conditions.push_back(getCondition(conditionTokens));
            }

            i += 4;
        }
        //if (expectedCondition) {
        //    msg = "A conditional statement was expected.";
        //    raiseException(msg);
        //}
        if (!validCondition) {
            raiseException(msg);
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
            name = "ElSE";
        }
        return name;
    }

    vector<TokenInstance> getConditionLinkers(vector<TokenInstance> tokens) {
        //This takes a conditional statement and gets the AND|OR values
        vector<TokenInstance>conditionLinkers;
        size_t size = tokens.size();
        if (size > 3) {
            bool expectedLinker = true;
            bool isLinker;
            bool expectedCondition = false;
            size_t j = 3;
            string msg = "Expected either an AND or an OR operator.";
            //cout << "analysing linkers" << endl;
            while (j < size) {
                expectedLinker = true;
                //cout << "Token at "  << j << " is " << tokens[j].lexeme << endl;

                isLinker = isConditionLinker(tokens[j]);

                if (isLinker) {
                    conditionLinkers.push_back(tokens[j]);
                    expectedLinker = false;
                    expectedCondition = true;
                }

                if (j < size - 1) { //If the value after the AND|OR is a <variable>|<value>, we assume it is part of a condition, keeping in mind that the conditions have already been checked.
                    //cout << "Token at " << j + 1 << " is " << tokens[j + 1].lexeme << endl;
                    expectedCondition = !isVariableORValue(tokens[j + 1].type);
                }
                j += 4;
            }
            if (expectedLinker) {
                raiseException(msg);
            }
            if (expectedCondition) {
                msg = "A conditional statement was expected.";
                raiseException(msg);
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

    Branch* parseBranch(bool isElseBranch, const vector<TokenInstance>& headerTokens, BranchNode* parentNode = nullptr) {
        cout << "parseBranch: next token is " << interpreter.getTokenString(tokens[0].type) << " (" << tokens[0].lexeme << ")" << endl;
        ConditionalStatement cond;
        if (!isElseBranch) {
            size_t start = 1;
            size_t end = headerTokens.size();
            if (headerTokens.back().type == Token::THEN) end--;
            vector<TokenInstance> condTokens(headerTokens.begin() + start, headerTokens.begin() + end);
            cond = parseConditionalStatement(condTokens);
        }
        vector<Statement*> statements;
        bool done = false;
        while (!done && !isFinalLine && validSyntax) {
            if (tokens.empty()) {
                generateNewTokens();
                if (tokens.empty()) continue;
            }
            cout << "parseIfStatement: lookahead token is " << interpreter.getTokenString(tokens[0].type) << " (" << tokens[0].lexeme << ")" << endl;
            Token t = tokens[0].type;
            if (t == Token::IF) {
                BranchNode* nestedIfNode = parseIfStatement();
                IFStatement* nestedIfStatement = new IFStatement(nestedIfNode);
                statements.push_back(nestedIfStatement);
                statement = nullptr;
                generateNewTokens();
            }
            else if (t == Token::ELSEIF || t == Token::ELSE || t == Token::ENDIF) {
                done = true;
            }
            else {
                parseExpression();
                if (statement) statements.push_back(statement);
                statement = nullptr;
                generateNewTokens();
            }
            checkFinalLine();
        }
        return new Branch(cond, statements);
    }

    BranchNode* parseIfStatement() {
        cout << "parseIfStatement: lookahead token is " << interpreter.getTokenString(tokens[0].type) << " (" << tokens[0].lexeme << ")" << endl;
        bool isElse = tokens[0].type == Token::ELSE;
        vector<TokenInstance> headerTokens = this->tokens;
        generateNewTokens(); // <-- Advance to the first line of the branch body!
        BranchNode* node = new BranchNode();
        node->isElseBranch = isElse;
        node->current = parseBranch(isElse, headerTokens, node);

        // Now look ahead for ELSE IF, ELSE, or END IF
        BranchNode* lastNode = node;
        bool done = false;
        while (!isFinalLine && !done && validSyntax) {
            if (tokens.empty()) {
                generateNewTokens();
                if (tokens.empty()) continue;
            }
            cout << "parseIfStatement: lookahead token is " << interpreter.getTokenString(tokens[0].type) << " (" << tokens[0].lexeme << ")" << endl;
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
                generateNewTokens();
            }
            checkFinalLine();
        }
        return node;
    }



    void forLoop() {

        //for-loop
        /*EBNF: FOR <integer variable> = <start number> | <variable> TO <end number> | <variable>
                      {<statements>(may include more loops or if-statements)}
                NEXT
        */
        //precondition: tokens[0] == FOR
        //postcondition: NEXT expected
    }

    void whileLoop() {
        //while-loop
        //EBNF: WHILE <condition> 
        //precondition: tokens[0] == WHILE
        //postcondition: ENDWHILE expected 
    }

    void variableExpression() {
        //put values into variables
        //use mathematicalExpression() for type int and float
        //precondition: tokens[0].type == <variable>
        //EBNF: <variable> = <variable>|<value> {<operator> <variable>|<value>}
        //The actual complexities of type concatenation will be handled by the Runtime Analyser
        size_t size = this->tokens.size();
        bool validExpression = true;
        string msg = "Invalid variable expression.";
        if (size < 3) {
            validExpression = false;
        }
        else if (tokens[1].lexeme != "=") {
            validExpression = false;
            msg = "Invalid equality operator.";
        }
        vector<TokenInstance> values;
        vector<OpType> operations;
        size_t idx = 2;
        bool valueExpected = true;
        bool opExpected = false;
        Token type;
        while (idx < size && validExpression) {
            type = this->tokens[idx].type;
            if (valueExpected && (isVariableORValue(type) || isBrace(type))) {
                valueExpected = false;
                values.push_back(this->tokens[idx]);
                if (idx < size - 1) {
                    opExpected = true;
                }
            }
            else if (opExpected && isOperand(type)) {
                operations.push_back(getArithmeticOperator(this->tokens[idx]));
                opExpected = false;
                valueExpected = true;
            }
            else {
                validExpression = false;
            }
            idx++;
        }
        if (valueExpected) {
            msg = "A variable or value is expected.";
        }
        if (opExpected) {
            msg = "Missing an arithmetic operator.";
        }
        this->statement = new VariableExpression(values, operations);
        if (!validExpression) {
            raiseException(msg);
        }
    }

    void mathematicalExpression() { //CREATE AST FROM THIS
        //TDLR
        Expression* expr;
        Expression* left;
        Expression* right;
        vector<TokenInstance> tokens_leftArray;
        vector<TokenInstance> tokens_rightArray;

        int i = 0;
        int start = 0;
        int end = 1;
        while (i < this->tokens.size()) {
            //check to find operator
            //apply BODMAS based in operator
            //apply recursion with smaller token sub-arrays
            //
            try {

            }
            catch (const std::exception e) {
                raiseException("Invalid expression.");
            }
            i++;
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
        cout << "Result of 5 * ( (10 + 2) / 6 ) = " << expr->evaluate();
        delete expr;
        /*In C++ memory is manually allocated. Therefore, objects must be deleted once they go out of scope to avoid memory leaks. Memory leaks are errors that occur
        when a program manually allocates memory but fails to free memory when it is no longer needed. Preventing memory leaks improves performance, prevents
        unpredictable behaviour, and avoids memory clashes.*/
    }
    void parsedArithmetic_validBrackets() {
        //The goal here is to recreate the above expression, but with a series of tokens
        vector<TokenInstance> tokens = {
            TokenInstance {Token::Literal, "5"},
            TokenInstance {Token::MULTIPLY, "*"},
            TokenInstance {Token::OpenBrace, "("},
            TokenInstance {Token::OpenBrace, "("},
            TokenInstance {Token::Literal, "10"},
            TokenInstance {Token::ADD, "+"},
            TokenInstance {Token::Literal, "2"},
            TokenInstance {Token::ClosedBrace, ")"},
            TokenInstance {Token::DIV, "/"},
            TokenInstance {Token::Literal, "6"},
            TokenInstance {Token::ClosedBrace, ")"}
        };
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

    void outputStatement_invalidADD() { //passes (fails as expected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::DISPLAY, "DISPLAY"},
            TokenInstance {Token::Literal, "name: "},
            TokenInstance {Token::ADD, "+"},
        };
        Parser p(tokens);
        p.outputStatement();
    }

    void outputStatement_invalidRandomToken() { //passes (fails as expected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::DISPLAY, "DISPLAY"},
            TokenInstance {Token::Literal, "name: "},
            TokenInstance {Token::DIV, "/"},
            TokenInstance {Token::Variable, "name"}
        };
        Parser p(tokens);
        p.outputStatement();
    }
    void outputStatement_invalidLength() { //passes (fails as expected)
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
            TokenInstance {Token::Int, "Integer"}
        };
        Parser p(tokens);
        p.assignmentStatement();
    }

    void assignmentStatement_invalidDatatype() { // passes (fails as expected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::SET, "SET"},
            TokenInstance {Token::Variable, "var"},
            TokenInstance {Token::AS, "AS"},
            TokenInstance {Token::Variable, "Double"} //we assume here that the tokenizer interperets "Double" as a variable because there is no Double datatype in our language
        };
        Parser p(tokens);
        p.assignmentStatement();
    }

    void assignmentStatement_invalidLength() { // passes (fails as expected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::SET, "SET"},
            TokenInstance {Token::Variable, "var"},
        };
        Parser p(tokens);
        p.assignmentStatement();
    }

    void assignmentStatement_invalidRandomTokens() { // passes (fails as expected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::SET, "SET"},
            TokenInstance {Token::Variable, "var"},
            TokenInstance {Token::AS, "AS"},
            TokenInstance {Token::Int, "Integer"},
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

    void conditionalStatement_invalidToken() { // passes (fails as expected)
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
    void conditionalStatement_invalid_expectedCondition() { // passes (fails as expected)
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

    void conditionalStatement_invalid_expectedLinker() { // passes (fails as expected)
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

    void variableExpression_invalidLength() { // passes (fails as expected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::Variable, "var"},
            TokenInstance {Token::Literal, "="}
        };
        Parser p(tokens);
        p.variableExpression();
    }

    void variableExpression_invalidEqualityOp() { // passes (fails as expected)
        vector<TokenInstance> tokens = {
            TokenInstance {Token::Variable, "var"},
            TokenInstance {Token::EqualityOp, ">"},
            TokenInstance {Token::Literal, "0"}
        };
        Parser p(tokens);
        p.variableExpression();
    }

    void variableExpression_invalid_expectedOperand() { // passes (fails as expected)
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

    void variableExpression_invalid_expectedValue() { // passes (fails as expected)
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

    void variableExpression_invalidOperand() { // passes (fails as expected)
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
        vector<TokenInstance> tokens;
        Lexer lexer;
        Parser parser;
        for (auto& line : lines) {
            lexer.setLine(line);
            tokens = lexer.tokenizeLine();
            parser.setNewTokens(tokens);
            parser.parseExpression();
        }
    }

    void ifStatement_validShort() {
        vector<string> lines = {
            "IF var > 1 THEN",
                "var = var + 1",
            "END IF" };

    }
    void ifStatement_validLonger() {
        vector<string> lines = {
            "IF var > 1 THEN",
                "var = var + 1",
            "ELSE IF var > 0 AND var < 1 THEN",
                "var = var - 1",
            "ELSE",
                "var = 0",
            "END IF" };

    }

    void ifStatement_validLonger_nesting() {
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

    }

};

Interpreter interpreter; // Definition of the global variable

int main() {
    /*Test test = Test();
    test.outputStatement_valid();*/
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

    interpreter.setLines(lines);
    vector<TokenInstance> tokens = interpreter.getNewTokens();
    interpreter.printTokens(tokens);

    Parser p(tokens);
    p.parseExpression();

    return 0;
}

