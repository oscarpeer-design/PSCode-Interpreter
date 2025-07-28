# PSCode-Interpreter

The PSCode programming lanuage is very basic and is intended for educational use. It mimics the style of pseudocode I was taught as a Software Design and Development student, and wasted countless hours pouring over for my 2025 HSC -  writing madeup code on paper like it's the 1960s.  

Below is the EBNF definitions of all the current statements in my language. Note that a <statement> is an example of any form of sequence, selection, or repetition.

#Sequence

output statement: DISPLAY <Variable>|<Value>

Where a <Variable> is defined as:
Letter {Letter|Digit}
Letter = A...Z && a...z
Digit = 0...9

And a <value> can be any of the 4 data types:
string: "<character>" //includes any character combination inbetween double-quotes
char: '<some character>' //includes any ASCII character
int: Digit{Digit} // integer
float: Digit.{Digit} //floating-point
bool: TRUE|FALSE //boolean

Note that every variable must have a fixed data type and a value of that same type.

variable assignment: SET <variable> AS <value>

variable expression: <variable> = <variable> | <value> {<Arithmetic Operator> <variable>|<value>}

Where an <Arithmetic Operator> is:
Addition: +
Subtraction: -
Multiplication: *
Division: /
Modulo operator: %

#Selection

An IF-statement has the following syntax

IF <conditional statement> THEN
    {<statement>}
{ELSE IF <conditional statement> THEN}
    {<statement}
[ELSE]
    {<statement>}
END IF

Where a <conditional statement> is defined as:
<condition> {AND|OR <condition>}

And a <condition> is defined as:
<variable>|<value> <Equality Operator> <variable|value>

Where an <Equality Operator> is:
IS EQUAL TO: =
IS GREATER THAN: >
IS GREATER THAN OR EQUAL TO: >=
IS LESS THAN: <
IS LESS THAN OR EQUAL TO: <=

Note that nesting is valid for both selection and repetition. Also, remember that <condition>s and <conditional statement>s are not <statement>s in their own right, but rather things to be evaluated.

#Repetition

A FOR loop has the following syntax:

FOR <variable> = <value> TO <value>
{<statement>}
NEXT

Note that the <variable> after the FOR token in this statement must be previously declared to avoid a runtime error.

A WHILE loop has the following syntax:

WHILE <conditional statement>
{<statement>}
END WHILE

Note that the <conditional statement> definition for a WHILE loop is the same as the previously mentioned definition for <conditional statement> in an IF-statement.

Hopefully, this extremely basic programming language can utterly extinguish my HSC pseudocode trauma.
# PSCode-Interpreter