/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;
    int address = 0;
    int level = 0;
    struct symbol_table table[100];
    // record element type in array
    char ele_typ[20];
    // use for error
    int addone = 0;
    int undefined = 0;
    int liter = 0;
    void yyerror (char const *s)
    {
        if(addone == 0)
            printf("error:%d: %s\n", yylineno, s);
        else
            printf("error:%d: %s\n", yylineno+1, s);
    }

    /* Symbol table function - you can add new function if needed. */
    static void create_symbol();
    static void insert_symbol(char* iden, char *typ);
    static int lookup_symbol(char* iden);
    static void dump_symbol();
    static char* lookup_symbol_type(char* iden);
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
}

/* Token without return */
%token VAR NEWLINE
%token INT FLOAT BOOL STRING IDENT
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token LAND LOR
%token INC DEC GEQ LEQ EQL NEQ
%token PRINT PRINTLN
%token IF ELSE FOR


/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT
%token <i_val> BOOL_LIT

%left LOR
%left LAND
%left EQL NEQ '<' LEQ '>' GEQ
%left '+' '-'
%left '*' '/' '%'

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : StatementList {dump_symbol();}
;
Block
    : '{' {level++; create_symbol();} StatementList '}' {dump_symbol();}
;
StatementList
    : StatementList Statement
    | Statement
;
Statement
    : DeclarationStmt NEWLINE
    | SimpleStmt NEWLINE
    | Block NEWLINE
    | IfStmt NEWLINE
    | ForStmt NEWLINE
    | PrintStmt NEWLINE
    | NEWLINE
;
IfStmt
    : IF Condition Block
    | IF Condition Block ELSE IfStmt
    | IF Condition Block ELSE Block
;
Condition
    : Expression
        {
            if(strcmp($<s_val>1, "bool")) {
                char s[500];
                sprintf(s, "non-bool (type %s) used as for condition", $<s_val>1);
                addone = 1;
                yyerror(s);
                addone = 0;
            }
        }
;
ForStmt
    : FOR Condition Block
    | FOR ForClause Block
;
ForClause
    : InitStmt ';' Condition ';' PostStmt
;
InitStmt
    : SimpleStmt
;
PostStmt
    : SimpleStmt
;
PrintStmt
    : PRINT '(' Expression ')'  {printf("PRINT %s\n", $<s_val>3);}
    | PRINTLN '(' Expression ')' {printf("PRINTLN %s\n", $<s_val>3);}
;
SimpleStmt
    : AssignmentStmt
    | ExpressionStmt
    | IncDecStmt
;
DeclarationStmt
    : VAR IDENT Type {insert_symbol($<s_val>2, $<s_val>3);}
    | VAR IDENT Type DeclarExtend   {insert_symbol($<s_val>2, $<s_val>3);}
;
DeclarExtend
    : '=' Expression {liter = 0;}
;
AssignmentStmt
    : Expression assign_op Expression
    {
        if(liter == 1) {
            char s[500];
            sprintf(s, "cannot assign to int32");
            yyerror(s);
        }
        else if(strcmp($<s_val>1, $<s_val>3) && !undefined) {
            char s[500];
            sprintf(s, "invalid operation: %s (mismatched types %s and %s)", $<s_val>2, $<s_val>1, $<s_val>3);
            yyerror(s);
            undefined = 0;
        }
        liter = 0;
        printf("%s\n", $<s_val>2);
    }
;
assign_op
    : '='   {$<s_val>$ = "ASSIGN";}
    | ADD_ASSIGN    {$<s_val>$ = "ADD_ASSIGN";}
    | SUB_ASSIGN    {$<s_val>$ = "SUB_ASSIGN";}
    | MUL_ASSIGN    {$<s_val>$ = "MUL_ASSIGN";}
    | QUO_ASSIGN    {$<s_val>$ = "QUO_ASSIGN";}
    | REM_ASSIGN    {$<s_val>$ = "REM_ASSIGN";}
;
ExpressionStmt
    : Expression
;
IncDecStmt
    : Expression IncDec_op
;
IncDec_op
    : INC   {printf("INC\n");}
    | DEC   {printf("DEC\n");}
Expression
    : UnaryExpr {$<s_val>$ = $<s_val>1;}
    | Expression LOR Expression
        {
            if(strcmp($<s_val>1, "bool")) {
                char s[500];
                sprintf(s, "invalid operation: (operator LOR not defined on %s)", $<s_val>1);
                yyerror(s);
            }
            if(strcmp($<s_val>3, "bool")) {
                char s[500];
                sprintf(s, "invalid operation: (operator LOR not defined on %s)", $<s_val>3);
                yyerror(s);
            }
            printf("%s\n", "LOR");
            $<s_val>$ = "bool";
        }
    | Expression LAND Expression
        {
            if(strcmp($<s_val>1, "bool")) {
                char s[500];
                sprintf(s, "invalid operation: (operator LAND not defined on %s)", $<s_val>1);
                yyerror(s);
            }
            if(strcmp($<s_val>3, "bool")) {
                char s[500];
                sprintf(s, "invalid operation: (operator LAND not defined on %s)", $<s_val>3);
                yyerror(s);
            }
            printf("%s\n", "LAND");
            $<s_val>$ = "bool";
        }
    | Expression EQL Expression {printf("%s\n", "EQL"); $<s_val>$ = "bool";}
    | Expression NEQ Expression {printf("%s\n", "NEQ"); $<s_val>$ = "bool";}
    | Expression '<' Expression {printf("%s\n", "LSS"); $<s_val>$ = "bool";}
    | Expression LEQ Expression {printf("%s\n", "LEQ"); $<s_val>$ = "bool";}
    | Expression '>' Expression {printf("%s\n", "GTR"); $<s_val>$ = "bool";}
    | Expression GEQ Expression {printf("%s\n", "GEQ"); $<s_val>$ = "bool";}
    | Expression '+' Expression
        {
            if(strcmp($<s_val>1, $<s_val>3)) {
                char s[500];
                sprintf(s, "invalid operation: ADD (mismatched types %s and %s)", $<s_val>1, $<s_val>3);
                yyerror(s);
            }
            printf("%s\n", "ADD");
        }
    | Expression '-' Expression
        {
            if(strcmp($<s_val>1, $<s_val>3)) {
                char s[500];
                sprintf(s, "invalid operation: SUB (mismatched types %s and %s)", $<s_val>1, $<s_val>3);
                yyerror(s);
            }
            printf("%s\n", "SUB");
        }
    | Expression '*' Expression {printf("%s\n", "MUL");}
    | Expression '/' Expression {printf("%s\n", "QUO");}
    | Expression '%' Expression
        {
            if(strcmp($<s_val>1, "int32")) {
                char s[500];
                sprintf(s, "invalid operation: (operator REM not defined on %s)", $<s_val>1);
                yyerror(s);
            }
            if(strcmp($<s_val>3, "int32")) {
                char s[500];
                sprintf(s, "invalid operation: (operator REM not defined on %s)", $<s_val>3);
                yyerror(s);
            }
            printf("%s\n", "REM");
        }
;
UnaryExpr
    : PrimaryExpr   {$<s_val>$ = $<s_val>1;}
    | unary_op UnaryExpr    {$<s_val>$ = $<s_val>2; printf("%s\n", $<s_val>1);}
;
PrimaryExpr
    : Operand   {$<s_val>$ = $<s_val>1;}
    | IndexExpr
    | ConversionExpr
;
unary_op
    : '+'   {$<s_val>$ = "POS";}
    | '-'   {$<s_val>$ = "NEG";}
    | '!'   {$<s_val>$ = "NOT";}
;
Operand
    : Literal   {$<s_val>$ = $<s_val>1;}
    | IDENT
        {
            int temp = lookup_symbol($<s_val>1);
            if(temp != -1) {
                printf("IDENT (name=%s, address=%d)\n", $<s_val>1, temp);
                strcpy($<s_val>$, lookup_symbol_type($<s_val>1));
            }
            if(liter == 0)
                liter = 2;
        }
    | '(' Expression ')'    {$<s_val>$ = $<s_val>2;}
;
Literal
    : INT_LIT   { printf("INT_LIT %d\n", $<i_val>1); $<s_val>$ = "int32"; if(liter == 0) liter = 1;}
    | FLOAT_LIT { printf("FLOAT_LIT %.6f\n", $<f_val>1); $<s_val>$ = "float32";}
    | BOOL_LIT  { printf("%s\n", $<s_val>1); $<s_val>$ = "bool";}
    | STRING_LIT    { printf("STRING_LIT %s\n", $<s_val>1); $<s_val>$ = "string";}
;
IndexExpr
    : PrimaryExpr '[' Expression ']'
;
ConversionExpr
    : Type '(' Expression ')'   {printf("%c to %c\n", $<s_val>3[0]-32, $<s_val>1[0]-32);}
;
Type
    : TypeName  {$<s_val>$ = $<s_val>1;}
    | ArrayType {$<s_val>$ = "array";}
;
TypeName
    : INT {$<s_val>$ = "int32";}
    | FLOAT {$<s_val>$ = "float32";}
    | STRING {$<s_val>$ = "string";}
    | BOOL {$<s_val>$ = "bool";}
;
ArrayType
    : '[' Expression ']' Type {strcpy(ele_typ, $<s_val>4); liter = 0;}
;
%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }
    for(int i = 0; i < 100; ++i) {
        table[i].head = NULL;
        table[i].tail = NULL;
        table[i].size = 0;
    }
    yylineno = 0;
    yyparse();

	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}

static void create_symbol() {
    table[level].head = NULL;
    table[level].tail = NULL;
    table[level].size = 0;
}

static void insert_symbol(char* iden, char* typ) {
    for(struct symbol_entry* cur = table[level].head; cur != NULL; cur = cur->next) {
        if(!strcmp(cur->name, iden)) {
            char s[500];
            sprintf(s, "%s redeclared in this block. previous declaration at line %d", iden, cur->line);
            yyerror(s);
            return;
        }
    }
    printf("> Insert {%s} into symbol table (scope level: %d)\n", iden, level);

    struct symbol_entry* cur = (struct symbol_entry*)malloc(sizeof(struct symbol_entry));
    cur->idx = table[level].size++;
    strcpy(cur->name, iden);
    strcpy(cur->typ, typ);
    cur->addr = address++;
    cur->line = yylineno;
    cur->next = NULL;

    if(!strcmp(typ, "array"))
        strcpy(cur->ele_typ, ele_typ);
    else
        strcpy(cur->ele_typ, "-");

    if(table[level].head == NULL) {
        table[level].head = cur;
        table[level].tail = cur;
    }
    else {
        table[level].tail->next = cur;
        table[level].tail = cur;
    }
}

static int lookup_symbol(char* iden){
    for(int i = level; i >= 0; --i) {
        for(struct symbol_entry* cur = table[i].head; cur != NULL; cur = cur->next)
            if(!strcmp(cur->name, iden))
                return cur->addr;
    }
    char s[500];
    sprintf(s, "undefined: %s", iden);
    addone = 1;
    yyerror(s);
    addone = 0;
    undefined = 1;
    return -1;
}

static void dump_symbol() {
    printf("> Dump symbol table (scope level: %d)\n", level);
    printf("%-10s%-10s%-10s%-10s%-10s%s\n",
           "Index", "Name", "Type", "Address", "Lineno", "Element type");

    for(struct symbol_entry* cur = table[level].head; cur != NULL; cur = cur->next)
        printf("%-10d%-10s%-10s%-10d%-10d%s\n", cur->idx, cur->name, cur->typ, cur->addr, cur->line, cur->ele_typ);
    level--;
}

static char* lookup_symbol_type(char* iden){
    for(int i = level; i >= 0; --i) {
        for(struct symbol_entry* cur = table[i].head; cur != NULL; cur = cur->next) {
            if(!strcmp(cur->name, iden) && !strcmp(cur->ele_typ, "-"))
                return cur->typ;
            if(!strcmp(cur->name, iden) && strcmp(cur->ele_typ, "-"))
                return cur->ele_typ;
        }
    }
    return "";
}