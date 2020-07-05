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
    int addone = 0;
    int undefined = 0;
    int liter = 0;
    int label = 1;
    int isElse = 0;
    int false_pool[200];
    int exit_pool[200];
    int begin_pool[200];
    int pre_pool[200];
    int post_pool[200];
    int false_idx = 0;
    int exit_idx = 0;
    int begin_idx = 0;
    int pre_idx = 0;
    int post_idx = 0;
    FILE *fp;

    int HAS_ERROR = 0;
    void yyerror (char const *s)
    {
        HAS_ERROR = 1;
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
    static char* lookup_array_type(char* iden);
    static void print(char* typ, int idx);
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
    :   '{' { level++; create_symbol();} StatementList '}' { dump_symbol();}
;
Block_If
    :   '{'
        {
            level++;
            create_symbol();
        }
        StatementList '}'
        {
            dump_symbol();
            if(!isElse) {
                exit_pool[exit_idx++] = label;
                fprintf(fp, "goto L_if_exit_%d\n", label++);
                fprintf(fp, "L_if_false_%d:\n",  false_pool[--false_idx]);
            }
            isElse = 0;
        }
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
    : IF Condition_If Block_If
        {
            while(1) {
                if(exit_idx <= 0)
                    break;
                fprintf(fp, "L_if_exit_%d:\n", exit_pool[--exit_idx]);
            }

        }
    | IF Condition_If Block_If ELSE IfStmt
    | IF Condition_If Block_If ELSE {isElse = 1;} Block_If
        {
            fprintf(fp, "L_if_exit_%d:\n", exit_pool[--exit_idx]);
        }
;
Condition_If
    : Expression
        {
            if(!strcmp(lookup_symbol_type($<s_val>1), "int32") || !strcmp($<s_val>1, "int32")) {
                char s[500];
                sprintf(s, "non-bool (type int32) used as for condition");
                addone = 1;
                yyerror(s);
                addone = 0;
            }
            else if(!strcmp(lookup_symbol_type($<s_val>1), "float32") || !strcmp($<s_val>1, "float32")) {
                char s[500];
                sprintf(s, "non-bool (type float32) used as for condition");
                addone = 1;
                yyerror(s);
                addone = 0;
            }
            fprintf(fp, "iconst_0\n");
            fprintf(fp, "goto L_cmp_%d\n", label++);
            fprintf(fp, "L_cmp_%d:\n", label-2);
            fprintf(fp, "iconst_1\n");
            fprintf(fp, "L_cmp_%d:\n", label-1);
            false_pool[false_idx++] = label;
            fprintf(fp, "ifeq L_if_false_%d\n", label++);
        }
;
ForStmt
    : Begin Condition_For_While Block_For_While
        {
            fprintf(fp, "L_for_exit_%d:\n", exit_pool[--exit_idx]);
        }
    | Begin ForClause Block_For_Clause
        {
            fprintf(fp, "goto L_for_post_%d\n", post_pool[level]);
            fprintf(fp, "L_for_exit_%d:\n", exit_pool[--exit_idx]);
            begin_idx--;
        }
;
Begin
    : FOR
        {
            begin_pool[begin_idx++] = label;
            fprintf(fp, "L_for_begin_%d:\n", label++);
        }
;
ForClause
    : InitStmt ';'
        {
            begin_pool[begin_idx++] = label;
            fprintf(fp, "L_for_begin_%d:\n", label++);
        }
        Condition_For_Clause ';' PostStmt
        {
            fprintf(fp, "goto L_for_begin_%d\n", begin_pool[--begin_idx]);
            fprintf(fp, "L_for_pre_%d:\n", pre_pool[--pre_idx]);
            exit_pool[exit_idx++] = label;
            fprintf(fp, "ifeq L_for_exit_%d\n", label++);
        }
;
Condition_For_Clause
    : Expression
        {
            fprintf(fp, "iconst_0\n");
            fprintf(fp, "goto L_cmp_%d\n", label++);
            fprintf(fp, "L_cmp_%d:\n", label-2);
            fprintf(fp, "iconst_1\n");
            fprintf(fp, "L_cmp_%d:\n", label-1);
            pre_pool[pre_idx++] = label;
            fprintf(fp, "goto L_for_pre_%d\n", label++);
            post_pool[post_idx++] = label;
            fprintf(fp, "L_for_post_%d:\n", label++);
        }
;
Block_For_Clause
    :   '{'
        {
            level++;
            create_symbol();
        }
        StatementList '}'
        {
            dump_symbol();
        }
;
Condition_For_While
    : Expression
        {
            if(!strcmp(lookup_symbol_type($<s_val>1), "int32") || !strcmp($<s_val>1, "int32")) {
                char s[500];
                sprintf(s, "non-bool (type int32) used as for condition");
                addone = 1;
                yyerror(s);
                addone = 0;
            }
            else if(!strcmp(lookup_symbol_type($<s_val>1), "float32") || !strcmp($<s_val>1, "float32")) {
                char s[500];
                sprintf(s, "non-bool (type float32) used as for condition");
                addone = 1;
                yyerror(s);
                addone = 0;
            }

            exit_pool[exit_idx++] = label;
            fprintf(fp, "ifeq L_for_exit_%d\n", label++);
        }
;
Block_For_While
    :   '{'
        {
            level++;
            create_symbol();
        }
        StatementList '}'
        {
            dump_symbol();
            fprintf(fp, "goto L_for_begin_%d\n", begin_pool[--begin_idx]);

        }
;
InitStmt
    : SimpleStmt
;
PostStmt
    : SimpleStmt
;
PrintStmt
    : PRINT '(' Expression ')'
    {
        char* e_type = lookup_symbol_type($<s_val>3);
        if(!strcmp(e_type, "array")) {
            if(!strcmp(lookup_array_type($<s_val>3), "int32")) {
                print("int32", 0);
            }
            else if(!strcmp(lookup_array_type($<s_val>3), "float32")) {
                print("float32", 0);
            }
        }
        else if(!strcmp(e_type, "int32") || !strcmp($<s_val>3, "int32")){
            print("int32", 0);
        }
        else if(!strcmp(e_type, "float32") || !strcmp($<s_val>3, "float32")){
            print("float32", 0);
        }
        else if(!strcmp(e_type, "bool") || !strcmp($<s_val>3, "bool")){
            print("bool", 0);
        }
        else if(!strcmp(e_type, "string") || !strcmp($<s_val>3, "string")){
            print("string", 0);
        }
    }
    | PRINTLN '('  Expression ')'
    {
        char* e_type = lookup_symbol_type($<s_val>3);
        if(!strcmp(e_type, "array")) {
            if(!strcmp(lookup_array_type($<s_val>3), "int32")) {
                print("int32", 1);
            }
            else if(!strcmp(lookup_array_type($<s_val>3), "float32")) {
                print("float32", 1);
            }
        }
        else if(!strcmp(e_type, "int32") || !strcmp($<s_val>3, "int32")){
            print("int32", 1);
        }
        else if(!strcmp(e_type, "float32") || !strcmp($<s_val>3, "float32")){
            print("float32", 1);
        }
        else if(!strcmp(e_type, "bool") || !strcmp($<s_val>3, "bool")){
            print("bool", 1);
        }
        else if(!strcmp(e_type, "string") || !strcmp($<s_val>3, "string")){
            print("string", 1);
        }
    }
;
SimpleStmt
    : AssignmentStmt
    | ExpressionStmt
    | IncDecStmt
;
DeclarationStmt
    : VAR IDENT Type
        {
            if(!strcmp($<s_val>3, "int32"))
                fprintf(fp, "ldc 0\n");
            if(!strcmp($<s_val>3, "float32"))
                fprintf(fp, "ldc 0.0\n");
            if(!strcmp($<s_val>3, "string"))
                fprintf(fp, "ldc \"\"\n");
            if(!strcmp($<s_val>3, "bool"))
                fprintf(fp, "iconst_0\n");

            insert_symbol($<s_val>2, $<s_val>3);
        }
    | VAR IDENT Type DeclarExtend   {insert_symbol($<s_val>2, $<s_val>3);}
;
DeclarExtend
    : '=' Expression {liter = 0;}
;
AssignmentStmt
    : Left_Expression '=' Expression
        {
            if((!strcmp($<s_val>1, "int32")||!strcmp(lookup_symbol_type($<s_val>1), "int32")) && (!strcmp($<s_val>3, "float32")||!strcmp(lookup_symbol_type($<s_val>3), "float32"))) {
                char s[500];
                sprintf(s, "invalid operation: ASSIGN (mismatched types int32 and float32)");
                yyerror(s);
            }
            else if((!strcmp($<s_val>3, "int32")||!strcmp(lookup_symbol_type($<s_val>3), "int32")) && (!strcmp($<s_val>1, "float32")||!strcmp(lookup_symbol_type($<s_val>1), "float32"))) {
                char s[500];
                sprintf(s, "invalid operation: ASSIGN (mismatched types float32 and int32)");
                yyerror(s);
            }

            char* e_type = lookup_symbol_type($<s_val>1);
            if(!strcmp(e_type, "array")) {
                if(!strcmp(lookup_array_type($<s_val>1), "int32"))
                    fprintf(fp, "iastore\n");
                else
                    fprintf(fp, "fastore\n");
            }
            else if(!strcmp(e_type, "int32") || !strcmp(e_type, "bool")){
                fprintf(fp, "istore %d\n", lookup_symbol($<s_val>1));
            }
            else if(!strcmp(e_type, "float32")){
                fprintf(fp, "fstore %d\n", lookup_symbol($<s_val>1));
            }
            else if(!strcmp(e_type, "string")){
                fprintf(fp, "astore %d\n", lookup_symbol($<s_val>1));
            }
            liter = 0;
        }
    | Left_Expression ADD_ASSIGN Expression
        {
            if(liter == 1) {
                char s[500];
                sprintf(s, "cannot assign to int32");
                yyerror(s);
            }
            liter = 0;
            char* e_type = lookup_symbol_type($<s_val>1);
            if(!strcmp(e_type, "int32")){
                fprintf(fp, "iload %d\n", lookup_symbol($<s_val>1));
                fprintf(fp, "iadd\n");
                fprintf(fp, "istore %d\n", lookup_symbol($<s_val>1));
            }
            else if(!strcmp(e_type, "float32")){
                fprintf(fp, "fload %d\n", lookup_symbol($<s_val>1));
                fprintf(fp, "fadd\n");
                fprintf(fp, "fstore %d\n", lookup_symbol($<s_val>1));;
            }
        }
    | Left_Expression SUB_ASSIGN Expression
        {
            char* e_type = lookup_symbol_type($<s_val>1);
            if(!strcmp(e_type, "int32")){
                fprintf(fp, "iload %d\n", lookup_symbol($<s_val>1));
                fprintf(fp, "swap\n");
                fprintf(fp, "isub\n");
                fprintf(fp, "istore %d\n", lookup_symbol($<s_val>1));
            }
            else if(!strcmp(e_type, "float32")){
                fprintf(fp, "fload %d\n", lookup_symbol($<s_val>1));
                fprintf(fp, "swap\n");
                fprintf(fp, "fsub\n");
                fprintf(fp, "fstore %d\n", lookup_symbol($<s_val>1));;
            }
        }
    | Left_Expression MUL_ASSIGN Expression
        {
            char* e_type = lookup_symbol_type($<s_val>1);
            if(!strcmp(e_type, "int32")){
                fprintf(fp, "iload %d\n", lookup_symbol($<s_val>1));
                fprintf(fp, "imul\n");
                fprintf(fp, "istore %d\n", lookup_symbol($<s_val>1));
            }
            else if(!strcmp(e_type, "float32")){
                fprintf(fp, "fload %d\n", lookup_symbol($<s_val>1));
                fprintf(fp, "fmul\n");
                fprintf(fp, "fstore %d\n", lookup_symbol($<s_val>1));;
            }
        }
    | Left_Expression QUO_ASSIGN Expression
        {
            char* e_type = lookup_symbol_type($<s_val>1);
            if(!strcmp(e_type, "int32")){
                fprintf(fp, "iload %d\n", lookup_symbol($<s_val>1));
                fprintf(fp, "swap\n");
                fprintf(fp, "idiv\n");
                fprintf(fp, "istore %d\n", lookup_symbol($<s_val>1));
            }
            else if(!strcmp(e_type, "float32")){
                fprintf(fp, "fload %d\n", lookup_symbol($<s_val>1));
                fprintf(fp, "swap\n");
                fprintf(fp, "fdiv\n");
                fprintf(fp, "fstore %d\n", lookup_symbol($<s_val>1));;
            }
        }
    | Left_Expression REM_ASSIGN Expression
        {
            char* e_type = lookup_symbol_type($<s_val>1);
            if(!strcmp(e_type, "int32")){
                fprintf(fp, "iload %d\n", lookup_symbol($<s_val>1));
                fprintf(fp, "swap\n");
                fprintf(fp, "irem\n");
                fprintf(fp, "istore %d\n", lookup_symbol($<s_val>1));
            }
        }
;
Left_Expression
    : INT_LIT
        {
            $<s_val>$ = "int32";
            if(liter == 0)
                liter = 1;
        }
    | FLOAT_LIT
    | BOOL_LIT
    | STRING_LIT
    | IDENT
        {
            if(lookup_symbol($<s_val>1) == -1);
            if(liter == 0)
                liter = 2;
        }
    | PrimaryExpr '[' Expression ']'
;
ExpressionStmt
    : Expression
;
IncDecStmt
    : Expression IncDec_op
    {
        if(!strcmp($<s_val>2, "INC")) {
            if(!strcmp(lookup_symbol_type($<s_val>1), "int32")) {
                fprintf(fp, "ldc 1\n");
                fprintf(fp, "iadd\n");
                fprintf(fp, "istore %d\n", lookup_symbol($<s_val>1));
            }
            else {
                fprintf(fp, "ldc 1.0\n");
                fprintf(fp, "fadd\n");
                fprintf(fp, "fstore %d\n", lookup_symbol($<s_val>1));
            }
        }
        else {
            if(!strcmp(lookup_symbol_type($<s_val>1), "int32")) {
                fprintf(fp, "ldc 1\n");
                fprintf(fp, "isub\n");
                fprintf(fp, "istore %d\n", lookup_symbol($<s_val>1));
            }
            else {
                fprintf(fp, "ldc 1.0\n");
                fprintf(fp, "fsub\n");
                fprintf(fp, "fstore %d\n", lookup_symbol($<s_val>1));
            }
        }
        // printf("%s?!\n", $<s_val>1);
    }
;
IncDec_op
    : INC   {$<s_val>$ = "INC";}
    | DEC   {$<s_val>$ = "DEC";}
Expression
    : UnaryExpr {$<s_val>$ = $<s_val>1;}
    | Expression LOR Expression
        {
            char s[500];
            if(strcmp($<s_val>1, "bool")) {
                sprintf(s, "invalid operation: (operator LOR not defined on %s)", $<s_val>1);
                yyerror(s);
            }
            if(strcmp($<s_val>3, "bool")) {
                sprintf(s, "invalid operation: (operator LOR not defined on %s)", $<s_val>3);
                yyerror(s);
            }
            fprintf(fp, "ior\n");
            $<s_val>$ = "bool";
        }
    | Expression LAND Expression
        {
            char s[500];
            if(strcmp($<s_val>1, "bool")) {
                sprintf(s, "invalid operation: (operator LAND not defined on %s)", $<s_val>1);
                yyerror(s);
            }
            if(strcmp($<s_val>3, "bool")) {
                sprintf(s, "invalid operation: (operator LAND not defined on %s)", $<s_val>3);
                yyerror(s);
            }
            fprintf(fp, "iand\n");
            $<s_val>$ = "bool";
        }
    | Expression EQL Expression
        {
            char* e_type = lookup_symbol_type($<s_val>1);
            if(!strcmp($<s_val>1, "int32") || !strcmp(e_type, "int32"))
                fprintf(fp, "isub\n");
            else if(!strcmp($<s_val>1, "float32") || !strcmp(e_type, "float32"))
                fprintf(fp, "fcmpl\n");
            fprintf(fp, "ifeq L_cmp_%d\n", label++);
            $<s_val>$ = "bool";
        }
    | Expression NEQ Expression
        {
            char* e_type = lookup_symbol_type($<s_val>1);
            if(!strcmp($<s_val>1, "int32") || !strcmp(e_type, "int32"))
                fprintf(fp, "isub\n");
            else if(!strcmp($<s_val>1, "float32") || !strcmp(e_type, "float32"))
                fprintf(fp, "fcmpl\n");
            fprintf(fp, "ifne L_cmp_%d\n", label++);
            $<s_val>$ = "bool";
        }
    | Expression '<' Expression
        {
            char* e_type = lookup_symbol_type($<s_val>1);
            if(!strcmp($<s_val>1, "int32") || !strcmp(e_type, "int32"))
                fprintf(fp, "isub\n");
            else if(!strcmp($<s_val>1, "float32") || !strcmp(e_type, "float32"))
                fprintf(fp, "fcmpl\n");
            fprintf(fp, "iflt L_cmp_%d\n", label++);
            $<s_val>$ = "bool";
        }
    | Expression LEQ Expression
        {
            char* e_type = lookup_symbol_type($<s_val>1);
            if(!strcmp($<s_val>1, "int32") || !strcmp(e_type, "int32"))
                fprintf(fp, "isub\n");
            else if(!strcmp($<s_val>1, "float32") || !strcmp(e_type, "float32"))
                fprintf(fp, "fcmpl\n");
            fprintf(fp, "ifle L_cmp_%d\n", label++);
            $<s_val>$ = "bool";
        }
    | Expression '>' Expression
        {
            char* e_type = lookup_symbol_type($<s_val>1);
            if(!strcmp($<s_val>1, "int32") || !strcmp(e_type, "int32"))
                fprintf(fp, "isub\n");
            else if(!strcmp($<s_val>1, "float32") || !strcmp(e_type, "float32"))
                fprintf(fp, "fcmpl\n");
            fprintf(fp, "ifgt L_cmp_%d\n", label++);
            fprintf(fp, "iconst_0\n");
            fprintf(fp, "goto L_cmp_%d\n", label++);
            fprintf(fp, "L_cmp_%d:\n", label-2);
            fprintf(fp, "iconst_1\n");
            fprintf(fp, "L_cmp_%d:\n", label-1);
            $<s_val>$ = "bool";
        }
    | Expression GEQ Expression {fprintf(fp, "%s\n", "GEQ"); $<s_val>$ = "bool";}
    | Expression '+' Expression
        {

            if((!strcmp($<s_val>1, "int32")||!strcmp(lookup_symbol_type($<s_val>1), "int32")) && (!strcmp($<s_val>3, "float32")||!strcmp(lookup_symbol_type($<s_val>3), "float32"))) {
                char s[500];
                sprintf(s, "invalid operation: ADD (mismatched types int32 and float32)");
                yyerror(s);
            }
            else if((!strcmp($<s_val>3, "int32")||!strcmp(lookup_symbol_type($<s_val>3), "int32")) && (!strcmp($<s_val>1, "float32")||!strcmp(lookup_symbol_type($<s_val>1), "float32"))) {
                char s[500];
                sprintf(s, "invalid operation: ADD (mismatched types float32 and int32)");
                yyerror(s);
            }

            char* e_type = lookup_symbol_type($<s_val>1);
            if(strcmp(e_type, "array")) {
                if(!strcmp($<s_val>1, "int32") || !strcmp(e_type, "int32"))
                    fprintf(fp, "iadd\n");
                else if(!strcmp($<s_val>1, "float32") || !strcmp(e_type, "float32"))
                    fprintf(fp, "fadd\n");
            }
            else {
                if(!strcmp(lookup_array_type($<s_val>1), "int32"))
                    fprintf(fp, "iadd\n");
                else
                    fprintf(fp, "fadd\n");
            }

        }
    | Expression '-' Expression
        {
            if((!strcmp($<s_val>1, "int32")||!strcmp(lookup_symbol_type($<s_val>1), "int32")) && (!strcmp($<s_val>3, "float32")||!strcmp(lookup_symbol_type($<s_val>3), "float32"))) {
                char s[500];
                sprintf(s, "invalid operation: SUB (mismatched types int32 and float32)");
                yyerror(s);
            }
            else if((!strcmp($<s_val>3, "int32")||!strcmp(lookup_symbol_type($<s_val>3), "int32")) && (!strcmp($<s_val>1, "float32")||!strcmp(lookup_symbol_type($<s_val>1), "float32"))) {
                char s[500];
                sprintf(s, "invalid operation: SUB (mismatched types float32 and int32)");
                yyerror(s);
            }
            char* e_type = lookup_symbol_type($<s_val>1);
            if(strcmp(e_type, "array")) {
                if(!strcmp($<s_val>1, "int32") || !strcmp(e_type, "int32"))
                    fprintf(fp, "isub\n");
                else if(!strcmp($<s_val>1, "float32") || !strcmp(e_type, "float32"))
                    fprintf(fp, "fsub\n");
            }
            else {
                if(!strcmp(lookup_array_type($<s_val>1), "int32"))
                    fprintf(fp, "isub\n");
                else
                    fprintf(fp, "fsub\n");
            }
        }
    | Expression '*' Expression
        {
            char* e_type = lookup_symbol_type($<s_val>1);
            if(strcmp(e_type, "array")) {
                if(!strcmp($<s_val>1, "int32") || !strcmp(e_type, "int32"))
                    fprintf(fp, "imul\n");
                else if(!strcmp($<s_val>1, "float32") || !strcmp(e_type, "float32"))
                    fprintf(fp, "fmul\n");
            }
            else {
                if(!strcmp(lookup_array_type($<s_val>1), "int32"))
                    fprintf(fp, "imul\n");
                else
                    fprintf(fp, "fmul\n");
            }
        }
    | Expression '/' Expression
        {
            char* e_type = lookup_symbol_type($<s_val>1);
            if(strcmp(e_type, "array")) {
                if(!strcmp($<s_val>1, "int32") || !strcmp(e_type, "int32"))
                    fprintf(fp, "idiv\n");
                else if(!strcmp($<s_val>1, "float32") || !strcmp(e_type, "float32"))
                    fprintf(fp, "fdiv\n");
            }
            else {
                if(!strcmp(lookup_array_type($<s_val>1), "int32"))
                    fprintf(fp, "idiv\n");
                else
                    fprintf(fp, "fdiv\n");
            }
        }
    | Expression '%' Expression
        {
            char s[500];
            if(!strcmp($<s_val>1, "float32") || !strcmp(lookup_symbol_type($<s_val>1), "float32")) {
                sprintf(s, "invalid operation: (operator REM not defined on float32)");
                yyerror(s);
            }
            if(!strcmp($<s_val>3, "float32") || !strcmp(lookup_symbol_type($<s_val>3), "float32")) {
                sprintf(s, "invalid operation: (operator REM not defined on float32)");
                yyerror(s);
            }
            fprintf(fp, "irem\n");
        }
;
UnaryExpr
    : PrimaryExpr   {$<s_val>$ = $<s_val>1;}
    | unary_op UnaryExpr
        {
            $<s_val>$ = $<s_val>2;
            if(!strcmp($<s_val>1, "NEG")) {
                if(!strcmp($<s_val>2, "int32"))
                    fprintf(fp, "ineg\n");
                else
                    fprintf(fp, "fneg\n");
            }
            else if(!strcmp($<s_val>1, "NOT")) {
                fprintf(fp, "iconst_1\n");
                fprintf(fp, "ixor\n");
            }
            // fprintf(fp, "%s\n", $<s_val>1);
        }
;
PrimaryExpr
    : Operand { $<s_val>$ = $<s_val>1;}
    | PrimaryExpr '[' Expression ']'
        {
            if(!strcmp(lookup_array_type($<s_val>1), "int32"))
                fprintf(fp, "iaload\n");
            else
                fprintf(fp, "faload\n");
            $<s_val>$ = $<s_val>1;
            // printf("%s\n", $<s_val>$);
        }
    | Type '(' Expression ')'
        {
            char* e_type = lookup_symbol_type($<s_val>3);
            if(!strcmp(e_type, "array")) {
                if(!strcmp($<s_val>1, "int32") && !strcmp(lookup_array_type($<s_val>3), "float32"))
                    fprintf(fp, "f2i\n");
                else
                    fprintf(fp, "i2f\n");
            }
            else if(!strcmp($<s_val>1, "int32") && (!strcmp(e_type, "float32") || !strcmp($<s_val>3, "float32")))
                fprintf(fp, "f2i\n");
            else if(!strcmp($<s_val>1, "float32") && (!strcmp(e_type, "int32") || !strcmp($<s_val>3, "int32")))
                fprintf(fp, "i2f\n");

        }
;
unary_op
    : '+'   {$<s_val>$ = "POS";}
    | '-'   {$<s_val>$ = "NEG";}
    | '!'   {$<s_val>$ = "NOT";}
;
Operand
    : INT_LIT
        {
            fprintf(fp, "ldc %d\n", $<i_val>1);
            $<s_val>$ = "int32";
            if(liter == 0)
                liter = 1;
        }
    | FLOAT_LIT { fprintf(fp, "ldc %lf\n", $<f_val>1); $<s_val>$ = "float32";}
    | BOOL_LIT
        {
            if(!strcmp($<s_val>1, "TRUE")) {
                fprintf(fp, "iconst_1\n");
            }
            else {
                fprintf(fp, "iconst_0\n");
            }
            // fprintf(fp, "%s\n", $<s_val>1);
            $<s_val>$ = "bool";
        }
    | STRING_LIT    { fprintf(fp, "ldc \"%s\"\n", $<s_val>1); $<s_val>$ = "string";}
    | IDENT
        {
            char* e_type = lookup_symbol_type($<s_val>1);
            if(!strcmp(e_type, "array")) {
                fprintf(fp, "aload %d\n", lookup_symbol($<s_val>1));
            }
            if(strcmp(e_type, "array")) {
                if(!strcmp(lookup_symbol_type($<s_val>1), "float32"))
                    fprintf(fp, "fload %d\n", lookup_symbol($<s_val>1));
                else if(!strcmp(lookup_symbol_type($<s_val>1), "string"))
                    fprintf(fp, "aload %d\n", lookup_symbol($<s_val>1));
                else
                    fprintf(fp, "iload %d\n", lookup_symbol($<s_val>1));
            }
            $<s_val>$ = $<s_val>1;
            if(liter == 0)
                liter = 2;
        }
    | '(' Expression ')'    {$<s_val>$ = $<s_val>2;}
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
    fp = fopen("hw3.j", "w");
    fprintf(fp, ".source hw3.j\n");
    fprintf(fp, ".class public Main\n");
    fprintf(fp, ".super java/lang/Object\n");
    fprintf(fp, ".method public static main([Ljava/lang/String;)V\n");
    fprintf(fp, ".limit stack 100\n");
    fprintf(fp, ".limit locals 100\n");


    yyparse();

    fprintf(fp, "return\n");
    fprintf(fp, ".end method\n");

	// fprintf(fp, "Total lines: %d\n", yylineno);
    fclose(yyin);

    if (HAS_ERROR) {
        remove("hw3.j");
    }

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
            /* printf("%s\n", iden); */
            return;
        }
    }
    // fprintf(fp, "> Insert {%s} into symbol table (scope level: %d)\n", iden, level);

    struct symbol_entry* cur = (struct symbol_entry*)malloc(sizeof(struct symbol_entry));
    cur->idx = table[level].size++;
    strcpy(cur->name, iden);
    strcpy(cur->typ, typ);
    cur->addr = address++;
    cur->line = yylineno;
    cur->next = NULL;

    if(!strcmp(typ, "array")) {
        strcpy(cur->ele_typ, ele_typ);
        if(!strcmp(ele_typ, "int32"))
            fprintf(fp, "newarray int\n");
        else
            fprintf(fp, "newarray float\n");
        fprintf(fp, "astore %d\n", cur->addr);
    }
    else {
        strcpy(cur->ele_typ, "-");
        if(!strcmp(typ, "float32"))
            fprintf(fp, "fstore %d\n", cur->addr);
        else if(!strcmp(typ, "string"))
            fprintf(fp, "astore %d\n", cur->addr);
        else
            fprintf(fp, "istore %d\n", cur->addr);
    }

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
            if(!strcmp(cur->name, iden)) {
                return cur->addr;
            }
    }
    /* printf("%s\n", iden); */
    char s[500];
    sprintf(s, "undefined: %s", iden);
    addone = 1;
    yyerror(s);
    addone = 0;
    undefined = 1;
    return -1;
}

static void dump_symbol() {
    // printf("> Dump symbol table (scope level: %d)\n", level);
    // printf("%-10s%-10s%-10s%-10s%-10s%s\n",
    //        "Index", "Name", "Type", "Address", "Lineno", "Element type");

    // for(struct symbol_entry* cur = table[level].head; cur != NULL; cur = cur->next)
    //     printf("%-10d%-10s%-10s%-10d%-10d%s\n", cur->idx, cur->name, cur->typ, cur->addr, cur->line, cur->ele_typ);
    level--;
}

static char* lookup_symbol_type(char* iden){
    for(int i = level; i >= 0; --i) {
        for(struct symbol_entry* cur = table[i].head; cur != NULL; cur = cur->next) {
            if(!strcmp(cur->name, iden))
                return cur->typ;

        }
    }
    return "";
}
static char* lookup_array_type(char* iden){
    for(int i = level; i >= 0; --i) {
        for(struct symbol_entry* cur = table[i].head; cur != NULL; cur = cur->next) {
            if(!strcmp(cur->name, iden) && strcmp(cur->ele_typ, "-"))
                return cur->ele_typ;
        }
    }
    return "";
}
static void print(char* typ, int idx){
    if(!strcmp(typ, "int32")) {
        fprintf(fp, "getstatic java/lang/System/out Ljava/io/PrintStream;\n");
        fprintf(fp, "swap\n");
        if(idx == 0)
            fprintf(fp, "invokevirtual java/io/PrintStream/print(I)V\n");
        else
            fprintf(fp, "invokevirtual java/io/PrintStream/println(I)V\n");
    }
    else if(!strcmp(typ, "float32")) {
        fprintf(fp, "getstatic java/lang/System/out Ljava/io/PrintStream;\n");
        fprintf(fp, "swap\n");
        if(idx == 0)
            fprintf(fp, "invokevirtual java/io/PrintStream/print(F)V\n");
        else
            fprintf(fp, "invokevirtual java/io/PrintStream/println(F)V\n");
    }
    else if(!strcmp(typ, "string")) {
        fprintf(fp, "getstatic java/lang/System/out Ljava/io/PrintStream;\n");
        fprintf(fp, "swap\n");
        if(idx == 0)
            fprintf(fp, "invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
        else
            fprintf(fp, "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
    }
    else {
        fprintf(fp, "ifne L_cmp_%d\n", label++);
        fprintf(fp, "ldc \"false\"\n");
        fprintf(fp, "goto L_cmp_%d\n", label++);
        fprintf(fp, "L_cmp_%d:\n", label-2);
        fprintf(fp, "ldc \"true\"\n");
        fprintf(fp, "L_cmp_%d:\n", label-1);
        fprintf(fp, "getstatic java/lang/System/out Ljava/io/PrintStream;\n");
        fprintf(fp, "swap\n");
        if(idx == 0)
            fprintf(fp, "invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
        else
            fprintf(fp, "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
    }
}

