%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

%}

%type <operand> EXPI
%type <operand> EXPB
%type <operand> EXPF

%token OP_PLUS
%token OP_MINUS
%token OP_DIV
%token OP_MULT
%token OP_OP
%token OP_CP
%token OP_OC
%token OP_CC
%token OP_COMMA
%token KW_AND
%token KW_OR
%token KW_NOT
%token KW_EQUAL
%token KW_SET
%token KW_DEFFUN
%token KW_DEFVAR
%token KW_FOR
%token KW_IF
%token KW_EXIT
%token KW_TRUE
%token KW_FALSE
%token COMMENT
%token <operand> VALUE
%token <expr> IDENTIFIER
%token KW_GT VALUEF


%{

int yylex();
int yyerror(char *error);

typedef struct myStruct
{
    char str[100];
    int variable;
    struct myStruct *next;

}structVar;

structVar *root= NULL;
structVar *var = NULL;

int yyerror(char *error) {
    printf("SYNTAX ERROR\n");
}

%}
%union{
int operand;
int *operands;
char expr[30];
}
%start START
%%
START:
    EXPI {printf("SYNTAX OK. \nResult: %d\n\n", $1);} ;

START:
    START OP_OP KW_EXIT OP_CP { printf("Terminated....\n"); exit(-1); }
    |
    START EXPB {printf("SYNTAX OK. \nResult: %s\n\n", $2 == 1 ? "T" : "NIL");}
    |
    EXPB {printf("SYNTAX OK. \nResult: %s\n\n", $1 == 1 ? "T" : "NIL");}    |    START EXPI {printf("SYNTAX OK. \nResult: %d\n\n", $2);}
    |
    COMMENT {}    |    START COMMENT {}    |    OP_OP KW_EXIT OP_CP { printf("Terminated...\n"); exit(-1);};
    

EXPI:
    OP_OP OP_PLUS EXPI EXPI OP_CP  {$$=$3+$4;}
    |
    OP_OP OP_MINUS EXPI EXPI OP_CP {$$=$3-$4;}
    |
    OP_OP OP_MULT EXPI EXPI OP_CP  {$$=$3*$4;}
    |
    OP_OP OP_DIV EXPI EXPI OP_CP   {$$=$3/$4;}
    |    VALUE {$$ = $1;};

EXPF:
  OP_OP KW_DEFVAR IDENTIFIER EXPF OP_CP {$$ = $4;}
| OP_OP OP_PLUS EXPF EXPF OP_CP {$$ = 5;}
| OP_OP OP_MINUS EXPF EXPF OP_CP {$$ = $3 - $4;}
| OP_OP OP_MULT EXPF EXPF OP_CP {$$ = $3 * $4;};


EXPB:
    KW_TRUE  { $$ = 1; }    |    KW_FALSE   { $$ = 0; }
    |
    OP_OP KW_AND EXPB EXPB OP_CP {$$ = $3 && $4;}    |    OP_OP KW_OR EXPB EXPB OP_CP  {$$ = $3 || $4;}
    |
    OP_OP KW_NOT EXPB OP_CP  {$$ = ! ($3);}    |    OP_OP KW_EQUAL EXPB EXPB OP_CP {$$ = ($3 == $4);}
    |
    OP_OP KW_EQUAL EXPI EXPI OP_CP {$$ = ($3 == $4);};


%%

void printList(int *ls){
    printf("( ");

    for(int i=0;ls[i]!=-1; ++i){
        printf("%d ", ls[i]);
    }

    printf(")\n");
}


int main(){
   	printf("\nTo terminate program, enter an empty string\n");
   	printf("Program will print outputs to terminal when program is terminated.n");
    char* newLine = NULL;
    char* line = (char*)malloc(100*sizeof(char));
    int isEmpty;
    size_t size = 0;

    while(isEmpty != 1){
        printf("\nEnter an Input-->");
        isEmpty = getline(&newLine, &size, stdin);
        if(isEmpty != 1){
            line = (char *) realloc(line, (strlen(line)+size+5)*sizeof(char));
            strcat(line,newLine);
        }    
    }


    yy_scan_string(line);


    yyparse();
    exit(-1);
}
