
%option noyywrap

%{ 
#ifdef _MSC_VER //if visual stdio
  #define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdio.h>
#include"array.h"

#ifdef _WIN32
  #include "calc.tab.h"
#else //LINUX
  #include "y.tab.h"
#endif

#include <math.h>
int semantic;
char string_var[10];

%}

/* These are not tokens. They are pattern definitions.  */
NEWLINE  \n
WSPACE   [ \t\r]*
WORDCHAR [a-zA-Z]+
NUMBER   [0-9]*\.?[0-9]*
 
%%

{NEWLINE}  {   /* Code that you want run when Lex encounters your pattern 
                  goes between the curly brackets. */

               return NEWLINE;
 
          }
"//".* {
	/*Discard*/
	
    }

{WSPACE}    {
                /* Discard */
            }

[a-z]+ {
		memcpy(string_var,yytext,strlen(yytext)+1);
		
		return X;
    	  }
[A-Z]+ {
	 printf("Unknown Identificator(Lexical mistake):%s at %d.You can not use capital A,B,C...It's prohibited.\n",yytext,counter);
			exit(-1);
	}

"/"     {
		printf("Unknown Identificator(Lexical mistake):%s at %d.Division is not implemented yet\n",yytext,counter);
			exit(-1);
	}


{NUMBER}  {
		  if(strstr(yytext,"."))
			{
			  semantic=1;
			  printf("Warining semantic mistake: you may receive unclear answer because of using float point numbers\n");
			}
		//if(semantic==1)
		   //printf("Warining semantic mistake: you may receive unclear answer because of using float point numbers\n");
		  #ifdef _MSC_VER	   //   
				sscanf_s(yytext, "%d", &yylval.num);//округлим в меньшую сторону с понижением до int 
		   #else // LINUX or not Visual Stdio
		        sscanf(yytext, "%d", &yylval.num);//округлим в меньшую сторону с понижением до int 
		   #endif
              return NUMBER;
          }

"+" {
      return ADD;
    }

"-" {
      return SUB;
    }

"*" { 
      return MUL;
    }

"^" {
      return POWER;
    }   

"(" {
      return PAREN_OPEN;
    }

")" { 
      return PAREN_CLOSE;
    }


%%


