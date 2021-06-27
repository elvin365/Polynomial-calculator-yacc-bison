
  


%{
#ifdef _MSC_VER  //if visual stdio
  #define _CRT_SECURE_NO_WARNINGS
#endif

#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include<string.h>
#include<ctype.h>
#include"array.h"
extern FILE* yyin;
char variable[1024];
struct Node *a; 
int yydebug=0;
int counter=1;
void yyerror(const char *s);
int yylex();
void yy_switch_to_buffer(void*);
void *yy_scan_string(const char*);
void *yylex_destroy(); 
void check(char*);
int check_variable(char*);
int check_polynom(char*);
void check_variable_insert(char*);
int check_polynom2(char*);
void add_caret_return(char* buf);
void check_zero(char* r1);
void poly_to_string(struct Int, char**);
struct Int polyadd(struct Int*,struct Int*);
struct Int array_zpad(struct Int*,int);
struct Int* init(struct Int*,struct Int*);
void safeadd(struct Int*,struct Int,struct Int);
void polymult(struct Int*,struct Int,struct Int);
void make_negative(struct Int*);
void show(struct Int);

void str(int,char*);
//#define YYSTYPE int*
%}



%token   ADD SUB MUL POWER PAREN_OPEN PAREN_CLOSE NEWLINE X //NUMBER
%token<num> NUMBER
%type<mass_and_size> powopt intopt mon tm expr quest program

%union
{
	struct Int mass_and_size;
	int num;
}



%%

program : 
	quest program {free($1.mass_of_ints);}
	| quest
	;



quest: 
      expr NEWLINE
	{
	char* r=NULL;
	poly_to_string($1,&r);
	semantic=0;
	//printf("%s\n",r);
	char* r1=(char*)malloc( (strlen(r)+3)*sizeof(char) );
	r1[0]='(';
	memcpy(r1+1,r,strlen(r)+1);
	strcat(r1,")");
	//check_zero(r1);
	if(!strcmp(r1,"()"))
	{
	  int temp = strlen(r1);
	r1 = (char*)realloc(r1, strlen(r1) + 2);
	r1[temp - 1] = '0';
	r1[temp] = ')';
	r1[temp+1] = '\0';
	}
		if(strstr(variable,"$"))
		{
			struct Node *del=NULL;
			if(del = Find(a, variable))
			{
			  free(del->Name_of_Variable);
			  free(del->Polynom);
			  del->Name_of_Variable = (char*)calloc(strlen(variable)+1, sizeof(char));
			  sprintf(del->Name_of_Variable, "%s", variable);
			  del->Polynom= (char*)calloc(strlen(r1)+1, sizeof(char));
			  sprintf(del->Polynom, "%s", r1);
			}
			else
			{
			   a=add(a,variable,r1);
			}
		 
		}
		else if(strstr(variable,"print"))
		{
			char* r2=(char*)malloc( (strlen(r1)-1)*sizeof(char) );//no paren
			memcpy(r2,r1+1,(strlen(r1)-1)*sizeof(char));
			r2[strlen(r1)-2]='\0';
			printf("%s\n",r2);
		}
		
	free($1.mass_of_ints);
	


	//exit(0);
	
	}
	| NEWLINE
	{
	 
	} 
	;

expr: 	tm 
	{

	     $$=$1;

	}
	| SUB tm 
	{
	   struct Int temp;
	   temp.mass_of_ints=(int*)calloc($2.size_of_array,sizeof(int));
	   memcpy(temp.mass_of_ints,$2.mass_of_ints,$2.size_of_array*sizeof(int));
	   temp.size_of_array=$2.size_of_array; 
	   //make_negative(&temp);
		int i=0;
		for(i=0;i<temp.size_of_array;i++)
		{
			temp.mass_of_ints[i]=temp.mass_of_ints[i]*(-1);
		}
	   $$=temp;
	}
	| expr ADD tm 
	{

	   $$=polyadd(&$1,&$3);
 
	}
	| expr SUB tm  
	{ 
 
	struct Int temp;
	temp.mass_of_ints=(int*)calloc($3.size_of_array,sizeof(int));
	memcpy(temp.mass_of_ints,$3.mass_of_ints,$3.size_of_array*sizeof(int));
	temp.size_of_array=$3.size_of_array;

		int i=0;
		for(i=0;i<temp.size_of_array;i++)
		{
			temp.mass_of_ints[i]=temp.mass_of_ints[i]*(-1);
		}
	
	 $$=polyadd(&$1,&temp); 
 
	}
	;


tm: 	mon 
	{

	 $$=$1;

         }
	| PAREN_OPEN expr PAREN_CLOSE { $$=$2; }
	| tm PAREN_OPEN expr PAREN_CLOSE 
	{

	struct Int temp;
	temp.size_of_array=0;
	temp.mass_of_ints=NULL;
	polymult(&temp,$1,$3);
	$$.size_of_array=temp.size_of_array;
	$$.mass_of_ints=(int*)realloc(temp.mass_of_ints,(sizeof(int)*temp.size_of_array));
	memcpy($$.mass_of_ints,temp.mass_of_ints,sizeof(int)*temp.size_of_array);

	 }
	;

mon: intopt X powopt 
	{

		$$.mass_of_ints=NULL;//(int*)calloc(1,sizeof(int));//??
		$$.size_of_array=0;	

		safeadd(&$$,$3,$1);

		
	}
      |
	NUMBER 
	{
		$$.mass_of_ints=(int*)calloc(1,sizeof(int));
		$$.size_of_array++;	
		$$.mass_of_ints[0]=$1;

	}
	;


intopt: NUMBER {

			$$.mass_of_ints=(int*)calloc(1,sizeof(int));
			$$.size_of_array++;	

			$$.mass_of_ints[0]=$1;
			
			

		}
	| %empty {
			$$.mass_of_ints=(int*)calloc(1,sizeof(int));
			$$.size_of_array++;	
			$$.mass_of_ints[0]=1;

		 }
	;

powopt: POWER NUMBER { 
			$$.mass_of_ints=(int*)calloc(1,sizeof(int));
			$$.size_of_array++;	
			$$.mass_of_ints[0]=$2;

		     }
	| %empty { $$.mass_of_ints=(int*)calloc(1,sizeof(int));
		   $$.mass_of_ints[0]=1;
		   $$.size_of_array++;	

		 }
	;	 


%%
void yyerror(const char *err)
{
    printf("At line %d\n",counter);
    fprintf(stderr, "Error: %s\n", err);
    exit(-1);
}

void make_negative(struct Int* par)
{
	
	int i=0;
	for(i=0;i<par->size_of_array;i++)
	{
		par->mass_of_ints[i]=(-1)*par->mass_of_ints[i];
	}

}


void safeadd(struct Int* q,struct Int i, struct Int x)
{
	
	if(i.mass_of_ints[0]<q->size_of_array)
		{
		q->mass_of_ints[i.mass_of_ints[0]]+=x.mass_of_ints[0];

		}
	else
	{
		//int j=0;
		for(;i.mass_of_ints[0]-q->size_of_array;)
		{

		   q->mass_of_ints=(int*)realloc(q->mass_of_ints,(sizeof(int)*(q->size_of_array+1)));

		   q->size_of_array++;

		   q->mass_of_ints[q->size_of_array-1]=0;

		}
		q->mass_of_ints=(int*)realloc(q->mass_of_ints,(sizeof(int)*(q->size_of_array+1)));

		q->size_of_array++;
		q->mass_of_ints[q->size_of_array-1]=x.mass_of_ints[0];

	}

}
void polymult(struct Int* r,struct Int p,struct Int q)
{

	int i=0;
	int j=0;
	struct Int p1;
	p1.mass_of_ints=(int*)calloc(p.size_of_array,sizeof(int));
	memcpy(p1.mass_of_ints,p.mass_of_ints,sizeof(int)*p.size_of_array);
	p1.size_of_array=p.size_of_array;

	for(i=0;i<p1.size_of_array;i++)
	{
		for(j=0;j<q.size_of_array;j++)
		{
		 struct Int temp1;
		 struct Int temp2;
		 temp1.mass_of_ints=(int*)calloc(1,sizeof(int));
		 temp1.mass_of_ints[0]=i+j;
		 temp1.size_of_array=1;
		 temp2.mass_of_ints=(int*)calloc(1,sizeof(int));
		 temp2.mass_of_ints[0]=p1.mass_of_ints[i]*q.mass_of_ints[j];
		  

		 temp2.size_of_array=1;


		 /*inline safeadd*/
		 
	if(temp1.mass_of_ints[0]<r->size_of_array)
		{
		r->mass_of_ints[temp1.mass_of_ints[0]]+=temp2.mass_of_ints[0];

		}
	else
	{
		//int j=0;
		for(;temp1.mass_of_ints[0]-r->size_of_array;)
		{

		   r->mass_of_ints=(int*)realloc(r->mass_of_ints,(sizeof(int)*(r->size_of_array+1)));

		   r->size_of_array++;

		   r->mass_of_ints[r->size_of_array-1]=0;

		}
		r->mass_of_ints=(int*)realloc(r->mass_of_ints,(sizeof(int)*(r->size_of_array+1)));

		r->size_of_array++;
		r->mass_of_ints[r->size_of_array-1]=temp2.mass_of_ints[0];

	}

		 free(temp1.mass_of_ints);
		free(temp2.mass_of_ints);
		}
	}

	memcpy(p.mass_of_ints,p1.mass_of_ints,sizeof(int)*p.size_of_array);
	
}
struct Int array_zpad(struct Int* q,int l)
{
	if(l>=q->size_of_array)
		{
			int i=0;
			for(i=0;i<l-q->size_of_array;i++)
			{
			q->mass_of_ints=(int*)realloc(q->mass_of_ints,(sizeof(int)*(q->size_of_array+1)));
		   	q->size_of_array++;
		   	q->mass_of_ints[q->size_of_array-1]=0;
			}
		}
	return *q;
}
struct Int polyadd(struct Int* p,struct Int* q)
{
	*p=array_zpad(p,q->size_of_array);
	*q=array_zpad(q,p->size_of_array);
	struct Int temp;
	temp.mass_of_ints=(int*)calloc(MAX(p->size_of_array,q->size_of_array) ,sizeof(int));
	temp.size_of_array=MAX(p->size_of_array,q->size_of_array);
	int i=0;
	for(i;i<MIN(p->size_of_array,q->size_of_array);i++)
	{
	  temp.mass_of_ints[i]=p->mass_of_ints[i]+q->mass_of_ints[i];
	}
	struct Int temp2;
	if(p->size_of_array>q->size_of_array)
	{
	 	temp2=*p;
	}
	else temp2=*q;
	for(i=MIN(p->size_of_array,q->size_of_array);i<MAX(p->size_of_array,q->size_of_array);i++)
	{
		
		temp.mass_of_ints[i]=temp2.mass_of_ints[i];
	}
	return temp;
	
}

void poly_to_string(struct Int p,char** r)
{
	char buf[1000];
	if(p.size_of_array==1) 
		{
		  str(p.mass_of_ints[0],buf);
		  *r=(char*)calloc((strlen(buf)+1),sizeof(char));
	 	 memcpy(*r,buf,sizeof(char)*(strlen(buf)+1));
		  return;
		}
	
	if((p.mass_of_ints[0]))
	{
	
	  
	  str(p.mass_of_ints[0],buf);

	  *r=(char*)calloc((strlen(buf)+1),sizeof(char));
	  memcpy(*r,buf,sizeof(char)*(strlen(buf)+1));


	}
	else 
	{
	*r=(char*)calloc(1,sizeof(char));
	*r[0]='\0';
	  

	}
	char msign[2]="\0";
	if(p.mass_of_ints[0]>0)
	{
	  msign[0]='+';

	}
	
	if(p.size_of_array>=1 && p.mass_of_ints[1]!=0)
	{

		if(p.mass_of_ints[1]==1)
		{

			char temp_buf[1000]="\0";
			//strcat(temp_buf,"x");
			strcat(temp_buf,string_var);
			strcat(temp_buf,msign);
			strcat(temp_buf,*r);

			*r=(char*)realloc(*r,(sizeof(char)*(strlen(temp_buf)+1)));
			memcpy(*r,temp_buf,strlen(temp_buf)+1);

		}
		else
		{

			char temp_buf[1000]="\0";

			str(p.mass_of_ints[1],buf);
			strcat(temp_buf,buf);
			
			//strcat(temp_buf,"x");
			strcat(temp_buf,string_var);
			strcat(temp_buf,msign);
			strcat(temp_buf,*r);


			*r=(char*)realloc(*r,(sizeof(char)*(strlen(temp_buf)+1)));
			memcpy(*r,temp_buf,strlen(temp_buf)+1);


		}
		if(p.mass_of_ints[1]<0)
		{

		 msign[0]='\0';
		}
	}
	int i=2;
	for(;i<p.size_of_array;i++)
	{
		if(p.mass_of_ints[i]!=0)
		{
			if(p.mass_of_ints[i]==1)
			{
			if(*r[0]!='\0' && *r[0]!='-')
				{
				msign[0]='+';
				}
			char temp_buf[1000]="\0";
			//strcat(temp_buf,"x^");
			strcat(temp_buf,string_var);
			strcat(temp_buf,"^");
			str(i,buf);
			
			strcat(temp_buf,buf);
			strcat(temp_buf,msign);
			strcat(temp_buf,*r);
			*r=(char*)realloc(*r,(sizeof(char)*(strlen(temp_buf)+1)));
			memcpy(*r,temp_buf,strlen(temp_buf)+1);

			}
			else
			{
			
			char temp_buf[1000]="\0";
			if(*r[0]!='\0' && *r[0]!='-')
				{
				msign[0]='+';
				}
			str(p.mass_of_ints[i],buf);
			strcat(temp_buf,buf);
			//strcat(temp_buf,"x^");
			strcat(temp_buf,string_var);
			strcat(temp_buf,"^");
			str(i,buf);
			strcat(temp_buf,buf);
			strcat(temp_buf,msign);

			strcat(temp_buf,*r);
			*r=(char*)realloc(*r,(sizeof(char)*(strlen(temp_buf)+1)));
			memcpy(*r,temp_buf,strlen(temp_buf)+1);	
		
			}
		
		if(p.mass_of_ints[i]<0)
		{
		 msign[0]='\0';

		}
		else
			{

			 msign[0]='+';
			}
		}

	}
	
//free(string_var);
memset(string_var,0,10);
}
void str(int num,char* r)
{
	int counter1=0;
	int num1=num;

	while(num!=0)
	{
		num=num/10;
		counter1++;
	}

	memset(r,'\0',(1000)*sizeof(char));
	sprintf(r,"%d",num1);

	
}

struct Node* add(struct Node *head, char* name , char* polynom)
{
	struct Node *New = (struct Node*)malloc(sizeof(struct Node));
	New->Name_of_Variable = (char*)malloc(sizeof(name)*(strlen(name)+1));
	sprintf(New->Name_of_Variable, "%s", name);
	New->Polynom = (char*)malloc(sizeof(polynom)*(strlen(polynom) + 1));
	sprintf(New->Polynom, "%s", polynom);
	New->next = head;
	return New;
}

void Show(struct Node* head)
{
	while (head)
	{
		printf("%s\n", head->Name_of_Variable);
		printf("%s\n", head->Polynom);
		puts("\n");
		head = head->next;
	}
}

struct Node* Find(struct Node *head, char* name)
{
	while (head)
	{
		if (!strcmp(head->Name_of_Variable,name))
			return head;
		head = head->next;
	}
	return NULL;
}

struct Node* Delete(struct Node* head, struct Node* removable)
{
	if (head == removable)
	{
		head = head->next;
		free(removable);
	}
	else
	{
		struct Node *prev = head;
		while (prev->next != removable)
		{
			prev = prev->next;
		}

		prev->next = removable->next;
		free(removable->Name_of_Variable);
		free(removable->Polynom);
		free(removable);
	}
	return head;
}


int check_variable(char* string)
{
     char* istr=NULL;
     istr=strtok(string,"=");
     if(istr[0]=='$')
     {
       if(istr[1]>='A' && istr[1]<='Z')
	 {
           if(strlen(istr)>2)
	   {
		  printf("Syntax error at %d\n",counter);
            	  yyerror("Variable writing rules violation\nAll variables should begin with $ sign and has only one capital letter and then '=' .Say like: $A,$B and so on. 			  Seems like you have written something wrong after variable");
	   }
	   else
		return 1;
         }
       else
         {
	    		  printf("Syntax error at %d\n",counter);
            yyerror("Variable writing rules violation\nAll variables should begin with $ sign and has only one capital letter and then '='.Say like: $A,$B and so on");
         } 
     }
     else
     {
        fprintf(stderr,"No spaces or non-variable letters allowed in front of assignment variables\n");
		    		  printf("Syntax error at %d\n",counter);
        yyerror("Variable writing rules violation\nAll variables should begin with $ sign and has only one capital letter and then '='.Say like: $A,$B and so on");
     }

}
int check_polynom(char* string)
{
     char* istr=NULL;
     istr=strtok(string,"=");
     istr=strtok(NULL,"=");
     int i=0;
     char loc_var=0;
     for(i=0;i<strlen(istr);i++)
     {
       if( (istr[i]>='A' && istr[i]<='Z') || istr[i]=='$' )
	{
	  return 0;
	}
       else
	 {
	    if(istr[i]=='+')
		continue;
	    else
		{
		  if(istr[i]=='-')
			continue;
		   else
		    {
			if(istr[i]=='(')
				continue;
			else
			  {
				if(istr[i]==')')
				     continue;
				else
				  {
				    if(isdigit(istr[i]))
					continue;
				    else
					{
					  if(istr[i]=='^')
						continue;
					  else
					     {
						if( isalpha(istr[i]) && islower(istr[i]) && isalpha(istr[i+1]) && islower(istr[i+1]) )
								{
								 printf("Error in writing variable's name.Should be used only one lowercase letter.\n");
								 yyerror("Syntax error");
								}
					       else
						 {
						  if( isalpha(istr[i]) && isupper(istr[i]) )
							continue;
						 else
						  {
						      if(istr[i]=='.')
							   continue;
						      else
						      {
							if(istr[i]=='\n')
								continue;
						       else
						       {
							
							if(isalpha(istr[i]) && islower(istr[i]))
						  	{
							if(loc_var==0)
							  loc_var=istr[i];
							else
							  {
								if(loc_var!=istr[i])
								{
								 printf("You cannot use more than one type of variable in one polynom\n");
								 yyerror("Lexical error");
								}
							  }
							
							continue;
							 }
							
					 	      else
						      {
							if(isalpha(istr[i]) && isdigit(istr[i+1]))
							{
							printf("%d%d is not allowed syntax (at %d string), use instead %d%d",istr[i],istr[i+1],counter,istr[i+1],istr[i]);
							yyerror("Syntax error");
							}
							else
							{
								if(istr[i]=='*')
							  	{
								printf("%c is a prohibited symbol(Syntax error) (at %d string) according to the grammar,use plain notation ,for example,as 2x but not 2*x ",istr[i],counter);
								yyerror("Syntax error");
							  	}
								else
							   	{
									if(istr[i]=='/')
									{
									  printf("Division is not possible at %d.",counter);
										yyerror("Lexical error");
									}
									else
									{
								printf("At %d.You might have misunderstood the rules:\nAll variables should begin with $ sign and has only one capital letter and then '='\nNo whitespaces allowed anywhere in polynom\n",counter);
								yyerror("Syntax error");
									}
							   	}
							}
						       }
						      }
						    }
						   }
						  }
					     }
					}
				  }
				
			  }
			
		    }

		}
	 }
     }
	return 1;
           
}
int check_polynom2(char* string)
{
    char* istr=NULL;
     istr=strtok(string,"=");
     istr=strtok(NULL,"=");
     int i=0;
	char loc_var=0;
     for(i=0;i<strlen(istr);i++)
     {
       if( (istr[i]=='$' && (istr[i+1]>='A' || istr[i+1]<='Z'))   )
	 {
	   continue;
  	 }
       else
	 {
	    if(istr[i]=='+')
		continue;
	    else
		{
		  if(istr[i]=='-')
			continue;
		   else
		    {
			if(istr[i]=='(')
				continue;
			else
			  {
				if(istr[i]==')')
				     continue;
				else
				  {
				    if(isdigit(istr[i]))
					continue;
				    else
					{
					  if(istr[i]=='^')
						continue;
					  else
					     {
						if( isalpha(istr[i]) && islower(istr[i]) && isalpha(istr[i+1]) && islower(istr[i+1]) )
								{
								 printf("Error in writing variable's name.Should be used only one lowercase letter.\n");
								 yyerror("Syntax error");
								}
						else
						{
						  if(isalpha(istr[i]) && isupper(istr[i]) )
							{
							  continue;
							}
						  else
						  {
						      if(istr[i]=='.')
							continue;
						      else
						      {
						       if(istr[i]=='\n')
								continue;
						      else
						      {
								
							if(isalpha(istr[i]) && islower(istr[i]))
						  	{
								if(loc_var==0)
							  	  loc_var=istr[i];
							 else
							   {
								if(loc_var!=istr[i])
								{
								 printf("You cannot use more than one type of variable in one polynom\n");
								 yyerror("Lexical error");
								}
							   }
							
							  continue;
						   	}
						       else
						       {
							if(isalpha(istr[i])&& isdigit(istr[i+1]))
							{
							printf("%d%d is not allowed syntax (at %d string), use instead %d%d",istr[i],istr[i+1],counter,istr[i+1],istr[i]);
							yyerror("Syntax error");
							}
							else
							{
								if(istr[i]=='*')
							  	{
								printf("%c is a prohibited symbol(Syntax error) (at %d string) according to the grammar,use plain notation ,for example,as 2x but not 2*x ",istr[i],counter);
								yyerror("Syntax error");
							  	}
								else
							   	{
								  if(istr[i]=='/')
									{
									  printf("Division is not possible at %d",counter);
										yyerror("Lexical error");
									}
								 else
								 {
								printf("At %d syntax error.You might have misunderstood the rules:\nAll variables should begin with $ sign and has only one capital letter and then '='\n No whitespaces allowed anywhere in polynom or in variable writing\n",counter);
								yyerror("Syntax error");
								 }
							   	}
							}
						       }
						      }
						     }
						    }
						  }
					     }
					}
				  }
				
			  }
			
		    }

		}
	 }
     }
	//
	i=0;
	int j=0;
	char* var_pointer=NULL;
        char buf[2048]="0";
	for(i=0;i<strlen(istr);i++,j++)
	{
	     var_pointer=strstr(istr+i,"$");
	     while(var_pointer!=NULL && var_pointer==(istr+i) )
	     {
		char name[10]="0";
		name[0]=(*var_pointer);
		name[1]=(*(var_pointer+1));//нашли имя переменной
		if(a==NULL)
		 {
		   printf("Stack has no such values(Syntax error at %d)\n",counter);
		   yyerror("Syntax error,you are trying to use variables but not any of them have been initialized");
		 }
		struct Node *del = Find(a, name);
		if(del==NULL)
		 {
			printf("You are trying to use variable %s which has no value(error at %d).It is a semantic mistake\n",name,counter);
		   yyerror("Semantic error");
		 }
		sprintf(buf+j,"%s",del->Polynom);
		j=strlen(buf);
		i+=2;
		var_pointer=strstr(istr+i,"$");
	     }
						
					 
	    buf[j]=istr[i];
	 }
	char buf2[2048]="0";
	strcat(buf2,"$A=");
	strcat(buf2,buf);
	if(!(check_polynom(buf2)) )
	{
	   return 0;
	}
	
	//
	return 1;
}
void check_variable_insert(char* string)
{
	char* istr=NULL;
    	istr=strtok(string,"=");
	sprintf(variable,"%s",istr);
    	istr=strtok(NULL,"=");
	int i=0;
	int j=0;
	char* var_pointer=NULL;
        char buf[2048]="0";
	for(i=0;i<strlen(istr);i++,j++)
	{
	     var_pointer=strstr(istr+i,"$");
	     while(var_pointer!=NULL && var_pointer==(istr+i) )
	     {
		char name[10]="0";
		name[0]=(*var_pointer);
		name[1]=(*(var_pointer+1));//нашли имя переменной
		if(a==NULL)
		 {
		   printf("Stack has no such values(Syntax error at %d)\n",counter); 
		   yyerror("Syntax error,you are trying to use variables but not any of them have been initialized");
		 }
		struct Node *del = Find(a, name);
		if(del==NULL)
		 {
			printf("You are trying to use variable %s which has no value(error at %d).It is a semantic mistake\n",name,counter);
		   yyerror("Semantic error");
		 }
		sprintf(buf+j,"%s",del->Polynom);
		j=strlen(buf);
		i+=2;
		var_pointer=strstr(istr+i,"$");
	     }
						
					 
	    buf[j]=istr[i];
	 }
	 add_caret_return(buf);
	 yy_scan_string(buf);
	 yyparse();
	 yylex_destroy();
} 
void add_caret_return(char* buf)
{
	if (buf[strlen(buf) - 1] == '\n')
		return;
	else
	{
		buf[strlen(buf) + 1] = '\0';
		buf[strlen(buf)] = '\n';
		
	}
}
void check_zero(char* a)
{
	if(!strcmp(a,"()"))
	{
	  int temp = strlen(a);
	a = (char*)realloc(a, strlen(a) + 2);
	a[temp - 1] = '0';
	a[temp] = ')';
	a[temp+1] = '\0';
	}
}
void check(char* string)
{
	if(strstr(string,"//"))// сначала мы ищем комментарий, и передаём его лексу
	{
		if(string[0]!='/')
		{
			printf("At line %d syntax error.",counter);
			yyerror("Error in writing comments. Use // at the very begginning of a string");
		}
		add_caret_return(string);
		yy_scan_string(string);
		yyparse();
		yylex_destroy();
	}
	else
	{
		if(strstr(string,"=") && (!strstr(string,"print")) )// Рассмотрим один из двух случаев: 1. $A=(полином)+или-или*(полином)
		// или 2. $C=($A+$B)+10x^228 
		{
			

		    
		    
		    if(check_variable(string))
		    {
			 string[strlen(string)]='=';
	    	  	 if(check_polynom(string))//1. $A=(полином)+или-или*(полином)
				{
				  char* istr=NULL;
				  string[strlen(string)]='=';
    					 istr=strtok(string,"=");
					 sprintf(variable,"%s",istr);
    					 istr=strtok(NULL,"=");
					 add_caret_return(istr);
					 yy_scan_string(istr);
					yyparse();
					yylex_destroy();
				}
			else 
			  {
				string[strlen(string)]='=';
				if(check_polynom2(string))//2. $C=($A+$B)+10x^228
				  {
					string[strlen(string)]='=';
			  		check_variable_insert(string);
				  }
                             
			  } 
		    }
		     
			
			
			
		}
		else
		 {
			if(strstr(string,"print"))
			{
			  char* istr1=NULL;
     			  istr1=strtok(string,"=");
			  if(!strcmp(istr1,"print"))
			  {
				string[strlen(string)]='=';
				if(check_polynom(string))//1. $A=(полином)+или-или*(полином)
				{
				 string[strlen(string)]='=';
				  char* istr=NULL;
    					 istr=strtok(string,"=");
					 sprintf(variable,"%s",istr);
    					 istr=strtok(NULL,"=");
					add_caret_return(istr);
					 yy_scan_string(istr);
					yyparse();
					yylex_destroy();
				}
				else
				{
					string[strlen(string)]='=';
					if(check_polynom2(string))//2. $C=($A+$B)+10x^228
			  		{
						string[strlen(string)]='=';
			     			check_variable_insert(string);
                             
			  		} 
				}
			  }
			  else
			  {
				printf("Undefined identificator at %d, did you mean print=$..?\n",counter);
				yyerror("Lexical error");
			  }
			  
     			  
			}
			else
			{
					add_caret_return(string);
					 yy_scan_string(string);
					yyparse();
					yylex_destroy();
			}
		 }
	
	}
	
	
	
}
int main (int argc, char** argv)
{

    const char* filename=argv[1];
    FILE* myfile=fopen(filename,"r");
    if(!myfile)
	{
	  printf("Can not open the file\n");
	  return -1;
	} 
    yyin=myfile;
    char buf[2048]={"0"};
    //int counter=1;
    while(!feof(yyin))
    {
    fgets(buf,2048,myfile);
    check(buf);// отправить на проверку прочитанную строку
     memset(variable,0,1024*sizeof(char));
    memset(buf,0,2048*sizeof(char));
    counter++;
	yyin=myfile;
    }
   /* printf("\n%s\n",buf);
     yy_scan_string(buf);
	yyparse();
	yylex_destroy();*/
    
    return 0;
}
