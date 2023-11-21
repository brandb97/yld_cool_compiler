/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
int comment_deep = 0;
bool cmt_expect_star = false;
bool cmt_expect_rpar = false;

%}

%s COMMENT COMMENT_DASHES STRING STRING_ERR

/*
 * Define names for regular expressions here.
 */

DARROW          =>
ASSIGN          <-
LE              <=
WHITESPACE      [\f\r\t\v' ']
DIGIT           [0-9]

%%

{WHITESPACE}* 

<INITIAL,COMMENT>"\n" curr_lineno++;

 /*
  *  Nested comments
  */
<INITIAL>"*)" {
  cool_yylval.error_msg = "Unmatched *)";
  return (ERROR);
}

<INITIAL>"(*" {
  BEGIN(COMMENT);
  comment_deep = 1;
}

<COMMENT>[^(*)\n]* { 
  cmt_expect_star = false;
  cmt_expect_rpar = false; 
}

<COMMENT>"(" {
  cmt_expect_star = true;
  cmt_expect_rpar = false;
}

<COMMENT>"*" {
  cmt_expect_rpar = false;
  if (cmt_expect_star) {
    comment_deep++;
  } else {
    cmt_expect_rpar = true;
  }
  cmt_expect_star = false;
}

<COMMENT>")" {
  cmt_expect_star = false;
  if (cmt_expect_rpar) {
    comment_deep--;
    if (comment_deep == 0) {
      BEGIN(INITIAL);
    }
  }
  cmt_expect_rpar = false;
}

<COMMENT>EOF {
  cool_yylval.error_msg = "EOF in comment";
  return (ERROR);
}

<INITIAL>"--" { BEGIN(COMMENT_DASHES); }

<COMMENT_DASHES>[^\n]*

<COMMENT_DASHES>EOF

<COMMENT_DASHES>"\n" {
  BEGIN(INITIAL);
  curr_lineno++;
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
<INITIAL>"\"" { BEGIN(STRING); string_buf_ptr = string_buf; }
<STRING>[^"\n\0]* {
  /* copy yytext to global value */
  int i = 0;
  while (string_buf_ptr - string_buf < MAX_STR_CONST && yytext[i]) {
    char c = yytext[i++];
    if (c == '\\') {
      char esacped_c = yytext[i];
      i += 1;

      switch (esacped_c) {
      case 'n':
        *string_buf_ptr++ = '\n';
        break;
      case 'b':
        *string_buf_ptr++ = '\b';
      case 't':
        *string_buf_ptr++ = '\t';
        break;
      case 'f':
        *string_buf_ptr++ = '\f';
        break;
      default:
        *string_buf_ptr++ = esacped_c;
        break;
      }
    } else {
      *string_buf_ptr++ = c;
    }
  }

  if (string_buf_ptr - string_buf >= MAX_STR_CONST) {
    BEGIN(STRING_ERR);
    cool_yylval.error_msg = "String constant too long";
    return (ERROR);
  } else {
    *string_buf_ptr++ = '\0';
  }
}

<STRING>"\0" {
  BEGIN(STRING_ERR);
  cool_yylval.error_msg = "String contains null character";
  return (ERROR);
}
<STRING>"\n" {
  BEGIN(STRING_ERR);
  cool_yylval.error_msg = "Unterminated string constant";
  curr_lineno++;
  return (ERROR);
}
<STRING,STRING_ERR>EOF {
  cool_yylval.error_msg = "EOF in string constant";
  return (ERROR);
}

<STRING_ERR>[^\n"]*

<STRING_ERR>("\n"|"\"") { 
  BEGIN(INITIAL);
  if (yytext[0] == '\n') {
    curr_lineno++;
  }
}

<STRING>"\"" { 
  BEGIN(INITIAL);
  cool_yylval.symbol = stringtable.add_string(string_buf);
  return (STR_CONST);
}

 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); }
{ASSIGN}    { return (ASSIGN); }
{LE}        { return (LE); }
"+"         { return '+'; }
"-"         { return '-'; }
"*"         { return '*'; }
"/"         { return '/'; }
"~"         { return '~'; }
"<"         { return '<'; }
"="         { return '='; }
"("         { return '('; }
")"         { return ')'; }
"."         { return '.'; }
"@"         { return '@'; }
":"         { return ':'; }
","         { return ','; }
";"         { return ';'; }
"{"         { return '{'; }
"}"         { return '}'; }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
(?i:class)       { return (CLASS); }
(?i:else)        { return (ELSE); }
(?i:fi)          { return (FI); }
(?i:if)          { return (IF); }
(?i:in)          { return (IN); }
(?i:inherits)    { return (INHERITS); }
(?i:let)         { return (LET); }
(?i:loop)        { return (LOOP); }
(?i:pool)        { return (POOL); }
(?i:then)        { return (THEN); }
(?i:while)       { return (WHILE); }
(?i:case)        { return (CASE); }
(?i:esac)        { return (ESAC); }
(?i:of)          { return (OF); }
(?i:new)         { return (NEW); }
(?i:isvoid)      { return (ISVOID); }
(?i:not)         { return (NOT); }

 /*
  * Boolean
  */
t(?i:rue)        { cool_yylval.boolean = 1; return (BOOL_CONST); }
f(?i:alse)       { cool_yylval.boolean = 0; return (BOOL_CONST); }


 /*
  * Integers
  */
{DIGIT}+ { cool_yylval.symbol = inttable.add_string(yytext); return (INT_CONST); }

 /*
  * Object Identifiers
  */
[a-z]([a-zA-Z]|{DIGIT}|"_")* { cool_yylval.symbol = idtable.add_string(yytext); return (OBJECTID); }


 /*
  * Type Identifiers
  */
[A-Z]([a-zA-Z]|{DIGIT}|"_")* { cool_yylval.symbol = idtable.add_string(yytext); return (TYPEID); }

 /*
  * Unrecognized character
  */
. { cool_yylval.error_msg = yytext; return (ERROR); }
 
%%
