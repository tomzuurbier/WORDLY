# wordly

Content:
dbquery.cob - database query subprogram  source file
wordl.cob   - wordl source file
wordly.csv  - Db2 WORDLY table export file in "of del" [delimited ASCII] format.


In order to compile a program containing EXEC SQL statements, the compiler must be able to connect to the database that the program will use.
You can specify the databse by either of these means:
1. Use the DATABASE suboption in the SQL option, like:
cob2_db2 -q"sql('database dbname')" MYSQL.cbl

2. Set the DB2DBDFT environment variable prior to invoking the compiler.
i.e. export DB2DBDFT=dbname

How to compile:
cob2 -I ./copybooks wordl.cob -q32  -qNODYNAM -c
cob2 wordl.o dbquery.o -q32 -I$HOME/sqllib/include/cobol_a -L$HOME/sqllib/lib32 -ldb2
cob2 wordl.o dbquery.o -q32 -I$HOME/sqllib/include/cobol_a -L$HOME/sqllib/lib32 -ldb2 -o wordly

usage:
invoke wordly from command line.
enter searchstring (* represents wildcard)
to exit enter "9"
~

