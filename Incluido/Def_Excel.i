 DEFINE VARIABLE E_NumColumn AS INTEGER   FORMAT 99.          /* (3) Numero de Columnas que se mandaran*/
 DEFINE VARIABLE E_NumFila   AS INTEGER   FORMAT 9999.        /* (4) Numero de la Fila que se manda*/
 DEFINE VARIABLE E_Fila      AS CHARACTER FORMAT "X(200)".    /* (5) Informacion del informe a tabular*/
 DEFINE VARIABLE E_Fila2     AS CHARACTER FORMAT "X(200)".    /* (5) Informacion del informe a tabular*/
 DEFINE VARIABLE E_CmpGrafic AS CHARACTER FORMAT "X(200)".    /* (5) Informacion del informe a tabular*/


 DEFINE VARIABLE chWorkbook         AS COM-HANDLE.
 DEFINE VARIABLE chWorksheet        AS COM-HANDLE.
 DEFINE VARIABLE chChart            AS COM-HANDLE.
 DEFINE VARIABLE chWorksheetRange   AS COM-HANDLE.
    
 DEFINE VARIABLE Columnas           AS CHARACTER FORMAT "X" INITIAL "ABCDEFGHIJKLMNOPQRST".
 DEFINE VARIABLE ValCol             AS CHARACTER FORMAT "X(3)".

 DEFINE VARIABLE i                  AS INTEGER FORMAT 99.
 DEFINE VARIABLE Pos                AS INTEGER FORMAT 999 INITIAL 1.
 DEFINE VARIABLE Car                AS INTEGER FORMAT 999.

 DEFINE VARIABLE chExcelApp      AS COM-HANDLE.
 CREATE "Excel.Application" chExcelApp.
