
/* Abre un Excel Existente y Escribe en èl */

DEF VAR i           AS INT NO-UNDO.
DEF VAR Pos         AS INT NO-UNDO.
DEF VAR Car         AS INT NO-UNDO.
DEF VAR cFile       AS CHA NO-UNDO.
DEF VAR ValCol      AS CHA NO-UNDO.
DEF VAR E_Fila      AS CHA NO-UNDO.  /* (5) Encabezado de las Columnas */
DEF VAR E_Fila2     AS CHA NO-UNDO.  /* (5) Contenido de las Columnas  */
DEF VAR Columnas    AS CHA NO-UNDO  INIT "ABCDEFGHIJKLMNOPQRST".
DEF VAR E_NumFila   AS INT NO-UNDO.  /* (4) Numero de la Fila          */
DEF VAR cFullFile   AS CHA NO-UNDO.
DEF VAR E_NumColumn AS INT NO-UNDO.  /* (3) Numero de Columnas         */
DEF VAR hDoc        AS COM-HANDLE.
DEF VAR chExcelApp  AS COM-HANDLE.
DEF VAR chWorksheet AS COM-HANDLE.

   CREATE "Excel.Application" chExcelApp.
   cFile = "C:\Info_Utrahuilca\Prueba.xls".
   cFullFile = SEARCH(cFile).
   IF cFullFile EQ ? THEN DO:
      MESSAGE cFile "no Encontrado" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   hDoc = chExcelApp:Workbooks:OPEN(cFile,).
/* MANDA ENCABEZADO DE HOJA DE CALCULO*/
   ASSIGN Pos = 1 E_NumFila = 3 E_NumColumn = 4. /* Posicion Inicial, Fila y Nro de Columnas  */
   E_Fila = "010" + "FECHA     "                 /* Ancho de la columna + Contenido */
          + "011" + "COMPROBANTE"      
          + "007" + "AGENCIA"
          + "015" + "VALOR".
   chExcelApp:Visible = TRUE.                /* launch Excel so it is visible to the user */
   chWorkSheet = chExcelApp:Sheets:Item(1).  /* get the active Worksheet                  */
   FOR EACH mov_contable NO-LOCK WHERE nit = "55160966".
       E_Fila2 = "010" + STRING(Mov_Contable.Fec_Contable,"99/99/9999")
               + "011" + STRING(Mov_Contable.Comprobante,"ZZZZZZZZZZZ")
               + "007" + STRING(Mov_Contable.Agencia,"ZZZZZZZ")
               + "015" + STRING(Mov_Contable.db,"->>>,>>>,>>>.99").
       IF E_NumFila EQ 3 THEN DO:
          DO I = 1 TO E_NumColumn:                               /* 1 TO Numero de Columnas                   */
             Car = INT(SUBSTR(E_Fila,Pos,3)).                    /* Car = ancho de la columna                 */
             ValCol = SUBSTR(Columnas,i,1).                      /* ValCol = Letra de la columna: A, B, C.... */
             chWorkSheet:Columns(ValCol):ColumnWidth = INT(Car). /* Fija el ancho de la columna               */
             ValCol = ValCol + STRING(E_NumFila).
             chWorkSheet:Range(ValCol):Value = STRING(SUBSTR(E_Fila,Pos + 3,Car)).
             Pos = Pos + Car + 3.
          END.
          E_NumFila = E_NumFila + 1.
          chWorkSheet:Range("A1:" + SUBSTR(Columnas,E_NumColumn,1) + "1"):Font:Bold = TRUE. /* Pone Negrilla a la Fila */
       END.
       ASSIGN Pos = 1 E_NumFila = E_NumFila + 1.
       DO I = 1 TO E_NumColumn:
          Car = INT(SUBSTR(E_Fila2,Pos,3)).
          ValCol = SUBSTR(Columnas,i,1) + STRING(E_NumFila).
          chWorkSheet:Range(ValCol):Value = STRING(SUBSTR(E_Fila2,Pos + 3,Car)).
          Pos = Pos + Car + 3.
       END.
   END.
   chExcelApp:displayalerts = FALSE.
   chExcelApp:ActiveWorkbook:SaveAs("c:\Info_Utrahuilca\Formato" + "_" + "Prueba",1,"","",FALSE,FALSE,,).
   chExcelApp:QUIT().
   RELEASE OBJECT hDoc.
   RELEASE OBJECT chExcelApp.      
   RELEASE OBJECT chWorksheet.
