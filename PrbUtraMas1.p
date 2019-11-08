{INCLUIDO\VARIABLE.I "SHARED"}
/* Abre un Excel Existente y Escribe en èl */
DEF VAR Dato          AS CHA NO-UNDO.
DEF VAR ValCol        AS CHA NO-UNDO.
DEF VAR SwExiste      AS CHA NO-UNDO.
DEF VAR InputFile     AS CHA NO-UNDO.
DEF VAR PrinterName   AS CHA NO-UNDO.
DEF VAR Tasa_Efectiva AS DEC NO-UNDO.
DEF VAR chExcelApp    AS COM-HANDLE NO-UNDO.
DEF VAR hWorkbooks    AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheet   AS COM-HANDLE NO-UNDO.

   FIND LAST Ahorros WHERE nit = "55160966" AND cod_aho = 18 NO-LOCK NO-ERROR.
   FIND Clientes OF Ahorros NO-LOCK NO-ERROR.
   FIND Agencias OF Ahorros NO-LOCK NO-ERROR.
   FIND Usuarios WHERE Usuarios.Usuario = W_Usuario NO-LOCK NO-ERROR.
   
   CREATE "Excel.Application" chExcelApp.
   InputFile = "c:\sfg\objetos\Formatos\AP - 300.xls".
   SwExiste = SEARCH(InputFile).
   IF SwExiste EQ ? THEN DO:
      MESSAGE InputFile "no Encontrado" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   hWorkbooks = chExcelApp:Workbooks:OPEN(InputFile,,TRUE,).
   chExcelApp:Visible = TRUE.
   chWorkSheet = chExcelApp:Sheets:Item(1).

   ASSIGN ValCol = "B7"  Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
   RUN LlenarCelda.
   ASSIGN ValCol = "AB7" Dato = Clientes.Nit.
   RUN LlenarCelda.
   ASSIGN ValCol = "A11" Dato = Agencias.Nombre.
   RUN LlenarCelda.
   ASSIGN ValCol = "G11" Dato = Ahorros.Cue_Ahorro.
   RUN LlenarCelda.
   ASSIGN ValCol = "K11" Dato = STRING(Ahorros.Cuota,"->>>,>>>,>>>.99").
   RUN LlenarCelda.
   ASSIGN ValCol = "P11" Dato = STRING(ROUND(Ahorros.Monto / Ahorros.Cuota,0) ,"->>>,>>>,>>>.99").
   RUN LlenarCelda.
   ASSIGN ValCol = "T11" Dato = STRING(Ahorros.Monto,"->>>,>>>,>>>.99").
   RUN LlenarCelda.
   ASSIGN ValCol = "Z11" Dato = STRING(Ahorros.Tasa,"->>>.99").
   RUN LlenarCelda.
/* RUN EFNV IN W_ManFin (Ahorros.Tasa,ROUND(Ahorros.Monto / Ahorros.Cuota,0),OUTPUT Tasa_Efectiva).*/
   ASSIGN ValCol = "AC11" Dato = STRING(Tasa_Efectiva,"->>>.99").
   RUN LlenarCelda.
   ASSIGN ValCol = "J13" Dato = STRING(Ahorros.Fec_Vencimiento,"99/99/9999").
   RUN LlenarCelda.
   ASSIGN ValCol = "AH27" Dato = Usuarios.Nombre.
   RUN LlenarCelda.

   SYSTEM-DIALOG PRINTER-SETUP. 
   PrinterName = SESSION:PRINTER-NAME.
   hWorkbooks:PrintOut(1,1,1,FALSE,PrinterName,).
   chExcelApp:displayalerts = FALSE.
   chExcelApp:Application:Workbooks:CLOSE() NO-ERROR.
   chExcelApp:Application:QUIT NO-ERROR.
   RELEASE OBJECT hWorkbooks.
   RELEASE OBJECT chExcelApp.      
   RELEASE OBJECT chWorksheet.

   PROCEDURE LlenarCelda.
     chWorkSheet:Range(ValCol):Value = Dato.
   END PROCEDURE.

