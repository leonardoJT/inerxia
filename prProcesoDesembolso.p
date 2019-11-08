/*
    Reporte de Asesoría Credito PreScoring
Creado  : Giocam
Fecha   : Nov 18 / 2007
*/


{incluido\iprmt_rpt.i}
{incluido\Variable.i "SHARED"}

    DEFINE SHARED TEMP-TABLE TTimpresion
        FIELD   reg01       AS CHARACTER
        FIELD   reg02       AS CHARACTER
        FIELD   reg03       AS CHARACTER
        FIELD   reg04       AS CHARACTER
        FIELD   reg05       AS CHARACTER
        FIELD   reg06       AS CHARACTER
        FIELD   reg07       AS CHARACTER
        FIELD   reg08       AS CHARACTER
        FIELD   reg09       AS CHARACTER
        FIELD   reg10       AS CHARACTER
        FIELD   reg11       AS CHARACTER
        FIELD   reg12       AS CHARACTER
        FIELD   reg13       AS CHARACTER
        FIELD   condiciones AS CHARACTER.

DEFINE VARIABLE vcCliente AS CHARACTER   NO-UNDO.

IF P_NomArchivo EQ "DEFAULT" THEN
    ASSIGN P_NomArchivo = SESSION:TEMP-DIRECTORY + "ProcDesembolso.txt".

ASSIGN P_NumLineas = 73.
OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).

    {incluido\RepHeaderEspacio.i}
    
    VIEW FRAME F-Encabezado.
    W_Reporte   = "REPORTE   : " + P_Titulo + " (prProcesoDesembolso.p) " 
                  + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
FIND FIRST clientes WHERE clientes.nit EQ pc01 NO-LOCK NO-ERROR.
IF AVAILABLE clientes THEN 
    ASSIGN vcCliente = "Cliente                     : " + 
            clientes.nit + " - " + clientes.apellido1 + " " + clientes.apellido2 + " " + clientes.nombre.
ELSE
    ASSIGN vcCliente = "N.N.".
FOR EACH TTimpresion NO-LOCK:
    FORM
        reg01       FORMAT "X(120)"   AT ROW 1    COLUMN  1   
        vcCliente   FORMAT "X(120)"   AT ROW 2    COLUMN  1
        reg02       FORMAT "X(120)"   AT ROW 3    COLUMN  1
        reg03       FORMAT "X(120)"   AT ROW 4    COLUMN  1
        reg04       FORMAT "X(120)"   AT ROW 5    COLUMN  1
        reg05       FORMAT "X(120)"   AT ROW 6    COLUMN  1
        reg06       FORMAT "X(120)"   AT ROW 7    COLUMN  1
        reg07       FORMAT "X(120)"   AT ROW 8    COLUMN  1
        reg08       FORMAT "X(120)"   AT ROW 9    COLUMN  1
        reg09       FORMAT "X(120)"   AT ROW 10   COLUMN  1
        reg10       FORMAT "X(120)"   AT ROW 11   COLUMN  1
        reg11       FORMAT "X(120)"   AT ROW 12   COLUMN  1
        reg12       FORMAT "X(120)"   AT ROW 13   COLUMN  1
        reg13       FORMAT "X(120)"   AT ROW 14  COLUMN   1
        condiciones AT ROW 16   COLUMN  1   VIEW-AS EDITOR INNER-CHARS 120 INNER-LINES 1

        WITH FRAME FProc DOWN COLUMN 1 WIDTH 130
        NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO OVERLAY FONT 0 NO-LABEL .
/*         SIDE-LABELS OVERLAY FONT 0 . */

    DISPLAY 
         reg01       
         vcCliente
         reg02       
         reg03       
         reg04       
         reg05       
         reg06       
         reg07       
         reg08       
         reg09       
         reg10       
         reg11       
         reg12       
         reg13       
         condiciones 
        WITH FRAME FProc.
    DOWN WITH FRAME FProc.

    VIEW FRAME f-pie.

END.


OUTPUT CLOSE.

