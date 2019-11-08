/*
    Creado: Mayo 13/08
    Creado por: Luis Alberto 
    Modificado: GCamacho - Mayo 13/08 - Ajuste a sstandar del departamento de desarrollo   
*/

{incluido\iprmt_rpt.i}
{incluido\Variable.i "SHARED"}
        
DEFINE VARIABLE tfei AS DATE.
DEFINE VARIABLE tfef AS DATE.
DEFINE VARIABLE tmes AS INTEGER.
DEFINE VARIABLE tano AS INTEGER.
DEFINE VARIABLE TCpte LIKE Mov_Contable.Db.    
DEFINE VARIABLE viCnt AS INTEGER INITIAL 0 NO-UNDO.


ASSIGN tfei = pdt01
    tfef = pdt02
    tmes = MONTH(tfei)
    tano = YEAR(tfei).
    
IF P_NomArchivo EQ "DEFAULT" THEN
    ASSIGN P_NomArchivo = W_Pathspl + "DocsDescuadrados.txt".

OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).
        

{incluido\RepHeader.i}

VIEW FRAME F-Encabezado.
W_Reporte   = "REPORTE   : " + P_Titulo + " (prDocumDescruadrados.p) " 
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").


ASSIGN viCnt = 0.
FOR EACH Mov_contable field( cuenta num_docum agencia Fec_contab Comproban )  WHERE Mov_contable.cuenta NE " "  AND 
                fec_contab GE tfei AND 
                fec_contab LE tfef NO-LOCK
         BREAK BY Agencia BY comprob BY num_docum:
    ASSIGN Tcpte = Tcpte + (Mov_Contable.Db - Mov_Contable.Cr).

    FORM 
        Agencia        COLUMN-LABEL "Agencia"   
        Fec_contab         COLUMN-LABEL "Fecha" 
        Comproban         COLUMN-LABEL "Comprob" 
        Num_docum        COLUMN-LABEL "Documento"
        tcpte        COLUMN-LABEL "Diferencia"
    WITH FRAME a DOWN COLUMN 1 WIDTH 200
    NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
        
    IF LAST-OF(num_docum) THEN DO:
       IF Tcpte NE 0 THEN DO:
            DISPLAY 
                  Agencia  
                  Fec_contab 
                  Comproban  
                  Num_docum 
                  tcpte
              WITH FRAME a.
              DOWN WITH FRAME a.
           Tcpte = 0.
       END.
       ELSE
           ASSIGN viCnt = viCnt + 1.
    END.
END.   

IF viCnt EQ 0 THEN 
    DISPLAY "No se encontraron documentos descuadrados" WITH FRAME b.

OUTPUT CLOSE.
    
    
