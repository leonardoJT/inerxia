/*
    Creado: Mayo 13/08
    Creado por: Luis Alberto 
    Modificado: GCamacho - Mayo 13/08 - Ajuste a sstandar del departamento de desarrollo   
*/

{incluido\iprmt_rpt.i}
{incluido\Variable.i "SHARED"}
        
DEFINE VARIABLE Tdb  LIKE Mov_Contable.Db NO-UNDO.
DEFINE VARIABLE Tcr  LIKE Mov_Contable.Cr NO-UNDO.
DEFINE VARIABLE Tcta LIKE cuentas.cuenta  NO-UNDO.
DEFINE VARIABLE tsde LIKE Sal_Cuenta.Db[1] NO-UNDO.
DEFINE VARIABLE tscr LIKE Sal_Cuenta.Cr[1] NO-UNDO.
DEFINE VARIABLE tfei AS DATE NO-UNDO.
DEFINE VARIABLE tfef AS DATE NO-UNDO.
DEFINE VARIABLE tmes AS INTEGER NO-UNDO.
DEFINE VARIABLE tano AS INTEGER NO-UNDO.
/* Define Variables Totales */
DEFINE VARIABLE Tgdbm  LIKE Mov_Contable.Db NO-UNDO.
DEFINE VARIABLE Tgcrm  LIKE Mov_Contable.Cr NO-UNDO.
DEFINE VARIABLE Tgdbs  LIKE Mov_Contable.Db NO-UNDO.
DEFINE VARIABLE Tgcrs  LIKE Mov_Contable.Cr NO-UNDO.
DEFINE VARIABLE Tgdbd  LIKE Mov_Contable.Db NO-UNDO.
DEFINE VARIABLE Tgcrd  LIKE Mov_Contable.Cr NO-UNDO.
    
DEFINE VARIABLE vdeDifDb LIKE Mov_Contable.Db NO-UNDO.
DEFINE VARIABLE vdeDifCr LIKE Mov_Contable.Db NO-UNDO.

/*--------------------------------------*/


ASSIGN tfei = pdt01
    tfef = pdt02
    tmes = MONTH(tfei)
    tano = YEAR(tfei).
    
IF P_NomArchivo EQ "DEFAULT" THEN
    ASSIGN P_NomArchivo = W_Pathspl + "\MvtoVsSaldo.txt".

MESSAGE W_Pathspl
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

 
OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).
    
{incluido\RepHeader.i}

VIEW FRAME F-Encabezado.
W_Reporte   = "REPORTE   : " + P_Titulo + " (prMovContaVsSaldos.p) " 
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

FORM 
    Tcta        COLUMN-LABEL "CUENTA"   
    tdb         COLUMN-LABEL "DB_MOVTO" 
    tcr         COLUMN-LABEL "CR_MOVTO" 
    tsde        COLUMN-LABEL "DB_SALDOS"
    tscr        COLUMN-LABEL "CR_SAlDOS"
    vdeDifDb    COLUMN-LABEL "DBM-DBS"  
    vdeDifCr    COLUMN-LABEL "CRM-CRS" 
        
WITH FRAME a DOWN COLUMN 1 WIDTH 200
NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    

FOR EACH Mov_contable FIELDS(cuenta db cr fec_contab)
    WHERE Mov_contable.cuenta NE " " AND
                Mov_contable.fec_contab GE tfei AND
                Mov_contable.fec_contab LE tfef NO-LOCK 
         BREAK BY Mov_contable.cuenta :
    ASSIGN Tdb = Tdb + Mov_Contable.Db 
           Tcr = Tcr +  Mov_Contable.Cr
           Tcta = Mov_contable.cuenta.
    IF LAST-OF(Mov_contable.cuenta) THEN DO:
        FOR EACH Sal_Cuenta WHERE  Sal_Cuenta.Ano = tano 
                 AND Sal_Cuenta.cuenta = Tcta NO-LOCK :
                  ASSIGN tsde = tsde + Sal_Cuenta.Db[tmes]
                         tscr = tscr + Sal_Cuenta.Cr[tmes].
        END. 

/*        FOR FIRST Sal_Cuenta fieldS(cuenta ano db cr) WHERE  Sal_Cuenta.cuenta = Tcta AND */
/*                 Sal_Cuenta.Ano = tano USE-INDEX idctaano NO-LOCK :                       */
/*                  ASSIGN tsde = tsde + Sal_Cuenta.Db[tmes]                                */
/*                         tscr = tscr + Sal_Cuenta.Cr[tmes].                               */
/*        END.                                                                              */
        ASSIGN vdeDifDb = (tdb - tsde)
               vdeDifCr = (tcr - tscr).
            
        DISPLAY 
                Tcta     
                tdb      
                tcr      
                tsde     
                tscr     
                vdeDifDb 
                vdeDifCr 
        WITH FRAME a.
        DOWN WITH FRAME a.
                            
       ASSIGN Tgdbm  = Tgdbm + Tdb
            Tgcrm  = Tgcrm + tcr
            Tgdbs  = Tgdbs + tsde
            Tgcrs  = Tgcrs + tscr
            Tgdbd  = Tgdbd + (tdb - tsde)
            Tgcrd  = Tgcrd + tcr - tscr
            Tdb = 0.
            tcr = 0.
            tsde = 0.
            tscr = 0.
    END.
END.
    
    
DISPLAY  
    "Totales" @ Tcta 
    Tgdbm     @ tdb     
    Tgcrm     @ tcr     
    Tgdbs     @ tsde    
    Tgcrs     @ tscr    
    Tgdbd     @ vdeDifDb
    Tgcrd     @ vdeDifCr
WITH FRAME a.        

OUTPUT CLOSE.
    
    
