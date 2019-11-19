/*
    Creado: Mayo 13/08
    Creado por: Luis Alberto 
    Modificado: GCamacho - Mayo 13/08 - Ajuste a sstandar del departamento de desarrollo   
*/

{incluido\iprmt_rpt.i}
{incluido\Variable.i "SHARED"}
        
DEFINE VARIABLE IPC_TIPO     AS CHARACTER FORMAT "X" INITIAL '2' NO-UNDO.
DEFINE VARIABLE Tcta         LIKE bdcentral.cuentas.cuenta     NO-UNDO.
DEFINE VARIABLE tnombreCta   LIKE bdcentral.cuentas.Nombre     NO-UNDO.
DEFINE VARIABLE tTipcta      LIKE bdcentral.cuentas.tipo       NO-UNDO.
DEFINE VARIABLE tNatural     LIKE bdcentral.cuentas.naturaleza NO-UNDO.
DEFINE VARIABLE tctaHom      LIKE bdcentral.cuentas.cuenta     NO-UNDO. 
DEFINE VARIABLE tNomCtaHom   LIKE bdcentral.cuentas.Nombre     NO-UNDO. 
DEFINE VARIABLE tTipCtaHom   LIKE bdcentral.cuentas.tipo       NO-UNDO. 
DEFINE VARIABLE tNatuHom     LIKE bdcentral.cuentas.naturaleza NO-UNDO. 
DEFINE VARIABLE tcpto        AS   CHARACTER FORMAT "X(35)" NO-UNDO.


/*--------------------------------------*/


IF P_NomArchivo EQ "DEFAULT" THEN
    ASSIGN P_NomArchivo = W_Pathspl + "\SinCtaHomol.txt".

 
OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).
    
{incluido\RepHeader.i}

VIEW FRAME F-Encabezado.
W_Reporte   = "REPORTE   : " + P_Titulo + " (prMovContaVsSaldos.p) " 
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

FORM 
    Tcta        COLUMN-LABEL "CUENTA"   
    tnombreCta  COLUMN-LABEL "NOMBRE" 
    tTipcta     COLUMN-LABEL "TIPO" 
    tNatural    COLUMN-LABEL "NATURALEZA"
    tctaHom     COLUMN-LABEL "CUENTA HOMOLOGA"
    tcpto       COLUMN-LABEL "CONCEPTO"
WITH FRAME a DOWN COLUMN 1 WIDTH 200
NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    

FOR EACH bdcentral.cuentas WHERE IF INT(pc01) = 3 THEN bdcentral.cuentas.tipo > 0 ELSE bdcentral.cuentas.tipo = INT(pc01) AND 
                                 (bdcentral.cuentas.cta_homologada = '' OR bdcentral.cuentas.cta_homologada = ?) NO-LOCK:
    ASSIGN Tcta     = bdcentral.cuentas.Cuenta
           tnombreCta = bdcentral.cuentas.nombre 
           tTipcta    = bdcentral.cuentas.tipo 
           tNatural   = bdcentral.cuentas.naturaleza
           tctaHom    = bdcentral.cuentas.cta_homologada
           tCpto      = IF (bdcentral.cuentas.cta_homologada = '' OR bdcentral.cuentas.cta_homologada = ?) 
                         THEN 'Sin Cuenta Homologa ' ELSE 'Cuenta Homologa NO existe en Multi'.
    
    DISPLAY Tcta     
            tnombreCta 
            tTipcta    
            tNatural   
            tctaHom    
            tcpto WITH FRAME A.
      DOWN WITH FRAME a.
END.

OUTPUT CLOSE.
    
    
