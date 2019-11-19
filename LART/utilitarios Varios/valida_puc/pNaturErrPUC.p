/* Created by: Edgar Ortiz
   Date: 13 agosto de 2008 
   Description: Permite validar la naturaleza de las cuentas del puc de acuerdo a la naturaleza de la cuenta mayor (padre) */
/*--------------------------------------*/

{incluido\iprmt_rpt.i}
{incluido\Variable.i "SHARED"}

DEFINE VARIABLE vc_stado AS CHARACTER NO-UNDO INITIAL 'Activa'.
DEFINE VARIABLE vc_cpto  AS CHARACTER FORMAT "X(35)" NO-UNDO.
DEFINE TEMP-TABLE TT_cta 
  FIELD T_cuenta AS CHARACTER FORMAT "X" 
  FIELD T_Nombre LIKE Cuentas.Nombre
  FIELD T_Nleza  LIKE Cuentas.Naturaleza
  FIELD T_id     LIKE cuentas.id_cuenta
INDEX XPKTT_cta IS UNIQUE PRIMARY T_Cuenta.


IF P_NomArchivo EQ "DEFAULT" THEN
    ASSIGN P_NomArchivo = W_Pathspl + "\ErrNatuPUC.txt".

 
OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).
    
{incluido\RepHeader.i}

VIEW FRAME F-Encabezado.
W_Reporte   = "REPORTE   : " + P_Titulo + " (pNaturErrPUC.p) " 
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

FORM 
    cuentas.cuenta      COLUMN-LABEL "CUENTA"   
    Cuentas.nombre      COLUMN-LABEL "NOMBRE" 
    T_Nombre            FORMAT "X(20)" COLUMN-LABEL "CLASIFICACIÓN" 
    Cuentas.naturaleza  COLUMN-LABEL "NATURALEZA"
    vc_stado            COLUMN-LABEL "ESTADO"
    vc_cpto             COLUMN-LABEL "CONCEPTO"
    
WITH FRAME a DOWN COLUMN 1 WIDTH 350
NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    
FOR EACH cuentas WHERE LENGTH(cuentas.Cuenta) = 1 AND cuentas.estado = 1 NO-LOCK:
 FIND FIRST TT_Cta WHERE T_Cuenta = cuentas.cuenta NO-LOCK NO-ERROR.
 IF NOT AVAILABLE TT_Cta THEN DO:
   CREATE TT_Cta.
   ASSIGN T_Cuenta = cuentas.cuenta
          T_Nombre = cuentas.nombre
          T_Nleza  = cuentas.naturaleza
          T_Id     = cuentas.id_cuenta.
 END.
END.

FOR EACH  TT_Cta NO-LOCK:
  FOR EACH cuentas WHERE cuentas.cuenta BEGINS TT_Cta.T_Cuenta AND estado = 1 NO-LOCK:
    IF cuentas.naturaleza <> TT_Cta.T_Nleza THEN DO:
      ASSIGN vc_cpto = IF naturaleza = 'cr' THEN "Naturaleza debería ser Débito"
              ELSE "Naturaleza debería ser Crédito".
      DISPLAY cuentas.cuenta 
              cuentas.nombre
              T_Nombre
              cuentas.naturaleza
              vc_stado
              vc_cpto WITH FRAME a.
      DOWN WITH FRAME a.
    END.
  END.
END.
OUTPUT CLOSE.
