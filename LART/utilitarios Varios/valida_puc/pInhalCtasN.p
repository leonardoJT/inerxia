/* Created by: Edgar Ortiz
   Date: 13 agosto de 2008 
   Description: Listar cuentas del PUC que empiezan por cualquier caracter 
   Parameters: pc01 = filtra por begins o por igual  pc02 = cuenta a filtrar */
   
/*--------------------------------------*/

{incluido\iprmt_rpt.i}
{incluido\Variable.i "SHARED"}
        
DEFINE VARIABLE vc_stado AS CHARACTER FORMAT "X(12)" NO-UNDO.


IF P_NomArchivo EQ "DEFAULT" THEN
    ASSIGN P_NomArchivo = W_Pathspl + "\CuentasXInhal.txt".

 
OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).
    
{incluido\RepHeader.i}

VIEW FRAME F-Encabezado.
W_Reporte   = "REPORTE   : " + P_Titulo + " (pInhalCtasN.p) " 
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

FORM 
    cuentas.cuenta      COLUMN-LABEL "CUENTA"   
    Cuentas.nombre      COLUMN-LABEL "NOMBRE" 
    Cuentas.naturaleza  COLUMN-LABEL "NATURALEZA"
    vc_stado            COLUMN-LABEL "ESTADO"
    cuentas.fec_retiro  COLUMN-LABEL "FECHA INACTIVA"
    
WITH FRAME a DOWN COLUMN 1 WIDTH 200
NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    
FOR EACH cuentas WHERE  (IF INT(pc01) = 1 THEN cuentas.cuenta BEGINS pc02 ELSE cuentas.cuenta = pc02 ) NO-LOCK:
  ASSIGN vc_stado = IF cuentas.estado = 1 THEN "Activa" ELSE "Inactiva".
  DISPLAY cuentas.cuenta 
          cuentas.nombre
          cuentas.naturaleza
          vc_stado
          cuentas.fec_retiro  WITH FRAME a.
  DOWN WITH FRAME a.
END.
OUTPUT CLOSE.
