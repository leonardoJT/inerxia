/* created by: Edgar Ortiz
   Date: 13 de agoto de 2008 
   Description: Inactiva aquellas cuentas definidas en el filtro con parametros recibidos desde wRP-Homol_Ctas.w */

DEFINE INPUT PARAMETER pc01 AS CHARACTER FORMAT "X" NO-UNDO.     /* Tipo de filtro */
DEFINE INPUT PARAMETER pc02 AS CHARACTER FORMAT "X(14)" NO-UNDO. /* Cuenta objeto del filtro */

FOR EACH cuentas WHERE  (IF INT(pc01) = 1 THEN cuentas.cuenta BEGINS pc02 ELSE cuentas.cuenta = pc02 ) AND estado = 1 SHARE-LOCK:
    ASSIGN cuentas.estado = 2
           Cuentas.Fec_Retiro = TODAY.
END.




  

