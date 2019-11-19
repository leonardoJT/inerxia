INPUT FROM "C:\info_fodun\credito-32434942.d".
FOR EACH creditos WHERE nit EQ "32434942" AND num_credito EQ 10442 :
    DELETE creditos.
END.

REPEAT:
    CREATE creditos.
    IMPORT creditos.
END.
INPUT CLOSE.
FOR EACH creditos WHERE nit EQ "32434942" AND num_credito EQ 10442 :
    UPDATE cuo_pagadas = 17.
END.

/*Borrar*/
FOR EACH CONTROL_pago WHERE CONTROL_pago.nit EQ "32434942" AND 
                     CONTROL_pago.num_credito = 10442:
    DELETE CONTROL_pago.
END.

FOR EACH planpagos WHERE planPagos.nit = "32434942"
                     AND planPagos.num_credito = 10442:
    DELETE planPagos.
END.


/*IMPORTAR DATOS ORIGINALES*/
/* INPUT FROM "C:\info_fodun\CONTROL_pago-32434942.d". */
/* REPEAT:                                             */
/*     CREATE CONTROL_pago.                            */
/*     IMPORT CONTROL_pago.                            */
/* END.                                                */
/* INPUT CLOSE.                                        */
/*                                                     */
/* INPUT FROM "C:\info_fodun\planpagos-32434942.d".    */
/* REPEAT:                                             */
/*     CREATE planPagos.                               */
/*     IMPORT planPagos.                               */
/* END.                                                */
/* INPUT CLOSE.                                        */

MESSAGE "fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
