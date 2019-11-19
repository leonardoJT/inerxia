DEFINE VAR cont AS INTEGER.

DEFINE TEMP-TABLE fechas
    FIELD nit AS CHARACTER
    FIELD linea AS INTEGER
    FIELD num_credito AS INTEGER
    FIELD fecha AS DATE.

INPUT FROM c:\INFO_Fodun\Leonardo\Fechas.csv.
REPEAT:
    CREATE fechas.
    IMPORT DELIMITER ";" fechas NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
        DELETE fechas.
END.
INPUT CLOSE.

FOR EACH fechas WHERE num_credito <> 0 NO-LOCK:
    FIND FIRST creditos WHERE creditos.nit = fechas.nit
                          AND creditos.num_credito = fechas.num_credito
                          /*AND creditos.sdo_capital > 0*/ NO-ERROR.
    IF AVAILABLE creditos THEN DO:
        creditos.fec_pago = fechas.fecha.
        creditos.dias_atraso = 0.
        creditos.cuo_atraso = 0.

        FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                 AND CONTROL_pagos.num_credito = creditos.num_credito
                                 AND CONTROL_pagos.fec_Vcto < fechas.fecha
                                 AND CONTROL_pagos.id_pdoMes < 2 BY CONTROL_pagos.nro_cuota:
            CONTROL_pagos.id_pdoMes = 2.
        END.
    END.
    ELSE DO:
        MESSAGE fechas.nit fechas.num_credito
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.
