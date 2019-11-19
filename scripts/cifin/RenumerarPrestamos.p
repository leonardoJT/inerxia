DISABLE TRIGGERS FOR LOAD OF creditos.
DISABLE TRIGGERS FOR LOAD OF mov_creditos.
DISABLE TRIGGERS FOR LOAD OF planPagos.
DISABLE TRIGGERS FOR LOAD OF control_pagos.
DISABLE TRIGGERS FOR LOAD OF rep_creditos.

DEFINE VAR newnumber AS CHARACTER.

FIND FIRST creditos WHERE creditos.nit = "79386588"
                      AND creditos.num_credito = 3209
                      AND creditos.estado = 2 NO-ERROR.
IF AVAILABLE creditos THEN DO:
    IF SUBSTRING(STRING(creditos.num_credito),1,4) = "6200" THEN
        newNumber = "990" + SUBSTRING(string(creditos.num_credito),5).
    ELSE
        newNumber = "990" + STRING(creditos.num_credito).

    FOR EACH mov_creditos WHERE mov_creditos.nit = creditos.nit
                            AND mov_creditos.num_credito = creditos.num_credito:
        mov_creditos.num_credito = INTEGER(newNumber).
    END.

    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito:
        CONTROL_pagos.num_credito = INTEGER(newNumber).
    END.

    FOR EACH planPagos WHERE planPagos.nit = creditos.nit
                         AND planPagos.num_credito = creditos.num_credito:
        planPagos.num_credito = INTEGER(newNumber).
    END.

    FOR EACH rep_creditos WHERE rep_creditos.nit = creditos.nit
                            AND rep_creditos.num_credito = creditos.num_credito:
        rep_creditos.num_credito = INTEGER(newNumber).
    END.

    FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                           AND facturacion.num_credito = creditos.num_credito:
        facturacion.num_credito = INTEGER(newNumber).
    END.

    creditos.num_credito = INTEGER(newNumber).

    MESSAGE "Hecho!" newNumber
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
