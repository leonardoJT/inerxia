DISABLE TRIGGERS FOR LOAD OF creditos.
DISABLE TRIGGERS FOR LOAD OF mov_creditos.
DISABLE TRIGGERS FOR LOAD OF control_pagos.
DISABLE TRIGGERS FOR LOAD OF extras.
DISABLE TRIGGERS FOR LOAD OF planPagos.

OUTPUT TO c:\INFO_Fodun\Leonardo\133a27_1.txt.
FOR EACH creditos WHERE creditos.agencia = 1
                    AND creditos.cod_credito = 133
                    AND creditos.sdo_capital > 0:
    DISPLAY creditos.nit creditos.num_credito.

    FOR EACH mov_creditos WHERE mov_creditos.num_credito = creditos.num_credito
                            AND mov_creditos.nit = creditos.nit:
        mov_creditos.cod_credito = 27.
    END.

    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito:
        CONTROL_pagos.cod_credito = 27.
    END.

    FOR EACH extras WHERE extras.agencia = creditos.agencia
                      AND extras.nit = creditos.nit
                      AND extras.num_solicitud = creditos.num_solicitud:
        extras.cod_credito = 27.
    END.

    FOR EACH planPagos WHERE planPagos.nit = creditos.nit
                         AND planPagos.num_credito = creditos.num_credito:
        planPagos.cod_credito = 27.
    END.
    
    creditos.cod_credito = 27.
END.
