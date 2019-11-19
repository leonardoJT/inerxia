DEFINE SHARED VAR w_fecha AS DATE.
    
w_fecha = 09/16/2011.

OUTPUT TO d:\exportControlPagos.txt.
FOR EACH creditos WHERE creditos.cod_credito = 123
                    AND creditos.sdo_capital > 0:
    FOR EACH mov_creditos WHERE mov_creditos.nit = creditos.nit
                        AND mov_creditos.num_credito = creditos.num_credito
                        AND mov_creditos.fecha >= ADD-INTERVAL(09/16/2011,-1,"months") NO-LOCK BY mov_creditos.fecha DESCENDING:
        FIND FIRST operacion WHERE operacion.cod_operacion = mov_creditos.cod_operacion NO-LOCK NO-ERROR.
        IF AVAILABLE operacion THEN DO:
            IF operacion.tipo_operacion = 2 THEN DO:
                creditos.cuota = ROUND((creditos.sdo_capital * (((creditos.tasa / 1200) * EXP((creditos.tasa / 1200) + 1,creditos.plazo)) / (EXP((creditos.tasa / 1200) + 1,creditos.plazo) - 1))),6).
                creditos.cuota = TRUNCATE((creditos.cuota + 100) / 100,0) * 100.
                creditos.fec_desembolso = /*ADD-INTERVAL(09/16/2011,-1,"months") + 1.*/ DATE(10,5,2011).
                creditos.fec_pagAnti = DATE(10,5,2011).
                    
                RUN D:\SPS\soportes\fodun\ObjPruebas\CrearControlPagos.r(INPUT creditos.nit,
                                        INPUT creditos.num_credito,
                                        INPUT creditos.tasa).
                    
                FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                         AND CONTROL_pagos.num_credito = creditos.num_credito NO-LOCK BY CONTROL_pagos.nro_cuota:
                    EXPORT CONTROL_pagos.
                    /*DISPLAY CONTROL_pagos WITH 1 COL.*/
                END.
                    
                LEAVE.
            END.
        END.
    END.
END.
