FOR EACH creditos WHERE creditos.cod_credito = 123
                    AND creditos.cuota = 100
                    AND creditos.sdo_capital > 0:
    creditos.cuota = ROUND((creditos.sdo_capital * (((creditos.tasa / 1200) * EXP((creditos.tasa / 1200) + 1,creditos.plazo)) / (EXP((creditos.tasa / 1200) + 1,creditos.plazo) - 1))),6).
    creditos.cuota = TRUNCATE((creditos.cuota + 100) / 100,0) * 100.

    IF DAY(TODAY) > 16 THEN
        creditos.fec_pagAnti = ADD-INTERVAL(TODAY,2,"months").
    ELSE
        creditos.fec_pagAnti = ADD-INTERVAL(TODAY,1,"months").

    creditos.fec_pagAnti = DATE(MONTH(creditos.fec_pagAnti), 5, YEAR(creditos.fec_pagAnti)).

    RUN D:\SPS\soportes\fodun\Obj\CrearControlPagos.r(INPUT creditos.nit,
                                                      INPUT creditos.num_credito,
                                                      INPUT creditos.tasa).
END.
