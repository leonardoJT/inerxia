FOR EACH creditos WHERE sdo_capital GT 0 NO-LOCK:
    FIND FIRST control_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                  AND CONTROL_pagos.num_credito EQ creditos.num_credito NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(CONTROL_pagos) THEN DO:
       DISP creditos.cod_credito creditos.nit.
      RUN generaplanfodun.r(INPUT creditos.nit,INPUT creditos.num_credito).
    END.
END.
