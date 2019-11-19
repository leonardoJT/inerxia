FOR EACH creditos WHERE estado = 2 AND 
                        sdo_capital GT 0
                        NO-LOCK:
    FIND FIRST CONTROL_pagos WHERE control_pagos.Nit = creditos.nit
                             AND control_pagos.Num_Credito = creditos.num_credito NO-ERROR.
    IF NOT AVAILABLE(CONTROL_pagos) THEN DO:
        DISP creditos.nit creditos.num_credito sdo_capital creditos.fec_desembolso. 
        /*RUN generaplanfodun.r(INPUT creditos.nit,INPUT creditos.num_credito).*/
     END.
        

END.



