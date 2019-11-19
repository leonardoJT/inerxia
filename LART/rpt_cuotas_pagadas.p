FOR EACH creditos WHERE estado = 2  NO-LOCK:
    FIND LAST control_pagos WHERE control_pagos.Cod_Credito EQ Creditos.Cod_credito AND 
                                  control_pagos.Num_Credito EQ Creditos.Num_credito AND
                                  control_pagos.Nit EQ Creditos.nit NO-LOCK NO-ERROR. 
    IF NOT AVAILABLE control_pagos THEN DO:
/*         MESSAGE "No Hay control Pagos"         */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK. */
            NEXT.
    END.
    DISPLAY 
            Creditos.Num_Credito
            Creditos.Fec_Desembolso
            Creditos.Plazo
            control_pagos.Nro_Cuota
            Creditos.Cod_Credito
            control_pagos.Cuota
            control_pagos.Fec_Inic
            Creditos.Nit
            Creditos.Cuo_Pagadas
        WITH WIDTH 500.
        .


END.

 
 
