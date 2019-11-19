FOR EACH creditos WHERE creditos.nit = "47796" NO-LOCK :
    FIND LAST control_pagos WHERE control_pagos.Nit EQ creditos.nit AND
              control_pagos.Num_Credito EQ Creditos.Num_Credito NO-LOCK NO-ERROR.
     DISPLAY    Creditos.Cod_Credito 
                Creditos.Num_Credito 
                Creditos.Cuo_Pagadas 
                control_pagos.Nro_Cuota.
END.


