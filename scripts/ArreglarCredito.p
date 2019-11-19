DEFINE VAR cont AS INTEGER.

FOR EACH creditos WHERE creditos.nit = "38878784"
                    AND creditos.num_credito = 6134
                    AND creditos.sdo_capital > 0:
    UPDATE creditos WITH 1 COL.

    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.id_pdoMes < 2 BY CONTROL_pagos.nro_cuota:
        UPDATE CONTROL_pagos WITH 1 COL.
        /*id_pdoMes = 0.*/
        
        cont = cont + 1.

        IF cont = 3 THEN
            LEAVE.
        
        /*id_pdoMes = 0.
        cap_pagado = 0.*/
    END.
END.
