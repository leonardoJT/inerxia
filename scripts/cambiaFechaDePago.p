FOR EACH creditos WHERE creditos.nit = "72153067"
    AND num_credito = 17460:

    UPDATE creditos WITH 1 COL.
    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.id_pdoMes = 0 BY CONTROL_pagos.fec_vcto:
        UPDATE CONTROL_pagos WITH 1 COL.
        /*CONTROL_pagos.fec_vcto = CONTROL_pagos.fec_vcto + 10.*/
    END.
END.
