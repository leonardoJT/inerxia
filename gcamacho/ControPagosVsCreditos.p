
OUTPUT TO "c:\creditos2.csv".    
PUT "nit;cod_credito;num_credito;fec_ultPago;fec_pago" SKIP.
        
FOR EACH control_pagos /* WHERE nit EQ "19100277" */ NO-LOCK BREAK BY nit BY num_credito:
    IF FIRST-OF (num_credito) THEN DO:
        FIND FIRST creditos WHERE creditos.nit EQ control_pagos.nit AND
                                creditos.num_credito EQ control_pagos.num_credito NO-LOCK NO-ERROR.
    
        IF creditos.fec_ultPago EQ CONTROL_pagos.fec_inic AND
           creditos.fec_Pago NE CONTROL_pagos.fec_Vcto   THEN DO:
            EXPORT DELIMITER ";" creditos.nit 
                                creditos.cod_credito 
                                creditos.num_credito 
                                creditos.fec_ultPago 
                                creditos.fec_pago.
    
    /*         DISPLAY creditos.nit                      */
    /*             creditos.num_credito                  */
    /*             Creditos.Fec_Pago                     */
    /*             Creditos.Fec_UltPago                  */
    /*             Creditos.Cuo_Pagadas                  */
    /*             "*************"                       */
    /*     /*         control_pagos           */         */
    /*     /*         control_pagos.Id_PdoMes */         */
    /*     /*         control_pagos.Cuota     */         */
    /*     /*         control_pagos.Fec_Inic  */         */
    /*     /*         control_pagos.Fec_Vcto  */         */
    /*     /*         control_pagos.Nro_Cuota */         */
    /*     /*         "-------------------"   */         */
    /*             WITH FRAME a WIDTH 100 .              */
    /*         DISPLAY control_pagos WITH FRAME a 1 COL. */
        END.
    END.
END.

OUTPUT CLOSE.

