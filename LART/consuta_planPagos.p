
FOR EACH creditos WHERE fec_desembolso = (TODAY - 1)  NO-LOCK:
    
    DISPLAY
        Creditos.Nit
        Creditos.Cod_Credito Creditos.Num_Credito Creditos.Nit
        Creditos.Estado VIEW-AS TEXT
        Creditos.Monto 
        Creditos.Sdo_Capital
        Creditos.Cuota 
        Creditos.Fec_Aprobacion  LABEL "Aprobacion"
/*         Creditos.Fec_Bloqueo      */
/*         Creditos.Fec_CanceTotal   */
/*         Creditos.Fec_Calificacion */
        Creditos.Fec_Desembolso LABEL "Desembolso"
        Creditos.Fec_PagAnti    LABEL "PagoAnti"
        Creditos.Fec_Pago       LABEL "Fec_pago"
        Creditos.Fec_ProxLiquidacion  LABEL "Proxima Liquidacion"
        Creditos.Fec_UltLiquidacion   LABEL "Ultima Liquidacion"
/*         Creditos.Fec_UltPago */
        WITH 1 COL .

    /*  */
        FIND FIRST PlanPagos WHERE PlanPagos.Nit EQ Creditos.Nit
                             AND PlanPagos.Cod_Credito EQ Creditos.Cod_Credito
                             AND PlanPagos.Num_Credito EQ Creditos.Num_Credito
                              NO-LOCK NO-ERROR.
        IF NOT AVAILABLE PlanPagos  THEN DO:
            MESSAGE "No existe Plan Pagos,  Linea Credito : " + TRIM(string(Creditos.Cod_Credito)) + " Nro : " +
                    trim(string(Creditos.Num_Credito))
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            NEXT.
        END.


        FOR EACH PlanPagos WHERE PlanPagos.Nit EQ Creditos.Nit
                             AND PlanPagos.Cod_Credito EQ Creditos.Cod_Credito
                             AND PlanPagos.Num_Credito EQ Creditos.Num_Credito
                              NO-LOCK BY PlanPagos.Nro_Cuota:
            DISPLAY
/*                creditos.Nit */
               creditos.Cod_Credito COLUMN-LABEL "Linea"
               creditos.Num_Credito COLUMN-LABEL "Credito"
               PlanPagos.Nro_Cuota  COLUMN-LABEL "Cuota Nro."
/*                PlanPagos.Cuota */
               PlanPagos.Fec_Inic   COLUMN-LABEL "Inicio"
               PlanPagos.Fec_Vcto    COLUMN-LABEL "Vence"
               WITH WIDTH 500 .
        END.
    /*  */
    /*
    DISPLAY
           creditos.Nit 
           creditos.Cod_Credito
           creditos.Num_Credito 
           creditos.Fec_desembolso 
           creditos.Fec_pago
           Creditos.Fec_PagAnti 
        WITH WIDTH 500.
    */
END.
