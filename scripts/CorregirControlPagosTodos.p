DEFINE INPUT PARAMETER nitcredito AS CHARACTER.
DEFINE INPUT PARAMETER numCredito AS INTEGER.

DEF SHARED VAR W_ManFin AS HANDLE.
DEFINE VAR deuda AS DECIMAL.
DEFINE VAR fechaPago AS DATE.
DEFINE VAR numCuota AS INTEGER.
DEFINE VAR plz AS INTEGER.
DEFINE VAR fecDesembolso AS DATE.
DEFINE VAR deudaActual AS DECIMAL.
DEFINE VAR flag AS LOGICAL.
DEFINE VAR meses AS INTEGER.
DEFINE VAR W_NroDias AS INTEGER.
DEFINE VAR W_NMeses AS INTEGER.
DEFINE VAR W_NroPer AS INTEGER.
DEFINE VAR P_NomPer AS CHARACTER.
    
/*OUTPUT TO c:\RevisionCreditos.txt.*/

FOR EACH creditos WHERE creditos.sdo_capital > 0
                    AND creditos.fec_desembolso <= 02/28/2011
                    AND creditos.nit = nitCredito
                    AND creditos.num_Credito = numCredito NO-LOCK:

    fechaPago = creditos.fec_pago.

    RUN RUTFINAN.R PERSISTENT SET W_MANFIN.

    RUN HallarPeriodo IN W_ManFin (INPUT creditos.per_pago,
                                   INPUT creditos.plazo,
                                   OUTPUT W_NroDias,
                                   OUTPUT W_NMeses,
                                   OUTPUT W_NroPer,
                                   OUTPUT P_NomPer).

    /* Recorro control pagos */
    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.cod_credito = creditos.cod_credito
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.id_pdoMes < 2 BY CONTROL_pagos.nro_cuota:
        CONTROL_pagos.fec_Vcto = fechaPago.
        fechaPago = ADD-INTERVAL(fechaPago,integer(12 / W_NroPer),"months").
    END.
END.
