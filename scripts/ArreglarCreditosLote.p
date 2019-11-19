DEFINE TEMP-TABLE crds
    FIELD nit AS CHARACTER
    FIELD num_credito AS INTEGER FORMAT ">>>>>>>>>>>>".

INPUT FROM c:\INFO_fodun\leonardo\cambioFechas.csv.
REPEAT:
    CREATE crds.
    IMPORT DELIMITER ";" crds.
END.
INPUT CLOSE.

OUTPUT TO c:\INFO_fodun\leonardo\cambioFechas1.csv.
FOR EACH crds:
    FIND FIRST creditos WHERE creditos.nit = crds.nit
                          AND creditos.num_credito = crds.num_credito NO-LOCK NO-ERROR.
    IF AVAILABLE creditos THEN DO:
        IF creditos.sdo_Capital > creditos.sdo_Capital THEN
            EXPORT DELIMITER ";" creditos.nit creditos.num_credito creditos.sdo_Capital creditos.sdo_proyectado creditos.cuota.
        ELSE DO:
            FOR EACH creditos WHERE creditos.nit = crds.nit
                                AND creditos.num_credito = crds.num_credito
                                AND creditos.sdo_capital > 0:
                creditos.fec_pago = 04/10/2012.
                creditos.dias_atraso = 0.
                creditos.val_atraso = 0.
                creditos.cuo_atraso = 0.

                FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                         AND CONTROL_pagos.num_credito = creditos.num_credito
                                         AND CONTROL_pagos.fec_Vcto < 04/10/2012
                                         AND CONTROL_pagos.id_pdoMes < 2 BY CONTROL_pagos.nro_cuota:
                    id_PdoMes = 2.
                END.
            END.
        END.
    END.
    ELSE DO:
        MESSAGE crds.nit crds.num_credito
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.
OUTPUT CLOSE.
