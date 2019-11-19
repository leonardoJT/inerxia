FOR EACH creditos WHERE fec_desembolso >= 05/01/2013
    AND plazo <> 1
    AND cod_credito <> 123
    AND cuo_pagadas = 0
    AND sdo_capital <> monto NO-LOCK BY fec_desembolso BY nit:
    DISPLAY nit num_credito plazo cuo_pagadas.
END.
