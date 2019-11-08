FOR EACH creditos WHERE sdo_capital GT 0 AND fec_Desemb LT DATE (10,1,2006) NO-LOCK:
    IF sdo_capital GE creditos.monto THEN DO:
       IF cuo_pagadas NE 0 THEN
    END.

END.
