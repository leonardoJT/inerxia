/* Para ajustar una fecha para inicio del credito sea apartir de la fecha actulizadasa     */ 
FOR EACH creditos WHERE fec_desembolso EQ (TODAY - 3) AND nit = "11257395" :
    DISPLAY fec_paganti fec_pago. 
    ASSIGN fec_paganti = DATE(05/31/2011). 
    /* UPDATE fec_desembolso fec_paganti.  */ 
    
END.
