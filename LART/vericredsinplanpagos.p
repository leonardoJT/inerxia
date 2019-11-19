DEFINE VAR wcont AS INTEGER NO-UNDO INIT 0.
DEFINE VAR wsalida AS CHAR FORMAT "X(100)" NO-UNDO.
DEFINE VAR w_nit LIKE Creditos.nit NO-UNDO.
DEFINE VAR w_credito LIKE Creditos.Num_credito NO-UNDO.
OUTPUT TO c:\INFO_fodun\credsinplan.csv.
 
FOR EACH creditos WHERE Creditos.Estado = 2  NO-LOCK BY Creditos.Fec_Desembolso :
    FIND FIRST PlanPagos WHERE PlanPagos.Cod_Credito EQ Creditos.Cod_Credito AND  
                               PlanPagos.Num_Credito EQ Creditos.Num_Credito AND
                               PlanPagos.Nit EQ  Creditos.Nit NO-LOCK NO-ERROR.
    IF NOT AVAILABLE PlanPagos THEN DO:
        ASSIGN wsalida = string(Creditos.Cod_Credito) + ";" +  
               STRING(Creditos.Num_Credito) + ";" +  Creditos.Nit + ";" + 
               STRING(Creditos.Fec_Desembolso). 
        DISPLAY wsalida WITH WIDTH  200  NO-LABEL.
        ASSIGN wcont = wcont + 1.
        ASSIGN W_nit = Creditos.Nit
               w_credito = Credito.Num_credito.
        RUN GeneraPlanfodun.p(INPUT W_nit,
                       INPUT W_credito).

    END.
    
END.
 

/* 
FOR EACH creditos WHERE Creditos.Estado = 2 NO-LOCK:
    FIND FIRST control_pagos WHERE control_pagos.Cod_Credito EQ Creditos.Cod_Credito AND  
                               control_pagos.Num_Credito EQ Creditos.Num_Credito AND
                               control_pagos.Nit EQ  Creditos.Nit NO-LOCK NO-ERROR.
    IF NOT AVAILABLE control_pagos THEN DO:
        DISPLAY Creditos.Cod_Credito Creditos.Num_Credito Creditos.Nit
            .
        PAUSE 0.
        ASSIGN wcont = wcont + 1.
    END.
    
END.
*/ 
OUTPUT CLOSE.
MESSAGE "Total Creditos Sin Plan : " +  STRING(wcont)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


/* PROCEDURE GeneraPlanfodun: */
/*                            */
/* END PROCEDURE.             */
