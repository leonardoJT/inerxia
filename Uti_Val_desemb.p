DISABLE TRIGGERS FOR LOAD OF Creditos.
FOR EACH Creditos WHERE Creditos.Estado EQ 2:
    ASSIGN Creditos.Val_Desemb = Creditos.Monto.

    FOR EACH PlanPagos WHERE PlanPagos.Agencia        EQ Creditos.Agencia    
                           AND PlanPagos.Nit          EQ Creditos.Nit        
                           AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                           AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                           AND (PlanPagos.Id_PdoMes   EQ 4 OR PlanPagos.Id_PdoMes EQ 2) 
                           AND PlanPagos.Nro_Cuota    GE 0 NO-LOCK BY PlanPagos.Monto:
        IF PlanPagos.Monto GT Creditos.Val_Desemb THEN 
           ASSIGN Creditos.Val_Desemb = PlanPagos.Monto.
    END.
END.
