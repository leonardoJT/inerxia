DEFI VAR W_FecCorte      AS DATE.
DEFI VAR WFec_CancAtraso AS DATE.

W_FecCorte = DATE(04,30,2005).

FOR EACH Creditos WHERE Creditos.Val_Atraso  LE 0
                    AND Creditos.Fec_UltPago NE ? NO-LOCK:
    FIND LAST PlanPagos WHERE PlanPagos.Agencia    EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 3
                        AND PlanPagos.Nro_Cuota    GT 0
                        AND PlanPagos.Fec_Inic     NE W_FecCorte NO-LOCK NO-ERROR.
    IF   AVAIL(PlanPagos) 
    AND (PlanPagos.Capital_Acum GT PlanPagos.Pagos_CapitalAcum) THEN
         WFec_CancAtraso = Creditos.Fec_UltPago. 
END.
