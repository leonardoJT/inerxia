FOR EACH creditos WHERE Creditos.Sdo_capital LE 0 NO-LOCK:
    IF YEAR(Creditos.Fec_CanceTotal) NE 2011 AND 
        MONTH(Creditos.Fec_CanceTotal) NE 4 
        THEN DO: NEXT.
    END.
    IF MONTH(Creditos.Fec_CanceTotal) NE 4 
        THEN DO: NEXT.
    END.

    DISPLAY Creditos.Nit 
            Creditos.Num_Credito 
            Creditos.Fec_CanceTotal
            Creditos.Sdo_Capital 
            Creditos.Estado VIEW-AS TEXT
            Creditos.Abogado
        WITH WIDTH 250. 
    
END.
