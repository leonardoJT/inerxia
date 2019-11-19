FOR EACH mov_contable WHERE Mov_Contable.Fec_Contable GE (TODAY - 45)  
                            AND Mov_Contable.Nit EQ "31264801"  NO-LOCK:
    IF Mov_contable.Cuenta NE "24953001"   THEN DO: NEXT.
    END.
    DISPLAY Mov_Contable.Comprobante 
            Mov_Contable.Cuenta 
            Mov_Contable.Fec_Contable
            Mov_Contable.Db 
            Mov_Contable.Cr
            Mov_Contable.Fec_Grabacion
        WITH WIDTH 300.
    
END.

OUTPUT TO c:\INFO_fodun\rpt_31264801.csv. 
FOR EACH Mov_Ahorros WHERE Mov_Ahorros.Agencia EQ 4 AND 
                           Mov_Ahorros.Nit EQ "31264801"  AND 
                           Mov_Ahorros.Fecha GE (TODAY - 45) NO-LOCK:
    IF Mov_Ahorros.Cod_Operacion EQ 10101003 THEN DO: NEXT.
    END.
    FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_ahorro EQ 
                                Mov_Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
        DISPLAY Mov_Ahorros.Cod_Ahorro 
                Mov_Ahorros.Cue_Ahorros
                Pro_Ahorros.Nom_Producto 
                Mov_Ahorros.Fecha 
                Mov_Ahorros.Val_Efectivo 
                Mov_Ahorros.Val_Cheque
                Mov_Ahorros.Cod_Operacion
                Mov_Ahorros.Sdo_Disponible
               WITH WIDTH 300.
    
END.
OUTPUT CLOSE.
