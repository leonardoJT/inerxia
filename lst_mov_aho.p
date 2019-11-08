    /* mov_aportes */
    DEFINE VARIABLE xdb LIKE mov_contable.db.
    DEFINE VARIABLE xcr LIKE mov_contable.cr.
    FOR EACH ahorros WHERE tip_ahorro = 4:
        FOR EACH mov_ahorros WHERE  ahorros.nit = mov_ahorros.nit AND 
                                    ahorros.cue_ahorro = mov_ahorros.cue_ahorro AND 
                                   MONTH(fecha) = 4 :
          IF SUBSTRING(string(cod_operacion),4,2) = "02" THEN  
             xdb = xdb + val_efectivo + val_cheque.
          ELSE xcr = xcr + val_efectivo + val_cheque.
        END.
       
    END.
    DISPLAY xdb xcr.
