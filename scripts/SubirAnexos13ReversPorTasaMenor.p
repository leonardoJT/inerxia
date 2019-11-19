disable TRIGGERS FOR LOAD OF anexos13.
    
FOR EACH mov_ahorros WHERE fecha = 01/11/2012
                       AND cpte = 20
                       AND descrip = "RevPorTasaMenor" NO-LOCK:
    FIND FIRST anexos13 WHERE anexos13.nit = mov_ahorros.nit
                          AND anexos13.agencia = mov_ahorros.agencia
                          AND anexos13.cuenta = "61752001"
                          AND anexos13.ano = 2012
                          AND anexos13.cen_costos = 999 NO-ERROR.
    IF NOT AVAILABLE(anexos13) THEN DO:
        CREATE anexos13.
        ASSIGN anexos13.Agencia = Mov_ahorros.Agencia
               anexos13.Nit = Mov_ahorros.Nit
               anexos13.Cuenta = "61752001"
               anexos13.Ano = 2012
               anexos13.Cen_Costos = 999.
    END.

    anexos13.Cr[1] = anexos13.Cr[1] + mov_ahorros.val_efectivo.
END.

MESSAGE "Terminó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
