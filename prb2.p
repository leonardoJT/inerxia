DEF VAR i AS INT.
FOR EACH plastico WHERE nit = "55160966".
    DELETE plastico.
END.
FOR EACH ahorros NO-LOCK WHERE nit = "55160966".
    CREATE plastico.
    i = i + 1.
    ASSIGN plastico.Agencia = ahorros.agencia plastico.Cod_ahorro = ahorros.cod_ahorro
           plastico.Cue_Ahorros = ahorros.cue_ahorro plastico.Num_Plastico = INT(ahorros.nit) + i.
END.
