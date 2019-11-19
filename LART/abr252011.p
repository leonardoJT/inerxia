FOR EACH Ahorros WHERE Ahorros.Nit EQ "52174568" NO-LOCK :
    DISPLAY Ahorros.Agencia 
            Ahorros.Cod_ahorro 
            Ahorros.Cue_Ahorros 
            Ahorros.Exento_3xm 
            Ahorros.FecIniExentoGMF.
END.


FOR EACH clientes WHERE Clientes.Nit EQ "52174568" NO-LOCK :
    DISPLAY Clientes.Apellido1 Clientes.Nombre.
END.
