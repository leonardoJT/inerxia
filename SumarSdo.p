DEFI TEMP-TABLE CopAnex LIKE Sal_Cuenta.
    
FOR EACH Sal_Cuenta WHERE Sal_Cuenta.cen_costo NE 999:
    CREATE CopAnex.
    BUFFER-COPY Sal_Cuenta TO CopAnex.
    ASSIGN CopAnex.Cen_costo = 999.
    DELETE Sal_Cuenta.
END.
    
FOR EACH CopAnex:
    FIND FIRST Sal_Cuenta WHERE Sal_Cuenta.Agencia EQ CopAnex.Agencia AND
                            Sal_Cuenta.Cuenta  EQ CopAnex.Cuenta  AND
                            Sal_Cuenta.Ano     EQ CopAnex.ano  NO-ERROR.
    IF NOT AVAIL(Sal_Cuenta) THEN DO:
       CREATE Sal_Cuenta.
       BUFFER-COPY CopAnex TO Sal_Cuenta.
    END.
    ELSE DO:
       Sal_Cuenta.Sal_Inicial = Sal_Cuenta.Sal_Inicial + CopAnex.Sal_Inicial.

       ASSIGN Sal_Cuenta.Db[1] = Sal_Cuenta.Db[1] + CopAnex.Db[1] 
              Sal_Cuenta.Db[2] = Sal_Cuenta.Db[2] + CopAnex.Db[2]
              Sal_Cuenta.Db[3] = Sal_Cuenta.Db[3] + CopAnex.Db[3]
              Sal_Cuenta.Db[4] = Sal_Cuenta.Db[4] + CopAnex.Db[4]
              Sal_Cuenta.Db[5] = Sal_Cuenta.Db[5] + CopAnex.Db[5]
              Sal_Cuenta.Db[6] = Sal_Cuenta.Db[6] + CopAnex.Db[6]
              Sal_Cuenta.Db[7] = Sal_Cuenta.Db[7] + CopAnex.Db[7]
              Sal_Cuenta.Db[8] = Sal_Cuenta.Db[8] + CopAnex.Db[8]
              Sal_Cuenta.Db[9] = Sal_Cuenta.Db[9] + CopAnex.Db[9]
              Sal_Cuenta.Db[10] = Sal_Cuenta.Db[10] + CopAnex.Db[10].

       ASSIGN Sal_Cuenta.Cr[1] = Sal_Cuenta.Cr[1] + CopAnex.Cr[1] 
              Sal_Cuenta.Cr[2] = Sal_Cuenta.Cr[2] + CopAnex.Cr[2] 
              Sal_Cuenta.Cr[3] = Sal_Cuenta.Cr[3] + CopAnex.Cr[3] 
              Sal_Cuenta.Cr[4] = Sal_Cuenta.Cr[4] + CopAnex.Cr[4] 
              Sal_Cuenta.Cr[5] = Sal_Cuenta.Cr[5] + CopAnex.Cr[5] 
              Sal_Cuenta.Cr[6] = Sal_Cuenta.Cr[6] + CopAnex.Cr[6] 
              Sal_Cuenta.Cr[7] = Sal_Cuenta.Cr[7] + CopAnex.Cr[7] 
              Sal_Cuenta.Cr[8] = Sal_Cuenta.Cr[8] + CopAnex.Cr[8] 
              Sal_Cuenta.Cr[9] = Sal_Cuenta.Cr[9] + CopAnex.Cr[9]
              Sal_Cuenta.Cr[10] =Sal_Cuenta.Cr[10] + CopAnex.Cr[10].
    END.
END.
