DEFI TEMP-TABLE CopAnex LIKE anexos.
    
FOR EACH anexos WHERE anexos.cen_costo NE 999:
    CREATE CopAnex.
    BUFFER-COPY anexos TO CopAnex.
    ASSIGN CopAnex.Cen_costo = 999.
    DELETE anexos.
END.
    
FOR EACH CopAnex:
    FIND FIRST Anexos WHERE Anexos.Agencia EQ CopAnex.Agencia AND
                            Anexos.Cuenta  EQ CopAnex.Cuenta  AND
                            Anexos.Nit     EQ CopAnex.nit     AND
                            Anexos.Ano     EQ CopAnex.ano  NO-ERROR.
    IF NOT AVAIL(Anexos) THEN DO:
       CREATE Anexos.
       BUFFER-COPY CopAnex TO Anexos.
    END.
    ELSE DO:
       ASSIGN Anexos.Db[1] = CopAnex.Db[1] 
              Anexos.Db[2] = CopAnex.Db[2]
              Anexos.Db[3] = CopAnex.Db[3]
              Anexos.Db[4] = CopAnex.Db[4]
              Anexos.Db[5] = CopAnex.Db[5]
              Anexos.Db[6] = CopAnex.Db[6]
              Anexos.Db[7] = CopAnex.Db[7]
              Anexos.Db[8] = CopAnex.Db[8]
              Anexos.Db[9] = CopAnex.Db[9].
       ASSIGN Anexos.Cr[1] = CopAnex.Cr[1] 
              Anexos.Cr[2] = CopAnex.Cr[2] 
              Anexos.Cr[3] = CopAnex.Cr[3] 
              Anexos.Cr[4] = CopAnex.Cr[4] 
              Anexos.Cr[5] = CopAnex.Cr[5] 
              Anexos.Cr[6] = CopAnex.Cr[6] 
              Anexos.Cr[7] = CopAnex.Cr[7] 
              Anexos.Cr[8] = CopAnex.Cr[8] 
              Anexos.Cr[9] = CopAnex.Cr[9].
    END.
END.
