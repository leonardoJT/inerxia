
DEFINE VARIABLE vcAgencia LIKE agencias.ciudad.

OUTPUT TO "c:\CreditosSinCliente.txt".
FOR EACH creditos WHERE creditos.estado = 2 OR creditos.estado = 5 NO-LOCK:    
    FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
    IF NOT AVAILABLE clientes THEN DO:
        ASSIGN vcAgencia = "".
        FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
        IF AVAILABLE agencias THEN 
            ASSIGN vcAgencia = Agencias.Ciudad.
        ELSE
            ASSIGN vcAgencia = "SIN AGENCIA".

        FORM 
            Creditos.Nit            COLUMN-LABEL "NIT"
            Creditos.Num_Credito    COLUMN-LABEL "Num. Credito"
            Creditos.Cod_Credito    COLUMN-LABEL "Cod. Credito"
            vcAgencia               COLUMN-LABEL "Agencia"
            Creditos.Num_Solicitud  COLUMN-LABEL "Num. Solicitud"
      WITH FRAME frep DOWN COLUMN 1 WIDTH 120
      NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.

        DISPLAY 
            Creditos.Nit 
            Creditos.Num_Credito 
            Creditos.Cod_Credito 
            vcAgencia
            Creditos.Num_Solicitud
        SKIP(1)
        WITH FRAME frep.

        DOWN  WITH FRAME frep.
    END.
END.

OUTPUT CLOSE.


