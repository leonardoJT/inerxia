DEFINE VAR i AS INTEGER.
DEFINE VAR saldo AS DECIMAL.
DEFINE VAR pSecuencia AS INTEGER.

DEFINE TEMP-TABLE tsaldos
    FIELD cenCostos AS INTEGER
    FIELD agencia AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD tsaldo AS DECIMAL.

FOR EACH agencias NO-LOCK:
    FOR EACH anexos WHERE anexos.ano = 2011
                      AND anexos.agencia = agencias.agencia
                      AND anexos.cuenta = "61750504" NO-LOCK:
        saldo = anexos.sdo_inicial.

        DO i = 1 TO 12:
            saldo = saldo + anexos.db[i] - anexos.cr[i].
        END.

        IF saldo <> 0 THEN DO:
            DISPLAY agencias.agencia nit cuenta saldo.

            CREATE tsaldos.
            ASSIGN tSaldos.cenCostos = anexos.cen_costos
                   tsaldos.agencia = agencias.agencia
                   tsaldos.cuenta = anexos.cuenta
                   tsaldos.nit = anexos.nit
                   tsaldos.tsaldo = saldo.
        END.

        /*DISPLAY tsaldos WITH 1 COL.*/
    END.
END.

FOR EACH tsaldos NO-LOCK BREAK BY tsaldos.agencia:
    IF FIRST-OF(tsaldos.agencia) THEN DO:
        FIND FIRST comprobantes WHERE comprobantes.agencia = tsaldos.agencia
                                  AND comprobantes.comprobante = 4 NO-ERROR.
        IF AVAILABLE comprobantes THEN DO:
            comprobantes.secuencia = comprobantes.secuencia + 1.
            pSecuencia = comprobantes.secuencia.
        END.
    END.

    CREATE mov_contable.
    ASSIGN mov_contable.cen_costos = tSaldos.cenCostos
           Mov_Contable.Agencia = tsaldos.agencia
           Mov_Contable.Destino = tSaldos.agencia
           Mov_Contable.Comprobante = 4
           Mov_Contable.Num_Documento = pSecuencia
           Mov_contable.Doc_referencia = STRING(pSecuencia)
           Mov_Contable.Fec_Contable = TODAY
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Cuenta = "61750505"
           Mov_Contable.Comentario = "Traslado de Saldos"
           Mov_Contable.Usuario = "2305"
           Mov_Contable.Estacion = "005"
           mov_contable.nit = tSaldos.nit
           Mov_contable.db = tSaldos.tsaldo.

    IF tSaldos.tsaldo < 0 THEN
        ASSIGN mov_contable.db = 0
               mov_contable.cr = tSaldos.tsaldo * -1.

    CREATE mov_contable.
    ASSIGN mov_contable.cen_costos = tSaldos.cenCostos
           Mov_Contable.Agencia = tsaldos.agencia
           Mov_Contable.Destino = tSaldos.agencia
           Mov_Contable.Comprobante = 4
           Mov_Contable.Num_Documento = pSecuencia
           Mov_contable.Doc_referencia = STRING(pSecuencia)
           Mov_Contable.Fec_Contable = TODAY
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Cuenta = Tsaldos.cuenta
           Mov_Contable.Comentario = "Traslado de Saldos"
           Mov_Contable.Usuario = "2305"
           Mov_Contable.Estacion = "005"
           mov_contable.nit = tSaldos.nit
           Mov_contable.cr = tSaldos.tsaldo.

    IF tSaldos.tsaldo < 0 THEN
        ASSIGN mov_contable.cr = 0
               mov_contable.db = tSaldos.tsaldo * -1.
END.
