DEFINE VAR i AS INTEGER.
DEFINE VAR saldo AS DECIMAL.
DEFINE VAR pSecuencia AS INTEGER.
DEFINE VAR pNaturaleza AS CHARACTER.

DEFINE TEMP-TABLE tsaldos
    FIELD cenCostos AS INTEGER
    FIELD agencia AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD tsaldo AS DECIMAL FORMAT "->>>,>>>,>>>,>>>".

FOR EACH agencias WHERE agencias.agencia = 4 NO-LOCK:
    FOR EACH anexos WHERE anexos.ano = 2011
                      AND anexos.agencia = agencias.agencia
                      AND anexos.cen_costos = 0 NO-LOCK:
        FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
        IF NOT AVAILABLE cuentas THEN DO:
            pNaturaleza = "DB".

            /*saldo = anexos.sdo_inicial.

            DO i = 1 TO 12:
                saldo = saldo + anexos.db[i] - anexos.cr[i].
            END.

            IF saldo <> 0 THEN DO:
                MESSAGE "La cuenta" anexos.cuenta "no existe en el PUC" SKIP
                    "Se cancela la operación"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

                DISPLAY anexos WITH 1 COL.

                RETURN ERROR.
            END.
            ELSE
                NEXT.*/
        END.
        ELSE
            pNaturaleza = cuentas.naturaleza.

        saldo = anexos.sdo_inicial.

        DO i = 1 TO 12:
            IF pNaturaleza = "DB" THEN
                saldo = saldo + anexos.db[i] - anexos.cr[i].
            ELSE
                saldo = saldo - anexos.db[i] + anexos.cr[i].
        END.

        IF saldo <> 0 THEN DO:
            /*MESSAGE agencias.agencia anexos.nit anexos.cuenta saldo VIEW-AS ALERT-BOX.*/

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

FOR EACH tsaldos:
    FIND FIRST cuentas WHERE cuentas.cuenta = tSaldos.cuenta NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentas THEN DO:
        MESSAGE "Cuenta no existe" tsaldos.cuenta
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN ERROR.
    END.
    DISPLAY tsaldos WITH 1 COL.
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

    FIND FIRST cuentas WHERE cuentas.cuenta = tsaldos.cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN DO:
        /* 1. Cancelo los saldos en el centro de costos correspondiente */
        CREATE mov_contable.
        ASSIGN mov_contable.cen_costos = tSaldos.cenCostos
               Mov_Contable.Agencia = tsaldos.agencia
               Mov_Contable.Destino = tSaldos.agencia
               Mov_Contable.Comprobante = 4
               Mov_Contable.Num_Documento = pSecuencia
               Mov_contable.Doc_referencia = STRING(pSecuencia)
               Mov_Contable.Fec_Contable = TODAY
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Cuenta = tSaldos.cuenta
               Mov_Contable.Comentario = "Traslado de Saldos"
               Mov_Contable.Usuario = "2305"
               Mov_Contable.Estacion = "005"
               mov_contable.nit = tSaldos.nit.
               
        IF cuentas.naturaleza = "DB" THEN DO:
            IF tSaldos.tSaldo > 0 THEN
                mov_contable.cr = tSaldos.tSaldo.
            ELSE
                mov_contable.db = tSaldos.tSaldo * -1.
        END.
        ELSE DO:
            IF tSaldos.tSaldo > 0 THEN
                mov_contable.db = tSaldos.tSaldo.
            ELSE
                mov_contable.cr = tSaldos.tSaldo * -1.
        END.

        /* 2. Traslado estos saldos para el centro de costos 999 */
        CREATE mov_contable.
        ASSIGN mov_contable.cen_costos = 999
               Mov_Contable.Agencia = tsaldos.agencia
               Mov_Contable.Destino = tSaldos.agencia
               Mov_Contable.Comprobante = 4
               Mov_Contable.Num_Documento = pSecuencia
               Mov_contable.Doc_referencia = STRING(pSecuencia)
               Mov_Contable.Fec_Contable = TODAY
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Cuenta = tSaldos.cuenta
               Mov_Contable.Comentario = "Traslado de Saldos"
               Mov_Contable.Usuario = "2305"
               Mov_Contable.Estacion = "005"
               mov_contable.nit = tSaldos.nit.
               
        IF cuentas.naturaleza = "DB" THEN DO:
            IF tSaldos.tSaldo > 0 THEN
                mov_contable.db = tSaldos.tSaldo.
            ELSE
                mov_contable.cr = tSaldos.tSaldo * -1.
        END.
        ELSE DO:
            IF tSaldos.tSaldo > 0 THEN
                mov_contable.cr = tSaldos.tSaldo.
            ELSE
                mov_contable.db = tSaldos.tSaldo * -1.
        END.
    END.
    ELSE DO:
        MESSAGE "Cuenta no existe" tsaldos.cuenta
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN ERROR.
    END.
END.

