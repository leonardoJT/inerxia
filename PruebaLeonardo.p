DEFINE VAR flagErrorMov AS LOGICAL.
DEFINE VAR totalGMF AS DECIMAL.

DEFINE TEMP-TABLE TT_consecutivo
    FIELD numero AS INTEGER.

DEFINE TEMP-TABLE TT_Movimientos NO-UNDO
    FIELD consecutivo AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD tarjeta AS CHARACTER
    FIELD adquiriente AS INTEGER
    FIELD secuencia AS INTEGER
    FIELD codTerminal AS CHARACTER
    FIELD nombreTerminal AS CHARACTER
    FIELD fechaTransaccion AS DATE
    FIELD horaTransaccion AS INTEGER
    FIELD monto AS DECIMAL
    FIELD comision AS DECIMAL
    FIELD tipoTransaccion AS CHARACTER
    FIELD codError AS INTEGER
    FIELD tipoterminal AS INTEGER
    FIELD red AS INTEGER
    FIELD numTerminal AS CHARACTER
    FIELD numTransaccion AS CHARACTER
    FIELD aplicar AS LOGICAL
    FIELD descripcionError AS CHARACTER
    FIELD agencia AS INTEGER
    FIELD nit AS CHARACTER
    FIELD cod_ahorros AS INTEGER
    FIELD cue_ahorros AS CHARACTER.

DEFINE VAR registro AS CHARACTER.

INPUT FROM VALUE("c:\Prueba.mov").
    REPEAT:
        IMPORT UNFORMATTED registro.

        IF SUBSTRING(registro,1,6) <> "FOOTER" THEN DO:
            CREATE TT_Movimientos.
            ASSIGN TT_Movimientos.consecutivo = INTEGER(SUBSTRING(registro,1,6))
                   TT_Movimientos.cuenta = SUBSTRING(registro,7,10)
                   TT_Movimientos.tarjeta = REPLACE(SUBSTRING(registro,17,19)," ","")
                   TT_Movimientos.adquiriente = INTEGER(SUBSTRING(registro,36,5))
                   TT_Movimientos.secuencia = INTEGER(SUBSTRING(registro,41,4))
                   TT_Movimientos.codTerminal = SUBSTRING(registro,45,8)
                   TT_Movimientos.nombreTerminal = SUBSTRING(registro,53,30)
                   TT_Movimientos.fechaTransaccion = DATE(INTEGER(SUBSTRING(registro,87,2)),
                                                          INTEGER(SUBSTRING(registro,89,2)),
                                                          INTEGER(SUBSTRING(registro,83,4)))
                   TT_Movimientos.horaTransaccion = INTEGER(SUBSTRING(registro,91,2)) * 3600 +
                                                    INTEGER(SUBSTRING(registro,93,2)) * 60 +
                                                    INTEGER(SUBSTRING(registro,95,2))
                   TT_Movimientos.Monto = DECIMAL(SUBSTRING(registro,97,15)) / 100
                   TT_Movimientos.Comision = DECIMAL(SUBSTRING(registro,112,15)) / 100
                   TT_Movimientos.tipoTransaccion = SUBSTRING(registro,127,1)
                   TT_Movimientos.codError = INTEGER(SUBSTRING(registro,128,2))
                   TT_Movimientos.tipoTerminal = INTEGER(SUBSTRING(registro,130,1))
                   TT_Movimientos.red = INTEGER(SUBSTRING(registro,131,1))
                   TT_Movimientos.numTerminal = SUBSTRING(registro,132,8)
                   TT_Movimientos.numTransaccion = SUBSTRING(registro,140,12).

            FIND FIRST tarjetas WHERE tarjetas.tarjetaDB = TT_Movimientos.tarjeta NO-LOCK NO-ERROR.
            IF AVAILABLE tarjetas THEN DO:
                FIND FIRST ahorros WHERE ahorros.nit = tarjetas.nit
                                     AND ahorros.tarjetaDB = TT_Movimientos.tarjeta
                                     /*AND ahorros.cod_ahorro = INTEGER(SUBSTRING(TT_Movimientos.cuenta,1,2))*/
                                     /*AND INTEGER(ahorros.cue_ahorros) = INTEGER(SUBSTRING(TT_Movimientos.cuenta,3))*/ NO-LOCK NO-ERROR.
                IF AVAILABLE ahorros THEN DO:
                    TT_Movimientos.aplicar = YES.
                    TT_Movimientos.agencia = ahorros.agencia.
                    TT_Movimientos.nit = ahorros.nit.
                    TT_Movimientos.cod_ahorro = ahorros.cod_ahorro.
                    TT_Movimientos.cue_ahorro = ahorros.cue_ahorro.
                END.
                ELSE DO:
                    flagErrorMov = YES.
                    TT_Movimientos.aplicar = NO.
                    TT_Movimientos.descripcionError = "No existe ningún producto de ahorros con este número de tarjeta matriculado".
                END.
            END.
            ELSE DO:
                flagErrorMov = YES.
                TT_Movimientos.aplicar = NO.
                TT_Movimientos.descripcionError = "Esta tarjeta no se encuentra matriculada".
            END.
        END.
    END.
INPUT CLOSE.

/* Aplicar Movimientos - oakley */
DEFINE VAR totalDebitos AS DECIMAL.
DEFINE VAR totalDebitosComision AS DECIMAL.
DEFINE VAR totalCreditos AS DECIMAL.
DEFINE VAR totalCreditosComision AS DECIMAL.

FOR EACH TT_Movimientos WHERE TT_Movimientos.aplicar = YES:

    IF TT_Movimientos.monto = 100000 THEN DO:
        FIND FIRST TT_consecutivo NO-ERROR.
        IF AVAILABLE TT_consecutivo THEN
            DELETE TT_consecutivo.
    END.

    IF TT_movimientos.tipoTransaccion <> "5" THEN DO:
        totalDebitos = totalDebitos + TT_Movimientos.monto.
        totalDebitosComision = totalDebitosComision + TT_Movimientos.comision.
    END.
    ELSE DO:
        totalCreditos = totalCreditos + TT_Movimientos.monto.
        totalCreditosComision = totalCreditosComision + TT_Movimientos.comision.
    END.
END.

MESSAGE totalDebitos totaldebitosComision totalCreditos TotalCreditosComision
    VIEW-AS ALERT-BOX INFO BUTTONS OK.



DEFINE VAR baseGMF AS DECIMAL.
DEFINE VAR gmfAplicado AS DECIMAL.

DEFINE VAR numDocumento17 AS DECIMAL INITIAL 0.
DEFINE VAR totalMovAgenciaDB17 AS DECIMAL INITIAL 0.
DEFINE VAR totalMovAgenciaCR17 AS DECIMAL INITIAL 0.
DEFINE VAR numDocumento24 AS DECIMAL INITIAL 0.
DEFINE VAR totalMovAgenciaDB24 AS DECIMAL INITIAL 0.
DEFINE VAR totalMovAgenciaCR24 AS DECIMAL INITIAL 0.
DEFINE VAR numDocumento30 AS DECIMAL INITIAL 0.
DEFINE VAR totalMovAgenciaDB30 AS DECIMAL INITIAL 0.
DEFINE VAR totalMovAgenciaCR30 AS DECIMAL INITIAL 0.

/* Capturamos los datos para realizar la Contabilización */

/* Se inicia la aplicación */
totalMovAgenciaCR17 = 0.
    
    /* Cargamos todos los movimientos para la correspondiente agencia */
FOR EACH TT_Movimientos WHERE TT_Movimientos.aplicar = YES BREAK BY TT_Movimientos.cod_ahorro
                                                                 BY TT_Movimientos.agencia:
    baseGMF = 0.

    /* Buscamos la cuenta de ahorros correspondiente a este movimiento para realizar el DB o el CR */
    FIND FIRST ahorros WHERE ahorros.nit = TT_Movimientos.nit
                             /*AND ahorros.tarjetaDB = TT_Movimientos.tarjeta*/
                             AND ahorros.cod_ahorro = TT_Movimientos.cod_ahorro
                             AND ahorros.cue_ahorros = TT_Movimientos.cue_ahorros NO-ERROR.
    IF AVAILABLE ahorros THEN DO:
        IF TT_movimientos.tipoTransaccion <> "5" /* Es distinto de Reversión */ THEN DO:
            /* 1. Debitamos la Comisión */
            IF TT_Movimientos.comision > 0 THEN DO:
                baseGMF = baseGMF + TT_Movimientos.comision.

                totalMovAgenciaDB17 = totalMovAgenciaDB17 + TT_Movimientos.comision.
                totalMovAgenciaDB24 = totalMovAgenciaDB24 + TT_Movimientos.comision.
            END.

            /* 2. Buscamos en los movimientos para asegurarnos que la transacción se haya realizado */
            IF TT_Movimientos.Monto > 0 THEN DO:
                
                FIND FIRST mov_ahorros WHERE mov_ahorros.nit = ahorros.nit
                                         AND mov_ahorros.cod_ahorro = ahorros.cod_ahorro
                                         AND mov_ahorros.cue_ahorros = ahorros.cue_ahorros
                                         AND mov_ahorros.val_efectivo = TT_Movimientos.Monto
                                         AND mov_ahorros.cpte = 24
                                         AND mov_ahorros.num_documento = TT_Movimientos.numTransaccion
                                         AND mov_ahorros.fecha = 04/24/2010 NO-ERROR.
                IF NOT AVAILABLE mov_ahorros THEN DO:
                    baseGMF = baseGMF + TT_Movimientos.Monto.

                    totalMovAgenciaDB17 = totalMovAgenciaDB17 + TT_Movimientos.Monto.
                    totalMovAgenciaDB24 = totalMovAgenciaDB24 + TT_Movimientos.Monto.
                END.
            END.

            IF baseGMF > 0 THEN DO:
                /* 3. Aplicamos el GMF */
                RUN \\192.168.101.9\desarrollo\objetos\RutGMF.r(INPUT FALSE, /* No lo aplica, solo lo calcula.  La aplicación la hacemos aquí ya que RutGMF.r no permite sobregirar las cuentas */
                             INPUT 1,
                             INPUT ahorros.agencia,
                             INPUT 1,
                             INPUT ahorros.cod_ahorro,
                             INPUT ahorros.nit,
                             INPUT ahorros.cue_ahorros,
                             INPUT 010102001,
                             INPUT baseGMF,
                             INPUT 30,
                             INPUT /*numDocumento30*/ 1,
                             INPUT "GMF x Transaccion en Redes",
                             INPUT 0,
                             INPUT 0,
                             OUTPUT GMFaplicado).

                IF INTEGER(baseGMF * 0.004) <> INTEGER(GMFaplicado) THEN
                    MESSAGE INTEGER(baseGMF * 0.004) GMFaplicado
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                totalGMF = totalGMF + GMFaplicado.

                totalMovAgenciaDB17 = totalMovAgenciaDB17 + GMFaplicado.
                totalMovAgenciaDB30 = totalMovAgenciaDB30 + GMFaplicado.
            END.
        END.

        IF LAST-OF(TT_Movimientos.cod_ahorro) THEN DO:
            FIND FIRST CortoLargo WHERE cortoLargo.clase_producto = 1
                                    AND cortoLargo.cod_producto = TT_Movimientos.cod_ahorro NO-LOCK NO-ERROR.
            IF AVAILABLE CortoLargo THEN DO:
                totalMovAgenciaCR17 = totalMovAgenciaCR17 + totalMovAgenciaDB17.
                totalMovAgenciaDB17 = 0.
            END.
        END.
        /*
        IF LAST-OF(TT_Movimientos.agencia) THEN DO:
            FIND FIRST CortoLargo WHERE cortoLargo.clase_producto = 1
                                    AND cortoLargo.cod_producto = TT_Movimientos.cod_ahorro
                                    AND cortoLargo.agencia = agencias.agencia NO-LOCK NO-ERROR.
            IF AVAILABLE CortoLargo THEN DO:
                totalMovAgenciaCR17 = 0.

                totalMovAgenciaCR24 = totalMovAgenciaCR24 + totalMovAgenciaDB24.
                totalmovAgenciaDB24 = 0.
                totalMovAgenciaCR30 = totalMovAgenciaCR30 + totalMovAgenciaDB30.
                totalmovAgenciaDB30 = 0.
            END.
        END.*/
    END.
END.

MESSAGE totalMovAgenciadb24 totalMovAgenciaDB30 totalGMF
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
