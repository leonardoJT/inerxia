DEFINE VAR vAgencia AS INTEGER INITIAL 3.
DEFINE VAR fec_corte  AS DATE INITIAL 04/30/2016.

DEFINE TEMP-TABLE tt
    FIELD id AS CHARACTER
    FIELD sdo_ini AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD avances AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD pagos AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD sdo_finRep AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD sdo_FinCalc AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD dif AS DECIMAL FORMAT ">>>,>>>,>>9.99".

FOR EACH rep_creditos WHERE rep_creditos.fecCorte = ADD-INTERVAL(fec_corte,DAY(fec_corte) * -1,"days")
                        AND rep_creditos.agencia = vAgencia
                        AND rep_creditos.cod_credito = 17 NO-LOCK:
    FIND FIRST tt WHERE tt.id = rep_creditos.nit NO-ERROR.
    IF NOT AVAILABLE tt THEN DO:
        CREATE tt.
        tt.id = rep_creditos.nit.
    END.

    /* Capital */
    /*tt.sdo_ini = tt.sdo_ini + rep_creditos.sdo_capital.*/

    /* Mora */
    tt.sdo_ini = tt.sdo_ini + rep_creditos.INT_morCobrar.
END.

FOR EACH mov_contable WHERE mov_contable.agencia = vAgencia
                        AND mov_contable.fec_contable >= ADD-INTERVAL(fec_corte,DAY(fec_corte) * -1,"days") + 1
                        AND mov_contable.fec_contable <= fec_corte
                        AND (mov_contable.cuenta = "1443050201" OR
                             mov_contable.cuenta = "1443050201") NO-LOCK:
    FIND FIRST tt WHERE tt.id = mov_contable.nit NO-ERROR.
    IF NOT AVAILABLE tt THEN DO:
        CREATE tt.
        tt.id = mov_contable.nit.
    END.

    tt.avances = tt.avances + mov_contable.db.
    tt.pagos = tt.pagos + mov_contable.cr.
END.

FOR EACH rep_creditos WHERE rep_creditos.fecCorte = fec_corte
                        AND rep_creditos.agencia = vAgencia
                        AND rep_creditos.cod_credito = 17
                        AND rep_creditos.estado = 2 NO-LOCK:
    FIND FIRST tt WHERE tt.id = rep_creditos.nit NO-ERROR.
    IF NOT AVAILABLE tt THEN DO:
        CREATE tt.
        tt.id = rep_creditos.nit.
    END.

    /* Capital */
    /*tt.sdo_finRep = tt.sdo_finRep + rep_creditos.sdo_capital.*/

    /* Mora */
    tt.sdo_finRep = tt.sdo_finRep + rep_creditos.INT_morCobrar.
END.

IF fec_corte >= TODAY THEN DO:
    FOR EACH creditos WHERE creditos.agencia = vAgencia
                        AND creditos.cod_credito = 17
                        AND creditos.estado = 2 NO-LOCK:
        FIND FIRST tt WHERE tt.id = creditos.nit NO-ERROR.
        IF NOT AVAILABLE tt THEN DO:
            CREATE tt.
            tt.id = creditos.nit.
        END.

        /* Capital */
        /*tt.sdo_finRep = tt.sdo_finRep + creditos.sdo_capital.*/

        /* Mora */
        tt.sdo_finRep = tt.sdo_finRep + creditos.INT_morCobrar.
    END.
END.


FOR EACH tt:
    tt.sdo_finCalc = tt.sdo_ini + tt.avances - tt.pagos.
    tt.dif = tt.sdo_finRep - tt.sdo_finCalc.

    IF tt.dif <> 0 THEN
        DISPLAY tt WITH 1 COL.
END.
