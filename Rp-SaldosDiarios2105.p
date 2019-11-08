DEFINE INPUT PARAMETER pFecCorte AS DATE.
DEFINE INPUT PARAMETER pRuta AS CHARACTER.

DEFINE TEMP-TABLE ttSaldos
    FIELD cod AS INTEGER INITIAL 1
    FIELD cont AS INTEGER
    FIELD descripcion AS CHARACTER INITIAL "SALDO DIA "
    FIELD saldo AS DECIMAL FORMAT ">>>>>>>>>>>9".

DEFINE VAR vCont AS INTEGER.
DEFINE VAR vSaldoFinal AS DECIMAL.

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,4) = "2105"
                      AND sal_cuenta.ano = YEAR(pFecCorte) NO-LOCK:
    vSaldoFinal = vSaldoFinal + sal_cuenta.sal_inicial.

    DO vCont = 1 TO MONTH(pFecCorte):
        vSaldoFinal = vSaldoFinal + sal_cuenta.cr[vCont] - sal_cuenta.db[vCont].
    END.
END.

CREATE ttSaldos.
ttSaldos.cont = DAY(pFecCorte).
ttSaldos.descripcion = ttSaldos.descripcion + STRING(DAY(pFecCorte)).
ttSaldos.saldo = vSaldoFinal.

DO vCont = DAY(pFecCorte) TO 2 BY -1:
    CREATE ttSaldos.
    ttSaldos.cont = vCont - 1.
    ttSaldos.descripcion = ttSaldos.descripcion + STRING(vCont - 1).

    FOR EACH agencias NO-LOCK:
        FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                                AND SUBSTRING(mov_contable.cuenta,1,4) = "2105"
                                AND mov_contable.fec_contable = DATE(MONTH(pFecCorte),vCont,YEAR(pFecCorte)) NO-LOCK:
            vSaldoFinal = vSaldoFinal - mov_contable.cr + mov_contable.db.
        END.

        FOR EACH mov_contable2 WHERE mov_contable2.agencia = agencias.agencia
                                AND mov_contable2.comprobante >= 0
                                AND mov_contable2.cen_costos >= 0
                                AND mov_contable2.fec_contable = DATE(MONTH(pFecCorte),vCont,YEAR(pFecCorte))
                                AND SUBSTRING(mov_contable2.cuenta,1,4) = "2105" NO-LOCK:
            vSaldoFinal = vSaldoFinal - mov_contable2.cr + mov_contable2.db.
        END.
    END.

    ttSaldos.saldo = vSaldoFinal.
END.

OUTPUT TO VALUE(pRuta).
FOR EACH ttSaldos NO-LOCK BY ttSaldos.cont:
    EXPORT DELIMITER ";" ttSaldos.
END.
OUTPUT CLOSE.
