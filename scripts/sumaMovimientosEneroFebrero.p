DEFINE VAR totalDB AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9.99".
DEFINE VAR totalCR AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9.99".

DISABLE TRIGGERS FOR LOAD OF mov_contable.

OUTPUT TO c:\INFO_Fodun\Leonardo\cuentasFebrero.csv.
EXPORT DELIMITER ";" "AGENCIA" "CUENTA" "CEN_COSTOS" "DB_BALANCE" "CR_BALANCE" "SALDO_BALANCE" " " "DB_MOVIMIENTOS" "CR_MOVIMIENTOS" "SALDO_MOVMIENTOS".

FOR EACH agencias NO-LOCK:
    FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                          AND sal_cuenta.ano = 2011
                          AND sal_cuenta.cen_costos = 999 NO-LOCK:
        totalDB = 0.
        totalCR = 0.

        FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                                AND mov_contable.cuenta = sal_cuenta.cuenta
                                AND mov_contable.fec_contable >= 02/01/2011
                                AND mov_contable.fec_contable <= 02/28/2011
                                /*AND mov_contable.cen_costos = sal_cuenta.cen_costos*/ NO-LOCK:
            totalDB = totalDB + mov_contable.db.
            totalCR = totalCR + mov_contable.CR.
        END.

        IF sal_cuenta.db[2] - sal_cuenta.cr[2] <> totalDB - totalCR THEN
            EXPORT DELIMITER ";" sal_cuenta.agencia
                                 sal_cuenta.cuenta
                                 sal_cuenta.cen_costos
                                 sal_cuenta.db[2]
                                 sal_cuenta.cr[2]
                                 sal_cuenta.db[2] - sal_cuenta.cr[2]
                                 " "
                                 totalDB
                                 totalCR
                                 totalDB - totalCR.
    END.
END.
