OUTPUT TO c:\INFO_fodun\Cartera.txt.

DEFINE VAR totalCreditos AS DECIMAL.
DEFINE VAR totalCaja AS DECIMAL.
DEFINE VAR totalNomina AS DECIMAL.
DEFINE VAR saldoCuentaNomina AS DECIMAL.
DEFINE VAR saldoCuentaCaja AS DECIMAL.
DEFINE VAR totalDiferenciaNomina AS DECIMAL.
DEFINE VAR totalDiferenciaCaja AS DECIMAL.
DEFINE VAR pAgencia AS INTEGER INITIAL 4.
DEFINE VAR i AS INTEGER.

FOR EACH carteraVencida WHERE carteraVencida.cta_AsoAddb <> carteraVencida.cta_Noaaddb NO-LOCK BREAK BY carteraVencida.cta_AsoAdDB:
    FOR EACH creditos WHERE creditos.agencia = pAgencia
                        AND creditos.cod_credito = carteraVencida.cod_producto
                        AND creditos.sdo_Capital > 0
                        AND creditos.dias_atraso >= carteraVencida.per_inicial
                        AND creditos.dias_atraso <= carteraVencida.per_final
                        AND creditos.for_pago = 2 NO-LOCK:
        totalNomina = totalNomina + creditos.sdo_Capital.
    END.

    IF LAST-OF(carteraVencida.cta_AsoAddb) THEN DO:
        FOR EACH sal_cuenta WHERE sal_cuenta.agencia = pAgencia
                              AND sal_cuenta.ano = 2011
                              AND sal_cuenta.cuenta = carteraVencida.cta_AsoAdDb NO-LOCK:
            saldoCuentaNomina = saldoCuentaNomina + sal_cuenta.sal_inicial.

            DO i = 1 TO 9:
                saldoCuentaNomina = saldoCuentaNomina + sal_cuenta.db[i] - sal_cuenta.cr[i].
            END.
        END.

        IF saldoCuentaNomina - totalNomina <> 0 THEN
            DISPLAY carteraVencida.cod_producto
                    carteraVencida.categoria
                    carteraVencida.cta_AsoAdDb
                    saldoCuentaNomina FORMAT "->>>,>>>,>>>,>>9.99"
                    totalNomina FORMAT "->>>,>>>,>>>,>>9.99"
                    saldoCuentaNomina - totalNomina FORMAT "->>>,>>>,>>>,>>9.99"
                WITH WIDTH 200.

        totalDiferenciaNomina = totalDiferenciaNomina + saldoCuentaNomina - totalNomina.

        totalCaja = 0.
        totalNomina = 0.
        saldoCuentaNomina = 0.
        saldoCuentaCaja = 0.
    END.
END.

MESSAGE "Diferencia Nómina" totalDiferenciaNomina
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH carteraVencida WHERE carteraVencida.cta_AsoAddb <> carteraVencida.cta_Noaaddb NO-LOCK BREAK BY carteraVencida.cta_NoAAdDB:
    FOR EACH creditos WHERE creditos.agencia = pAgencia
                        AND creditos.cod_credito = carteraVencida.cod_producto
                        AND creditos.sdo_Capital > 0
                        AND creditos.dias_atraso >= carteraVencida.per_inicial
                        AND creditos.dias_atraso <= carteraVencida.per_final
                        AND creditos.for_pago = 1 NO-LOCK:
        totalCaja = totalCaja + creditos.sdo_Capital.
    END.

    IF LAST-OF(carteraVencida.cta_NoAAddb) THEN DO:
        FOR EACH sal_cuenta WHERE sal_cuenta.agencia = pAgencia
                              AND sal_cuenta.ano = 2011
                              AND sal_cuenta.cuenta = carteraVencida.cta_NoAAdDb NO-LOCK:
            saldoCuentaCaja = saldoCuentaCaja + sal_cuenta.sal_inicial.

            DO i = 1 TO 9:
                saldoCuentaCaja = saldoCuentaCaja + sal_cuenta.db[i] - sal_cuenta.cr[i].
            END.
        END.

        IF saldoCuentaCaja - totalCaja <> 0 THEN
            DISPLAY carteraVencida.cod_producto
                    carteraVencida.categoria
                    carteraVencida.cta_NoAAdDb
                    saldoCuentaCaja FORMAT "->>>,>>>,>>>,>>9.99"
                    totalCaja FORMAT "->>>,>>>,>>>,>>9.99"
                    saldoCuentaCaja - totalCaja FORMAT "->>>,>>>,>>>,>>9.99"
                WITH WIDTH 200.

        totalDiferenciaCaja = totalDiferenciaCaja + saldoCuentaCaja - totalCaja.

        totalCaja = 0.
        totalNomina = 0.
        saldoCuentaNomina = 0.
        saldoCuentaCaja = 0.
    END.
END.

MESSAGE "Diferencia Caja" totalDiferenciaCaja
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

totalDiferenciaCaja = 0.


FOR EACH carteraVencida WHERE carteraVencida.cta_AsoAddb = carteraVencida.cta_Noaaddb NO-LOCK BREAK BY carteraVencida.cta_NoAAdDB:
    FOR EACH creditos WHERE creditos.agencia = pAgencia
                        AND creditos.cod_credito = carteraVencida.cod_producto
                        AND creditos.sdo_Capital > 0
                        AND creditos.dias_atraso >= carteraVencida.per_inicial
                        AND creditos.dias_atraso <= carteraVencida.per_final NO-LOCK:
        totalCaja = totalCaja + creditos.sdo_Capital.
    END.

    IF LAST-OF(carteraVencida.cta_NoAAddb) THEN DO:
        FOR EACH sal_cuenta WHERE sal_cuenta.agencia = pAgencia
                              AND sal_cuenta.ano = 2011
                              AND sal_cuenta.cuenta = carteraVencida.cta_NoAAdDb NO-LOCK:
            saldoCuentaCaja = saldoCuentaCaja + sal_cuenta.sal_inicial.

            DO i = 1 TO 9:
                saldoCuentaCaja = saldoCuentaCaja + sal_cuenta.db[i] - sal_cuenta.cr[i].
            END.
        END.

        IF saldoCuentaCaja - totalCaja <> 0 THEN
            DISPLAY carteraVencida.cod_producto
                    carteraVencida.categoria
                    carteraVencida.cta_NoAAdDb
                    saldoCuentaCaja FORMAT "->>>,>>>,>>>,>>9.99"
                    totalCaja FORMAT "->>>,>>>,>>>,>>9.99"
                    saldoCuentaCaja - totalCaja FORMAT "->>>,>>>,>>>,>>9.99"
                WITH WIDTH 200.

        totalDiferenciaCaja = totalDiferenciaCaja + saldoCuentaCaja - totalCaja.

        totalCaja = 0.
        totalNomina = 0.
        saldoCuentaNomina = 0.
        saldoCuentaCaja = 0.
    END.
END.

MESSAGE "Diferencia Caja2" totalDiferenciaCaja
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
