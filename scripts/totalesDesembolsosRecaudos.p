DEFINE VAR cont AS INTEGER.

DEFINE TEMP-TABLE totales
    FIELD cod_producto AS INTEGER
    FIELD nombre_producto AS CHARACTER
    FIELD db1 AS DECIMAL
    FIELD cr1 AS DECIMAL
    FIELD db2 AS DECIMAL
    FIELD cr2 AS DECIMAL        
    FIELD db3 AS DECIMAL
    FIELD cr3 AS DECIMAL        
    FIELD db4 AS DECIMAL
    FIELD cr4 AS DECIMAL        
    FIELD db5 AS DECIMAL
    FIELD cr5 AS DECIMAL        
    FIELD db6 AS DECIMAL
    FIELD cr6 AS DECIMAL        
    FIELD db7 AS DECIMAL
    FIELD cr7 AS DECIMAL        
    FIELD db8 AS DECIMAL
    FIELD cr8 AS DECIMAL        
    FIELD db9 AS DECIMAL
    FIELD cr9 AS DECIMAL        
    FIELD db10 AS DECIMAL
    FIELD cr10 AS DECIMAL.

DEFINE TEMP-TABLE ttcuentas
    FIELD cuentas AS CHARACTER.
    
FOR EACH pro_ahorros WHERE pro_ahorros.estado = 1 NO-LOCK:
    CREATE totales.
    totales.cod_producto = pro_ahorros.cod_ahorro.
    totales.nombre_producto = pro_ahorros.nom_producto.

    /* Reviso la configuracion para capturar la cuenta contable */
    FIND FIRST cortoLargo WHERE cortoLargo.agencia = 1
                            AND cortoLargo.clase_producto = 1
                            AND cortoLargo.cod_producto = pro_ahorros.cod_ahorro NO-LOCK NO-ERROR.
    IF AVAILABLE cortoLargo THEN DO:
        FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = cortoLargo.Cta_AsoAd
                              AND sal_cuenta.ano = 2018 NO-LOCK:
            totales.db1 = totales.db1 + sal_cuenta.db[1].
            totales.cr1 = totales.cr1 + sal_cuenta.cr[1].
            totales.db2 = totales.db2 + sal_cuenta.db[2].
            totales.cr2 = totales.cr2 + sal_cuenta.cr[2].
            totales.db3 = totales.db3 + sal_cuenta.db[3].
            totales.cr3 = totales.cr3 + sal_cuenta.cr[3].
            totales.db4 = totales.db4 + sal_cuenta.db[4].
            totales.cr4 = totales.cr4 + sal_cuenta.cr[4].
            totales.db5 = totales.db5 + sal_cuenta.db[5].
            totales.cr5 = totales.cr5 + sal_cuenta.cr[5].
            totales.db6 = totales.db6 + sal_cuenta.db[6].
            totales.cr6 = totales.cr6 + sal_cuenta.cr[6].
            totales.db7 = totales.db7 + sal_cuenta.db[7].
            totales.cr7 = totales.cr7 + sal_cuenta.cr[7].
            totales.db8 = totales.db8 + sal_cuenta.db[8].
            totales.cr8 = totales.cr8 + sal_cuenta.cr[8].
            totales.db9 = totales.db9 + sal_cuenta.db[9].
            totales.cr9 = totales.cr9 + sal_cuenta.cr[9].
            totales.db10 = totales.db10 + sal_cuenta.db[10].
            totales.cr10 = totales.cr10 + sal_cuenta.cr[10].
        END.
    END.
END.

FOR EACH pro_creditos WHERE pro_creditos.estado = 1 NO-LOCK:
    CREATE totales.
    totales.cod_producto = pro_creditos.cod_credito.
    totales.nombre_producto = pro_creditos.nom_producto.

    FOR EACH carteraVencida WHERE carteraVencida.cod_producto = pro_creditos.cod_credito NO-LOCK:
        FIND FIRST ttCuentas WHERE ttCuentas.cuenta = carteraVencida.cta_AsoAddb NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttCuentas THEN DO:
            CREATE ttCuentas.
            ttCuentas.cuenta = carteraVencida.cta_AsoAddb.

            FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = ttCuentas.cuenta
                                  AND sal_cuenta.ano = 2018 NO-LOCK:
                DO cont = 1 TO 12:
                    totales.db1 = totales.db1 + sal_cuenta.db[1].
                    totales.cr1 = totales.cr1 + sal_cuenta.cr[1].
                    totales.db2 = totales.db2 + sal_cuenta.db[2].
                    totales.cr2 = totales.cr2 + sal_cuenta.cr[2].
                    totales.db3 = totales.db3 + sal_cuenta.db[3].
                    totales.cr3 = totales.cr3 + sal_cuenta.cr[3].
                    totales.db4 = totales.db4 + sal_cuenta.db[4].
                    totales.cr4 = totales.cr4 + sal_cuenta.cr[4].
                    totales.db5 = totales.db5 + sal_cuenta.db[5].
                    totales.cr5 = totales.cr5 + sal_cuenta.cr[5].
                    totales.db6 = totales.db6 + sal_cuenta.db[6].
                    totales.cr6 = totales.cr6 + sal_cuenta.cr[6].
                    totales.db7 = totales.db7 + sal_cuenta.db[7].
                    totales.cr7 = totales.cr7 + sal_cuenta.cr[7].
                    totales.db8 = totales.db8 + sal_cuenta.db[8].
                    totales.cr8 = totales.cr8 + sal_cuenta.cr[8].
                    totales.db9 = totales.db9 + sal_cuenta.db[9].
                    totales.cr9 = totales.cr9 + sal_cuenta.cr[9].
                    totales.db10 = totales.db10 + sal_cuenta.db[10].
                    totales.cr10 = totales.cr10 + sal_cuenta.cr[10].
                END.
            END.
        END.

        FIND FIRST ttCuentas WHERE ttCuentas.cuenta = carteraVencida.cta_Noaaddb NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttCuentas THEN DO:
            CREATE ttCuentas.
            ttCuentas.cuenta = carteraVencida.cta_Noaaddb.
            
            FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = ttCuentas.cuenta
                                  AND sal_cuenta.ano = 2018 NO-LOCK:
                DO cont = 1 TO 12:
                    totales.db1 = totales.db1 + sal_cuenta.db[1].
                    totales.cr1 = totales.cr1 + sal_cuenta.cr[1].
                    totales.db2 = totales.db2 + sal_cuenta.db[2].
                    totales.cr2 = totales.cr2 + sal_cuenta.cr[2].
                    totales.db3 = totales.db3 + sal_cuenta.db[3].
                    totales.cr3 = totales.cr3 + sal_cuenta.cr[3].
                    totales.db4 = totales.db4 + sal_cuenta.db[4].
                    totales.cr4 = totales.cr4 + sal_cuenta.cr[4].
                    totales.db5 = totales.db5 + sal_cuenta.db[5].
                    totales.cr5 = totales.cr5 + sal_cuenta.cr[5].
                    totales.db6 = totales.db6 + sal_cuenta.db[6].
                    totales.cr6 = totales.cr6 + sal_cuenta.cr[6].
                    totales.db7 = totales.db7 + sal_cuenta.db[7].
                    totales.cr7 = totales.cr7 + sal_cuenta.cr[7].
                    totales.db8 = totales.db8 + sal_cuenta.db[8].
                    totales.cr8 = totales.cr8 + sal_cuenta.cr[8].
                    totales.db9 = totales.db9 + sal_cuenta.db[9].
                    totales.cr9 = totales.cr9 + sal_cuenta.cr[9].
                    totales.db10 = totales.db10 + sal_cuenta.db[10].
                    totales.cr10 = totales.cr10 + sal_cuenta.cr[10].
                END.
            END.
        END.
    END.
END.


OUTPUT TO d:\Leonardo\RecaudosAvances_2018.csv.
EXPORT DELIMITER ";" "COD_PRODUCTO" "NOMBRE_PRODUCTO" "DB_1" "CR_1" "DB_2" "CR_2" "DB_3" "CR_3" "DB_4" "CR_4" "DB_5" "CR_5" "DB_6" "CR_6" "DB_7" "CR_7" "DB_8" "CR_8" "DB_9" "CR_9" "DB_10" "CR_10".
FOR EACH totales NO-LOCK:
    EXPORT DELIMITER ";" totales.
END.
OUTPUT CLOSE.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
