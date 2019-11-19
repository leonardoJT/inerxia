/* Riesgo de Liquidez */

/* Parámetros de entrada */
DEFINE VAR fec_corte AS DATE INITIAL 02/29/2016.

DEFINE VAR time1 AS INTEGER.
DEFINE VAR time2 AS INTEGER.
DEFINE VAR saldoDia AS DECIMAL.
DEFINE VAR saldoPromedioAno AS DECIMAL.
DEFINE VAR cont AS INTEGER.
DEFINE VAR contFec AS DATE.
DEFINE VAR sumaSaldos AS DECIMAL.
DEFINE VAR diferencia AS DECIMAL.

/*DEFINE VAR vSaldoActual AS DECIMAL.
DEFINE VAR saldo AS DECIMAL.
DEFINE VAR saldosBandas AS DECIMAL EXTENT 7.
DEFINE VAR saldoPromedioDiaYear AS DECIMAL.*/

DEFINE TEMP-TABLE registros
    FIELD tipo AS INTEGER
    FIELD codRenglon AS CHARACTER
    FIELD nombreRenglon AS CHARACTER
    FIELD saldoActual AS DECIMAL
    FIELD banda AS DECIMAL EXTENT 7.

DEFINE VAR ahorrosCortes AS DECIMAL EXTENT 6.
DEFINE VAR fondoLiquidezCortes AS DECIMAL EXTENT 6.

/*DEFINE TEMP-TABLE ttRegistros LIKE registros.
DEFINE VAR sdoPromedioDiaYear AS DECIMAL.*/

time1 = TIME.

/* 1 - 005 - Disponible */
CREATE registros.
registros.tipo = 1.
registros.codRenglon = "005".
registros.nombreRenglon = "DISPONIBLE".

/* Saldo actual */
FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,2) = "11"
                      AND SUBSTRING(sal_cuenta.cuenta,1,4) <> "1120"
                      AND sal_cuenta.ano = YEAR(fec_corte) NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

    IF AVAILABLE cuentas THEN DO:
        registros.SaldoActual = registros.SaldoActual + round(sal_cuenta.sal_inicial,0).

        DO cont = 1 TO MONTH(fec_corte):
            IF cuentas.naturaleza = "DB" THEN
                registros.SaldoActual = registros.SaldoActual + round(sal_cuenta.db[cont],0) - round(sal_cuenta.cr[cont],0).
            ELSE
                registros.SaldoActual = registros.SaldoActual - round(sal_cuenta.db[cont],0) + round(sal_cuenta.cr[cont],0).
        END.
    END.
END.

/* Saldo promedio año */
FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,2) = "11"
                      AND SUBSTRING(sal_cuenta.cuenta,1,4) <> "1120"
                      AND sal_cuenta.ano = YEAR(ADD-INTERVAL(fec_corte, -1, "years")) NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.
    
    IF AVAILABLE cuentas THEN DO:
        saldoDia = saldoDia + sal_cuenta.sal_inicial.

        DO cont = 1 TO MONTH(ADD-INTERVAL(fec_corte, -1, "years")):
            IF cuentas.naturaleza = "DB" THEN
                saldoDia = saldoDia + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
            ELSE
                saldoDia = saldoDia - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
        END.
    END.
END.

cont = 0.

DO contFec = ADD-INTERVAL(fec_corte, -1, "years") + 1 TO fec_corte:
    IF cont = 0 THEN

    FOR EACH agencias NO-LOCK:
        FOR EACH cuentas WHERE SUBSTRING(cuentas.cuenta,1,2) = "11" AND SUBSTRING(cuentas.cuenta,1,4) <> "1120" NO-LOCK:
            FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                                    AND mov_contable.cuenta = cuentas.cuenta
                                    AND mov_contable.fec_contable = contFec NO-LOCK:
                IF cuentas.naturaleza = "DB" THEN
                    saldoDia = saldoDia + mov_contable.db - mov_contable.cr.
                ELSE
                    saldoDia = saldoDia - mov_contable.db + mov_contable.cr.
            END.
        END.
    END.

    sumaSaldos = sumaSaldos + saldoDia.
    cont = cont + 1.
END.

saldoPromedioAno = ROUND(sumaSaldos / cont,0).
registros.banda[7] = saldoPromedioAno.

IF registros.saldoActual > saldoPromedioAno THEN DO:
    diferencia = registros.saldoActual - saldoPromedioAno.

    RUN maduraDisponible(INPUT 12, INPUT 1, INPUT 1).
    RUN maduraDisponible(INPUT 11, INPUT 1, INPUT 2).
    RUN maduraDisponible(INPUT 10, INPUT 1, INPUT 3).
    RUN maduraDisponible(INPUT 9, INPUT 3, INPUT 4).
    RUN maduraDisponible(INPUT 6, INPUT 3, INPUT 5).
    RUN maduraDisponible(INPUT 3, INPUT 3, INPUT 6).
END.
ELSE
    registros.banda[7] = registros.saldoActual.

/* ----------------------- */

/* 1 - 007 - Inversiones negociables */
CREATE registros.
registros.tipo = 1.
registros.codRenglon = "007".
registros.nombreRenglon = "INVERSIONES NEGOCIABLES".

/* Saldo actual */
FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "1204" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "1206")
                      AND sal_cuenta.ano = YEAR(fec_corte) NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

    IF AVAILABLE cuentas THEN DO:
        registros.SaldoActual = registros.SaldoActual + round(sal_cuenta.sal_inicial,0).

        DO cont = 1 TO MONTH(fec_corte):
            IF cuentas.naturaleza = "DB" THEN
                registros.SaldoActual = registros.SaldoActual + round(sal_cuenta.db[cont],0) - round(sal_cuenta.cr[cont],0).
            ELSE
                registros.SaldoActual = registros.SaldoActual - round(sal_cuenta.db[cont],0) + round(sal_cuenta.cr[cont],0).
        END.
    END.
END.

registros.banda[7] = registros.saldoActual.


/* 1 - 015 - Fondo de Liquidez */
CREATE registros.
registros.tipo = 1.
registros.codRenglon = "015".
registros.nombreRenglon = "FONDO DE LIQUIDEZ".

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "1203" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "1120")
                      AND sal_cuenta.ano = YEAR(fec_corte) NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

    IF AVAILABLE cuentas THEN DO:
        registros.saldoActual = registros.saldoActual + round(sal_cuenta.sal_inicial,0).

        DO cont = 1 TO MONTH(fec_corte):
            IF cuentas.naturaleza = "DB" THEN
                registros.saldoActual = registros.saldoActual + round(sal_cuenta.db[cont],0) - round(sal_cuenta.cr[cont],0).
            ELSE
                registros.saldoActual = registros.saldoActual - round(sal_cuenta.db[cont],0) + round(sal_cuenta.cr[cont],0).
        END.
    END.
END.

RUN procesoFondoDeLiquidez(INPUT 11, INPUT 1).
RUN procesoFondoDeLiquidez(INPUT 10, INPUT 2).
RUN procesoFondoDeLiquidez(INPUT 9, INPUT 3).
RUN procesoFondoDeLiquidez(INPUT 8, INPUT 4).
RUN procesoFondoDeLiquidez(INPUT 7, INPUT 4).
RUN procesoFondoDeLiquidez(INPUT 6, INPUT 4).
RUN procesoFondoDeLiquidez(INPUT 5, INPUT 5).
RUN procesoFondoDeLiquidez(INPUT 4, INPUT 5).
RUN procesoFondoDeLiquidez(INPUT 3, INPUT 5).
RUN procesoFondoDeLiquidez(INPUT 2, INPUT 6).
RUN procesoFondoDeLiquidez(INPUT 1, INPUT 6).
RUN procesoFondoDeLiquidez(INPUT 0, INPUT 6).

diferencia = registros.saldoActual.

DO cont = 1 TO 6:
    IF ahorrosCortes[cont] > fondoLiquidezCortes[cont] THEN DO:
        IF diferencia >= ahorrosCortes[cont] - fondoLiquidezCortes[cont] THEN DO:
            registros.banda[cont] = ahorrosCortes[cont] - fondoLiquidezCortes[cont].
            diferencia = diferencia - registros.banda[cont].
        END.
        ELSE DO:
            registros.banda[cont] = diferencia.
            diferencia = 0.
            LEAVE.
        END.
    END.
END.

registros.banda[7] = diferencia.

/* ---------------------------- */

/*
/* 1 - 020 - Compromiso de Reventa de Inversiones */
/*CREATE registros.
registros.tipo = 1.
registros.codRenglon = "020".
registros.nombreRenglon = "COMPROMISO DE REVENTA DE INVERSIONES".*/
/* ------------------------------- */


/* 1 - 025 - Compromiso de Reventa de Inversiones */
/*CREATE registros.
registros.tipo = 1.
registros.codRenglon = "025".
registros.nombreRenglon = "COMPROMISO DE REVENTA DE CARTERA".*/
/* ------------------------------- */


/* 1 - 028 - Inversiones para mantener hasta el vencimiento */
/*CREATE registros.
registros.tipo = 1.
registros.codRenglon = "028".
registros.nombreRenglon = "INVERSIONES PARA MANTENER HASTA EL VENCIMIENTO".*/
/* ------------------------------- */


/* 1 - 035 - Derechos de recompra de inversiones */
/*CREATE registros.
registros.tipo = 1.
registros.codRenglon = "035".
registros.nombreRenglon = "DERECHOS DE RECOMPRA DE INVERSIONES".*/
/* ------------------------------- */


/* 1 - 038 - Inversiones disponibles para la venta */
/*CREATE registros.
registros.tipo = 1.
registros.codRenglon = "038".
registros.nombreRenglon = "INVERSIONES DISPONIBLES PARA LA VENTA".*/
/* ------------------------------- */


/* 1 - 045 - Inventarios */
/*CREATE registros.
registros.tipo = 1.
registros.codRenglon = "045".
registros.nombreRenglon = "INVENTARIOS".*/
/* ------------------------------- */


/* 1 - 050 - Cartera de Créditos Consumo */
/*CREATE registros.
registros.tipo = 1.
registros.codRenglon = "050".
registros.nombreRenglon = "CARTERA DE CRÉDITOS CONSUMO".

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "1411" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "1412" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "1441" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "1442")
                      AND sal_cuenta.ano = YEAR(fec_corte) NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

    IF AVAILABLE cuentas THEN DO:
        registros.saldoActual = registros.saldoActual + round(sal_cuenta.sal_inicial,0).

        DO cont = 1 TO MONTH(fec_corte):
            IF cuentas.naturaleza = "DB" THEN
                registros.saldoActual = registros.saldoActual + round(sal_cuenta.db[cont],0) - round(sal_cuenta.cr[cont],0).
            ELSE
                registros.saldoActual = registros.saldoActual - round(sal_cuenta.db[cont],0) + round(sal_cuenta.cr[cont],0).
        END.
    END.
END.

FOR EACH rep_creditos WHERE rep_creditos.fecCorte = fec_corte
                        AND rep_creditos.tip_credito = 1
                        AND rep_creditos.cod_credito <> 62
                        AND rep_creditos.categoriames = "A" NO-LOCK:
    RUN carteraCreditos(INPUT 1, INPUT 0, INPUT 1).
    RUN carteraCreditos(INPUT 2, INPUT 1, INPUT 2).
    RUN carteraCreditos(INPUT 3, INPUT 2, INPUT 3).
    RUN carteraCreditos(INPUT 4, INPUT 3, INPUT 6).
    RUN carteraCreditos(INPUT 5, INPUT 6, INPUT 9).
    RUN carteraCreditos(INPUT 6, INPUT 9, INPUT 12).
    RUN carteraCreditos(INPUT 7, INPUT 12, INPUT 9999).
END.*/
/* ------------------------------- */


/* 1 - 055 - Cartera de Créditos Comercial */
/*CREATE registros.
registros.tipo = 1.
registros.codRenglon = "055".
registros.nombreRenglon = "CARTERA DE CRÉDITOS COMERCIAL".

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "1460" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "1462" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "1463" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "1465")
                      AND sal_cuenta.ano = YEAR(fec_corte) NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

    IF AVAILABLE cuentas THEN DO:
        registros.saldoActual = registros.saldoActual + round(sal_cuenta.sal_inicial,0).

        DO cont = 1 TO MONTH(fec_corte):
            IF cuentas.naturaleza = "DB" THEN
                registros.saldoActual = registros.saldoActual + round(sal_cuenta.db[cont],0) - round(sal_cuenta.cr[cont],0).
            ELSE
                registros.saldoActual = registros.saldoActual - round(sal_cuenta.db[cont],0) + round(sal_cuenta.cr[cont],0).
        END.
    END.
END.

FOR EACH rep_creditos WHERE rep_creditos.fecCorte = fec_corte
                        AND rep_creditos.tip_credito = 2
                        AND rep_creditos.cod_credito <> 62
                        AND rep_creditos.categoriames = "A" NO-LOCK:
    RUN carteraCreditos(INPUT 1, INPUT 0, INPUT 1).
    RUN carteraCreditos(INPUT 2, INPUT 1, INPUT 2).
    RUN carteraCreditos(INPUT 3, INPUT 2, INPUT 3).
    RUN carteraCreditos(INPUT 4, INPUT 3, INPUT 6).
    RUN carteraCreditos(INPUT 5, INPUT 6, INPUT 9).
    RUN carteraCreditos(INPUT 6, INPUT 9, INPUT 12).
    RUN carteraCreditos(INPUT 7, INPUT 12, INPUT 9999).
END.*/
/* ------------------------------- */


/* 1 - 060 - Cartera de Créditos Vivienda */
/*CREATE registros.
registros.tipo = 1.
registros.codRenglon = "060".
registros.nombreRenglon = "CARTERA DE CRÉDITOS VIVIENDA".

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "1404" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "1405")
                      AND sal_cuenta.ano = YEAR(fec_corte) NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

    IF AVAILABLE cuentas THEN DO:
        registros.saldoActual = registros.saldoActual + round(sal_cuenta.sal_inicial,0).

        DO cont = 1 TO MONTH(fec_corte):
            IF cuentas.naturaleza = "DB" THEN
                registros.saldoActual = registros.saldoActual + round(sal_cuenta.db[cont],0) - round(sal_cuenta.cr[cont],0).
            ELSE
                registros.saldoActual = registros.saldoActual - round(sal_cuenta.db[cont],0) + round(sal_cuenta.cr[cont],0).
        END.
    END.
END.

FOR EACH rep_creditos WHERE rep_creditos.fecCorte = fec_corte
                        AND rep_creditos.tip_credito = 3
                        AND rep_creditos.cod_credito <> 62
                        AND rep_creditos.categoriames = "A" NO-LOCK:
    RUN carteraCreditos(INPUT 1, INPUT 0, INPUT 1).
    RUN carteraCreditos(INPUT 2, INPUT 1, INPUT 2).
    RUN carteraCreditos(INPUT 3, INPUT 2, INPUT 3).
    RUN carteraCreditos(INPUT 4, INPUT 3, INPUT 6).
    RUN carteraCreditos(INPUT 5, INPUT 6, INPUT 9).
    RUN carteraCreditos(INPUT 6, INPUT 9, INPUT 12).
    RUN carteraCreditos(INPUT 7, INPUT 12, INPUT 9999).
END.*/
/* ------------------------------- */


/* 1 - 062 - Cartera de Microcrédito inmobiliario */
/*CREATE registros.
registros.tipo = 1.
registros.codRenglon = "062".
registros.nombreRenglon = "CARTERA DE MICROCRÉDITO INMOBILIARIO".

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "1454" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "1455")
                      AND sal_cuenta.ano = YEAR(fec_corte) NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

    IF AVAILABLE cuentas THEN DO:
        registros.saldoActual = registros.saldoActual + round(sal_cuenta.sal_inicial,0).

        DO cont = 1 TO MONTH(fec_corte):
            IF cuentas.naturaleza = "DB" THEN
                registros.saldoActual = registros.saldoActual + round(sal_cuenta.db[cont],0) - round(sal_cuenta.cr[cont],0).
            ELSE
                registros.saldoActual = registros.saldoActual - round(sal_cuenta.db[cont],0) + round(sal_cuenta.cr[cont],0).
        END.
    END.
END.

FOR EACH rep_creditos WHERE rep_creditos.fecCorte = fec_corte
                        AND rep_creditos.tip_credito = 4
                        AND rep_creditos.cod_credito <> 62
                        AND rep_creditos.categoriames = "A"
                        AND (SUBSTRING(rep_creditos.cta_contable,1,4) = "1454" OR SUBSTRING(rep_creditos.cta_contable,1,4) = "1455") NO-LOCK:
    RUN carteraCreditos(INPUT 1, INPUT 0, INPUT 1).
    RUN carteraCreditos(INPUT 2, INPUT 1, INPUT 2).
    RUN carteraCreditos(INPUT 3, INPUT 2, INPUT 3).
    RUN carteraCreditos(INPUT 4, INPUT 3, INPUT 6).
    RUN carteraCreditos(INPUT 5, INPUT 6, INPUT 9).
    RUN carteraCreditos(INPUT 6, INPUT 9, INPUT 12).
    RUN carteraCreditos(INPUT 7, INPUT 12, INPUT 9999).
END.*/
/* ------------------------------- */


/* 1 - 065 - Cartera de créditos microcréditos */
/*CREATE registros.
registros.tipo = 1.
registros.codRenglon = "065".
registros.nombreRenglon = "CARTERA CRÉDITOS MICROCRÉDITOS".

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "1456" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "1457" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "1458" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "1459")
                      AND sal_cuenta.ano = YEAR(fec_corte) NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

    IF AVAILABLE cuentas THEN DO:
        registros.saldoActual = registros.saldoActual + round(sal_cuenta.sal_inicial,0).

        DO cont = 1 TO MONTH(fec_corte):
            IF cuentas.naturaleza = "DB" THEN
                registros.saldoActual = registros.saldoActual + round(sal_cuenta.db[cont],0) - round(sal_cuenta.cr[cont],0).
            ELSE
                registros.saldoActual = registros.saldoActual - round(sal_cuenta.db[cont],0) + round(sal_cuenta.cr[cont],0).
        END.
    END.
END.

FOR EACH rep_creditos WHERE rep_creditos.fecCorte = fec_corte
                        AND rep_creditos.tip_credito = 4
                        AND rep_creditos.cod_credito <> 62
                        AND rep_creditos.categoriames = "A"
                        AND (SUBSTRING(rep_creditos.cta_contable,1,4) = "1456" OR SUBSTRING(rep_creditos.cta_contable,1,4) = "1457" OR
                             SUBSTRING(rep_creditos.cta_contable,1,4) = "1458" OR SUBSTRING(rep_creditos.cta_contable,1,4) = "1459") NO-LOCK:
    RUN carteraCreditos(INPUT 1, INPUT 0, INPUT 1).
    RUN carteraCreditos(INPUT 2, INPUT 1, INPUT 2).
    RUN carteraCreditos(INPUT 3, INPUT 2, INPUT 3).
    RUN carteraCreditos(INPUT 4, INPUT 3, INPUT 6).
    RUN carteraCreditos(INPUT 5, INPUT 6, INPUT 9).
    RUN carteraCreditos(INPUT 6, INPUT 9, INPUT 12).
    RUN carteraCreditos(INPUT 7, INPUT 12, INPUT 9999).
END.*/
/* ------------------------------- */


/* 1 - 075 - Cuentas por Cobrar */
/*CREATE registros.
registros.tipo = 1.
registros.codRenglon = "075".
registros.nombreRenglon = "CUENTAS POR COBRAR".

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,2) = "16"
                      AND sal_cuenta.ano = YEAR(fec_corte) NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

    IF AVAILABLE cuentas THEN DO:
        registros.saldoActual = registros.saldoActual + round(sal_cuenta.sal_inicial,0).

        DO cont = 1 TO MONTH(fec_corte):
            IF cuentas.naturaleza = "DB" THEN
                registros.saldoActual = registros.saldoActual + round(sal_cuenta.db[cont],0) - round(sal_cuenta.cr[cont],0).
            ELSE
                registros.saldoActual = registros.saldoActual - round(sal_cuenta.db[cont],0) + round(sal_cuenta.cr[cont],0).
        END.
    END.
END.

/* Intereses causados no recibidos */
FOR EACH rep_creditos WHERE rep_creditos.fecCorte = fec_corte
                        AND rep_creditos.cod_credito <> 62 /* Para FODUN - Créditos de Empleados */
                        AND rep_creditos.categoriames = "A" NO-LOCK:
    registros.banda[1] = registros.banda[1] + round(rep_creditos.INT_corriente + rep_creditos.INT_morCobrar,0).
END.

/* Deudores patronales */
FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "1650" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "1620" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "1625" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "1635")
                      AND sal_cuenta.ano = YEAR(fec_corte) NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

    IF AVAILABLE cuentas THEN DO:
        registros.banda[1] = registros.banda[1] + round(sal_cuenta.sal_inicial,0).

        DO cont = 1 TO MONTH(fec_corte):
            IF cuentas.naturaleza = "DB" THEN
                registros.banda[1] = registros.banda[1] + round(sal_cuenta.db[cont],0) - round(sal_cuenta.cr[cont],0).
            ELSE
                registros.banda[1] = registros.banda[1] - round(sal_cuenta.db[cont],0) + round(sal_cuenta.cr[cont],0).
        END.
    END.
END.

/* Créditos de empleados */
FOR EACH rep_creditos WHERE rep_creditos.fecCorte = fec_corte
                        AND rep_creditos.cod_credito = 62 /* Créditos de empleados para FODUN */
                        AND rep_creditos.categoriames = "A" NO-LOCK:
    RUN carteraCreditos(INPUT 1, INPUT 0, INPUT 1).
    RUN carteraCreditos(INPUT 2, INPUT 1, INPUT 2).
    RUN carteraCreditos(INPUT 3, INPUT 2, INPUT 3).
    RUN carteraCreditos(INPUT 4, INPUT 3, INPUT 6).
    RUN carteraCreditos(INPUT 5, INPUT 6, INPUT 9).
    RUN carteraCreditos(INPUT 6, INPUT 9, INPUT 12).
    RUN carteraCreditos(INPUT 7, INPUT 12, INPUT 9999).
END.*/
/* ------------------------------- */


/* 1 - 080 - Propiedad Planta y Equipo */
/*CREATE registros.
registros.tipo = 1.
registros.codRenglon = "080".
registros.nombreRenglon = "PROPIEDAD PLANTA Y EQUIPO".

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,2) = "17"
                      AND sal_cuenta.ano = YEAR(fec_corte) NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

    IF AVAILABLE cuentas THEN DO:
        registros.saldoActual = registros.saldoActual + round(sal_cuenta.sal_inicial,0).

        DO cont = 1 TO MONTH(fec_corte):
            IF cuentas.naturaleza = "DB" THEN
                registros.saldoActual = registros.saldoActual + round(sal_cuenta.db[cont],0) - round(sal_cuenta.cr[cont],0).
            ELSE
                registros.saldoActual = registros.saldoActual - round(sal_cuenta.db[cont],0) + round(sal_cuenta.cr[cont],0).
        END.
    END.
END.

registros.banda[7] = registros.saldoActual.*/
/* ------------------------------- */


/* 1 - 085 - Diferidos */
/*CREATE registros.
registros.tipo = 1.
registros.codRenglon = "085".
registros.nombreRenglon = "DIFERIDOS".

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,2) = "18"
                      AND sal_cuenta.ano = YEAR(fec_corte) NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

    IF AVAILABLE cuentas THEN DO:
        registros.saldoActual = registros.saldoActual + round(sal_cuenta.sal_inicial,0).

        DO cont = 1 TO MONTH(fec_corte):
            IF cuentas.naturaleza = "DB" THEN
                registros.saldoActual = registros.saldoActual + round(sal_cuenta.db[cont],0) - round(sal_cuenta.cr[cont],0).
            ELSE
                registros.saldoActual = registros.saldoActual - round(sal_cuenta.db[cont],0) + round(sal_cuenta.cr[cont],0).
        END.
    END.
END.

registros.banda[7] = registros.saldoActual.*/
/* ------------------------------- */


/* 1 - 090 - Otros Activos */
/*CREATE registros.
registros.tipo = 1.
registros.codRenglon = "090".
registros.nombreRenglon = "OTROS ACTIVOS".

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,2) = "19"
                      AND sal_cuenta.ano = YEAR(fec_corte) NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

    IF AVAILABLE cuentas THEN DO:
        registros.saldoActual = registros.saldoActual + round(sal_cuenta.sal_inicial,0).

        DO cont = 1 TO MONTH(fec_corte):
            IF cuentas.naturaleza = "DB" THEN
                registros.saldoActual = registros.saldoActual + round(sal_cuenta.db[cont],0) - round(sal_cuenta.cr[cont],0).
            ELSE
                registros.saldoActual = registros.saldoActual - round(sal_cuenta.db[cont],0) + round(sal_cuenta.cr[cont],0).
        END.
    END.
END.

registros.banda[7] = registros.saldoActual.*/
/* ------------------------------- */


/* 1 - 095 - Contingentes Deudoras */
/*CREATE registros.
registros.tipo = 1.
registros.codRenglon = "095".
registros.nombreRenglon = "CONTINGENTES DEUDORAS".

vSaldoActual = 0.

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "8120" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "8115")
                      AND sal_cuenta.ano = YEAR(fec_corte) NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

    IF AVAILABLE cuentas THEN DO:
        vSaldoActual = vSaldoActual + sal_cuenta.sal_inicial.

        DO cont = 1 TO MONTH(fec_corte):
            IF cuentas.naturaleza = "DB" THEN
                vSaldoActual = vSaldoActual + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
            ELSE
                vSaldoActual = vSaldoActual - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
        END.
    END.
END.

registros.saldoActual = vSaldoActual.

RUN contingentesDeudoras(INPUT 12, INPUT 1,  INPUT 1).
RUN contingentesDeudoras(INPUT 11, INPUT 1, INPUT 2).
RUN contingentesDeudoras(INPUT 10, INPUT 1, INPUT 3).
RUN contingentesDeudoras(INPUT 9, INPUT 3, INPUT 4).
RUN contingentesDeudoras(INPUT 6, INPUT 3, INPUT 5).
RUN contingentesDeudoras(INPUT 3, INPUT 3, INPUT 6).

registros.banda[7] = vSaldoActual.*/
/* ------------------------------- */


/* 1 - 999 - Total posiciones activas */
/*CREATE ttRegistros.
ttRegistros.tipo = 1.
ttRegistros.codRenglon = "999".
ttRegistros.nombreRenglon = "TOTAL POSICIONES ACTIVAS".

FOR EACH registros WHERE registros.tipo = 1 NO-LOCK:
    ttRegistros.saldoActual = ttRegistros.saldoActual + registros.saldoActual.
    ttRegistros.banda[1] = ttRegistros.banda[1] + registros.banda[1].
    ttRegistros.banda[2] = ttRegistros.banda[2] + registros.banda[2].
    ttRegistros.banda[3] = ttRegistros.banda[3] + registros.banda[3].
    ttRegistros.banda[4] = ttRegistros.banda[4] + registros.banda[4].
    ttRegistros.banda[5] = ttRegistros.banda[5] + registros.banda[5].
    ttRegistros.banda[6] = ttRegistros.banda[6] + registros.banda[6].
    ttRegistros.banda[7] = ttRegistros.banda[7] + registros.banda[7].
END.
/* ------------------------------- */



/* 2 - 005 - Depósitos de Ahorro */
CREATE registros.
registros.tipo = 2.
registros.codRenglon = "005".
registros.nombreRenglon = "DEPOSITOS DE AHORRO".

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,4) = "2105"
                      AND sal_cuenta.ano = YEAR(fec_corte) NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

    IF AVAILABLE cuentas THEN DO:
        registros.saldoActual = registros.saldoActual + ROUND(sal_cuenta.sal_inicial,0).

        DO cont = 1 TO MONTH(fec_corte):
            IF cuentas.naturaleza = "DB" THEN
                registros.saldoActual = registros.saldoActual + round(sal_cuenta.db[cont],0) - round(sal_cuenta.cr[cont],0).
            ELSE
                registros.saldoActual = registros.saldoActual - round(sal_cuenta.db[cont],0) + round(sal_cuenta.cr[cont],0).
        END.
    END.
END.

RUN saldoPromedioDiaYearDepositosDeAhorro.

registros.banda[7] = sdoPromedioDiaYear.

MESSAGE "Saldo_actual" registros.saldoActual
        "sdoPromedioDiaYear" sdoPromedioDiaYear
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*RETURN.*/

IF registros.saldoActual > sdoPromedioDiaYear THEN DO:
    diferencia = registros.saldoActual - sdoPromedioDiaYear.

    IF diferencia > 0 THEN
        RUN AhorroAlaVista(INPUT 12, INPUT 1, INPUT 1).

    IF diferencia > 0 THEN
        RUN AhorroAlaVista(INPUT 11, INPUT 1, INPUT 2).

    IF diferencia > 0 THEN
        RUN AhorroAlaVista(INPUT 10, INPUT 1, INPUT 3).

    IF diferencia > 0 THEN
        RUN AhorroAlaVista(INPUT 9, INPUT 3, INPUT 4).

    IF diferencia > 0 THEN
        RUN AhorroAlaVista(INPUT 6, INPUT 3, INPUT 5).

    IF diferencia > 0 THEN
        RUN AhorroAlaVista(INPUT 3, INPUT 3, INPUT 6).
END.
ELSE
    registros.banda[7] = registros.saldoActual.

/* ----------------------- */
*/

OUTPUT TO d:\Leonardo\RiesgoLiquidez.csv.
FOR EACH registros NO-LOCK:
    EXPORT DELIMITER ";" registros.
END.
OUTPUT CLOSE.

time2 = TIME.

MESSAGE "Finalizó" STRING(time2 - time1,"HH:MM:SS")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*
PROCEDURE carteraCreditos:
    DEFINE INPUT PARAMETER vBanda AS INTEGER.
    DEFINE INPUT PARAMETER meses1 AS INTEGER.
    DEFINE INPUT PARAMETER meses2 AS INTEGER.

    DEFINE VAR fecha1 AS DATE.
    DEFINE VAR fecha2 AS DATE.

    fecha1 = ADD-INTERVAL(fec_corte, meses1, "months").
    fecha2 = ADD-INTERVAL(fec_corte, meses2, "months").

    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = rep_creditos.nit
                             AND CONTROL_pagos.num_credito = rep_creditos.num_credito
                             AND CONTROL_pagos.fec_vcto > fecha1
                             AND CONTROL_pagos.fec_vcto <= fecha2 NO-LOCK:
        registros.banda[vBanda] = registros.banda[vBanda] + ROUND(CONTROL_pagos.cuota,0).
    END.                                          
END.


PROCEDURE contingentesDeudoras:
    DEFINE INPUT PARAMETER vMeses AS INTEGER.
    DEFINE INPUT PARAMETER vPeriodos AS INTEGER.
    DEFINE INPUT PARAMETER vBanda AS INTEGER.

    DEFINE VAR fec1 AS DATE.
    DEFINE VAR fec2 AS DATE.
    DEFINE VAR cont AS INTEGER.

    fec1 = ADD-INTERVAL(fec_corte,vMeses * -1,"months") + 1.
    fec2 = ADD-INTERVAL(fec1,vPeriodos,"months") - 1.
    saldo = 0.

    IF YEAR(fec1) = YEAR(fec2) THEN DO:
        FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,4) = "8120"
                              AND sal_cuenta.ano = YEAR(fec1) NO-LOCK:
            DO cont = MONTH(fec1) TO MONTH(fec2):
                saldo = saldo + round(sal_cuenta.cr[cont],0).
            END.
        END.
    END.
    ELSE DO:
        FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,4) = "8120"
                              AND sal_cuenta.ano = YEAR(fec1) NO-LOCK:
            DO cont = MONTH(fec1) TO 12:
                saldo = saldo + round(sal_cuenta.cr[cont],0).
            END.
        END.

        FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,4) = "8120"
                              AND sal_cuenta.ano = YEAR(fec2) NO-LOCK:
            DO cont = 1 TO MONTH(fec2):
                saldo = saldo + round(sal_cuenta.cr[cont],0).
            END.
        END.
    END.

    IF saldo <= vSaldoActual THEN DO:
        registros.banda[vBanda] = saldo.
        vSaldoActual = vSaldoActual - saldo.
    END.
    ELSE DO:
        registros.banda[vBanda] = vSaldoActual.
        vSaldoActual = 0.
    END.

END PROCEDURE.


PROCEDURE ahorroAlaVista:
    DEFINE INPUT PARAMETER vMeses AS INTEGER.
    DEFINE INPUT PARAMETER vPeriodos AS INTEGER.
    DEFINE INPUT PARAMETER vBanda AS INTEGER.

    DEFINE VAR fec1 AS DATE.
    DEFINE VAR fec2 AS DATE.
    DEFINE VAR saldoPeriodo AS DECIMAL.
    DEFINE VAR saldoPromedio AS DECIMAL.
    DEFINE VAR saldoPromedioPeriodo AS DECIMAL.
    
    fec1 = ADD-INTERVAL(fec_corte,vMeses * -1,"months").
    fec2 = ADD-INTERVAL(fec1,vPeriodos,"months").

    FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,4) = "2105"
                      AND sal_cuenta.ano = YEAR(fec2) NO-LOCK BREAK BY sal_cuenta.cuenta:
        IF FIRST-OF(sal_cuenta.cuenta) THEN
            FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

        IF AVAILABLE cuentas THEN DO:
            saldoPeriodo = saldoPeriodo + sal_cuenta.sal_inicial.

            DO cont = 1 TO MONTH(fec2):
                IF cuentas.naturaleza = "DB" THEN
                    saldoPeriodo = saldoPeriodo + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                ELSE
                    saldoPeriodo = saldoPeriodo - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
            END.
        END.
    END.
    
    FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,4) = "2105"
                          AND sal_cuenta.ano = YEAR(fec1) NO-LOCK BREAK BY sal_cuenta.cuenta:
        IF FIRST-OF(sal_cuenta.cuenta) THEN
            FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

        IF AVAILABLE cuentas THEN DO:
            saldoPromedio = saldoPromedio + sal_cuenta.sal_inicial.

            DO cont = 1 TO MONTH(fec1):
                IF cuentas.naturaleza = "DB" THEN
                    saldoPromedio = saldoPromedio + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                ELSE
                    saldoPromedio = saldoPromedio - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
            END.
        END.
    END.

    fec1 = fec1 + 1.

    MESSAGE fec1 fec2
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    FOR EACH mov_contable WHERE SUBSTRING(mov_contable.cuenta,1,4) = "2105"
                            AND mov_contable.fec_contable >= fec1
                            AND mov_contable.fec_contable <= fec2 NO-LOCK BREAK BY mov_contable.fec_contable
                                                                                BY mov_contable.cuenta:
        IF FIRST-OF(mov_contable.cuenta) THEN
            FIND FIRST cuentas WHERE cuentas.cuenta = mov_contable.cuenta NO-LOCK NO-ERROR.

        IF AVAILABLE cuentas THEN DO:
            IF cuentas.naturaleza = "DB" THEN
                saldoPromedio = saldoPromedio + round(mov_contable.db,0) - round(mov_contable.cr,0).
            ELSE
                saldoPromedio = saldoPromedio - round(mov_contable.db,0) + round(mov_contable.cr,0).
        END.

        IF LAST-OF(mov_contable.fec_contable) THEN
            saldoPromedioPeriodo = saldoPromedioPeriodo + saldoPromedio.
    END.

    saldoPromedioPeriodo = round(saldoPromedioPeriodo / (vPeriodos * 30),0).

    IF saldoPeriodo > saldoPromedioPeriodo THEN DO:
        IF (saldoPeriodo - saldoPromedioPeriodo) >= diferencia THEN DO:
            registros.banda[vBanda] = saldoPeriodo - saldoPromedioPeriodo.
            diferencia = diferencia - registros.banda[vBanda].
        END.
        ELSE DO:
            registros.banda[vBanda] = diferencia.
            diferencia = 0.
        END.
    END.

END PROCEDURE.


PROCEDURE saldoPromedioDiaYearDepositosDeAhorro:
    DEFINE VAR fec1 AS DATE.
    DEFINE VAR fec2 AS DATE.
    DEFINE VAR sdoInicial AS DECIMAL.
    DEFINE VAR cont AS INTEGER.
    
    OUTPUT TO d:\Leonardo\saldos_.csv.
    fec1 = ADD-INTERVAL(fec_corte,-1,"years").

    FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,4) = "2105"
                          AND sal_cuenta.ano = YEAR(fec1) NO-LOCK:
        sdoInicial = sdoInicial + ROUND(sal_cuenta.sal_inicial,0).

        DO cont = 1 TO MONTH(fec1):
            sdoInicial = sdoInicial - round(sal_cuenta.db[cont],0) + round(sal_cuenta.cr[cont],0).
        END.
    END.

    EXPORT DELIMITER ";" fec1 sdoInicial.

    fec1 = fec1 + 1.
    fec2 = fec_corte.

    MESSAGE sdoInicial SKIP
            fec1 SKIP
            fec2 SKIP
            sdoPromedioDiaYear
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    FOR EACH mov_contable WHERE SUBSTRING(mov_contable.cuenta,1,4) = "2105"
                            AND mov_contable.fec_contable >= fec1
                            AND mov_contable.fec_contable <= fec2 NO-LOCK BREAK BY mov_contable.fec_contable:
        sdoInicial = sdoInicial - round(mov_contable.db,0) + round(mov_contable.cr,0).
        
        IF LAST-OF(mov_contable.fec_contable) THEN DO:
            sdoPromedioDiaYear = sdoPromedioDiaYear + sdoInicial.
            EXPORT DELIMITER ";" mov_contable.fec_contable sdoInicial sdoPromedioDiaYear.
        END.
    END.
    
    sdoPromedioDiaYear = round(sdoPromedioDiaYear / 365,0).

    OUTPUT CLOSE.

END PROCEDURE.

*/
*/

PROCEDURE maduraDisponible:
    DEFINE INPUT PARAMETER vMeses AS INTEGER.
    DEFINE INPUT PARAMETER vPeriodos AS INTEGER.
    DEFINE INPUT PARAMETER vBanda AS INTEGER.

    DEFINE VAR fec1 AS DATE.
    DEFINE VAR fec2 AS DATE.
    DEFINE VAR saldoCortePeriodo AS DECIMAL.
    DEFINE VAR pContFec AS DATE.
    DEFINE VAR pSaldoDia AS DECIMAL.
    DEFINE VAR pSumaSaldos AS DECIMAL.
    DEFINE VAR saldoPromedioPeriodo AS DECIMAL.
    DEFINE VAR pCont AS INTEGER.

    fec1 = ADD-INTERVAL(fec_corte + 1,vMeses * -1,"months").
    fec2 = ADD-INTERVAL(fec1,vPeriodos,"months") - 1.

    /* Calculamos el saldo al corte del periodo */
    FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,2) = "11"
                          AND SUBSTRING(sal_cuenta.cuenta,1,4) <> "1120"
                          AND sal_cuenta.ano = YEAR(fec2) NO-LOCK BREAK BY sal_cuenta.cuenta:
        IF FIRST-OF(sal_cuenta.cuenta) THEN
            FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

        IF AVAILABLE cuentas THEN DO:
            saldoCortePeriodo = saldoCortePeriodo + sal_cuenta.sal_inicial.

            DO cont = 1 TO MONTH(fec2):
                IF cuentas.naturaleza = "DB" THEN
                    saldoCortePeriodo = saldoCortePeriodo + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                ELSE
                    saldoCortePeriodo = saldoCortePeriodo - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
            END.
        END.
    END.

    /* Calculo el saldo para arrancar el periodo */
    FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,2) = "11"
                          AND SUBSTRING(sal_cuenta.cuenta,1,4) <> "1120"
                          AND sal_cuenta.ano = YEAR(fec1 - 1) NO-LOCK BREAK BY sal_cuenta.cuenta:
        IF FIRST-OF(sal_cuenta.cuenta) THEN
            FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

        IF AVAILABLE cuentas THEN DO:
            pSaldoDia = pSaldoDia + sal_cuenta.sal_inicial.

            DO cont = 1 TO MONTH(fec1 - 1):
                IF cuentas.naturaleza = "DB" THEN
                    pSaldoDia = pSaldoDia + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                ELSE
                    pSaldoDia = pSaldoDia - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
            END.
        END.
    END.

    /* Calculo el saldo diario y lo voy acumulando para al final dividir entre el número de días del periodo */
    DO pContFec = fec1 TO fec2:
        FOR EACH agencias NO-LOCK:
            FOR EACH cuentas WHERE SUBSTRING(cuentas.cuenta,1,2) = "11" AND SUBSTRING(cuentas.cuenta,1,4) <> "1120" NO-LOCK:
                FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                                        AND mov_contable.cuenta = cuentas.cuenta
                                        AND mov_contable.fec_contable = pContFec NO-LOCK:
                    IF cuentas.naturaleza = "DB" THEN
                        pSaldoDia = pSaldoDia + mov_contable.db - mov_contable.cr.
                    ELSE
                        pSaldoDia = pSaldoDia - mov_contable.db + mov_contable.cr.
                END.
            END.
        END.

        pSumaSaldos = pSumaSaldos + pSaldoDia.
        pCont = pCont + 1.
    END.

    saldoPromedioPeriodo = ROUND(pSumaSaldos / pCont, 0).

    IF saldoCortePeriodo > saldoPromedioPeriodo THEN DO:
        IF diferencia > saldoCortePeriodo - saldoPromedioPeriodo THEN DO:
            registros.banda[vBanda] = saldoCortePeriodo - saldoPromedioPeriodo.
            diferencia = diferencia - registros.banda[vBanda].
        END.
        ELSE DO:
            registros.banda[vBanda] = diferencia.
            diferencia = 0.
        END.
    END.

END PROCEDURE.


PROCEDURE procesoFondoDeLiquidez:
    DEFINE INPUT PARAMETER vMeses AS INTEGER.
    DEFINE INPUT PARAMETER vBanda AS INTEGER.

    DEFINE VAR fec1 AS DATE.

    fec1 = ADD-INTERVAL(fec_corte + 1,vMeses * -1,"months") - 1.

    MESSAGE fec1
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    FOR EACH rep_ahorros WHERE rep_ahorros.fecCorte = fec1
                           AND (rep_ahorros.tip_ahorro = 1 OR rep_ahorros.tip_ahorro = 2 OR rep_ahorros.tip_ahorro = 3) NO-LOCK:
        IF rep_ahorros.tip_ahorro = 2 THEN
            ahorrosCortes[vBanda] = ahorrosCortes[vBanda] + ROUND((rep_ahorros.sdo_disponible + rep_ahorros.sdo_canje) * 0.02,0).
        ELSE
            ahorrosCortes[vBanda] = ahorrosCortes[vBanda] + ROUND((rep_ahorros.sdo_disponible + rep_ahorros.sdo_canje) * 0.1,0).
    END.

    FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "1203" OR
                               SUBSTRING(sal_cuenta.cuenta,1,4) = "1120")
                          AND sal_cuenta.ano = YEAR(fec1) NO-LOCK BREAK BY sal_cuenta.cuenta:
        IF FIRST-OF(sal_cuenta.cuenta) THEN
            FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

        IF AVAILABLE cuentas THEN DO:
            fondoLiquidezCortes[vBanda] = fondoLiquidezCortes[vBanda] + round(sal_cuenta.sal_inicial,0).

            DO cont = 1 TO MONTH(fec1):
                IF cuentas.naturaleza = "DB" THEN
                    fondoLiquidezCortes[vBanda] = fondoLiquidezCortes[vBanda] + round(sal_cuenta.db[cont] - sal_cuenta.cr[cont],0).
                ELSE
                    fondoLiquidezCortes[vBanda] = fondoLiquidezCortes[vBanda] - round(sal_cuenta.db[cont] + sal_cuenta.cr[cont],0).
            END.
        END.
    END.

END PROCEDURE.
