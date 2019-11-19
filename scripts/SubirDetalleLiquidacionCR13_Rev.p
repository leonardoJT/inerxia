DISABLE TRIGGERS FOR LOAD OF anexos13.

DEFINE VAR sumaN AS DECIMAL.
DEFINE VAR sumaC AS DECIMAL.
DEFINE VAR vCuentaN AS CHARACTER.
DEFINE VAR vCuentaC AS CHARACTER.
DEFINE VAR vCuenta AS CHARACTER.
DEFINE VAR vAgencia AS INTEGER.
DEFINE VAR vProducto AS INTEGER.
DEFINE VAR vMes AS INTEGER.
DEFINE VAR vValor AS DECIMAL.

DEFINE TEMP-TABLE archivo
    FIELD agencia AS INTEGER
    FIELD producto AS INTEGER
    FIELD num_credito AS INTEGER
    FIELD cedula AS CHARACTER
    FIELD corriente AS DECIMAL
    FIELD dificilCobro AS DECIMAL
    FIELD moraDificilCobro AS DECIMAL
    FIELD mora AS DECIMAL.

/* ----------------------- */
vMes = 9.
INPUT FROM D:\Leonardo\Liquidacion\Creditos_Mayo_29-30.csv.
REPEAT :
    CREATE archivo.
    IMPORT DELIMITER ";" archivo NO-ERROR.

    IF ERROR-STATUS:ERROR OR (archivo.corriente = 0 AND archivo.dificilCobro = 0 AND archivo.moraDificilCobro = 0 AND archivo.mora = 0) THEN
        DELETE archivo.
END.
/* ----------------------- */

vProducto = 17.
vCuentaN = "41851001".
vCuentaC = "41851021".

RUN pAnexosCorriente.

vCuentaN = "41851051".
vCuentaC = "41851081".

RUN pAnexosMora.
/* --------------- */

vProducto = 22.
vCuentaN = "41851002".
vCuentaC = "41851026".

RUN pAnexosCorriente.

vCuentaN = "41851052".
vCuentaC = "41851086".

RUN pAnexosMora.
/* --------------- */

vProducto = 27.
vCuentaN = "41851003".
vCuentaC = "41851027".

RUN pAnexosCorriente.

vCuentaN = "41851053".
vCuentaC = "41851087".

RUN pAnexosMora.
/* --------------- */

vProducto = 32.
vCuentaN = "41851005".
vCuentaC = "41851024".

RUN pAnexosCorriente.

vCuentaN = "41851054".
vCuentaC = "41851089".

RUN pAnexosMora.
/* --------------- */

vProducto = 57.
vCuentaN = "41851010".
vCuentaC = "41851028".

RUN pAnexosCorriente.

vCuentaN = "41851059".
vCuentaC = "41851088".

RUN pAnexosMora.
/* --------------- */

vProducto = 62.
vCuentaN = "41851004".
vCuentaC = "41851004".

RUN pAnexosCorriente.

vCuentaN = "41851066".
vCuentaC = "41851066".

RUN pAnexosMora.
/* --------------- */

vProducto = 103.
vCuentaN = "41851001".
vCuentaC = "41851001".

RUN pAnexosCorriente.

vCuentaN = "41851051".
vCuentaC = "41851051".

RUN pAnexosMora.
/* --------------- */

vProducto = 108.
vCuentaN = "41851022".
vCuentaC = "41851022".

RUN pAnexosCorriente.

vCuentaN = "41851082".
vCuentaC = "41851082".

RUN pAnexosMora.
/* --------------- */

vProducto = 113.
vCuentaN = "41851023".
vCuentaC = "41851023".

RUN pAnexosCorriente.

vCuentaN = "41851083".
vCuentaC = "41851083".

RUN pAnexosMora.
/* --------------- */

vProducto = 114.
vCuentaN = "41851030".
vCuentaC = "41851030".

RUN pAnexosCorriente.

vCuentaN = "41851090".
vCuentaC = "41851090".

RUN pAnexosMora.
/* --------------- */

vProducto = 118.
vCuentaN = "41851001".
vCuentaC = "41851001".

RUN pAnexosCorriente.

vCuentaN = "41851051".
vCuentaC = "41851051".

RUN pAnexosMora.
/* --------------- */

vProducto = 123.
vCuentaN = "41851025".
vCuentaC = "41851025".

RUN pAnexosCorriente.

vCuentaN = "41851085".
vCuentaC = "41851085".

RUN pAnexosMora.
/* --------------- */

vProducto = 128.
vCuentaN = "41851001".
vCuentaC = "41851001".

RUN pAnexosCorriente.

vCuentaN = "41851051".
vCuentaC = "41851051".

RUN pAnexosMora.
/* --------------- */

vProducto = 133.
vCuentaN = "41851001".
vCuentaC = "41851001".

RUN pAnexosCorriente.

vCuentaN = "41851051".
vCuentaC = "41851051".

RUN pAnexosMora.
/* --------------- */

vProducto = 158.
vCuentaN = "41851016".
vCuentaC = "41851016".

RUN pAnexosCorriente.

vCuentaN = "41851064".
vCuentaC = "41851064".

RUN pAnexosMora.
/* --------------- */

vProducto = 185.
vCuentaN = "41851001".
vCuentaC = "41851001".

RUN pAnexosCorriente.

vCuentaN = "41851051".
vCuentaC = "41851051".

RUN pAnexosMora.
/* --------------- */

MESSAGE "Terminó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


PROCEDURE pAnexosCorriente:
    FOR EACH agencias NO-LOCK BY agencia:
        vAgencia = agencias.agencia.
        sumaN = 0.
        sumaC = 0.

        FOR EACH archivo WHERE archivo.agencia = vAgencia
                           AND archivo.producto = vProducto NO-LOCK:
            vValor = archivo.corriente.
            vValor = archivo.corriente / 3.
            
            FIND FIRST creditos WHERE creditos.nit = archivo.cedula
                                  AND creditos.num_credito = archivo.num_credito NO-LOCK NO-ERROR.
            IF AVAILABLE creditos THEN DO:
                IF creditos.FOR_pago = 1 THEN DO:
                    vCuenta = vCuentaC.
                    sumaC = sumaC + vValor.
                END.

                IF creditos.FOR_pago = 2 THEN DO:
                    vCuenta = vCuentaN.
                    sumaN = sumaN + vValor.
                END.
            END.
            ELSE DO:
                MESSAGE archivo.cedula archivo.num_credito
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            LEAVE.
        END.

        FIND FIRST anexos13 WHERE anexos13.nit = archivo.cedula
                              AND anexos13.agencia = vAgencia
                              AND anexos13.cuenta = vCuenta
                              AND anexos13.ano = 2012
                              AND anexos13.cen_costos = 999 NO-ERROR.
            IF NOT AVAILABLE anexos13 THEN DO:
                CREATE anexos13.
                ASSIGN anexos13.nit = archivo.cedula
                       anexos13.agencia = vAgencia
                       anexos13.cuenta = vCuenta
                       anexos13.ano = 2012
                       anexos13.cen_costos = 999.
            END.
        
            anexos13.cr[vMes] = anexos13.cr[vMes] - vValor.
        END.
    END.
END PROCEDURE.

PROCEDURE pAnexosMora:
    FOR EACH agencias NO-LOCK BY agencia:
        vAgencia = agencias.agencia.
        sumaN = 0.
        sumaC = 0.

        FOR EACH archivo WHERE archivo.agencia = vAgencia
                           AND archivo.producto = vProducto NO-LOCK:
            vValor = archivo.mora.
            vValor = archivo.mora / 3.
            
            FIND FIRST creditos WHERE creditos.nit = archivo.cedula
                                  AND creditos.num_credito = archivo.num_credito NO-LOCK NO-ERROR.
            IF AVAILABLE creditos THEN DO:
                IF creditos.FOR_pago = 1 THEN DO:
                    vCuenta = vCuentaC.
                    sumaC = sumaC + vValor.
                END.

                IF creditos.FOR_pago = 2 THEN DO:
                    vCuenta = vCuentaN.
                    sumaN = sumaN + vValor.
                END.
            END.
            ELSE DO:
                MESSAGE archivo.cedula archivo.num_credito
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            LEAVE.
        END.

        FIND FIRST anexos13 WHERE anexos13.nit = archivo.cedula
                              AND anexos13.agencia = vAgencia
                              AND anexos13.cuenta = vCuenta
                              AND anexos13.ano = 2012
                              AND anexos13.cen_costos = 999 NO-ERROR.
            IF NOT AVAILABLE anexos13 THEN DO:
                CREATE anexos13.
                ASSIGN anexos13.nit = archivo.cedula
                       anexos13.agencia = vAgencia
                       anexos13.cuenta = vCuenta
                       anexos13.ano = 2012
                       anexos13.cen_costos = 999.
            END.
        
            anexos13.cr[vMes] = anexos13.cr[vMes] - vValor.
        END.
    END.
END PROCEDURE.

