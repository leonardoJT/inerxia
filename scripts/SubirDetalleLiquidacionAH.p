DISABLE TRIGGERS FOR LOAD OF anexos.

DEFINE VAR suma AS DECIMAL.
DEFINE VAR vCuenta AS CHARACTER.
DEFINE VAR vAgencia AS INTEGER.
DEFINE VAR vProducto AS INTEGER.
DEFINE VAR vMes AS INTEGER.

DEFINE TEMP-TABLE archivo
    FIELD agencia AS INTEGER
    FIELD producto AS INTEGER
    FIELD cedula AS CHARACTER
    FIELD interes AS DECIMAL
    FIELD causado AS INTEGER.

/* ----------------------- */

vMes = 9.

INPUT FROM c:\INFO_Fodun\Leonardo\Liquidacion\Ahorros_Septiembre.csv.
REPEAT :
    CREATE archivo.
    IMPORT DELIMITER ";" archivo NO-ERROR.

    IF ERROR-STATUS:ERROR OR (archivo.interes = 0 AND archivo.causado = 0) THEN
        DELETE archivo.
END.

/* ----------------------- */

vProducto = 3.
vCuenta = "61752001".

FOR EACH agencias NO-LOCK:
    vAgencia = agencias.agencia.

    FOR EACH archivo WHERE archivo.agencia = vAgencia
                       AND archivo.producto = vProducto NO-LOCK:
        FIND FIRST anexos WHERE anexos.nit = archivo.cedula
                            AND anexos.agencia = vAgencia
                            AND anexos.cuenta = vCuenta
                            AND anexos.ano = 2012
                            AND anexos.cen_costos = 999 NO-ERROR.
        IF NOT AVAILABLE anexos THEN DO:
            CREATE anexos.
            ASSIGN anexos.nit = archivo.cedula
                   anexos.agencia = vAgencia
                   anexos.cuenta = vCuenta
                   anexos.ano = 2012
                   anexos.cen_costos = 999.
        END.

        anexos.cr[vMes] = anexos.cr[vMes] + archivo.interes + archivo.causado.

        suma = suma + archivo.interes + archivo.causado.
    END.

    suma = 0.
END.

vProducto = 4.
vCuenta = "61750503".

FOR EACH agencias NO-LOCK:
    vAgencia = agencias.agencia.

    FOR EACH archivo WHERE archivo.agencia = vAgencia
                       AND archivo.producto = vProducto NO-LOCK:
        FIND FIRST anexos WHERE anexos.nit = archivo.cedula
                            AND anexos.agencia = vAgencia
                            AND anexos.cuenta = vCuenta
                            AND anexos.ano = 2012
                            AND anexos.cen_costos = 999 NO-ERROR.
        IF NOT AVAILABLE anexos THEN DO:
            CREATE anexos.
            ASSIGN anexos.nit = archivo.cedula
                   anexos.agencia = vAgencia
                   anexos.cuenta = vCuenta
                   anexos.ano = 2012
                   anexos.cen_costos = 999.
        END.

        anexos.cr[vMes] = anexos.cr[vMes] + archivo.interes + archivo.causado.

        suma = suma + archivo.interes + archivo.causado.
    END.

    suma = 0.
END.

vProducto = 5.
vCuenta = "61751002".

FOR EACH agencias NO-LOCK:
    vAgencia = agencias.agencia.

    FOR EACH archivo WHERE archivo.agencia = vAgencia
                       AND archivo.producto = vProducto NO-LOCK:
        FIND FIRST anexos WHERE anexos.nit = archivo.cedula
                            AND anexos.agencia = vAgencia
                            AND anexos.cuenta = vCuenta
                            AND anexos.ano = 2012
                            AND anexos.cen_costos = 999 NO-ERROR.
        IF NOT AVAILABLE anexos THEN DO:
            CREATE anexos.
            ASSIGN anexos.nit = archivo.cedula
                   anexos.agencia = vAgencia
                   anexos.cuenta = vCuenta
                   anexos.ano = 2012
                   anexos.cen_costos = 999.
        END.

        anexos.cr[vMes] = anexos.cr[vMes] + archivo.interes + archivo.causado.

        suma = suma + archivo.interes + archivo.causado.
    END.

    suma = 0.
END.

vProducto = 6.
vCuenta = "61751001".

FOR EACH agencias NO-LOCK:
    vAgencia = agencias.agencia.

    FOR EACH archivo WHERE archivo.agencia = vAgencia
                       AND archivo.producto = vProducto NO-LOCK:
        FIND FIRST anexos WHERE anexos.nit = archivo.cedula
                            AND anexos.agencia = vAgencia
                            AND anexos.cuenta = vCuenta
                            AND anexos.ano = 2012
                            AND anexos.cen_costos = 999 NO-ERROR.
        IF NOT AVAILABLE anexos THEN DO:
            CREATE anexos.
            ASSIGN anexos.nit = archivo.cedula
                   anexos.agencia = vAgencia
                   anexos.cuenta = vCuenta
                   anexos.ano = 2012
                   anexos.cen_costos = 999.
        END.

        anexos.cr[vMes] = anexos.cr[vMes] + archivo.interes + archivo.causado.

        suma = suma + archivo.interes + archivo.causado.
    END.

    suma = 0.
END.

vProducto = 7.
vCuenta = "61751501".

FOR EACH agencias NO-LOCK:
    vAgencia = agencias.agencia.

    FOR EACH archivo WHERE archivo.agencia = vAgencia
                       AND archivo.producto = vProducto NO-LOCK:
        FIND FIRST anexos WHERE anexos.nit = archivo.cedula
                            AND anexos.agencia = vAgencia
                            AND anexos.cuenta = vCuenta
                            AND anexos.ano = 2012
                            AND anexos.cen_costos = 999 NO-ERROR.
        IF NOT AVAILABLE anexos THEN DO:
            CREATE anexos.
            ASSIGN anexos.nit = archivo.cedula
                   anexos.agencia = vAgencia
                   anexos.cuenta = vCuenta
                   anexos.ano = 2012
                   anexos.cen_costos = 999.
        END.

        anexos.cr[vMes] = anexos.cr[vMes] + archivo.interes + archivo.causado.

        suma = suma + archivo.interes + archivo.causado.
    END.

    suma = 0.
END.

vProducto = 8.
vCuenta = "61750505".

FOR EACH agencias NO-LOCK:
    vAgencia = agencias.agencia.

    FOR EACH archivo WHERE archivo.agencia = vAgencia
                       AND archivo.producto = vProducto NO-LOCK:
        FIND FIRST anexos WHERE anexos.nit = archivo.cedula
                            AND anexos.agencia = vAgencia
                            AND anexos.cuenta = vCuenta
                            AND anexos.ano = 2012
                            AND anexos.cen_costos = 999 NO-ERROR.
        IF NOT AVAILABLE anexos THEN DO:
            CREATE anexos.
            ASSIGN anexos.nit = archivo.cedula
                   anexos.agencia = vAgencia
                   anexos.cuenta = vCuenta
                   anexos.ano = 2012
                   anexos.cen_costos = 999.
        END.

        anexos.cr[vMes] = anexos.cr[vMes] + archivo.interes + archivo.causado.

        suma = suma + archivo.interes + archivo.causado.
    END.

    suma = 0.
END.

vProducto = 9.
vCuenta = "61750505".

FOR EACH agencias NO-LOCK:
    vAgencia = agencias.agencia.

    FOR EACH archivo WHERE archivo.agencia = vAgencia
                       AND archivo.producto = vProducto NO-LOCK:
        FIND FIRST anexos WHERE anexos.nit = archivo.cedula
                            AND anexos.agencia = vAgencia
                            AND anexos.cuenta = vCuenta
                            AND anexos.ano = 2012
                            AND anexos.cen_costos = 999 NO-ERROR.
        IF NOT AVAILABLE anexos THEN DO:
            CREATE anexos.
            ASSIGN anexos.nit = archivo.cedula
                   anexos.agencia = vAgencia
                   anexos.cuenta = vCuenta
                   anexos.ano = 2012
                   anexos.cen_costos = 999.
        END.

        anexos.cr[vMes] = anexos.cr[vMes] + archivo.interes + archivo.causado.

        suma = suma + archivo.interes + archivo.causado.
    END.

    suma = 0.
END.

MESSAGE "Terminó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
