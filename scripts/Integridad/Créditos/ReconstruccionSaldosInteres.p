DEFINE TEMP-TABLE tabla
    FIELD nit AS CHARACTER
    FIELD sdo_ini AS DECIMAL
    FIELD avances AS DECIMAL
    FIELD pagos AS DECIMAL
    FIELD sdo_fin AS DECIMAL
    FIELD calculado AS DECIMAL
    FIELD diferencia AS DECIMAL.

DEFINE VAR vAgencia AS INTEGER INITIAL 1.
DEFINE VAR fecIni AS DATE INITIAL 07/31/2018.
DEFINE VAR fecFin AS DATE INITIAL 08/31/2018.

/* Saldos iniciales */
FOR EACH rep_creditos WHERE rep_creditos.agencia = vAgencia
                        AND rep_creditos.cod_credito <> 62
                        AND fecCorte = fecIni NO-LOCK:
    FIND FIRST tabla WHERE tabla.nit = rep_creditos.nit NO-ERROR.
    IF NOT AVAILABLE tabla THEN DO:
        CREATE tabla.
        tabla.nit = rep_creditos.nit.
    END.

    tabla.sdo_ini = tabla.sdo_ini + rep_creditos.INT_corriente + rep_creditos.INT_morCobrar.
END.

/* Movimientos */
FOR EACH mov_contable WHERE mov_contable.agencia = vAgencia
                        AND mov_contable.fec_contable > fecIni
                        AND mov_contable.fec_contable <= fecFin
                        AND SUBSTRING(mov_contable.cuenta,1,4) = "1443"
                        AND mov_contable.nit <> "" NO-LOCK:
    FIND FIRST tabla WHERE tabla.nit = mov_contable.nit NO-ERROR.
    IF NOT AVAILABLE tabla THEN DO:
        CREATE tabla.
        tabla.nit = mov_contable.nit.
    END.

    tabla.avances = tabla.avances + mov_contable.db.
    tabla.pagos = tabla.pagos + mov_contable.cr.
END.

/* Saldos finales */
FOR EACH rep_creditos WHERE rep_creditos.agencia = vAgencia
                        AND rep_creditos.cod_credito <> 62
                        AND fecCorte = fecFin NO-LOCK:
    FIND FIRST tabla WHERE tabla.nit = rep_creditos.nit NO-ERROR.
    IF NOT AVAILABLE tabla THEN DO:
        CREATE tabla.
        tabla.nit = rep_creditos.nit.
    END.

    tabla.sdo_fin = tabla.sdo_fin + rep_creditos.INT_corriente + rep_creditos.INT_morCobrar.
END.

/* Calculamos */
OUTPUT TO d:\Leonardo\res.csv.
FOR EACH tabla:
    tabla.calculado = tabla.sdo_ini + tabla.avances - tabla.pagos.
    tabla.diferencia = tabla.sdo_fin - tabla.calculado.

    IF tabla.diferencia <> 0 THEN
        EXPORT DELIMITER ";" tabla.
END.
OUTPUT CLOSE.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
