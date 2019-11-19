DISABLE TRIGGERS FOR LOAD OF cuentas.

DEFINE TEMP-TABLE ttcuentas LIKE cuentas.
DEFINE TEMP-TABLE tempCuentas LIKE cuentas.

DEFINE VAR vNombre AS CHARACTER.

/* Paso a la temporal los registros cambiados*/
FOR EACH cuentas WHERE cuentas.cuentaNIIF <> "" AND cuentas.tipo = 2 NO-LOCK:
    FIND FIRST ttcuentas WHERE ttCuentas.cuenta = cuentas.cuentaNIIF NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttcuentas THEN DO:
        EMPTY TEMP-TABLE tempCuentas.
        CREATE tempCuentas.
        BUFFER-COPY cuentas TO tempCuentas.
        tempCuentas.cuenta = cuentas.cuentaNIIF.

        CREATE ttcuentas.
        BUFFER-COPY tempCuentas TO ttcuentas.
    END.
END.

/* Borro la tabla cuentas */
FOR EACH cuentas:
    DELETE cuentas.
END.

/* Creo cuerntas a partir de cuentas_niif */
FOR EACH cuentas_niif NO-LOCK:
    CREATE cuentas.
    BUFFER-COPY cuentas_niif TO cuentas.
END.

/* Actualizo la tabla cuentas con los registros homologados de la temporal */
FOR EACH ttcuentas NO-LOCK:
    FIND FIRST cuentas WHERE cuentas.cuenta = ttcuentas.cuenta NO-ERROR.
    IF AVAILABLE cuentas THEN DO:
        vNombre = cuentas.nombre.
        BUFFER-COPY ttcuentas TO cuentas.
        cuentas.nombre = vNombre.
    END.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
