DISABLE TRIGGERS FOR LOAD OF sal_cuenta.
DISABLE TRIGGERS FOR LOAD OF sal_cuenta_niif.
DISABLE TRIGGERS FOR LOAD OF anexos.
DISABLE TRIGGERS FOR LOAD OF anexos_niif.
    
FOR EACH sal_cuenta_niif WHERE ano = 2015:
    DELETE sal_cuenta_niif.
END.

FOR EACH sal_cuenta WHERE sal_cuenta.ano = 2015 NO-LOCK:
    FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentas THEN
        MESSAGE sal_cuenta.cuenta
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE DO:
        IF cuentas.cuentaNIIF <> "" THEN DO:
            FIND FIRST sal_cuenta_niif WHERE sal_cuenta_niif.cuenta = cuentas.cuentaNIIF
                                         AND sal_cuenta_niif.agencia = sal_cuenta.agencia
                                         AND sal_cuenta_niif.cen_costos = sal_cuenta.cen_costos
                                         AND sal_cuenta_niif.ano = sal_cuenta.ano NO-ERROR.
            IF NOT AVAILABLE sal_cuenta_niif THEN DO:
                CREATE sal_cuenta_niif.
                BUFFER-COPY sal_cuenta TO sal_cuenta_niif.
                sal_cuenta_niif.cuenta = cuentas.cuentaNIIF.
            END.
            ELSE
                sal_cuenta_niif.sal_inicial = sal_cuenta_niif.sal_inicial + sal_cuenta.sal_inicial.
        END.
    END.
END.

MESSAGE "Sal_Cuenta OK!"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*
FOR EACH anexos_niif WHERE ano = 2015:
    DELETE anexos_niif.
END.

FOR EACH anexos WHERE anexos.ano = 2015 NO-LOCK:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentas THEN
        MESSAGE sal_cuenta.cuenta
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE DO:
        IF cuentas.cuentaNIIF <> "" THEN DO:
            FIND FIRST anexos_niif WHERE anexos_niif.cuenta = cuentas.cuentaNIIF
                                     AND anexos_niif.agencia = anexos.agencia
                                     AND anexos_niif.cen_costos = anexos.cen_costos
                                     AND anexos_niif.ano = anexos.ano
                                     AND anexos_niif.nit = anexos.nit NO-ERROR.
            IF NOT AVAILABLE anexos_niif THEN DO:
                CREATE anexos_niif.
                BUFFER-COPY anexos TO anexos_niif.
                anexos_niif.cuenta = cuentas.cuentaNIIF.
            END.
            ELSE
                anexos_niif.sdo_inicial = anexos_niif.sdo_inicial + anexos.sdo_inicial.
        END.
    END.
END.

MESSAGE "Anexos OK!"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
