DISABLE TRIGGERS FOR LOAD OF sal_cuenta.
DISABLE TRIGGERS FOR LOAD OF sal_cuenta_niif.
DISABLE TRIGGERS FOR LOAD OF anexos.
DISABLE TRIGGERS FOR LOAD OF anexos_niif.

FOR EACH sal_cuenta_niif WHERE sal_cuenta_NIIF.ano = 2017:
    sal_cuenta_NIIF.sal_inicial = 0.
END.

FOR EACH sal_cuenta WHERE sal_cuenta.ano = 2017 NO-LOCK:
    FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN DO:
        IF cuentas.cuentaNIIF <> "" THEN DO:
            FIND FIRST sal_Cuenta_NIIF WHERE sal_Cuenta_NIIF.agencia = sal_cuenta.agencia
                                         AND sal_Cuenta_NIIF.cuenta = cuentas.cuentaNIIF
                                         AND sal_Cuenta_NIIF.cen_costos = sal_cuenta.cen_costos
                                         AND sal_Cuenta_NIIF.ano = 2017 NO-ERROR.
            IF NOT AVAILABLE sal_Cuenta_NIIF THEN DO:
                CREATE sal_Cuenta_NIIF.
                sal_Cuenta_NIIF.agencia = sal_cuenta.agencia.
                sal_Cuenta_NIIF.cuenta = cuentas.cuentaNIIF.
                sal_Cuenta_NIIF.cen_costos = sal_cuenta.cen_costos.
                sal_Cuenta_NIIF.ano = 2017.
            END.

            sal_Cuenta_NIIF.sal_inicial = sal_Cuenta_NIIF.sal_inicial + sal_cuenta.sal_inicial.
        END.
    END.
END.

MESSAGE "Sal_Cuenta OK!"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


FOR EACH anexos_niif WHERE anexos_niif.ano = 2017:
    anexos_NIIF.sdo_inicial = 0.
END.

FOR EACH anexos WHERE anexos.ano = 2017 AND SUBSTRING(anexos.cuenta,1,1) <> "4" AND SUBSTRING(anexos.cuenta,1,1) <> "5" AND SUBSTRING(anexos.cuenta,1,1) <> "6" NO-LOCK:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN DO:
        IF cuentas.cuentaNIIF <> "" THEN DO:
            FIND FIRST anexos_NIIF WHERE anexos_NIIF.agencia = anexos.agencia
                                     AND anexos_NIIF.cuenta = cuentas.cuentaNIIF
                                     AND anexos_NIIF.cen_costos = anexos.cen_costos
                                     AND anexos_NIIF.nit = anexos.nit
                                     AND anexos_NIIF.ano = 2017 NO-ERROR.
            IF NOT AVAILABLE anexos_NIIF THEN DO:
                CREATE anexos_NIIF.
                anexos_NIIF.agencia = anexos.agencia.
                anexos_NIIF.cuenta = cuentas.cuentaNIIF.
                anexos_NIIF.cen_costos = anexos.cen_costos.
                anexos_NIIF.nit = anexos.nit.
                anexos_NIIF.ano = 2017.
            END.

            anexos_NIIF.sdo_inicial = anexos_NIIF.sdo_inicial + anexos.sdo_inicial.
        END.
    END.
END.

MESSAGE "anexos OK!"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
