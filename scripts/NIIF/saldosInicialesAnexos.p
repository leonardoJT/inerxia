DISABLE TRIGGERS FOR LOAD OF anexos_NIIF.

DEFINE TEMP-TABLE ttanexos
    FIELD agencia AS INTEGER
    FIELD cen_costos AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD saldo AS DECIMAL
    INDEX idx agencia cen_costos cuenta nit.
    
FOR EACH anexos WHERE anexos.ano = 2016 AND anexos.sdo_inicial <> 0 NO-LOCK:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN DO:
        IF cuentas.cuentaNIIF <> "" THEN DO:
            FIND FIRST ttanexos WHERE ttanexos.agencia = anexos.agencia
                                  AND ttanexos.cen_costos = anexos.cen_costos
                                  AND ttanexos.cuenta = cuentas.cuentaNIIF
                                  AND ttanexos.nit = anexos.nit NO-ERROR.
            IF NOT AVAILABLE ttanexos THEN DO:
                CREATE ttanexos.
                ttanexos.agencia = anexos.agencia.
                ttanexos.cen_costos = anexos.cen_costos.
                ttanexos.cuenta = cuentas.cuentaNIIF.
                ttanexos.nit = anexos.nit.
            END.

            ttAnexos.saldo = ttAnexos.saldo + anexos.sdo_inicial.
        END.
    END.
END.

MESSAGE "Fin Carga ttanexos"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


FOR EACH anexos_NIIF WHERE anexos_NIIF.ano = 2016 AND anexos_NIIF.sdo_inicial <> 0:
    anexos_NIIF.sdo_inicial = 0.
END.

MESSAGE "Fin borrado saldo inicial anexos_NIIF"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


FOR EACH ttanexos NO-LOCK:
    FIND FIRST anexos_NIIF WHERE anexos_NIIF.agencia = ttanexos.agencia
                             AND anexos_NIIF.cen_costos = ttanexos.cen_costos
                             AND anexos_NIIF.cuenta = ttanexos.cuenta
                             AND anexos_NIIF.nit = ttanexos.nit
                             AND anexos_NIIF.ano = 2016 NO-ERROR.
    IF NOT AVAILABLE anexos_NIIF THEN DO:
        CREATE anexos_NIIF.
        anexos_NIIF.agencia = ttanexos.agencia.
        anexos_NIIF.Ano = 2016.
        anexos_NIIF.Cen_Costos = ttanexos.cen_costos.
        anexos_NIIF.Nit = ttanexos.nit.
    END.

    anexos_NIIF.sdo_inicial = anexos_NIIF.sdo_inicial + ttanexos.saldo.
END.

MESSAGE "Fin saldos iniciales NIIF"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
