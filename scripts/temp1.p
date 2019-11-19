DEFINE VAR cont AS INTEGER.
DEFINE VAR saldo AS DECIMAL.
DEFINE BUFFER bfrAnexos FOR anexos13.

DEFINE VAR ano1 AS INTEGER INITIAL 2016.
DEFINE VAR ano2 AS INTEGER INITIAL 2017.

FOR EACH anexos WHERE anexos.ano = ano2
                  AND substring(anexos.cuenta,1,6) = "249510":
    anexos.sdo_inicial = 0.
END.

FOR EACH anexos13 WHERE anexos13.ano = ano2
                    AND substring(anexos13.cuenta,1,6) = "249510":
    anexos13.sdo_inicial = 0.
END.
    
FOR EACH anexos13 WHERE anexos13.ano = ano1
                    AND substring(anexos13.cuenta,1,6) = "249510"
                    /*AND sdo_inicial <> 0*/ NO-LOCK BREAK BY anexos13.nit
                                                       BY anexos13.cuenta
                                                       BY anexos13.cen_costos:
    IF FIRST-OF(anexos13.cen_costos) THEN
        saldo = 0.    
    
    saldo = saldo + anexos13.sdo_inicial.
    
    DO cont = 1 TO 12:
        saldo = saldo + anexos13.cr[cont] - anexos13.db[cont].
    END.

    IF LAST-OF(anexos13.cen_costos) THEN DO:
        FIND FIRST anexos WHERE anexos.nit = anexos13.nit
                            AND anexos.cuenta = anexos13.cuenta
                            AND anexos.cen_costos = anexos13.cen_costos
                            AND anexos.ano = ano2 NO-ERROR.
        IF AVAILABLE anexos THEN
            anexos.sdo_inicial = saldo.

        FIND FIRST bfrAnexos WHERE bfrAnexos.nit = anexos13.nit
                               AND bfrAnexos.cuenta = anexos13.cuenta
                               AND bfrAnexos.cen_costos = anexos13.cen_costos
                               AND bfrAnexos.ano = ano2 NO-ERROR.
        IF AVAILABLE bfrAnexos THEN
            bfrAnexos.sdo_inicial = saldo.
    END.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
