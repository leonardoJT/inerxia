DEFINE VAR sdo AS DECIMAL.
DEFINE VAR cont AS INTEGER.

OUTPUT TO c:\INFO_Fodun\Leonardo\Retefuente2011_Corregido.csv.

FOR EACH anexos13 WHERE (SUBSTRING(anexos13.Cuenta,1,4) = "2442" OR
                         SUBSTRING(anexos13.Cuenta,1,4) = "2445" OR
                         SUBSTRING(anexos13.Cuenta,1,4) = "2448")
                    /*AND anexos13.nit = "10222588"
                    AND anexos13.cuenta = "24451501"*/
                    AND anexos13.ano = 2011 NO-LOCK BREAK BY anexos13.nit
                                                          BY anexos13.cuenta
                                                          BY anexos13.cen_costos:
    IF FIRST-OF(anexos13.cuenta) THEN DO:
        sdo = anexos13.sdo_inicial.

        FIND FIRST cuentas WHERE cuentas.cuenta = anexos13.cuenta NO-LOCK NO-ERROR.
    END.

    IF AVAILABLE cuentas THEN DO:
        DO cont = 1 TO 12:
            IF cuentas.naturaleza = "DB" THEN
                sdo = sdo + anexos13.db[cont] - anexos13.cr[cont].
            ELSE
                sdo = sdo - anexos13.db[cont] + anexos13.cr[cont].
        END.
    END.

    IF LAST-OF(anexos13.cen_costos) THEN
        EXPORT DELIMITER ";" anexos13.cuenta
                             anexos13.cen_costos
                             anexos13.nit
                             anexos13.sdo_inicial
                             anexos13.db[1]
                             anexos13.cr[1]
                             anexos13.db[2]
                             anexos13.cr[2]
                             anexos13.db[3]
                             anexos13.cr[3]
                             anexos13.db[4]
                             anexos13.cr[4]
                             anexos13.db[5]
                             anexos13.cr[5]
                             anexos13.db[6]
                             anexos13.cr[6]
                             anexos13.db[7]
                             anexos13.cr[7]
                             anexos13.db[8]
                             anexos13.cr[8]
                             anexos13.db[9]
                             anexos13.cr[9]
                             anexos13.db[10]
                             anexos13.cr[10]
                             anexos13.db[11]
                             anexos13.cr[11]
                             anexos13.db[12]
                             anexos13.cr[12]
                             sdo.
END.
