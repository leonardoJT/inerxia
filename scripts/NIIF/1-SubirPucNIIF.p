/* Importar el PUC_NIIF */
DISABLE TRIGGERS FOR LOAD OF cuentas_NIIF.
DISABLE TRIGGERS FOR LOAD OF cuentas.

DEFINE TEMP-TABLE reg
    FIELD cuenta AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD naturaleza AS CHARACTER
    FIELD tipo AS INTEGER.

INPUT FROM d:\Leonardo\niif\PucNiif.csv.
REPEAT:
    CREATE reg.
    IMPORT DELIMITER ";" reg.
END.
INPUT CLOSE.

FOR EACH cuentas:
    cuentas.cuentaNIIF = "".
END.

FOR EACH cuentas_NIIF:
    DELETE cuentas_NIIF.
END.

FOR EACH reg WHERE reg.cuenta <> "" NO-LOCK:
    CREATE cuentas_niif.
    BUFFER-COPY reg TO cuentas_NIIF.
END.

FOR EACH cuentas_NIIF WHERE cuentas_NIIF.tipo = 2:
    cuentas_NIIF.id_nit = TRUE.
END.

FOR EACH cuentas_NIIF:
    IF LENGTH(cuentas_NIIF.cuenta) = 1 THEN
        cuentas_NIIF.nivel = 1.
    ELSE
        IF LENGTH(cuentas_NIIF.cuenta) = 2 THEN
            cuentas_NIIF.nivel = 2.
        ELSE
            IF LENGTH(cuentas_NIIF.cuenta) = 4 THEN
                cuentas_NIIF.nivel = 3.
            ELSE
                IF LENGTH(cuentas_NIIF.cuenta) = 6 THEN
                    cuentas_NIIF.nivel = 4.
                ELSE
                    IF LENGTH(cuentas_NIIF.cuenta) = 8 THEN
                        cuentas_NIIF.nivel = 5.
                    ELSE
                        IF LENGTH(cuentas_NIIF.cuenta) = 10 THEN
                            cuentas_NIIF.nivel = 6.
                        ELSE
                            IF LENGTH(cuentas_NIIF.cuenta) = 12 THEN
                                cuentas_NIIF.nivel = 7.

    CASE SUBSTRING(cuentas_NIIF.cuenta,1,1):
        WHEN "1" THEN cuentas_NIIF.id_cuenta = 1.
        WHEN "2" THEN cuentas_NIIF.id_cuenta = 2.
        WHEN "3" THEN cuentas_NIIF.id_cuenta = 3.
        WHEN "4" THEN cuentas_NIIF.id_cuenta = 4.
        WHEN "5" THEN cuentas_NIIF.id_cuenta = 4.
        WHEN "6" THEN cuentas_NIIF.id_cuenta = 4.
        WHEN "8" THEN cuentas_NIIF.id_cuenta = 5.
        WHEN "9" THEN cuentas_NIIF.id_cuenta = 5.
    END CASE.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
