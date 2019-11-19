DISABLE TRIGGERS FOR LOAD OF cuentas.
DISABLE TRIGGERS FOR LOAD OF cuentas_NIIF.

DEFINE TEMP-TABLE homologa
    FIELD cuenta AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD cuentaNiif AS CHARACTER
    FIELD nombreNiif AS CHARACTER.

DEFINE TEMP-TABLE pucNiif 
    FIELD cuenta AS CHARACTER
    FIELD nombre AS CHARACTER.

DEFINE TEMP-TABLE tempCuentas LIKE cuentas.

FOR EACH cuentas_NIIf:
    DELETE cuentas_NIIF.
END.

FOR EACH cuentas:
    cuentas.cuentaNIIF = "".
END.

/* Importamos el archivo entregado por Rafael */
INPUT FROM d:\Leonardo\niif\puc_niif.csv.
REPEAT:
    CREATE homologa.
    IMPORT DELIMITER ";" homologa.
END.
INPUT CLOSE.

/* Extraemos todas las cuentas de NIIF */

MESSAGE "Extraemos todas las cuentas de NIIF"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH homologa WHERE homologa.cuenta <> "" AND homologa.cuentaNiif <> "" NO-LOCK:
    FIND FIRST pucNiif WHERE pucNiif.cuenta = homologa.cuentaNiif NO-LOCK NO-ERROR.
    IF NOT AVAILABLE pucNiif THEN DO:
        CREATE pucNiif.
        pucNiif.cuenta = homologa.cuentaNiif.
        pucNiif.nombre = homologa.nombreNiif.
    END.
END.

/* Revisamos cuáles de las cuentas NIIF no están creadas en el PUC_NIIF y las creamos en una temporal para luego pasarlas al PUC NIIF */

MESSAGE "Revisamos cuáles de las cuentas NIIF no están creadas en el PUC_NIIF y las creamos en una temporal para luego pasarlas al PUC NIIF"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH pucNiif NO-LOCK BY pucNiif.cuenta:
    FIND FIRST cuentas_niif WHERE cuentas_niif.cuenta = pucNiif.cuenta NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentas_Niif THEN DO:
        FIND FIRST homologa WHERE homologa.cuenta <> "" AND homologa.cuentaNiif = pucNiif.cuenta NO-LOCK.
        IF NOT AVAILABLE homologa THEN
            DISPLAY homologa WITH 1 COL.
        ELSE DO:
            FIND FIRST cuentas WHERE cuentas.cuenta = homologa.cuenta NO-LOCK NO-ERROR.
            IF NOT AVAILABLE cuentas THEN
                DISPLAY homologa WITH 1 COL.
            ELSE DO:
                CREATE cuentas_NIIF.
                BUFFER-COPY cuentas TO cuentas_NIIF.
                cuentas_NIIF.cuenta = homologa.cuentaNIIF.
                cuentas_NIIF.nombre = homologa.nombreNIIF.

                RELEASE cuentas_NIIF.
            END.
        END.
    END.
END.

/* Hacemos la correspondiente homologacion del PUC_SS al PUC_NIIF */

MESSAGE "Homologamos"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH cuentas WHERE cuentas.tipo = 2:
    FIND FIRST homologa WHERE homologa.cuenta = cuentas.cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE homologa THEN
        cuentas.cuentaNIIF = homologa.cuentaNiif.
END.
