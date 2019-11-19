DEFINE VAR ppTasa AS DECIMAL.

MESSAGE "Inicia proceso"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

DEFINE TEMP-TABLE tasas
    FIELD nit AS CHARACTER
    FIELD numCredito AS INTEGER
    FIELD tasa AS DECIMAL.



INPUT FROM "C:\INFO_FODUN\nomina medellin\Tasas PAL.csv".
REPEAT:
    CREATE tasas.
    IMPORT DELIMITER ";" tasas.
    /*DISPLAY tasas WITH 1 COL.*/
END.

FOR EACH creditos WHERE creditos.agencia = 4 AND creditos.sdo_capital > 0:

    FIND FIRST tasas WHERE tasas.nit = creditos.nit
                       AND tasas.numcredito = creditos.num_credito NO-LOCK NO-ERROR.
    IF AVAILABLE tasas THEN
        ppTasa = tasas.tasa * 12.
    ELSE
        ppTasa = creditos.tasa.

    RUN D:\SPS\soportes\fodun\Prog\scripts\GeneraPlanDePagos3.p(INPUT creditos.nit,
                                                                INPUT creditos.num_credito,
                                                                INPUT ppTasa) NO-ERROR.
END.
MESSAGE "Finaliza Proceso"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
