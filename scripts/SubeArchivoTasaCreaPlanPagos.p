DEFINE TEMP-TABLE tasas
    FIELD numCredito AS INTEGER FORMAT ">>>>>>>>>>"
    FIELD tasa1 AS DECIMAL FORMAT ">9.99999"
    FIELD tasa2 AS DECIMAL FORMAT ">9.99999".

INPUT FROM "C:\archivos sfg\CREDITOSYTASASPAL.csv".

REPEAT:
    CREATE tasas.
    IMPORT DELIMITER ";" tasas.

    tasas.tasa1 = tasas.tasa1 * 100 * 12.
    tasas.tasa2 = tasas.tasa2 * 100 * 12.

/*    DISPLAY tasas WITH 1 COL.*/
END.
INPUT CLOSE.

MESSAGE "Inicia"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH tasas:
    FIND FIRST creditos WHERE creditos.agencia = 4
                          AND creditos.num_credito = tasas.numCredito NO-ERROR.
    IF AVAILABLE creditos THEN DO:
        creditos.tasa = tasas.tasa1.


        RUN \\192.168.1.100\sps\soportes\fodun\Prog\scripts\GeneraPlanDePagos3.p(INPUT creditos.nit,
                                                                                 INPUT creditos.num_credito,
                                                                                 INPUT creditos.tasa) NO-ERROR.
    END.
END.

MESSAGE "Finalizó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
