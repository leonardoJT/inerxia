DEFINE TEMP-TABLE plazos
    FIELD agencia AS INTEGER
    FIELD cedula AS CHARACTER
    FIELD numCredito AS DECIMAL
    FIELD plazo AS INTEGER.
    
INPUT FROM c:\ErrorPlazos.txt.
REPEAT:
    CREATE plazos.
    IMPORT DELIMITER ";" plazos.

    IF plazos.plazo > 0 THEN DO:
        RUN D:\SPS\soportes\fodun\Prog\scripts\GeneraPlanDePagos.p(INPUT plazos.cedula,
                                                                   INPUT plazos.numCredito,
                                                                   INPUT plazos.plazo).

        RUN D:\SPS\soportes\fodun\Prog\scripts\CorregirControlPagosTodos.p(INPUT plazos.cedula,
                                                                           INPUT plazos.numCredito).
    END.
END.
INPUT CLOSE.
