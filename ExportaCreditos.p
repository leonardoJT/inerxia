
DEFINE VARIABLE Nom AS CHARACTER FORMAT "x(70)" NO-UNDO.
DEFINE VARIABLE Num AS INTEGER     NO-UNDO.
DEFINE VARIABLE tCuota AS INTEGER     NO-UNDO.

OUTPUT TO c:\INFO_cooprudea\Creditos.csv.

ASSIGN num = 1.
FOR EACH clientes WHERE Tipo_Vinculo = 1 NO-LOCK:
    ASSIGN nom = "".
    ASSIGN nom = trim(clientes.apellido1 + " " + clientes.apellido2 + " " + clientes.Nombre).
    ASSIGN num = num + 1.
    FOR EACH creditos WHERE creditos.nit = clientes.nit AND creditos.pagare NE ? NO-LOCK:
        ASSIGN tcuota = 0.
        FIND FIRST ahorros WHERE cod_ahorro = 217 AND ahorros.cue_ahorros = creditos.pagare AND ahorros.nit = creditos.nit NO-LOCK NO-ERROR.
        IF available ahorros THEN
            DO:
                tcuota = creditos.cuota + ahorros.cuota.
            END.
        ELSE
            DO:
                tcuota = creditos.cuota.
            END.
        PUT creditos.nit ";" num ";" creditos.nit ";" nom ";" creditos.pagare ";" creditos.Monto ";" creditos.Fec_Desembolso ";" creditos.Sdo_Capital ";" creditos.Fec_UltPago ";" tcuota ";;" (creditos.Plazo - creditos.Cuo_Pagadas) ";" SKIP(0).
    END.
END.
