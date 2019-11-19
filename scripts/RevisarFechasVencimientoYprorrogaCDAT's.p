DEFINE VAR fechaAux AS DATE.

OUTPUT TO c:\INFO_Fodun\CDAT's.csv.
FOR EACH ahorros WHERE tip_ahorro = 3
                   AND estado = 1
                   AND sdo_disponible > 0 NO-LOCK BY fec_apertura:
    IF fec_prorroga <> ? THEN
        fechaAux = fec_ultliq + 1.
    ELSE
        fechaAux = ?.

    EXPORT DELIMITER ";" agencia nit cue_ahorro fec_apertura fechaAux plazo fec_vencimiento.
END.
