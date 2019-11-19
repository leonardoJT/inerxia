OUTPUT TO d:\Leonardo\planes.csv.
FOR EACH amortizacion NO-LOCK BY num_Credito BY nro_cuota:
    EXPORT DELIMITER ";" amortizacion.
END.
OUTPUT CLOSE.

