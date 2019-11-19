DEFINE VAR cont AS INTEGER.
    
FOR EACH rep_activosFijos WHERE mesesDepreciar = 0
                        AND (valorDepreciado = valorCompra / 2 OR
                             valorActual = valorCompra / 2)
                        AND estado = 1 BY fechaCompra:
    cont = cont + 1.

    UPDATE rep_activosFijos WITH WIDTH 320 1 COL.

    IF cont = 23 THEN
        LEAVE.
END.
