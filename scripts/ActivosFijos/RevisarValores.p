FOR EACH activosFijos WHERE contabilizado = YES
                        AND valorACtual = 0
                        AND valorDepreciado = 0
                        AND valorCompra > 0 BY fechaCompra DESC:
    UPDATE activosFijos WITH WIDTH 300 1 COL.
END.
