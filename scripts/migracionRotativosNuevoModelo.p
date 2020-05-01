FOR EACH utilizacionesRotativo:
    DELETE utilizacionesRotativo.
END.
    
FOR EACH creditos WHERE creditos.cod_credito = 123
                    AND creditos.sdo_capital > 0 NO-LOCK:
    CREATE utilizacionesRotativo.
    utilizacionesRotativo.canal = "SFG".
    utilizacionesRotativo.cliente_id = creditos.nit.
    utilizacionesRotativo.credito_id = creditos.num_credito.
    utilizacionesRotativo.cuotas = creditos.plazo.
    utilizacionesRotativo.cuotas_restantes = creditos.plazo - creditos.cuo_pagadas.
    utilizacionesRotativo.cuota_capital = ROUND(creditos.sdo_capital / utilizacionesRotativo.cuotas_restantes,0).
    utilizacionesRotativo.descripcion = "Utilización original".
    utilizacionesRotativo.estado = 1.
    utilizacionesRotativo.fec_cancelacion = ?.
    utilizacionesRotativo.fec_utilizacion = TODAY.
    utilizacionesRotativo.hora_utilizacion = STRING(TIME,"HH:MM").
    utilizacionesRotativo.monto = creditos.sdo_capital.
    utilizacionesRotativo.saldo = creditos.sdo_capital.
    utilizacionesRotativo.tasa = credito.tasa / 12.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
