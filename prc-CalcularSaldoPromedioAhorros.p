DEFINE INPUT PARAMETER pFecha AS DATE.

FOR EACH ahorros WHERE ahorros.tip_ahorro <> 3
                   AND ahorros.fec_apertura <> ?
                   AND ahorros.estado = 1:
    ahorros.sdo_promedio = ((ahorros.sdo_promedio * (pFecha - ahorros.fec_apertura - 1)) + ahorros.sdo_disponible) / (pFecha - ahorros.fec_apertura).
END.


