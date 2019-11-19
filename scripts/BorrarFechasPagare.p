FOR EACH ahorros WHERE ahorros.tip_ahorro = 4 AND ahorros.agencia = 1
    AND ahorros.estado = 1 NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-ERROR.
    IF AVAILABLE clientes THEN
        clientes.fecPagare = ?.
END.
