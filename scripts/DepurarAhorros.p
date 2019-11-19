DISABLE TRIGGERS FOR LOAD OF ahorros.

FOR EACH ahorros WHERE estado <> 1 AND sdo_disponible = 0:
    DELETE ahorros.
END.
