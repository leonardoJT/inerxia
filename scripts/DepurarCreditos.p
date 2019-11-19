DISABLE TRIGGERS FOR LOAD OF creditos.

FOR EACH creditos WHERE estado <> 2 AND sdo_capital = 0:
    DELETE creditos.
END.
