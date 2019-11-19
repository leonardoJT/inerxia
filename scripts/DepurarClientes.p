DISABLE TRIGGERS FOR LOAD OF clientes.

FOR EACH clientes WHERE estado <> 1:
    DELETE clientes.
END.
