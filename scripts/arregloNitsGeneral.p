DISABLE TRIGGERS FOR LOAD OF mov_contable.
DISABLE TRIGGERS FOR LOAD OF anexos.
DISABLE TRIGGERS FOR LOAD OF anexos13.

DEFINE VAR nitAux AS CHARACTER.
DEFINE BUFFER bfrClientes FOR clientes.

FOR EACH clientes WHERE INDEX(clientes.nit,"-") > 0
                    AND INDEX(clientes.nit,"X") = 0
                    /*AND clientes.estado = 1*/:
    nitAux = clientes.nit.

    FIND FIRST bfrClientes WHERE bfrClientes.nit = SUBSTRING(clientes.nit,1,INDEX(clientes.nit,"-") - 1) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bfrClientes THEN
        clientes.nit = SUBSTRING(clientes.nit,1,INDEX(clientes.nit,"-") - 1).
    ELSE
        clientes.nit = clientes.nit + "X".

    FIND FIRST anexos_cliente WHERE anexos_cliente.nit = nitAux NO-ERROR.
    IF AVAILABLE anexos_cliente THEN
        anexos_cliente.nit = SUBSTRING(anexos_cliente.nit,1,INDEX(anexos_cliente.nit,"-") - 1).

    FOR EACH mov_contable WHERE mov_contable.nit = nitAux:
        mov_contable.nit = SUBSTRING(mov_contable.nit,1,INDEX(mov_contable.nit,"-") - 1).
    END.

    FOR EACH anexos WHERE anexos.nit = nitAux:
        anexos.nit = SUBSTRING(anexos.nit,1,INDEX(anexos.nit,"-") - 1).
    END.

    FOR EACH anexos13 WHERE anexos13.nit = nitAux:
        anexos13.nit = SUBSTRING(anexos13.nit,1,INDEX(anexos13.nit,"-") - 1).
    END.
END.
