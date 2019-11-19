DISABLE TRIGGERS FOR LOAD OF mov_contable.
DISABLE TRIGGERS FOR LOAD OF anexos.
DISABLE TRIGGERS FOR LOAD OF anexos13.

DEFINE VAR registro AS CHARACTER.

INPUT FROM d:\Leonardo\nits.txt.
REPEAT :
    IMPORT registro.

    FIND FIRST clientes WHERE clientes.nit = registro NO-ERROR.
    IF AVAILABLE clientes THEN
        /*clientes.nit = clientes.nit + "X".*/
        clientes.nit = SUBSTRING(clientes.nit,1,INDEX(clientes.nit,"-") - 1).

    FIND FIRST anexos_cliente WHERE anexos_cliente.nit = registro NO-ERROR.
    IF AVAILABLE anexos_cliente THEN
        /*anexos_cliente.nit = anexos_cliente.nit + "X".*/
        anexos_cliente.nit = SUBSTRING(anexos_cliente.nit,1,INDEX(anexos_cliente.nit,"-") - 1).

    FOR EACH mov_contable WHERE mov_contable.nit = registro:
        mov_contable.nit = SUBSTRING(mov_contable.nit,1,INDEX(mov_contable.nit,"-") - 1).
    END.

    FOR EACH anexos WHERE anexos.nit = registro:
        anexos.nit = SUBSTRING(anexos.nit,1,INDEX(anexos.nit,"-") - 1).
    END.

    FOR EACH anexos13 WHERE anexos13.nit = registro:
        anexos13.nit = SUBSTRING(anexos13.nit,1,INDEX(anexos13.nit,"-") - 1).
    END.
END.
