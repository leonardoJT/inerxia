DISABLE TRIGGERS FOR LOAD OF clientes.
DEF TEMP-TABLE t NO-UNDO
    FIELD nit AS CHAR.
CREATE t.
t.nit = "4900733".    
CREATE t.
t.nit = "5886161".    
DO WHILE TRUE:
    FOR EACH t,
        EACH clientes NO-LOCK
            WHERE
            clientes.nit = t.nit,
        EACH anexos_clientes NO-LOCK
            WHERE
                anexos_clientes.nit = clientes.nit:
        DISPLAY clientes.nit anexos_clientes.delphi.
    END.
END.
