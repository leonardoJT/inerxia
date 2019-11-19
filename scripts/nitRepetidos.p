DEFINE BUFFER bfrClientes FOR clientes.

OUTPUT TO d:\Leonardo\nits_3.csv.
    FOR EACH clientes WHERE LENGTH(clientes.nit) >= 5
                        AND INDEX(clientes.nit,"-") > 0 NO-LOCK:
        FOR EACH bfrClientes WHERE bfrClientes.nit <> clientes.nit
                               AND LENGTH(bfrClientes.nit) >= 5
                               AND INDEX(clientes.nit,bfrClientes.nit) = 1 NO-LOCK:
            EXPORT DELIMITER ";"
                clientes.nit clientes.estado bfrClientes.nit bfrClientes.estado.
        END.
    END.
OUTPUT CLOSE.
