FOR EACH aplicarVisionamos WHERE doc_cliente = "19406849"
                             AND (valor > 0 OR valorComision > 0)
                             AND tipoCuenta1_origen = '10'
                             AND fecha = "20140921" BY fecha:
    UPDATE
        grupo_transaccional
        estado
        terminal_ubicacion FORMAT "X(40)"
        fecha
        hora
        transaccion
        cod_error
        cuenta1_origen
        doc_Cliente
        flagReverso
        tipoCuenta1_Origen
        valor FORMAT ">>>>>>>>9"
        valorComision FORMAT ">>>>>>>>9"
        WITH 1 COL.
END.
