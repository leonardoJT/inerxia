DEFINE TEMP-TABLE TTMovAhorros LIKE mov_creditos.
FOR EACH mov_creditos WHERE nit = "14432747" AND fecha = 09/23/2011
    AND cod_credito = 17 NO-LOCK:
    DISPLAY mov_creditos WITH 1 COL.
    CREATE TTMOvAhorros.
    BUFFER-COPY mov_creditos TO TTMovahorros.

    TTMovAhorros.fecha = TODAY.
    TTMovAhorros.descrip = "Rev-" + TTMovAhorros.descrip.
    TTMovAhorros.hora = TIME.

    IF TTMovAhorros.cod_operacion = 020101001 THEN
        TTMovAhorros.cod_operacion = 020102001.
    ELSE
        TTMovAhorros.cod_operacion = 020102006.
END.

FOR EACH TTMovAhorros:
    CREATE mov_creditos.
    BUFFER-COPY TTmovahorros TO mov_creditos.
END.

