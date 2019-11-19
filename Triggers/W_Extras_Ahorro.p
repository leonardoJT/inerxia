TRIGGER PROCEDURE FOR WRITE OF Extras_Ahorro
NEW inserted OLD deleted.

    FIND FIRST Ahorros WHERE (
        inserted.Oficina = Ahorros.Oficina and
        inserted.Cod_Producto = Ahorros.Cod_Producto and
        inserted.Cue_Ahorros = Ahorros.Cue_Ahorros) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Ahorros THEN DO:
      MESSAGE "No puede Actualizar 'Extras_Ahorro' Porque 'Ahorros' NO EXISTE".
      RETURN ERROR.
    END.

{valida/W_Extras_Ahorro.v}
