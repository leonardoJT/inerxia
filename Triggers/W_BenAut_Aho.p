TRIGGER PROCEDURE FOR WRITE OF BenAut_Aho
NEW inserted OLD deleted.
    FIND FIRST Terceros WHERE (
        inserted.Cedula = Terceros.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Terceros THEN DO:
      MESSAGE "No puede Actualizar 'BenAut_Aho' Porque 'Terceros' NO EXISTE".
      RETURN ERROR.
    END.

    FIND FIRST Ahorros WHERE (
        inserted.Oficina = Ahorros.Oficina and
        inserted.Cod_Producto = Ahorros.Cod_Producto and
        inserted.Cue_Ahorros = Ahorros.Cue_Ahorros) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Ahorros THEN DO:
      MESSAGE "No puede Actualizar 'BenAut_Aho' Porque 'Ahorros' NO EXISTE".
      RETURN ERROR.
    END.

{valida/W_BenAut_Aho.v}
