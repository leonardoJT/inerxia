TRIGGER PROCEDURE FOR WRITE OF Che_Transito
NEW inserted OLD deleted.
    IF inserted.Cod_Compensa EQ ?
    OR inserted.Cod_Compensa EQ 0 THEN
       RETURN.

    FIND FIRST Bancos WHERE (
        inserted.Cod_Compensa = Bancos.Cod_Compensa) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Bancos THEN DO:
      MESSAGE "No puede Actualizar 'Che_Transito' Porque 'Bancos' NO EXISTE".
      RETURN ERROR.
    END.

{valida/W_Che_Transito.v}
