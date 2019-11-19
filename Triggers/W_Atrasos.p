TRIGGER PROCEDURE FOR WRITE OF Atrasos
NEW inserted OLD deleted.
    FIND FIRST Creditos WHERE (
        inserted.agencia = Creditos.agencia and
        inserted.Cod_credito = Creditos.Cod_credito and
        inserted.Pagare = Creditos.Pagare) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Creditos THEN DO:
      MESSAGE "No puede Actualizar 'Atrasos' Porque 'Creditos' NO EXISTE".
      RETURN ERROR.
    END.

{valida/W_Atrasos.v}
