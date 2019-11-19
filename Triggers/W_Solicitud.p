TRIGGER PROCEDURE FOR WRITE OF Solicitud
NEW inserted OLD deleted.

  IF
    deleted.Num_Solicitud <> inserted.Num_Solicitud
  THEN DO:
    FOR FIRST Garantias WHERE (
        Garantias.Num_Solicitud = deleted.Num_Solicitud) NO-LOCK:
      MESSAGE "No puede Actualizar 'Solicitud' Porque 'Garantias' EXISTE".
      RETURN ERROR.
    END.
  END.

  IF
    deleted.Num_Solicitud <> inserted.Num_Solicitud
  THEN DO:
    FOR FIRST Creditos WHERE (
        Creditos.Num_Solicitud = deleted.Num_Solicitud AND
        Creditos.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Solicitud' Porque 'Creditos' EXISTE".
      RETURN ERROR.
    END.
  END.

    FIND FIRST Clientes WHERE (
        inserted.Nit = Clientes.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Clientes THEN DO:
      MESSAGE "No puede Actualizar 'Solicitud' Porque 'Clientes' NO EXISTE".
      RETURN ERROR.
    END.

    IF inserted.Num_Asesoria EQ ?
    OR inserted.Num_Asesoria EQ 0 THEN
       RETURN.

    FIND FIRST Asesoria WHERE (
        inserted.Num_Asesoria = Asesoria.Num_Asesoria) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Asesoria THEN DO:
      MESSAGE "No puede Actualizar 'Solicitud' Porque 'Asesoria' NO EXISTE".
      RETURN ERROR.
    END.


    FIND FIRST Pro_Creditos WHERE (
        inserted.Cod_Credito = Pro_Creditos.Cod_Credito) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Pro_Creditos THEN DO:
      MESSAGE "No puede Actualizar 'Solicitud' Porque 'Pro_Creditos' NO EXISTE".
      RETURN ERROR.
    END.



{valida/W_Solicitud.v}
