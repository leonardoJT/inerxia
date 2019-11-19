TRIGGER PROCEDURE FOR WRITE OF Empresas
NEW inserted OLD deleted.


  IF
    deleted.Agencia <> inserted.Agencia or 
    deleted.Cod_Empresa <> inserted.Cod_Empresa
  THEN DO:
    FOR FIRST Rec_Nomina WHERE (
        Rec_Nomina.Agencia = deleted.Agencia and
        Rec_Nomina.Cod_Empresa = deleted.Cod_Empresa) NO-LOCK:
      MESSAGE "No puede Actualizar 'Empresas' Porque 'Rec_Nomina' EXISTE".
      RETURN ERROR.
    END.
  END.

    FIND FIRST Clientes WHERE (
        inserted.Nit = Clientes.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Clientes THEN DO:
      MESSAGE "No puede Actualizar 'Empresas' Porque 'Clientes' NO EXISTE".
      RETURN ERROR.
    END.



{valida/W_Empresas.v}
