TRIGGER PROCEDURE FOR WRITE OF Terceros
NEW inserted OLD deleted.

  IF
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Empresas WHERE (
        Empresas.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Terceros' Porque 'Empresas' EXISTE".
      RETURN ERROR.
    END.
  END.


  IF
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Garantias WHERE (
        Garantias.Nit_Aseguradora = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Terceros' Porque 'Garantias' EXISTE".
      RETURN ERROR.
    END.
  END.

/*
  IF
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Creditos WHERE (
        Creditos.Abogado = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Terceros' Porque 'Creditos' EXISTE".
      RETURN ERROR.
    END.
  END.


  IF
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST BenAut_Aho WHERE (
        BenAut_Aho.Cedula = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Terceros' Porque 'BenAut_Aho' EXISTE".
      RETURN ERROR.
    END.
  END.


  IF
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Inf_Juridica WHERE (
        Inf_Juridica.Nit_RepLegal = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Terceros' Porque 'Inf_Juridica' EXISTE".
      RETURN ERROR.
    END.
  END.


  IF
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Inf_Familiar WHERE (
        Inf_Familiar.Nit_Familiar = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Terceros' Porque 'Inf_Familiar' EXISTE".
      RETURN ERROR.
    END.
  END.
*/

  IF
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Clientes WHERE (
        Clientes.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Terceros' Porque 'Clientes' EXISTE".
      RETURN ERROR.
    END.
  END.



{valida/W_Terceros.v}
