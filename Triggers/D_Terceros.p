TRIGGER PROCEDURE FOR DELETE OF Terceros.

  FOR FIRST Empresas WHERE (
      Empresas.Nit = Terceros.Nit) NO-LOCK:
    MESSAGE "No puede Borrar 'Terceros' Porque 'Empresas' EXISTE".
    RETURN ERROR.
  END.

  FOR FIRST Garantias WHERE (
      Garantias.Nit_Aseguradora = Terceros.Nit) NO-LOCK:
    MESSAGE "No puede Borrar 'Terceros' Porque 'Garantias' EXISTE".
    RETURN ERROR.
  END.

  FOR FIRST Creditos WHERE (
      Creditos.Abogado = Terceros.Nit) NO-LOCK:
    MESSAGE "No puede Borrar 'Terceros' Porque 'Creditos' EXISTE".
    RETURN ERROR.
  END.


  FOR FIRST BenAut_Aho WHERE (
      BenAut_Aho.Cedula = Terceros.Nit) NO-LOCK:
    MESSAGE "No puede Borrar 'Terceros' Porque 'BenAut_Aho' EXISTE".
    RETURN ERROR.
  END.


  FOR FIRST Inf_Juridica WHERE (
      Inf_Juridica.Nit_RepLegal = Terceros.Nit) NO-LOCK:
    MESSAGE "No puede Borrar 'Terceros' Porque 'Inf_Juridica' EXISTE".
    RETURN ERROR.
  END.


  FOR FIRST Inf_Familiar WHERE (
      Inf_Familiar.Nit_Familiar = Terceros.Nit) NO-LOCK:
    MESSAGE "No puede Borrar 'Terceros' Porque 'Inf_Familiar' EXISTE".
    RETURN ERROR.
  END.


  FOR FIRST Clientes WHERE (
      Clientes.Nit = Terceros.Nit) NO-LOCK:
    MESSAGE "No puede Borrar 'Terceros' Porque 'Clientes' EXISTE".
    RETURN ERROR.
  END.

  FOR FIRST Anexos WHERE (
      Anexos.Nit = Terceros.Nit) NO-LOCK:
    MESSAGE "No puede Borrar 'Terceros' Porque 'Anexos' EXISTE".
    RETURN ERROR.
  END.




