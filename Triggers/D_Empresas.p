TRIGGER PROCEDURE FOR DELETE OF Empresas.

 FOR FIRST Rec_Nomina WHERE (
      Rec_Nomina.Oficina = Empresas.Oficina and
      Rec_Nomina.Cod_Empresa = Empresas.Cod_Empresa) NO-LOCK:
    MESSAGE "No puede Borrar 'Empresas' Porque 'Rec_Nomina' EXISTE".
    RETURN ERROR.
  END.




