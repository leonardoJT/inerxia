TRIGGER PROCEDURE FOR DELETE OF Mov_Extracto.
  
    FOR FIRST Conciliacion WHERE (
      Conciliacion.Agencia = Mov_Extracto.Agencia and
      Conciliacion.Cuenta = Mov_Extracto.Cuenta and
      Conciliacion.Reg_MovExtrac = Mov_Extracto.Reg_MovExtrac) NO-LOCK:
    MESSAGE "No puede Borrar 'Mov_Extracto' Porque 'Conciliacion' EXISTE".
    RETURN ERROR.
  END.



