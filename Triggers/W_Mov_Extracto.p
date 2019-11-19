TRIGGER PROCEDURE FOR WRITE OF Mov_Extracto
NEW inserted OLD deleted.

IF
    deleted.Agencia <> inserted.Agencia or 
    deleted.Cuenta <> inserted.Cuenta or 
    deleted.Reg_MovExtrac <> inserted.Reg_MovExtrac
  THEN DO:
    FOR FIRST Conciliacion WHERE (
        Conciliacion.Agencia = deleted.Agencia and
        Conciliacion.Cuenta = deleted.Cuenta and
        Conciliacion.Reg_MovExtrac = deleted.Reg_MovExtrac) NO-LOCK:
      MESSAGE "No puede Actualizar 'Mov_Extracto' Porque 'Conciliacion' EXISTE".
      RETURN ERROR.
    END.
  END.


{valida/W_Mov_Extracto.v}
