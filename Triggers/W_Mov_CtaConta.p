TRIGGER PROCEDURE FOR WRITE OF Mov_CtaConta
NEW inserted OLD deleted.


  IF
    deleted.Agencia <> inserted.Agencia or 
    deleted.Cuenta <> inserted.Cuenta or 
    deleted.Reg_CtaConta <> inserted.Reg_CtaConta
  THEN DO:
    FOR FIRST Conciliacion WHERE (
        Conciliacion.Agencia = deleted.Agencia and
        Conciliacion.Cuenta = deleted.Cuenta and
        Conciliacion.Reg_CtaConta = deleted.Reg_CtaConta) NO-LOCK:
      MESSAGE "No puede Actualizar 'Mov_CtaConta' Porque 'Conciliacion' EXISTE".
      RETURN ERROR.
    END.
  END.


{valida/W_Mov_CtaConta.v}
