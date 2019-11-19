TRIGGER PROCEDURE FOR DELETE OF Mov_CtaConta.
FOR FIRST Conciliacion WHERE (
     Conciliacion.Oficina = Mov_CtaConta.Oficina and
      Conciliacion.Cuenta = Mov_CtaConta.Cuenta and
      Conciliacion.Reg_CtaConta = Mov_CtaConta.Reg_CtaConta) NO-LOCK:
    MESSAGE "No puede Borrar 'Mov_CtaConta' Porque 'Conciliacion' EXISTE".
    RETURN ERROR.
  END.

