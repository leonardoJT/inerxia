TRIGGER PROCEDURE FOR WRITE OF Conciliacion

NEW inserted OLD deleted.

    
    IF  inserted.Reg_CtaConta NE ""
    AND inserted.Reg_CtaConta NE ? THEN DO:
        FIND FIRST Mov_CtaConta WHERE (
            inserted.Agencia = Mov_CtaConta.Agencia and
            inserted.Cuenta = Mov_CtaConta.Cuenta and
            inserted.Reg_CtaConta = Mov_CtaConta.Reg_CtaConta) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Mov_CtaConta THEN DO:
           MESSAGE "No puede Actualizar 'Conciliacion' Porque 'Mov_CtaConta' NO EXISTE".
           RETURN ERROR.
        END.
    END.

    
    IF  inserted.Reg_MovExtrac NE ""
    AND inserted.Reg_MovExtrac NE ? THEN DO:
        FIND FIRST Mov_Extracto WHERE (
            inserted.Agencia = Mov_Extracto.Agencia and
            inserted.Cuenta = Mov_Extracto.Cuenta and
            inserted.Reg_MovExtrac = Mov_Extracto.Reg_MovExtrac) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Mov_Extracto THEN DO:
           MESSAGE "No puede Actualizar 'Conciliacion' Porque 'Mov_Extracto' NO EXISTE".
           RETURN ERROR.
        END.
    END.


{valida/W_Conciliacion.v}
