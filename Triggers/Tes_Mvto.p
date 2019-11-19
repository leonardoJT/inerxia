TRIGGER PROCEDURE FOR WRITE OF Mov_Contable.
{Incluido\VARIABLE.I}

FIND FIRST Cuentas WHERE Cuentas.Cuenta = Mov_Contable.Cuenta NO-LOCK NO-ERROR.

IF Mov_Contable.Cuenta <> "" THEN DO:
    REPEAT:
        FIND FIRST Sal_Cuenta WHERE Sal_Cuenta.Agencia = Mov_Contable.Agencia
                                AND Sal_Cuenta.Cen_Costos = Mov_Contable.Cen_Costos
                                AND Sal_Cuenta.Cuenta = Mov_Contable.Cuenta
                                AND Sal_Cuenta.Ano = YEAR(Mov_Contable.Fec_Contable) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF NOT AVAIL Sal_Cuenta THEN DO:
            IF LOCKED Sal_Cuenta THEN
                NEXT.

            CREATE Sal_Cuenta.
            ASSIGN Sal_Cuenta.Agencia = Mov_Contable.Agencia
                   Sal_Cuenta.Cuenta = Mov_Contable.Cuenta
                   Sal_Cuenta.Ano = YEAR(Mov_Contable.Fec_Contable)
                   Sal_Cuenta.Cen_Costos = Mov_Contable.Cen_Costos.
        END.

        IF Mov_Contable.Db GT 0 THEN
            Sal_Cuenta.Db[MONTH(Mov_Contable.Fec_Contable)] = Sal_Cuenta.Db[MONTH(Mov_Contable.Fec_Contable)] + Mov_Contable.Db.

        IF Mov_Contable.Cr GT 0 THEN
            Sal_Cuenta.Cr[MONTH(Mov_Contable.Fec_Contable)] = Sal_Cuenta.Cr[MONTH(Mov_Contable.Fec_Contable)] + Mov_Contable.Cr.

        RELEASE Sal_Cuenta.
        LEAVE.
    END.
END.

IF Mov_Contable.Nit <> "" AND cuentas.id_nit = YES THEN DO:
    REPEAT:
        FIND FIRST Anexos WHERE Anexos.Agencia = Mov_Contable.Agencia
                            AND Anexos.Cen_Costos = Mov_Contable.Cen_Costos
                            AND Anexos.Cuenta = Mov_Contable.Cuenta
                            AND Anexos.Nit = Mov_Contable.Nit
                            AND Anexos.Ano EQ YEAR(Mov_Contable.Fec_Contable) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF NOT AVAIL Anexos THEN DO:
            IF LOCKED Anexos THEN
                NEXT.

            CREATE Anexos.
            ASSIGN Anexos.Agencia = Mov_Contable.Agencia
                   Anexos.Nit = Mov_Contable.Nit
                   Anexos.Cuenta = Mov_Contable.Cuenta
                   Anexos.Ano = YEAR(Mov_Contable.Fec_Contable)
                   Anexos.Cen_Costos = Mov_Contable.Cen_Costos.
        END.

        Anexos.Base[MONTH(Mov_Contable.Fec_Contable)] = Anexos.Base[MONTH(Mov_Contable.Fec_Contable)] + Mov_Contable.Base.

        IF Mov_Contable.Db GT 0 THEN
            Anexos.Db[MONTH(Mov_Contable.Fec_Contable)] = Anexos.Db[MONTH(Mov_Contable.Fec_Contable)] + Mov_Contable.Db.

        IF Mov_Contable.Cr GT 0 THEN
            Anexos.Cr[MONTH(Mov_Contable.Fec_Contable)] = Anexos.Cr[MONTH(Mov_Contable.Fec_Contable)] + Mov_Contable.Cr.

        RELEASE Anexos.
        LEAVE.
    END.
END.

IF Cuentas.Id_Detalle THEN DO:
    REPEAT:
        FIND FIRST Detalle WHERE Detalle.Agencia EQ Mov_Contable.Agencia
                             AND Detalle.Cuenta EQ Mov_Contable.Cuenta
                             AND Detalle.Nit EQ Mov_Contable.Nit
                             AND Detalle.Doc_referencia EQ Mov_Contable.Doc_Referencia EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF NOT AVAILABLE(Detalle) THEN
            IF LOCKED(Detalle) THEN
                NEXT.
            ELSE DO:
                CREATE Detalle.
                ASSIGN Detalle.Agencia = Mov_Contable.Agencia
                       Detalle.Nit = Mov_Contable.Nit
                       Detalle.Cuenta = Mov_Contable.Cuenta
                       Detalle.Cen_Costos = Mov_Contable.Cen_Costos
                       Detalle.Fec_Contable = Mov_Contable.Fec_Contable
                       Detalle.Fec_Grabacion = Mov_Contable.Fec_Grabacion
                       Detalle.Plazo = Mov_Contable.Det_Plazo
                       Detalle.Doc_Referencia = Mov_Contable.Doc_Referencia
                       Detalle.Valor_amortizacion = Mov_Contable.Det_ValAmortizacion
                       Detalle.Fec_ProntoPago = Mov_Contable.Fec_ProntoPago
                       Detalle.Por_ProntoPago = Mov_Contable.Por_ProntoPago.

                IF Mov_Contable.Cr GT 0 THEN
                    Detalle.Valor_Inicial = Mov_Contable.Cr.
                ELSE
                    Detalle.Valor_Inicial = Mov_Contable.Db.
            END.

        ASSIGN Detalle.Db = Detalle.Db + Mov_Contable.Db
               Detalle.Cr = Detalle.Cr + Mov_Contable.Cr
               Detalle.Fec_UltActualizacion = Mov_Contable.Fec_Contable.

        RELEASE Detalle.
        LEAVE.
    END.
 END.
