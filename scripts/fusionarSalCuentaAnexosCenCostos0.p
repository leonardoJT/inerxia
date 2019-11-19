DISABLE TRIGGERS FOR LOAD OF sal_cuenta.

DEFINE TEMP-TABLE tt LIKE sal_cuenta.

FOR EACH sal_cuenta WHERE cen_costos = 0:
    CREATE tt.
    BUFFER-COPY sal_cuenta TO tt.
    DELETE sal_cuenta.
END.

FOR EACH tt NO-LOCK:
    FIND FIRST sal_cuenta WHERE sal_cuenta.agencia = tt.agencia
                            AND sal_cuenta.cen_costos = 999
                            AND sal_cuenta.cuenta = tt.cuenta
                            AND sal_cuenta.ano = tt.ano NO-ERROR.
    IF NOT AVAILABLE sal_cuenta THEN DO:
        CREATE sal_cuenta.
        BUFFER-COPY tt TO sal_cuenta.
        sal_cuenta.cen_costos = 999.
    END.
    ELSE DO:
        Sal_Cuenta.Base[1] = sal_cuenta.base[1] + tt.base[1].
        Sal_Cuenta.Base[2] = sal_cuenta.base[2] + tt.base[2].
        Sal_Cuenta.Base[3] = sal_cuenta.base[3] + tt.base[3].
        Sal_Cuenta.Base[4] = sal_cuenta.base[4] + tt.base[4].
        Sal_Cuenta.Base[5] = sal_cuenta.base[5] + tt.base[5].
        Sal_Cuenta.Base[6] = sal_cuenta.base[6] + tt.base[6].
        Sal_Cuenta.Base[7] = sal_cuenta.base[7] + tt.base[7].
        Sal_Cuenta.Base[8] = sal_cuenta.base[8] + tt.base[8].
        Sal_Cuenta.Base[9] = sal_cuenta.base[9] + tt.base[9].
        Sal_Cuenta.Base[10] = sal_cuenta.base[10] + tt.base[10].
        Sal_Cuenta.Base[11] = sal_cuenta.base[11] + tt.base[11].
        Sal_Cuenta.Base[12] = sal_cuenta.base[12] + tt.base[12].
        Sal_Cuenta.Cr[1] = Sal_Cuenta.Cr[1] + tt.Cr[1].
        Sal_Cuenta.Cr[2] = Sal_Cuenta.Cr[2] + tt.Cr[2].
        Sal_Cuenta.Cr[3] = Sal_Cuenta.Cr[3] + tt.Cr[3].
        Sal_Cuenta.Cr[4] = Sal_Cuenta.Cr[4] + tt.Cr[4].
        Sal_Cuenta.Cr[5] = Sal_Cuenta.Cr[5] + tt.Cr[5].
        Sal_Cuenta.Cr[6] = Sal_Cuenta.Cr[6] + tt.Cr[6].
        Sal_Cuenta.Cr[7] = Sal_Cuenta.Cr[7] + tt.Cr[7].
        Sal_Cuenta.Cr[8] = Sal_Cuenta.Cr[8] + tt.Cr[8].
        Sal_Cuenta.Cr[9] = Sal_Cuenta.Cr[9] + tt.Cr[9].
        Sal_Cuenta.Cr[10] = Sal_Cuenta.Cr[10] + tt.Cr[10].
        Sal_Cuenta.Cr[11] = Sal_Cuenta.Cr[11] + tt.Cr[11].
        Sal_Cuenta.Cr[12] = Sal_Cuenta.Cr[12] + tt.Cr[12].
        Sal_Cuenta.Db[1] = Sal_Cuenta.Db[1] + tt.Db[1].
        Sal_Cuenta.Db[2] = Sal_Cuenta.Db[2] + tt.Db[2].
        Sal_Cuenta.Db[3] = Sal_Cuenta.Db[3] + tt.Db[3].
        Sal_Cuenta.Db[4] = Sal_Cuenta.Db[4] + tt.Db[4].
        Sal_Cuenta.Db[5] = Sal_Cuenta.Db[5] + tt.Db[5].
        Sal_Cuenta.Db[6] = Sal_Cuenta.Db[6] + tt.Db[6].
        Sal_Cuenta.Db[7] = Sal_Cuenta.Db[7] + tt.Db[7].
        Sal_Cuenta.Db[8] = Sal_Cuenta.Db[8] + tt.Db[8].
        Sal_Cuenta.Db[9] = Sal_Cuenta.Db[9] + tt.Db[9].
        Sal_Cuenta.Db[10] = Sal_Cuenta.Db[10] + tt.Db[10].
        Sal_Cuenta.Db[11] = Sal_Cuenta.Db[11] + tt.Db[11].
        Sal_Cuenta.Db[12] = Sal_Cuenta.Db[12] + tt.Db[12].
        Sal_Cuenta.sal_inicial = Sal_Cuenta.sal_inicial + tt.sal_inicial.
        Sal_Cuenta.Sdo_Base = Sal_Cuenta.Sdo_Base + tt.Sdo_Base.
    END.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
