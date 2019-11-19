DISABLE TRIGGERS FOR LOAD OF anexos.

DEFINE TEMP-TABLE tt LIKE anexos.
DEFINE VAR cont AS INTEGER.

FOR EACH anexos WHERE cen_costos = 0:
    CREATE tt.
    BUFFER-COPY anexos TO tt.
    DELETE anexos.
END.

FOR EACH tt NO-LOCK:
    cont = cont + 1.

    FIND FIRST anexos WHERE anexos.agencia = tt.agencia
                        AND anexos.cen_costos = 999
                        AND anexos.cuenta = tt.cuenta
                        AND anexos.nit = tt.nit
                        AND anexos.ano = tt.ano NO-ERROR.
    IF NOT AVAILABLE anexos THEN DO:
        CREATE anexos.
        BUFFER-COPY tt TO anexos.
        anexos.cen_costos = 999.
    END.
    ELSE DO:
        anexos.Base[1] = anexos.base[1] + tt.base[1].
        anexos.Base[2] = anexos.base[2] + tt.base[2].
        anexos.Base[3] = anexos.base[3] + tt.base[3].
        anexos.Base[4] = anexos.base[4] + tt.base[4].
        anexos.Base[5] = anexos.base[5] + tt.base[5].
        anexos.Base[6] = anexos.base[6] + tt.base[6].
        anexos.Base[7] = anexos.base[7] + tt.base[7].
        anexos.Base[8] = anexos.base[8] + tt.base[8].
        anexos.Base[9] = anexos.base[9] + tt.base[9].
        anexos.Base[10] = anexos.base[10] + tt.base[10].
        anexos.Base[11] = anexos.base[11] + tt.base[11].
        anexos.Base[12] = anexos.base[12] + tt.base[12].
        anexos.Cr[1] = anexos.Cr[1] + tt.Cr[1].
        anexos.Cr[2] = anexos.Cr[2] + tt.Cr[2].
        anexos.Cr[3] = anexos.Cr[3] + tt.Cr[3].
        anexos.Cr[4] = anexos.Cr[4] + tt.Cr[4].
        anexos.Cr[5] = anexos.Cr[5] + tt.Cr[5].
        anexos.Cr[6] = anexos.Cr[6] + tt.Cr[6].
        anexos.Cr[7] = anexos.Cr[7] + tt.Cr[7].
        anexos.Cr[8] = anexos.Cr[8] + tt.Cr[8].
        anexos.Cr[9] = anexos.Cr[9] + tt.Cr[9].
        anexos.Cr[10] = anexos.Cr[10] + tt.Cr[10].
        anexos.Cr[11] = anexos.Cr[11] + tt.Cr[11].
        anexos.Cr[12] = anexos.Cr[12] + tt.Cr[12].
        anexos.Db[1] = anexos.Db[1] + tt.Db[1].
        anexos.Db[2] = anexos.Db[2] + tt.Db[2].
        anexos.Db[3] = anexos.Db[3] + tt.Db[3].
        anexos.Db[4] = anexos.Db[4] + tt.Db[4].
        anexos.Db[5] = anexos.Db[5] + tt.Db[5].
        anexos.Db[6] = anexos.Db[6] + tt.Db[6].
        anexos.Db[7] = anexos.Db[7] + tt.Db[7].
        anexos.Db[8] = anexos.Db[8] + tt.Db[8].
        anexos.Db[9] = anexos.Db[9] + tt.Db[9].
        anexos.Db[10] = anexos.Db[10] + tt.Db[10].
        anexos.Db[11] = anexos.Db[11] + tt.Db[11].
        anexos.Db[12] = anexos.Db[12] + tt.Db[12].
        Anexos.Sal_Credito = Anexos.Sal_Credito + tt.sal_credito.
        Anexos.Sal_Debito = anexos.sal_debito + tt.sal_debito.
        Anexos.Sdo_Base = Anexos.Sdo_Base + tt.sdo_base.
        Anexos.Sdo_Final = Anexos.Sdo_Final + tt.sdo_final.
        Anexos.Sdo_Inicial = Anexos.Sdo_Inicial + tt.sdo_inicial.
    END.
END.

MESSAGE "Fin" cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
