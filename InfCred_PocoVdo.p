OUTPUT TO C:\INFO_utrahuilca\Inf_CredVdo10000.Txt.

SESSION:SET-WAIT-STATE("General").

DISPLAY "  COOPERATIVA UTRAHUILCA -   Creditos con Valor-Vencido hasta $10,000" SKIP(1)
    WITH WIDTH 100 NO-LABELS.
FOR EACH Creditos WHERE Estado EQ 2 AND Val_Atraso GT 0 AND Val_Atraso LE 10000 NO-LOCK
                     BY Agencia BY nit:
    DISPLAY Creditos.Agencia      LABEL "Ag."
            Creditos.Nit          LABEL "Ced./Nit"
            Creditos.Val_Atraso   LABEL "Vlr.Vencido"
            Creditos.Sdo_Capital  LABEL "Sdo-Capital"
            Creditos.Dias_Atraso  LABEL "Dias-Vdo"
            Creditos.Categoria    LABEL "Calif."
        WITH DOWN WIDTH 100 FRAME Det NO-BOX NO-LABELS USE-TEXT STREAM-IO.
END.

OUTPUT CLOSE.
