/*Programa UtiCred_2.P...Créditos con plazo total cumplido"
  Julio 7/05 GAER*/

DEFI VAR Tot AS INTEG FORM "9999999" LABEL "No.Total : ".

OUTPUT TO C:\InfRed\CrPlazo_TotVdo.Txt.

DISPLAY "COOP.BELEN     -    Créditos con plazo Total Cumplido - " +
        STRING (TODAY,"99/99/9999") FORM "X(150)" SKIP(1) 
    WITH WIDTH 200 FRAME FEnc NO-LABELS.

FOR EACH creditos WHERE creditos.sdo_capit GT 0     AND
                       ( Capital_Acum      GE Monto OR Sdo_proyect LE 0)
                       NO-LOCK:
    Tot = Tot + 1.
    DISPLAY Agencia     LABEL "Ag."
            Nit         LABEL "Ced/Nit"
            Cod_Cred    LABEL "Pdcto"
            Num_credito LABEL "No.Crédito"
            Fec_Desem   LABEL "F-Desemb."
            Fec_UltPago LABEL "F-UltPago"
            Fec_pago    LABEL "F-Px.Pago"
            For_pago    LABEL "FP"
            Per_Pago    LABEL "PP"
            Plazo       LABEL "Plazo"
            Cuo_pagad   LABEL "C.Pagas"
            Monto       LABEL "Monto Desembolso" FORM "->>>>,>>>,>>9"
            Cuota       LABEL "Vr.de Cuota"      FORM "->>>,>>>,>>9"
            Sdo_Capital LABEL "Sdo.de Capital"   FORM "->>>>,>>>,>>9"
            INT_Corrien + Int_DifCobro     LABEL "Ctes+Ctes.DC"      FORM "->>>>,>>>,>>9"
            Int_MoraDifCob + Int_MorCobrar LABEL "Mora+Mora.DC"      FORM "->>>>,>>>,>>9"
            Polizas + Honorarios + Costas  LABEL "X Cobro Jurídico"  FORM "->>>>,>>>,>>9"
            Sdo_Capital + INT_Corrien + Int_DifCobro + Int_MoraDifCob + Int_MorCobrar +
            Polizas + Honorarios + Costas - Int_Anticipado LABEL "Vr.Total Deuda" FORM "->>>>,>>>,>>9"
        WITH DOWN WIDTH 400 FRAME F1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.
END.
DISPLAY Tot.
OUTPUT CLOSE.

