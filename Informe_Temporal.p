DEFINE VAR Apo LIKE Ahorros.Sdo_Disponible.
DEFINE VAR Gar LIKE Ahorros.Sdo_Disponible.
DEFINE VAR Cla AS CHARACTER FORMAT "X(12)".

OUTPUT TO "C:\sicobel\informe_Creditos_Temporal".
DISPLAY
 "Nit          Clasificacion          Sdo.Capital        Int.Corrientes               Aportes             Garantias D.Atr Calif   Provision"       
 WITH FRAME Fenc WIDTH 150.
FOR EACH Creditos WHERE creditos.sdo_Capital GT 0 NO-LOCK BREAK BY Creditos.Nit:
    IF FIRST-OF(Creditos.Nit) THEN DO:
       Apo = 0.
       FOR EACH Ahorros WHERE  
                Ahorros.Tip_Ahorro EQ 4 AND
                Ahorros.Nit        EQ Creditos.Nit NO-LOCK:
           Apo = Apo + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje.
       END.
    END.
    Gar = 0.
    FOR EACH Garantias WHERE
             Garantias.Cod_Credito EQ Creditos.Cod_Credito AND
             Garantias.Tip_Credito EQ Creditos.Tip_Credito AND
             Garantias.Num_Credito EQ Creditos.Num_Credito AND
             Garantias.Estado      EQ 1 NO-LOCK:
       Gar = Gar + Garantias.Val_Bien.
    END.
    CASE Creditos.Tip_Credito:
       WHEN 1 THEN Cla = "Consumo".
       WHEN 2 THEN Cla = "Comercial".
       WHEN 3 THEN Cla = "Hipotecario".
       WHEN 4 THEN Cla = "Microcredito".
    END CASE.
    DISPLAY
       Creditos.Nit
       Cla
       Creditos.Sdo_Capital
       Creditos.Int_Corrientes
       Apo
       Gar
       Creditos.Dias_Atraso
       Creditos.Categoria
       Creditos.Provision
    WITH FRAME FF WIDTH 150 STREAM-IO NO-LABELS USE-TEXT NO-BOX.
END.
OUTPUT CLOSE.
