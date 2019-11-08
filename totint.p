DEFINE VARIABLE totint AS DECIMAL.
OUTPUT TO c:\intereses_2007.txt.
FOR EACH mov_creditos WHERE cod_operacion = 020101003 BREAK BY nit:
    IF year(fecha) NE 2007 THEN NEXT.
    totint = totint + (Val_Efectivo + val_cheque).
    IF LAST-OF(nit) THEN DO:
        put Nit ";" Num_Credito ";" Fecha ";" Cpte ";" Descrip  ";" totint FORMAT ">>>,>>>,>>>,>>>" SKIP.
        totint = 0.
    END.
END.
OUTPUT CLOSE.
