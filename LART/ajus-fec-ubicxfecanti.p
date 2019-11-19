

FOR EACH credito WHERE fec_pago EQ ? AND estado EQ 2 AND agencia EQ 2  :
    DISPLAY agencia nit cod_credito num_credito Creditos.Fec_PagAnti
        WITH WIDTH 300.
     ASSIGN fec_pago =Creditos.Fec_PagAnti. 
END.


