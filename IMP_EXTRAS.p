    OUTPUT TO C:\EXTRAS_DICIEMBRE_2008.TXT.
     PUT "Extras.Nit;CREDITOS.PAGARE;Extras.Cod_Credito;Extras.Num_Solicitud;Extras.Nro_Cuota;Extras.Estado;Fec_Vcto;Extras.Vr_CuoExtra" SKIP.
    FOR EACH creditos WHERE MONTH(fec_desembolso) = 11 AND 
                        YEAR(fec_desembolso) = 2008 AND estado = 2 NO-LOCK:

   FOR EACH extras WHERE creditos.num_solicitud = extras.num_solicitud AND
                         creditos.nit           = extras.nit NO-LOCK:

       IF MONTH(FEC_VCTO) = 12 AND YEAR(FEC_VCTO) = 2008 THEN DO:
       
       PUT Extras.Nit ";"       
             CREDITOS.PAGARE ";"
             Extras.Cod_Credito ";"
             Extras.Num_Solicitud  ";"
             Extras.Nro_Cuota         ";"
             Extras.Estado               ";"
             Fec_Vcto                       ";"
             Extras.Vr_CuoExtra  SKIP.
       END.
   END.
END.
