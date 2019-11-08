FOR EACH mov_contable WHERE comprobante = 10 AND
    (num_documento = 0200432 OR num_documento = 0200431) /*0200331*/ /*0200432*/
    AND nit = "13811384" NO-LOCK:
    DISPLAY agencia nit fec_contable cuenta Num_Documento enlace db cr.
END.
