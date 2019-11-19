OUTPUT TO d:\Leonardo\sinNit.txt.
FOR EACH mov_contable WHERE cuenta = "16989501"
                        AND nit = ""
                        AND agencia = 1 NO-LOCK:
    DISPLAY fec_contable comprobante num_documento cuenta comentario db cr WITH WIDTH 300.
END.
