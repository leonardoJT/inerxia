FOR EACH mov_cont_mult WHERE fec_contable LT TODAY:
    DISP fec_contable cuenta.
    DELETE mov_cont_mult.
END.
