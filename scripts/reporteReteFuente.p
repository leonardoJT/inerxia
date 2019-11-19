DEFINE TEMP-TABLE docs
    FIELD agencia AS INTEGER
    FIELD 
    FIELD cuenta AS CHARACTER
    FIELD nit AS CHARACTER
FOR EACH mov_contable WHERE mov_contable.fec_contable >= 01/01/2015
                        AND mov_contable.fec_contable <= 01/31/2015
                        AND SUBSTRING(mov_contable.cuenta,1,6) = "244515" NO-LOCK BREAK BY mov_contable.cuenta
                                                                                          BY mov_contable.nit:
    IF
END.
