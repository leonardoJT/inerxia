DISABLE TRIGGERS FOR LOAD OF mov_contable.
    
FOR EACH mov_contable WHERE cuenta = "6140104195" AND YEAR(fec_contable) = 2011
    AND (MONTH(fec_contable) = 1 OR MONTH(fec_contable) = 2):
    DISPLAY mov_contable WITH WIDTH 200 1 COL.
    cuenta = "6140101195".
END.
