DISABLE TRIGGERS FOR LOAD OF +mov_contable.
FOR EACH mov_contable:
    IF fec_contable LT DATE(01,20,2008) THEN
       DELETE mov_contable.
END.7-0-
