FOR EACH carteravencida WHERE cod_producto = 108 AND Per_Final <= 30:
    UPDATE carteravencida WITH 1 COL.
END.

/*DISABLE TRIGGERS FOR LOAD OF CarteraVencida.

DEFINE TEMP-TABLE ttcv LIKE carteravencida.
    
FOR EACH carteravencida WHERE cod_producto = 114 AND Per_Final <= 30:
    CREATE ttcv.
    BUFFER-COPY carteraVencida TO ttcv.
    ttcv.per_inicial = 0.
    ttcv.per_final = 0.
    UPDATE carteraVencida WITH 1 COL.
END.

FOR EACH ttcv NO-LOCK:
    CREATE carteraVencida.
    BUFFER-COPY ttcv TO carteraVencida.
END.*/
