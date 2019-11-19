DEFINE TEMP-TABLE tt LIKE mov_contable.
        
FOR EACH mov_contable WHERE agencia = 4
                        AND fec_contable = 09/30/2011
                        AND num_documento = 443
                        AND cr = 151790061 NO-LOCK:
    DISPLAY mov_contable WITH WIDTH 200 1 COL.
        
    CREATE tt.
    BUFFER-COPY mov_contable TO tt.
    tt.cr = 33047376.
END.
    
FOR EACH tt:
    CREATE mov_contable.
    BUFFER-COPY tt TO mov_contable.
END.
