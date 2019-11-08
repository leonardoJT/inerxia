  DEFINE VAR Cont AS INTEGER INITIAL 0.  
  FIND FIRST Cfg_Varios NO-LOCK NO-ERROR.  
  OPEN QUERY B-Indices FOR EACH Varios WHERE Cfg_Varios.Tipo = Varios.Tipo NO-LOCK.
  FOR EACH Varios WHERE Cfg_Varios.Tipo = Varios.Tipo NO-LOCK:  
    Cont = Cont + 1.
  END.
  Cont = Cont + 1.
  FIND FIRST Varios NO-LOCK NO-ERROR.
  GET FIRST B-Indices.
  DO WHILE Cont NE 0: 
    /*APPLY 'Mouse-Select-Dblclick' TO B-Indices. 
    REPOSITION B-Indices FORWARDS -1.    
    GET NEXT B-Indices.*/
    cont = cont - 1.    
  END.
