DEFINE VAR wcuotaa AS INTEG NO-UNDO.
FOR EACH creditos WHERE Creditos.Cuo_Pagadas GE 90  :
    ASSIGN wcuotaa = (Creditos.Cuo_Pagadas / 30 ).
    DISPLAY wcuotaa WITH 1 COL.
    ASSIGN Creditos.Cuo_Pagadas = wcuotaa.
    
END.
