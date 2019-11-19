/*FOR EACH mov_contable WHERE cuenta = "25159501" AND nit = "" NO-LOCK:
    DISPLAY mov_contable WITH WIDTH 300 1 COL.
END.*/

FOR EACH anexos WHERE cuenta = "24159501"
                  AND nit = "8254563"
                  AND ano = 2013:
    UPDATE anexos WITH 1 COL.
END.
