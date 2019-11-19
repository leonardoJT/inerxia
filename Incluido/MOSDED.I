/*************************************************************************
 
**************************************************************************/
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST Deducible WHERE {1} = Deducible.Cod_Deducible AND
                                     Deducible.Estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Deducible) THEN
       ASSIGN {2}:SCREEN-VALUE = Deducible.Nom_Deducible.
  END.       
