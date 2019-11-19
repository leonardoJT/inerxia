  DEFINE VAR adm-current-page AS INTEGER NO-UNDO.
  
  RUN Get-attribute IN THIS-PROCEDURE ('current-page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).