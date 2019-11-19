TRIGGER PROCEDURE FOR WRITE OF Subgrupo_CIIU
NEW inserted OLD deleted.
IF
    deleted.Grupo <> inserted.Grupo or 
    deleted.Subgrupo <> inserted.Subgrupo
  THEN DO:
    FOR FIRST Ciiu WHERE (
        Ciiu.Grupo = deleted.Grupo and
        Ciiu.Subgrupo = deleted.Subgrupo) NO-LOCK:
      MESSAGE "No puede Actualizar 'Subgrupo_CIIU' Porque 'Ciiu' EXISTE".
      RETURN ERROR.
    END.
  END.


    FIND FIRST Grupo_CIIU WHERE (
        inserted.Grupo = Grupo_CIIU.Grupo) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Grupo_CIIU THEN DO:
      MESSAGE "No puede Actualizar 'Subgrupo_CIIU' Porque 'Grupo_CIIU' NO EXISTE".
      RETURN ERROR.
    END.


{valida/W_Subgrupo_CIIU.v}
