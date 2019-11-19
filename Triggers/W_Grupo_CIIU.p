TRIGGER PROCEDURE FOR WRITE OF Grupo_CIIU
NEW inserted OLD deleted.


  IF
    deleted.Grupo <> inserted.Grupo
  THEN DO:
    FOR FIRST Subgrupo_CIIU WHERE (
        Subgrupo_CIIU.Grupo = deleted.Grupo) NO-LOCK:
      MESSAGE "No puede Actualizar 'Grupo_CIIU' Porque 'Subgrupo_CIIU' EXISTE".
      RETURN ERROR.
    END.
  END.


  IF
    deleted.Grupo <> inserted.Grupo
  THEN DO:
    FOR FIRST Ciiu WHERE (
        Ciiu.Grupo = deleted.Grupo) NO-LOCK:
      MESSAGE "No puede Actualizar 'Grupo_CIIU' Porque 'Ciiu' EXISTE".
      RETURN ERROR.
    END.
  END.


{valida/W_Grupo_CIIU.v}
