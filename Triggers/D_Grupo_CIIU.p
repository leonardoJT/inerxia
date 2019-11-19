TRIGGER PROCEDURE FOR DELETE OF Grupo_CIIU.
FOR FIRST Subgrupo_CIIU WHERE (
     Subgrupo_CIIU.Grupo = Grupo_CIIU.Grupo) NO-LOCK:
    MESSAGE "No puede Borrar 'Grupo_CIIU' Porque 'Subgrupo_CIIU' EXISTE".
    RETURN ERROR.
  END.

  FOR FIRST Ciiu WHERE (
     Ciiu.Grupo = Grupo_CIIU.Grupo) NO-LOCK:
    MESSAGE "No puede Borrar 'Grupo_CIIU' Porque 'Ciiu' EXISTE".
    RETURN ERROR.
  END.

