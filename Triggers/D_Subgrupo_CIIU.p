TRIGGER PROCEDURE FOR DELETE OF Subgrupo_CIIU.

  FOR FIRST Ciiu WHERE (
      Ciiu.Grupo = Subgrupo_CIIU.Grupo and
      Ciiu.Subgrupo = Subgrupo_CIIU.Subgrupo) NO-LOCK:
    MESSAGE "No puede Borrar 'Subgrupo_CIIU' Porque 'Ciiu' EXISTE".
    RETURN ERROR.
  END.

