DISABLE TRIGGERS FOR LOAD OF usuarios.

FOR EACH usuarios NO-LOCK BY usuarios.usuario:
    DISPLAY nombre usuarios.usuario.
END.

FOR EACH usuarios WHERE usuario = "9":
    UPDATE usuarios.usuario FORMAT "X(20)" usuarios.nombre.
    /*DELETE usuarios.*/
END.
  
