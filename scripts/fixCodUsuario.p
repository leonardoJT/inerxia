DISABLE TRIGGERS FOR LOAD OF mov_contable.
DISABLE TRIGGERS FOR LOAD OF usuarios.

DEFINE VAR codUsu AS CHARACTER.

FOR EACH usuarios WHERE usuarios.usuario = "22":
    codUsu = usuarios.usuario.

    UPDATE usuarios.nombre
           usuarios.usuario FORMAT "X(25)" WITH 1 COL.

    FOR EACH mov_contable WHERE mov_contable.usuario = codUsu:
        mov_contable.usuario = usuarios.usuario.
    END.
END.

MESSAGE "Finalizó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
