TRIGGER PROCEDURE FOR DELETE OF Pro_Ahorros.

   FOR FIRST Ahorros WHERE (
      Ahorros.Cod_ahorro = Pro_Ahorros.Cod_Ahorro) NO-LOCK:
    MESSAGE "No puede Borrar 'Pro_Ahorros' Porque 'Ahorros' EXISTE".
    RETURN ERROR.
  END.





