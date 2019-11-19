TRIGGER PROCEDURE FOR DELETE OF Bancos.


  FOR FIRST Che_Transito WHERE (
      Che_Transito.Cod_Compensa = Bancos.Cod_Compensa) NO-LOCK:
    MESSAGE "No puede Borrar 'Bancos' Porque 'Che_Transito' EXISTE".
    RETURN ERROR.
  END.





