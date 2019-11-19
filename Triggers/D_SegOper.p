TRIGGER PROCEDURE FOR DELETE OF Seg_Oper.
  
  FOR FIRST Clientes WHERE Clientes.Cod_Segmento       EQ Seg_Oper.Cod_Segmento AND
                           YEAR(Clientes.Fec_Ingreso)  EQ Seg_Oper.Ano_Operacion NO-LOCK:
    MESSAGE "No puede Borrar 'Seg_Oper' Porque en 'Clientes' EXISTE".
    RETURN ERROR.
  END.

