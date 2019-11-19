TRIGGER PROCEDURE FOR WRITE OF Bancos
NEW inserted OLD deleted.
  IF
    deleted.Cod_Compensa <> inserted.Cod_Compensa
  THEN DO:
    FOR FIRST Che_Transito WHERE (
        Che_Transito.Cod_Compensa = deleted.Cod_Compensa) NO-LOCK:
      MESSAGE "No puede Actualizar 'Bancos' Porque 'Che_Transito' EXISTE".
      RETURN ERROR.
    END.
  END.
    IF inserted.Cod_RemNego EQ ""
    OR inserted.Cod_RemNego EQ ?
    OR inserted.Cod_RemNego EQ STRING(0) THEN
       RETURN.

    FIND FIRST Deducible WHERE (
        inserted.Cod_RemNego = Deducible.Cod_Deducible) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Deducible THEN DO:
      MESSAGE "No puede Actualizar 'Bancos' Porque 'Deducible' NO EXISTE".
      RETURN ERROR.
    END.




  /* ERwin Builtin Wed Mar 10 13:49:53 1999 */
  /* Deducible Comisión  X R.Cobro Bancos ON CHILD UPDATE RESTRICT */
    IF inserted.Cod_RemCobro EQ ""
    OR inserted.Cod_RemCobro EQ ?
    OR inserted.Cod_RemCobro EQ STRING(0) THEN
       RETURN.

    FIND FIRST Deducible WHERE (
        /* %JoinFKPK(inserted,Deducible," = "," and") */
        inserted.Cod_RemCobro = Deducible.Cod_Deducible) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Deducible THEN DO:
      MESSAGE "No puede Actualizar 'Bancos' Porque 'Deducible' NO EXISTE".
      RETURN ERROR.
    END.





/* ERwin Builtin Wed Mar 10 13:49:53 1999 */

{valida/W_Bancos.v}
