TRIGGER PROCEDURE FOR WRITE OF Mov_Creditos
/* ERwin Builtin Wed Mar 10 13:49:56 1999 */
/* WRITE trigger on Mov_Creditos */
/*NEW inserted OLD deleted.*/

  /* ERwin Builtin Wed Mar 10 13:49:56 1999 */
  /* Operacion Operaciones Afectada Mov_Creditos ON CHILD UPDATE RESTRICT */

/*    FIND FIRST Operacion WHERE (
        /* %JoinFKPK(inserted,Operacion," = "," and") */
        inserted.Cod_Operacion = Operacion.Cod_Operacion) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Operacion THEN DO:
      MESSAGE "No puede Actualizar 'Mov_Creditos' Porque 'Operacion' " inserted.Cod_Operacion " NO EXISTE".
      RETURN ERROR.
    END.     */

  /* ERwin Builtin Wed Mar 10 13:49:56 1999 */
  /* Creditos Movimiento Créditos Mov_Creditos ON CHILD UPDATE RESTRICT */

   /* FIND FIRST Creditos WHERE (
        /* %JoinFKPK(inserted,Creditos," = "," and") */
        /*inserted.Agencia = Creditos.Agencia and*/
        inserted.Cod_Credito = Creditos.Cod_Credito and
        inserted.Num_Credito = Creditos.Num_Credito) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Creditos THEN DO:
      MESSAGE "No puede Actualizar 'Mov_Creditos' Porque 'Creditos' NO EXISTE".
      RETURN ERROR.
    END.
     */

/* ERwin Builtin Wed Mar 10 13:49:56 1999 */

/*{valida/W_Mov_Creditos.v}*/
