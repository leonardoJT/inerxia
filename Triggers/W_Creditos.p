TRIGGER PROCEDURE FOR WRITE OF Creditos
/* ERwin Builtin Wed Mar 10 13:49:54 1999 */
/* WRITE trigger on Creditos */
/*EW inserted OLD deleted.*/

/* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Creditos Garantias Garantias ON PARENT UPDATE RESTRICT */
 /*
  IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
    deleted.Agencia <> inserted.Agencia or 
    deleted.Cod_Credito <> inserted.Cod_Credito or 
    deleted.Pagare <> inserted.Pagare
  THEN DO:
    FOR FIRST Garantias WHERE (
        /*  %JoinFKPK(Garantias,deleted," = "," and") */
        Garantias.Agencia = deleted.Agencia and
        Garantias.Cod_Credito = deleted.Cod_Credito and
        Garantias.Pagare = deleted.Pagare) NO-LOCK:
      MESSAGE "No puede Actualizar 'Creditos' Porque 'Garantias' EXISTE".
   /*   RETURN ERROR.*/
    END.
  END.
*/
  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Creditos Movimiento Créditos Mov_Creditos ON PARENT UPDATE RESTRICT */
/*
  IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
   /* deleted.Agencia <> inserted.Agencia or */
    deleted.Cod_Credito <> inserted.Cod_Credito or 
    deleted.Num_Credito <> inserted.Num_Credito
  THEN DO:
    FOR FIRST Mov_Creditos WHERE (
        /*  %JoinFKPK(Mov_Creditos,deleted," = "," and") */
       /* Mov_Creditos.Agencia = deleted.Agencia and*/
        Mov_Creditos.Cod_Credito = deleted.Cod_Credito and
        Mov_Creditos.Num_Credito = deleted.Num_Credito) NO-LOCK:
      MESSAGE "No puede Actualizar 'Creditos' Porque 'Mov_Creditos' EXISTE".
      RETURN ERROR.
    END.
  END.
*/
  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Clientes Créditos Creditos ON CHILD UPDATE RESTRICT */

  /*  FIND FIRST Clientes WHERE (
        /* %JoinFKPK(inserted,Clientes," = "," and") */
        inserted.Nit = Clientes.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Clientes THEN DO:
      MESSAGE "No puede Actualizar 'Creditos' Porque 'Clientes' NO EXISTE".
      RETURN ERROR.
    END.
    */
  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Terceros Abogado Creditos ON CHILD UPDATE RESTRICT */
    /*IF inserted.Abogado EQ ""
    OR inserted.Abogado EQ ?
    OR inserted.Abogado EQ STRING(0) THEN
       RETURN.

    FIND FIRST Terceros WHERE (
        /* %JoinFKPK(inserted,Terceros," = "," and") */
        inserted.Abogado = Terceros.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Terceros THEN DO:
      MESSAGE "No puede Actualizar 'Creditos' Porque 'Terceros' NO EXISTE".
      RETURN ERROR.
    END.*/




  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Solicitud Solcitud de Crédito Creditos ON CHILD UPDATE RESTRICT */
/*
    FIND FIRST Solicitud WHERE (
        /* %JoinFKPK(inserted,Solicitud," = "," and") */
        inserted.Num_Solicitud = Solicitud.Num_Solicitud) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Solicitud THEN DO:
      MESSAGE "No puede Actualizar 'Creditos' Porque 'Solicitud' NO EXISTE".
      RETURN ERROR.
    END.
 */
  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Pro_Creditos Créditos Creditos ON CHILD UPDATE RESTRICT */

 /*   FIND FIRST Pro_Creditos WHERE (
        /* %JoinFKPK(inserted,Pro_Creditos," = "," and") */
        inserted.Cod_Credito = Pro_Creditos.Cod_Credito) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Pro_Creditos THEN DO:
      MESSAGE "No puede Actualizar 'Creditos' Porque 'Pro_Creditos' NO EXISTE".
      RETURN ERROR.
    END.
   */

/* ERwin Builtin Wed Mar 10 13:49:54 1999 */

/*{valida/W_Creditos.v}
                       */
