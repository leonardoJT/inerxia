TRIGGER PROCEDURE FOR WRITE OF Clientes
/* ERwin Builtin Wed Mar 10 13:49:54 1999 */
/* WRITE trigger on Clientes */
NEW inserted OLD deleted.

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Clientes R/111 Capacitacion ON PARENT UPDATE RESTRICT */

/*  IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Capacitacion WHERE (
        /*  %JoinFKPK(Capacitacion,deleted," = "," and") */
        Capacitacion.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Capacitacion' EXISTE".
      RETURN ERROR.
    END.
  END.

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Clientes Clientes Rec_Nomina ON PARENT UPDATE RESTRICT */

  IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Rec_Nomina WHERE (
        /*  %JoinFKPK(Rec_Nomina,deleted," = "," and") */
        Rec_Nomina.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Rec_Nomina' EXISTE".
      RETURN ERROR.
    END.
  END.

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Clientes Solicitud Crédito Solicitud ON PARENT UPDATE RESTRICT */

  IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Solicitud WHERE (
        /*  %JoinFKPK(Solicitud,deleted," = "," and") */
        Solicitud.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Solicitud' EXISTE".
      RETURN ERROR.
    END.
  END.

   /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Clientes Codeudor Garantias ON PARENT UPDATE RESTRICT */
    IF  deleted.Nit NE ""
    AND deleted.Nit NE ?
    AND deleted.Nit NE STRING(0) THEN DO:
 
IF
    /* deleted.Nit <> inserted.Nit */

    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Garantias WHERE (
        /*  Garantias.Nit_Codeudor = deleted.Nit */
        Garantias.Nit_Codeudor = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Garantias' EXISTE".
      RETURN ERROR.
    END.
  END.
  END.

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Clientes Créditos Creditos ON PARENT UPDATE RESTRICT */

  IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Creditos WHERE (
        /*  %JoinFKPK(Creditos,deleted," = "," and") */
        Creditos.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Creditos' EXISTE".
      RETURN ERROR.
    END.
  END.

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Clientes Hoja de Vida Productos Hoja_Pro ON PARENT UPDATE RESTRICT */

  IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Hoja_Pro WHERE (
        /*  %JoinFKPK(Hoja_Pro,deleted," = "," and") */
        Hoja_Pro.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Hoja_Pro' EXISTE".
      RETURN ERROR.
    END.
  END.

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Clientes Operaciones Realizadas  por Cliente Mov_Ope ON PARENT UPDATE RESTRICT */

  /*IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Mov_Ope WHERE (
        /*  %JoinFKPK(Mov_Ope,deleted," = "," and") */
        Mov_Ope.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Mov_Ope' EXISTE".
      RETURN ERROR.
    END.
  END.*/

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Clientes Ahorros de Clientes Ahorros ON PARENT UPDATE RESTRICT */

  IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Ahorros WHERE (
        /*  %JoinFKPK(Ahorros,deleted," = "," and") */
        Ahorros.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Ahorros' EXISTE".
      RETURN ERROR.
    END.
  END.

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Clientes Referencias Ref_Ingreso ON PARENT UPDATE RESTRICT */

  IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Ref_Ingreso WHERE (
        /*  %JoinFKPK(Ref_Ingreso,deleted," = "," and") */
        Ref_Ingreso.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Ref_Ingreso' EXISTE".
      RETURN ERROR.
    END.
  END.

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Clientes Bienes Cliente Bienes ON PARENT UPDATE RESTRICT */

  /*IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Bienes WHERE (
        /*  %JoinFKPK(Bienes,deleted," = "," and") */
        Bienes.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Bienes' EXISTE".
      RETURN ERROR.
    END.
  END.*/

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Clientes Datos Asociados Asociados ON PARENT UPDATE RESTRICT */

  IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Asociados WHERE (
        /*  %JoinFKPK(Asociados,deleted," = "," and") */
        Asociados.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Asociados' EXISTE".
      RETURN ERROR.
    END.
  END.

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Clientes Datos P.Jurídicas Inf_Juridica ON PARENT UPDATE RESTRICT */

  IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Inf_Juridica WHERE (
        /*  %JoinFKPK(Inf_Juridica,deleted," = "," and") */
        Inf_Juridica.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Inf_Juridica' EXISTE".
      RETURN ERROR.
    END.
  END.

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Clientes Datos  Actividad Inf_Laboral ON PARENT UPDATE RESTRICT */

  /*IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Inf_Laboral WHERE (
        /*  %JoinFKPK(Inf_Laboral,deleted," = "," and") */
        Inf_Laboral.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Inf_Laboral' EXISTE".
      RETURN ERROR.
    END.
  END.*/

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Clientes Familiares Cliente Inf_Familiar ON PARENT UPDATE RESTRICT */

  IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Inf_Familiar WHERE (
        /*  %JoinFKPK(Inf_Familiar,deleted," = "," and") */
        Inf_Familiar.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Inf_Familiar' EXISTE".
      RETURN ERROR.
    END.
  END.

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Clientes Seguimiento Eventos Hoja_Vida ON PARENT UPDATE RESTRICT */

  /*IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Hoja_Vida WHERE (
        /*  %JoinFKPK(Hoja_Vida,deleted," = "," and") */
        Hoja_Vida.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Hoja_Vida' EXISTE".
      RETURN ERROR.
    END.
  END.*/

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Ciiu Código CIIU Clientes ON CHILD UPDATE RESTRICT */

    FIND FIRST Ciiu WHERE (
        /* %JoinFKPK(inserted,Ciiu," = "," and") */
        inserted.Codigo_CIIU = Ciiu.Codigo_CIIU) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Ciiu THEN DO:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Ciiu' NO EXISTE".
      RETURN ERROR.
    END.

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Terceros Clientes Clientes ON CHILD UPDATE RESTRICT */

    FIND FIRST Terceros WHERE (
        /* %JoinFKPK(inserted,Terceros," = "," and") */
        inserted.Nit = Terceros.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Terceros THEN DO:
      MESSAGE "No puede Actualizar 'Clientes' Porque 'Terceros' NO EXISTE".
      RETURN ERROR.
    END.*/


/* ERwin Builtin Wed Mar 10 13:49:54 1999 */
