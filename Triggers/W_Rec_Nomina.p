TRIGGER PROCEDURE FOR WRITE OF Rec_Nomina
/* ERwin Builtin Fri Apr 30 16:06:00 1999 */
/* WRITE trigger on Rec_Nomina */
/*NEW inserted OLD deleted.*/

  /* ERwin Builtin Fri Apr 30 16:06:00 1999 */
  /* Rec_Nomina Detalle.Recaudo Recaudos ON PARENT UPDATE RESTRICT */

 /* IF
    /* %JoinPKPK(deleted,inserted," <> "," or ") */
    deleted.Oficina <> inserted.Oficina or 
    deleted.Cod_Empresa <> inserted.Cod_Empresa or 
    deleted.Nro_Pago <> inserted.Nro_Pago or 
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Recaudos WHERE (
        /*  %JoinFKPK(Recaudos,deleted," = "," and") */
        Recaudos.Oficina = deleted.Oficina and
        Recaudos.Cod_Empresa = deleted.Cod_Empresa and
        Recaudos.Nro_Pago = deleted.Nro_Pago and
        Recaudos.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Rec_Nomina' Porque 'Recaudos' EXISTE".
      RETURN ERROR.
    END.
  END.
   */
  /* ERwin Builtin Fri Apr 30 16:06:00 1999 */
  /* Clientes Clientes Rec_Nomina ON CHILD UPDATE RESTRICT */

/*   FIND FIRST Clientes WHERE (
        /* %JoinFKPK(inserted,Clientes," = "," and") */
        inserted.Nit = Clientes.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Clientes THEN DO:
      MESSAGE "No puede Actualizar 'Rec_Nomina' Porque 'Clientes' NO EXISTE".
      RETURN ERROR.
    END.
  */
  /* ERwin Builtin Fri Apr 30 16:06:00 1999 */
  /* Empresas Recaudos Nómina Rec_Nomina ON CHILD UPDATE RESTRICT */

 /*   FIND FIRST Empresas WHERE (
        /* %JoinFKPK(inserted,Empresas," = "," and") */
        inserted.Oficina = Empresas.Oficina and
        inserted.Cod_Empresa = Empresas.Cod_Empresa) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Empresas THEN DO:
      MESSAGE "No puede Actualizar 'Rec_Nomina' Porque 'Empresas' NO EXISTE".
      RETURN ERROR.
    END.*/


/* ERwin Builtin Fri Apr 30 16:06:00 1999 */
/*
{valida/W_Rec_Nomina.v}
  */
