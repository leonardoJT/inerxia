TRIGGER PROCEDURE FOR DELETE OF Rec_Nomina.
/* ERwin Builtin Wed Mar 10 13:49:58 1999 */
/* DELETE trigger on Rec_Nomina */

  /* ERwin Builtin Wed Mar 10 13:49:58 1999 */
  /* Rec_Nomina Detalle.Recaudo Recaudos ON PARENT DELETE RESTRICT */

 /* FOR FIRST Recaudos WHERE (
      /*  %JoinFKPK(Recaudos,Rec_Nomina," = "," and") */
      Recaudos.Oficina = Rec_Nomina.Oficina and
      Recaudos.Cod_Empresa = Rec_Nomina.Cod_Empresa and
      Recaudos.Nro_Pago = Rec_Nomina.Nro_Pago and
      Recaudos.Nit = Rec_Nomina.Nit) NO-LOCK:
    MESSAGE "No puede Borrar 'Rec_Nomina' Porque 'Recaudos' EXISTE".
    RETURN ERROR.
  END.
   */

/* ERwin Builtin Wed Mar 10 13:49:58 1999 */


