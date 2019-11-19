TRIGGER PROCEDURE FOR DELETE OF Creditos.
/* ERwin Builtin Wed Mar 10 13:49:54 1999 */
/* DELETE trigger on Creditos */

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Creditos R/107 Atrasos ON PARENT DELETE RESTRICT */

 /* FOR FIRST Atrasos WHERE (
      /*  %JoinFKPK(Atrasos,Creditos," = "," and") */
      Atrasos.Oficina = Creditos.Oficina and
      Atrasos.Cod_Producto = Creditos.Cod_Producto and
      Atrasos.Pagare = Creditos.Pagare) NO-LOCK:
    MESSAGE "No puede Borrar 'Creditos' Porque 'Atrasos' EXISTE".
    RETURN ERROR.
  END.

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Creditos Garantias Garantias ON PARENT DELETE RESTRICT */

  FOR FIRST Garantias WHERE (
      /*  %JoinFKPK(Garantias,Creditos," = "," and") */
      Garantias.Oficina = Creditos.Oficina and
      Garantias.Cod_Producto = Creditos.Cod_Producto and
      Garantias.Pagare = Creditos.Pagare) NO-LOCK:
    MESSAGE "No puede Borrar 'Creditos' Porque 'Garantias' EXISTE".
    RETURN ERROR.
  END.

  /* ERwin Builtin Wed Mar 10 13:49:54 1999 */
  /* Creditos Movimiento Créditos Mov_Creditos ON PARENT DELETE RESTRICT */

  FOR FIRST Mov_Creditos WHERE (
      /*  %JoinFKPK(Mov_Creditos,Creditos," = "," and") */
      /*Mov_Creditos.Oficina = Creditos.Oficina and*/
      Mov_Creditos.Cod_Producto = Creditos.Cod_Producto and
      Mov_Creditos.Pagare = Creditos.Pagare) NO-LOCK:
    MESSAGE "No puede Borrar 'Creditos' Porque 'Mov_Creditos' EXISTE".
    RETURN ERROR.
  END.
*/

/* ERwin Builtin Wed Mar 10 13:49:54 1999 */


