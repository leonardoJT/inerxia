/* SELECT per_factura,COUNT(*) FROM facturacion GROUP BY per_factura.  */

/* SELECT COUNT(distinct(num_credito)) FROM facturacion  */
/*     WHERE per_factura = 5 AND estado = 1.             */


SELECT COUNT(*) FROM facturacion
    WHERE per_factura = 4 AND estado = 1.

FOR EACH per_factura:
   DISPLAY per_factura WITH 1 COLUMN.
END.
