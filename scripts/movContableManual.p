DEFINE VAR vAgencia AS INTEGER INITIAL 2.
DEFINE VAR vComprobante AS INTEGER INITIAL 4.
DEFINE VAR vComentario AS CHARACTER INITIAL "AjusteContable".
DEFINE VAR vSec AS INTEGER.
DEFINE VAR vFecha AS DATE INITIAL TODAY.

FIND FIRST comprobantes WHERE comprobantes.agencia = vAgencia
                          AND comprobantes.comprobante = vComprobante NO-ERROR.

comprobantes.secuencia = comprobantes.secuencia + 1.
vSec = comprobantes.secuencia.
/*vSec = 86.*/

CREATE mov_contable.
mov_contable.agencia = vAgencia.
mov_contable.comprobante = vComprobante.
mov_contable.num_documento = vSec.
mov_contable.fec_contable = vFecha.
mov_contable.fec_grabacion = TODAY.
mov_contable.cen_costos = 999.
mov_contable.usuario = "desarrollo".
mov_contable.estacion = "000005".
mov_contable.destino = vAgencia.
mov_contable.comentario = vComentario.

UPDATE mov_contable WITH WIDTH 300 1 COL.

CREATE mov_contable.
mov_contable.agencia = vAgencia.
mov_contable.comprobante = vComprobante.
mov_contable.num_documento = vSec.
mov_contable.fec_contable = vFecha.
mov_contable.fec_grabacion = TODAY.
mov_contable.cen_costos = 999.
mov_contable.usuario = "desarrollo".
mov_contable.estacion = "000005".
mov_contable.destino = vAgencia.
mov_contable.comentario = vComentario.

UPDATE mov_contable WITH WIDTH 300 1 COL.

/*FOR EACH activosFijos WHERE idActivo = mov_contable.nit:
    UPDATE activosFijos WITH WIDTH 320 1 COL.
END.*/


/*CREATE mov_contable.
mov_contable.agencia = vAgencia.
mov_contable.comprobante = vComprobante.
mov_contable.num_documento = vSec.
mov_contable.fec_contable = vFecha.
mov_contable.fec_grabacion = TODAY.
mov_contable.cen_costos = 999.
mov_contable.usuario = "desarrollo".
mov_contable.estacion = "000005".
mov_contable.destino = vAgencia.
mov_contable.comentario = vComentario.
UPDATE mov_contable WITH WIDTH 300 1 COL.

CREATE mov_contable.
mov_contable.agencia = vAgencia.
mov_contable.comprobante = vComprobante.
mov_contable.num_documento = vSec.
mov_contable.fec_contable = vFecha.
mov_contable.fec_grabacion = TODAY.
mov_contable.cen_costos = 999.
mov_contable.usuario = "desarrollo".
mov_contable.estacion = "000005".
mov_contable.destino = vAgencia.
mov_contable.comentario = vComentario.
UPDATE mov_contable WITH WIDTH 300 1 COL.


CREATE mov_contable.
mov_contable.agencia = vAgencia.
mov_contable.comprobante = vComprobante.
mov_contable.num_documento = vSec.
mov_contable.fec_contable = vFecha.
mov_contable.fec_grabacion = TODAY.
mov_contable.cen_costos = 999.
mov_contable.usuario = "desarrollo".
mov_contable.estacion = "000005".
mov_contable.destino = vAgencia.
mov_contable.comentario = vComentario.
UPDATE mov_contable WITH WIDTH 300 1 COL.

CREATE mov_contable.
mov_contable.agencia = vAgencia.
mov_contable.comprobante = vComprobante.
mov_contable.num_documento = vSec.
mov_contable.fec_contable = vFecha.
mov_contable.fec_grabacion = TODAY.
mov_contable.cen_costos = 999.
mov_contable.usuario = "desarrollo".
mov_contable.estacion = "000005".
mov_contable.destino = vAgencia.
mov_contable.comentario = vComentario.
UPDATE mov_contable WITH WIDTH 300 1 COL.


/*CREATE mov_contable.
mov_contable.agencia = vAgencia.
mov_contable.comprobante = vComprobante.
mov_contable.num_documento = vSec.
mov_contable.fec_contable = vFecha.
mov_contable.fec_grabacion = TODAY.
mov_contable.cen_costos = 999.
mov_contable.usuario = "desarrollo".
mov_contable.estacion = "000005".
mov_contable.destino = vAgencia.
mov_contable.comentario = vComentario.
UPDATE mov_contable WITH WIDTH 300 1 COL.

CREATE mov_contable.
mov_contable.agencia = vAgencia.
mov_contable.comprobante = vComprobante.
mov_contable.num_documento = vSec.
mov_contable.fec_contable = vFecha.
mov_contable.fec_grabacion = TODAY.
mov_contable.cen_costos = 999.
mov_contable.usuario = "desarrollo".
mov_contable.estacion = "000005".
mov_contable.destino = vAgencia.
mov_contable.comentario = vComentario.
UPDATE mov_contable WITH WIDTH 300 1 COL.


CREATE mov_contable.
mov_contable.agencia = vAgencia.
mov_contable.comprobante = vComprobante.
mov_contable.num_documento = vSec.
mov_contable.fec_contable = vFecha.
mov_contable.fec_grabacion = TODAY.
mov_contable.cen_costos = 999.
mov_contable.usuario = "desarrollo".
mov_contable.estacion = "000005".
mov_contable.destino = vAgencia.
mov_contable.comentario = vComentario.
UPDATE mov_contable WITH WIDTH 300 1 COL.

CREATE mov_contable.
mov_contable.agencia = vAgencia.
mov_contable.comprobante = vComprobante.
mov_contable.num_documento = vSec.
mov_contable.fec_contable = vFecha.
mov_contable.fec_grabacion = TODAY.
mov_contable.cen_costos = 999.
mov_contable.usuario = "desarrollo".
mov_contable.estacion = "000005".
mov_contable.destino = vAgencia.
mov_contable.comentario = vComentario.
UPDATE mov_contable WITH WIDTH 300 1 COL.


CREATE mov_contable.
mov_contable.agencia = vAgencia.
mov_contable.comprobante = vComprobante.
mov_contable.num_documento = vSec.
mov_contable.fec_contable = vFecha.
mov_contable.fec_grabacion = TODAY.
mov_contable.cen_costos = 999.
mov_contable.usuario = "desarrollo".
mov_contable.estacion = "000005".
mov_contable.destino = vAgencia.
mov_contable.comentario = vComentario.
UPDATE mov_contable WITH WIDTH 300 1 COL.
*/
*/
