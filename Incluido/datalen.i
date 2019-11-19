/* datalen.i */

if _data-type = "character" then
  assign datalength = length (string ("a",_format))
         datalength = datalength + if datalength > 240 then 3 else 1.
else if _data-type = "integer" or
        _data-type = "recid" or
        _data-type = "rowid" then
        assign datalength = length (string (1, _format))
        datalength = if datalength >= 10 then 5
                else if datalength >=  9 then 4
                else if datalength >=  6 then 3
                else if datalength >=  3 then 2
                else 1.
      else if _data-type = "decimal" then
              assign datalength = length (string (1, _format))
                     datalength = 2 + (datalength + 1) / 2.
           else if _data-type = "date" then datalength = 3.
                else if _data-type = "logical" then datalength = (if _initial = "yes" then 2 else 1).
datalength = if _extent > 0 then datalength * _extent else datalength.
