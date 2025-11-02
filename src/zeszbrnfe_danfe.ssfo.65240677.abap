CLEAR: V_IMG_CANHOTO, V_GRAPHIC_CANHOTO.

"BREAK-POINT.

concatenate 'CANHOTO_DANFE_' I_CABECALHO-CHAVE+6(8) into V_IMG_CANHOTO.

select single tdname
  from stxbitmaps into V_GRAPHIC_CANHOTO
 where tdobject eq 'GRAPHICS'
  and tdname    eq V_IMG_CANHOTO
  and tdid      eq 'BMAP'
  and tdbtype   eq 'BMON'.























