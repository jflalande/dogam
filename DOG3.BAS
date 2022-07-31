DECLARE SUB ralenti ()
DECLARE SUB boutligne ()
DECLARE SUB testhelpd ()
DECLARE SUB testhelp ()
DECLARE SUB pausedt ()
DECLARE SUB testhelpmenu ()
DECLARE SUB paused ()
DECLARE SUB menuairhelp ()
DECLARE SUB menuhelp ()
DECLARE SUB grandchoixhelp ()
DECLARE SUB present ()
DECLARE SUB sautacrou ()
DECLARE SUB deplace ()
DECLARE SUB bemol ()
DECLARE SUB lecon2 ()
DECLARE SUB lecon ()
DECLARE SUB Soupir ()
DECLARE SUB testons ()
DECLARE SUB expli ()
DECLARE SUB grandchoix ()
DECLARE SUB leon ()
DECLARE SUB description ()
DECLARE SUB apui ()
DECLARE SUB graphique ()
DECLARE SUB pareil ()
DECLARE SUB nul ()
DECLARE SUB feliz ()
DECLARE SUB score ()
DECLARE SUB phrases ()
DECLARE SUB barre ()
DECLARE SUB espacenote ()
DECLARE SUB asar3 ()
DECLARE SUB ASAR4 ()
DECLARE SUB bonn ()
DECLARE SUB attention ()
DECLARE SUB asar2 ()
DECLARE SUB melange ()
DECLARE SUB asar ()
DECLARE SUB geretemp ()
DECLARE SUB horloge ()
DECLARE SUB frere ()
DECLARE SUB menuair ()
DECLARE SUB queuerouge ()
DECLARE SUB queue ()
DECLARE SUB circleair ()
DECLARE SUB ligneair ()
DECLARE SUB air ()
DECLARE SUB apres ()
DECLARE SUB clefa ()
DECLARE SUB clesol ()
DECLARE SUB jouefa ()
DECLARE SUB jouesol ()
DECLARE SUB efface ()
DECLARE SUB notesol ()
DECLARE SUB notefa ()
DECLARE SUB verificationsol ()
DECLARE SUB verificationfa ()
DECLARE SUB choixnotes ()
DECLARE SUB ligne ()
DECLARE SUB Menu ()
DECLARE SUB acroupi ()
DECLARE SUB decor ()
DECLARE SUB decori ()
DECLARE SUB saut ()
DECLARE SUB atten ()
DECLARE SUB gauche ()
DECLARE SUB droite ()
DECLARE SUB bin ()
COMMON SHARED mva, fin, abon, abin, bbon, bbin, mes, col, profil, cle
COMMON SHARED nombre, music, a, b, f, rep, bon, not$, l, g, plus, poin
COMMON SHARED ref, temp, p, inte, plusq, esp, tep, kd, gf, tome$, vol
COMMON SHARED ha$, vc, uti0, uti1, uti2, uti3, uti4, uti5, dez, uti6
COMMON SHARED uti7, uti8, uti9, uti10, uti11, frt, colp, dod, re, mi
COMMON SHARED fa, sol, la, si, dod2, re2, mi2, fa2, sol2, bar, nb
COMMON SHARED points, nom$, primo, rend, porq, sop, sup, pouet, uti12
COMMON SHARED la2, aff, vc2, bem, dictee, cligne, colcli, aide, voiraide
COMMON SHARED adictee, bdictee, adicteer, bdicteer, sens

SUB acroupi
col = 12
bin
atten
col = 0
bin
decori
col = 12
bbin = bbin + mes / 2
CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col

LINE (abin, bbin + 2.5 * mes)-(abin - 1.5 * mes, bbin + 2.5 * mes + .75 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin + 1.5 * mes, bbin + 2.5 * mes + .75 * mes), col
LINE (abin - 1.5 * mes, bbin + 2.5 * mes + .75 * mes)-(abin - 1 * mes, bbin + 2.5 * mes + 1.5 * mes), col
LINE (abin + 1.5 * mes, bbin + 2.5 * mes + .75 * mes)-(abin + 1 * mes, bbin + 2.5 * mes + 1.5 * mes), col
LINE (abin, bbin + mes)-(abin - 1.5 * mes, bbin + 1.5 * mes), col
LINE (abin, bbin + mes)-(abin + 1.5 * mes, bbin + 1.5 * mes), col
atten

col = 0
CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin - 1.5 * mes, bbin + 2.5 * mes + .75 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin + 1.5 * mes, bbin + 2.5 * mes + .75 * mes), col
LINE (abin - 1.5 * mes, bbin + 2.5 * mes + .75 * mes)-(abin - 1 * mes, bbin + 2.5 * mes + 1.5 * mes), col
LINE (abin + 1.5 * mes, bbin + 2.5 * mes + .75 * mes)-(abin + 1 * mes, bbin + 2.5 * mes + 1.5 * mes), col
LINE (abin, bbin + mes)-(abin - 1.5 * mes, bbin + 1.5 * mes), col
LINE (abin, bbin + mes)-(abin + 1.5 * mes, bbin + 1.5 * mes), col
decori

col = 12: bbin = bbin + mes / 2
CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin - 2 * mes, bbin + 2.5 * mes + .5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin + 2 * mes, bbin + 2.5 * mes + .5 * mes), col
LINE (abin - 2 * mes, bbin + 2.5 * mes + .5 * mes)-(abin - 1 * mes, bbin + 2.5 * mes + mes), col
LINE (abin + 2 * mes, bbin + 2.5 * mes + .5 * mes)-(abin + 1 * mes, bbin + 2.5 * mes + mes), col
LINE (abin, bbin + mes)-(abin - 2 * mes, bbin + mes), col
LINE (abin, bbin + mes)-(abin + 2 * mes, bbin + mes), col
atten

col = 0
CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin - 2 * mes, bbin + 2.5 * mes + .5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin + 2 * mes, bbin + 2.5 * mes + .5 * mes), col
LINE (abin - 2 * mes, bbin + 2.5 * mes + .5 * mes)-(abin - 1 * mes, bbin + 2.5 * mes + mes), col
LINE (abin + 2 * mes, bbin + 2.5 * mes + .5 * mes)-(abin + 1 * mes, bbin + 2.5 * mes + mes), col
LINE (abin, bbin + mes)-(abin - 2 * mes, bbin + mes), col
LINE (abin, bbin + mes)-(abin + 2 * mes, bbin + mes), col
decori

col = 12
bbin = bbin - mes / 2
CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin - 1.5 * mes, bbin + 2.5 * mes + .75 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin + 1.5 * mes, bbin + 2.5 * mes + .75 * mes), col
LINE (abin - 1.5 * mes, bbin + 2.5 * mes + .75 * mes)-(abin - 1 * mes, bbin + 2.5 * mes + 1.5 * mes), col
LINE (abin + 1.5 * mes, bbin + 2.5 * mes + .75 * mes)-(abin + 1 * mes, bbin + 2.5 * mes + 1.5 * mes), col
LINE (abin, bbin + mes)-(abin - 1.5 * mes, bbin + 1.5 * mes), col
LINE (abin, bbin + mes)-(abin + 1.5 * mes, bbin + 1.5 * mes), col
atten

col = 0
CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin - 1.5 * mes, bbin + 2.5 * mes + .75 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin + 1.5 * mes, bbin + 2.5 * mes + .75 * mes), col
LINE (abin - 1.5 * mes, bbin + 2.5 * mes + .75 * mes)-(abin - 1 * mes, bbin + 2.5 * mes + 1.5 * mes), col
LINE (abin + 1.5 * mes, bbin + 2.5 * mes + .75 * mes)-(abin + 1 * mes, bbin + 2.5 * mes + 1.5 * mes), col
LINE (abin, bbin + mes)-(abin - 1.5 * mes, bbin + 1.5 * mes), col
LINE (abin, bbin + mes)-(abin + 1.5 * mes, bbin + 1.5 * mes), col
decori

bbin = bbin - mes / 2

END SUB

SUB air
SCREEN 9
CLS
LOCATE 2, 30: COLOR 15: PRINT "Aupräs de ma blonde"
cle = 0: a = 90: esp = 34: plus = 0: vol = 11: nb = 4: bar = 192 + 34 / 2: ref = 110
LINE (0, 80 + plus)-(617, 80 + plus), 3
LINE (0, 90 + plus)-(617, 90 + plus), 3
LINE (0, 100 + plus)-(617, 100 + plus), 3
LINE (0, 110 + plus)-(617, 110 + plus), 3
LINE (0, 120 + plus)-(617, 120 + plus), 3
IF cle = 0 THEN clesol
IF cle = 1 THEN clefa
COLOR 9: LOCATE 7, 9: PRINT "2"
COLOR 9: LOCATE 8, 9: PRINT "4"
bem = 100
bemol
SOUND 349, 9: b = 115: inte = 1: circleair
SOUND 392, 3: b = 110: inte = 1: circleair
'LINE (128, 90)-(128, 85), 11


a = a - esp + esp / 3
'LINE (128, 85)-(128 + esp / 3, 85), 11
SOUND 440, 3: b = 105: inte = 1: circleair
a = a - esp + esp / 3
LINE (128, 90)-(128 + esp / 3 + esp / 3, 80), 11
SOUND 466, 3: b = 100: inte = 1: circleair
a = a - esp + esp / 3

SOUND 440, 9: b = 105: inte = 1: circleair
SOUND 523, 9: b = 95: inte = 1: circleair
barre
SOUND 392, 6: b = 110: inte = 1: circleair
FOR d = 2 TO 0 STEP -1
CIRCLE (234, 110 + plus), d, 11
NEXT

a = a - esp / 2
SOUND 466, 3: b = 100: inte = 1: circleair
LINE (230 + 34 / 2, 84)-(230 + 34 / 2 - 5, 87), 11
LINE (230, 90)-(230 + 34 / 2, 80), 11
a = a - esp / 2
SOUND 440, 6: b = 105: inte = 1: circleair
LINE (230 + 34, 85)-(230 + 51, 90), 11
FOR d = 2 TO 0 STEP -1
CIRCLE (234 + esp, 105 + plus), d, 11
NEXT

a = a - esp / 2
SOUND 392, 3: b = 110: inte = 1: circleair
LINE (230 + 1.5 * 34, 93)-(230 + 1.5 * 34 - 5, 92), 11
a = a - esp / 2
SOUND 349, 6: b = 115: inte = 1: circleair
FOR d = 2 TO 0 STEP -1
CIRCLE (234 + 2 * esp, 115 + plus), d, 11
NEXT

LINE (282 + 34 / 2, 95)-(282 + 34, 105), 11
a = a - esp / 2
SOUND 294, 3: b = 125: inte = 1: circleair
LINE (230 + 2.5 * 34, 108)-(230 + 2.5 * 34 - 5, 106.5), 11
a = a - esp / 2
SOUND 262, 9: b = 130: inte = 1: circleair
barre
SOUND 349, 9: b = 115: inte = 1: circleair
SOUND 392, 3: b = 110: inte = 1: circleair
'LINE (128 + esp * 8, 90)-(128 + esp * 8, 85), 11
a = a - esp + esp / 3

SOUND 440, 3: b = 105: inte = 1: circleair
'LINE (128 + esp * 8, 90)-(128 + esp / 3 + esp * 8, 80), 11
a = a - esp + esp / 3
SOUND 466, 3: b = 100: inte = 1: circleair 'si bemol
LINE (128 + esp * 8, 90)-(128 + esp / 3 + esp / 3 + esp * 8, 80), 11
a = a - esp + esp / 3
SOUND 440, 9: b = 105: inte = 1: circleair
SOUND 523, 9: b = 95: inte = 1: circleair
barre
SOUND 392, 6: b = 110: inte = 1: circleair
FOR d = 2 TO 0 STEP -1
CIRCLE (234 + esp * 8, 110 + plus), d, 11
NEXT

LINE (230 + esp * 8, 90)-(230 + 34 / 2 + esp * 8, 80), 11
a = a - esp + esp / 2
SOUND 466, 3: b = 100: inte = 1: circleair  'si bemol
LINE (230 + 34 / 2 + esp * 8, 84)-(230 + 34 / 2 - 5 + esp * 8, 87), 11
a = a - esp + esp / 2
SOUND 440, 6: b = 105: inte = 1: circleair
FOR d = 2 TO 0 STEP -1
CIRCLE (234 + 9 * esp, 105 + plus), d, 11
NEXT

LINE (230 + 34 + esp * 8, 85)-(230 + 51 + esp * 8, 90), 11
a = a - esp + esp / 2
SOUND 392, 3: b = 110: inte = 1: circleair
a = a - esp + esp / 2
SOUND 349, 18: b = 115: inte = 4: circleair
LINE (230 + 1.5 * 34 + esp * 8, 93)-(230 + 1.5 * 34 - 5 + esp * 8, 92), 11
barre

'soupir
LINE (479 + esp * 4, 80 + plus)-(479 + esp * 4, 120 + plus), 11

apui
END SUB

SUB apui
COLOR 14: LOCATE 23, 20: PRINT "Appuyez sur ENTREE pour poursuivre..."
588 a$ = INKEY$: IF LEN(a$) = 0 THEN 588
IF ASC(a$) = 13 THEN 589 ELSE 588

589 LOCATE 23, 20: PRINT "                                               "
END SUB

SUB asar
690 ha$ = ""
IF aff = 1 THEN vc = INT(RND * 6)
IF vc = 0 AND uti0 = 0 THEN
ha$ = "DO": uti0 = 1: dod = dez
ELSEIF vc = 1 AND uti1 = 0 THEN
ha$ = "RE": uti1 = 1: re = dez
ELSEIF vc = 2 AND uti2 = 0 THEN
ha$ = "MI": uti2 = 1: mi = dez
ELSEIF vc = 3 AND uti3 = 0 THEN
ha$ = "FA": uti3 = 1: fa = dez
ELSEIF vc = 4 AND uti4 = 0 THEN
ha$ = "SOL": uti4 = 1: sol = dez
ELSEIF vc = 5 AND uti5 = 0 THEN
ha$ = "LA": uti5 = 1: la = dez
END IF
vc = vc + 1
IF ha$ = "" THEN 690
END SUB

SUB asar2
790 ha$ = ""
IF aff = 1 THEN vc2 = INT(RND * 7)
IF vc2 = 0 AND uti6 = 0 THEN
ha$ = "SI": uti6 = 1: si = dez
ELSEIF vc2 = 1 AND uti7 = 0 THEN
ha$ = "DO": uti7 = 1: dod2 = dez
ELSEIF vc2 = 2 AND uti8 = 0 THEN
ha$ = "RE": uti8 = 1: re2 = dez
ELSEIF vc2 = 3 AND uti9 = 0 THEN
ha$ = "MI": uti9 = 1: mi2 = dez
ELSEIF vc2 = 4 AND uti10 = 0 THEN
ha$ = "FA": uti10 = 1: fa2 = dez
ELSEIF vc2 = 5 AND uti11 = 0 THEN
ha$ = "SOL": uti11 = 1: sol2 = dez
ELSEIF vc2 = 6 AND uti12 = 0 THEN
ha$ = "LA": uti12 = 1: la2 = dez
END IF
vc2 = vc2 + 1
IF ha$ = "" THEN 790

END SUB

SUB asar3
750 ha$ = ""
IF aff = 1 THEN vc = INT(RND * 6)
IF vc = 0 AND uti0 = 0 THEN
ha$ = "MI": uti0 = 1: dod = dez
ELSEIF vc = 1 AND uti1 = 0 THEN
ha$ = "FA": uti1 = 1: re = dez
ELSEIF vc = 2 AND uti2 = 0 THEN
ha$ = "SOL": uti2 = 1: mi = dez
ELSEIF vc = 3 AND uti3 = 0 THEN
ha$ = "LA": uti3 = 1: fa = dez
ELSEIF vc = 4 AND uti4 = 0 THEN
ha$ = "SI": uti4 = 1: sol = dez
ELSEIF vc = 5 AND uti5 = 0 THEN
ha$ = "DO": uti5 = 1: la = dez
END IF
vc = vc + 1
IF ha$ = "" THEN 750

END SUB

SUB ASAR4
760 ha$ = ""
IF aff = 1 THEN vc2 = INT(RND * 7)
IF vc2 = 0 AND uti6 = 0 THEN
ha$ = "RE": uti6 = 1: si = dez
ELSEIF vc2 = 1 AND uti7 = 0 THEN
ha$ = "MI": uti7 = 1: dod2 = dez
ELSEIF vc2 = 2 AND uti8 = 0 THEN
ha$ = "FA": uti8 = 1: re2 = dez
ELSEIF vc2 = 3 AND uti9 = 0 THEN
ha$ = "SOL": uti9 = 1: mi2 = dez
ELSEIF vc2 = 4 AND uti10 = 0 THEN
ha$ = "LA": uti10 = 1: fa2 = dez
ELSEIF vc2 = 5 AND uti11 = 0 THEN
ha$ = "SI": uti11 = 1: sol2 = dez
ELSEIF vc2 = 6 AND uti12 = 0 THEN
ha$ = "DO": uti12 = 1: la2 = dez
END IF
vc2 = vc2 + 1
IF ha$ = "" THEN 760

END SUB

SUB atten
decor
'FOR jh = 1 TO 50: NEXT
misezero = TIMER
DO

LOOP WHILE TIMER < misezero + .02 AND TIMER > 5

END SUB

SUB attention
bon = 0
frk = (frt * 5) / 40
frk = CINT(frk)
' LOCATE 3, 19: PRINT "frk"; frk; "b"; b; "dod"; dod; "re"; re; "mi"; mi
IF dod = frk AND b = 130 THEN bon = 1
IF re = frk AND b = 125 THEN bon = 1
IF mi = frk AND b = 120 THEN bon = 1
IF fa = frk AND b = 115 THEN bon = 1
IF sol = frk AND b = 110 THEN bon = 1
IF la = frk AND b = 105 THEN bon = 1
IF si = frk AND b = 100 THEN bon = 1
IF dod2 = frk AND b = 95 THEN bon = 1
IF re2 = frk AND b = 90 THEN bon = 1
IF mi2 = frk AND b = 85 THEN bon = 1
IF fa2 = frk AND b = 80 THEN bon = 1
IF sol2 = frk AND b = 75 THEN bon = 1
IF la2 = frk AND b = 70 THEN bon = 1
END SUB

SUB barre
LINE (bar, 80 + plus)-(bar, 120 + plus), 11
bar = bar + esp * nb
END SUB

SUB bemol

LINE (80, bem + 5)-(80, bem - 15), 3
CIRCLE (80, bem), 5, 3, 3.14 + 3.14 / 2, 3.14 / 2







END SUB

SUB bin
profil = 0


CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin - mes, bbin + 2.5 * mes + 2 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin + mes, bbin + 2.5 * mes + 2 * mes), col
LINE (abin, bbin + mes)-(abin - mes, bbin + 2 * mes), col
LINE (abin, bbin + mes)-(abin + mes, bbin + 2 * mes), col

IF col = 0 THEN decori ELSE decor


END SUB

SUB bonn

IF aff = 0 OR aff = 2 THEN
CIRCLE (frt, 233), 11, colp

ELSEIF aff = 0 OR aff = 1 THEN
'CIRCLE (frt, 273), 20, colp
LINE (frt - 15, 273 + 7)-(frt + 18, 273 - 7), colp, B
END IF
END SUB

SUB boutligne

LINE (adictee - 20, 80 + plus)-(adictee + 20, 80 + plus), 3
LINE (adictee - 20, 90 + plus)-(adictee + 20, 90 + plus), 3
LINE (adictee - 20, 100 + plus)-(adictee + 20, 100 + plus), 3
LINE (adictee - 20, 110 + plus)-(adictee + 20, 110 + plus), 3
LINE (adicte - 20, 120 + plus)-(adictee + 20, 120 + plus), 3

END SUB

SUB choixnotes
RANDOMIZE TIMER
IF cle = 0 THEN
f = INT(RND * nombre)
IF f = 0 THEN b = 130
IF f = 1 THEN b = 125
IF f = 2 THEN b = 120
IF f = 3 THEN b = 115
IF f = 4 THEN b = 110
IF f = 5 THEN b = 105
IF f = 6 THEN b = 100
IF f = 7 THEN b = 95
IF f = 8 THEN b = 90
IF f = 9 THEN b = 85
IF f = 10 THEN b = 80
IF f = 11 THEN b = 75
IF f = 12 THEN b = 70
ELSEIF cle = 1 THEN
f = INT(RND * nombre)
IF f = 12 THEN b = 130
IF f = 11 THEN b = 125
IF f = 10 THEN b = 120
IF f = 9 THEN b = 115
IF f = 8 THEN b = 110
IF f = 7 THEN b = 105
IF f = 6 THEN b = 100
IF f = 5 THEN b = 95
IF f = 4 THEN b = 90
IF f = 3 THEN b = 85
IF f = 2 THEN b = 80
IF f = 1 THEN b = 75
IF f = 0 THEN b = 70
END IF
IF cle = 0 AND music = 1 THEN jouesol
IF cle = 1 AND music = 1 THEN jouefa
END SUB

SUB circleair
FOR d = 4 TO 1 STEP -inte: CIRCLE (a, b + plus), d, 11: NEXT:        'dessin de note
IF b = 130 THEN LINE (a - 8, 130 + plus)-(a + 8, 130 + plus), 11
IF porq = 2 THEN 230
queue
230
a = a + esp
porq = 0
ralenti
END SUB

SUB clefa
CIRCLE (35, ref - 20 + plus), 12, 11, 0, 3.14
CIRCLE (23, ref - 20 + plus), 24, 11, 4.8, 0
FOR d = 2 TO 0 STEP -1
CIRCLE (52, ref - 24 + plus), d, 11
NEXT
FOR d = 2 TO 0 STEP -1
CIRCLE (52, ref - 16 + plus), d, 11
NEXT
FOR d = 2 TO 0 STEP -1
CIRCLE (26, ref - 19 + plus), d, 11
NEXT
END SUB

SUB clesol
COLOR 11
CIRCLE (40, ref + plus), 12, 11, 0, 3.14
CIRCLE (33, ref + plus), 19, 11, 3.14, 0
LINE (14, ref + plus)-(51, ref - 55 + plus), 11
CIRCLE (45, ref - 55 + plus), 6, 11, 0, 3.14
LINE (39, ref - 55 + plus)-(39, ref + 25 + plus), 11
CIRCLE (33, ref + 25 + plus), 6, 11, 3.14, 0
END SUB

SUB decor
IF profil = 0 THEN


CIRCLE (abin - mes / 6, bbin - mes / 7), mes / 15, 11
CIRCLE (abin + mes / 6, bbin - mes / 7), mes / 15, 11

ELSEIF profil = 1 THEN
CIRCLE (abin - mes / 6, bbin - mes / 7), mes / 15, 11
ELSEIF profil = 2 THEN
CIRCLE (abin + mes / 6, bbin - mes / 7), mes / 15, 11
END IF
IF profil = 0 THEN
CIRCLE (abin, bbin - mes / 8), mes / 2, 11, 4.5, 5.2
ELSEIF profil = 1 THEN
CIRCLE (abin - mes / 4, bbin - mes / 8), mes / 2, 11, 4.6, 5
ELSEIF profil = 2 THEN
CIRCLE (abin + mes / 4, bbin - mes / 8), mes / 2, 11, 4.4, 4.8
END IF
IF profil = 0 THEN LINE (abin, bbin)-(abin, bbin + mes / 25), 11
LINE (abin, bbin + mes / 2.6)-(abin, bbin + mes), 12


'IF profil = 0 THEN
'
'FOR tr = abin  - mes / 2 TO abin  + mes * 2
'LINE (tr, bbin - 10)-(tr, bbin - 20), 12
'NEXT
'END IF


END SUB

SUB decori
IF profil = 0 THEN


CIRCLE (abin - mes / 6, bbin - mes / 7), mes / 15, 0
CIRCLE (abin + mes / 6, bbin - mes / 7), mes / 15, 0

ELSEIF profil = 1 THEN
CIRCLE (abin - mes / 6, bbin - mes / 7), mes / 15, 0
ELSEIF profil = 2 THEN
CIRCLE (abin + mes / 6, bbin - mes / 7), mes / 15, 0
END IF
IF profil = 0 THEN
CIRCLE (abin, bbin - mes / 8), mes / 2, 0, 4.5, 5.2
ELSEIF profil = 1 THEN
CIRCLE (abin - mes / 4, bbin - mes / 8), mes / 2, 0, 4.6, 5
ELSEIF profil = 2 THEN
CIRCLE (abin + mes / 4, bbin - mes / 8), mes / 2, 0, 4.4, 4.8
END IF
IF profil = 0 THEN LINE (abin, bbin)-(abin, bbin + mes / 25), 0
LINE (abin, bbin + mes / 2.6)-(abin, bbin + mes), 0

END SUB

SUB deplace
LINE (0, 4.5 * mes + bbin + 2)-(800, 4.5 * mes + bbin + 2), 13
IF mva = 75 THEN gauche
IF mva = 77 THEN droite
IF mva = 72 THEN saut
IF mva = 80 THEN acroupi
col = 12
bin

END SUB

SUB description
ligneair
COLOR 12: LOCATE 2, 30: PRINT "Voici les notes Ö retenir:"
bw = 130: aw = 90: pl = 11

FOR ty = 1 TO nombre

IF bw < 100 THEN vol = 10 ELSE vol = 11
FOR d = 4 TO 1 STEP -1: CIRCLE (aw, bw + plus), d, vol: NEXT:        'dessin de note
IF bw = 130 THEN LINE (aw - 8, 130 + plus)-(aw + 8, 130 + plus), vol
IF bw = 130 OR bw = 125 OR bw = 120 OR bw = 115 OR bw = 110 OR bw = 105 OR bw = 100 THEN plusq = -20
IF bw < 96 THEN plusq = 20
LINE (aw - 4, bw + plus)-(aw - 4, bw + plusq + plus), vol
b = bw
IF cle = 0 THEN notesol
IF cle = 1 THEN notefa
LOCATE 5, pl: COLOR 7: PRINT not$,
FOR g = 1 TO 200: NEXT
bw = bw - 5: aw = aw + 40: pl = pl + 5


NEXT
apui
CLS
END SUB

SUB droite
col = 0
bin
profil = 2



col = 12
CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin, bbin + 2.5 * mes + 2 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin + mes / 2, bbin + 2.5 * mes + mes), col
LINE (abin + mes / 2, bbin + 3.5 * mes)-(abin + .5 * mes, bbin + 3.5 * mes + mes), col
LINE (abin, bbin + mes)-(abin - .5 * mes, bbin + 2.5 * mes), col
LINE (abin, bbin + mes)-(abin + .5 * mes, bbin + 2.5 * mes), col
atten


col = 0
CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin, bbin + 2.5 * mes + 2 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin + mes / 2, bbin + 2.5 * mes + mes), col
LINE (abin + mes / 2, bbin + 3.5 * mes)-(abin + .5 * mes, bbin + 3.5 * mes + mes), col
LINE (abin, bbin + mes)-(abin - .5 * mes, bbin + 2.5 * mes), col
LINE (abin, bbin + mes)-(abin + .5 * mes, bbin + 2.5 * mes), col
decori



col = 12
CIRCLE (abin, bbin), mes / 2, col                      'tete
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col   'tronc
LINE (abin, bbin + 2.5 * mes)-(abin, bbin + 2.5 * mes + 2 * mes), col    'jambe
LINE (abin, bbin + 2.5 * mes)-(abin + mes, bbin + 2.5 * mes + mes), col    'jambe  1/2
LINE (abin + mes, bbin + 3.5 * mes)-(abin + mes, bbin + 3.5 * mes + .5 * mes), col  'jambe 1/2
LINE (abin, bbin + mes)-(abin, bbin + 2.5 * mes), col    'bras
LINE (abin, bbin + mes)-(abin, bbin + 2.5 * mes), col    'bras
atten

col = 0
CIRCLE (abin, bbin), mes / 2, col                      'tete
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col   'tronc
LINE (abin, bbin + 2.5 * mes)-(abin, bbin + 2.5 * mes + 2 * mes), col    'jambe
LINE (abin, bbin + 2.5 * mes)-(abin + mes, bbin + 2.5 * mes + mes), col    'jambe  1/2
LINE (abin + mes, bbin + 3.5 * mes)-(abin + mes, bbin + 3.5 * mes + .5 * mes), col  'jambe 1/2
LINE (abin, bbin + mes)-(abin, bbin + 2.5 * mes), col    'bras
LINE (abin, bbin + mes)-(abin, bbin + 2.5 * mes), col    'bras
decori

col = 12: abin = abin + .5 * mes
CIRCLE (abin, bbin), mes / 2, col                   'tete
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col   'tronc
LINE (abin, bbin + 2.5 * mes)-(abin - mes, bbin + 2.5 * mes + 2 * mes), col          'jambe
LINE (abin, bbin + 2.5 * mes)-(abin + mes, bbin + 2.5 * mes + 2 * mes), col   'jambe
LINE (abin, bbin + mes)-(abin + .5 * mes, bbin + 2.5 * mes), col  'bras
LINE (abin, bbin + mes)-(abin - .5 * mes, bbin + 2.5 * mes), col  'bras
atten

col = 0
CIRCLE (abin, bbin), mes / 2, col                   'tete
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col   'tronc
LINE (abin, bbin + 2.5 * mes)-(abin - mes, bbin + 2.5 * mes + 2 * mes), col          'jambe
LINE (abin, bbin + 2.5 * mes)-(abin + mes, bbin + 2.5 * mes + 2 * mes), col   'jambe
LINE (abin, bbin + mes)-(abin + .5 * mes, bbin + 2.5 * mes), col  'bras
LINE (abin, bbin + mes)-(abin - .5 * mes, bbin + 2.5 * mes), col  'bras
decori

col = 12: abin = abin + .5 * mes
CIRCLE (abin, bbin), mes / 2, col                   'tete
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col   'tronc
LINE (abin, bbin + 2.5 * mes)-(abin, bbin + 2.5 * mes + 2 * mes), col         'jambe
LINE (abin, bbin + 2.5 * mes)-(abin - .5 * mes, bbin + 2.5 * mes + mes), col   'jambe 1/2
LINE (abin - .5 * mes, bbin + 2.5 * mes + mes)-(abin - 1.5 * mes, bbin + 2.5 * mes + 1.5 * mes), col  'jambe 1/2
LINE (abin, bbin + mes)-(abin + mes, bbin + 2 * mes), col    'bras
LINE (abin, bbin + mes)-(abin - mes, bbin + 2 * mes), col  'bras
atten
col = 0
CIRCLE (abin, bbin), mes / 2, col                   'tete
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col   'tronc
LINE (abin, bbin + 2.5 * mes)-(abin, bbin + 2.5 * mes + 2 * mes), col         'jambe
LINE (abin, bbin + 2.5 * mes)-(abin - .5 * mes, bbin + 2.5 * mes + mes), col   'jambe 1/2
LINE (abin - .5 * mes, bbin + 2.5 * mes + mes)-(abin - 1.5 * mes, bbin + 2.5 * mes + 1.5 * mes), col  'jambe 1/2
LINE (abin, bbin + mes)-(abin + mes, bbin + 2 * mes), col    'bras
LINE (abin, bbin + mes)-(abin - mes, bbin + 2 * mes), col  'bras
decori
col = 12
CIRCLE (abin, bbin), mes / 2, col                   'tete
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col   'tronc
LINE (abin, bbin + 2.5 * mes)-(abin, bbin + 2.5 * mes + 2 * mes), col         'jambe
LINE (abin, bbin + 2.5 * mes)-(abin + .5 * mes, bbin + 2.5 * mes + mes), col   'jambe 1/2
LINE (abin + .5 * mes, bbin + 2.5 * mes + mes)-(abin - .5 * mes, bbin + 2.5 * mes + 1.5 * mes), col  'jambe 1/2
LINE (abin, bbin + mes)-(abin + mes, bbin + 2 * mes), col    'bras
LINE (abin, bbin + mes)-(abin - mes, bbin + 2 * mes), col  'bras
atten
col = 0
CIRCLE (abin, bbin), mes / 2, col                   'tete
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col   'tronc
LINE (abin, bbin + 2.5 * mes)-(abin, bbin + 2.5 * mes + 2 * mes), col         'jambe
LINE (abin, bbin + 2.5 * mes)-(abin + .5 * mes, bbin + 2.5 * mes + mes), col   'jambe 1/2
LINE (abin + .5 * mes, bbin + 2.5 * mes + mes)-(abin - .5 * mes, bbin + 2.5 * mes + 1.5 * mes), col  'jambe 1/2
LINE (abin, bbin + mes)-(abin + mes, bbin + 2 * mes), col    'bras
LINE (abin, bbin + mes)-(abin - mes, bbin + 2 * mes), col  'bras
decori




END SUB

