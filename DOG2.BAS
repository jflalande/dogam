DECLARE SUB morceaux ()
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
COMMON SHARED nombre, music, a, B, f, rep, bon, not$, l, g, plus, poin
COMMON SHARED ref, temp, p, inte, plusq, esp, tep, kd, gf, tome$, vol
COMMON SHARED ha$, vc, uti0, uti1, uti2, uti3, uti4, uti5, dez, uti6
COMMON SHARED uti7, uti8, uti9, uti10, uti11, frt, colp, dod, re, mi
COMMON SHARED fa, sol, la, si, dod2, re2, mi2, fa2, sol2, bar, nb
COMMON SHARED points, nom$, primo, rend, porq, sop, sup, pouet, uti12
COMMON SHARED la2, aff, vc2, bem, dictee, cligne, colcli, aide, voiraide
COMMON SHARED adictee, bdictee, adicteer, bdicteer, sens

SUB efface
FOR TUY = 1 TO 2500: NEXT
g = 650
210 LINE (g, 43)-(g, 140), 0
g = g - 1: IF g = 0 THEN 220 ELSE 210
220
ligne
a = 90: l = 11
END SUB

SUB espacenote
SOUND 0, .2: B = 610: inte = 1: circleair
a = a - esp
END SUB

SUB expli
CLS
COLOR 12
LOCATE 1, 37: PRINT "NOTICE"
LOCATE 3, 1: COLOR 10
PRINT "Le but de ce jeu est d'apprendre le solfŠge en s'amusant..."
PRINT
PRINT "Les notions ‚l‚mentaires de solfŠge vous sont pr‚sent‚es de maniŠre r‚cr‚ative."
PRINT ""
PRINT "A la page suivante vous pourrez choisir entre :  "
PRINT
PRINT
PRINT
PRINT "              - Apprendre les notes en cl‚ de sol ou de fa"
PRINT
PRINT "              - Tester vos connaissances sur la lecture des notes"
PRINT
PRINT "              - Composer quelques m‚lodies"
PRINT
PRINT
PRINT
PRINT
COLOR 13
PRINT "              A tous moments:   F1 = Aide"



apui
END SUB

SUB feliz
CLS
COLOR 6: LOCATE 2, 34: PRINT "RESULTAT"
COLOR 10: LOCATE 10, 5: PRINT "F‚licitation ! ! !"
COLOR 12: LOCATE 15, 7: PRINT "Avec vos "; poin; " points en 1 minute ", nom$
LOCATE 16, 7: PRINT "… ‚t‚ battu:il avait eu "; points; " points."
apui
COLOR 14: LOCATE 17, 7: INPUT "Votre nom"; nom$
points = poin
CLOSE
OPEN "score.pia" FOR OUTPUT AS #1
WRITE #1, nom$, points
CLOSE #1
apui
END SUB

SUB felizfa
CLS
COLOR 6: LOCATE 2, 34: PRINT "RESULTAT"
COLOR 10: LOCATE 10, 5: PRINT "F‚licitation ! ! !"
COLOR 12: LOCATE 15, 7: PRINT "Avec vos "; poin; " points en 1 minute ", nom$
LOCATE 16, 7: PRINT "… ‚t‚ battu:il avait eu "; points; " points. Et en cl‚ de FA !!!"
apui
COLOR 14: LOCATE 17, 7: INPUT "Votre nom"; nom$
points = poin
CLOSE
OPEN "scorefa.pia" FOR OUTPUT AS #1
WRITE #1, nom$, points
CLOSE #1
apui

END SUB

SUB frere
SCREEN 9
CLS : plus = plus + 8
LOCATE 2, 32: COLOR 15: PRINT "FrŠre Jacques"
COLOR 9: LOCATE 7, 9: PRINT "2"
COLOR 9: LOCATE 8, 9: PRINT "4"
cle = 0: a = 90: esp = 34: vol = 11: bar = 209: nb = 4: plus = 0: ref = 110
LINE (0, 80 + plus)-(617, 80 + plus), 3
LINE (0, 90 + plus)-(617, 90 + plus), 3
LINE (0, 100 + plus)-(617, 100 + plus), 3
LINE (0, 110 + plus)-(617, 110 + plus), 3
LINE (0, 120 + plus)-(617, 120 + plus), 3
IF cle = 0 THEN clesol
IF cle = 1 THEN clefa

SOUND 392, 9: B = 110: inte = 1: circleair
SOUND 440, 9: B = 105: inte = 1: circleair
SOUND 493, 9: B = 100: inte = 1: circleair

SOUND 392, 9: B = 110: inte = 1: circleair
barre
espacenote
SOUND 392, 9: B = 110: inte = 1: circleair
SOUND 440, 9: B = 105: inte = 1: circleair

SOUND 493, 9: B = 100: inte = 1: circleair
SOUND 392, 9: B = 110: inte = 1: circleair
barre
SOUND 493, 9: B = 100: inte = 1: circleair
SOUND 523, 9: B = 95: inte = 1: circleair
SOUND 587, 18: B = 90: inte = 4: circleair
barre
a = a + esp
SOUND 493, 9: B = 100: inte = 1: circleair
SOUND 523, 9: B = 95: inte = 1: circleair
SOUND 587, 18: B = 90: inte = 4: circleair
barre
a = a + esp
espacenote
plus = 120: cle = 0: a = 90: esp = 34: vol = 11: bar = 209: nb = 4
LINE (0, 80 + plus)-(617, 80 + plus), 3
LINE (0, 90 + plus)-(617, 90 + plus), 3
LINE (0, 100 + plus)-(617, 100 + plus), 3
LINE (0, 110 + plus)-(617, 110 + plus), 3
LINE (0, 120 + plus)-(617, 120 + plus), 3
IF cle = 0 THEN clesol
IF cle = 1 THEN clefa

SOUND 587, 6.75: B = 90: esp = 17: inte = 1: circleair
FOR d = 2 TO 0 STEP -1
CIRCLE (98, 90 + plus), d, 11
NEXT
SOUND 659, 2.25: B = 85: esp = 17: inte = 1: circleair
LINE (86, 110 + plus)-(103, 105 + plus), 11
LINE (103, 102 + plus)-(98, 104 + plus), 11
SOUND 587, 4.5: B = 90: esp = 17: inte = 1: circleair
SOUND 523, 4.5: B = 95: esp = 17: inte = 1: circleair
LINE (120, 110 + plus)-(137, 115 + plus), 11
SOUND 496, 9: B = 100: esp = 34: inte = 1: circleair
SOUND 392, 9: B = 110: inte = 1: circleair
barre

SOUND 587, 6.75: B = 90: esp = 17: inte = 1: circleair
FOR d = 2 TO 0 STEP -1
CIRCLE (98 + esp * 8, 90 + plus), d, 11
NEXT
SOUND 659, 2.25: B = 85: esp = 17: inte = 1: circleair
LINE (88 + esp * 8, 110 + plus)-(103 + esp * 8, 105 + plus), 11
LINE (103 + esp * 8, 102 + plus)-(98 + esp * 8, 104 + plus), 11
SOUND 587, 4.5: B = 90: esp = 17: inte = 1: circleair
SOUND 523, 4.5: B = 95: esp = 17: inte = 1: circleair
LINE (120 + esp * 8, 110 + plus)-(137 + esp * 8, 115 + plus), 11
SOUND 496, 9: B = 100: esp = 34: inte = 1: circleair
SOUND 392, 9: B = 110: inte = 1: circleair
barre








espacenote
SOUND 392, 9: B = 110: inte = 1: circleair
SOUND 293, 9: B = 125: inte = 1: circleair
SOUND 392, 18: B = 110: inte = 4: circleair
barre
a = a + esp
espacenote
SOUND 392, 9: B = 110: inte = 1: circleair
SOUND 293, 9: B = 125: inte = 1: circleair
SOUND 392, 18: B = 110: inte = 4: circleair
barre
LINE (479 + esp * 4, 80 + plus)-(479 + esp * 4, 120 + plus), 11
apui
END SUB

SUB gauche
col = 0
bin
profil = 1

col = 12
CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin, bbin + 2.5 * mes + 2 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin - mes / 2, bbin + 2.5 * mes + mes), col
LINE (abin - mes / 2, bbin + 2.5 * mes + mes)-(abin - .5 * mes, bbin + 3.5 * mes + mes), col
LINE (abin, bbin + mes)-(abin - .5 * mes, bbin + 2.5 * mes), col
LINE (abin, bbin + mes)-(abin + .5 * mes, bbin + 2.5 * mes), col
atten


col = 0
CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin, bbin + 2.5 * mes + 2 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin - mes / 2, bbin + 2.5 * mes + mes), col
LINE (abin - mes / 2, bbin + 3.5 * mes)-(abin - .5 * mes, bbin + 3.5 * mes + mes), col
LINE (abin, bbin + mes)-(abin - .5 * mes, bbin + 2.5 * mes), col
LINE (abin, bbin + mes)-(abin + .5 * mes, bbin + 2.5 * mes), col
decori




col = 12
CIRCLE (abin, bbin), mes / 2, col                      'tete
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col   'tronc
LINE (abin, bbin + 2.5 * mes)-(abin, bbin + 2.5 * mes + 2 * mes), col    'jambe
LINE (abin, bbin + 2.5 * mes)-(abin - mes, bbin + 2.5 * mes + mes), col    'jambe  1/2
LINE (abin - mes, bbin + 3.5 * mes)-(abin - mes, bbin + 3.5 * mes + .5 * mes), col  'jambe 1/2
LINE (abin, bbin + mes)-(abin, bbin + 2.5 * mes), col    'bras
LINE (abin, bbin + mes)-(abin, bbin + 2.5 * mes), col    'bras
atten

col = 0
CIRCLE (abin, bbin), mes / 2, col                      'tete
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col   'tronc
LINE (abin, bbin + 2.5 * mes)-(abin, bbin + 2.5 * mes + 2 * mes), col    'jambe
LINE (abin, bbin + 2.5 * mes)-(abin - mes, bbin + 2.5 * mes + mes), col    'jambe  1/2
LINE (abin - mes, bbin + 3.5 * mes)-(abin - mes, bbin + 3.5 * mes + .5 * mes), col  'jambe 1/2
LINE (abin, bbin + mes)-(abin, bbin + 2.5 * mes), col    'bras
LINE (abin, bbin + mes)-(abin, bbin + 2.5 * mes), col    'bras
decori

col = 12: abin = abin - .5 * mes
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

col = 12: abin = abin - .5 * mes
CIRCLE (abin, bbin), mes / 2, col                   'tete
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col   'tronc
LINE (abin, bbin + 2.5 * mes)-(abin, bbin + 2.5 * mes + 2 * mes), col         'jambe
LINE (abin, bbin + 2.5 * mes)-(abin + .5 * mes, bbin + 2.5 * mes + mes), col   'jambe 1/2
LINE (abin + .5 * mes, bbin + 2.5 * mes + mes)-(abin + 1.5 * mes, bbin + 2.5 * mes + 1.5 * mes), col  'jambe 1/2
LINE (abin, bbin + mes)-(abin + mes, bbin + 2 * mes), col    'bras
LINE (abin, bbin + mes)-(abin - mes, bbin + 2 * mes), col  'bras
atten


col = 0
CIRCLE (abin, bbin), mes / 2, col                   'tete
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col   'tronc
LINE (abin, bbin + 2.5 * mes)-(abin, bbin + 2.5 * mes + 2 * mes), col         'jambe
LINE (abin, bbin + 2.5 * mes)-(abin + .5 * mes, bbin + 2.5 * mes + mes), col   'jambe 1/2
LINE (abin + .5 * mes, bbin + 2.5 * mes + mes)-(abin + 1.5 * mes, bbin + 2.5 * mes + 1.5 * mes), col  'jambe 1/2
LINE (abin, bbin + mes)-(abin + mes, bbin + 2 * mes), col    'bras
LINE (abin, bbin + mes)-(abin - mes, bbin + 2 * mes), col  'bras
decori

col = 12
CIRCLE (abin, bbin), mes / 2, col                   'tete
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col   'tronc
LINE (abin, bbin + 2.5 * mes)-(abin, bbin + 2.5 * mes + 2 * mes), col         'jambe
LINE (abin, bbin + 2.5 * mes)-(abin - .5 * mes, bbin + 2.5 * mes + mes), col   'jambe 1/2
LINE (abin - .5 * mes, bbin + 2.5 * mes + mes)-(abin + .5 * mes, bbin + 2.5 * mes + 1.5 * mes), col  'jambe 1/2
LINE (abin, bbin + mes)-(abin + mes, bbin + 2 * mes), col    'bras
LINE (abin, bbin + mes)-(abin - mes, bbin + 2 * mes), col  'bras
atten

col = 0
CIRCLE (abin, bbin), mes / 2, col                   'tete
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col   'tronc
LINE (abin, bbin + 2.5 * mes)-(abin, bbin + 2.5 * mes + 2 * mes), col         'jambe
LINE (abin, bbin + 2.5 * mes)-(abin - .5 * mes, bbin + 2.5 * mes + mes), col   'jambe 1/2
LINE (abin - .5 * mes, bbin + 2.5 * mes + mes)-(abin + .5 * mes, bbin + 2.5 * mes + 1.5 * mes), col  'jambe 1/2
LINE (abin, bbin + mes)-(abin + mes, bbin + 2 * mes), col    'bras
LINE (abin, bbin + mes)-(abin - mes, bbin + 2 * mes), col  'bras
decori
END SUB

SUB geretemp
IF TIME$ = tome$ THEN 650
tome$ = TIME$
horloge



650 END SUB

SUB grandchoix
549 CLS
SCREEN 0
56 colr = INT(RND * 16)
IF colr = 0 THEN 56
COLOR colr
LOCATE 15, 10: PRINT "ÛÛÛÛÛÛÛÛÛ    ÛÛÛÛÛÛÛÛÛ ÛÛÛÛÛÛÛÛ    ÛÛ    ÛÛ      ÛÛ"
LOCATE 16, 10: PRINT "Û        Û   Û       Û Û          Û  Û   Û Û    Û Û"
LOCATE 17, 10: PRINT "Û         Û  Û       Û Û         Û    Û  Û  Û  Û  Û"
LOCATE 18, 10: PRINT "Û          Û Û       Û Û        Û      Û Û   ÛÛ   Û"
LOCATE 19, 10: PRINT "Û         Û  Û       Û Û   ÛÛÛÛ ÛÛÛÛÛÛÛÛ Û        Û"
LOCATE 20, 10: PRINT "Û        Û   Û       Û Û      Û Û      Û Û        Û"
LOCATE 21, 10: PRINT "ÛÛÛÛÛÛÛÛÛ    ÛÛÛÛÛÛÛÛÛ ÛÛÛÛÛÛÛÛ Û      Û Û        Û"


primo = 0
temp = 0
548
COLOR 12
LOCATE 1, 34:
PRINT "SUPER MENU"
COLOR 14
LOCATE 1, 54: PRINT "F1 = Aide"
COLOR 13
LOCATE 1, 10: PRINT "Version 1.0"
COLOR 10
LOCATE 4, 12: PRINT "1 - Le‡ons de solfŠge"
LOCATE 5, 12: PRINT "2 - Tests de solfŠge"
LOCATE 6, 12: PRINT "3 - morceaux de musique"
LOCATE 7, 12: PRINT "4 - A propos de DOGAM"
LOCATE 8, 12: PRINT "5 - Quitter DOGAM"
LOCATE 10, 22: PRINT "Tapez 1, 2 ,3, 4 ou 5 comme s‚lection:"
547 COLOR 7: LOCATE 3, 42: PRINT "Heure: "; TIME$
a$ = INKEY$: IF LEN(a$) = 0 THEN 547
mv = ASC(RIGHT$(a$, 1))
IF mv = 49 THEN
lecon
GOTO 549
ELSEIF mv = 50 THEN
pass = 1
ELSEIF mv = 51 THEN
morceaux
GOTO 549
ELSEIF mv = 53 THEN
COLOR 15
CLS
PRINT "Merci d'avoir jou‚ avec DOGAM."
END
ELSEIF mv = 52 THEN
graphique
GOTO 549
ELSEIF mv = 59 THEN
grandchoixhelp
GOTO 548
END IF
IF pass = 0 THEN 548
cle = 0
aff = 1
END SUB

SUB grandchoixhelp
COLOR 14
LOCATE 9, 1: PRINT CHR$(218); STRING$(78, 196); CHR$(191)
FOR cadre = 10 TO 22
PRINT CHR$(179); STRING$(78, 32); CHR$(179)
NEXT
PRINT CHR$(192); STRING$(78, 196); CHR$(217)
COLOR 15
LOCATE 10, 2: PRINT "Aide:                                                                        "
LOCATE 12, 2: PRINT "Dans ce menu, vous pouvez choisir d' apprendre le nom des notes et essayer   "
LOCATE 13, 2: PRINT "de m‚moriser leurs sons; pour cela appuyez sur la touche -1- du clavier. Vous"
LOCATE 14, 2: PRINT "pouvez tester vos connaissances sur la lecture des notes en lan‡ant le second"
LOCATE 15, 2: PRINT "menu de DOGAM (touche -2-). De plus il est possible de composer ses propres"
LOCATE 16, 2: PRINT "m‚lodies (touche -3-). Pour finir vous pouvez lancer la pr‚sentation de DOGAM"
LOCATE 17, 2: PRINT "(touche -4-) ou quitter DOGAM (touche -5-)."
LOCATE 18, 2: PRINT "Appuyez sur la touche 1, 2, 3, 4 ou 5 pour s‚lectionner votre choix."
LOCATE 19, 53: PRINT "ÛÛÛ  ÛÛÛÛ ÛÛÛÛ ÛÛÛÛ Û   Û"
LOCATE 20, 53: PRINT "Û  Û Û  Û Û    Û  Û ÛÛ ÛÛ"
LOCATE 21, 53: PRINT "Û  Û Û  Û Û  Û ÛÛÛÛ Û Û Û"
LOCATE 22, 53: PRINT "ÛÛÛ  ÛÛÛÛ ÛÛÛÛ Û  Û Û   Û"
END SUB

SUB horloge
SCREEN 9


CIRCLE (610, 25), 21, 11
FOR ray = 1 TO 20
CIRCLE (610, 25), ray, 12, gf, gf + 6.28 / 60
NEXT

kd = kd + 1
IF kd > 59 THEN tep = 1
gf = gf - 6.28 / 60
IF gf < 0 THEN gf = 6.28 - 6.28 / 60

END SUB

SUB jouefa
IF B = 130 + pouet THEN SOUND 82.41, 10
IF B = 125 + pouet THEN SOUND 87.31, 10
IF B = 120 + pouet THEN SOUND 98, 10
IF B = 115 + pouet THEN SOUND 110, 10
IF B = 110 + pouet THEN SOUND 123.47, 10
IF B = 105 + pouet THEN SOUND 130.81, 10
IF B = 100 + pouet THEN SOUND 146.83, 10
IF B = 95 + pouet THEN SOUND 164.81, 10
IF B = 90 + pouet THEN SOUND 174.61, 10
IF B = 85 + pouet THEN SOUND 196, 10
IF B = 80 + pouet THEN SOUND 220, 10
IF B = 75 + pouet THEN SOUND 246.94, 10
IF B = 70 + pouet THEN SOUND 261.63, 10
END SUB

SUB jouesol
IF B = 130 THEN SOUND 261.63, 10
IF B = 125 THEN SOUND 293.66, 10
IF B = 120 THEN SOUND 329.63, 10
IF B = 115 THEN SOUND 349.23, 10
IF B = 110 THEN SOUND 392, 10
IF B = 105 THEN SOUND 440, 10
IF B = 100 THEN SOUND 493.88, 10
IF B = 95 THEN SOUND 523.25, 10
IF B = 90 THEN SOUND 587.33, 10
IF B = 85 THEN SOUND 659.26, 10
IF B = 80 THEN SOUND 698.46, 10
IF B = 75 THEN SOUND 783.99, 10
IF B = 70 THEN SOUND 880, 10
END SUB

SUB lecon
octave = 3
duree = 40
minute = 120
morceau$ = ""

PLAY "o" + STR$(octave) + "l" + STR$(duree) + "t" + STR$(minute) + "MB"

CLS
SCREEN 9
COLOR 12: LOCATE 1, 35: PRINT "Le‡ons..."
COLOR 11
'1ere port‚e
LINE (0, 80 + plus)-(640, 80 + plus), 3
LINE (0, 90 + plus)-(640, 90 + plus), 3
LINE (0, 100 + plus)-(640, 100 + plus), 3
LINE (0, 110 + plus)-(640, 110 + plus), 3
LINE (0, 120 + plus)-(640, 120 + plus), 3
'2 eme port‚e
LINE (0, 80 + 60)-(640, 80 + 60), 3
LINE (0, 90 + 60)-(640, 90 + 60), 3
LINE (0, 100 + 60)-(640, 100 + 60), 3
LINE (0, 110 + 60)-(640, 110 + 60), 3
LINE (0, 120 + 60)-(640, 120 + 60), 3
LOCATE 3, 1: COLOR 12: PRINT "Cl‚ de sol"
LOCATE 16, 1: PRINT "Cl‚ de fa"
mot = 6
lec = 50
lec2 = 135
ref = 110
clesol
ref = 110 + 60
clefa
PLAY "c4d4e4f4g4a4b4>c4d4e4f4g4a4"
tour = PLAY(1)
850
a$ = INKEY$: IF LEN(a$) = 1 THEN 687 ELSE 688

687 IF ASC(a$) = 27 THEN 975
688
tour2 = PLAY(1)
IF tour2 = tour THEN tour = tour2: GOTO 850
tour = tour2
lec = lec + 40
lec2 = lec2 - 5
mot = mot + 5
B = lec2

B = 0

FOR d = 4 TO 1 STEP -1: CIRCLE (lec, lec2), d, 11: NEXT:        'dessin de note
IF lec2 = 130 OR lec2 = 70 THEN LINE (lec - 8, lec2)-(lec + 8, lec2), 11
IF lec2 >= 95 THEN plusq = -20
IF lec2 < 96 THEN plusq = 20
IF plusq = 20 THEN gadr = -4 ELSE gadr = 4
LINE (lec + gadr, lec2)-(lec + gadr, lec2 + plusq), 11
B = lec2
notesol

COLOR 10
LOCATE 4, mot: PRINT not$

IF lec2 = 70 THEN 859
GOTO 850
859
lecon2
975
END SUB

SUB lecon2
COLOR 12: LOCATE 1, 35: PRINT "Le‡ons..."
COLOR 11
'1ere port‚e
'LINE (0, 80 + plus)-(640, 80 + plus), 3
'LINE (0, 90 + plus)-(640, 90 + plus), 3
'LINE (0, 100 + plus)-(640, 100 + plus), 3
'LINE (0, 110 + plus)-(640, 110 + plus), 3
'LINE (0, 120 + plus)-(640, 120 + plus), 3
'2 eme port‚e
'LINE (0, 80 + 60)-(640, 80 + 60), 3
'LINE (0, 90 + 60)-(640, 90 + 60), 3
'LINE (0, 100 + 60)-(640, 100 + 60), 3
'LINE (0, 110 + 60)-(640, 110 + 60), 3
'LINE (0, 120 + 60)-(640, 120 + 60), 3

mot = 6
lec = 50
lec2 = 125
ref = 110
clesol
ref = 110 + 60
clefa
PLAY "c4<b4a4g4f4e4d4c4<b4a4g4f4e4"
tour2 = PLAY(1)
851
a$ = INKEY$: IF LEN(a$) = 1 THEN 677 ELSE 678

677 IF ASC(a$) = 27 THEN 965
678
tour = PLAY(1)
IF tour = tour2 THEN tour2 = tour: GOTO 851
tour2 = tour

lec = lec + 40
lec2 = lec2 + 5
mot = mot + 5
B = lec2
pouet = 60

pouet = 0
B = 0
FOR d = 4 TO 1 STEP -1: CIRCLE (lec, lec2), d, 11: NEXT:        'dessin de note
IF lec2 = 190 OR lec2 = 130 THEN LINE (lec - 8, lec2)-(lec + 8, lec2), 11
IF lec2 > 94 + 60 THEN plusq = -20
IF lec2 <= 95 + 60 THEN plusq = 20
IF plusq = 20 THEN gadr = -4 ELSE gadr = 4
LINE (lec + gadr, lec2)-(lec + gadr, lec2 + plusq), 11
B = lec2
pouet = 60
notefa

COLOR 10
LOCATE 15, mot: PRINT not$


ralenti
IF lec2 = 190 THEN 857
GOTO 851
857
apui
965
END SUB

SUB ligne
IF dictee = 0 THEN
SCREEN 9: COLOR 12: LOCATE 1, 32: PRINT "Lecture de notes": COLOR 11
ELSE
SCREEN 9: COLOR 12: LOCATE 1, 32: PRINT "Dict‚e musicale": COLOR 11
END IF
LINE (0, 80 + plus)-(640, 80 + plus), 3
LINE (0, 90 + plus)-(640, 90 + plus), 3
LINE (0, 100 + plus)-(640, 100 + plus), 3
LINE (0, 110 + plus)-(640, 110 + plus), 3
LINE (0, 120 + plus)-(640, 120 + plus), 3
COLOR 14: LOCATE 1, 51: PRINT "Score:"; poin

IF cle = 0 THEN clesol
IF cle = 1 THEN clefa

IF temp = 1 AND nombre = 13 AND aff = 1 THEN 75 ELSE 76
75 COLOR 10: LOCATE 13, 33: PRINT CHR$(218); STRING$(12, 196); CHR$(191)
LOCATE 14, 33: PRINT CHR$(179): LOCATE 14, 46: PRINT CHR$(179)
LOCATE 15, 33: PRINT CHR$(192); STRING$(12, 196); CHR$(217)
76
IF temp = 0 AND aff = 1 THEN
58 colr = INT(RND * 16)
IF colr = 0 THEN 58
COLOR colr
LOCATE 12, 15: PRINT "ÛÛÛÛÛÛÛÛÛ    ÛÛÛÛÛÛÛÛÛ ÛÛÛÛÛÛÛÛ    ÛÛ    ÛÛ      ÛÛ"
LOCATE 13, 15: PRINT "Û        Û   Û       Û Û          Û  Û   Û Û    Û Û"
LOCATE 14, 15: PRINT "Û         Û  Û       Û Û         Û    Û  Û  Û  Û  Û"
LOCATE 15, 15: PRINT "Û          Û Û       Û Û        Û      Û Û   ÛÛ   Û"
LOCATE 16, 15: PRINT "Û         Û  Û       Û Û   ÛÛÛÛ ÛÛÛÛÛÛÛÛ Û        Û"
LOCATE 17, 15: PRINT "Û        Û   Û       Û Û      Û Û      Û Û        Û"
LOCATE 18, 15: PRINT "ÛÛÛÛÛÛÛÛÛ    ÛÛÛÛÛÛÛÛÛ ÛÛÛÛÛÛÛÛ Û      Û Û        Û"
END IF
cl = 9
IF aff <> 1 AND cle = 0 THEN
LINE (50, 140)-(570, 250), cl, B
FOR sep = 520 / 13 TO 520 STEP 520 / 13
LINE (sep + 50, 140)-(sep + 50, 250), cl
NEXT
FOR tnoir = 520 / 13 TO 520 STEP 520 / 13
k = k + 1
IF k = 3 OR k = 7 OR k = 10 THEN 921
IF k = 13 THEN fine = (520 / 13) / 4
LINE (tnoir + 50 - (520 / 13) / 6, 140)-(tnoir + 50 + (520 / 13) / 6 - fine, 210), cl, B
fine = 0
921
NEXT
'couleur des touches noires
k = 0
FOR tnoir = 520 / 13 TO 520 STEP 520 / 13
k = k + 1
IF k = 3 OR k = 7 OR k = 10 THEN 922
PAINT (tnoir + 50 - (520 / 13) / 6 + 1, 140 + 5), cl
IF k = 13 THEN 923
PAINT (tnoir + 50 + (520 / 13) / 6 - 1, 140 + 5), cl
922
NEXT
923
LINE (566, 140)-(570, 210), cl, B
PAINT (569, 141), cl

                                   






ELSEIF aff <> 1 AND cle = 1 THEN
LINE (50, 140)-(570, 250), cl, B
FOR sep = 520 / 13 TO 520 STEP 520 / 13
LINE (sep + 50, 140)-(sep + 50, 250), cl
NEXT
FOR tnoir = 520 / 13 TO 520 STEP 520 / 13
k = k + 1
IF k = 1 OR k = 5 OR k = 8 OR k = 12 THEN 926
IF k = 13 THEN fine = (520 / 13) / 4
LINE (tnoir + 50 - (520 / 13) / 6, 140)-(tnoir + 50 + (520 / 13) / 6 - fine, 210), cl, B
fine = 0
926
NEXT
'couleur des touches noires
k = 0
FOR tnoir = 520 / 13 TO 520 STEP 520 / 13
k = k + 1
IF k = 1 OR k = 5 OR k = 8 OR k = 12 THEN 927
PAINT (tnoir + 50 - (520 / 13) / 6 + 1, 140 + 5), cl
IF k = 13 THEN 928
PAINT (tnoir + 50 + (520 / 13) / 6 - 1, 140 + 5), cl
927
NEXT
928
LINE (566, 140)-(570, 210), cl, B
PAINT (569, 141), cl

END IF





END SUB

SUB ligneair
SCREEN 9
LINE (0, 80 + plus)-(640, 80 + plus), 3
LINE (0, 90 + plus)-(640, 90 + plus), 3
LINE (0, 100 + plus)-(640, 100 + plus), 3
LINE (0, 110 + plus)-(640, 110 + plus), 3
LINE (0, 120 + plus)-(640, 120 + plus), 3
IF cle = 0 THEN clesol
IF cle = 1 THEN clefa

END SUB

SUB melange
vc = 0: vc2 = 0
IF aff = 1 THEN
        COLOR 10
        dez = 9

670     IF cle = 0 THEN asar ELSE asar3
                LOCATE 20, dez: PRINT ha$
                dez = dez + 5
                IF dez = 9 + 5 * 6 THEN nada2 = 0 ELSE 670
                vc = 0: vc2 = 0
                COLOR 11
                
680     IF cle = 0 THEN asar2 ELSE ASAR4
                LOCATE 20, dez: PRINT ha$
                dez = dez + 5
                IF dez = 9 + 5 * 6 + 5 * 7 THEN nada2 = 0 ELSE 680
                vc = 0: vc2 = 0
ELSEIF aff = 2 OR aff = 0 THEN
        COLOR 10
        dez = 9

674     IF cle = 0 THEN asar ELSE asar3
                LOCATE 20, dez: PRINT ha$
                dez = dez + 5
                IF dez = 9 + 5 * 6 THEN nada2 = 0 ELSE 674
                vc = 0: vc2 = 0
                COLOR 11
                
681     IF cle = 0 THEN asar2 ELSE ASAR4
                LOCATE 20, dez: PRINT ha$
                dez = dez + 5
                IF dez = 9 + 5 * 6 + 5 * 7 THEN nada2 = 0 ELSE 681
                vc = 0: vc2 = 0
                ELSEIF aff = 2 THEN
END IF
IF aff = 2 THEN
LOCATE 20, 1: PRINT "                                                                                          "
END IF
        COLOR 13
        LOCATE 19, 1: PRINT "Perdu"
        'LOCATE 20, 1: PRINT "-----"
        LOCATE 19, 75: PRINT "Gagn‚"
        'LOCATE 20, 75: PRINT "-----"
END SUB

SUB Menu
aide = 0
IF primo = 0 THEN 53
primo = 1
IF temp = 1 AND aff = 1 AND nombre = 13 THEN score ELSE 53

54 GOTO 53

53 poin = 0: primo = 1: SCREEN 0: CLS : plus = 0: a = 90: l = 11: plus = 0: p = 4: kd = 0: tep = 0: frt = 0: uti0 = 0: uti1 = 0: uti2 = 0: uti3 = 0: uti4 = 0: uti5 = 0: uti6 = 0: uti7 = 0: uti8 = 0: uti9 = 0: uti10 = 0: uti11 = 0: dod = 0: re = 0: _
                                                      mi = 0: fa = 0: sol = 0: la = 0: poin = 0
57 colr = INT(RND * 16)
IF colr = 0 THEN 57
COLOR colr
LOCATE 16, 10: PRINT "ÛÛÛÛÛÛÛÛÛ    ÛÛÛÛÛÛÛÛÛ ÛÛÛÛÛÛÛÛ    ÛÛ    ÛÛ      ÛÛ"
LOCATE 17, 10: PRINT "Û        Û   Û       Û Û          Û  Û   Û Û    Û Û"
LOCATE 18, 10: PRINT "Û         Û  Û       Û Û         Û    Û  Û  Û  Û  Û"
LOCATE 19, 10: PRINT "Û          Û Û       Û Û        Û      Û Û   ÛÛ   Û"
LOCATE 20, 10: PRINT "Û         Û  Û       Û Û   ÛÛÛÛ ÛÛÛÛÛÛÛÛ Û        Û"
LOCATE 21, 10: PRINT "Û        Û   Û       Û Û      Û Û      Û Û        Û"
LOCATE 22, 10: PRINT "ÛÛÛÛÛÛÛÛÛ    ÛÛÛÛÛÛÛÛÛ ÛÛÛÛÛÛÛÛ Û      Û Û        Û"

5 LOCATE 1, 35: COLOR 12: PRINT "Menu"
LOCATE 1, 60: COLOR 14: PRINT "F1 = Aide"
LOCATE 4, 10: COLOR 10: PRINT "1 - Cl‚ de"
IF cle = 0 THEN 10 ELSE 20
10 COLOR 14: LOCATE 4, 21: PRINT "sol": GOTO 30
20 COLOR 14: LOCATE 4, 21: PRINT "fa "
30 LOCATE 5, 10: COLOR 10: PRINT "2 - Musique"
IF music = 0 THEN 40 ELSE 50
40 LOCATE 5, 22: COLOR 14: PRINT "Non": GOTO 60
50 LOCATE 5, 22: COLOR 14: PRINT "Oui"
60 LOCATE 6, 10: COLOR 10: PRINT "3 - ": LOCATE 6, 17: PRINT "notes"
COLOR 14: LOCATE 6, 13: PRINT nombre
COLOR 10: LOCATE 7, 10: PRINT "4 - Temps"
IF temp = 0 THEN 65 ELSE 66
65 COLOR 14: LOCATE 7, 20: PRINT "Non": GOTO 67
66 COLOR 14: LOCATE 7, 20: PRINT "Oui"
67 COLOR 10: LOCATE 12, 10: PRINT "8 - Quitter"
LOCATE 10, 10: PRINT "6 - Exercice sur la lecture des notes"
LOCATE 11, 10: PRINT "7 - Dict‚e musicale"
REM LOCATE 12, 10: PRINT "8 - Petit air"
LOCATE 8, 10: PRINT "5 - Notes ‚crites    , Clavier dessin‚      "
IF aff = 0 THEN
LOCATE 8, 28: COLOR 14: PRINT "Oui"
LOCATE 8, 49: PRINT "Oui"
ELSEIF aff = 1 THEN
LOCATE 8, 28: COLOR 14: PRINT "Oui"
LOCATE 8, 49: PRINT "Non"
ELSEIF aff = 2 THEN
LOCATE 8, 28: COLOR 14: PRINT "Non"
LOCATE 8, 49: PRINT "Oui"
END IF
COLOR 10
LOCATE 14, 20: PRINT "Tapez un chiffre de 1 … 8 comme s‚lection:"
IF nombre = 13 AND temp = 1 AND aff = 1 THEN
COLOR 13
LOCATE 3, 10: PRINT "Mode score"
ELSE
LOCATE 3, 10: PRINT "           "
END IF
70 COLOR 7: LOCATE 3, 42: PRINT "Heure: "; TIME$
a$ = INKEY$: IF LEN(a$) = 0 THEN 70
mv = ASC(RIGHT$(a$, 1))
IF mv = 49 THEN 80 ELSE 85
80 IF cle = 0 THEN cle = 1 ELSE cle = 0
GOTO 5
85 IF mv = 53 THEN 325 ELSE 90
325 IF aff = 0 THEN
aff = 1: GOTO 5
ELSEIF aff = 1 THEN
aff = 2: GOTO 5
ELSEIF aff = 2 THEN
aff = 0: GOTO 5
END IF
90 IF mv = 50 THEN 100 ELSE 110
100 IF music = 0 THEN music = 1 ELSE music = 0
GOTO 5
110 IF mv = 51 THEN nombre = nombre - 1 ELSE 120
IF nombre = 2 THEN nombre = 13
GOTO 5
120 IF mv = 56 OR mv = 27 THEN rend = 1 ELSE 121
GOTO 130
121 IF mv = 54 THEN 130 ELSE 125
125 IF mv = 52 THEN 126 ELSE 128
126 IF temp = 0 THEN temp = 1 ELSE 127
GOTO 5
127 temp = 0: GOTO 5
128
IF mv = 13 AND aide = -1 THEN
aide = 0
GOTO 53
END IF
IF mv = 13 AND aide > 0 THEN
menuhelp
END IF
IF mv = 59 THEN
aide = 0
menuhelp
GOTO 5
END IF
IF mv = 55 THEN
dictee = 1
CLS : GOTO 130
END IF
GOTO 5
130 CLS : mva = 0: plus = 0: abin = 320: abon = 320: frt = 50 + (540 / 13) / 2: tome$ = TIME$: gf = 1.57 - 6.28 / 60: ha$ = "": uti0 = 0: uti1 = 0: uti2 = 0: uti3 = 0: uti4 = 0: uti5 = 0: uti6 = 0: uti7 = 0: uti8 = 0: uti9 = 0: uti10 = 0: uti11 = 0 _
: uti12 = 0: uti6 = 0
octave = 3
duree = 40
minute = 60
loref = 12
morceau$ = ""

PLAY "o" + STR$(octave) + "l" + STR$(duree) + "t" + STR$(minute) + "MB"

END SUB

SUB ralenti
FOR espaces = 1 TO 500: NEXT
END SUB

