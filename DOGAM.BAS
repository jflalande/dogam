DECLARE SUB present2 ()
DECLARE SUB enregistre ()
DECLARE SUB felizfa ()
DECLARE SUB vitesse ()
DECLARE SUB lenom ()
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
COMMON SHARED nombre, music, a, B, f, rep, bon, not$, L, g, plus, poin
COMMON SHARED ref, temp, p, inte, plusq, esp, tep, kd, gf, tome$, vol
COMMON SHARED ha$, vc, uti0, uti1, uti2, uti3, uti4, uti5, dez, uti6
COMMON SHARED uti7, uti8, uti9, uti10, uti11, frt, colp, dod, re, mi
COMMON SHARED fa, sol, la, si, dod2, re2, mi2, fa2, sol2, bar, nb
COMMON SHARED points, nom$, primo, rend, porq, sop, sup, pouet, uti12
COMMON SHARED la2, aff, vc2, bem, dictee, cligne, colcli, aide, voiraide
COMMON SHARED adictee, bdictee, adicteer, bdicteer, sens, fichier$, noplace, lavitesse
COMMON SHARED placex, placey

DIM SHARED musi(1000, 3) AS SINGLE
DIM SHARED dogpres(51, 7) AS INTEGER

ON ERROR GOTO 3
RANDOMIZE TIMER
nombre = 13: a = 90: L = 11: ref = 110: p = 4: temp = 1: gf = 1.57: dictee = 0
frt = 60 + (520 / 13) / 2
mes = 13: abon = 320: abin = 320: bbin = 285: bbon = 285: col = 12
placex = 14: placey = 13
fichier$ = "-SANSNOM-"

enregistre
aff = 1
present
expli
456 rend = 0
grandchoix
490 Menu
mva = 0
IF rend = 1 THEN 456
ref = 110
IF dictee = 1 THEN temp = 0
ligne
melange
colp = 11
bonn
deplace
pass = 1
adictee = 40
510
sautacrou
bdictee = 130
sens = -5
IF fin = 1 THEN
fin = 0
GOTO 490
END IF

IF nombre < 13 OR aff < 1 OR aff > 1 THEN temp = 0
COLOR 14
LOCATE 1, 2: PRINT "F1 = AIDE"
COLOR 15
LOCATE 1, 13: PRINT "F4 = PAUSE"
LOCATE 1, 62: PRINT "ESC = Quitter"
IF dictee = 1 THEN
COLOR 13
temp = 0
music = 1
LOCATE 3, 23: PRINT "F2 = Rejoue le LA"
COLOR 15
LOCATE 3, 47: PRINT "F3 = Note Ö trouver"
COLOR 12
IF pass = 1 THEN
LOCATE p, L: PRINT "LA"
IF cle = 0 THEN LOCATE p + 1, L: PRINT "440"
IF cle = 1 THEN LOCATE p + 1, L: PRINT "220"
IF cle = 0 THEN B = 105 ELSE B = 80
IF cle = 0 THEN jouesol ELSE jouefa
vol = 15
FOR d = 4 TO 1 STEP -1: CIRCLE (a, B + plus), d, vol: NEXT:        'dessin de note
IF B = 130 OR B = 70 THEN LINE (a - 8, B + plus)-(a + 8, B + plus), vol
queue
L = L + 5
a = a + 40
END IF
adictee = a
misezero = INT(TIMER)
DO
COLOR 15
LOCATE 3, 4: PRINT "Heure: "; TIME$
LOOP WHILE INT(TIMER) < misezero + .5 AND TIMER > 5
pass = 0
END IF
COLOR 15
LOCATE p, L: PRINT "??"
cligne = 0
colcli = 15
choixnotes
IF B < 105 THEN vol = 11 ELSE vol = 10
IF dictee = 0 THEN
FOR d = 4 TO 1 STEP -1: CIRCLE (a, B + plus), d, vol: NEXT:        'dessin de note
IF B = 130 OR B = 70 THEN LINE (a - 8, B + plus)-(a + 8, B + plus), vol
queue
END IF

500 COLOR 15: LOCATE 3, 4: PRINT "Heure: "; TIME$
IF cligne < 3 AND dictee = 1 THEN
cligne = cligne + 1
ELSEIF dictee = 1 AND cligne = 3 AND colcli = 15 THEN
colcli = 0
cligne = 0
ELSEIF dictee = 1 AND cligne = 3 AND colcli = 0 THEN
colcli = 15
cligne = 0
ELSEIF cligne < 3 AND dictee = 0 THEN
cligne = cligne + 1
ELSEIF dictee = 0 AND cligne = 3 AND colcli = 15 THEN
colcli = 0
cligne = 0
ELSEIF dictee = 0 AND cligne = 3 AND colcli = 0 THEN
colcli = 15
cligne = 0

END IF

COLOR colcli
LOCATE p, L: PRINT "??"
IF tep = 1 AND temp = 1 THEN 490
IF temp = 1 THEN geretemp
IF dictee = 1 THEN
vol = 0
FOR d = 4 TO 1 STEP -1: CIRCLE (adicteer, bdicteer + plus), d, vol: NEXT:        'dessin de note
IF bdicteer = 130 OR bdicteer = 70 THEN LINE (adicteer - 8, bdicteer + plus)-(adicteer + 8, bdicteer + plus), vol
IF bdicteer = 130 OR bdicteer = 125 OR bdicteer = 120 OR bdicteer = 115 OR bdicteer = 110 OR bdicteer = 105 OR bdicteer = 100 THEN plusq = -20
IF bdicteer < 96 THEN plusq = 20
IF plusq = 20 THEN gadr = -4 ELSE gadr = 4
LINE (adicteer + gadr, bdicteer + plus)-(adicteer + gadr, bdicteer + plusq + plus), vol
boutligne
bdictee = bdictee + sens

IF bdictee = 65 THEN
bdictee = 70
sens = 5
ELSEIF bdictee = 135 THEN
bdictee = 130
sens = -5
END IF
bdicteer = bdictee
adicteer = adictee

IF bdictee < 105 THEN vol = 11 ELSE vol = 10
FOR d = 4 TO 1 STEP -1: CIRCLE (adictee, bdictee + plus), d, vol: NEXT:        'dessin de note
IF bdictee = 130 OR bdictee = 70 THEN LINE (adictee - 8, bdictee + plus)-(adictee + 8, bdictee + plus), vol
IF bdictee = 130 OR bdictee = 125 OR bdictee = 120 OR bdictee = 115 OR bdictee = 110 OR bdictee = 105 OR bdictee = 100 THEN plusq = -20
IF bdictee < 96 THEN plusq = 20
IF plusq = 20 THEN gadr = -4 ELSE gadr = 4
LINE (adictee + gadr, bdictee + plus)-(adictee + gadr, bdictee + plusq + plus), vol
END IF
misezero = TIMER
5681
IF TIMER >= misezero + .01 OR TIMER < 5 THEN 500
a$ = INKEY$: IF a$ = "" THEN 5681'verification touches
mv = ASC(RIGHT$(a$, 1))
vol = 0
FOR d = 4 TO 1 STEP -1: CIRCLE (adicteer, bdicteer + plus), d, vol: NEXT:        'dessin de note
IF bdicteer = 130 OR bdicteer = 70 THEN LINE (adicteer - 8, bdicteer + plus)-(adicteer + 8, bdicteer + plus), vol
IF bdicteer = 130 OR bdicteer = 125 OR bdicteer = 120 OR bdicteer = 115 OR bdicteer = 110 OR bdicteer = 105 OR bdicteer = 100 THEN plusq = -20
IF bdicteer < 96 THEN plusq = 20
IF plusq = 20 THEN gadr = -4 ELSE gadr = 4
LINE (adicteer + gadr, bdicteer + plus)-(adicteer + gadr, bdicteer + plusq + plus), vol
boutligne
adicteer = -100
IF B < 105 THEN vol = 11 ELSE vol = 10
IF tep = 1 AND temp = 1 THEN 490
IF temp = 1 THEN geretemp
rep = 0
IF mv = 62 THEN paused
IF mv = 60 AND dictee = 1 AND cle = 0 THEN SOUND 440, 10
IF mv = 60 AND dictee = 1 AND cle = 1 THEN SOUND 220, 10
IF mv = 61 AND cle = 0 AND dictee = 1 THEN jouesol
IF mv = 61 AND cle = 1 AND dictee = 1 THEN jouefa
IF mv = 59 THEN testhelpmenu
IF mv = 27 THEN
IF dictee = 1 THEN dictee = 0
GOTO 490
END IF
colp = 0: bonn


IF mv = 75 THEN frt = frt - 520 / 13
IF mv = 77 THEN frt = frt + 520 / 13
IF frt < 50 + (520 / 13) / 2 THEN frt = 50 + (520 / 13) / 2
IF frt > 550 THEN frt = 550
colp = 11: bonn
IF mv = 32 THEN rep = 50
IF rep = 0 THEN 500

attention



IF bon = 0 THEN 700
IF cle = 0 THEN notesol
IF cle = 1 THEN notefa
COLOR 5: LOCATE p, L: PRINT not$
IF dictee = 1 THEN
FOR d = 4 TO 1 STEP -1: CIRCLE (a, B + plus), d, vol: NEXT:        'dessin de note
IF B = 130 OR B = 70 THEN LINE (a - 8, B + plus)-(a + 8, B + plus), vol
queue
END IF
poin = poin + 1
COLOR 14: LOCATE 1, 51: PRINT "Score:"; poin
'IF mv = 75 THEN gauche
'IF mv = 77 THEN droite
'IF mv = 72 THEN saut
'IF mv = 80 THEN acroupi
'IF mv = 27 THEN END
mva = 77
deplace

IF temp = 1 AND nombre = 13 AND aff = 1 THEN phrases
600 a = a + 40: L = L + 5
IF a = 650 THEN efface




GOTO 510


700 REM faux
COLOR 12
FOR d = 4 TO 1 STEP -1: CIRCLE (a, B + plus), d, 12: NEXT:        'dessin de note
IF B = 130 OR B = 70 THEN LINE (a - 8, B + plus)-(a + 8, B + plus), 12
queuerouge
IF cle = 0 THEN notesol
IF cle = 1 THEN notefa
COLOR 12: LOCATE p, L: PRINT not$
mva = 75
deplace
GOTO 600

3 CLOSE
IF ERR = 53 THEN
OPEN "score.pia" FOR OUTPUT AS #1
points = 3: nom$ = "Einstein"

WRITE #1, nom$, points
CLOSE
OPEN "scorefa.pia" FOR OUTPUT AS #1
points = 3: nom$ = "Einstein"

WRITE #1, nom$, points
CLOSE

RESUME
ELSE
CLS
PRINT "Erreur dans le programme DOGAM..."
PRINT DATE$
PRINT TIME$
apui
END
END IF


DATA " DOGAM, le jeu du solfäge et de la musique...   "
DATA "  DOGAM, pour apprendre en s'amusant...   "
DATA "  Programmation Jean Franáois Lalande   "

SUB enregistre

COLOR 11
'LOCATE 3, 15: PRINT "€€€€€€€€€    €€€€€€€€€ €€€€€€€€    €€    €€      €€"
'LOCATE 4, 15: PRINT "€        €   €       € €          €  €   € €    € €"
'LOCATE 5, 15: PRINT "€         €  €       € €         €    €  €  €  €  €"
'LOCATE 6, 15: PRINT "€          € €       € €        €      € €   €€   €"
'LOCATE 7, 15: PRINT "€         €  €       € €   €€€€ €€€€€€€€ €        €"
'LOCATE 8, 15: PRINT "€        €   €       € €      € €      € €        €"
'LOCATE 9, 15: PRINT "€€€€€€€€€    €€€€€€€€€ €€€€€€€€ €      € €        €"
'Ligne 1
FOR rthyt = 1 TO 9: dogpres(rthyt, 1) = 1: NEXT
FOR rthyt = 14 TO 22: dogpres(rthyt, 1) = 1: NEXT
FOR rthyt = 24 TO 31: dogpres(rthyt, 1) = 1: NEXT
FOR rthyt = 36 TO 37: dogpres(rthyt, 1) = 1: NEXT
FOR rthyt = 42 TO 43: dogpres(rthyt, 1) = 1: NEXT
FOR rthyt = 50 TO 51: dogpres(rthyt, 1) = 1: NEXT
'Ligne 7
FOR rthyt = 1 TO 9: dogpres(rthyt, 7) = 1: NEXT
FOR rthyt = 14 TO 22: dogpres(rthyt, 7) = 1: NEXT
FOR rthyt = 24 TO 31: dogpres(rthyt, 7) = 1: NEXT
dogpres(33, 7) = 1: dogpres(40, 7) = 1: dogpres(42, 7) = 1: dogpres(51, 7) = 1
'colonne 1
FOR rthyt = 2 TO 6: dogpres(1, rthyt) = 1: NEXT
'colonne 10-11-12
dogpres(10, 2) = 1: dogpres(11, 3) = 1: dogpres(12, 4) = 1
dogpres(11, 5) = 1: dogpres(10, 6) = 1
' colonne 14-22-24
FOR rthyt = 2 TO 6: dogpres(14, rthyt) = 1: NEXT
FOR rthyt = 2 TO 6: dogpres(22, rthyt) = 1: NEXT
FOR rthyt = 2 TO 6: dogpres(24, rthyt) = 1: NEXT
' ligne 5 (g) et (a)
FOR rthyt = 28 TO 31: dogpres(rthyt, 5) = 1: NEXT
FOR rthyt = 33 TO 40: dogpres(rthyt, 5) = 1: NEXT
' colonne 31
dogpres(31, 6) = 1
' collone 33 et 40
FOR rthyt = 4 TO 7: dogpres(33, rthyt) = 1: dogpres(40, rthyt) = 1: NEXT
' haut du A
dogpres(34, 3) = 1: dogpres(35, 2) = 1: dogpres(36, 1) = 1
dogpres(37, 1) = 1: dogpres(38, 2) = 1: dogpres(39, 3) = 1
' colonne 42 et 51
FOR rthyt = 2 TO 6: dogpres(42, rthyt) = 1: dogpres(51, rthyt) = 1: NEXT
' milieu du M
dogpres(44, 2) = 1: dogpres(45, 3) = 1: dogpres(46, 4) = 1
dogpres(47, 4) = 1: dogpres(48, 3) = 1: dogpres(49, 2) = 1


END SUB

SUB graphique
CLS
SCREEN 9
RANDOMIZE TIMER
FOR i = 1 TO 50
a$ = INKEY$: IF LEN(a$) = 1 THEN 652
a = INT(RND * 640)
a1 = INT(RND * 640)
B = INT(RND * 350)
b1 = INT(RND * 350)
vol = INT(RND * 15)
LINE (a, B)-(a1, b1), vol
FOR d = 1 TO 4: CIRCLE (a, B + plus), d, vol: NEXT:       'dessin de note
IF B = 130 THEN LINE (a - 8, 130 + plus)-(a + 8, 130 + plus), 11
queue
856 bg = INT(RND * 6) + 1'3000
'IF bg < 38 THEN 856
'SOUND bg, .5
PLAY "o" + STR$(bg) + "a35d35"
DO: LOOP WHILE PLAY(1) <> 0
NEXT
fr = 500
568 fr = fr + 2
SOUND fr, .2
a$ = INKEY$: IF LEN(a$) = 1 THEN 652
IF fr >= 1000 THEN 785
GOTO 568

785 FOR t = 1 TO 2
FOR col = 1 TO 15
CIRCLE (280, 240), 60, col
PAINT (301, 200), col
LINE (340, 240)-(340, 25), col
a$ = INKEY$: IF LEN(a$) = 1 THEN 652

misezero = TIMER
DO: LOOP WHILE TIMER < misezero + .1 AND TIMER > 5
NEXT
NEXT
'COLOR 11: LOCATE 6, 48: PRINT "D"
'FOR gt = 1 TO 950: NEXT
'LOCATE 6, 51: PRINT "A"
'FOR gt = 1 TO 950: NEXT
'LOCATE 6, 52: PRINT "M"
'FOR gt = 1 TO 950: NEXT
'LOCATE 6, 49: PRINT "O"
'FOR gt = 1 TO 950: NEXT
'LOCATE 6, 50: PRINT "G"
'END
COLOR 11
LOCATE 3, 15: PRINT "€€€€€€€€€    €€€€€€€€€ €€€€€€€€    €€    €€      €€"
LOCATE 4, 15: PRINT "€        €   €       € €          €  €   € €    € €"
LOCATE 5, 15: PRINT "€         €  €       € €         €    €  €  €  €  €"
LOCATE 6, 15: PRINT "€          € €       € €        €      € €   €€   €"
LOCATE 7, 15: PRINT "€         €  €       € €   €€€€ €€€€€€€€ €        €"
LOCATE 8, 15: PRINT "€        €   €       € €      € €      € €        €"
LOCATE 9, 15: PRINT "€€€€€€€€€    €€€€€€€€€ €€€€€€€€ €      € €        €"





COLOR 12
565 READ d$
w = LEN(d$)
c$ = LEFT$(d$, w / 2)
c1$ = RIGHT$(d$, w / 2)

fg = 1: gh = 1: h = 80: po = 0
562 ty$ = RIGHT$(c$, fg): ty2$ = LEFT$(c1$, fg)
 LOCATE 11, gh: PRINT ty$:
 LOCATE 11, h: PRINT ty2$
IF LEN(ty$) >= LEN(c$) THEN 528
fg = fg + 1
GOTO 563
528
gh = gh + 1
569
563 h = h - 1:
a$ = INKEY$: IF LEN(a$) = 1 THEN 295
misezero = TIMER
DO: LOOP WHILE TIMER < misezero + .01 AND TIMER > 5

IF h = 40 THEN 564
GOTO 562
564 SLEEP 1
IF LEN(d$) = 40 THEN RESTORE
GOTO 565
652
295 END SUB

SUB menuhelp
COLOR 14
aide = aide + 1
LOCATE 15, 1: PRINT CHR$(218); STRING$(78, 196); CHR$(191)
FOR cadre = 17 TO 22
PRINT CHR$(179); STRING$(78, 32); CHR$(179)
NEXT
PRINT CHR$(192); STRING$(78, 196); CHR$(217)
COLOR 13
LOCATE 22, 40: PRINT "Appuyez sur ENTREE pour poursuivre."
COLOR 15
' 6 lignes
IF aide = 1 THEN
FOR loca = 4 TO 8
LOCATE loca, 58: PRINT CHR$(179)
NEXT
FOR loca = 10 TO 12
LOCATE loca, 58: PRINT CHR$(179)
NEXT
LOCATE 6, 59: PRINT "RÇglages"
LOCATE 11, 59: PRINT "Actions"
LOCATE 16, 2: PRINT "Aide:"
LOCATE 18, 2: PRINT "Ce menu prÇcise les options avant l'exÇcution du test.Vous pouvez les modifier"
LOCATE 19, 2: PRINT "en appuyant sur les touches de -1- Ö -5-."
LOCATE 21, 2: PRINT "La clÇ: peut àtre une clÇ de sol ou de fa (tapez 1 pour passer de sol Ö fa)."
ELSEIF aide = 2 THEN
LOCATE 16, 2: PRINT "La musique peut àtre activÇe pour entendre le son de la note jouÇe pendant le"
LOCATE 17, 2: PRINT "test. Cette option s'active automatiquement lors de la dictÇe musicale. Tapez"
LOCATE 18, 2: PRINT "-2- pour passer de -oui- Ö -non- et vice versa."
LOCATE 20, 2: PRINT "Le nombre de notes: En appuyant sur la touche -3- vous reduisez le nombre de"
LOCATE 21, 2: PRINT "notes Ö tester. Le minimum est de 3 notes et le maximum est de 13."
ELSEIF aide = 3 THEN
LOCATE 16, 2: PRINT "Le temps affiche une minuterie pendant le test de lecture des notes. Elle "
LOCATE 17, 2: PRINT "vous laisse une minute pour donner un maximum de noms de notes."
LOCATE 19, 2: PRINT "Notes/clavier: 3 modes sont possibles. Le 1er est le mode notes seules qui af-"
LOCATE 20, 2: PRINT "fiche les noms des notes dans le dÇsordre. Le 2Çme est le mode clavier de pia-"
LOCATE 21, 2: PRINT "no seul. Le dernier mode associe les deux deux premiers. Appuyer sur le -5-."
ELSEIF aide = 4 THEN
LOCATE 16, 2: PRINT "Les numÇros de -1- Ö -5- sont des rÇglages. Les parties de -6- Ö -9- sont des"
LOCATE 17, 2: PRINT "embranchements pour d'autres menus ou activitÇs."
LOCATE 19, 2: PRINT "Remarque: Si vous sÇlectionnez 13 notes, le temps, et les notes seules, un"
LOCATE 20, 2: PRINT "mode spÇcial se met en route et garde votre meilleur score. A ce moment il est"
LOCATE 21, 2: PRINT "affichÇ Ö l'Çcran -Mode score-."
ELSEIF aide = 5 THEN
LOCATE 16, 2: PRINT "Test:Lance le test avec toutes les options choisies (touche -6-)."
LOCATE 18, 2: PRINT "DictÇe musicale: la est jouÇe et vous devez l'inscrire sur la portÇe musicale"
LOCATE 19, 2: PRINT "(touche -7-)."
aide = -1
LOCATE 21, 2: PRINT "Quitter: Revient au SUPER MENU (Touche -8- ou ESC)"
FOR loca = 4 TO 14
LOCATE loca, 54: PRINT "               "
NEXT
END IF
END SUB

SUB notefa
IF B = 130 + pouet THEN not$ = "mi"
IF B = 125 + pouet THEN not$ = "fa"
IF B = 120 + pouet THEN not$ = "sol"
IF B = 115 + pouet THEN not$ = "la"
IF B = 110 + pouet THEN not$ = "si"
IF B = 105 + pouet THEN not$ = "do"
IF B = 100 + pouet THEN not$ = "rÇ"
IF B = 95 + pouet THEN not$ = "mi"
IF B = 90 + pouet THEN not$ = "fa"
IF B = 85 + pouet THEN not$ = "sol"
IF B = 80 + pouet THEN not$ = "la"
IF B = 75 + pouet THEN not$ = "si"
IF B = 70 + pouet THEN not$ = "do"
END SUB

SUB notesol
IF B = 130 THEN not$ = "do"
IF B = 125 THEN not$ = "rÇ"
IF B = 120 THEN not$ = "mi"
IF B = 115 THEN not$ = "fa"
IF B = 110 THEN not$ = "sol"
IF B = 105 THEN not$ = "la"
IF B = 100 THEN not$ = "si"
IF B = 95 THEN not$ = "do"
IF B = 90 THEN not$ = "rÇ"
IF B = 85 THEN not$ = "mi"
IF B = 80 THEN not$ = "fa"
IF B = 75 THEN not$ = "sol"
IF B = 70 THEN not$ = "la"
END SUB

SUB nul
CLS
COLOR 6: LOCATE 2, 34: PRINT "RESULTAT"
COLOR 12: LOCATE 10, 5: PRINT "Dommage mais "; nom$; " avait fait"; points; "points en 1 minute"
COLOR 14: LOCATE 15, 7: PRINT "alors que vous vous avez fait"; poin; "points."

apui
END SUB

SUB pareil
CLS
COLOR 6: LOCATE 2, 34: PRINT "RESULTAT"
COLOR 10: LOCATE 10, 5: PRINT "Pas de chance..."
COLOR 12: LOCATE 12, 4: PRINT nom$; " avait fait "; points; "points et vous pareil..."
COLOR 14: LOCATE 13, 4: PRINT "Je laisse "; nom$; " le gagnant..."
apui
END SUB

SUB paused
COLOR 12
tpsdebut = INT(TIMER)
1352
IF tpsdebut + 1 = INT(TIMER) THEN
LOCATE 1, 25: PRINT "PAUSE"
ELSEIF tpsdebut + 2 <= INT(TIMER) OR TIMER < 5 THEN
LOCATE 1, 25: PRINT "      "
tpsdebut = INT(TIMER)
END IF
a$ = INKEY$: IF LEN(a$) = 0 THEN 1352
mv = ASC(RIGHT$(a$, 1))
IF mv <> 62 THEN 1352
LOCATE 1, 25: PRINT "       "
END SUB

SUB pausedt
COLOR 12


tpsdebut = INT(TIMER)
1353
IF tpsdebut + 1 = INT(TIMER) THEN
LOCATE 1, 25: PRINT "PAUSE"
ELSEIF tpsdebut + 2 <= INT(TIMER) OR TIMER < 5 THEN
LOCATE 1, 25: PRINT "      "
tpsdebut = INT(TIMER)
END IF

a$ = INKEY$: IF LEN(a$) = 0 THEN 1353
mv = ASC(RIGHT$(a$, 1))
IF mv <> 13 AND mv <> 27 THEN 1353
LOCATE 1, 25: PRINT "       "
voiraide = mv
END SUB

SUB phrases
COLOR 12
IF poin = 5 THEN
LOCATE 14, 37: PRINT "Bof..."
ELSEIF poin = 10 THEN
LOCATE 14, 37: PRINT "Mieux..."
ELSEIF poin = 15 THEN
LOCATE 14, 34: PRINT "   Bien!!   "
ELSEIF poin = 20 THEN
LOCATE 14, 34: PRINT "  Super!!!  "
ELSEIF poin = 23 THEN
LOCATE 14, 34: PRINT " La bàte... "
ELSEIF poin = 25 THEN
LOCATE 14, 34: PRINT "  D'enfer!  "
ELSEIF poin = 28 THEN
LOCATE 14, 34: PRINT " Mega-super "
ELSEIF poin = 30 THEN
LOCATE 14, 34: PRINT " Fichtre... "
ELSEIF poin = 33 THEN
LOCATE 14, 34: PRINT "Perfection.."
ELSEIF poin = 35 THEN
LOCATE 14, 34: PRINT " Je me tais "
ELSEIF poin = 36 THEN
jk = 34
FOR ghj = 1 TO 12
LOCATE 14, jk: COLOR ghj: PRINT CHR$(1)
jk = jk + 1
NEXT
END IF

END SUB

SUB present
CLS
SCREEN 9
increment = 1.5
mesdo = 0
aprese = 340
Bpres = 100
haut = increment

1200
mesdo = mesdo + increment
aprese = aprese - 2.5 * haut
Bpres = Bpres - .5 * haut


cpres = 15

'cadre
LINE (aprese - (1 / 10) * mesdo, Bpres - (1 / 10) * mesdo)-(aprese + 4.5 * mesdo + (1 / 10) * mesdo, Bpres + mesdo + (1 / 10) * mesdo), cpres, B
' D
LINE (aprese, Bpres)-(aprese, Bpres + mesdo), cpres
LINE (aprese, Bpres + mesdo)-(aprese + mesdo / 2, Bpres + mesdo * (5 / 6)), cpres
LINE (aprese + mesdo / 2, Bpres + mesdo * (1 / 6))-(aprese, Bpres), cpres
LINE (aprese + mesdo / 2, Bpres + mesdo * (1 / 6))-(aprese + mesdo / 2, Bpres + mesdo * (5 / 6)), cpres
' O
aprese = aprese + mesdo
LINE (aprese, Bpres)-(aprese + mesdo / 2, Bpres + mesdo), cpres, B
' G
aprese = aprese + mesdo
LINE (aprese, Bpres)-(aprese + mesdo / 2, Bpres), cpres
LINE (aprese, Bpres)-(aprese, Bpres + mesdo), cpres
LINE (aprese, Bpres + mesdo)-(aprese + mesdo / 2, Bpres + mesdo), cpres
LINE (aprese + mesdo / 2, Bpres + mesdo)-(aprese + mesdo / 2, Bpres + mesdo / 2), cpres
LINE (aprese + mesdo / 2, Bpres + mesdo / 2)-(aprese + mesdo / 3, Bpres + mesdo / 2), cpres
' aprese
aprese = aprese + mesdo
LINE (aprese, Bpres)-(aprese + mesdo / 2, Bpres), cpres
LINE (aprese, Bpres)-(aprese, Bpres + mesdo), cpres
LINE (aprese + mesdo / 2, Bpres)-(aprese + mesdo / 2, Bpres + mesdo), cpres
LINE (aprese, Bpres + mesdo / 2)-(aprese + mesdo / 2, Bpres + mesdo / 2), cpres
' M
aprese = aprese + mesdo
LINE (aprese, Bpres)-(aprese, Bpres + mesdo), cpres
LINE (aprese, Bpres)-(aprese + mesdo / 4, Bpres + mesdo / 2), cpres
LINE (aprese + mesdo / 4, Bpres + mesdo / 2)-(aprese + mesdo / 2, Bpres), cpres
LINE (aprese + mesdo / 2, Bpres)-(aprese + mesdo / 2, Bpres + mesdo), cpres


cpres = 0
aprese = aprese - 4 * mesdo
misezero = TIMER
DO
present2
LOOP WHILE TIMER < misezero + .01 AND TIMER > 5
IF mesdo >= 100 THEN 1210


'cadre
LINE (aprese - (1 / 10) * mesdo, Bpres - (1 / 10) * mesdo)-(aprese + 4.5 * mesdo + (1 / 10) * mesdo, Bpres + mesdo + (1 / 10) * mesdo), cpres, B

' D
LINE (aprese, Bpres)-(aprese, Bpres + mesdo), cpres
LINE (aprese, Bpres + mesdo)-(aprese + mesdo / 2, Bpres + mesdo * (5 / 6)), cpres
LINE (aprese + mesdo / 2, Bpres + mesdo * (1 / 6))-(aprese, Bpres), cpres
LINE (aprese + mesdo / 2, Bpres + mesdo * (1 / 6))-(aprese + mesdo / 2, Bpres + mesdo * (5 / 6)), cpres
' O
aprese = aprese + mesdo
LINE (aprese, Bpres)-(aprese + mesdo / 2, Bpres + mesdo), cpres, B
' G
aprese = aprese + mesdo
LINE (aprese, Bpres)-(aprese + mesdo / 2, Bpres), cpres
LINE (aprese, Bpres)-(aprese, Bpres + mesdo), cpres
LINE (aprese, Bpres + mesdo)-(aprese + mesdo / 2, Bpres + mesdo), cpres
LINE (aprese + mesdo / 2, Bpres + mesdo)-(aprese + mesdo / 2, Bpres + mesdo / 2), cpres
LINE (aprese + mesdo / 2, Bpres + mesdo / 2)-(aprese + mesdo / 3, Bpres + mesdo / 2), cpres
' aprese
aprese = aprese + mesdo
LINE (aprese, Bpres)-(aprese + mesdo / 2, Bpres), cpres
LINE (aprese, Bpres)-(aprese, Bpres + mesdo), cpres
LINE (aprese + mesdo / 2, Bpres)-(aprese + mesdo / 2, Bpres + mesdo), cpres
LINE (aprese, Bpres + mesdo / 2)-(aprese + mesdo / 2, Bpres + mesdo / 2), cpres
' M
aprese = aprese + mesdo
LINE (aprese, Bpres)-(aprese, Bpres + mesdo), cpres
LINE (aprese, Bpres)-(aprese + mesdo / 4, Bpres + mesdo / 2), cpres
LINE (aprese + mesdo / 4, Bpres + mesdo / 2)-(aprese + mesdo / 2, Bpres), cpres
LINE (aprese + mesdo / 2, Bpres)-(aprese + mesdo / 2, Bpres + mesdo), cpres
aprese = aprese - 4 * mesdo

GOTO 1200
1210
FOR thyt = 1 TO 51
FOR thyt2 = 1 TO 7
IF dogpres(thyt, thyt2) = 0 THEN
LOCATE thyt2 + placey, thyt + placex
PRINT " "
ELSE
COLOR INT(RND * 15) + 1
LOCATE thyt2 + placey, thyt + placex
PRINT CHR$(219)
END IF
NEXT
NEXT

FOR tourne = 1 TO 80
COLOR INT(RND * 15 + 1)
LOCATE 1, tourne: PRINT CHR$(14)
NEXT
FOR tourne = 1 TO 23
COLOR INT(RND * 15 + 1)
LOCATE tourne, 80: PRINT CHR$(14)
NEXT
FOR tourne = 80 TO 1 STEP -1
COLOR INT(RND * 15 + 1)
LOCATE 23, tourne: PRINT CHR$(14)
NEXT
FOR tourne = 23 TO 1 STEP -1
COLOR INT(RND * 15 + 1)
LOCATE tourne, 1: PRINT CHR$(14)
NEXT



apui

END SUB

SUB present2


xposition = INT(RND * 51) + 1
yposition = INT(RND * 7) + 1
couleur = INT(RND * 15) + 1

IF dogpres(xposition, yposition) <> 2 THEN
        LOCATE yposition + placey, xposition + placex: COLOR couleur: PRINT CHR$(219)
END IF
IF dogpres(xposition, yposition) = 1 THEN
        dogpres(xposition, yposition) = 2
END IF

END SUB

SUB queue
IF B = 130 OR B = 125 OR B = 120 OR B = 115 OR B = 110 OR B = 105 OR B = 100 THEN plusq = -20
IF B < 96 THEN plusq = 20
IF plusq = 20 THEN gadr = -4 ELSE gadr = 4
LINE (a + gadr, B + plus)-(a + gadr, B + plusq + plus), vol

END SUB

SUB queuerouge
IF B = 130 OR B = 125 OR B = 120 OR B = 115 OR B = 110 OR B = 105 OR B = 100 THEN plusq = -20
IF B < 96 THEN plusq = 20
IF plusq = 20 THEN gadr = -4 ELSE gadr = 4
LINE (a + gadr, B + plus)-(a + gadr, B + plusq + plus), 12

END SUB

SUB saut
col = 12
bin
atten
col = 0
bin
decori
'erreur de dogam lors du exe: resolu conard !!
bbin = bbin - mes / 2: col = 12
CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col
LINE (abin, bbin + (2.5 * mes))-(abin + (2 * mes), bbin + 2.5 * mes + mes), col
LINE (abin, (bbin + mes))-(abin - (1.5 * mes), bbin + 1.5 * mes), col
LINE (abin, bbin + (2.5 * mes))-(abin - (2 * mes), bbin + (2.5 * mes) + mes), col
LINE (abin, bbin + mes)-(abin + 1.5 * mes, bbin + 1.5 * mes), col
atten

col = 0
CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin - 2 * mes, bbin + 2.5 * mes + mes), col
LINE (abin, bbin + 2.5 * mes)-(abin + 2 * mes, bbin + 2.5 * mes + mes), col
LINE (abin, bbin + mes)-(abin - 1.5 * mes, bbin + 1.5 * mes), col
LINE (abin, bbin + mes)-(abin + 1.5 * mes, bbin + 1.5 * mes), col
decori
bbin = bbin - mes / 2: col = 12
CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin - 3 * mes, bbin + 2.5 * mes - mes / 2), col
LINE (abin, bbin + 2.5 * mes)-(abin + 3 * mes, bbin + 2.5 * mes - mes / 2), col
LINE (abin, bbin + mes)-(abin - mes, bbin + .5 * mes), col
LINE (abin, bbin + mes)-(abin + mes, bbin + .5 * mes), col
atten

 col = 0
CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin - 3 * mes, bbin + 2.5 * mes - mes / 2), col
LINE (abin, bbin + 2.5 * mes)-(abin + 3 * mes, bbin + 2.5 * mes - mes / 2), col
LINE (abin, bbin + mes)-(abin - mes, bbin + .5 * mes), col
LINE (abin, bbin + mes)-(abin + mes, bbin + .5 * mes), col
decori






bbin = bbin + mes / 2: col = 12
CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin - 2 * mes, bbin + 2.5 * mes + mes), col
LINE (abin, bbin + 2.5 * mes)-(abin + 2 * mes, bbin + 2.5 * mes + mes), col
LINE (abin, bbin + mes)-(abin - 1.5 * mes, bbin + 1.5 * mes), col
LINE (abin, bbin + mes)-(abin + 1.5 * mes, bbin + 1.5 * mes), col
atten

col = 0
CIRCLE (abin, bbin), mes / 2, col
LINE (abin, bbin + mes / 2)-(abin, bbin + 2.5 * mes), col
LINE (abin, bbin + 2.5 * mes)-(abin - 2 * mes, bbin + 2.5 * mes + mes), col
LINE (abin, bbin + 2.5 * mes)-(abin + 2 * mes, bbin + 2.5 * mes + mes), col
LINE (abin, bbin + mes)-(abin - 1.5 * mes, bbin + 1.5 * mes), col
LINE (abin, bbin + mes)-(abin + 1.5 * mes, bbin + 1.5 * mes), col
decori
bbin = bbin + mes / 2


END SUB

SUB sautacrou
        'bonhomme qui saute ou pas...
        IF abin > 605 THEN
        IF temp = 1 AND aff = 1 AND nombre = 13 THEN
        mva = 72
        deplace
        col = 0
        bin
        abin = 320
        abon = 320
        col = 12
        bin
        GOTO 257
        END IF
        mva = 72
        FOR pml = 1 TO 10
        deplace
        NEXT
        'IF mv = 75 THEN gauche
        'IF mv = 77 THEN droite
        'IF mv = 72 THEN saut
        'IF mv = 80 THEN acroupi
        'IF mv = 27 THEN END
        mva = 77
        deplace
        deplace
        deplace
        deplace
        deplace
        mva = 75
        deplace
        deplace
        deplace
        deplace
        deplace
        mva = 72
        deplace
        deplace
        deplace
        deplace
        deplace
        fin = 1
        ELSEIF abin < 35 THEN
        IF temp = 1 AND aff = 1 AND nombre = 13 THEN
        mva = 72
        deplace
        col = 0
        bin
        abin = 320
        abon = 320
        col = 12
        bin
        GOTO 257
        END IF
        mva = 80
        FOR pml = 1 TO 10
        deplace
        NEXT
        mva = 75
        deplace
        deplace
        deplace
        deplace
        deplace
        mva = 77
        deplace
        deplace
        deplace
        deplace
        deplace
        mva = 80
        deplace
        deplace
        deplace
        fin = 1
       
        fin = 1
        END IF
257

END SUB

SUB score
  
ON ERROR GOTO 3

852 IF cle = 0 THEN OPEN "score.pia" FOR INPUT AS #1 ELSE OPEN "scorefa.pia" FOR INPUT AS #1


854 PRINT nom$: INPUT #1, nom$, points
IF points > poin THEN
nul
ELSEIF points < poin THEN
IF cle = 0 THEN feliz ELSE felizfa
ELSEIF points = poin THEN
pareil
END IF
CLOSE #1

END SUB

SUB Soupir
LINE (sop, sup + 10)-(sop - 4, sup + 7), 3
LINE (sop - 4, sup + 7)-(sop + 6, sup + 5), 3
LINE (sop + 6, sup + 5)-(sop, sup), 3
LINE (sop - 2, sup)-(sop + 4, sup - 10), 3
LINE (sop + 4, sup - 10)-(sop, sup - 15), 3

END SUB

SUB testhelp
COLOR 14
LOCATE 12, 1: PRINT CHR$(218); STRING$(78, 196); CHR$(191)
FOR cadre = 13 TO 17
PRINT CHR$(179); STRING$(78, 32); CHR$(179)
NEXT
PRINT CHR$(192); STRING$(78, 196); CHR$(217)
COLOR 13
LOCATE 18, 12: PRINT "Appuyez sur ENTREE pour poursuivre ou sur ESC pour fermer l'aide."
COLOR 15

' Aide de la ligne 13-17
IF aide = 1 THEN
LOCATE 13, 2: PRINT "Aide:"
LOCATE 15, 2: PRINT "Pour ce test vous devez donner le nom de la note Çcrite sur la portÇe musicale"
LOCATE 16, 2: PRINT ", et avec les fläches de gauche et de droite choisir la bonne rÇponse."
LOCATE 17, 2: PRINT "Quand le rectangle est placÇ sur la rÇponse, validez avec la barre d'espace."
ELSEIF aide = 2 THEN
LOCATE 13, 2: PRINT "ATTENTION: Cherchez le nom de la note dans la couleur correspondante."
LOCATE 14, 2: PRINT ""
LOCATE 15, 2: PRINT "De plus, si vous avez choisi le mode -notes seules- les notes Çcrites sont "
LOCATE 16, 2: PRINT "mÇlangÇes. Ceci est volontaire: cela oblige l'utilisateur Ö penser au nom des"
LOCATE 17, 2: PRINT "notes, et non Ö assimiler la place du nom Ö la note de la portÇe musicale."
ELSEIF aide = 3 THEN
LOCATE 13, 2: PRINT "Quand vous rÇpondez juste, le nom de la note est affichÇe au-dessus en violet,"
LOCATE 14, 2: PRINT "sinon la note devient rouge et la correction est affichÇ en rouge."
LOCATE 15, 2: PRINT "De plus si vous repondez juste le bonhomme ira vers la droite, vers -gagnÇ-,"
LOCATE 16, 2: PRINT "sinon il ira vers la gauche, vers -perdu-, et rendu Ö un certain point, le"
LOCATE 17, 2: PRINT "test se termine."
aide = -1
END IF
END SUB

SUB testhelpd
COLOR 14
LOCATE 12, 1: PRINT CHR$(218); STRING$(78, 196); CHR$(191)
FOR cadre = 13 TO 17
PRINT CHR$(179); STRING$(78, 32); CHR$(179)
NEXT
PRINT CHR$(192); STRING$(78, 196); CHR$(217)
COLOR 13
LOCATE 18, 12: PRINT "Appuyez sur ENTREE pour poursuivre ou sur ESC pour fermer l'aide."
COLOR 15

' Aide de la ligne 13-17
IF aide = 1 THEN
LOCATE 13, 2: PRINT "Aide:"
LOCATE 15, 2: PRINT "Pour ce test vous devez donner le nom de la note d'apräs le son entendu et,"
LOCATE 16, 2: PRINT "avec les fläches de gauche et de droite choisir la bonne rÇponse."
LOCATE 17, 2: PRINT "Quand le rectangle est placÇ sur la rÇponse, validez avec la barre d'espace."
ELSEIF aide = 2 THEN
LOCATE 13, 2: PRINT "Avec la touche F2 vous pouvez entendre le LA 440 pour vous donner un repäre,"
LOCATE 14, 2: PRINT "et avec la touche F3, vous pouvez rÇentendre la note Ö trouver."
LOCATE 15, 2: PRINT "Il vous faudra beaucoup de patience avant de rÇussir ce genre d'exercice: "
LOCATE 16, 2: PRINT "Lors du test sur la lecture des notes, rÇglez l'option music sur -oui- pour"
LOCATE 17, 2: PRINT "obtenir de meilleurs rÇsultats sur la dictÇe musicale."

ELSEIF aide = 3 THEN
LOCATE 13, 2: PRINT "ATTENTION: Cherchez le nom de la note dans la couleur correspondante."
LOCATE 14, 2: PRINT ""
LOCATE 15, 2: PRINT "De plus, si vous avez choisi le mode -notes seules- les notes Çcrites sont "
LOCATE 16, 2: PRINT "mÇlangÇes. Ceci est volontaire: cela oblige l'utilisateur Ö penser au nom des"
LOCATE 17, 2: PRINT "notes, et non Ö assimiler la place du nom Ö la note de la portÇe musicale."
ELSEIF aide = 4 THEN
LOCATE 13, 2: PRINT "Quand vous rÇpondez juste, le nom de la note est affichÇe au-dessus en violet,"
LOCATE 14, 2: PRINT "sinon la note devient rouge et la correction est affichÇ en rouge."
LOCATE 15, 2: PRINT "De plus si vous repondez juste le bonhomme ira vers la droite, vers -gagnÇ-,"
LOCATE 16, 2: PRINT "sinon il ira vers la gauche, vers -perdu-, et rendu Ö un certain point, le"
LOCATE 17, 2: PRINT "test se termine."
aide = -1
END IF

END SUB

SUB testhelpmenu
aide = 0
IF aff <> 1 THEN
FOR hauteurdeligne = 140 TO 250
LINE (50, hauteurdeligne)-(570, hauteurdeligne), 0
NEXT
END IF

1685
aide = aide + 1

IF dictee = 0 THEN testhelp ELSE testhelpd
pausedt
IF aide > 0 AND voiraide = 13 THEN 1685
FOR cadre = 12 TO 18
LOCATE cadre, 1
PRINT STRING$(80, 32)
NEXT

IF aff = 1 AND nombre = 13 AND temp = 1 THEN
COLOR 10: LOCATE 13, 33: PRINT CHR$(218); STRING$(12, 196); CHR$(191)
LOCATE 14, 33: PRINT CHR$(179): LOCATE 14, 46: PRINT CHR$(179)
LOCATE 15, 33: PRINT CHR$(192); STRING$(12, 196); CHR$(217)
phrases
ELSEIF aff = 1 THEN
68 colr = INT(RND * 16)
IF colr = 0 THEN 68
COLOR colr
LOCATE 12, 15: PRINT "€€€€€€€€€    €€€€€€€€€ €€€€€€€€    €€    €€      €€"
LOCATE 13, 15: PRINT "€        €   €       € €          €  €   € €    € €"
LOCATE 14, 15: PRINT "€         €  €       € €         €    €  €  €  €  €"
LOCATE 15, 15: PRINT "€          € €       € €        €      € €   €€   €"
LOCATE 16, 15: PRINT "€         €  €       € €   €€€€ €€€€€€€€ €        €"
LOCATE 17, 15: PRINT "€        €   €       € €      € €      € €        €"
LOCATE 18, 15: PRINT "€€€€€€€€€    €€€€€€€€€ €€€€€€€€ €      € €        €"
ELSEIF aff <> 1 THEN
cl = 9
IF aff <> 1 AND cle = 0 THEN
LINE (50, 140)-(570, 250), cl, B
FOR sep = 520 / 13 TO 520 STEP 520 / 13
LINE (sep + 50, 140)-(sep + 50, 250), cl
NEXT
FOR tnoir = 520 / 13 TO 520 STEP 520 / 13
k = k + 1
IF k = 3 OR k = 7 OR k = 10 THEN 1921
IF k = 13 THEN fine = (520 / 13) / 4
LINE (tnoir + 50 - (520 / 13) / 6, 140)-(tnoir + 50 + (520 / 13) / 6 - fine, 210), cl, B
fine = 0
1921
NEXT
'couleur des touches noires
k = 0
FOR tnoir = 520 / 13 TO 520 STEP 520 / 13
k = k + 1
IF k = 3 OR k = 7 OR k = 10 THEN 1922
PAINT (tnoir + 50 - (520 / 13) / 6 + 1, 140 + 5), cl
IF k = 13 THEN 1923
PAINT (tnoir + 50 + (520 / 13) / 6 - 1, 140 + 5), cl
1922
NEXT
1923
LINE (566, 140)-(570, 210), cl, B
PAINT (569, 141), cl

                                  






ELSEIF aff <> 1 AND cle = 1 THEN
LINE (50, 140)-(570, 250), cl, B
FOR sep = 520 / 13 TO 520 STEP 520 / 13
LINE (sep + 50, 140)-(sep + 50, 250), cl
NEXT
FOR tnoir = 520 / 13 TO 520 STEP 520 / 13
k = k + 1
IF k = 1 OR k = 5 OR k = 8 OR k = 12 THEN 1926
IF k = 13 THEN fine = (520 / 13) / 4
LINE (tnoir + 50 - (520 / 13) / 6, 140)-(tnoir + 50 + (520 / 13) / 6 - fine, 210), cl, B
fine = 0
1926
NEXT
'couleur des touches noires
k = 0
FOR tnoir = 520 / 13 TO 520 STEP 520 / 13
k = k + 1
IF k = 1 OR k = 5 OR k = 8 OR k = 12 THEN 1927
PAINT (tnoir + 50 - (520 / 13) / 6 + 1, 140 + 5), cl
IF k = 13 THEN 1928
PAINT (tnoir + 50 + (520 / 13) / 6 - 1, 140 + 5), cl
1927
NEXT
1928
LINE (566, 140)-(570, 210), cl, B
PAINT (569, 141), cl

END IF


END IF


END SUB

SUB vitesse
SCREEN 0
CLS
COLOR 15
PRINT "Chargement de DOGAM en cours."
PRINT "Recherche du facteur vitesse."
tut = TIMER
lavitesse = 0
r = 1
DO
r = r + 1
LOCATE 3, 1: PRINT INT(r / 100)
IF INT(r / 100) < 75 THEN LOCATE 4, (INT(r / 100)) + 1: PRINT "."
LOOP WHILE tut + 1 > TIMER
DO
lavitesse = lavitesse + 1
LOOP WHILE tut + 2 > TIMER

END SUB

