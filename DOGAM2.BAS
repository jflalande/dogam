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

TYPE fiche
haut AS INTEGER
nature AS SINGLE
END TYPE

REM From DOG4: gestion des erreurs ouverture de fichiers
REM 2987
REM COLOR 12
REM LOCATE 7, 5: PRINT "Ce fichier n'existe pas."
REM apui
REM passages = 0
REM RESUME NEXT

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





REM #################################################################################################
REM #†DOG2
REM #################################################################################################
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
PRINT "Le but de ce jeu est d'apprendre le solfäge en s'amusant..."
PRINT
PRINT "Les notions ÇlÇmentaires de solfäge vous sont prÇsentÇes de maniäre rÇcrÇative."
PRINT ""
PRINT "A la page suivante vous pourrez choisir entre :  "
PRINT
PRINT
PRINT
PRINT "              - Apprendre les notes en clÇ de sol ou de fa"
PRINT
PRINT "              - Tester vos connaissances sur la lecture des notes"
PRINT
PRINT "              - Composer quelques mÇlodies"
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
COLOR 10: LOCATE 10, 5: PRINT "FÇlicitation ! ! !"
COLOR 12: LOCATE 15, 7: PRINT "Avec vos "; poin; " points en 1 minute ", nom$
LOCATE 16, 7: PRINT "Ö ÇtÇ battu:il avait eu "; points; " points."
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
COLOR 10: LOCATE 10, 5: PRINT "FÇlicitation ! ! !"
COLOR 12: LOCATE 15, 7: PRINT "Avec vos "; poin; " points en 1 minute ", nom$
LOCATE 16, 7: PRINT "Ö ÇtÇ battu:il avait eu "; points; " points. Et en clÇ de FA !!!"
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
LOCATE 2, 32: COLOR 15: PRINT "Fräre Jacques"
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
LOCATE 15, 10: PRINT "€€€€€€€€€    €€€€€€€€€ €€€€€€€€    €€    €€      €€"
LOCATE 16, 10: PRINT "€        €   €       € €          €  €   € €    € €"
LOCATE 17, 10: PRINT "€         €  €       € €         €    €  €  €  €  €"
LOCATE 18, 10: PRINT "€          € €       € €        €      € €   €€   €"
LOCATE 19, 10: PRINT "€         €  €       € €   €€€€ €€€€€€€€ €        €"
LOCATE 20, 10: PRINT "€        €   €       € €      € €      € €        €"
LOCATE 21, 10: PRINT "€€€€€€€€€    €€€€€€€€€ €€€€€€€€ €      € €        €"


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
LOCATE 4, 12: PRINT "1 - Leáons de solfäge"
LOCATE 5, 12: PRINT "2 - Tests de solfäge"
LOCATE 6, 12: PRINT "3 - morceaux de musique"
LOCATE 7, 12: PRINT "4 - A propos de DOGAM"
LOCATE 8, 12: PRINT "5 - Quitter DOGAM"
LOCATE 10, 22: PRINT "Tapez 1, 2 ,3, 4 ou 5 comme sÇlection:"
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
PRINT "Merci d'avoir jouÇ avec DOGAM."
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
LOCATE 13, 2: PRINT "de mÇmoriser leurs sons; pour cela appuyez sur la touche -1- du clavier. Vous"
LOCATE 14, 2: PRINT "pouvez tester vos connaissances sur la lecture des notes en lanáant le second"
LOCATE 15, 2: PRINT "menu de DOGAM (touche -2-). De plus il est possible de composer ses propres"
LOCATE 16, 2: PRINT "mÇlodies (touche -3-). Pour finir vous pouvez lancer la prÇsentation de DOGAM"
LOCATE 17, 2: PRINT "(touche -4-) ou quitter DOGAM (touche -5-)."
LOCATE 18, 2: PRINT "Appuyez sur la touche 1, 2, 3, 4 ou 5 pour sÇlectionner votre choix."
LOCATE 19, 53: PRINT "€€€  €€€€ €€€€ €€€€ €   €"
LOCATE 20, 53: PRINT "€  € €  € €    €  € €€ €€"
LOCATE 21, 53: PRINT "€  € €  € €  € €€€€ € € €"
LOCATE 22, 53: PRINT "€€€  €€€€ €€€€ €  € €   €"
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
COLOR 12: LOCATE 1, 35: PRINT "Leáons..."
COLOR 11
'1ere portÇe
LINE (0, 80 + plus)-(640, 80 + plus), 3
LINE (0, 90 + plus)-(640, 90 + plus), 3
LINE (0, 100 + plus)-(640, 100 + plus), 3
LINE (0, 110 + plus)-(640, 110 + plus), 3
LINE (0, 120 + plus)-(640, 120 + plus), 3
'2 eme portÇe
LINE (0, 80 + 60)-(640, 80 + 60), 3
LINE (0, 90 + 60)-(640, 90 + 60), 3
LINE (0, 100 + 60)-(640, 100 + 60), 3
LINE (0, 110 + 60)-(640, 110 + 60), 3
LINE (0, 120 + 60)-(640, 120 + 60), 3
LOCATE 3, 1: COLOR 12: PRINT "ClÇ de sol"
LOCATE 16, 1: PRINT "ClÇ de fa"
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
COLOR 12: LOCATE 1, 35: PRINT "Leáons..."
COLOR 11
'1ere portÇe
'LINE (0, 80 + plus)-(640, 80 + plus), 3
'LINE (0, 90 + plus)-(640, 90 + plus), 3
'LINE (0, 100 + plus)-(640, 100 + plus), 3
'LINE (0, 110 + plus)-(640, 110 + plus), 3
'LINE (0, 120 + plus)-(640, 120 + plus), 3
'2 eme portÇe
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
SCREEN 9: COLOR 12: LOCATE 1, 32: PRINT "DictÇe musicale": COLOR 11
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
LOCATE 12, 15: PRINT "€€€€€€€€€    €€€€€€€€€ €€€€€€€€    €€    €€      €€"
LOCATE 13, 15: PRINT "€        €   €       € €          €  €   € €    € €"
LOCATE 14, 15: PRINT "€         €  €       € €         €    €  €  €  €  €"
LOCATE 15, 15: PRINT "€          € €       € €        €      € €   €€   €"
LOCATE 16, 15: PRINT "€         €  €       € €   €€€€ €€€€€€€€ €        €"
LOCATE 17, 15: PRINT "€        €   €       € €      € €      € €        €"
LOCATE 18, 15: PRINT "€€€€€€€€€    €€€€€€€€€ €€€€€€€€ €      € €        €"
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
        LOCATE 19, 75: PRINT "GagnÇ"
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
LOCATE 16, 10: PRINT "€€€€€€€€€    €€€€€€€€€ €€€€€€€€    €€    €€      €€"
LOCATE 17, 10: PRINT "€        €   €       € €          €  €   € €    € €"
LOCATE 18, 10: PRINT "€         €  €       € €         €    €  €  €  €  €"
LOCATE 19, 10: PRINT "€          € €       € €        €      € €   €€   €"
LOCATE 20, 10: PRINT "€         €  €       € €   €€€€ €€€€€€€€ €        €"
LOCATE 21, 10: PRINT "€        €   €       € €      € €      € €        €"
LOCATE 22, 10: PRINT "€€€€€€€€€    €€€€€€€€€ €€€€€€€€ €      € €        €"

5 LOCATE 1, 35: COLOR 12: PRINT "Menu"
LOCATE 1, 60: COLOR 14: PRINT "F1 = Aide"
LOCATE 4, 10: COLOR 10: PRINT "1 - ClÇ de"
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
LOCATE 11, 10: PRINT "7 - DictÇe musicale"
REM LOCATE 12, 10: PRINT "8 - Petit air"
LOCATE 8, 10: PRINT "5 - Notes Çcrites    , Clavier dessinÇ      "
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
LOCATE 14, 20: PRINT "Tapez un chiffre de 1 Ö 8 comme sÇlection:"
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

REM #################################################################################################
REM #†END DOG2
REM #################################################################################################

REM #################################################################################################
REM #†DOG3
REM #################################################################################################
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

REM #################################################################################################
REM #†END DOG3
REM #################################################################################################

REM #################################################################################################
REM #†DOG4
REM #################################################################################################


SUB affcurse
LINE (selecti2 - 8, 47 + descente)-(selecti2 + 8, 53 + descente), 0, BF
LINE (selecti - 8, 47 + descente + desc)-(selecti + 8, 53 + descente + desc), 12, BF
selecti2 = selecti
END SUB

SUB affiche
COLOR 14
IF musi(3, 2) >= 7 THEN musi(3, 2) = 1
IF musi(2, 2) >= 7 THEN musi(2, 2) = 1
IF musi(1, 2) >= 6 THEN musi(1, 2) = 1
IF musi(6, 2) >= 3 THEN musi(6, 2) = 1
' Octave
IF musi(6, 2) = 1 AND musi(7, 2) > 6 THEN musi(7, 2) = 0
IF musi(6, 2) = 2 AND musi(7, 2) < 2 THEN musi(7, 2) = 2
IF musi(6, 2) = 2 AND musi(7, 2) > 8 THEN musi(7, 2) = 2
IF musi(8, 2) >= 256 THEN musi(8, 2) = 32
IF musi(9, 2) >= 4 THEN musi(9, 2) = 1

IF musi(3, 2) > 1 OR musi(2, 2) = 3 OR musi(2, 2) > 4 OR nonume > 640 AND musi(2, 2) = 1 THEN COLOR 12 ELSE COLOR 14
IF musi(1, 2) = 1 THEN
LOCATE 12, 23: PRINT "Noire        "
ELSEIF musi(1, 2) = 2 THEN
LOCATE 12, 23: PRINT "Blanche      "
ELSEIF musi(1, 2) = 3 THEN
LOCATE 12, 23: PRINT "Ronde        "
ELSEIF musi(1, 2) = 4 THEN
LOCATE 12, 23: PRINT "Croche       "
ELSEIF musi(1, 2) = 5 THEN
LOCATE 12, 23: PRINT "Double Croche"
END IF

IF modification = 0 THEN COLOR 14 ELSE COLOR 12
IF musi(2, 2) = 1 THEN
LOCATE 13, 10: PRINT "Ajouter  "
ELSEIF musi(2, 2) = 2 THEN
LOCATE 13, 10: PRINT "Modifier "
ELSEIF musi(2, 2) = 3 THEN
LOCATE 13, 10: PRINT "Supprimer"
ELSEIF musi(2, 2) = 4 THEN
LOCATE 13, 10: PRINT "Inserer  "
ELSEIF musi(2, 2) = 5 THEN
LOCATE 13, 10: PRINT "Diäse    "
ELSEIF musi(2, 2) = 6 THEN
LOCATE 13, 10: PRINT "BÇmol    "
END IF

IF musi(2, 2) = 3 OR musi(2, 2) > 4 OR nonume > 640 AND musi(2, 2) = 1 THEN COLOR 12 ELSE COLOR 14
IF musi(3, 2) = 1 THEN
LOCATE 14, 10: PRINT "Note           "
ELSEIF musi(3, 2) = 2 THEN
LOCATE 14, 10: PRINT "Soupir         "
ELSEIF musi(3, 2) = 3 THEN
LOCATE 14, 10: PRINT "Demi soupir    "
ELSEIF musi(3, 2) = 4 THEN
LOCATE 14, 10: PRINT "Quart de soupir"
ELSEIF musi(3, 2) = 5 THEN
LOCATE 14, 10: PRINT "Pause          "
ELSEIF musi(3, 2) = 6 THEN
LOCATE 14, 10: PRINT "Demi pause       "
END IF

COLOR 14
IF musi(6, 2) = 1 THEN
LOCATE 13, 48: PRINT "Sol"
ELSE
LOCATE 13, 48: PRINT "Fa "
END IF
LOCATE 14, 54: PRINT musi(7, 2); "  "
LOCATE 15, 67: PRINT musi(8, 2) + 28; "  "

IF musi(9, 2) = 1 THEN
LOCATE 16, 60: PRINT "normales "
ELSEIF musi(9, 2) = 2 THEN
LOCATE 16, 60: PRINT "liÇes    "
ELSEIF musi(9, 2) = 3 THEN
LOCATE 16, 60: PRINT "dÇtachÇes"
END IF

END SUB

SUB affichedite

COLOR 12
LOCATE 1, 30: PRINT "Edition:"
COLOR 14
LOCATE 1, 44: PRINT "F1 = Aide"
COLOR 13
LOCATE 3, 10: PRINT "F2 = "; CHR$(17)
LOCATE 3, 20: PRINT "F3 = "; CHR$(16)
LOCATE 3, 30: PRINT "F5 = Jouer l'Çcran"
LOCATE 3, 54: PRINT "F10 = Tout jouer"
COLOR 10
LOCATE 12, 5: PRINT "Valeur de la note:"
LOCATE 13, 5: PRINT "Mode:"
LOCATE 14, 5: PRINT "Note:"
LOCATE 15, 5: PRINT "Valider"
LOCATE 16, 5: PRINT "Options..."
' Test du tableau pour mettre les valeurs
COLOR 15
LOCATE 1, 60: PRINT "ESC = Quitter"

END SUB

SUB affichela
IF nonume < 640 THEN

        IF musi(noplace, 1) > 10 AND musi(noplace, 1) < 17 THEN
                vol = 13
                IF musi(noplace, 1) = 12 THEN lesoupir 'Soupir
                IF musi(noplace, 1) = 13 THEN demisoupir 'Demi soupir
                IF musi(noplace, 1) = 14 THEN quartsoupir ' Quart de soupir
                IF musi(noplace, 1) = 15 THEN pause 'Pause
                IF musi(noplace, 1) = 16 THEN demipause 'Demi pause
        ELSE
                IF b < 105 THEN vol = 11 ELSE vol = 10
                a = nonume
                b = musi(noplace, 1)
                IF b < 200 THEN dise$ = ""
                IF b > 200 AND b < 500 THEN dise$ = "#": b = b - 200
                IF b > 500 THEN dise$ = "b": b = b - 500
               
                IF b < 105 THEN vol = 11 ELSE vol = 10
                plus = 0
                IF musi(noplace, 2) <> 3 AND musi(noplace, 2) <> 2 THEN
                        FOR d = 4 TO 1 STEP -1: CIRCLE (a, b + plus), d, vol: NEXT:        'dessin de note
                ELSE
                        CIRCLE (a, b + plus), 4, vol        'dessin de note
                END IF
                IF b = 130 OR b = 70 THEN LINE (a - 8, b + plus)-(a + 8, b + plus), vol

                IF musi(noplace, 2) <> 3 THEN queuAIR
                r = INT(80 * a / 640): COLOR 9: LOCATE 11, (r + 1): PRINT dise$

        END IF
END IF

END SUB

SUB affichenoms
aff = 0
vc = 0
vc2 = 0
uti0 = 0: uti1 = 0: uti2 = 0: uti3 = 0: uti4 = 0: uti5 = 0: uti6 = 0: uti7 = 0: uti8 = 0: uti9 = 0: uti10 = 0: uti11 = 0: uti12 = 0
        COLOR 10
        dez = 9
LOCATE 20, 1
PRINT STRING$(80, 32)
LOCATE 21, 1
PRINT STRING$(80, 32)

654     IF musi(6, 2) = 1 THEN asar ELSE asar3
                LOCATE 20, dez: PRINT ha$
                dez = dez + 5
                IF dez = 9 + 5 * 6 THEN nada2 = 0 ELSE 654
                vc = 0: vc2 = 0
                COLOR 11
              
651     IF musi(6, 2) = 1 THEN asar2 ELSE ASAR4
                LOCATE 20, dez: PRINT ha$
                dez = dez + 5
                IF dez = 9 + 5 * 6 + 5 * 7 THEN nada2 = 0 ELSE 651
                vc = 0: vc2 = 0



END SUB

SUB affichenote
selecti2 = -100
vc = 0
vc2 = 0

g = 43
LOCATE 11, 1: PRINT "                                     "
2210 LINE (0, g)-(650, g), 0
thu = (g / 2) + 10: IF thu = INT(thu) THEN LOCATE 11, thu: PRINT " "
g = g + 1: IF g = 140 THEN 2220 ELSE 2210
2220
ligneair
nonume = 90
noplace = noref
2843
IF musi(noplace, 1) = -10 OR nonume >= 640 THEN 2365

affichela

nonume = nonume + valref * musi(noplace, 2)

noplace = noplace + 1
GOTO 2843
2365

END SUB

SUB afficheopte
COLOR 10
LOCATE 12, 37: PRINT CHR$(218) + STRING$(40, 196) + CHR$(191)
LOCATE 13, 37: PRINT CHR$(179); TAB(40 + 38); CHR$(179)
LOCATE 14, 37: PRINT CHR$(179); TAB(40 + 38); CHR$(179)
LOCATE 15, 37: PRINT CHR$(179); TAB(40 + 38); CHR$(179)
LOCATE 16, 37: PRINT CHR$(179); TAB(40 + 38); CHR$(179)
LOCATE 17, 5: PRINT CHR$(218) + STRING$(31, 196) + CHR$(193) + STRING$(40, 196) + CHR$(180)
LOCATE 18, 5: PRINT CHR$(179); TAB(78); CHR$(179)
LOCATE 19, 5: PRINT CHR$(192) + STRING$(72, 196) + CHR$(217)
LOCATE 13, 41: PRINT "ClÇ de"
LOCATE 14, 41: PRINT "Octave numÇro"
LOCATE 15, 41: PRINT "Nombre de notes par minute"
LOCATE 16, 41: PRINT "Musique avec notes"
END SUB

SUB clrcom
FOR huy = 6 TO 40 + 37
LOCATE 18, huy: PRINT " "
NEXT
END SUB

SUB comment
IF curs <> lecomment THEN clrcom

COLOR 12
LOCATE 18, 6
IF curs = 12 THEN
        PRINT "Choisissez la valeur d'une note en la changeant avec ESPACE."
ELSEIF curs = 13 THEN
        PRINT "En appuyant sur ESPACE, changez l'action Ö effectuer."
ELSEIF curs = 14 THEN
        PRINT "SÇlectionnez un symbole (note, silence) Ö afficher avec ESPACE."
ELSEIF curs = 15 THEN
        PRINT "Validez les paramätres et affichez la note voulue avec ESPACE."
ELSEIF curs = 16 THEN
        PRINT "Pour changer les caractÇristiques du morceau, appuyez sur ESPACE."
END IF
lecomment = curs
END SUB

SUB commentopt
IF curs <> lecomment THEN clrcom

COLOR 12
LOCATE 18, 6
IF curs = 13 THEN
        PRINT "En appuyant sur ESPACE, changez la clÇ du morceau."
ELSEIF curs = 14 THEN
        PRINT "Choisissez l'octave oó le morceau est jouÇ."
ELSEIF curs = 15 THEN
        PRINT "DÇtermine le nombre de notes jouÇes Ö la minute."
ELSEIF curs = 16 THEN
        PRINT "Lecture les notes d'une certaine faáon."
END IF
lecomment = curs

END SUB

SUB demipause
sup = 89
LINE (nonume - 8, sup)-(nonume + 8, sup - 4), 13, BF

END SUB

SUB demisoupir
sup = 110

LINE (nonume + 1, sup)-(nonume + 5, sup - 15), 13
CIRCLE (nonume - 1, sup - 3 - 15 + 3), 6, 13, 3.6, 5.8


END SUB

SUB diesebemol
IF musi(selecta, 1) < 60 THEN EXIT SUB
stockerdiese = musi(selecta, 1)

IF musi(selecta, 1) > 200 AND musi(selecta, 1) < 500 THEN 2519

IF musi(2, 2) = 5 THEN
        IF musi(selecta, 1) > 500 THEN musi(selecta, 1) = musi(selecta, 1) - 500
        IF musi(selecta, 1) <> 120 AND musi(selecta, 1) <> 100 AND musi(6, 2) = 1 AND musi(selecta, 1) <> 85 THEN musi(selecta, 1) = musi(selecta, 1) + 200
        IF musi(selecta, 1) <> 130 AND musi(selecta, 1) <> 110 AND musi(selecta, 1) <> 75 AND musi(selecta, 1) <> 95 AND musi(6, 2) = 2 THEN musi(selecta, 1) = musi(selecta, 1) + 200
END IF
2519

IF musi(selecta, 1) > 500 THEN 2674
IF musi(2, 2) = 6 THEN
        IF musi(selecta, 1) > 200 THEN musi(selecta, 1) = musi(selecta, 1) - 200
        IF musi(selecta, 1) <> 130 AND musi(selecta, 1) <> 115 AND musi(selecta, 1) <> 95 AND musi(6, 2) = 1 AND musi(selecta, 1) <> 80 THEN musi(selecta, 1) = musi(selecta, 1) + 500
        IF musi(selecta, 1) <> 125 AND musi(selecta, 1) <> 105 AND musi(selecta, 1) <> 70 AND musi(selecta, 1) <> 90 AND musi(6, 2) = 2 THEN musi(selecta, 1) = musi(selecta, 1) + 500
END IF
2674

IF stockerdiese <> musi(selecta, 1) THEN affichenote
END SUB

SUB editer
selecti = 90
selecta = noref
' Affichage de l'environement

SCREEN 9
CLS
frt = 50 + (520 / 13) / 2: frt2 = frt
cle = 0
vc = 0
vc2 = 0
curs2 = 15
curs = 15



IF musi(10, 1) = -10 THEN
        musi(1, 2) = 1
        musi(2, 2) = 1
        musi(3, 2) = 1
ELSE
END IF


affichedite
afficheopte
affichenoms
affichenote
affiche


521
comment
LOCATE curs2, 3: COLOR 14: PRINT CHR$(16)
IF frt2 <> frt THEN
        LINE (frt2 - 15, 273 + 7)-(frt2 + 18, 273 - 7), 0, B
END IF
frt2 = frt
LINE (frt - 15, 273 + 7)-(frt + 18, 273 - 7), 11, B
IF curs <> curs2 THEN LOCATE curs, 3: PRINT " ": curs = curs2
IF selecti2 <> selecti THEN descente = 0: desc = 0: affcurse

IF jouons = -125 THEN
        atournera = 10
        atournepas = -10
        jouela
        jouons = 0
END IF

          a$ = INKEY$: IF a$ = "" THEN 521     'verification touches
mv = ASC(RIGHT$(a$, 1))
IF mv = 27 THEN EXIT SUB

' Curseur jaune
IF nonume > 640 AND musi(2, 2) = 1 THEN
IF mv = 80 THEN curs2 = 16
IF mv = 72 THEN curs2 = 13
ELSE
        IF mv = 80 THEN
                curs2 = curs2 + 1
                IF curs2 > 16 THEN curs2 = 16
                IF curs2 = 14 AND (musi(2, 2) = 3 OR musi(2, 2) > 4) THEN curs2 = curs2 + 1
        ELSEIF mv = 72 THEN
                curs2 = curs2 - 1
                IF curs2 < 12 THEN curs2 = 12
                IF curs2 = 14 AND (musi(2, 2) = 3 OR musi(2, 2) > 4) THEN curs2 = curs2 - 1
                IF curs2 = 12 AND (musi(2, 2) = 3 OR musi(2, 2) > 4 OR musi(3, 2) <> 1) THEN curs2 = 13
        END IF
END IF

IF mv = 75 AND curs2 <> 13 THEN frt = frt - 520 / 13
IF mv = 77 AND curs2 <> 13 THEN frt = frt + 520 / 13
IF frt < 50 + (520 / 13) / 2 THEN frt = 50 + (520 / 13) / 2
IF frt > 550 THEN frt = 550

' barre d' espacement pour changer les attributs

IF mv = 32 THEN
        IF curs = 12 THEN musi(1, 2) = musi(1, 2) + 1
        IF curs = 13 THEN musi(2, 2) = musi(2, 2) + 1
        IF curs = 14 THEN musi(3, 2) = musi(3, 2) + 1
        affiche
END IF

' aide

IF mv = 59 THEN
        helpediter
END IF



IF mv = 32 AND curs = 16 THEN
        Options
END IF


' F2 Aller Ö gauche

IF mv = 60 AND musi(10, 1) <> -10 AND noref > 10 THEN

        ouondoit = 10
        ouonest = 9
        hyu = 90
        oucasera = 10

259
        ouonest = ouonest + 1
        hyu = hyu + valref * musi(ouonest, 2)
        IF ouonest = noref THEN 256
        IF hyu < 640 THEN 259
        hyu = 90
        oucasera = ouondoit
        ouondoit = ouonest + 1
        GOTO 259
256
        noref = oucasera
        noplace = noref
        selecti = 90
        selecta = noref
        affichenote
        affiche
END IF

' F3  Aller Ö droite

IF mv = 61 AND nonume > 640 THEN
        hyu = 90
        noessai = noref
258

        hyu = hyu + valref * musi(noessai, 2)
        noessai = noessai + 1
       
        IF hyu < 640 THEN 258
        noref = noessai
        noplace = noref
        selecti = 90
        selecta = noref
        affichenote
        affiche
END IF

' Quand les notes dÇpasses de l'Çcran et qu il y a ajouter
IF nonume > 640 AND musi(2, 2) = 1 THEN
        IF curs = 16 THEN 1258
        IF curs = 13 THEN 1258
        LOCATE curs, 3: PRINT " "
        curs = 13: curs2 = 13
        COLOR 14
        LOCATE 13, 3: PRINT CHR$(16)
        affiche
1258
END IF


' F10 Jouer le morceau
IF mv = 68 AND musi(10, 1) <> -10 AND musi(10, 1) <> 0 AND PLAY(1) = 0 THEN
        atournera = 10
        atournepas = -10
        jouela
        jouons = 0
END IF

' F5 Jouer la partie de l'Çcran

IF mv = 63 AND musi(10, 1) <> -10 AND musi(10, 1) <> 0 AND PLAY(1) = 0 THEN
        atournera = noref
        atournepas = noplace - 1
        jouela
        jouons = 0
END IF

' barre d'espacement pour valider et ajouter

IF mv = 32 AND curs = 15 AND musi(2, 2) = 1 AND nonume < 640 THEN

        frk = (frt * 5) / 40
        frk = CINT(frk)

        IF dod = frk THEN b = 130
        IF re = frk THEN b = 125
        IF mi = frk THEN b = 120
        IF fa = frk THEN b = 115
        IF sol = frk THEN b = 110
        IF la = frk THEN b = 105
        IF si = frk THEN b = 100
        IF dod2 = frk THEN b = 95
        IF re2 = frk THEN b = 90
        IF mi2 = frk THEN b = 85
        IF fa2 = frk THEN b = 80
        IF sol2 = frk THEN b = 75

        IF la2 = frk THEN b = 70
 
        IF musi(3, 2) = 1 THEN
                musi(noplace, 1) = b
               
                IF musi(1, 2) < 4 THEN
                musi(noplace, 2) = musi(1, 2)
                ELSEIF musi(1, 2) = 4 THEN
                musi(noplace, 2) = musi(1, 2) / 8
                ELSEIF musi(1, 2) = 5 THEN
                musi(noplace, 2) = (musi(1, 2) - 1) / 16
                END IF
       
        ELSE
                musi(noplace, 1) = 10 + musi(3, 2)
               
                IF musi(3, 2) = 2 THEN
                        musi(noplace, 2) = 1
                ELSEIF musi(3, 2) = 3 THEN
                        musi(noplace, 2) = .5
                ELSEIF musi(3, 2) = 4 THEN
                        musi(noplace, 2) = .25
                ELSEIF musi(3, 2) = 5 THEN
                        musi(noplace, 2) = 4
                ELSEIF musi(3, 2) = 6 THEN
                        musi(noplace, 2) = 2
                END IF
                      
        END IF



        affichela
        nonume = nonume + valref * musi(noplace, 2)
        noplace = noplace + 1
        musi(noplace, 1) = -10



        IF nonume > 640 THEN
        noref = noplace
        affichenote
        selecti = 90
        selecta = noref
        END IF

END IF

' Barre d'espacement et modifier

IF mv = 32 AND curs = 15 AND musi(2, 2) = 2 AND musi(selecta, 1) <> -10 THEN
        MOdif
END IF

' Barre d'espacement et supprimer
IF mv = 32 AND curs = 15 AND musi(2, 2) = 3 AND musi(selecta, 1) <> -10 THEN
        supprimer
END IF

' Barre d'espacement et inserer
IF mv = 32 AND curs = 15 AND musi(2, 2) = 4 AND musi(selecta, 1) <> -10 THEN
        inserer
END IF

' Barre d'espacement et diäse/bÇmol
IF mv = 32 AND curs = 15 AND musi(2, 2) > 4 AND musi(selecta, 1) <> -10 THEN
        diesebemol
END IF


IF curs = 13 AND musi(2, 2) <> 1 THEN
        IF mv = 75 THEN
        selecta = selecta - 1
        selecti = selecti - musi(selecta, 2) * valref
        IF selecti < 90 THEN
                selecti = selecti + musi(selecta, 2) * valref
                selecta = selecta + 1
        END IF
        ELSEIF mv = 77 THEN
        selecti = selecti + musi(selecta, 2) * valref
        selecta = selecta + 1
        IF selecti > 640 OR musi(selecta - 1, 1) = -10 THEN
                selecti = selecti - musi(selecta - 1, 2) * valref
                selecta = selecta - 1
        END IF
        END IF
END IF


GOTO 521

END SUB

SUB helpediter

aide = 0
LOCATE 1, 15: COLOR 12: PRINT "PAUSE"
pausoi = 0

1236

aide = aide + 1


COLOR 14
LOCATE 12, 1: PRINT CHR$(218); STRING$(78, 196); CHR$(191)
FOR cadre = 13 TO 20
PRINT CHR$(179); STRING$(78, 32); CHR$(179)
NEXT
PRINT CHR$(192); STRING$(78, 196); CHR$(217)


' Aide de la ligne 13-20
IF aide = 1 THEN
COLOR 13
LOCATE 21, 12: PRINT "Appuyez sur ENTREE pour poursuivre ou sur ESC pour fermer l'aide."
COLOR 15
LOCATE 13, 2: PRINT "Aide:"
LOCATE 15, 2: PRINT "Cette partie du logiciel permet de composer une mÇlodie. Avec les fläches du  "
LOCATE 16, 2: PRINT "bas et du haut, vous dirigez le curseur jaune dans le menu en bas Ö gauche.   "
LOCATE 17, 2: PRINT "Pour afficher une note, il faut choisir la valeur de cette note: noire (1 tps)"
LOCATE 18, 2: PRINT "blanche (2 tps), ronde (3), croche (0.5), double croche (0.25). Le temps de la"
LOCATE 19, 2: PRINT "note dÇtermine sa longueur: 4 croches=2 noires=1 blanche, 1 ronde=3 noires.   "
LOCATE 20, 2: PRINT "Pour changer la longueur d'une note, placez le curseur jaune en face de       "
ELSEIF aide = 2 THEN
COLOR 13
LOCATE 21, 12: PRINT "Appuyez sur ENTREE pour poursuivre ou sur ESC pour fermer l'aide."
COLOR 15

LOCATE 13, 2: PRINT "-valeur de la note- et appuyez sur ESPACE. Si vous appuyez sur les fläches de "
LOCATE 14, 2: PRINT "gauche et de droite vous changez le nom de la note signalÇ en bas de l'Çcran. "
LOCATE 15, 2: PRINT "Pour afficher la note sur la portÇe musicale, positionnez-vous en face de     "
LOCATE 16, 2: PRINT "-valider- et appuyez sur ESPACE. Si vous dÇsirez afficher des silences dans le"
LOCATE 17, 2: PRINT "morceau, pointez le curseur en face de -signe- et appuyez sur ESPACE. Comme   "
LOCATE 18, 2: PRINT "pour les notes les silences durent un certain temps: soupir (1 tps), demi-sou-"
LOCATE 19, 2: PRINT "-pirs (0.5), quart de soupir (0.25), pause (4), demi-pause (2). Ex: 1 pause=  "
LOCATE 20, 2: PRINT "4 soupirs = 16 demi-soupirs."
ELSEIF aide = 3 THEN
COLOR 13
LOCATE 21, 12: PRINT "Appuyez sur ENTREE pour poursuivre ou sur ESC pour fermer l'aide."
COLOR 15

LOCATE 13, 2: PRINT "La plupart du temps vous ajoutez des notes les unes Ö la suite des autres. On "
LOCATE 14, 2: PRINT "peut aussi -supprimer-, -modifier-, -insÇrer-, des notes ou des silences. On  "
LOCATE 15, 2: PRINT "change ces diffÇrentes actions avec le curseur et ESPACE en face de -mode-.   "
LOCATE 16, 2: PRINT "Pour indiquer quelle note doit àtre modifiÇe, supprimÇe, ou Ö quel endroit on "
LOCATE 17, 2: PRINT "doit insÇrer une note, on dÇplace le rectangle rouge au dessus de la portÇe   "
LOCATE 18, 2: PRINT "musicale. Attention, pour dÇplacer le rectangle, le curseur jaune doit se     "
LOCATE 19, 2: PRINT "trouver en face de -mode-, sinon on dÇplace le rectangle bleu pour choisir le "
LOCATE 20, 2: PRINT "le nom d'une note, en bas de l'Çcran. Diäse/bÇmol ajoute 1/2 ton Ö la note.   "

ELSEIF aide = 4 THEN
COLOR 13
LOCATE 21, 12: PRINT "Appuyez sur ENTREE pour poursuivre ou sur ESC pour fermer l'aide."
COLOR 15

LOCATE 13, 2: PRINT "Avec les touches F2 et F3, vous pouvez vous diriger dans le morceau. En appuya"
LOCATE 14, 2: PRINT "nt sur F2, vous pouvez voir la partie du morceau Ö gauche de celle prÇsente Ö "
LOCATE 15, 2: PRINT "l'Çcran. Il est possible d'Çcouter tout le morceau: pour cela appuyez sur F10."
LOCATE 16, 2: PRINT "Si vous ne voulez Çcouter que la partie du morceau affichÇe Ö l'Çcran, appuyez"
LOCATE 17, 2: PRINT "sur F5. Pour sortir de l'Çdition utilisez la touche ESC ou Echap. Il reste la "
LOCATE 18, 2: PRINT "partie du menu -options-. Si vous appuyez sur ESPACE lorsque le curseur est en"
LOCATE 19, 2: PRINT "face de -options-, vous accÇdez Ö un autre menu qui permet de changer les op- "
LOCATE 20, 2: PRINT "-tions du morceau lors de sa lecture. ESC permet de revenir au menu prÇcÇdent."
ELSEIF aide = 5 THEN
COLOR 13
LOCATE 21, 12: PRINT "Appuyez sur ESC pour fermer l'aide."
COLOR 15

LOCATE 13, 2: PRINT "             €€€€€€€€€    €€€€€€€€€ €€€€€€€€    €€    €€      €€"
LOCATE 14, 2: PRINT "             €        €   €       € €          €  €   € €    € €"
LOCATE 15, 2: PRINT "             €         €  €       € €         €    €  €  €  €  €"
LOCATE 16, 2: PRINT "             €          € €       € €        €      € €   €€   €"
LOCATE 17, 2: PRINT "             €         €  €       € €   €€€€ €€€€€€€€ €        €"
LOCATE 18, 2: PRINT "             €        €   €       € €      € €      € €        €"
LOCATE 19, 2: PRINT "             €€€€€€€€€    €€€€€€€€€ €€€€€€€€ €      € €        €"
COLOR 12
LOCATE 20, 2: PRINT "             "; STRING$(51, 240)
aide = 0

END IF

2568
LOCATE 1, 15: COLOR 12
pausoi = pausoi + 1
IF pausoi = 1 THEN PRINT "PAUSE" ELSE PRINT "     "
IF pausoi = 2 THEN pausoi = 0
misezero = INT(TIMER)
2648
IF INT(TIMER) > misezero + 1 OR TIMER < 5 THEN 2568

a$ = INKEY$: IF LEN(a$) = 0 THEN GOTO 2648
IF ASC(a$) = 27 AND aide = 0 THEN GOTO 2687
IF ASC(a$) <> 13 AND ASC(a$) <> 27 THEN GOTO 2568
IF ASC(a$) = 13 THEN GOTO 1236
2687

LOCATE 1, 15: PRINT "     "

FOR cadre = 12 TO 21
LOCATE cadre, 1
PRINT STRING$(80, 32)
NEXT

affichedite
afficheopte
affichenoms
affiche


END SUB

SUB helpmenumorceau
COLOR 14
LOCATE 9, 1: PRINT CHR$(218); STRING$(78, 196); CHR$(191)
FOR cadre = 10 TO 22
PRINT CHR$(179); STRING$(78, 32); CHR$(179)
NEXT
PRINT CHR$(192); STRING$(78, 196); CHR$(217)

COLOR 15
LOCATE 10, 2: PRINT "Aide:"
LOCATE 12, 2: PRINT "Ce menu permet de gÇrer vos diffÇrents morceaux de musique. Pour crÇer une   "
LOCATE 13, 2: PRINT "nouvelle mÇlodie, tapez -1-. Si vous dÇsirez charger un morceau enregistrÇ,  "
LOCATE 14, 2: PRINT "appuyez sur la touche -2-. Le nom du morceau s'affichera dans le cadre apräs "
LOCATE 15, 2: PRINT "l'avoir chargÇ. Pour pouvoir visualiser et modifier le morceau appuyez sur la"
LOCATE 16, 2: PRINT "touche -4-:Çditer. Si vous voulez jouer le morceau, la touche -5- permet de  "
LOCATE 17, 2: PRINT "lancer l'Çdition et la lecture du morceau."
LOCATE 18, 2: PRINT "Appuyez sur la touche 1, 2, 3, 4 ou 5 pour sÇlectionner votre choix."
LOCATE 19, 53: PRINT "€€€  €€€€ €€€€ €€€€ €   €"
LOCATE 20, 53: PRINT "€  € €  € €    €  € €€ €€"
LOCATE 21, 53: PRINT "€  € €  € €  € €€€€ € € €"
LOCATE 22, 53: PRINT "€€€  €€€€ €€€€ €  € €   €"


END SUB

SUB inserer
        modification = 1
        affiche
       
        ' 2eme partie attente du choix de la note
529
        LOCATE curs2, 3: COLOR 14: PRINT CHR$(16)
        IF frt2 <> frt THEN
                LINE (frt2 - 15, 273 + 7)-(frt2 + 18, 273 - 7), 0, B
        END IF
        frt2 = frt
        LINE (frt - 15, 273 + 7)-(frt + 18, 273 - 7), 11, B
        IF curs <> curs2 THEN LOCATE curs, 3: PRINT " ": curs = curs2
      
        a$ = INKEY$: IF a$ = "" THEN 529     'verification touches
        mv = ASC(RIGHT$(a$, 1))
        IF mv = 75 AND curs2 <> 13 THEN frt = frt - 520 / 13
        IF mv = 77 AND curs2 <> 13 THEN frt = frt + 520 / 13
        IF frt < 50 + (520 / 13) / 2 THEN frt = 50 + (520 / 13) / 2
        IF frt > 550 THEN frt = 550
        IF mv = 80 THEN
                        curs2 = curs2 + 1
                        IF curs2 > 15 THEN curs2 = 15
                        IF curs2 = 13 THEN curs2 = curs2 + 1
                ELSEIF mv = 72 THEN
                        curs2 = curs2 - 1
                        IF curs2 < 12 THEN curs2 = 12
                        IF curs2 = 13 THEN curs2 = curs2 - 1
                        IF curs2 = 12 AND musi(3, 2) <> 1 THEN curs2 = 14
        END IF

        ' barre d' espacement pour changer les attributs

        IF mv = 32 THEN
                IF curs = 12 THEN musi(1, 2) = musi(1, 2) + 1
                IF curs = 13 THEN musi(2, 2) = musi(2, 2) + 1
                IF curs = 14 THEN musi(3, 2) = musi(3, 2) + 1
                affiche
        END IF
      
        IF mv <> 32 OR curs2 <> 15 THEN GOTO 529

        frk = (frt * 5) / 40
        frk = CINT(frk)

        IF dod = frk THEN b = 130
        IF re = frk THEN b = 125
        IF mi = frk THEN b = 120
        IF fa = frk THEN b = 115
        IF sol = frk THEN b = 110
        IF la = frk THEN b = 105
        IF si = frk THEN b = 100
        IF dod2 = frk THEN b = 95
        IF re2 = frk THEN b = 90
        IF mi2 = frk THEN b = 85
        IF fa2 = frk THEN b = 80
        IF sol2 = frk THEN b = 75
        IF la2 = frk THEN b = 70

        
        sel = selecta
        selfin = selecta
        ' on cherche la derniere note
        DO
        selfin = selfin + 1
        LOOP WHILE musi(selfin - 1, 1) <> -10
        
        ' On decale tout vers la droite
        DO
        musi(selfin, 1) = musi(selfin - 1, 1)
        musi(selfin, 2) = musi(selfin - 1, 2)
        selfin = selfin - 1
       
       
        LOOP WHILE selfin <> sel
        ' on met la nouvel note
       
        IF musi(3, 2) = 1 THEN
                musi(selecta, 1) = b
        ELSE
                musi(selecta, 1) = 10 + musi(3, 2)
        END IF
      
        IF musi(3, 2) = 1 THEN
                IF musi(1, 2) < 4 THEN
                        musi(selecta, 2) = musi(1, 2)
                ELSEIF musi(1, 2) = 4 THEN
                        musi(selecta, 2) = musi(1, 2) / 8
                ELSEIF musi(1, 2) = 5 THEN
                        musi(selecta, 2) = (musi(1, 2) - 1) / 16
                END IF
        ELSE
                IF musi(3, 2) = 2 THEN
                        musi(selecta, 2) = 1
                ELSEIF musi(3, 2) = 3 THEN
                        musi(selecta, 2) = .5
                ELSEIF musi(3, 2) = 4 THEN
                        musi(selecta, 2) = .25
                ELSEIF musi(3, 2) = 5 THEN
                        musi(selecta, 2) = 4
                ELSEIF musi(3, 2) = 6 THEN
                        musi(selecta, 2) = 2
                END IF
        END IF
       
        modification = 0

        affichenote
        affiche
      

END SUB

SUB jouela
COLOR 12
clrcom
LOCATE 18, 6: PRINT "Mise en mÇmoire du morceau. Patientez..."

octave = musi(7, 2)
minute = musi(8, 2)
loref = 12
morceau$ = ""
IF musi(9, 2) = 1 THEN
opt$ = "MN"
ELSEIF musi(9, 2) = 2 THEN
opt$ = "ML"
ELSEIF musi(9, 2) = 3 THEN
opt$ = "MS"
END IF

IF musi(6, 2) = 2 THEN octave = octave - 2
PLAY "O" + RIGHT$(STR$(octave), LEN(STR$(octave)) - 1) + " " + opt$ + " T" + RIGHT$(STR$(minute), LEN(STR$(minute)) - 1) + " MB"

total = 0
2145
IF musi(atournera, 2) = 1 THEN thetemps = loref
IF musi(atournera, 2) = 2 OR musi(atournera, 1) = 16 THEN thetemps = loref / 2
IF musi(atournera, 2) = 3 THEN thetemps = loref / 3
IF musi(atournera, 2) = 4 THEN thetemps = loref / 4
IF musi(atournera, 2) = .5 THEN thetemps = loref * 2
IF musi(atournera, 2) = .25 THEN thetemps = loref * 4

IF musi(6, 2) = 2 THEN jkh = 10 ELSE jkh = 0

IF musi(atournera, 1) < 200 THEN dise$ = "": labonnenote = musi(atournera, 1)
IF musi(atournera, 1) > 200 AND musi(atournera, 1) < 500 THEN dise$ = "+": labonnenote = musi(atournera, 1) - 200
IF musi(atournera, 1) > 500 THEN dise$ = "-": labonnenote = musi(atournera, 1) - 500


IF labonnenote = 75 AND musi(6, 2) = 2 THEN hauteur$ = ">b"
IF labonnenote = 70 AND musi(6, 2) = 2 THEN hauteur$ = ">c"
IF labonnenote = jkh + 130 THEN hauteur$ = "c"
IF labonnenote = jkh + 125 THEN hauteur$ = "d"
IF labonnenote = jkh + 120 THEN hauteur$ = "e"
IF labonnenote = jkh + 115 THEN hauteur$ = "f"
IF labonnenote = jkh + 110 THEN hauteur$ = "g"
IF labonnenote = jkh + 105 THEN hauteur$ = "a"
IF labonnenote = jkh + 100 THEN hauteur$ = "b"
IF labonnenote = jkh + 95 THEN hauteur$ = ">c"
IF labonnenote = jkh + 90 THEN hauteur$ = ">d"
IF labonnenote = jkh + 85 THEN hauteur$ = ">e"
IF labonnenote = jkh + 80 THEN hauteur$ = ">f"
IF labonnenote = jkh + 75 THEN hauteur$ = ">g"
IF labonnenote = jkh + 70 THEN hauteur$ = ">a"

IF labonnenote > 11 AND labonnenote < 17 THEN hauteur$ = "P"


thetp$ = STR$(thetemps)
thetp$ = RIGHT$(thetp$, LEN(thetp$) - 1)
morceau$ = morceau$ + hauteur$ + dise$ + thetp$
IF LEFT$(hauteur$, 1) = ">" THEN
morceau$ = morceau$ + "<"
ELSEIF LEFT$(hauteur$, 1) = "<" THEN
morceau$ = morceau$ + ">"
END IF
atournera = atournera + 1
total = total + 1
IF musi(atournera, 1) <> -10 AND atournera - 1 <> atournepas THEN GOTO 2145
clrcom
LOCATE 18, 6
PRINT "Lecture du morceau. Patientez..."
PLAY morceau$
clrcom
LOCATE 18, 6: PRINT "Pour revenir Ö l'Çdition: ESPACE.Il reste "

DO
a$ = INKEY$: IF LEN(a$) = 0 THEN 2369
IF ASC(a$) = 32 THEN EXIT DO
2369
LOCATE 18, 47
IF PLAY(1) > 1 THEN PRINT PLAY(1); "notes sur"; total; ". " ELSE PRINT PLAY(1); "note sur"; total; ". "
LOOP WHILE PLAY(1) > 0
clrcom

END SUB

SUB jouer
IF musi(10, 1) <> -10 AND musi(10, 1) <> 0 THEN
jouons = -125
editer
jouons = 0
END IF
END SUB

SUB lesoupir
sup = 100

LINE (nonume, sup + 10)-(nonume - 4, sup + 7), 13
LINE (nonume - 4, sup + 7)-(nonume + 6, sup + 5), 13
LINE (nonume + 6, sup + 5)-(nonume, sup), 13
LINE (nonume - 2, sup)-(nonume + 4, sup - 10), 13
LINE (nonume + 4, sup - 10)-(nonume, sup - 15), 13


LINE (nonume + 1, sup + 10)-(nonume - 4 + 1, sup + 7), 13
LINE (nonume + 1 - 4, sup + 7)-(nonume + 1 + 6, sup + 5), 13
LINE (nonume + 1 + 6, sup + 5)-(nonume + 1, sup), 13
LINE (nonume + 1 - 2, sup)-(nonume + 1 + 4, sup - 10), 13
LINE (nonume + 1 + 4, sup - 10)-(nonume + 1, sup - 15), 13
                                        
LINE (nonume - 1, sup + 10)-(nonume - 4 - 1, sup + 7), 13
LINE (nonume - 1 - 4, sup + 7)-(nonume - 1 + 6, sup + 5), 13
LINE (nonume - 1 + 6, sup + 5)-(nonume - 1, sup), 13
LINE (nonume - 1 - 2, sup)-(nonume + 4 - 1, sup - 10), 13
LINE (nonume - 1 + 4, sup - 10)-(nonume - 1, sup - 15), 13

END SUB

SUB MOdif
        modification = 1
        clrcom
        COLOR 12
        LOCATE 18, 6: PRINT "Choisissez la note Ö afficher et validez avec ESPACE."
        desc = 7
        FOR descente = 0 TO 94 STEP 7
        affcurse
        misezero = TIMER
        DO: LOOP WHILE TIMER < misezero + .01 AND TIMER > 5
        NEXT
        desc = -7
        FOR descente = 102 TO 1 STEP -7
        affcurse
        misezero = TIMER
        DO: LOOP WHILE TIMER < misezero + .01 AND TIMER > 5
        NEXT
        desc = 0
        affiche
        ' 2eme partie attente du choix de la note
526
        LOCATE curs2, 3: COLOR 14: PRINT CHR$(16)
        IF frt2 <> frt THEN
                LINE (frt2 - 15, 273 + 7)-(frt2 + 18, 273 - 7), 0, B
        END IF
        frt2 = frt
        LINE (frt - 15, 273 + 7)-(frt + 18, 273 - 7), 11, B
        IF curs <> curs2 THEN LOCATE curs, 3: PRINT " ": curs = curs2
       
        a$ = INKEY$: IF a$ = "" THEN 526     'verification touches
        mv = ASC(RIGHT$(a$, 1))
        IF mv = 75 AND curs2 <> 13 THEN frt = frt - 520 / 13
        IF mv = 77 AND curs2 <> 13 THEN frt = frt + 520 / 13
        IF frt < 50 + (520 / 13) / 2 THEN frt = 50 + (520 / 13) / 2
        IF frt > 550 THEN frt = 550
        IF mv = 80 THEN
                        curs2 = curs2 + 1
                        IF curs2 > 15 THEN curs2 = 15
                        IF curs2 = 13 THEN curs2 = curs2 + 1
                ELSEIF mv = 72 THEN
                        curs2 = curs2 - 1
                        IF curs2 < 12 THEN curs2 = 12
                        IF curs2 = 13 THEN curs2 = curs2 - 1
                        IF curs2 = 12 AND musi(3, 2) <> 1 THEN curs2 = 14
        END IF
        ' barre d' espacement pour changer les attributs

        IF mv = 32 THEN
                IF curs = 12 THEN musi(1, 2) = musi(1, 2) + 1
                IF curs = 13 THEN musi(2, 2) = musi(2, 2) + 1
                IF curs = 14 THEN musi(3, 2) = musi(3, 2) + 1
                affiche
        END IF
       
        IF mv <> 32 OR curs2 <> 15 THEN GOTO 526

        frk = (frt * 5) / 40
        frk = CINT(frk)

        IF dod = frk THEN b = 130
        IF re = frk THEN b = 125
        IF mi = frk THEN b = 120
        IF fa = frk THEN b = 115
        IF sol = frk THEN b = 110
        IF la = frk THEN b = 105
        IF si = frk THEN b = 100
        IF dod2 = frk THEN b = 95
        IF re2 = frk THEN b = 90
        IF mi2 = frk THEN b = 85
        IF fa2 = frk THEN b = 80
        IF sol2 = frk THEN b = 75
        IF la2 = frk THEN b = 70

        IF musi(3, 2) = 1 THEN
                musi(selecta, 1) = b
        ELSE
                musi(selecta, 1) = 10 + musi(3, 2)
        END IF

        IF musi(3, 2) = 1 THEN
                IF musi(1, 2) < 4 THEN
                        musi(selecta, 2) = musi(1, 2)
                ELSEIF musi(1, 2) = 4 THEN
                        musi(selecta, 2) = musi(1, 2) / 8
                ELSEIF musi(1, 2) = 5 THEN
                        musi(selecta, 2) = (musi(1, 2) - 1) / 16
                END IF
        ELSE
                IF musi(3, 2) = 2 THEN
                        musi(selecta, 2) = 1
                ELSEIF musi(3, 2) = 3 THEN
                        musi(selecta, 2) = .5
                ELSEIF musi(3, 2) = 4 THEN
                        musi(selecta, 2) = .25
                ELSEIF musi(3, 2) = 5 THEN
                        musi(selecta, 2) = 4
                ELSEIF musi(3, 2) = 6 THEN
                        musi(selecta, 2) = 2
               
                END IF
        END IF
                modification = 0

        affichenote
        affiche
       
END SUB

SUB morceaux

plus = 0
valref = 72
noplace = 10
noref = 10
ref = 110
musi(10, 1) = -10
fichier$ = "-SANSNOM-"
musi(6, 2) = 1
musi(7, 2) = 3
musi(8, 2) = 32
musi(9, 2) = 40
549 SCREEN 0
CLS
56 colr = INT(RND * 16)
IF colr = 0 THEN 56
COLOR colr
LOCATE 15, 10: PRINT "€€€€€€€€€    €€€€€€€€€ €€€€€€€€    €€    €€      €€"
LOCATE 16, 10: PRINT "€        €   €       € €          €  €   € €    € €"
LOCATE 17, 10: PRINT "€         €  €       € €         €    €  €  €  €  €"
LOCATE 18, 10: PRINT "€          € €       € €        €      € €   €€   €"
LOCATE 19, 10: PRINT "€         €  €       € €   €€€€ €€€€€€€€ €        €"
LOCATE 20, 10: PRINT "€        €   €       € €      € €      € €        €"
LOCATE 21, 10: PRINT "€€€€€€€€€    €€€€€€€€€ €€€€€€€€ €      € €        €"


primo = 0
temp = 0
548
COLOR 12
LOCATE 1, 30:
PRINT "Morceaux de musique:"
COLOR 14
LOCATE 1, 54: PRINT "F1 = Aide"
COLOR 10
LOCATE 3, 12: PRINT "1 - Nouveau                     ⁄ƒƒƒƒƒƒƒƒƒƒƒƒø "
LOCATE 4, 12: PRINT "2 - Ouvrir                      ≥            ≥"
LOCATE 4, 45: PRINT fichier$
LOCATE 5, 12: PRINT "3 - Sauver                      ¿ƒƒƒƒƒƒƒƒƒƒƒƒŸ"
LOCATE 6, 12: PRINT "4 - Editer"
LOCATE 7, 12: PRINT "5 - Jouer"
LOCATE 8, 12: PRINT "6 - Quitter"
LOCATE 10, 22: PRINT "Tapez 1, 2 ,3, 4, 5 ou 6 comme sÇlection:"

'REM REcherche des touches

547 COLOR 7: LOCATE 7, 44: PRINT "Heure: "; TIME$
a$ = INKEY$: IF LEN(a$) = 0 THEN 547
mv = ASC(RIGHT$(a$, 1))

IF mv = 49 THEN

nouveau
GOTO 549
ELSEIF mv = 50 THEN
Ouvrir
GOTO 549
ELSEIF mv = 51 THEN
sauver
GOTO 549
ELSEIF mv = 52 THEN
IF musi(10, 1) = -10 OR musi(10, 1) = 0 THEN nouveau ELSE editer
GOTO 549
ELSEIF mv = 53 THEN
jouer
GOTO 549
ELSEIF mv = 54 OR mv = 27 THEN
GOTO 2354
ELSEIF mv = 59 THEN
helpmenumorceau
ELSE
GOTO 547
END IF

GOTO 547

2354
sortir
IF passage = 0 THEN GOTO 549
IF passage = 5 THEN sauver
CLS
END SUB

SUB nouveau
sortir
IF passage = 5 THEN sauver
IF passage = 0 THEN EXIT SUB
ERASE musi
vc = 0
vc2 = 0
musi(10, 1) = -10
noplace = 10
noref = 10
nonume = 90
musi(6, 2) = 1
musi(7, 2) = 3
musi(8, 2) = 32
musi(9, 2) = 40


fichier$ = "-SANSNOM-"

editer

END SUB

SUB Options

COLOR 14
FOR tourne = 3 TO 38 STEP 2
LOCATE 16, tourne: PRINT "  "; CHR$(16)
misezero = TIMER
DO: LOOP WHILE TIMER < misezero + .01 AND TIMER > 5
NEXT
COLOR 10: LOCATE 16, 37: PRINT CHR$(179)
clrcom
528
commentopt
LOCATE curs2, 39: COLOR 14: PRINT CHR$(16)

IF curs <> curs2 THEN LOCATE curs, 39: PRINT " ": curs = curs2

a$ = INKEY$: IF a$ = "" THEN 528     'verification touches
mv = ASC(RIGHT$(a$, 1))
IF mv = 27 THEN LOCATE curs2, 39: PRINT " ": GOTO 569

' Curseur jaune
        IF mv = 80 THEN
                curs2 = curs2 + 1
                IF curs2 > 16 THEN curs2 = 16
        ELSEIF mv = 72 THEN
                curs2 = curs2 - 1
                IF curs2 < 13 THEN curs2 = 13
        END IF

' Changement de clef

IF mv = 32 AND curs = 13 THEN
musi(6, 2) = musi(6, 2) + 1
affiche
affichenoms
cle = musi(6, 2) - 1

VERIFDIESE
affichenote
END IF

' Changement d'octave
IF mv = 32 AND curs = 14 THEN
musi(7, 2) = musi(7, 2) + 1
affiche
END IF

'Changement du nombre de note par minute
IF mv = 32 AND curs = 15 THEN
musi(8, 2) = musi(8, 2) + 5
affiche
END IF

' Changement du deroulement du morceau
IF mv = 32 AND curs = 16 THEN
musi(9, 2) = musi(9, 2) + 1
affiche
END IF

GOTO 528


569
LOCATE curs2, 38: PRINT " "
curs2 = 16
curs = 16
COLOR 14
FOR tourne = 37 TO 3 STEP -2
LOCATE 16, tourne: PRINT CHR$(16); "  "
misezero = TIMER
DO: LOOP WHILE TIMER < misezero + .01 AND TIMER > 5
IF tourne = 33 THEN COLOR 10: LOCATE 16, 37: PRINT CHR$(179): COLOR 14
NEXT
COLOR 10
LOCATE 16, 5: PRINT "Options..."
COLOR 14
clrcom
END SUB

SUB Ouvrir
sortir
IF passage = 5 THEN sauver
IF passage = 0 THEN EXIT SUB
CLS
COLOR 12
LOCATE 1, 37: PRINT "Ouvrir:"
COLOR 10
LOCATE 5, 5: PRINT "Cherchez dans la liste suivante le fichier que vous dÇsirez ouvrir."
apui
COLOR 11
CLS
FILES "*.mus"
apui
CLS
CLS
COLOR 12
LOCATE 1, 37: PRINT "Ouvrir:"
COLOR 10

2696
COLOR 14
LOCATE 5, 5: PRINT "                                                                     "
LOCATE 5, 5: INPUT "Nom du fichier sans suffixe:"; thenom$
IF LEN(thenom$) > 8 THEN 2696
passage = 1
FOR yui = 1 TO LEN(thenom$)
l = ASC(MID$(thenom$, yui, 1))
IF l < 48 THEN passage = 0
IF l > 57 AND l < 65 THEN passage = 0
IF l > 90 AND l < 97 THEN passage = 0
IF l > 122 THEN passage = 0
NEXT
IF passage = 0 THEN 2696
fichier1$ = thenom$ + ".mus"
passages = 1



REM ON ERROR GOTO 2987
OPEN fichier1$ FOR INPUT AS #1
CLOSE
IF passages = 0 THEN EXIT SUB
COLOR 10
LOCATE 7, 5: PRINT "Fichier chargÇ."
fichier$ = fichier1$



' chargement
OPEN fichier$ FOR RANDOM AS #1
DIM lafiche AS fiche
toure = 0
DO
toure = toure + 1
GET #1, toure, lafiche
musi(toure, 1) = lafiche.haut
musi(toure, 2) = lafiche.nature
LOOP WHILE musi(toure, 1) <> -10
CLOSE #1


apui

END SUB

SUB pause
sup = 81
LINE (nonume - 8, sup)-(nonume + 8, sup + 4), 13, BF

END SUB

SUB quartsoupir
sup = 110

LINE (nonume + 1, sup)-(nonume + 5, sup - 15), 13
CIRCLE (nonume - 1, sup - 3 - 15 + 3), 6, 13, 3.6, 5.8
CIRCLE (nonume - 2, sup - 3 - 15 + 4 + 2), 6, 13, 3.6, 5.8
END SUB

SUB queuAIR



IF b = 130 OR b = 125 OR b = 120 OR b = 115 OR b = 110 OR b = 105 OR b = 100 THEN plusq = -20
IF b < 96 THEN plusq = 20
IF plusq = 20 THEN gadr = -4 ELSE gadr = 4
LINE (a + gadr, b + plus)-(a + gadr, b + plusq + plus), vol


IF musi(noplace, 2) = musi(noplace - 1, 2) AND musi(noplace, 2) = .5 AND nonume <> 90 AND musi(noplace - 1, 1) > 17 THEN
LINE (a + gadr, b + plusq + plus)-(musi(4, 2), musi(5, 2)), vol
END IF
IF musi(noplace, 2) = musi(noplace - 1, 2) AND musi(noplace, 2) = .25 AND nonume <> 90 AND musi(noplace - 1, 1) > 17 THEN
LINE (a + gadr, b + plusq + plus + 3)-(musi(4, 2), musi(5, 2) + 3), vol
LINE (a + gadr, b + plusq + plus)-(musi(4, 2), musi(5, 2)), vol
END IF
IF musi(noplace, 2) = .25 AND plusq = 20 THEN
LINE (a + gadr, b + plusq + plus)-(a + gadr, b + plusq + plus + 3), vol
END IF


musi(4, 2) = a + gadr
musi(5, 2) = b + plusq + plus


END SUB

SUB sauver
IF musi(10, 1) = -10 OR musi(10, 1) = 0 THEN EXIT SUB
CLS
COLOR 12
LOCATE 1, 37: PRINT "Sauver:"
IF fichier$ = "-SANSNOM-" THEN GOTO 2697
COLOR 10
LOCATE 10, 5: PRINT "Ce morceau est dÇjÖ sauvÇ sous "; fichier$
COLOR 14
LOCATE 12, 5: PRINT "Sauver sous le màme nom ? "
COLOR 10
LOCATE 12, 35: PRINT "Oui"
LOCATE 13, 35: PRINT "Non"
LOCATE 14, 35: PRINT "Annuler"
curseur = 12
IF passage = 5 THEN GOTO 2698
523
COLOR 14
LOCATE curseur, 33: PRINT CHR$(16)
a$ = INKEY$: IF a$ = "" THEN 523     'verification touches
mv = ASC(RIGHT$(a$, 1))
IF mv <> 80 AND mv <> 72 AND mv <> 13 THEN 523
IF mv = 13 AND curseur = 14 THEN EXIT SUB
IF mv = 13 AND curseur = 12 THEN 2698
IF mv = 13 AND curseur = 13 THEN 2697
LOCATE curseur, 33: PRINT " "
IF mv = 80 THEN curseur = curseur + 1
IF mv = 72 THEN curseur = curseur - 1
IF curseur = 11 THEN curseur = 12
IF curseur = 15 THEN curseur = 14
GOTO 523

2697
COLOR 14
LOCATE 16, 5: PRINT "                                                                      "
LOCATE 16, 5: INPUT "Nom du fichier sans suffixe:", thenom$
IF LEN(thenom$) > 8 THEN 2697
passage = 1
FOR yui = 1 TO LEN(thenom$)
l = ASC(MID$(thenom$, yui, 1))
IF l < 48 THEN passage = 0
IF l > 57 AND l < 65 THEN passage = 0
IF l > 90 AND l < 97 THEN passage = 0
IF l > 122 THEN passage = 0
NEXT
IF passage = 0 THEN 2697
fichier$ = thenom$ + ".mus"




' Sauvegarde
2698

COLOR 10
LOCATE 17, 4: PRINT CHR$(218) + STRING$(70, 196) + CHR$(191)
LOCATE 18, 4: PRINT CHR$(179); TAB(75); CHR$(179)
LOCATE 19, 4: PRINT CHR$(192) + STRING$(70, 196) + CHR$(217)

nombret = 1
DO
IF musi(nombret, 1) = -10 THEN EXIT DO
nombret = nombret + 1
LOOP


OPEN fichier$ FOR RANDOM AS #1
DIM lafiche AS fiche
toure = 0
rendueav = 0
DO
toure = toure + 1
lafiche.haut = musi(toure, 1)
lafiche.nature = musi(toure, 2)
PUT #1, toure, lafiche
COLOR 14
rendue = INT((toure * 70) / nombret)
LOCATE 18, 5 + rendueav: PRINT STRING$(rendue - rendueav, CHR$(219))
rendueav = rendue
LOOP WHILE musi(toure, 1) <> -10
CLOSE #1
COLOR 10
LOCATE 20, 5: PRINT "Morceau enregistrÇ dans le fichier "; fichier$
apui

END SUB

SUB sortir
differ = 1
IF fichier$ = "-SANSNOM-" AND musi(10, 1) <> -10 THEN differ = 0: GOTO 568

IF musi(10, 1) = -10 THEN differ = 1: GOTO 568
OPEN fichier$ FOR RANDOM AS #1
DIM lafiche AS fiche
tkshd = 1
DO
GET #1, tkshd, lafiche
IF musi(tkshd, 1) <> lafiche.haut THEN differ = 0
IF musi(tkshd, 2) <> lafiche.nature AND tkshd >= 6 THEN differ = 0
tkshd = tkshd + 1
LOOP WHILE lafiche.haut <> -10

568
IF differ = 1 THEN passage = 1: GOTO 5623

passage = 0
COLOR 10: LOCATE 5, 15: PRINT CHR$(201) + STRING$(48, 205) + CHR$(187)
LOCATE 7, 15: PRINT CHR$(186); "Votre morceau de musique a ÇtÇ modifiÇ.         "; CHR$(186)
COLOR 10
LOCATE 6, 15: PRINT CHR$(186); "                                                "; CHR$(186)
COLOR 12
LOCATE 6, 33: PRINT "ATTENTION!!!"
COLOR 10
LOCATE 8, 15: PRINT CHR$(186); "Voulez vous continuer et perdre le morceau en   "; CHR$(186)
LOCATE 9, 15: PRINT CHR$(186); "cours ou le sauver ?                            "; CHR$(186)
LOCATE 10, 15: PRINT CHR$(200) + STRING$(9, 205); CHR$(187); "   Sauver    "; CHR$(201); STRING$(24, 205); CHR$(188)
LOCATE 11, 25: PRINT CHR$(186); "   Continuer "; CHR$(186)
LOCATE 12, 25: PRINT CHR$(186); "   Annuler   "; CHR$(186)
LOCATE 13, 25: PRINT CHR$(200) + STRING$(13, 205); CHR$(188)
curseur = 10
527
COLOR 14
LOCATE curseur, 27: PRINT CHR$(16)
a$ = INKEY$: IF a$ = "" THEN 527     'verification touches
mv = ASC(RIGHT$(a$, 1))
IF mv <> 80 AND mv <> 72 AND mv <> 13 THEN 527
IF mv = 13 AND curseur = 10 THEN passage = 5: GOTO 5623
IF mv = 13 AND curseur = 11 THEN passage = 1: GOTO 5623
IF mv = 13 AND curseur = 12 THEN passage = 0: GOTO 5623
LOCATE curseur, 27: PRINT " "
IF mv = 80 THEN curseur = curseur + 1
IF mv = 72 THEN curseur = curseur - 1
IF curseur = 13 THEN curseur = 12
IF curseur = 9 THEN curseur = 10
GOTO 527
5623
CLS
CLOSE
END SUB

SUB supprimer
        clrcom
        COLOR 12
        LOCATE 18, 6: PRINT "Choisissez la note Ö afficher et validez avec ESPACE."
        desc = 7
        FOR descente = 0 TO 96 STEP 7
        affcurse
        misezero = TIMER
        DO: LOOP WHILE TIMER < misezero + .01 AND TIMER > 5
        NEXT
        desc = -7
        FOR descente = 102 TO 1 STEP -7
        affcurse
        misezero = TIMER
        DO: LOOP WHILE TIMER < misezero + .01 AND TIMER > 5
        NEXT
        desc = 0
   
' On redecale toute les notes d'un cran
memoselecta = selecta
DO
musi(selecta, 1) = musi(selecta + 1, 1)
musi(selecta, 2) = musi(selecta + 1, 2)
selecta = selecta + 1
LOOP WHILE musi(selecta, 1) <> -10
selecta = memoselecta
affichenote
affiche

END SUB

SUB VERIFDIESE
selecta2 = 10
clrcom
LOCATE 18, 6: COLOR 12: PRINT "Changement de clÇ. VÇrification de la possibilitÇ des diÇse/bÇmol."
SLEEP 1


DO
notesbon = 1

IF musi(selecta2, 1) < 135 THEN 2679
nombret = musi(selecta2, 1)
        IF musi(selecta2, 1) > 500 THEN nombret = musi(selecta2, 1) - 500
        IF musi(selecta2, 1) > 200 AND musi(selecta2, 1) < 500 THEN nombret = musi(selecta2, 1) - 200


IF musi(6, 2) = 1 THEN
        IF (nombret = 120 OR nombret = 100 OR nombret = 85) AND musi(selecta2, 1) > 200 AND musi(selecta2, 1) < 500 THEN notesbon = 0
        IF (nombret = 130 OR nombret = 115 OR nombret = 95 OR nombret = 80) AND musi(selecta2, 1) > 500 THEN notesbon = 0
END IF


IF musi(6, 2) = 2 THEN
        IF (nombret = 125 OR nombret = 105 OR nombret = 70 OR nombret = 90) AND musi(selecta2, 1) > 500 THEN notesbon = 0
        IF (nombret = 130 OR nombret = 110 OR nombret = 75 OR nombret = 95) AND musi(selecta2, 1) > 200 AND musi(selecta2, 1) < 500 THEN notesbon = 0
END IF
IF notesbon = 1 THEN GOTO 2679
musi(selecta2, 1) = nombret
2679
selecta2 = selecta2 + 1
LOOP WHILE musi(selecta2, 1) <> -10

clrcom
END SUB

REM #################################################################################################
REM #†END DOG4
REM #################################################################################################
