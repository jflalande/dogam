DECLARE SUB VERIFDIESE ()
DECLARE SUB diesebemol ()
DECLARE SUB affichedite ()
DECLARE SUB helpediter ()
DECLARE SUB helpmenumorceau ()
DECLARE SUB commentopt ()
DECLARE SUB demipause ()
DECLARE SUB quartsoupir ()
DECLARE SUB demisoupir ()
DECLARE SUB pause ()
DECLARE SUB lesoupir ()
DECLARE SUB sortir ()
DECLARE SUB Options ()
DECLARE SUB inserer ()
DECLARE SUB supprimer ()
DECLARE SUB MOdif ()
DECLARE SUB affcurse ()
DECLARE SUB affichenoms ()
DECLARE SUB comment ()
DECLARE SUB clrcom ()
DECLARE SUB afficheopte ()
DECLARE SUB ligneair ()
DECLARE SUB asar3 ()
DECLARE SUB ASAR4 ()
DECLARE SUB asar2 ()
DECLARE SUB asar ()
DECLARE SUB jouela ()
DECLARE SUB queuAIR ()
DECLARE SUB affichela ()
DECLARE SUB affichenote ()
DECLARE SUB affiche ()
DECLARE SUB sauver ()
DECLARE SUB nouveau ()
DECLARE SUB Ouvrir ()
DECLARE SUB editer ()
DECLARE SUB jouer ()
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
COMMON SHARED nombre, music, a, b, f, rep, bon, not$, l, g, plus, poin
COMMON SHARED ref, temp, p, inte, plusq, esp, tep, kd, gf, tome$, vol
COMMON SHARED ha$, vc, uti0, uti1, uti2, uti3, uti4, uti5, dez, uti6
COMMON SHARED uti7, uti8, uti9, uti10, uti11, frt, colp, dod, re, mi
COMMON SHARED fa, sol, la, si, dod2, re2, mi2, fa2, sol2, bar, nb
COMMON SHARED points, nom$, primo, rend, porq, sop, sup, pouet, uti12
COMMON SHARED la2, aff, vc2, bem, dictee, cligne, colcli, aide, voiraide
COMMON SHARED adictee, bdictee, adicteer, bdicteer, sens

COMMON SHARED fichier$, noplace, nonume, valref, noref, jouons, curs, lecomment
COMMON SHARED passages, selecti, selecta, selecti2, descente, desc, curs2
COMMON SHARED modification, passage, atournera, atournepas

DIM SHARED musi(1000, 2) AS SINGLE

TYPE fiche
haut AS INTEGER
nature AS SINGLE
END TYPE
2987
COLOR 12
LOCATE 7, 5: PRINT "Ce fichier n'existe pas."
apui
passages = 0
RESUME NEXT

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
LOCATE 13, 10: PRINT "DiŠse    "
ELSEIF musi(2, 2) = 6 THEN
LOCATE 13, 10: PRINT "B‚mol    "
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
LOCATE 16, 60: PRINT "li‚es    "
ELSEIF musi(9, 2) = 3 THEN
LOCATE 16, 60: PRINT "d‚tach‚es"
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
LOCATE 3, 30: PRINT "F5 = Jouer l'‚cran"
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
LOCATE 13, 41: PRINT "Cl‚ de"
LOCATE 14, 41: PRINT "Octave num‚ro"
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
        PRINT "En appuyant sur ESPACE, changez l'action … effectuer."
ELSEIF curs = 14 THEN
        PRINT "S‚lectionnez un symbole (note, silence) … afficher avec ESPACE."
ELSEIF curs = 15 THEN
        PRINT "Validez les paramŠtres et affichez la note voulue avec ESPACE."
ELSEIF curs = 16 THEN
        PRINT "Pour changer les caract‚ristiques du morceau, appuyez sur ESPACE."
END IF
lecomment = curs
END SUB

SUB commentopt
IF curs <> lecomment THEN clrcom

COLOR 12
LOCATE 18, 6
IF curs = 13 THEN
        PRINT "En appuyant sur ESPACE, changez la cl‚ du morceau."
ELSEIF curs = 14 THEN
        PRINT "Choisissez l'octave o— le morceau est jou‚."
ELSEIF curs = 15 THEN
        PRINT "D‚termine le nombre de notes jou‚es … la minute."
ELSEIF curs = 16 THEN
        PRINT "Lecture les notes d'une certaine fa‡on."
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


' F2 Aller … gauche

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

' F3  Aller … droite

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

' Quand les notes d‚passes de l'‚cran et qu il y a ajouter
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

' F5 Jouer la partie de l'‚cran

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

' Barre d'espacement et diŠse/b‚mol
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
LOCATE 15, 2: PRINT "Cette partie du logiciel permet de composer une m‚lodie. Avec les flŠches du  "
LOCATE 16, 2: PRINT "bas et du haut, vous dirigez le curseur jaune dans le menu en bas … gauche.   "
LOCATE 17, 2: PRINT "Pour afficher une note, il faut choisir la valeur de cette note: noire (1 tps)"
LOCATE 18, 2: PRINT "blanche (2 tps), ronde (3), croche (0.5), double croche (0.25). Le temps de la"
LOCATE 19, 2: PRINT "note d‚termine sa longueur: 4 croches=2 noires=1 blanche, 1 ronde=3 noires.   "
LOCATE 20, 2: PRINT "Pour changer la longueur d'une note, placez le curseur jaune en face de       "
ELSEIF aide = 2 THEN
COLOR 13
LOCATE 21, 12: PRINT "Appuyez sur ENTREE pour poursuivre ou sur ESC pour fermer l'aide."
COLOR 15

LOCATE 13, 2: PRINT "-valeur de la note- et appuyez sur ESPACE. Si vous appuyez sur les flŠches de "
LOCATE 14, 2: PRINT "gauche et de droite vous changez le nom de la note signal‚ en bas de l'‚cran. "
LOCATE 15, 2: PRINT "Pour afficher la note sur la port‚e musicale, positionnez-vous en face de     "
LOCATE 16, 2: PRINT "-valider- et appuyez sur ESPACE. Si vous d‚sirez afficher des silences dans le"
LOCATE 17, 2: PRINT "morceau, pointez le curseur en face de -signe- et appuyez sur ESPACE. Comme   "
LOCATE 18, 2: PRINT "pour les notes les silences durent un certain temps: soupir (1 tps), demi-sou-"
LOCATE 19, 2: PRINT "-pirs (0.5), quart de soupir (0.25), pause (4), demi-pause (2). Ex: 1 pause=  "
LOCATE 20, 2: PRINT "4 soupirs = 16 demi-soupirs."
ELSEIF aide = 3 THEN
COLOR 13
LOCATE 21, 12: PRINT "Appuyez sur ENTREE pour poursuivre ou sur ESC pour fermer l'aide."
COLOR 15

LOCATE 13, 2: PRINT "La plupart du temps vous ajoutez des notes les unes … la suite des autres. On "
LOCATE 14, 2: PRINT "peut aussi -supprimer-, -modifier-, -ins‚rer-, des notes ou des silences. On  "
LOCATE 15, 2: PRINT "change ces diff‚rentes actions avec le curseur et ESPACE en face de -mode-.   "
LOCATE 16, 2: PRINT "Pour indiquer quelle note doit ˆtre modifi‚e, supprim‚e, ou … quel endroit on "
LOCATE 17, 2: PRINT "doit ins‚rer une note, on d‚place le rectangle rouge au dessus de la port‚e   "
LOCATE 18, 2: PRINT "musicale. Attention, pour d‚placer le rectangle, le curseur jaune doit se     "
LOCATE 19, 2: PRINT "trouver en face de -mode-, sinon on d‚place le rectangle bleu pour choisir le "
LOCATE 20, 2: PRINT "le nom d'une note, en bas de l'‚cran. DiŠse/b‚mol ajoute 1/2 ton … la note.   "

ELSEIF aide = 4 THEN
COLOR 13
LOCATE 21, 12: PRINT "Appuyez sur ENTREE pour poursuivre ou sur ESC pour fermer l'aide."
COLOR 15

LOCATE 13, 2: PRINT "Avec les touches F2 et F3, vous pouvez vous diriger dans le morceau. En appuya"
LOCATE 14, 2: PRINT "nt sur F2, vous pouvez voir la partie du morceau … gauche de celle pr‚sente … "
LOCATE 15, 2: PRINT "l'‚cran. Il est possible d'‚couter tout le morceau: pour cela appuyez sur F10."
LOCATE 16, 2: PRINT "Si vous ne voulez ‚couter que la partie du morceau affich‚e … l'‚cran, appuyez"
LOCATE 17, 2: PRINT "sur F5. Pour sortir de l'‚dition utilisez la touche ESC ou Echap. Il reste la "
LOCATE 18, 2: PRINT "partie du menu -options-. Si vous appuyez sur ESPACE lorsque le curseur est en"
LOCATE 19, 2: PRINT "face de -options-, vous acc‚dez … un autre menu qui permet de changer les op- "
LOCATE 20, 2: PRINT "-tions du morceau lors de sa lecture. ESC permet de revenir au menu pr‚c‚dent."
ELSEIF aide = 5 THEN
COLOR 13
LOCATE 21, 12: PRINT "Appuyez sur ESC pour fermer l'aide."
COLOR 15

LOCATE 13, 2: PRINT "             ÛÛÛÛÛÛÛÛÛ    ÛÛÛÛÛÛÛÛÛ ÛÛÛÛÛÛÛÛ    ÛÛ    ÛÛ      ÛÛ"
LOCATE 14, 2: PRINT "             Û        Û   Û       Û Û          Û  Û   Û Û    Û Û"
LOCATE 15, 2: PRINT "             Û         Û  Û       Û Û         Û    Û  Û  Û  Û  Û"
LOCATE 16, 2: PRINT "             Û          Û Û       Û Û        Û      Û Û   ÛÛ   Û"
LOCATE 17, 2: PRINT "             Û         Û  Û       Û Û   ÛÛÛÛ ÛÛÛÛÛÛÛÛ Û        Û"
LOCATE 18, 2: PRINT "             Û        Û   Û       Û Û      Û Û      Û Û        Û"
LOCATE 19, 2: PRINT "             ÛÛÛÛÛÛÛÛÛ    ÛÛÛÛÛÛÛÛÛ ÛÛÛÛÛÛÛÛ Û      Û Û        Û"
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
LOCATE 12, 2: PRINT "Ce menu permet de g‚rer vos diff‚rents morceaux de musique. Pour cr‚er une   "
LOCATE 13, 2: PRINT "nouvelle m‚lodie, tapez -1-. Si vous d‚sirez charger un morceau enregistr‚,  "
LOCATE 14, 2: PRINT "appuyez sur la touche -2-. Le nom du morceau s'affichera dans le cadre aprŠs "
LOCATE 15, 2: PRINT "l'avoir charg‚. Pour pouvoir visualiser et modifier le morceau appuyez sur la"
LOCATE 16, 2: PRINT "touche -4-:‚diter. Si vous voulez jouer le morceau, la touche -5- permet de  "
LOCATE 17, 2: PRINT "lancer l'‚dition et la lecture du morceau."
LOCATE 18, 2: PRINT "Appuyez sur la touche 1, 2, 3, 4 ou 5 pour s‚lectionner votre choix."
LOCATE 19, 53: PRINT "ÛÛÛ  ÛÛÛÛ ÛÛÛÛ ÛÛÛÛ Û   Û"
LOCATE 20, 53: PRINT "Û  Û Û  Û Û    Û  Û ÛÛ ÛÛ"
LOCATE 21, 53: PRINT "Û  Û Û  Û Û  Û ÛÛÛÛ Û Û Û"
LOCATE 22, 53: PRINT "ÛÛÛ  ÛÛÛÛ ÛÛÛÛ Û  Û Û   Û"


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
LOCATE 18, 6: PRINT "Mise en m‚moire du morceau. Patientez..."

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
LOCATE 18, 6: PRINT "Pour revenir … l'‚dition: ESPACE.Il reste "

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
        LOCATE 18, 6: PRINT "Choisissez la note … afficher et validez avec ESPACE."
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
LOCATE 1, 30:
PRINT "Morceaux de musique:"
COLOR 14
LOCATE 1, 54: PRINT "F1 = Aide"
COLOR 10
LOCATE 3, 12: PRINT "1 - Nouveau                     ÚÄÄÄÄÄÄÄÄÄÄÄÄ¿ "
LOCATE 4, 12: PRINT "2 - Ouvrir                      ³            ³"
LOCATE 4, 45: PRINT fichier$
LOCATE 5, 12: PRINT "3 - Sauver                      ÀÄÄÄÄÄÄÄÄÄÄÄÄÙ"
LOCATE 6, 12: PRINT "4 - Editer"
LOCATE 7, 12: PRINT "5 - Jouer"
LOCATE 8, 12: PRINT "6 - Quitter"
LOCATE 10, 22: PRINT "Tapez 1, 2 ,3, 4, 5 ou 6 comme s‚lection:"

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
LOCATE 5, 5: PRINT "Cherchez dans la liste suivante le fichier que vous d‚sirez ouvrir."
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



ON ERROR GOTO 2987
OPEN fichier1$ FOR INPUT AS #1
CLOSE
IF passages = 0 THEN EXIT SUB
COLOR 10
LOCATE 7, 5: PRINT "Fichier charg‚."
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
LOCATE 10, 5: PRINT "Ce morceau est d‚j… sauv‚ sous "; fichier$
COLOR 14
LOCATE 12, 5: PRINT "Sauver sous le mˆme nom ? "
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
LOCATE 20, 5: PRINT "Morceau enregistr‚ dans le fichier "; fichier$
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
LOCATE 7, 15: PRINT CHR$(186); "Votre morceau de musique a ‚t‚ modifi‚.         "; CHR$(186)
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
        LOCATE 18, 6: PRINT "Choisissez la note … afficher et validez avec ESPACE."
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
LOCATE 18, 6: COLOR 12: PRINT "Changement de cl‚. V‚rification de la possibilit‚ des di‚se/b‚mol."
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

