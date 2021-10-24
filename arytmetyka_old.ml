type wartosc = float*float;;

(*---------- POMOCNICZE FUNKCJE ----------*)
(* funkcja obliczająca wartość bezwzględną *)
let wartosc_bez (x:float) =
    if x<0.0 then
        x*.(-1.0)
    else
        x
;;

(* funkcja obliczająca minimum spośród 4 liczb *)
let min_helper (a:float) (b:float) (c:float) (d:float) = 
    if a<b then
        if a<c  then
            if a<d  then
                a
            else
                d
        else 
            if c<d then
                c
            else
                d
    else 
        if b<c then
            if b<d then
                b
            else
                d
        else 
            if c<d then
                c
            else
            d
;;

(* funkcja obliczająca maksimum spośród 4 liczb *)
let max_helper (a:float) (b:float) (c:float) (d:float) = 
    if a>b then
        if a>c  then
            if a>d  then
                a
            else
                d
        else 
            if c>d then
                c
            else
                d
    else 
        if b>c then
            if b>d then
                b
            else
                d
        else 
            if c>d then
                c
            else
            d
;;

(* funkcja do wypisywania sumy zbiorów *)
let suma_zb (a:wartosc) (b:wartosc) = ((a:wartosc),(b:wartosc));;


(*---------- KONSTRUKTORY ----------*)
let wartosc_dokladna (x:float) = ((x,x):wartosc);;

let wartosc_od_do (x:float) (y:float) = ((x,y):wartosc);;

let wartosc_dokladnosc (x:float) (p:float) = (((x-.(wartosc_bez x*.(p/.100.0))),(x+.(wartosc_bez x*.(p/.100.0)))):wartosc);;


(*---------- SELEKTORY ----------*)
let in_wartosc (x:float) (y:float) = 
    if x=y then
        true
    else
        false;;

let min_wartosc (x:wartosc) = fst x;;

let max_wartosc (x:wartosc) = snd x;;

let sr_wartosc (x:wartosc) = 
    if fst x=neg_infinity && snd x=infinity then
        nan
    else
        (fst x+.snd x)/.2.0 
;;


(*---------- MODYFIKATORY ----------*)
let plus (a:wartosc) (b:wartosc) = (((fst a+.fst b),(snd a+.snd b)):wartosc);;

let minus (a:wartosc) (b:wartosc) = (((fst a-.snd b),(snd a-.fst b)):wartosc);;

let razy (a:wartosc) (b:wartosc) = ((min_helper (fst a*.fst b) (fst a*.snd b) (snd a*.fst b) (snd a*.snd b)),(max_helper (fst a*.fst b) (fst a*.snd b) (snd a*.fst b) (snd a*.snd b)):wartosc);;

let podzielic (a:wartosc) (b:wartosc) = 
    if fst b<0.0 && snd b>0.0 then
        if fst (razy (neg_infinity, fst b) a)>fst (razy (snd b, infinity) a) then
            (*suma_zb (razy (snd b, infinity) a) (razy (neg_infinity, fst b) a)*)
            (0.0,0.0)
        else
            (*suma_zb (razy (neg_infinity, fst b) a) (razy (snd b, infinity) a)*)
            (0.0,0.0)
    else
        if snd b=0.0 then
            (neg_infinity,(1.0/.fst b))
        else
            if fst b=0.0 then
                ((1.0/.snd b),infinity)
            else
                ((1.0/.snd b),(1.0/.fst b))
;;

wartosc_dokladna 1.0;;

wartosc_od_do 0.0 2.0;;

wartosc_dokladnosc 5.0 50.0;;

in_wartosc 0.0 0.0;;

min_wartosc(wartosc_dokladnosc 5.0 50.0);;

max_wartosc(wartosc_dokladnosc 5.0 50.0);;

sr_wartosc(0.5, 3.65);;
let zero = wartosc_dokladna 0.0;;
let jeden = wartosc_dokladna 1.0;;
let duzo = plus jeden (wartosc_od_do 0.0 1.0);;
sr_wartosc duzo;;

let a = wartosc_od_do (-1.) 1.
let b = wartosc_dokladna (-1.) 
let c = podzielic b a                      
let e = wartosc_dokladna 0.                                       
let h = wartosc_dokladnosc (-10.) 50.
let l = plus a b;;
let s = wartosc_dokladnosc (-0.0001) 100.;;
let f = minus a a;;
let j = wartosc_od_do (-6.) 5.;;
let k = razy j j;;
let l = razy zero j;;
let (n:wartosc) = (0.0,infinity);;
let (o:wartosc) = (neg_infinity,0.0);;
let q = plus n o;;

