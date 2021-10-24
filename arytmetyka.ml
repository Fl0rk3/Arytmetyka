type wartosc = float*float;;

(*---------- POMOCNICZE FUNKCJE ----------*)
(* funkcja do sprawdzania czy zmienna to NaN *)
let is_nan (a:float) = compare a nan=0;;

(* funkcja obliczająca wartość bezwzględną *)
let wartosc_bez (x:float) =
    if x<0.0 then
        x*.(-1.0)
    else
        x
;;

(* funkcja do zamiany nan w celu znalezienia minimum *)
let min_nan (a:float) = 
    if is_nan a then
        infinity
    else
        a
;;

(* funkcja obliczająca minimum spośród 4 liczb *)
let min_helper (a:float) (b:float) (c:float) (d:float) = 
    if a=neg_infinity || b=neg_infinity || c=neg_infinity || d=neg_infinity then (* sprawdzanie czy kres dolny okreslony*)
        neg_infinity
    else
    min (min_nan a) (min (min_nan b) (min (min_nan c) (min_nan d)))
;;

(* funkcja do zamiany nan w celu znalezienia maksimum *)
let max_nan (a:float) = 
    if is_nan a then
        neg_infinity
    else
        a
;;

(* funkcja obliczająca maksimum spośród 4 liczb *)
let max_helper (a:float) (b:float) (c:float) (d:float) = 
    if a=infinity || b=infinity || c=infinity || d=infinity then (* sprawdzanie czy kres gorny okreslony*)
        infinity
    else
    max (max_nan a) (max (max_nan b) (max (max_nan c) (max_nan d)))
;;

(* funkcja do zamiany -0.0 na 0.0 *)
let dodatnie_zero (a:float) =
    if a = -0.0 then
        0.0
    else
        a
;;

(* funkcja łączenia dwóch zbiorów *)
let suma_zb (a:wartosc) (b:wartosc) = 
    if (fst a=neg_infinity && snd a=infinity) || (fst b=neg_infinity && snd b=infinity) then
        ((neg_infinity,infinity):wartosc)
    else (**!!!!!!!!!!!! zrobić łączenia zbiorów *)
        a
;;


(*---------- KONSTRUKTORY ----------*)
let wartosc_dokladna (x:float) = ((x,x):wartosc);;

let wartosc_od_do (x:float) (y:float) = ((x,y):wartosc);;

let wartosc_dokladnosc (x:float) (p:float) = (((x-.(wartosc_bez x*.(p/.100.0))),(x+.(wartosc_bez x*.(p/.100.0)))):wartosc);;


(*---------- SELEKTORY ----------*)
let in_wartosc (x:wartosc) (y:float) = 
    if fst x<snd x then
        if fst x<=y && snd x>=y then
            true
        else
            false
    else
        if y<snd x || y>fst x then
            true
        else
            false
;;    

let min_wartosc (x:wartosc) = 
    if fst x<snd x then
        fst x
    else
        neg_infinity
;;

let max_wartosc (x:wartosc) = 
    if fst x<snd x then
        snd x
    else
        infinity
;;

let sr_wartosc (x:wartosc) = 
    if fst x=neg_infinity && snd x=infinity then
        nan
    else
        if fst x<=snd x then
            (fst x+.snd x)/.2.0
        else
            nan
;;


(*---------- MODYFIKATORY ----------*)
let plus (a:wartosc) (b:wartosc) = (* dodawanie *)
    if fst a<=snd a && fst b<=snd b then (* (a,b)+(c,d) *)
        (((fst a+.fst b),(snd a+.snd b)):wartosc)
    else
        if fst a>snd a && fst b>snd b then (* oba zbiory to suma dwoch podzbiorow *)
            ((neg_infinity,infinity):wartosc)
        else (* jeden ze zbiorow jest suma dwoch podzbiorow *)
        if (snd a+.snd b)>=(fst a+.fst b) then (* sprawdzenie czy suma tworzy zbiór R *)
            ((neg_infinity,infinity):wartosc)
        else
            (((fst a+.fst b),(snd a+.snd b)):wartosc)
;;

let minus (a:wartosc) (b:wartosc) = (* odejmowanie *)
    if fst a<=snd a && fst b<=snd b then (* (a,b)-(c,d) *)
        (((fst a-.snd b),(snd a-.fst b)):wartosc)
    else
        if fst a>snd a && fst b>snd b then (* oba zbiory to suma dwoch podzbiorow *)
            ((neg_infinity,infinity):wartosc)
        else (* jeden ze zbiorow jest suma dwoch podzbiorow *)
        if (fst a-.snd b)>=(snd a-.fst b) then (* sprawdzenie czy różnica tworzy zbiór R *)
            ((neg_infinity,infinity):wartosc)
        else
            (((fst a-.snd b),(snd a-.fst b)):wartosc)
;;

(* POMOCNICZA funkcja dla razy - przedział zbieżny(b) i rozbieżny(a) *)
let razy_zr (a:wartosc) (b:wartosc) =
    if (dodatnie_zero(min (fst a*.fst b) (fst a*.snd b)))<=(dodatnie_zero(max (snd a*.fst b) (snd a*.snd b))) then (* sprawdzenie czy wynikiem jest zbiór R *)
        ((neg_infinity,infinity):wartosc)
    else
        (((dodatnie_zero(min (fst a*.fst b) (fst a*.snd b))),(dodatnie_zero(max (snd a*.fst b) (snd a*.snd b)))):wartosc)
;;

let razy (a:wartosc) (b:wartosc) = (* mnożenie *)
    if (fst a = 0.0 && snd a = 0.0) || (fst b = 0.0 && snd b = 0.0) then (* sprawdzenie, czy któryś ze zbiorów to zbiór (0.0,0.0) *)
                ((0.0,0.0):wartosc)
    else
    if fst a<=snd a && fst b<=snd b then (* (a,b)*(c,d) *)
        if (fst a = neg_infinity && snd a = infinity) || (fst b = neg_infinity && snd b = infinity) then (* sprawdzenie, czy któryś ze zbiorów to zbiór R *)
                ((neg_infinity,infinity):wartosc)
        else
            ((dodatnie_zero (min_helper (fst a*.fst b) (fst a*.snd b) (snd a*.fst b) (snd a*.snd b))),(dodatnie_zero(max_helper (fst a*.fst b) (fst a*.snd b) (snd a*.fst b) (snd a*.snd b))):wartosc)
    else
        if fst a>snd a && fst b>snd b then (* oba zbiory to suma dwoch podzbiorow *)
            suma_zb (razy_zr b (neg_infinity,snd a)) (razy_zr b (fst a, infinity))
        else (* jeden ze zbiorow jest suma dwoch podzbiorow *)
        if (fst a = neg_infinity && snd a = infinity) || (fst b = neg_infinity && snd b = infinity) then (* sprawdzenie, czy któryś ze zbiorów to zbiór R *)
            ((neg_infinity,infinity):wartosc)
        else
            if(fst a>snd a) then (* sprawdzenie, który ciąg jest rozbieżny *)
                razy_zr a b
            else
                razy_zr b a
            
;;

let podzielic (a:wartosc) (b:wartosc) = (* dzielenie *)
    if (fst a = 0.0 && snd a = 0.0) || (fst b = 0.0 && snd b = 0.0) then (* sprawdzenie, czy któryś ze zbiorów to zbiór (0.0,0.0) *)
                ((nan,nan):wartosc)
    else
    if fst b<0.0 && snd b>0.0 then (* 0 in [b1,b2] *)
        if fst (razy (neg_infinity, fst b) a)>fst (razy (snd b, infinity) a) then
            ((fst (razy (neg_infinity, fst b) a)),(snd (razy (snd b, infinity) a))) (*1/y*)
        else
            ((fst (razy (snd b, infinity) a)),(snd (razy (neg_infinity, fst b) a))) (*1/y*)
    else
        if snd b=0.0 then (* [b1,0] *)
            razy a (neg_infinity,(1.0/.fst b))
        else
            if fst b=0.0 then (* [0,b2] *)
                razy a ((1.0/.snd b),infinity)
            else (* [b1,b2] *)
                razy a ((1.0/.snd b),(1.0/.fst b))
;;

let a = wartosc_od_do (-1.) 1.            (* <-1, 1> *)
let b = wartosc_dokladna (-1.)            (* <-1, -1> *)
let c = podzielic b a                     (* (-inf -1> U <1 inf) *)
let d = plus c a                          (* (-inf, inf) *)
let e = wartosc_dokladna 0.               (* <0, 0> *)
let f = razy c e                          (* <0, 0> *)
let g = razy d e                          (* <0, 0> *)
let h = wartosc_dokladnosc (-10.) 50.     (* <-15, -5> *)
let i = podzielic h e                     (* nan, przedzial pusty*)
let j = wartosc_od_do (-6.) 5.            (* <-6, 5> *)
let k = razy j j                          (* <-30, 36> *)
let l = plus a b                          (* <-2, 0> *)
let m = razy b l                          (* <0, 2> *)
let n = podzielic l l                     (* <0, inf) *)
let o = podzielic l m                     (* (-inf, 0) *)
let p = razy o a                          (* (-inf, inf) *)
let q = plus n o                          (* (-inf, inf) *)
let r = minus n n                         (* (-inf, inf) *)
let s = wartosc_dokladnosc (-0.0001) 100. (* <-0.0002, 0> *)
let t = razy n s;;                        (* (-inf, 0) *)


let aa = wartosc_od_do (-1.0) 1.0
let ba = wartosc_dokladna 1.0
let ca = podzielic ba aa
let da = wartosc_dokladna 3.0
let ea = plus ca da      (* (-inf, 2> U <4 inf) *)
let fa = podzielic ba ea (* (-inf, 1/4> U <1/2, inf) *)
let ga = podzielic da aa (* (-inf, -3> U <3, inf) *)
let ha = podzielic ga fa (* (-inf, inf *)
let ia = plus fa ga;;    (* (-inf, inf) *)

let a = wartosc_od_do 1. 4.     (* <1.0, 4.0> *)
let b = wartosc_od_do (-.2.) 3. (* <-2.0, 3.0> *)
let c = podzielic a b           (* (-inf, -1/2> U <1/3, inf) *)
let d = podzielic c b           (* (-inf, -1/6> U <1/9, inf) *)
let e = plus d (wartosc_dokladna 1.)              (* (-inf, 5/6> U <10/9, inf) *)
let f = sr_wartosc (podzielic (wartosc_dokladna 1.)  (wartosc_dokladna 9.));; (* 1/9 *)
let wtf = razy (20.0,infinity) (100.0,-2.0);;

let jed = wartosc_dokladna 1.
let zero = wartosc_dokladna 0.;;
let a = podzielic jed zero;; (* nan *)

let holo = razy (20.0,-500.0) (100.0,-2.0)
let test = razy (2.0,4.0) (7.0,3.0)