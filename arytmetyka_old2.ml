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
    if a=neg_infinity || b=neg_infinity || c=neg_infinity || d=neg_infinity then (* sprawdzanie czy kres dolny okreslony*)
        neg_infinity
    else
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
    if a=infinity || b=infinity || c=infinity || d=infinity then (* sprawdzanie czy kres gorny okreslony*)
        infinity
    else
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

let pokaz (a:wartosc) = a;;


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
        if fst x<snd x then
            (fst x+.snd x)/.2.0
        else
            nan
;;


(*---------- MODYFIKATORY ----------*)
let plus (a:wartosc) (b:wartosc) = (* dodawanie *)
    if fst a<=snd a && fst b<=snd b then (* (a,b)+(c,d) *)
        (((fst a+.fst b),(snd a+.snd b)):wartosc)
    else
        if fst a<snd a || fst b<snd b then (* jeden ze zbiorow jest suma dwoch podzbiorow *)
                if (snd a+.snd b)>=(fst a+.fst b) then (* sprawdzenie czy suma tworzy zbiór R *)
                    ((neg_infinity,infinity):wartosc)
                else
                    (((fst a+.fst b),(snd a+.snd b)):wartosc)
        else (* oba zbiory to suma dwoch podzbiorow *)
            ((neg_infinity,infinity):wartosc)
;;

let minus (a:wartosc) (b:wartosc) = (((fst a-.snd b),(snd a-.fst b)):wartosc);;

let razy (a:wartosc) (b:wartosc) = ((min_helper (fst a*.fst b) (fst a*.snd b) (snd a*.fst b) (snd a*.snd b)),(max_helper (fst a*.fst b) (fst a*.snd b) (snd a*.fst b) (snd a*.snd b)):wartosc);;

let podzielic (a:wartosc) (b:wartosc) = 
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
let dc = plus (0.0,1.0) c
let dd = podzielic d d
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

