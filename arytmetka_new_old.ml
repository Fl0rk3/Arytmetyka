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

(* sumowanie zbiorów *)
let suma_zb (a:wartosc) (b:wartosc) =
    if (fst a=neg_infinity && snd a=infinity) || (fst b=neg_infinity && snd b=infinity) then
        ((neg_infinity,infinity):wartosc)
    else
    if (fst a=neg_infinity && fst b=neg_infinity) then (* oba zrbiory to (-inf,a) *)
        ((neg_infinity,(max (snd a) (snd b) )):wartosc)
    else 
    if (snd a=infinity && snd b=infinity) then (* oba zrbiory to (a,+inf) *)
        (((min (fst a) (fst b) ),infinity):wartosc)
    else (* zbiory (-inf,a) i (b,inf) *)
    if fst a=neg_infinity then
        if snd a>= fst b then
            ((neg_infinity,infinity):wartosc)
        else
            ((fst b, snd a):wartosc)
    else (* zbiory (-inf,b) i (a,inf) *)
        if snd b>=fst a then
            ((neg_infinity,infinity):wartosc)
        else
            ((fst a, snd b):wartosc)
;;


(*---------- KONSTRUKTORY ----------*)
let wartosc_dokladna (x:float) = ((x,x):wartosc);;

let wartosc_od_do (x:float) (y:float) = ((x,y):wartosc);;

let wartosc_dokladnosc (x:float) (p:float) = (((x-.(wartosc_bez x*.(p/.100.0))),(x+.(wartosc_bez x*.(p/.100.0)))):wartosc);;


(*---------- SELEKTORY ----------*)
let in_wartosc (x:wartosc) (y:float) = 
    if fst x<=snd x then (* zbiór zbieżny *)
        if fst x>=y && snd x<=y then
            true
        else
            false
    else (* zbiór rozbieżny *)
        if y<=snd x || y>=fst x then
            true
        else
            false
;;

let min_wartosc (x:wartosc) = 
    if is_nan (fst x) then
        nan
    else
    if fst x<=snd x then (* zbiór zbieżny *)
        fst x
    else (* zbiór rozbieżny *)
        neg_infinity
;;

let max_wartosc (x:wartosc) = 
    if is_nan (fst x) then
        nan
    else
    if fst x<=snd x then (* zbiór zbieżny *)
        snd x
    else (* zbiór rozbieżny *)
        infinity
;;

let sr_wartosc (x:wartosc) = 
    if is_nan (fst x) then
        nan
    else
    if fst x<=snd x then (* zbiór zbieżny *)
        if fst x=neg_infinity && snd x=infinity then
            nan
        else
            (fst x+.snd x)/.2.0 
    else (* zbiór rozbieżny *)
        nan
;;


(*---------- MODYFIKATORY ----------*)
let plus (a:wartosc) (b:wartosc) = 
    if (is_nan (fst a) || is_nan (fst b)) then (* gdy któryś to nan *)
        ((nan,nan):wartosc)
    else
    if fst a<=snd a &&  fst b<=snd b then (* zbiór zbieżny + zbiór zbieżny*)
        (((fst a+.fstb),(snd a+.snd b)):wartosc)
    else if fst a>snd a &&  fst b>snd b then (* zbiór rozbieżny + zbiór rozbieżny*)
        ((neg_infinity,infinity):wartosc)
    else (* zbiór zbieżny + zbiór rozbieżny*)
        if (fst a+.fst b<=snd a+.snd b) then (* czy ciągi się łączą *)
            ((neg_infinity,infinity):wartosc)
        else
            (((fst a+.fst b),(snd a+.snd b)):wartosc)
;;

let minus (a:wartosc) (b:wartosc) = 
    if (is_nan (fst a) || is_nan (fst b)) then (* gdy któryś to nan *)
        ((nan,nan):wartosc)
    else
    if fst a<=snd a &&  fst b<=snd b then (* zbiór zbieżny - zbiór zbieżny*)
        (((fst a-.snd b),(snd a-.fst b)):wartosc)
    else if fst a>snd a &&  fst b>snd b then (* zbiór rozbieżny - zbiór rozbieżny*)
        ((neg_infinity,infinity):wartosc)
    else (* zbiór zbieżny - zbiór rozbieżny*)
        if (fst a-.snd b<=snd a-.fst b) then (* czy ciągi się łączą *)
            ((neg_infinity,infinity):wartosc)
        else
            (((fst a-.snd b),(snd a-.fst b)):wartosc)
;;

(* funkcja do mnożenia z udziałem zbiorów rozbieżnych *)
let razy_zb (a:wartosc) (b:wartosc) = 
    if fst a>snd a && fst b>snd b then (* zbiór rozbieżny * zbiór rozbieżny *)
        (* (neg_inf, snd a)u(fst a, inf) *)
        (* (neg_inf, snd b)u(fst b, inf) *)
        (* (neg_inf, snd a)*(neg_inf, snd b)u(neg_inf, snd a)*(fst b, inf) *)
        (* (fst a, inf)*(neg_inf, snd b)u(fst a, inf)*(fst b, inf) *)
        ((suma_zb (suma_zb (razy (neg_infinity,snd a) (neg_infinity,snd b) ) (razy (neg_infinity,snd a) (fst b, infinity) ) ) (suma_zb (razy (fst a,infinity) (neg_infinity,snd b) ) (razy (fst a, infinity) (fst b, infinity) ) ) ):wartosc)

    else
        (* (neg_inf, snd a)u(fst a, inf) *)
        (* (fst b, snd b) *)
        (* (neg_inf, snd a)*b u(fst a, inf)*b *)
        ((suma_zb (razy (neg_infinity,snd a) b) (razy (fst a,infinity) b)):wartosc)
;;

let razy (a:wartosc) (b:wartosc) =
    if (is_nan (fst a) || is_nan (fst b)) then (* gdy któryś to nan *)
        ((nan,nan):wartosc)
    else
    if (fst a = 0.0 && snd a = 0.0) || (fst b = 0.0 && snd b = 0.0) then (* sprawdzenie, czy któryś ze zbiorów to zbiór (0.0,0.0) *)
                ((0.0,0.0):wartosc)
    else
    if fst a<=snd a &&  fst b<=snd b then (* zbiór zbieżny * zbiór zbieżny*)
        (((dodatnie_zero (min_helper (fst a*.fst b) (fst a*.snd b) (snd a*.fst b) (snd a*.snd b))),(dodatnie_zero (max_helper (fst a*.fst b) (fst a*.snd b) (snd a*.fst b) (snd a*.snd b))):wartosc))
    else if fst a>snd a &&  fst b>snd b then (* zbiór rozbieżny * zbiór rozbieżny *)
        ((razy_zb a b):wartosc)
    else (* zbiór zbieżny * zbiór rozbieżny*)
        if(fst a>snd a) then (* a - rozbieżny *)
            ((razy_zb a b):wartosc)
        else (* b - rozbieżny *) 
            ((razy_zb b a):wartosc)
;;

let podzielic (a:wartosc) (b:wartosc) =
    if (is_nan (fst a) || is_nan (fst b)) then (* gdy któryś to nan *)
        ((nan,nan):wartosc)
    else
    if fst a=0.0 && snd a=0.0 then (* dzielenie 0 przez dowolną wartość *)
        ((0.0,0.0):wartosc)
    else
    if fst b=0.0 && snd b=0.0 then (* dzielenie przez 0 *)
        ((nan,nan):wartosc)
    else
    if (fst a=neg_infinity && snd a=infinity) || (fst b=neg_infinity && snd b=infinity) then (* licznik lub mianownik to (-inf,inf) *)
        ((neg_infinity,infinity):wartosc)
    else
    if (fst a<=snd a &&  fst b<=snd b) || (fst a>snd a && fst b<=snd b) then (* zbiór zbieżny / zbiór zbieżny lub zbiór rozbieżny / zbiór zbieżny*)
        if fst b=0.0 then (* dzielenie przez (0,b)*)
            razy a ((1.0/.snd b),infinity)
        else 
        if snd b=0.0 (* dzielenie przez (b,0) *)
            razy a (neg_infinity,(1.0/.fst b))
        else 
        if fst b<=0 && snd b>=0 then (* dzielenie przez 0\in(a,b) *)
            razy a ((1.0/.snd b),(1.0/.fst b))
        else (* dzielenie przez 0\notin(a,b) *)
            razy a ((1.0/.snd b),(1.0/.fst b)) 
    else (* zbiór rozbieżny / zbiór rozbieżny lub zbiór zbieżny / zbiór rozbieżny*)
    if snd b=0.0 then (* sprwadzenie czy w podstawie mamy (-inf,0) *) then
        razy a (neg_infinity,(1.0/.fst a))
    else
        razy a ((1.0/.snd b),(1.0/.fst a))
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

