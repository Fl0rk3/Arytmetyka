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

(* funkcja łączenia dwóch zbiorów*)
let suma_zb (a:wartosc) (b:wartosc) = 
    if (fst a=neg_infinity && snd a=infinity) || (fst b=neg_infinity && snd b=infinity) then
        ((neg_infinity,infinity):wartosc)
    else (* !!!!!!!!!!!! zrobić łączenia zbiorów *)
        b
;;


(*---------- KONSTRUKTORY ----------*)
let wartosc_dokladna (x:float) = (((dodatnie_zero x),(dodatnie_zero x)):wartosc);;

let wartosc_od_do (x:float) (y:float) = (((dodatnie_zero x),(dodatnie_zero y)):wartosc);;

let wartosc_dokladnosc (x:float) (p:float) = (((x-.(wartosc_bez x*.(p/.100.0))),(x+.(wartosc_bez x*.(p/.100.0)))):wartosc);;


(*---------- SELEKTORY ----------*)
let in_wartosc (x:wartosc) (y:float) = 
    if fst x<=snd x then
        if fst x<=y && snd x>=y then
            true
        else
            false
    else
        if y<=snd x || y>=fst x then
            true
        else
            false
;;    

let min_wartosc (x:wartosc) = 
    if is_nan (fst x) then
        nan
    else
    if fst x<=snd x then
        fst x
    else
        neg_infinity
;;

let max_wartosc (x:wartosc) = 
    if is_nan (fst x) then
        nan
    else
    if fst x<=snd x then
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
        if (fst a-.snd b)<=(snd a-.fst b) then (* sprawdzenie czy różnica tworzy zbiór R *)
            ((neg_infinity,infinity):wartosc)
        else
            (((fst a-.snd b),(snd a-.fst b)):wartosc)
;;

(* POMOCNICZA funkcja dla razy - przedział zbieżny(b) i rozbieżny(a) *)
let razy_zr (a:wartosc) (b:wartosc) =
    if fst b=neg_infinity || snd b=infinity then
        if fst b=neg_infinity then
            (((dodatnie_zero(max (fst a*.fst b) (fst a*.snd b))),(dodatnie_zero(min (snd a*.fst b) (snd a*.snd b)))):wartosc)
        else
            (((dodatnie_zero(min (fst a*.fst b) (fst a*.snd b))),(dodatnie_zero(max (snd a*.fst b) (snd a*.snd b)))):wartosc)
    else
    if fst b=snd b && fst b<0.0 then (* mnożenie przez ujemną wartość *)
        ((dodatnie_zero(min (fst a*.fst b) (snd a*.fst b)), dodatnie_zero(max (fst a*.fst b) (snd a*.fst b))):wartosc)
    else
    if (dodatnie_zero(min (fst a*.fst b) (fst a*.snd b)))<=(dodatnie_zero(max (snd a*.fst b) (snd a*.snd b))) then (* sprawdzenie czy wynikiem jest zbiór R *)
        (((neg_infinity,infinity):wartosc))
    else
        (((dodatnie_zero(min (fst a*.fst b) (fst a*.snd b))),(dodatnie_zero(max (snd a*.fst b) (snd a*.snd b)))):wartosc)
;;

let razy (a:wartosc) (b:wartosc) = (* mnożenie *)
    if (is_nan (fst a) || is_nan (fst b)) then
        ((nan,nan):wartosc)
    else
    if (fst a = 0.0 && snd a = 0.0) || (fst b = 0.0 && snd b = 0.0) then (* sprawdzenie, czy któryś ze zbiorów to zbiór (0.0,0.0) *)
                ((0.0,0.0):wartosc)
    else
    (*if fst a=snd a then (* (a,a)*(b,c) *)
        ((dodatnie_zero(min (fst a*.fst b) (fst a*.snd b)), dodatnie_zero(max (fst a*.fst b) (fst a*.snd b))):wartosc)
    else
    if fst b=snd b then (* (a,b)*(c,c) *)
        ((dodatnie_zero(min (fst a*.fst b) (snd a*.fst b)), dodatnie_zero(max (fst a*.fst b) (snd a*.fst b))):wartosc)
    else*)
    if fst a<=snd a && fst b<=snd b then (* (a,b)*(c,d) *)
        if (fst a = neg_infinity && snd a = infinity) || (fst b = neg_infinity && snd b = infinity) then (* sprawdzenie, czy któryś ze zbiorów to zbiór R *)
                ((neg_infinity,infinity):wartosc)
        else
            (((dodatnie_zero (min_helper (fst a*.fst b) (fst a*.snd b) (snd a*.fst b) (snd a*.snd b))),(dodatnie_zero(max_helper (fst a*.fst b) (fst a*.snd b) (snd a*.fst b) (snd a*.snd b)))):wartosc)
    else
        if fst a>snd a && fst b>snd b then (* oba zbiory to suma dwoch podzbiorow *)
            suma_zb (razy_zr b (neg_infinity,snd a)) (razy_zr b (fst a, infinity))
        else (* jeden ze zbiorow jest suma dwoch podzbiorow *)
        if (fst a = neg_infinity && snd a = infinity) || (fst b = neg_infinity && snd b = infinity) then (* sprawdzenie, czy któryś ze zbiorów to zbiór R *)
            ((neg_infinity,infinity):wartosc)
        else
            if(fst a>snd a) then (* sprawdzenie, który ciąg jest rozbieżny *)
                (razy_zr a b)
            else
                razy_zr b a
            
;;

let podzielic (a:wartosc) (b:wartosc) = (* dzielenie *)
    if (is_nan (fst a) || is_nan (fst b)) then
        ((nan,nan):wartosc)
    else
    if (fst b = 0.0 && snd b = 0.0) then (* sprawdzenie, czy dzielimy przez zero (0.0,0.0) *)
        ((nan,nan):wartosc)
    else
    if (fst a = 0.0 && snd a = 0.0) then (* sprawdzenie, czy w liczebniku jest zero (0.0,0.0) *)
        ((0.0,0.0):wartosc)
    else
    if (fst a = neg_infinity && snd a = infinity) then (* dzielenie nieskończoności przez zbiór =! (0.0,0.0) *)
        ((neg_infinity, infinity):wartosc)
    else
    if (fst b = neg_infinity && snd b = infinity) then (* dzielenie zbioru =! (0.0,0.0) przez nieskończoność *)
        ((neg_infinity, infinity):wartosc)
    else
    if fst b<0.0 && snd b>0.0 then (* 0 in [b1,b2] *)
        if fst a>snd a && fst b>snd b then (* oba zbiory to suma dwoch podzbiorow *)
            ((neg_infinity,infinity):wartosc)(*?*)
        else
        if fst a>snd a then (* dzielimy sumę dwóch podzbiorów przez zbiór*)
            razy a ((1.0/.snd b),(1.0/.fst b))
        else
        if snd (razy (neg_infinity, (1.0/.fst b)) a) = fst (razy ((1.0/.snd b), infinity) a) then
            ((neg_infinity,infinity):wartosc)
        else
        if fst (razy (neg_infinity, (1.0/.fst b)) a)>fst (razy ((1.0/.snd b), infinity) a) then
            (((fst (razy (neg_infinity, (1.0/.fst b)) a)),(snd (razy ((1.0/.snd b), infinity) a))):wartosc) (*1/y*)
        else
            (((fst (razy ((1.0/.snd b), infinity) a)),(snd (razy (neg_infinity, (1.0/.fst b)) a))):wartosc) (*1/y*)
    else
    if snd b=0.0 then (* [b1,0] *)
        razy a (neg_infinity,(1.0/.fst b))
    else
    if fst b=0.0 then (* [0,b2] *)
        razy a ((1.0/.snd b),infinity)
    else 
    if fst a>snd a && fst b>snd b then (* oba zbiory to suma dwoch podzbiorow *)
            ((neg_infinity,infinity):wartosc)(*?*)
    else(* [b1,b2] *)
        razy a ((1.0/.snd b),(1.0/.fst b))
;;




let a = ( podzielic ( 
    podzielic ( wartosc_dokladna (-1.200000) ) ( 
        minus ( wartosc_dokladnosc (0.800000) (0.000000) ) ( 
            plus ( 
                minus ( wartosc_od_do (-8.200000) (-3.000000) ) ( wartosc_od_do (0.000000) (5.800000) ) ) ( 
                    minus ( 
                        podzielic ( wartosc_dokladnosc (7.600000) (2.800000) ) ( wartosc_dokladna (7.400000) ) ) ( wartosc_od_do (-5.200000) (7.000000) ) ) ) ) ) ( wartosc_dokladnosc (-0.600000) (0.000000) ) ) ;;
let g = podzielic (0.488642366613840418, -0.0576875103941460049) ( wartosc_dokladnosc (-0.600000) (0.000000) )
