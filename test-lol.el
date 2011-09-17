(require 'ert)
(require 'ert-extras)
(require 'lol)

(ert-deftest mkstr-creates-string-from-anything ()
  (are
   (string= "" (mkstr))
   (string= "beautifulsoup" (mkstr 'beau "tiful" 'soup))
   (string= "23" (mkstr 2 (1- 4)))
   (string= "nilpotent" (mkstr nil 'potent))
   (string= "staynil" (mkstr 'stay nil))
   (string= "vanilla" (mkstr 'va nil 'la))))

(ert-deftest symb-creates-symbols ()
  (are
   (null (symb nil))
   (eq 'nilpotent (symb nil 'potent))
   (eq 'test23 (symb 'test (+ 17 (* 2 3))))))

(ert-deftest flatten-flattens ()
  (are
   (equal nil (flatten '()))
   (equal '(1 2 3 4 5 6) (flatten '(((1) 2 3 () 4 ((5 (6)))))))))

(ert-deftest can-identify-g!-symbols ()
  "A g! symbol must start with \"g!\", and have at least one
more character."
  (are
   (g!-symbol-p 'g!foo)
   (not (g!-symbol-p 'g!))
   (not (g!-symbol-p 'o!dear))
   (not (g!-symbol-p 'og!gy))))

(ert-deftest can-identify-o!-symbols ()
  "An o! symbol must start with \"o!\", and have at least one
more character."
  (are
   (o!-symbol-p 'o!foo)
   (not (o!-symbol-p 'o!))
   (not (o!-symbol-p 'g!whiz))
   (not (o!-symbol-p 'go!fish))))

(ert-deftest can-convert-o!-to-g! ()
  (are
   (eq 'g!foo (o!-symbol-to-g!-symbol 'o!foo))))
