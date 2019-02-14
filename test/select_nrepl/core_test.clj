(ns select-nrepl.core-test
  (:require
   [midje.sweet :refer :all]
   [select-nrepl.test-helpers :refer :all]))

(facts "about selecting elements"
  (facts "about selecting whole elements"
    (fact "when the cursor is inside the element, it will expand the selection to the whole element"
      (tabular
        (select "whole" "element" ?input) => ?output
        ?input             ?output
        "  hello/|_world"  "  |hello/world_"
        " :foo/ba|_r"      " |:foo/bar_"
        " \\new|_line"     " |\\newline_"
        " \"a s|t_ring\" " " |\"a string\"_ "
        " 4200|_0  x"      " |42000_  x"
        "++ #\"|_x\" ;;-"  "++ |#\"x\"_ ;;-"
        " #fo|_o \"h\" "   " |#foo \"h\"_ "
        " \"h|_ello\nw\" " " |\"hello\nw\"_ "
        "a ^S foo|_b y"    "a |^S foob_ y"
        "a #^S foo|_b y"   "a |#^S foob_ y"

        "( he|_llo there)" "( |hello_ there)"
        "h |_"             "h "
        "|_ h ello"        " |h_ ello"
        "h |_ (ello t)"    "h  (|ello_ t)"
        "h |_ ([ello t])"  "h  ([|ello_ t])"))

    (fact "when a whole element is already selected, it will select the next one"
      (tabular
        (select "whole" "element" ?input) => ?output
        ?input             ?output
        "|foo_ -42 :baz"   "foo |-42_ :baz"
        "foo |-42_ :baz"   "foo -42 |:baz_"

        "|foo_ (-42) :baz" "foo (|-42_) :baz"
        "foo (|-42_) :baz" "foo (-42) |:baz_"

        "|foo_ (-42 :baz)" "foo (|-42_ :baz)"
        "foo (|-42_ :baz)" "foo (-42 |:baz_)"))
         
    (fact "when a count is given, it acts as repeated selection"
       (tabular
        (select "whole" "element" ?input {:count ?c}) => ?output
        ?input             ?c ?output
        "f|_oo -42 :baz"   0  "|foo_ -42 :baz"
        "f|_oo -42 :baz"   1  "|foo_ -42 :baz"
        "f|_oo -42 :baz"   2  "foo |-42_ :baz"
        "f|_oo -42 :baz"   3  "foo -42 |:baz_"
        "f|_oo -42 :baz"   4  "foo -42 :baz"))

    (fact "when the last whole element is already selected, it will select nothing"
      (select "whole" "element" "foo (-42 |:baz_)") => "foo (-42 :baz)"))

  (facts "about selecting the inside of an element"
    (tabular
      (select "inside" "element" ?input) => ?output
      ?input ?output
      " \"a s|t_ring\" "  " \"|a string_\" "
      " #\"a |_regex\" x" " #\"|a regex_\" x"
      " #fo|_o \"hi\" "   " #foo \"|hi_\" "
      " \"he\nt|_here \"" " \"|he\nthere _\"")))

(facts "about selecting forms"
  (facts "about selecting whole forms"
    (fact "when the cursor is inside the form, it will expand the selection to the whole form"
      (tabular
        (select "whole" "form" ?input) => ?output
        ?input                ?output
        "x (he|_l wo) 4"      "x |(hel wo)_ 4"
        "x [he|_l wo] 4"      "x |[hel wo]_ 4"
        "x {he|_l wo} 4"      "x |{hel wo}_ 4"
        "x #{he|_l wo} 4"     "x |#{hel wo}_ 4"
        "x #:foo{:b|_ar 4} 4" "x |#:foo{:bar 4}_ 4"
        "x `(foo ~b|_ar) 4"   "x |`(foo ~bar)_ 4"
        "x ~(foo ~b|_ar) 4"   "x |~(foo ~bar)_ 4"
        "x ~@(foo ~b|_ar) 4"  "x |~@(foo ~bar)_ 4"
        "x ^S {he|_l wo} 4"   "x |^S {hel wo}_ 4"
        "x #^S {he|_l wo} 4"  "x |#^S {hel wo}_ 4"
        "x '(he|_l wo) 4"     "x |'(hel wo)_ 4"
        "x #f/b (he|_l wo) 4" "x |#f/b (hel wo)_ 4"

        "x (he (ll|_o wo) l)" "x (he |(llo wo)_ l)"
        "x (h|_e (llo wo) l)" "x |(he (llo wo) l)_"))

    (fact "when a whole form is already selected, it selects the next-wider form"
      (tabular
        (select "whole" "form" ?input) => ?output
        ?input                ?output
        "( (he |(wo)_ th) x)" "( |(he (wo) th)_ x)"
        "( |(he (wo) th)_ x)" "|( (he (wo) th) x)_"))
    (fact "when a top-level form is already selected, it selects the following form"
      (select "whole" "form" "  |(hello () world)_ ()") => "  (hello () world) |()_"))

  (facts "about selecting inside forms"
    (tabular
      (select "inside" "form" ?input) => ?output
      ?input                ?output
      "x (he|_l wo) 4"      "x (|hel wo_) 4"
      "x [he|_l wo] 4"      "x [|hel wo_] 4"
      "x {he|_l wo} 4"      "x {|hel wo_} 4"
      "x #{he|_l wo} 4"     "x #{|hel wo_} 4"
      "x #:foo{:b|_ar 4} 4" "x #:foo{|:bar 4_} 4"
      "x `(foo ~b|_ar) 4"   "x `(|foo ~bar_) 4"
      "x ~(foo ~b|_ar) 4"   "x ~(|foo ~bar_) 4"
      "x ~@(foo ~b|_ar) 4"  "x ~@(|foo ~bar_) 4"

      "x (he (ll|_o wo) l)" "x (he (|llo wo_) l)"
      "x (h|_e (llo wo) l)" "x (|he (llo wo) l_)")))

(facts "about selecting whole toplevel forms"
  (fact "when inside a top-level, that top-level is selected"
    (tabular
      (select "whole" "toplevel" ?input) => ?output
      ?input                ?output
      "x (he (ll|_o wo) l)" "x |(he (llo wo) l)_"
      "x (h|_e (llo wo) l)" "x |(he (llo wo) l)_"))
  (fact "when not inside a top-level, the following top-level is selected"
    (select "whole" "toplevel" "x|_ (he (llo wo) l)") => "x |(he (llo wo) l)_")
  (fact "when a top-level is already selected, the following is selected"
    (select "whole" "toplevel" " |(foo () b)_ (bar) x") => " (foo () b) |(bar)_ x"))
