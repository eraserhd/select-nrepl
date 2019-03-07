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
        "  hello/|_world"  "  _hello/world|"
        " :foo/ba|_r"      " _:foo/bar|"
        " \\new|_line"     " _\\newline|"
        " \"a s|t_ring\" " " _\"a string\"| "
        " 4200|_0  x"      " _42000|  x"
        "++ #\"|_x\" ;;-"  "++ _#\"x\"| ;;-"
        " #fo|_o \"h\" "   " _#foo \"h\"| "
        " \"h|_ello\nw\" " " _\"hello\nw\"| "
        "a ^S foo|_b y"    "a _^S foob| y"
        "a #^S foo|_b y"   "a _#^S foob| y"

        "( he|_llo there)" "( _hello| there)"
        "h |_"             "h "
        "|_ h ello"        " _h| ello"
        "h |_ (ello t)"    "h  (_ello| t)"
        "h |_ ([ello t])"  "h  ([_ello| t])"))

    (fact "when a whole element is already selected, it will select the next one"
      (tabular
        (select "whole" "element" ?input) => ?output
        ?input             ?output
        "|foo_ -42 :baz"   "foo _-42| :baz"
        "foo |-42_ :baz"   "foo -42 _:baz|"

        "|foo_ (-42) :baz" "foo (_-42|) :baz"
        "foo (|-42_) :baz" "foo (-42) _:baz|"

        "|foo_ (-42 :baz)" "foo (_-42| :baz)"
        "foo (|-42_ :baz)" "foo (-42 _:baz|)"))
         
    (fact "when a count is given, it acts as repeated selection"
       (tabular
        (select "whole" "element" ?input {:count ?c}) => ?output
        ?input             ?c ?output
        "f|_oo -42 :baz"   0  "_foo| -42 :baz"
        "f|_oo -42 :baz"   1  "_foo| -42 :baz"
        "f|_oo -42 :baz"   2  "foo _-42| :baz"
        "f|_oo -42 :baz"   3  "foo -42 _:baz|"
        "f|_oo -42 :baz"   4  "foo -42 :baz"))

    (fact "when the last whole element is already selected, it will select nothing"
      (select "whole" "element" "foo (-42 |:baz_)") => "foo (-42 :baz)"
      (select "whole" "element" "foo -42 |:baz_") => "foo -42 :baz"))

  (facts "about selecting the inside of an element"
    (tabular
      (select "inside" "element" ?input) => ?output
      ?input ?output
      " \"a s|t_ring\" "  " \"_a string|\" "
      " #\"a |_regex\" x" " #\"_a regex|\" x"
      " #fo|_o \"hi\" "   " #foo \"_hi|\" "
      " \"he\nt|_here \"" " \"_he\nthere |\""))

  (facts "about selecting to the end of an element"
    (fact "it selects from the cursor when replacing"
      (select "whole" "element" "x :fo|o_bar" {:direction "to_end"}) => "x :fo_obar|")
    (fact "it selects from the anchor when extending"
      (select "whole" "element" "x :fo|o_bar" {:combine "extend", :direction "to_end"}) => "x :foo_bar|"
      (select "whole" "element" "x :fo_o|bar" {:combine "extend", :direction "to_end"}) => "x :fo_obar|")
    (fact "it selects to the end of successive elements"
      (tabular
        (select "whole" "element" ?input {:direction "to_end"}) => ?output
        ?input          ?output
        "_a|bc def ghi" "a_bc| def ghi"
        "a_bc| def ghi" "abc_ def| ghi"
        "abc_ def| ghi" "abc def_ ghi|")))
  (facts "about selecting to the beginning of an element"
    (fact "it selects from the cursor when replacing"
      (select "whole" "element" "x :fo|o_bar" {:direction "to_begin"}) => "x |:fo_obar")
    (fact "it selects from the anchor when extending"
      (select "whole" "element" "x :fo|o_bar" {:combine "extend", :direction "to_begin"}) => "x |:foo_bar"
      (select "whole" "element" "x :fo_o|bar" {:combine "extend", :direction "to_begin"}) => "x |:fo_obar")
    (fact "it selects to the end of successive elements"
      (tabular
        (select "whole" "element" ?input {:direction "to_begin"}) => ?output
        ?input          ?output
        "abc def g_hi|" "abc def |ghi_"
        "abc def |ghi_" "abc |def _ghi"))))

(facts "about selecting forms"
  (facts "about selecting whole forms"
    (fact "when the cursor is inside the form, it will expand the selection to the whole form"
      (tabular
        (select "whole" "form" ?input) => ?output
        ?input                ?output
        "x (he|_l wo) 4"      "x _(hel wo)| 4"
        "x [he|_l wo] 4"      "x _[hel wo]| 4"
        "x {he|_l wo} 4"      "x _{hel wo}| 4"
        "x #{he|_l wo} 4"     "x _#{hel wo}| 4"
        "x #:foo{:b|_ar 4} 4" "x _#:foo{:bar 4}| 4"
        "x `(foo ~b|_ar) 4"   "x _`(foo ~bar)| 4"
        "x ~(foo ~b|_ar) 4"   "x _~(foo ~bar)| 4"
        "x ~@(foo ~b|_ar) 4"  "x _~@(foo ~bar)| 4"
        "x ^S {he|_l wo} 4"   "x _^S {hel wo}| 4"
        "x #^S {he|_l wo} 4"  "x _#^S {hel wo}| 4"
        "x '(he|_l wo) 4"     "x _'(hel wo)| 4"
        "x #f/b (he|_l wo) 4" "x _#f/b (hel wo)| 4"

        "x (he (ll|_o wo) l)" "x (he _(llo wo)| l)"
        "x (h|_e (llo wo) l)" "x _(he (llo wo) l)|"))

    (fact "when a whole form is already selected, it selects the next-wider form"
      (tabular
        (select "whole" "form" ?input) => ?output
        ?input                ?output
        "( (he |(wo)_ th) x)" "( _(he (wo) th)| x)"
        "( |(he (wo) th)_ x)" "_( (he (wo) th) x)|"))
    (fact "when a top-level form is already selected, it selects the following form"
      (select "whole" "form" "  |(hello () world)_ ()") => "  (hello () world) _()|"))

  (facts "about selecting inside forms"
    (tabular
      (select "inside" "form" ?input) => ?output
      ?input                ?output
      "x (he|_l wo) 4"      "x (_hel wo|) 4"
      "x [he|_l wo] 4"      "x [_hel wo|] 4"
      "x {he|_l wo} 4"      "x {_hel wo|} 4"
      "x #{he|_l wo} 4"     "x #{_hel wo|} 4"
      "x #:foo{:b|_ar 4} 4" "x #:foo{_:bar 4|} 4"
      "x `(foo ~b|_ar) 4"   "x `(_foo ~bar|) 4"
      "x ~(foo ~b|_ar) 4"   "x ~(_foo ~bar|) 4"
      "x ~@(foo ~b|_ar) 4"  "x ~@(_foo ~bar|) 4"

      "x (he (ll|_o wo) l)" "x (he (_llo wo|) l)"
      "x (h|_e (llo wo) l)" "x (_he (llo wo) l|)")))

(facts "about selecting whole toplevel forms"
  (fact "when inside a top-level, that top-level is selected"
    (tabular
      (select "whole" "toplevel" ?input) => ?output
      ?input                ?output
      "x (he (ll|_o wo) l)" "x _(he (llo wo) l)|"
      "x (h|_e (llo wo) l)" "x _(he (llo wo) l)|"))
  (fact "when not inside a top-level, the following top-level is selected"
    (select "whole" "toplevel" "x|_ (he (llo wo) l)") => "x _(he (llo wo) l)|")
  (fact "when a top-level is already selected, the following is selected"
    (select "whole" "toplevel" " |(foo () b)_ (bar) x") => " (foo () b) _(bar)| x"))
