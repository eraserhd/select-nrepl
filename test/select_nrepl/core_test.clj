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
        "  hello/|_world"  "  <hello/world>"
        " :foo/ba|_r"      " <:foo/bar>"
        " \\new|_line"     " <\\newline>"
        " \"a s|t_ring\" " " <\"a string\"> "
        " 4200|_0  x"      " <42000>  x"
        "++ #\"|_x\" ;;-"  "++ <#\"x\"> ;;-"
        " #fo|_o \"h\" "   " <#foo \"h\"> "
        " \"h|_ello\nw\" " " <\"hello\nw\"> "
        "a ^S foo|_b y"    "a <^S foob> y"
        "a #^S foo|_b y"   "a <#^S foob> y"

        "( he|_llo there)" "( <hello> there)"
        "h |_"             "h "
        "|_ h ello"        " <h> ello"
        "h |_ (ello t)"    "h  (<ello> t)"
        "h |_ ([ello t])"  "h  ([<ello> t])"))

    (fact "when a whole element is already selected, it will select the next one"
      (tabular
        (select "whole" "element" ?input) => ?output
        ?input             ?output
        "|foo_ -42 :baz"   "foo <-42> :baz"
        "foo |-42_ :baz"   "foo -42 <:baz>"

        "|foo_ (-42) :baz" "foo (<-42>) :baz"
        "foo (|-42_) :baz" "foo (-42) <:baz>"

        "|foo_ (-42 :baz)" "foo (<-42> :baz)"
        "foo (|-42_ :baz)" "foo (-42 <:baz>)"))
         
    (fact "when a count is given, it acts as repeated selection"
       (tabular
        (select "whole" "element" ?input {:count ?c}) => ?output
        ?input             ?c ?output
        "f|_oo -42 :baz"   0  "<foo> -42 :baz"
        "f|_oo -42 :baz"   1  "<foo> -42 :baz"
        "f|_oo -42 :baz"   2  "foo <-42> :baz"
        "f|_oo -42 :baz"   3  "foo -42 <:baz>"
        "f|_oo -42 :baz"   4  "foo -42 :baz"))

    (fact "when the last whole element is already selected, it will select nothing"
      (select "whole" "element" "foo (-42 |:baz_)") => "foo (-42 :baz)"))

  (facts "about selecting the inside of an element"
    (tabular
      (select "inside" "element" ?input) => ?output
      ?input ?output
      " \"a s|t_ring\" "  " \"<a string>\" "
      " #\"a |_regex\" x" " #\"<a regex>\" x"
      " #fo|_o \"hi\" "   " #foo \"<hi>\" "
      " \"he\nt|_here \"" " \"<he\nthere >\"")))

(facts "about selecting forms"
  (facts "about selecting whole forms"
    (fact "when the cursor is inside the form, it will expand the selection to the whole form"
      (tabular
        (select "whole" "form" ?input) => ?output
        ?input                ?output
        "x (he|_l wo) 4"      "x <(hel wo)> 4"
        "x [he|_l wo] 4"      "x <[hel wo]> 4"
        "x {he|_l wo} 4"      "x <{hel wo}> 4"
        "x #{he|_l wo} 4"     "x <#{hel wo}> 4"
        "x #:foo{:b|_ar 4} 4" "x <#:foo{:bar 4}> 4"
        "x `(foo ~b|_ar) 4"   "x <`(foo ~bar)> 4"
        "x ~(foo ~b|_ar) 4"   "x <~(foo ~bar)> 4"
        "x ~@(foo ~b|_ar) 4"  "x <~@(foo ~bar)> 4"
        "x ^S {he|_l wo} 4"   "x <^S {hel wo}> 4"
        "x #^S {he|_l wo} 4"  "x <#^S {hel wo}> 4"
        "x '(he|_l wo) 4"     "x <'(hel wo)> 4"
        "x #f/b (he|_l wo) 4" "x <#f/b (hel wo)> 4"

        "x (he (ll|_o wo) l)" "x (he <(llo wo)> l)"
        "x (h|_e (llo wo) l)" "x <(he (llo wo) l)>"))

    (fact "when a whole form is already selected, it selects the next-wider form"
      (tabular
        (select "whole" "form" ?input) => ?output
        ?input                ?output
        "( (he |(wo)_ th) x)" "( <(he (wo) th)> x)"
        "( |(he (wo) th)_ x)" "<( (he (wo) th) x)>"))
    (fact "when a top-level form is already selected, it selects the following form"
      (select "whole" "form" "  |(hello () world)_ ()") => "  (hello () world) <()>"))

  (facts "about selecting inside forms"
    (tabular
      (select "inside" "form" ?input) => ?output
      ?input                ?output
      "x (he|_l wo) 4"      "x (<hel wo>) 4"
      "x [he|_l wo] 4"      "x [<hel wo>] 4"
      "x {he|_l wo} 4"      "x {<hel wo>} 4"
      "x #{he|_l wo} 4"     "x #{<hel wo>} 4"
      "x #:foo{:b|_ar 4} 4" "x #:foo{<:bar 4>} 4"
      "x `(foo ~b|_ar) 4"   "x `(<foo ~bar>) 4"
      "x ~(foo ~b|_ar) 4"   "x ~(<foo ~bar>) 4"
      "x ~@(foo ~b|_ar) 4"  "x ~@(<foo ~bar>) 4"

      "x (he (ll|_o wo) l)" "x (he (<llo wo>) l)"
      "x (h|_e (llo wo) l)" "x (<he (llo wo) l>)")))

(facts "about selecting whole toplevel forms"
  (fact "when inside a top-level, that top-level is selected"
    (tabular
      (select "whole" "toplevel" ?input) => ?output
      ?input                ?output
      "x (he (ll|_o wo) l)" "x <(he (llo wo) l)>"
      "x (h|_e (llo wo) l)" "x <(he (llo wo) l)>"))
  (fact "when not inside a top-level, the following top-level is selected"
    (select "whole" "toplevel" "x|_ (he (llo wo) l)") => "x <(he (llo wo) l)>")
  (fact "when a top-level is already selected, the following is selected"
    (select "whole" "toplevel" " |(foo () b)_ (bar) x") => " (foo () b) <(bar)> x"))
