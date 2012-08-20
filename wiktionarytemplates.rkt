#lang racket/base

(provide get-wiktionary-templates)

(define (get-wiktionary-templates)
  (define templates (make-hash))
  (hash-set! templates "sense" 
             (lambda (args)
               (format "(''~a''):" (car args))
               ))
  
  '(all the templates to look into implementing
        Occurence  Template name
        ---------  -------------
        47227	"defn"
        (example "{{defn|lang=yue|sort=高00}}"
                 is hidden -- probably meta-data)
        
        20907	"hani-forms"
        
        18171	"ja-kanjitab"
        (example "{{ja-kanjitab|高}}"
                 (into "Kanji in this term: 高"))
        
        14479	"cmn-noun"
        
        13011	"ja-noun"
        (example "{{ja-noun|k|hira=たか|rom=taka}}"
                 (into "高 (''hiragana'' ''[[たか]]'', ''romaji'' '''taka''')"))
        
        12599	"han ref"
        (example "{{Han ref|kx=1451.260|dkj=45313|dj=1979.040|hdz=74593.010|uh=9AD8|ud=39640|bh=B0AA|bd=45226}}"
                 (into "=== References ==="
                       "* KangXi: page 1451, character 26"
                       "* Dai Kanwa Jiten: character 45313"
                       "* Dae Jaweon: page 1979, character 4"
                       "* Hanyu Da Zidian: volume 7, page 4593, character 1"
                       "* Unihan data for U+9AD8"))
        
        12598	"han char"
        (example "{{Han char|rn=189|rad=高|as=00|sn=10|four=0022<sub>7</sub>|canj=卜口月口 (YRBR)|ids=⿳[[亠]][[口]][[冋]]}}"
                 (into "高 (''radical 189'' [[高]]+0, 10 ''strokes'', ''cangjie input'' 卜口月口 (YRBR), ''four-corner'' 0022<sub>7</sub>, ''composition'' ⿳[[亠]][[口]][[冋]])"))
        
        12254	"cmn-hanzi"
        (example "{{cmn-hanzi|pin=[[gāo]] ([[gao1]])|wg=kao<sup>1</sup>}}"
                 (into "高 (pinyin [[gāo]] ([[gao1]]), ''Wade-Giles'' kao<sup>1</sup>)"))
        
        12252	"ja-readings"
        (example "{{ja-readings|on=[[こう]] (kō)|kun=[[たかい]] (takai), [[たかまる]] (takamaru), [[たかさ]] (takasa)}}"
                 (into "* '''On:''' [[こう]] (kō)"
                       "* '''Kun:''' [[たかい]] (takai), [[たかまる]] (takamaru), [[たかさ]] (takasa)"))
        
        12251	"ja-kanji"
        (example "{{ja-kanji|grade=2|rs=高00}}"
                 (into "高 (''grade 2 “Kyōiku” kanji'')"))
        
        12151	"l"
        10008	"reference-book"
        8924	"cite web"
        
        8802	"yue-hanzi"
        (examples "{{yue-hanzi|jyut=|y=gou1}}"
                  (into "高 (''Yale'' gou1)"))
        
        8176	"ko-hanja"
        (example "{{ko-hanja|hangeul=고|eumhun=높을|rv=go|mr=ko|y=ko}}"
                 (into "高"
                       "'''Eumhun:'''"
                       "'''Sound''' (''hangeul''): [[고]] (''revised:'' go, ''McCune-Reischauer:'' ko, ''Yale:'' ko)"
                       "'''Name''' (''hangeul''): [[높을]]()"))
        
        7797	"zh-hanzi"
        
        6843	"ja-pos"
        (example "{{ja-pos|k|particle|rom=dake|hira=だけ|hidx=たけ'}}"
                 (into "丈 (''hiragana'' [[だけ]], ''romaji'' '''dake''')"))
        
        6418	"ipa"
        6364	"cmn-verb"
        5104	"also"
        (example "{{also|嶽}}"
                 (into ":See also [[嶽]]"))
        
        3701	"term"
        (example "{{term|高|taka|height}}"
                 (into "''[[高|taka]]'' (“height”)"))
        
        3666	"given name"
        (example "{{ja-kanji|grade=c|rs=一02}}"
                 (into "丈 (''hiragana'' [[じょう]], ''romaji'' '''Jō''')"
                       "# A male given name"))
        
        3288	"audio"
        (cant have this)
        
        3139	"vi-hantu"
        (example "{{vi-hantu|[[cao]], [[sào]]|rs=高00}}"
                 (into "高 ([[cao]], [[sào]])"))
        
        3036	"literary"
        3034	"pedialite"
        2976	"cmn-proper noun"
        
        2732	"archaic"
        (example "{{archaic|lang=mul}}"
                 (into "(''archaic'')"))
        
        2371	"cmn-adj"
        1839	"cmn-idiom"
        1721	"advanced mandarin"
        1507	"ja-verb"
        (example "{{ja-verb|kk|type=2|hira=たける|rom=takeru|hidx=たける}}"
                 (into "長ける (''ichidan conjugation'', ''hiragana'' [[たける]], ''romaji'' '''takeru''')"))
        
        1458	"han compound"
        (example "{{Han compound|丘|山}}"
                 (into "[[丘]] + [[山]]"))
        
        1239	"surname"
        1176	"ja-verb-suru"
        1134	"wikipedia"
        1078	"a"
        955	"cmn-adv"
        891	"usex"
        871	"ja-suru"
        
        844	"ja-adj"
        (example "{{ja-adj|kk|decl=i|hira=たかい|rom=takai}}"
                 (into "高い (い-i declension, ''hiragana'' [[たかい]], ''romaji'' '''takai''')"))
        
        825	"qualifier"
        805	"elementary mandarin"
        783	"bottom"
        756	"context"
        738	"stroke order"
        714	"nan-noun"
        681	"intermediate mandarin"
        560	"ja-see-also"
        530	"rfe"
        529	"zh-ts"
        522	"beginning mandarin"
        
        506	"sense"
        
        454	"cmn-proverb"
        432	"ja-na"
        407	"mid3"
        406	"compound"
        404	"etyl"
        370	"suffix"
        368	"slang"
        368	"ja-ichi"
        363	"alternative form of"
        349	"top2"
        341	"jpan"
        332	"mid2"
        317	"head"
        297	"nan-verb"
        296	"ja-forms"
        286	"t"
        278	"ko-noun"
        277	"ko-hanjatab"
        267	"mid4"
        251	"ja-go-ru"
        244	"ja-def"
        243	"hanja form of"
        239	"attention"
        237	"zh-attention"
        220	"ja-i" "declension of X" wont bother
        210	"colloquial"
        203	"top3"
        199	"han simp"
        198	"idiomatic"
        186	"cmn-interj"
        185	"liushu"
        183	"yue-noun"
        175	"py-to-ipa"
        173	"t-"
        173	"ja"
        165	"cmn"
        )
  
  templates
  )