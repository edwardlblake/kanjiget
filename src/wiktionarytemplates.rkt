#lang racket/base

#|

    KanjiGet
    Copyright 2011-2012 Edward L. Blake

    This file is part of KanjiGet.

    KanjiGet is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    KanjiGet is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with KanjiGet.  If not, see <http://www.gnu.org/licenses/>.

|#

(provide get-wiktionary-templates)

(define (get-wiktionary-templates)
  (define templates (make-hash))
  
  (define-syntax wikt-templates-table
    (syntax-rules (* show-as)
      [(wikt-templates-table * num classname (show-as fmt args argx) rst ...)
       (let ()
         (hash-set! templates classname
                     (lambda (args)
                       (format fmt . argx)
                       ))
         (wikt-templates-table rst ...))]
      
      [(wikt-templates-table * num classname _ (show-as fmt args argx) rst ...)
       (let ()
         (hash-set! templates classname
                     (lambda (args)
                       (format fmt . argx)
                       ))
         (wikt-templates-table rst ...))]
      
      [(wikt-templates-table a rst ...)
       (wikt-templates-table rst ...)
       ]
      [(wikt-templates-table)
       (void)
       ]))
    
  (wikt-templates-table all the templates to look into implementing
        Occurence  Template name
        ---------  -------------
        * 47227	"defn"        
        (show-as "" _ ())
        (example "{{defn|lang=yue|sort=高00}}"
                 is hidden -- probably meta-data)
        
        * 20907	"hani-forms"
        TODO
        
        * 18171	"ja-kanjitab"
        TODO
        (example "{{ja-kanjitab|高}}"
                 (into "Kanji in this term: 高"))
        
        * 14479	"cmn-noun"
        TODO
        
        * 13011	"ja-noun"
        TODO
        (example "{{ja-noun|k|hira=たか|rom=taka}}"
                 (into "高 (''hiragana'' ''[[たか]]'', ''romaji'' '''taka''')"))
        
        * 12599	"han ref"
        TODO
        (example "{{Han ref|kx=1451.260|dkj=45313|dj=1979.040|hdz=74593.010|uh=9AD8|ud=39640|bh=B0AA|bd=45226}}"
                 (into "=== References ==="
                       "* KangXi: page 1451, character 26"
                       "* Dai Kanwa Jiten: character 45313"
                       "* Dae Jaweon: page 1979, character 4"
                       "* Hanyu Da Zidian: volume 7, page 4593, character 1"
                       "* Unihan data for U+9AD8"))
        
        * 12598	"han char"
        TODO
        (example "{{Han char|rn=189|rad=高|as=00|sn=10|four=0022<sub>7</sub>|canj=卜口月口 (YRBR)|ids=⿳[[亠]][[口]][[冋]]}}"
                 (into "高 (''radical 189'' [[高]]+0, 10 ''strokes'', ''cangjie input'' 卜口月口 (YRBR), ''four-corner'' 0022<sub>7</sub>, ''composition'' ⿳[[亠]][[口]][[冋]])"))
        
        * 12254	"cmn-hanzi"
        TODO
        (example "{{cmn-hanzi|pin=[[gāo]] ([[gao1]])|wg=kao<sup>1</sup>}}"
                 (into "高 (pinyin [[gāo]] ([[gao1]]), ''Wade-Giles'' kao<sup>1</sup>)"))
        
        * 12252	"ja-readings"
        TODO
        (example "{{ja-readings|on=[[こう]] (kō)|kun=[[たかい]] (takai), [[たかまる]] (takamaru), [[たかさ]] (takasa)}}"
                 (into "* '''On:''' [[こう]] (kō)"
                       "* '''Kun:''' [[たかい]] (takai), [[たかまる]] (takamaru), [[たかさ]] (takasa)"))
        
        * 12251	"ja-kanji"
        TODO
        (example "{{ja-kanji|grade=2|rs=高00}}"
                 (into "高 (''grade 2 “Kyōiku” kanji'')"))
        
        * 12151	"l"
        TODO
        * 10008	"reference-book"
        TODO
        * 8924	"cite web"
        TODO
        
        * 8802	"yue-hanzi"
        TODO
        (examples "{{yue-hanzi|jyut=|y=gou1}}"
                  (into "高 (''Yale'' gou1)"))
        
        * 8176	"ko-hanja"
        TODO
        (example "{{ko-hanja|hangeul=고|eumhun=높을|rv=go|mr=ko|y=ko}}"
                 (into "高"
                       "'''Eumhun:'''"
                       "'''Sound''' (''hangeul''): [[고]] (''revised:'' go, ''McCune-Reischauer:'' ko, ''Yale:'' ko)"
                       "'''Name''' (''hangeul''): [[높을]]()"))
        
        * 7797	"zh-hanzi"
        TODO
        
        * 6843	"ja-pos"
        TODO
        (example "{{ja-pos|k|particle|rom=dake|hira=だけ|hidx=たけ'}}"
                 (into "丈 (''hiragana'' [[だけ]], ''romaji'' '''dake''')"))
        
        * 6418	"ipa"
        TODO
        * 6364	"cmn-verb"
        TODO
        * 5104	"also"
        TODO
        (example "{{also|嶽}}"
                 (into ":See also [[嶽]]"))
        
        * 3701	"term"
        TODO
        (example "{{term|高|taka|height}}"
                 (into "''[[高|taka]]'' (“height”)"))
        
        * 3666	"given name"
        TODO
        (example "{{ja-kanji|grade=c|rs=一02}}"
                 (into "丈 (''hiragana'' [[じょう]], ''romaji'' '''Jō''')"
                       "# A male given name"))
        
        * 3288	"audio"
        (show-as "" _ ())
        
        * 3139	"vi-hantu"
        TODO
        (example "{{vi-hantu|[[cao]], [[sào]]|rs=高00}}"
                 (into "高 ([[cao]], [[sào]])"))
        
        * 3036	"literary"
        TODO
        * 3034	"pedialite"
        TODO
        * 2976	"cmn-proper noun"
        TODO
        
        * 2732	"archaic"
        TODO
        (example "{{archaic|lang=mul}}"
                 (into "(''archaic'')"))
        
        * 2371	"cmn-adj"
        TODO
        * 1839	"cmn-idiom"
        TODO
        * 1721	"advanced mandarin"
        TODO
        * 1507	"ja-verb"
        TODO
        (example "{{ja-verb|kk|type=2|hira=たける|rom=takeru|hidx=たける}}"
                 (into "長ける (''ichidan conjugation'', ''hiragana'' [[たける]], ''romaji'' '''takeru''')"))
        
        * 1458	"han compound"
        TODO
        (example "{{Han compound|丘|山}}"
                 (into "[[丘]] + [[山]]"))
        
        * 1239	"surname"
        TODO
        * 1176	"ja-verb-suru"
        TODO
        * 1134	"wikipedia"
        TODO
        * 1078	"a"
        TODO
        * 955	"cmn-adv"
        TODO
        * 891	"usex"
        TODO
        * 871	"ja-suru"
        TODO
        
        * 844	"ja-adj"
        TODO
        (example "{{ja-adj|kk|decl=i|hira=たかい|rom=takai}}"
                 (into "高い (い-i declension, ''hiragana'' [[たかい]], ''romaji'' '''takai''')"))
        
        * 825	"qualifier"
        TODO
        * 805	"elementary mandarin"
        TODO
        * 783	"bottom"
        TODO
        * 756	"context"
        TODO
        * 738	"stroke order"
        TODO
        * 714	"nan-noun"
        TODO
        * 681	"intermediate mandarin"
        TODO
        * 560	"ja-see-also"
        TODO
        * 530	"rfe"
        TODO
        * 529	"zh-ts"
        TODO
        * 522	"beginning mandarin"
        TODO
        
        * 506	"sense"
        (show-as "(''~a''):" lst ((car lst)))
        
        * 454	"cmn-proverb"
        * 432	"ja-na"
        * 407	"mid3"
        * 406	"compound"
        * 404	"etyl"
        * 370	"suffix"
        * 368	"slang"
        * 368	"ja-ichi"
        * 363	"alternative form of"
        * 349	"top2"
        * 341	"jpan"
        * 332	"mid2"
        * 317	"head"
        * 297	"nan-verb"
        * 296	"ja-forms"
        * 286	"t"
        * 278	"ko-noun"
        * 277	"ko-hanjatab"
        * 267	"mid4"
        * 251	"ja-go-ru"
        * 244	"ja-def"
        * 243	"hanja form of"
        * 239	"attention"
        * 237	"zh-attention"
        
        * 220	"ja-i"
        (show-as "''Declension of ~s (TODO)''" lst ((car lst)))
        
        * 210	"colloquial"
        * 203	"top3"
        * 199	"han simp"
        * 198	"idiomatic"
        * 186	"cmn-interj"
        * 185	"liushu"
        * 183	"yue-noun"
        * 175	"py-to-ipa"
        * 173	"t-"
        * 173	"ja"
        * 165	"cmn"
        )
  
  templates
  )
