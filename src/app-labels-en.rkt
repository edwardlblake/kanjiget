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

(provide STR_WIN_ABOUTDIALOG
         
         STR_DRAW_KANJI_HERE
         STR_BTN_RESTARTOVER
         STR_BTN_SEARCH
         STR_WIN_KANJIFINDER
         STR_LABEL_SELECTRADICALS
         STR_FORMAT_RADICALSFOR
         
         STR_MENU_FILE
         STR_MENU_FILE_HELPSTRING
         STR_MENU_FILE_EXIT
         STR_MENU_FILE_EXIT_HELPSTRING
         STR_MENU_EDIT
         STR_MENU_VIEW
         STR_MENU_HELP
         STR_MENU_HELP_HELPSTRING
         STR_MENU_VIEW_HIDENOGRADE
         STR_MENU_VIEW_HIDENOGRADE_HELPSTRING
         STR_MENU_VIEW_HIDENOJLPT
         STR_MENU_VIEW_HIDENOJLPT_HELPSTRING
         STR_MENU_VIEW_HIDENOFREQ
         STR_MENU_VIEW_HIDENOFREQ_HELPSTRING
         STR_MENU_VIEW_ADDFILTERBYRADICAL
         STR_MENU_VIEW_ADDFILTERBYRADICAL_HELPSTRING
         STR_MENU_TOOLS
         STR_MENU_TOOLS_STAYONTOP
         STR_MENU_TOOLS_STAYONTOP_HELPSTRING
         STR_MENU_HELP_ABOUT
         STR_MENU_HELP_ABOUT_HELPSTRING
         STR_FORMAT_SLOT
         STR_FORMAT_SLOT_HELPSTRING
         
         STR_STROKENUMWT
         STR_STROKENUMWT_NONE
         STR_STROKENUMWT_LOW
         STR_STROKENUMWT_MEDIUM
         STR_STROKENUMWT_HIGH
         
         STR_LABEL_FILTERBYRADICAL
         STR_DESCTEXT_GRADE
         STR_DESCTEXT_STROKENUM
         STR_DESCTEXT_MISCOUNTS
         STR_DESCTEXT_VARIANTS
         STR_DESCTEXT_USAGEFREQ
         STR_DESCTEXT_JLPT
         STR_DESCTEXT_READINGS
         STR_DESCTEXT_MEANINGS
         STR_DESCTEXT_NANORI
         STR_DESCTEXT_DICTREFS
         
         STR_RESULTSLIST_COLUMN_KANJI
         STR_RESULTSLIST_COLUMN_GRADE
         STR_RESULTSLIST_COLUMN_READINGS
         STR_RESULTSLIST_COLUMN_MEANINGS
         STR_RESULTSLIST_COLUMN_SCORE
         STR_BTN_ADDASRADICAL
         STR_FORMAT_ADDTOSLOT_HELPSTRING
         STR_BTN_RADICALSOFKANJI
         STR_BTN_WIKTIONARY
         
         STR_WIN_SELECTRADICAL
         
         STR_WIN_WIKTIONARYVIEWER
         )

(define STR_WIN_ABOUTDIALOG "About Kanjiget")


(define STR_DRAW_KANJI_HERE "Draw Kanji Here")
(define STR_BTN_RESTARTOVER "Restart Over")
(define STR_BTN_SEARCH      "Search")

(define STR_WIN_KANJIFINDER "KanjiGet")
(define STR_LABEL_SELECTRADICALS "Select Radical")
(define STR_FORMAT_RADICALSFOR "Radicals for ~a")

(define STR_MENU_FILE "&File")
(define STR_MENU_FILE_HELPSTRING "File related options")
(define STR_MENU_FILE_EXIT "E&xit")
(define STR_MENU_FILE_EXIT_HELPSTRING "Exit")
(define STR_MENU_EDIT "&Edit")
(define STR_MENU_VIEW "&View")
(define STR_MENU_HELP "&Help")
(define STR_MENU_HELP_HELPSTRING "Help related options")

(define STR_MENU_VIEW_HIDENOGRADE "Hide Entries without JP &Grade")
(define STR_MENU_VIEW_HIDENOGRADE_HELPSTRING
  "Set whether to hide entries that do not have a Grade set in Japan")
(define STR_MENU_VIEW_HIDENOJLPT "Hide Entries without &JLPT")
(define STR_MENU_VIEW_HIDENOJLPT_HELPSTRING
  "Set whether to hide entries that are not part of a JLPT Level")
(define STR_MENU_VIEW_HIDENOFREQ "Hide Entries without Freq.")
(define STR_MENU_VIEW_HIDENOFREQ_HELPSTRING
  "Set whether to hide entries that do not have a Usage Frequency in Japan")

(define STR_MENU_VIEW_ADDFILTERBYRADICAL "Add Filter by Radical")
(define STR_MENU_VIEW_ADDFILTERBYRADICAL_HELPSTRING
  "Set whether to hide entries that do not have a Usage Frequency in Japan")
(define STR_MENU_TOOLS "&Tools")
(define STR_MENU_TOOLS_STAYONTOP "&Stay on Top")
(define STR_MENU_TOOLS_STAYONTOP_HELPSTRING
  "Set whether window stays on top")

(define STR_MENU_HELP_ABOUT "&About")
(define STR_MENU_HELP_ABOUT_HELPSTRING
  "Information about KanjiGet")

(define STR_FORMAT_SLOT "Slot ~a")
(define STR_FORMAT_SLOT_HELPSTRING
  "Manually add radical to slot ~a")

(define STR_STROKENUMWT "Stroke # wt:")
(define STR_STROKENUMWT_NONE   "None")
(define STR_STROKENUMWT_LOW    "Low")
(define STR_STROKENUMWT_MEDIUM "Medium")
(define STR_STROKENUMWT_HIGH   "High")

(define STR_LABEL_FILTERBYRADICAL "Filter by Radicals:")

(define STR_DESCTEXT_GRADE     "Grade:")
(define STR_DESCTEXT_STROKENUM "Stroke #:")
(define STR_DESCTEXT_MISCOUNTS "miscounts:")
(define STR_DESCTEXT_VARIANTS  "Variants:")
(define STR_DESCTEXT_USAGEFREQ "Usage Freq:")
(define STR_DESCTEXT_JLPT      "JLPT:")
(define STR_DESCTEXT_READINGS  "Readings:")
(define STR_DESCTEXT_MEANINGS  "Meanings:")
(define STR_DESCTEXT_NANORI    "Nanori:")
(define STR_DESCTEXT_DICTREFS  "Dictionary References:")

(define STR_RESULTSLIST_COLUMN_KANJI    "Kanji")
(define STR_RESULTSLIST_COLUMN_GRADE    "Grade")
(define STR_RESULTSLIST_COLUMN_READINGS "Readings")
(define STR_RESULTSLIST_COLUMN_MEANINGS "Meanings")
(define STR_RESULTSLIST_COLUMN_SCORE    "Score")

(define STR_BTN_ADDASRADICAL "Add as Radical")
(define STR_FORMAT_ADDTOSLOT_HELPSTRING
  "Add to slot ~a")
(define STR_BTN_RADICALSOFKANJI "Radicals of Kanji")
(define STR_BTN_WIKTIONARY "Wiktionary")

(define STR_WIN_SELECTRADICAL "Select Radical")

(define STR_WIN_WIKTIONARYVIEWER "Wiktionary Viewer")
