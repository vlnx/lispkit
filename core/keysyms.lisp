;; BUG: XCompose's Multi_key, not registered, gtk?, and keysyms need to be the char when not a modifier
(in-package :lispkit)

;;  This file is part of stumpwm.
;; Commentary:
;;
;; Mapping a keysym to a name is a client side activity in X11.  Some
;; of the code here was taken from the CMUCL Hemlocks code base.  The
;; actual mappings were taken from Xorg's keysymdefs.h.
;;
;; Code:

(defvar *keysym-name-translations* (make-hash-table))
(defvar *name-keysym-translations* (make-hash-table :test #'equal))

(defun define-keysym (keysym name)
  "Define a mapping from a keysym name to a keysym."
  (setf (gethash keysym *keysym-name-translations*) name
        (gethash name *name-keysym-translations*) keysym))

(defun keysym-name->keysym (name)
  "Return the keysym corresponding to NAME."
  (multiple-value-bind (value present-p)
      (gethash name *name-keysym-translations*)
    (declare (ignore present-p))
    value))

(defun keysym->keysym-name (keysym)
  "Return the name corresponding to KEYSYM."
  (multiple-value-bind (value present-p)
      (gethash keysym *keysym-name-translations*)
    (declare (ignore present-p))
    value))

(mapcar (lambda (args) (apply #'define-keysym args))
        '((#xffffff "VoidSymbol")   ;Void symbol
          (#xff08 "BackSpace")      ;Back space, back char
          (#xff09 "Tab")
          (#xff0a "Linefeed")       ;Linefeed, LF
          (#xff0b "Clear")
          (#xff0d "Return")         ;Return, enter
          (#xff13 "Pause")          ;Pause, hold
          (#xff14 "Scroll_Lock")
          (#xff15 "Sys_Req")
          (#xff1b "Escape")
          (#xffff "Delete")         ;Delete, rubout
          (#xff20 "Multi_key")      ;Multi-key character compose
          (#xff37 "Codeinput")
          (#xff3c "SingleCandidate")
          (#xff3d "MultipleCandidate")
          (#xff3e "PreviousCandidate")
          (#xff21 "Kanji")          ;Kanji, Kanji convert
          (#xff22 "Muhenkan")       ;Cancel Conversion
          (#xff23 "Henkan_Mode")    ;Start/Stop Conversion
          (#xff23 "Henkan")         ;Alias for Henkan_Mode
          (#xff24 "Romaji")         ;to Romaji
          (#xff25 "Hiragana")       ;to Hiragana
          (#xff26 "Katakana")       ;to Katakana
          (#xff27 "Hiragana_Katakana") ;Hiragana/Katakana toggle
          (#xff28 "Zenkaku")        ;to Zenkaku
          (#xff29 "Hankaku")        ;to Hankaku
          (#xff2a "Zenkaku_Hankaku") ;Zenkaku/Hankaku toggle
          (#xff2b "Touroku")        ;Add to Dictionary
          (#xff2c "Massyo")         ;Delete from Dictionary
          (#xff2d "Kana_Lock")      ;Kana Lock
          (#xff2e "Kana_Shift")     ;Kana Shift
          (#xff2f "Eisu_Shift")     ;Alphanumeric Shift
          (#xff30 "Eisu_toggle")    ;Alphanumeric toggle
          (#xff37 "Kanji_Bangou")   ;Codeinput
          (#xff3d "Zen_Koho")       ;Multiple/All Candidate(s)
          (#xff3e "Mae_Koho")       ;Previous Candidate
          (#xff50 "Home")
          (#xff51 "Left")           ;Move left, left arrow
          (#xff52 "Up")             ;Move up, up arrow
          (#xff53 "Right")          ;Move right, right arrow
          (#xff54 "Down")           ;Move down, down arrow
          (#xff55 "Prior")          ;Prior, previous
          (#xff55 "Page_Up")
          (#xff56 "Next")           ;Next
          (#xff56 "Page_Down")
          (#xff57 "End")            ;EOL
          (#xff58 "Begin")          ;BOL
          (#xff60 "Select")         ;Select, mark
          (#xff61 "Print")
          (#xff62 "Execute")        ;Execute, run, do
          (#xff63 "Insert")         ;Insert, insert here
          (#xff65 "Undo")
          (#xff66 "Redo")           ;Redo, again
          (#xff67 "Menu")
          (#xff68 "Find")           ;Find, search
          (#xff69 "Cancel")         ;Cancel, stop, abort, exit
          (#xff6a "Help")           ;Help
          (#xff6b "Break")
          (#xff7e "Mode_switch")    ;Character set switch
          (#xff7e "script_switch")  ;Alias for mode_switch
          (#xff7f "Num_Lock")
          (#xff80 "KP_Space")       ;Space
          (#xff89 "KP_Tab")
          (#xff8d "KP_Enter")       ;Enter
          (#xff91 "KP_F1")          ;PF1, KP_A, ...
          (#xff92 "KP_F2")
          (#xff93 "KP_F3")
          (#xff94 "KP_F4")
          (#xff95 "KP_Home")
          (#xff96 "KP_Left")
          (#xff97 "KP_Up")
          (#xff98 "KP_Right")
          (#xff99 "KP_Down")
          (#xff9a "KP_Prior")
          (#xff9a "KP_Page_Up")
          (#xff9b "KP_Next")
          (#xff9b "KP_Page_Down")
          (#xff9c "KP_End")
          (#xff9d "KP_Begin")
          (#xff9e "KP_Insert")
          (#xff9f "KP_Delete")
          (#xffbd "KP_Equal")       ;Equals
          (#xffaa "KP_Multiply")
          (#xffab "KP_Add")
          (#xffac "KP_Separator")   ;Separator, often comma
          (#xffad "KP_Subtract")
          (#xffae "KP_Decimal")
          (#xffaf "KP_Divide")
          (#xffb0 "KP_0")
          (#xffb1 "KP_1")
          (#xffb2 "KP_2")
          (#xffb3 "KP_3")
          (#xffb4 "KP_4")
          (#xffb5 "KP_5")
          (#xffb6 "KP_6")
          (#xffb7 "KP_7")
          (#xffb8 "KP_8")
          (#xffb9 "KP_9")
          (#xffbe "F1")
          (#xffbf "F2")
          (#xffc0 "F3")
          (#xffc1 "F4")
          (#xffc2 "F5")
          (#xffc3 "F6")
          (#xffc4 "F7")
          (#xffc5 "F8")
          (#xffc6 "F9")
          (#xffc7 "F10")
          (#xffc8 "F11")
          (#xffc9 "F12")
          (#xffca "F13")
          (#xffcb "F14")
          (#xffcc "F15")
          (#xffcd "F16")
          (#xffce "F17")
          (#xffcf "F18")
          (#xffd0 "F19")
          (#xffd1 "F20")
          (#xffd2 "F21")
          (#xffd3 "F22")
          (#xffd4 "F23")
          (#xffd5 "F24")
          (#xffd6 "F25")
          (#xffd7 "F26")
          (#xffd8 "F27")
          (#xffd9 "F28")
          (#xffda "F29")
          (#xffdb "F30")
          (#xffdc "F31")
          (#xffdd "F32")
          (#xffde "F33")
          (#xffdf "F34")
          (#xffe0 "F35")
          (#xffe1 "Shift_L")        ;Left shift
          (#xffe2 "Shift_R")        ;Right shift
          (#xffe3 "Control_L")      ;Left control
          (#xffe4 "Control_R")      ;Right control
          (#xffe5 "Caps_Lock")      ;Caps lock
          (#xffe6 "Shift_Lock")     ;Shift lock
          (#xffe7 "Meta_L")         ;Left meta
          (#xffe8 "Meta_R")         ;Right meta
          (#xffe9 "Alt_L")          ;Left alt
          (#xffea "Alt_R")          ;Right alt
          (#xffeb "Super_L")        ;Left super
          (#xffec "Super_R")        ;Right super
          (#xffed "Hyper_L")        ;Left hyper
          (#xffee "Hyper_R")        ;Right hyper
          (#xfe01 "ISO_Lock")
          (#xfe02 "ISO_Level2_Latch")
          (#xfe03 "ISO_Level3_Shift")
          (#xfe04 "ISO_Level3_Latch")
          (#xfe05 "ISO_Level3_Lock")
          (#xff7e "ISO_Group_Shift") ;Alias for mode_switch
          (#xfe06 "ISO_Group_Latch")
          (#xfe07 "ISO_Group_Lock")
          (#xfe08 "ISO_Next_Group")
          (#xfe09 "ISO_Next_Group_Lock")
          (#xfe0a "ISO_Prev_Group")
          (#xfe0b "ISO_Prev_Group_Lock")
          (#xfe0c "ISO_First_Group")
          (#xfe0d "ISO_First_Group_Lock")
          (#xfe0e "ISO_Last_Group")
          (#xfe0f "ISO_Last_Group_Lock")
          (#xfe20 "ISO_Left_Tab")
          (#xfe21 "ISO_Move_Line_Up")
          (#xfe22 "ISO_Move_Line_Down")
          (#xfe23 "ISO_Partial_Line_Up")
          (#xfe24 "ISO_Partial_Line_Down")
          (#xfe25 "ISO_Partial_Space_Left")
          (#xfe26 "ISO_Partial_Space_Right")
          (#xfe27 "ISO_Set_Margin_Left")
          (#xfe28 "ISO_Set_Margin_Right")
          (#xfe29 "ISO_Release_Margin_Left")
          (#xfe2a "ISO_Release_Margin_Right")
          (#xfe2b "ISO_Release_Both_Margins")
          (#xfe2c "ISO_Fast_Cursor_Left")
          (#xfe2d "ISO_Fast_Cursor_Right")
          (#xfe2e "ISO_Fast_Cursor_Up")
          (#xfe2f "ISO_Fast_Cursor_Down")
          (#xfe30 "ISO_Continuous_Underline")
          (#xfe31 "ISO_Discontinuous_Underline")
          (#xfe32 "ISO_Emphasize")
          (#xfe33 "ISO_Center_Object")
          (#xfe34 "ISO_Enter")
          (#xfe50 "dead_grave")
          (#xfe51 "dead_acute")
          (#xfe52 "dead_circumflex")
          (#xfe53 "dead_tilde")
          (#xfe54 "dead_macron")
          (#xfe55 "dead_breve")
          (#xfe56 "dead_abovedot")
          (#xfe57 "dead_diaeresis")
          (#xfe58 "dead_abovering")
          (#xfe59 "dead_doubleacute")
          (#xfe5a "dead_caron")
          (#xfe5b "dead_cedilla")
          (#xfe5c "dead_ogonek")
          (#xfe5d "dead_iota")
          (#xfe5e "dead_voiced_sound")
          (#xfe5f "dead_semivoiced_sound")
          (#xfe60 "dead_belowdot")
          (#xfe61 "dead_hook")
          (#xfe62 "dead_horn")
          (#xfed0 "First_Virtual_Screen")
          (#xfed1 "Prev_Virtual_Screen")
          (#xfed2 "Next_Virtual_Screen")
          (#xfed4 "Last_Virtual_Screen")
          (#xfed5 "Terminate_Server")
          (#xfe70 "AccessX_Enable")
          (#xfe71 "AccessX_Feedback_Enable")
          (#xfe72 "RepeatKeys_Enable")
          (#xfe73 "SlowKeys_Enable")
          (#xfe74 "BounceKeys_Enable")
          (#xfe75 "StickyKeys_Enable")
          (#xfe76 "MouseKeys_Enable")
          (#xfe77 "MouseKeys_Accel_Enable")
          (#xfe78 "Overlay1_Enable")
          (#xfe79 "Overlay2_Enable")
          (#xfe7a "AudibleBell_Enable")
          (#xfee0 "Pointer_Left")
          (#xfee1 "Pointer_Right")
          (#xfee2 "Pointer_Up")
          (#xfee3 "Pointer_Down")
          (#xfee4 "Pointer_UpLeft")
          (#xfee5 "Pointer_UpRight")
          (#xfee6 "Pointer_DownLeft")
          (#xfee7 "Pointer_DownRight")
          (#xfee8 "Pointer_Button_Dflt")
          (#xfee9 "Pointer_Button1")
          (#xfeea "Pointer_Button2")
          (#xfeeb "Pointer_Button3")
          (#xfeec "Pointer_Button4")
          (#xfeed "Pointer_Button5")
          (#xfeee "Pointer_DblClick_Dflt")
          (#xfeef "Pointer_DblClick1")
          (#xfef0 "Pointer_DblClick2")
          (#xfef1 "Pointer_DblClick3")
          (#xfef2 "Pointer_DblClick4")
          (#xfef3 "Pointer_DblClick5")
          (#xfef4 "Pointer_Drag_Dflt")
          (#xfef5 "Pointer_Drag1")
          (#xfef6 "Pointer_Drag2")
          (#xfef7 "Pointer_Drag3")
          (#xfef8 "Pointer_Drag4")
          (#xfefd "Pointer_Drag5")
          (#xfef9 "Pointer_EnableKeys")
          (#xfefa "Pointer_Accelerate")
          (#xfefb "Pointer_DfltBtnNext")
          (#xfefc "Pointer_DfltBtnPrev")
          (#xfd01 "3270_Duplicate")
          (#xfd02 "3270_FieldMark")
          (#xfd03 "3270_Right2")
          (#xfd04 "3270_Left2")
          (#xfd05 "3270_BackTab")
          (#xfd06 "3270_EraseEOF")
          (#xfd07 "3270_EraseInput")
          (#xfd08 "3270_Reset")
          (#xfd09 "3270_Quit")
          (#xfd0a "3270_PA1")
          (#xfd0b "3270_PA2")
          (#xfd0c "3270_PA3")
          (#xfd0d "3270_Test")
          (#xfd0e "3270_Attn")
          (#xfd0f "3270_CursorBlink")
          (#xfd10 "3270_AltCursor")
          (#xfd11 "3270_KeyClick")
          (#xfd12 "3270_Jump")
          (#xfd13 "3270_Ident")
          (#xfd14 "3270_Rule")
          (#xfd15 "3270_Copy")
          (#xfd16 "3270_Play")
          (#xfd17 "3270_Setup")
          (#xfd18 "3270_Record")
          (#xfd19 "3270_ChangeScreen")
          (#xfd1a "3270_DeleteWord")
          (#xfd1b "3270_ExSelect")
          (#xfd1c "3270_CursorSelect")
          (#xfd1d "3270_PrintScreen")
          (#xfd1e "3270_Enter")
          (#x0020 "space")          ;U+0020 SPACE
          (#x0021 "exclam")         ;U+0021 EXCLAMATION MARK
          (#x0022 "quotedbl")       ;U+0022 QUOTATION MARK
          (#x0023 "numbersign")     ;U+0023 NUMBER SIGN
          (#x0024 "dollar")         ;U+0024 DOLLAR SIGN
          (#x0025 "percent")        ;U+0025 PERCENT SIGN
          (#x0026 "ampersand")      ;U+0026 AMPERSAND
          (#x0027 "apostrophe")     ;U+0027 APOSTROPHE
          (#x0027 "quoteright")     ;deprecated
          (#x0028 "parenleft")      ;U+0028 LEFT PARENTHESIS
          (#x0029 "parenright")     ;U+0029 RIGHT PARENTHESIS
          (#x002a "asterisk")       ;U+002A ASTERISK
          (#x002b "plus")           ;U+002B PLUS SIGN
          (#x002c "comma")          ;U+002C COMMA
          (#x002d "minus")          ;U+002D HYPHEN-MINUS
          (#x002e "period")         ;U+002E FULL STOP
          (#x002f "slash")          ;U+002F SOLIDUS
          (#x0030 "0")              ;U+0030 DIGIT ZERO
          (#x0031 "1")              ;U+0031 DIGIT ONE
          (#x0032 "2")              ;U+0032 DIGIT TWO
          (#x0033 "3")              ;U+0033 DIGIT THREE
          (#x0034 "4")              ;U+0034 DIGIT FOUR
          (#x0035 "5")              ;U+0035 DIGIT FIVE
          (#x0036 "6")              ;U+0036 DIGIT SIX
          (#x0037 "7")              ;U+0037 DIGIT SEVEN
          (#x0038 "8")              ;U+0038 DIGIT EIGHT
          (#x0039 "9")              ;U+0039 DIGIT NINE
          (#x003a "colon")          ;U+003A COLON
          (#x003b "semicolon")      ;U+003B SEMICOLON
          (#x003c "less")           ;U+003C LESS-THAN SIGN
          (#x003d "equal")          ;U+003D EQUALS SIGN
          (#x003e "greater")        ;U+003E GREATER-THAN SIGN
          (#x003f "question")       ;U+003F QUESTION MARK
          (#x0040 "at")             ;U+0040 COMMERCIAL AT
          (#x0041 "A")              ;U+0041 LATIN CAPITAL LETTER A
          (#x0042 "B")              ;U+0042 LATIN CAPITAL LETTER B
          (#x0043 "C")              ;U+0043 LATIN CAPITAL LETTER C
          (#x0044 "D")              ;U+0044 LATIN CAPITAL LETTER D
          (#x0045 "E")              ;U+0045 LATIN CAPITAL LETTER E
          (#x0046 "F")              ;U+0046 LATIN CAPITAL LETTER F
          (#x0047 "G")              ;U+0047 LATIN CAPITAL LETTER G
          (#x0048 "H")              ;U+0048 LATIN CAPITAL LETTER H
          (#x0049 "I")              ;U+0049 LATIN CAPITAL LETTER I
          (#x004a "J")              ;U+004A LATIN CAPITAL LETTER J
          (#x004b "K")              ;U+004B LATIN CAPITAL LETTER K
          (#x004c "L")              ;U+004C LATIN CAPITAL LETTER L
          (#x004d "M")              ;U+004D LATIN CAPITAL LETTER M
          (#x004e "N")              ;U+004E LATIN CAPITAL LETTER N
          (#x004f "O")              ;U+004F LATIN CAPITAL LETTER O
          (#x0050 "P")              ;U+0050 LATIN CAPITAL LETTER P
          (#x0051 "Q")              ;U+0051 LATIN CAPITAL LETTER Q
          (#x0052 "R")              ;U+0052 LATIN CAPITAL LETTER R
          (#x0053 "S")              ;U+0053 LATIN CAPITAL LETTER S
          (#x0054 "T")              ;U+0054 LATIN CAPITAL LETTER T
          (#x0055 "U")              ;U+0055 LATIN CAPITAL LETTER U
          (#x0056 "V")              ;U+0056 LATIN CAPITAL LETTER V
          (#x0057 "W")              ;U+0057 LATIN CAPITAL LETTER W
          (#x0058 "X")              ;U+0058 LATIN CAPITAL LETTER X
          (#x0059 "Y")              ;U+0059 LATIN CAPITAL LETTER Y
          (#x005a "Z")              ;U+005A LATIN CAPITAL LETTER Z
          (#x005b "bracketleft")    ;U+005B LEFT SQUARE BRACKET
          (#x005c "backslash")      ;U+005C REVERSE SOLIDUS
          (#x005d "bracketright")   ;U+005D RIGHT SQUARE BRACKET
          (#x005e "asciicircum")    ;U+005E CIRCUMFLEX ACCENT
          (#x005f "underscore")     ;U+005F LOW LINE
          (#x0060 "grave")          ;U+0060 GRAVE ACCENT
          (#x0060 "quoteleft")      ;deprecated
          (#x0061 "a")              ;U+0061 LATIN SMALL LETTER A
          (#x0062 "b")              ;U+0062 LATIN SMALL LETTER B
          (#x0063 "c")              ;U+0063 LATIN SMALL LETTER C
          (#x0064 "d")              ;U+0064 LATIN SMALL LETTER D
          (#x0065 "e")              ;U+0065 LATIN SMALL LETTER E
          (#x0066 "f")              ;U+0066 LATIN SMALL LETTER F
          (#x0067 "g")              ;U+0067 LATIN SMALL LETTER G
          (#x0068 "h")              ;U+0068 LATIN SMALL LETTER H
          (#x0069 "i")              ;U+0069 LATIN SMALL LETTER I
          (#x006a "j")              ;U+006A LATIN SMALL LETTER J
          (#x006b "k")              ;U+006B LATIN SMALL LETTER K
          (#x006c "l")              ;U+006C LATIN SMALL LETTER L
          (#x006d "m")              ;U+006D LATIN SMALL LETTER M
          (#x006e "n")              ;U+006E LATIN SMALL LETTER N
          (#x006f "o")              ;U+006F LATIN SMALL LETTER O
          (#x0070 "p")              ;U+0070 LATIN SMALL LETTER P
          (#x0071 "q")              ;U+0071 LATIN SMALL LETTER Q
          (#x0072 "r")              ;U+0072 LATIN SMALL LETTER R
          (#x0073 "s")              ;U+0073 LATIN SMALL LETTER S
          (#x0074 "t")              ;U+0074 LATIN SMALL LETTER T
          (#x0075 "u")              ;U+0075 LATIN SMALL LETTER U
          (#x0076 "v")              ;U+0076 LATIN SMALL LETTER V
          (#x0077 "w")              ;U+0077 LATIN SMALL LETTER W
          (#x0078 "x")              ;U+0078 LATIN SMALL LETTER X
          (#x0079 "y")              ;U+0079 LATIN SMALL LETTER Y
          (#x007a "z")              ;U+007A LATIN SMALL LETTER Z
          (#x007b "braceleft")      ;U+007B LEFT CURLY BRACKET
          (#x007c "bar")            ;U+007C VERTICAL LINE
          (#x007d "braceright")     ;U+007D RIGHT CURLY BRACKET
          (#x007e "asciitilde")     ;U+007E TILDE
          (#x00a0 "nobreakspace")   ;U+00A0 NO-BREAK SPACE
          (#x00a1 "exclamdown")  ;U+00A1 INVERTED EXCLAMATION MARK
          (#x00a2 "cent")           ;U+00A2 CENT SIGN
          (#x00a3 "sterling")       ;U+00A3 POUND SIGN
          (#x00a4 "currency")       ;U+00A4 CURRENCY SIGN
          (#x00a5 "yen")            ;U+00A5 YEN SIGN
          (#x00a6 "brokenbar")      ;U+00A6 BROKEN BAR
          (#x00a7 "section")        ;U+00A7 SECTION SIGN
          (#x00a8 "diaeresis")      ;U+00A8 DIAERESIS
          (#x00a9 "copyright")      ;U+00A9 COPYRIGHT SIGN
          (#x00aa "ordfeminine") ;U+00AA FEMININE ORDINAL INDICATOR
          (#x00ab "guillemotleft") ;U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
          (#x00ac "notsign")        ;U+00AC NOT SIGN
          (#x00ad "hyphen")         ;U+00AD SOFT HYPHEN
          (#x00ae "registered")     ;U+00AE REGISTERED SIGN
          (#x00af "macron")         ;U+00AF MACRON
          (#x00b0 "degree")         ;U+00B0 DEGREE SIGN
          (#x00b1 "plusminus")      ;U+00B1 PLUS-MINUS SIGN
          (#x00b2 "twosuperior")    ;U+00B2 SUPERSCRIPT TWO
          (#x00b3 "threesuperior")  ;U+00B3 SUPERSCRIPT THREE
          (#x00b4 "acute")          ;U+00B4 ACUTE ACCENT
          (#x00b5 "mu")             ;U+00B5 MICRO SIGN
          (#x00b6 "paragraph")      ;U+00B6 PILCROW SIGN
          (#x00b7 "periodcentered") ;U+00B7 MIDDLE DOT
          (#x00b8 "cedilla")        ;U+00B8 CEDILLA
          (#x00b9 "onesuperior")    ;U+00B9 SUPERSCRIPT ONE
          (#x00ba "masculine") ;U+00BA MASCULINE ORDINAL INDICATOR
          (#x00bb "guillemotright") ;U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
          (#x00bc "onequarter") ;U+00BC VULGAR FRACTION ONE QUARTER
          (#x00bd "onehalf")      ;U+00BD VULGAR FRACTION ONE HALF
          (#x00be "threequarters") ;U+00BE VULGAR FRACTION THREE QUARTERS
          (#x00bf "questiondown")   ;U+00BF INVERTED QUESTION MARK
          (#x00c0 "Agrave") ;U+00C0 LATIN CAPITAL LETTER A WITH GRAVE
          (#x00c1 "Aacute") ;U+00C1 LATIN CAPITAL LETTER A WITH ACUTE
          (#x00c2 "Acircumflex") ;U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX
          (#x00c3 "Atilde") ;U+00C3 LATIN CAPITAL LETTER A WITH TILDE
          (#x00c4 "Adiaeresis") ;U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS
          (#x00c5 "Aring") ;U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE
          (#x00c6 "AE")            ;U+00C6 LATIN CAPITAL LETTER AE
          (#x00c7 "Ccedilla") ;U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA
          (#x00c8 "Egrave") ;U+00C8 LATIN CAPITAL LETTER E WITH GRAVE
          (#x00c9 "Eacute") ;U+00C9 LATIN CAPITAL LETTER E WITH ACUTE
          (#x00ca "Ecircumflex") ;U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX
          (#x00cb "Ediaeresis") ;U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS
          (#x00cc "Igrave") ;U+00CC LATIN CAPITAL LETTER I WITH GRAVE
          (#x00cd "Iacute") ;U+00CD LATIN CAPITAL LETTER I WITH ACUTE
          (#x00ce "Icircumflex") ;U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX
          (#x00cf "Idiaeresis") ;U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS
          (#x00d0 "ETH")          ;U+00D0 LATIN CAPITAL LETTER ETH
          (#x00d0 "Eth")            ;deprecated
          (#x00d1 "Ntilde") ;U+00D1 LATIN CAPITAL LETTER N WITH TILDE
          (#x00d2 "Ograve") ;U+00D2 LATIN CAPITAL LETTER O WITH GRAVE
          (#x00d3 "Oacute") ;U+00D3 LATIN CAPITAL LETTER O WITH ACUTE
          (#x00d4 "Ocircumflex") ;U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX
          (#x00d5 "Otilde") ;U+00D5 LATIN CAPITAL LETTER O WITH TILDE
          (#x00d6 "Odiaeresis") ;U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS
          (#x00d7 "multiply")       ;U+00D7 MULTIPLICATION SIGN
          (#x00d8 "Oslash") ;U+00D8 LATIN CAPITAL LETTER O WITH STROKE
          (#x00d8 "Ooblique") ;U+00D8 LATIN CAPITAL LETTER O WITH STROKE
          (#x00d9 "Ugrave") ;U+00D9 LATIN CAPITAL LETTER U WITH GRAVE
          (#x00da "Uacute") ;U+00DA LATIN CAPITAL LETTER U WITH ACUTE
          (#x00db "Ucircumflex") ;U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX
          (#x00dc "Udiaeresis") ;U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS
          (#x00dd "Yacute") ;U+00DD LATIN CAPITAL LETTER Y WITH ACUTE
          (#x00de "THORN")      ;U+00DE LATIN CAPITAL LETTER THORN
          (#x00de "Thorn")          ;deprecated
          (#x00df "ssharp")     ;U+00DF LATIN SMALL LETTER SHARP S
          (#x00e0 "agrave") ;U+00E0 LATIN SMALL LETTER A WITH GRAVE
          (#x00e1 "aacute") ;U+00E1 LATIN SMALL LETTER A WITH ACUTE
          (#x00e2 "acircumflex") ;U+00E2 LATIN SMALL LETTER A WITH CIRCUMFLEX
          (#x00e3 "atilde") ;U+00E3 LATIN SMALL LETTER A WITH TILDE
          (#x00e4 "adiaeresis") ;U+00E4 LATIN SMALL LETTER A WITH DIAERESIS
          (#x00e5 "aring") ;U+00E5 LATIN SMALL LETTER A WITH RING ABOVE
          (#x00e6 "ae")             ;U+00E6 LATIN SMALL LETTER AE
          (#x00e7 "ccedilla") ;U+00E7 LATIN SMALL LETTER C WITH CEDILLA
          (#x00e8 "egrave") ;U+00E8 LATIN SMALL LETTER E WITH GRAVE
          (#x00e9 "eacute") ;U+00E9 LATIN SMALL LETTER E WITH ACUTE
          (#x00ea "ecircumflex") ;U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX
          (#x00eb "ediaeresis") ;U+00EB LATIN SMALL LETTER E WITH DIAERESIS
          (#x00ec "igrave") ;U+00EC LATIN SMALL LETTER I WITH GRAVE
          (#x00ed "iacute") ;U+00ED LATIN SMALL LETTER I WITH ACUTE
          (#x00ee "icircumflex") ;U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX
          (#x00ef "idiaeresis") ;U+00EF LATIN SMALL LETTER I WITH DIAERESIS
          (#x00f0 "eth")            ;U+00F0 LATIN SMALL LETTER ETH
          (#x00f1 "ntilde") ;U+00F1 LATIN SMALL LETTER N WITH TILDE
          (#x00f2 "ograve") ;U+00F2 LATIN SMALL LETTER O WITH GRAVE
          (#x00f3 "oacute") ;U+00F3 LATIN SMALL LETTER O WITH ACUTE
          (#x00f4 "ocircumflex") ;U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX
          (#x00f5 "otilde") ;U+00F5 LATIN SMALL LETTER O WITH TILDE
          (#x00f6 "odiaeresis") ;U+00F6 LATIN SMALL LETTER O WITH DIAERESIS
          (#x00f7 "division")       ;U+00F7 DIVISION SIGN
          (#x00f8 "oslash") ;U+00F8 LATIN SMALL LETTER O WITH STROKE
          (#x00f8 "ooblique") ;U+00F8 LATIN SMALL LETTER O WITH STROKE
          (#x00f9 "ugrave") ;U+00F9 LATIN SMALL LETTER U WITH GRAVE
          (#x00fa "uacute") ;U+00FA LATIN SMALL LETTER U WITH ACUTE
          (#x00fb "ucircumflex") ;U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX
          (#x00fc "udiaeresis") ;U+00FC LATIN SMALL LETTER U WITH DIAERESIS
          (#x00fd "yacute") ;U+00FD LATIN SMALL LETTER Y WITH ACUTE
          (#x00fe "thorn")        ;U+00FE LATIN SMALL LETTER THORN
          (#x00ff "ydiaeresis") ;U+00FF LATIN SMALL LETTER Y WITH DIAERESIS
          (#x01a1 "Aogonek") ;U+0104 LATIN CAPITAL LETTER A WITH OGONEK
          (#x01a2 "breve")          ;U+02D8 BREVE
          (#x01a3 "Lstroke") ;U+0141 LATIN CAPITAL LETTER L WITH STROKE
          (#x01a5 "Lcaron") ;U+013D LATIN CAPITAL LETTER L WITH CARON
          (#x01a6 "Sacute") ;U+015A LATIN CAPITAL LETTER S WITH ACUTE
          (#x01a9 "Scaron") ;U+0160 LATIN CAPITAL LETTER S WITH CARON
          (#x01aa "Scedilla") ;U+015E LATIN CAPITAL LETTER S WITH CEDILLA
          (#x01ab "Tcaron") ;U+0164 LATIN CAPITAL LETTER T WITH CARON
          (#x01ac "Zacute") ;U+0179 LATIN CAPITAL LETTER Z WITH ACUTE
          (#x01ae "Zcaron") ;U+017D LATIN CAPITAL LETTER Z WITH CARON
          (#x01af "Zabovedot") ;U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE
          (#x01b1 "aogonek") ;U+0105 LATIN SMALL LETTER A WITH OGONEK
          (#x01b2 "ogonek")         ;U+02DB OGONEK
          (#x01b3 "lstroke") ;U+0142 LATIN SMALL LETTER L WITH STROKE
          (#x01b5 "lcaron") ;U+013E LATIN SMALL LETTER L WITH CARON
          (#x01b6 "sacute") ;U+015B LATIN SMALL LETTER S WITH ACUTE
          (#x01b7 "caron")          ;U+02C7 CARON
          (#x01b9 "scaron") ;U+0161 LATIN SMALL LETTER S WITH CARON
          (#x01ba "scedilla") ;U+015F LATIN SMALL LETTER S WITH CEDILLA
          (#x01bb "tcaron") ;U+0165 LATIN SMALL LETTER T WITH CARON
          (#x01bc "zacute") ;U+017A LATIN SMALL LETTER Z WITH ACUTE
          (#x01bd "doubleacute")    ;U+02DD DOUBLE ACUTE ACCENT
          (#x01be "zcaron") ;U+017E LATIN SMALL LETTER Z WITH CARON
          (#x01bf "zabovedot") ;U+017C LATIN SMALL LETTER Z WITH DOT ABOVE
          (#x01c0 "Racute") ;U+0154 LATIN CAPITAL LETTER R WITH ACUTE
          (#x01c3 "Abreve") ;U+0102 LATIN CAPITAL LETTER A WITH BREVE
          (#x01c5 "Lacute") ;U+0139 LATIN CAPITAL LETTER L WITH ACUTE
          (#x01c6 "Cacute") ;U+0106 LATIN CAPITAL LETTER C WITH ACUTE
          (#x01c8 "Ccaron") ;U+010C LATIN CAPITAL LETTER C WITH CARON
          (#x01ca "Eogonek") ;U+0118 LATIN CAPITAL LETTER E WITH OGONEK
          (#x01cc "Ecaron") ;U+011A LATIN CAPITAL LETTER E WITH CARON
          (#x01cf "Dcaron") ;U+010E LATIN CAPITAL LETTER D WITH CARON
          (#x01d0 "Dstroke") ;U+0110 LATIN CAPITAL LETTER D WITH STROKE
          (#x01d1 "Nacute") ;U+0143 LATIN CAPITAL LETTER N WITH ACUTE
          (#x01d2 "Ncaron") ;U+0147 LATIN CAPITAL LETTER N WITH CARON
          (#x01d5 "Odoubleacute") ;U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
          (#x01d8 "Rcaron") ;U+0158 LATIN CAPITAL LETTER R WITH CARON
          (#x01d9 "Uring") ;U+016E LATIN CAPITAL LETTER U WITH RING ABOVE
          (#x01db "Udoubleacute") ;U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
          (#x01de "Tcedilla") ;U+0162 LATIN CAPITAL LETTER T WITH CEDILLA
          (#x01e0 "racute") ;U+0155 LATIN SMALL LETTER R WITH ACUTE
          (#x01e3 "abreve") ;U+0103 LATIN SMALL LETTER A WITH BREVE
          (#x01e5 "lacute") ;U+013A LATIN SMALL LETTER L WITH ACUTE
          (#x01e6 "cacute") ;U+0107 LATIN SMALL LETTER C WITH ACUTE
          (#x01e8 "ccaron") ;U+010D LATIN SMALL LETTER C WITH CARON
          (#x01ea "eogonek") ;U+0119 LATIN SMALL LETTER E WITH OGONEK
          (#x01ec "ecaron") ;U+011B LATIN SMALL LETTER E WITH CARON
          (#x01ef "dcaron") ;U+010F LATIN SMALL LETTER D WITH CARON
          (#x01f0 "dstroke") ;U+0111 LATIN SMALL LETTER D WITH STROKE
          (#x01f1 "nacute") ;U+0144 LATIN SMALL LETTER N WITH ACUTE
          (#x01f2 "ncaron") ;U+0148 LATIN SMALL LETTER N WITH CARON
          (#x01f5 "odoubleacute") ;U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE
          (#x01fb "udoubleacute") ;U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE
          (#x01f8 "rcaron") ;U+0159 LATIN SMALL LETTER R WITH CARON
          (#x01f9 "uring") ;U+016F LATIN SMALL LETTER U WITH RING ABOVE
          (#x01fe "tcedilla") ;U+0163 LATIN SMALL LETTER T WITH CEDILLA
          (#x01ff "abovedot")       ;U+02D9 DOT ABOVE
          (#x02a1 "Hstroke") ;U+0126 LATIN CAPITAL LETTER H WITH STROKE
          (#x02a6 "Hcircumflex") ;U+0124 LATIN CAPITAL LETTER H WITH CIRCUMFLEX
          (#x02a9 "Iabovedot") ;U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
          (#x02ab "Gbreve") ;U+011E LATIN CAPITAL LETTER G WITH BREVE
          (#x02ac "Jcircumflex") ;U+0134 LATIN CAPITAL LETTER J WITH CIRCUMFLEX
          (#x02b1 "hstroke") ;U+0127 LATIN SMALL LETTER H WITH STROKE
          (#x02b6 "hcircumflex") ;U+0125 LATIN SMALL LETTER H WITH CIRCUMFLEX
          (#x02b9 "idotless") ;U+0131 LATIN SMALL LETTER DOTLESS I
          (#x02bb "gbreve") ;U+011F LATIN SMALL LETTER G WITH BREVE
          (#x02bc "jcircumflex") ;U+0135 LATIN SMALL LETTER J WITH CIRCUMFLEX
          (#x02c5 "Cabovedot") ;U+010A LATIN CAPITAL LETTER C WITH DOT ABOVE
          (#x02c6 "Ccircumflex") ;U+0108 LATIN CAPITAL LETTER C WITH CIRCUMFLEX
          (#x02d5 "Gabovedot") ;U+0120 LATIN CAPITAL LETTER G WITH DOT ABOVE
          (#x02d8 "Gcircumflex") ;U+011C LATIN CAPITAL LETTER G WITH CIRCUMFLEX
          (#x02dd "Ubreve") ;U+016C LATIN CAPITAL LETTER U WITH BREVE
          (#x02de "Scircumflex") ;U+015C LATIN CAPITAL LETTER S WITH CIRCUMFLEX
          (#x02e5 "cabovedot") ;U+010B LATIN SMALL LETTER C WITH DOT ABOVE
          (#x02e6 "ccircumflex") ;U+0109 LATIN SMALL LETTER C WITH CIRCUMFLEX
          (#x02f5 "gabovedot") ;U+0121 LATIN SMALL LETTER G WITH DOT ABOVE
          (#x02f8 "gcircumflex") ;U+011D LATIN SMALL LETTER G WITH CIRCUMFLEX
          (#x02fd "ubreve") ;U+016D LATIN SMALL LETTER U WITH BREVE
          (#x02fe "scircumflex") ;U+015D LATIN SMALL LETTER S WITH CIRCUMFLEX
          (#x03a2 "kra")            ;U+0138 LATIN SMALL LETTER KRA
          (#x03a2 "kappa")          ;deprecated
          (#x03a3 "Rcedilla") ;U+0156 LATIN CAPITAL LETTER R WITH CEDILLA
          (#x03a5 "Itilde") ;U+0128 LATIN CAPITAL LETTER I WITH TILDE
          (#x03a6 "Lcedilla") ;U+013B LATIN CAPITAL LETTER L WITH CEDILLA
          (#x03aa "Emacron") ;U+0112 LATIN CAPITAL LETTER E WITH MACRON
          (#x03ab "Gcedilla") ;U+0122 LATIN CAPITAL LETTER G WITH CEDILLA
          (#x03ac "Tslash") ;U+0166 LATIN CAPITAL LETTER T WITH STROKE
          (#x03b3 "rcedilla") ;U+0157 LATIN SMALL LETTER R WITH CEDILLA
          (#x03b5 "itilde") ;U+0129 LATIN SMALL LETTER I WITH TILDE
          (#x03b6 "lcedilla") ;U+013C LATIN SMALL LETTER L WITH CEDILLA
          (#x03ba "emacron") ;U+0113 LATIN SMALL LETTER E WITH MACRON
          (#x03bb "gcedilla") ;U+0123 LATIN SMALL LETTER G WITH CEDILLA
          (#x03bc "tslash") ;U+0167 LATIN SMALL LETTER T WITH STROKE
          (#x03bd "ENG")          ;U+014A LATIN CAPITAL LETTER ENG
          (#x03bf "eng")            ;U+014B LATIN SMALL LETTER ENG
          (#x03c0 "Amacron") ;U+0100 LATIN CAPITAL LETTER A WITH MACRON
          (#x03c7 "Iogonek") ;U+012E LATIN CAPITAL LETTER I WITH OGONEK
          (#x03cc "Eabovedot") ;U+0116 LATIN CAPITAL LETTER E WITH DOT ABOVE
          (#x03cf "Imacron") ;U+012A LATIN CAPITAL LETTER I WITH MACRON
          (#x03d1 "Ncedilla") ;U+0145 LATIN CAPITAL LETTER N WITH CEDILLA
          (#x03d2 "Omacron") ;U+014C LATIN CAPITAL LETTER O WITH MACRON
          (#x03d3 "Kcedilla") ;U+0136 LATIN CAPITAL LETTER K WITH CEDILLA
          (#x03d9 "Uogonek") ;U+0172 LATIN CAPITAL LETTER U WITH OGONEK
          (#x03dd "Utilde") ;U+0168 LATIN CAPITAL LETTER U WITH TILDE
          (#x03de "Umacron") ;U+016A LATIN CAPITAL LETTER U WITH MACRON
          (#x03e0 "amacron") ;U+0101 LATIN SMALL LETTER A WITH MACRON
          (#x03e7 "iogonek") ;U+012F LATIN SMALL LETTER I WITH OGONEK
          (#x03ec "eabovedot") ;U+0117 LATIN SMALL LETTER E WITH DOT ABOVE
          (#x03ef "imacron") ;U+012B LATIN SMALL LETTER I WITH MACRON
          (#x03f1 "ncedilla") ;U+0146 LATIN SMALL LETTER N WITH CEDILLA
          (#x03f2 "omacron") ;U+014D LATIN SMALL LETTER O WITH MACRON
          (#x03f3 "kcedilla") ;U+0137 LATIN SMALL LETTER K WITH CEDILLA
          (#x03f9 "uogonek") ;U+0173 LATIN SMALL LETTER U WITH OGONEK
          (#x03fd "utilde") ;U+0169 LATIN SMALL LETTER U WITH TILDE
          (#x03fe "umacron") ;U+016B LATIN SMALL LETTER U WITH MACRON
          (#x1001e02 "Babovedot") ;U+1E02 LATIN CAPITAL LETTER B WITH DOT ABOVE
          (#x1001e03 "babovedot") ;U+1E03 LATIN SMALL LETTER B WITH DOT ABOVE
          (#x1001e0a "Dabovedot") ;U+1E0A LATIN CAPITAL LETTER D WITH DOT ABOVE
          (#x1001e80 "Wgrave") ;U+1E80 LATIN CAPITAL LETTER W WITH GRAVE
          (#x1001e82 "Wacute") ;U+1E82 LATIN CAPITAL LETTER W WITH ACUTE
          (#x1001e0b "dabovedot") ;U+1E0B LATIN SMALL LETTER D WITH DOT ABOVE
          (#x1001ef2 "Ygrave") ;U+1EF2 LATIN CAPITAL LETTER Y WITH GRAVE
          (#x1001e1e "Fabovedot") ;U+1E1E LATIN CAPITAL LETTER F WITH DOT ABOVE
          (#x1001e1f "fabovedot") ;U+1E1F LATIN SMALL LETTER F WITH DOT ABOVE
          (#x1001e40 "Mabovedot") ;U+1E40 LATIN CAPITAL LETTER M WITH DOT ABOVE
          (#x1001e41 "mabovedot") ;U+1E41 LATIN SMALL LETTER M WITH DOT ABOVE
          (#x1001e56 "Pabovedot") ;U+1E56 LATIN CAPITAL LETTER P WITH DOT ABOVE
          (#x1001e81 "wgrave") ;U+1E81 LATIN SMALL LETTER W WITH GRAVE
          (#x1001e57 "pabovedot") ;U+1E57 LATIN SMALL LETTER P WITH DOT ABOVE
          (#x1001e83 "wacute") ;U+1E83 LATIN SMALL LETTER W WITH ACUTE
          (#x1001e60 "Sabovedot") ;U+1E60 LATIN CAPITAL LETTER S WITH DOT ABOVE
          (#x1001ef3 "ygrave") ;U+1EF3 LATIN SMALL LETTER Y WITH GRAVE
          (#x1001e84 "Wdiaeresis") ;U+1E84 LATIN CAPITAL LETTER W WITH DIAERESIS
          (#x1001e85 "wdiaeresis") ;U+1E85 LATIN SMALL LETTER W WITH DIAERESIS
          (#x1001e61 "sabovedot") ;U+1E61 LATIN SMALL LETTER S WITH DOT ABOVE
          (#x1000174 "Wcircumflex") ;U+0174 LATIN CAPITAL LETTER W WITH CIRCUMFLEX
          (#x1001e6a "Tabovedot") ;U+1E6A LATIN CAPITAL LETTER T WITH DOT ABOVE
          (#x1000176 "Ycircumflex") ;U+0176 LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
          (#x1000175 "wcircumflex") ;U+0175 LATIN SMALL LETTER W WITH CIRCUMFLEX
          (#x1001e6b "tabovedot") ;U+1E6B LATIN SMALL LETTER T WITH DOT ABOVE
          (#x1000177 "ycircumflex") ;U+0177 LATIN SMALL LETTER Y WITH CIRCUMFLEX
          (#x13bc "OE")          ;U+0152 LATIN CAPITAL LIGATURE OE
          (#x13bd "oe")            ;U+0153 LATIN SMALL LIGATURE OE
          (#x13be "Ydiaeresis") ;U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS
          (#x047e "overline")       ;U+203E OVERLINE
          (#x04a1 "kana_fullstop")  ;U+3002 IDEOGRAPHIC FULL STOP
          (#x04a2 "kana_openingbracket") ;U+300C LEFT CORNER BRACKET
          (#x04a3 "kana_closingbracket") ;U+300D RIGHT CORNER BRACKET
          (#x04a4 "kana_comma")     ;U+3001 IDEOGRAPHIC COMMA
          (#x04a5 "kana_conjunctive") ;U+30FB KATAKANA MIDDLE DOT
          (#x04a5 "kana_middledot") ;deprecated
          (#x04a6 "kana_WO")        ;U+30F2 KATAKANA LETTER WO
          (#x04a7 "kana_a")        ;U+30A1 KATAKANA LETTER SMALL A
          (#x04a8 "kana_i")        ;U+30A3 KATAKANA LETTER SMALL I
          (#x04a9 "kana_u")        ;U+30A5 KATAKANA LETTER SMALL U
          (#x04aa "kana_e")        ;U+30A7 KATAKANA LETTER SMALL E
          (#x04ab "kana_o")        ;U+30A9 KATAKANA LETTER SMALL O
          (#x04ac "kana_ya")      ;U+30E3 KATAKANA LETTER SMALL YA
          (#x04ad "kana_yu")      ;U+30E5 KATAKANA LETTER SMALL YU
          (#x04ae "kana_yo")      ;U+30E7 KATAKANA LETTER SMALL YO
          (#x04af "kana_tsu")     ;U+30C3 KATAKANA LETTER SMALL TU
          (#x04af "kana_tu")        ;deprecated
          (#x04b0 "prolongedsound") ;U+30FC KATAKANA-HIRAGANA PROLONGED SOUND MARK
          (#x04b1 "kana_A")         ;U+30A2 KATAKANA LETTER A
          (#x04b2 "kana_I")         ;U+30A4 KATAKANA LETTER I
          (#x04b3 "kana_U")         ;U+30A6 KATAKANA LETTER U
          (#x04b4 "kana_E")         ;U+30A8 KATAKANA LETTER E
          (#x04b5 "kana_O")         ;U+30AA KATAKANA LETTER O
          (#x04b6 "kana_KA")        ;U+30AB KATAKANA LETTER KA
          (#x04b7 "kana_KI")        ;U+30AD KATAKANA LETTER KI
          (#x04b8 "kana_KU")        ;U+30AF KATAKANA LETTER KU
          (#x04b9 "kana_KE")        ;U+30B1 KATAKANA LETTER KE
          (#x04ba "kana_KO")        ;U+30B3 KATAKANA LETTER KO
          (#x04bb "kana_SA")        ;U+30B5 KATAKANA LETTER SA
          (#x04bc "kana_SHI")       ;U+30B7 KATAKANA LETTER SI
          (#x04bd "kana_SU")        ;U+30B9 KATAKANA LETTER SU
          (#x04be "kana_SE")        ;U+30BB KATAKANA LETTER SE
          (#x04bf "kana_SO")        ;U+30BD KATAKANA LETTER SO
          (#x04c0 "kana_TA")        ;U+30BF KATAKANA LETTER TA
          (#x04c1 "kana_CHI")       ;U+30C1 KATAKANA LETTER TI
          (#x04c1 "kana_TI")        ;deprecated
          (#x04c2 "kana_TSU")       ;U+30C4 KATAKANA LETTER TU
          (#x04c2 "kana_TU")        ;deprecated
          (#x04c3 "kana_TE")        ;U+30C6 KATAKANA LETTER TE
          (#x04c4 "kana_TO")        ;U+30C8 KATAKANA LETTER TO
          (#x04c5 "kana_NA")        ;U+30CA KATAKANA LETTER NA
          (#x04c6 "kana_NI")        ;U+30CB KATAKANA LETTER NI
          (#x04c7 "kana_NU")        ;U+30CC KATAKANA LETTER NU
          (#x04c8 "kana_NE")        ;U+30CD KATAKANA LETTER NE
          (#x04c9 "kana_NO")        ;U+30CE KATAKANA LETTER NO
          (#x04ca "kana_HA")        ;U+30CF KATAKANA LETTER HA
          (#x04cb "kana_HI")        ;U+30D2 KATAKANA LETTER HI
          (#x04cc "kana_FU")        ;U+30D5 KATAKANA LETTER HU
          (#x04cc "kana_HU")        ;deprecated
          (#x04cd "kana_HE")        ;U+30D8 KATAKANA LETTER HE
          (#x04ce "kana_HO")        ;U+30DB KATAKANA LETTER HO
          (#x04cf "kana_MA")        ;U+30DE KATAKANA LETTER MA
          (#x04d0 "kana_MI")        ;U+30DF KATAKANA LETTER MI
          (#x04d1 "kana_MU")        ;U+30E0 KATAKANA LETTER MU
          (#x04d2 "kana_ME")        ;U+30E1 KATAKANA LETTER ME
          (#x04d3 "kana_MO")        ;U+30E2 KATAKANA LETTER MO
          (#x04d4 "kana_YA")        ;U+30E4 KATAKANA LETTER YA
          (#x04d5 "kana_YU")        ;U+30E6 KATAKANA LETTER YU
          (#x04d6 "kana_YO")        ;U+30E8 KATAKANA LETTER YO
          (#x04d7 "kana_RA")        ;U+30E9 KATAKANA LETTER RA
          (#x04d8 "kana_RI")        ;U+30EA KATAKANA LETTER RI
          (#x04d9 "kana_RU")        ;U+30EB KATAKANA LETTER RU
          (#x04da "kana_RE")        ;U+30EC KATAKANA LETTER RE
          (#x04db "kana_RO")        ;U+30ED KATAKANA LETTER RO
          (#x04dc "kana_WA")        ;U+30EF KATAKANA LETTER WA
          (#x04dd "kana_N")         ;U+30F3 KATAKANA LETTER N
          (#x04de "voicedsound") ;U+309B KATAKANA-HIRAGANA VOICED SOUND MARK
          (#x04df "semivoicedsound") ;U+309C KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
          (#xff7e "kana_switch")    ;Alias for mode_switch
          (#x10006f0 "Farsi_0") ;U+06F0 EXTENDED ARABIC-INDIC DIGIT ZERO
          (#x10006f1 "Farsi_1") ;U+06F1 EXTENDED ARABIC-INDIC DIGIT ONE
          (#x10006f2 "Farsi_2") ;U+06F2 EXTENDED ARABIC-INDIC DIGIT TWO
          (#x10006f3 "Farsi_3") ;U+06F3 EXTENDED ARABIC-INDIC DIGIT THREE
          (#x10006f4 "Farsi_4") ;U+06F4 EXTENDED ARABIC-INDIC DIGIT FOUR
          (#x10006f5 "Farsi_5") ;U+06F5 EXTENDED ARABIC-INDIC DIGIT FIVE
          (#x10006f6 "Farsi_6") ;U+06F6 EXTENDED ARABIC-INDIC DIGIT SIX
          (#x10006f7 "Farsi_7") ;U+06F7 EXTENDED ARABIC-INDIC DIGIT SEVEN
          (#x10006f8 "Farsi_8") ;U+06F8 EXTENDED ARABIC-INDIC DIGIT EIGHT
          (#x10006f9 "Farsi_9") ;U+06F9 EXTENDED ARABIC-INDIC DIGIT NINE
          (#x100066a "Arabic_percent") ;U+066A ARABIC PERCENT SIGN
          (#x1000670 "Arabic_superscript_alef") ;U+0670 ARABIC LETTER SUPERSCRIPT ALEF
          (#x1000679 "Arabic_tteh") ;U+0679 ARABIC LETTER TTEH
          (#x100067e "Arabic_peh")  ;U+067E ARABIC LETTER PEH
          (#x1000686 "Arabic_tcheh") ;U+0686 ARABIC LETTER TCHEH
          (#x1000688 "Arabic_ddal") ;U+0688 ARABIC LETTER DDAL
          (#x1000691 "Arabic_rreh") ;U+0691 ARABIC LETTER RREH
          (#x05ac "Arabic_comma")   ;U+060C ARABIC COMMA
          (#x10006d4 "Arabic_fullstop") ;U+06D4 ARABIC FULL STOP
          (#x1000660 "Arabic_0")   ;U+0660 ARABIC-INDIC DIGIT ZERO
          (#x1000661 "Arabic_1")    ;U+0661 ARABIC-INDIC DIGIT ONE
          (#x1000662 "Arabic_2")    ;U+0662 ARABIC-INDIC DIGIT TWO
          (#x1000663 "Arabic_3")  ;U+0663 ARABIC-INDIC DIGIT THREE
          (#x1000664 "Arabic_4")   ;U+0664 ARABIC-INDIC DIGIT FOUR
          (#x1000665 "Arabic_5")   ;U+0665 ARABIC-INDIC DIGIT FIVE
          (#x1000666 "Arabic_6")    ;U+0666 ARABIC-INDIC DIGIT SIX
          (#x1000667 "Arabic_7")  ;U+0667 ARABIC-INDIC DIGIT SEVEN
          (#x1000668 "Arabic_8")  ;U+0668 ARABIC-INDIC DIGIT EIGHT
          (#x1000669 "Arabic_9")   ;U+0669 ARABIC-INDIC DIGIT NINE
          (#x05bb "Arabic_semicolon") ;U+061B ARABIC SEMICOLON
          (#x05bf "Arabic_question_mark") ;U+061F ARABIC QUESTION MARK
          (#x05c1 "Arabic_hamza")   ;U+0621 ARABIC LETTER HAMZA
          (#x05c2 "Arabic_maddaonalef") ;U+0622 ARABIC LETTER ALEF WITH MADDA ABOVE
          (#x05c3 "Arabic_hamzaonalef") ;U+0623 ARABIC LETTER ALEF WITH HAMZA ABOVE
          (#x05c4 "Arabic_hamzaonwaw") ;U+0624 ARABIC LETTER WAW WITH HAMZA ABOVE
          (#x05c5 "Arabic_hamzaunderalef") ;U+0625 ARABIC LETTER ALEF WITH HAMZA BELOW
          (#x05c6 "Arabic_hamzaonyeh") ;U+0626 ARABIC LETTER YEH WITH HAMZA ABOVE
          (#x05c7 "Arabic_alef")    ;U+0627 ARABIC LETTER ALEF
          (#x05c8 "Arabic_beh")     ;U+0628 ARABIC LETTER BEH
          (#x05c9 "Arabic_tehmarbuta") ;U+0629 ARABIC LETTER TEH MARBUTA
          (#x05ca "Arabic_teh")     ;U+062A ARABIC LETTER TEH
          (#x05cb "Arabic_theh")    ;U+062B ARABIC LETTER THEH
          (#x05cc "Arabic_jeem")    ;U+062C ARABIC LETTER JEEM
          (#x05cd "Arabic_hah")     ;U+062D ARABIC LETTER HAH
          (#x05ce "Arabic_khah")    ;U+062E ARABIC LETTER KHAH
          (#x05cf "Arabic_dal")     ;U+062F ARABIC LETTER DAL
          (#x05d0 "Arabic_thal")    ;U+0630 ARABIC LETTER THAL
          (#x05d1 "Arabic_ra")      ;U+0631 ARABIC LETTER REH
          (#x05d2 "Arabic_zain")    ;U+0632 ARABIC LETTER ZAIN
          (#x05d3 "Arabic_seen")    ;U+0633 ARABIC LETTER SEEN
          (#x05d4 "Arabic_sheen")   ;U+0634 ARABIC LETTER SHEEN
          (#x05d5 "Arabic_sad")     ;U+0635 ARABIC LETTER SAD
          (#x05d6 "Arabic_dad")     ;U+0636 ARABIC LETTER DAD
          (#x05d7 "Arabic_tah")     ;U+0637 ARABIC LETTER TAH
          (#x05d8 "Arabic_zah")     ;U+0638 ARABIC LETTER ZAH
          (#x05d9 "Arabic_ain")     ;U+0639 ARABIC LETTER AIN
          (#x05da "Arabic_ghain")   ;U+063A ARABIC LETTER GHAIN
          (#x05e0 "Arabic_tatweel") ;U+0640 ARABIC TATWEEL
          (#x05e1 "Arabic_feh")     ;U+0641 ARABIC LETTER FEH
          (#x05e2 "Arabic_qaf")     ;U+0642 ARABIC LETTER QAF
          (#x05e3 "Arabic_kaf")     ;U+0643 ARABIC LETTER KAF
          (#x05e4 "Arabic_lam")     ;U+0644 ARABIC LETTER LAM
          (#x05e5 "Arabic_meem")    ;U+0645 ARABIC LETTER MEEM
          (#x05e6 "Arabic_noon")    ;U+0646 ARABIC LETTER NOON
          (#x05e7 "Arabic_ha")      ;U+0647 ARABIC LETTER HEH
          (#x05e7 "Arabic_heh")     ;deprecated
          (#x05e8 "Arabic_waw")     ;U+0648 ARABIC LETTER WAW
          (#x05e9 "Arabic_alefmaksura") ;U+0649 ARABIC LETTER ALEF MAKSURA
          (#x05ea "Arabic_yeh")     ;U+064A ARABIC LETTER YEH
          (#x05eb "Arabic_fathatan") ;U+064B ARABIC FATHATAN
          (#x05ec "Arabic_dammatan") ;U+064C ARABIC DAMMATAN
          (#x05ed "Arabic_kasratan") ;U+064D ARABIC KASRATAN
          (#x05ee "Arabic_fatha")   ;U+064E ARABIC FATHA
          (#x05ef "Arabic_damma")   ;U+064F ARABIC DAMMA
          (#x05f0 "Arabic_kasra")   ;U+0650 ARABIC KASRA
          (#x05f1 "Arabic_shadda")  ;U+0651 ARABIC SHADDA
          (#x05f2 "Arabic_sukun")   ;U+0652 ARABIC SUKUN
          (#x1000653 "Arabic_madda_above") ;U+0653 ARABIC MADDAH ABOVE
          (#x1000654 "Arabic_hamza_above") ;U+0654 ARABIC HAMZA ABOVE
          (#x1000655 "Arabic_hamza_below") ;U+0655 ARABIC HAMZA BELOW
          (#x1000698 "Arabic_jeh")  ;U+0698 ARABIC LETTER JEH
          (#x10006a4 "Arabic_veh")  ;U+06A4 ARABIC LETTER VEH
          (#x10006a9 "Arabic_keheh") ;U+06A9 ARABIC LETTER KEHEH
          (#x10006af "Arabic_gaf")  ;U+06AF ARABIC LETTER GAF
          (#x10006ba "Arabic_noon_ghunna") ;U+06BA ARABIC LETTER NOON GHUNNA
          (#x10006be "Arabic_heh_doachashmee") ;U+06BE ARABIC LETTER HEH DOACHASHMEE
          (#x10006cc "Farsi_yeh")  ;U+06CC ARABIC LETTER FARSI YEH
          (#x10006cc "Arabic_farsi_yeh") ;U+06CC ARABIC LETTER FARSI YEH
          (#x10006d2 "Arabic_yeh_baree") ;U+06D2 ARABIC LETTER YEH BARREE
          (#x10006c1 "Arabic_heh_goal") ;U+06C1 ARABIC LETTER HEH GOAL
          (#xff7e "Arabic_switch")  ;Alias for mode_switch
          (#x1000492 "Cyrillic_GHE_bar") ;U+0492 CYRILLIC CAPITAL LETTER GHE WITH STROKE
          (#x1000493 "Cyrillic_ghe_bar") ;U+0493 CYRILLIC SMALL LETTER GHE WITH STROKE
          (#x1000496 "Cyrillic_ZHE_descender") ;U+0496 CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
          (#x1000497 "Cyrillic_zhe_descender") ;U+0497 CYRILLIC SMALL LETTER ZHE WITH DESCENDER
          (#x100049a "Cyrillic_KA_descender") ;U+049A CYRILLIC CAPITAL LETTER KA WITH DESCENDER
          (#x100049b "Cyrillic_ka_descender") ;U+049B CYRILLIC SMALL LETTER KA WITH DESCENDER
          (#x100049c "Cyrillic_KA_vertstroke") ;U+049C CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
          (#x100049d "Cyrillic_ka_vertstroke") ;U+049D CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE
          (#x10004a2 "Cyrillic_EN_descender") ;U+04A2 CYRILLIC CAPITAL LETTER EN WITH DESCENDER
          (#x10004a3 "Cyrillic_en_descender") ;U+04A3 CYRILLIC SMALL LETTER EN WITH DESCENDER
          (#x10004ae "Cyrillic_U_straight") ;U+04AE CYRILLIC CAPITAL LETTER STRAIGHT U
          (#x10004af "Cyrillic_u_straight") ;U+04AF CYRILLIC SMALL LETTER STRAIGHT U
          (#x10004b0 "Cyrillic_U_straight_bar") ;U+04B0 CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
          (#x10004b1 "Cyrillic_u_straight_bar") ;U+04B1 CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE
          (#x10004b2 "Cyrillic_HA_descender") ;U+04B2 CYRILLIC CAPITAL LETTER HA WITH DESCENDER
          (#x10004b3 "Cyrillic_ha_descender") ;U+04B3 CYRILLIC SMALL LETTER HA WITH DESCENDER
          (#x10004b6 "Cyrillic_CHE_descender") ;U+04B6 CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
          (#x10004b7 "Cyrillic_che_descender") ;U+04B7 CYRILLIC SMALL LETTER CHE WITH DESCENDER
          (#x10004b8 "Cyrillic_CHE_vertstroke") ;U+04B8 CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
          (#x10004b9 "Cyrillic_che_vertstroke") ;U+04B9 CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE
          (#x10004ba "Cyrillic_SHHA") ;U+04BA CYRILLIC CAPITAL LETTER SHHA
          (#x10004bb "Cyrillic_shha") ;U+04BB CYRILLIC SMALL LETTER SHHA
          (#x10004d8 "Cyrillic_SCHWA") ;U+04D8 CYRILLIC CAPITAL LETTER SCHWA
          (#x10004d9 "Cyrillic_schwa") ;U+04D9 CYRILLIC SMALL LETTER SCHWA
          (#x10004e2 "Cyrillic_I_macron") ;U+04E2 CYRILLIC CAPITAL LETTER I WITH MACRON
          (#x10004e3 "Cyrillic_i_macron") ;U+04E3 CYRILLIC SMALL LETTER I WITH MACRON
          (#x10004e8 "Cyrillic_O_bar") ;U+04E8 CYRILLIC CAPITAL LETTER BARRED O
          (#x10004e9 "Cyrillic_o_bar") ;U+04E9 CYRILLIC SMALL LETTER BARRED O
          (#x10004ee "Cyrillic_U_macron") ;U+04EE CYRILLIC CAPITAL LETTER U WITH MACRON
          (#x10004ef "Cyrillic_u_macron") ;U+04EF CYRILLIC SMALL LETTER U WITH MACRON
          (#x06a1 "Serbian_dje") ;U+0452 CYRILLIC SMALL LETTER DJE
          (#x06a2 "Macedonia_gje") ;U+0453 CYRILLIC SMALL LETTER GJE
          (#x06a3 "Cyrillic_io")  ;U+0451 CYRILLIC SMALL LETTER IO
          (#x06a4 "Ukrainian_ie") ;U+0454 CYRILLIC SMALL LETTER UKRAINIAN IE
          (#x06a4 "Ukranian_je")    ;deprecated
          (#x06a5 "Macedonia_dse") ;U+0455 CYRILLIC SMALL LETTER DZE
          (#x06a6 "Ukrainian_i") ;U+0456 CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
          (#x06a6 "Ukranian_i")     ;deprecated
          (#x06a7 "Ukrainian_yi") ;U+0457 CYRILLIC SMALL LETTER YI
          (#x06a7 "Ukranian_yi")    ;deprecated
          (#x06a8 "Cyrillic_je")  ;U+0458 CYRILLIC SMALL LETTER JE
          (#x06a8 "Serbian_je")     ;deprecated
          (#x06a9 "Cyrillic_lje") ;U+0459 CYRILLIC SMALL LETTER LJE
          (#x06a9 "Serbian_lje")    ;deprecated
          (#x06aa "Cyrillic_nje") ;U+045A CYRILLIC SMALL LETTER NJE
          (#x06aa "Serbian_nje")    ;deprecated
          (#x06ab "Serbian_tshe") ;U+045B CYRILLIC SMALL LETTER TSHE
          (#x06ac "Macedonia_kje") ;U+045C CYRILLIC SMALL LETTER KJE
          (#x06ad "Ukrainian_ghe_with_upturn") ;U+0491 CYRILLIC SMALL LETTER GHE WITH UPTURN
          (#x06ae "Byelorussian_shortu") ;U+045E CYRILLIC SMALL LETTER SHORT U
          (#x06af "Cyrillic_dzhe") ;U+045F CYRILLIC SMALL LETTER DZHE
          (#x06af "Serbian_dze")    ;deprecated
          (#x06b0 "numerosign")     ;U+2116 NUMERO SIGN
          (#x06b1 "Serbian_DJE") ;U+0402 CYRILLIC CAPITAL LETTER DJE
          (#x06b2 "Macedonia_GJE") ;U+0403 CYRILLIC CAPITAL LETTER GJE
          (#x06b3 "Cyrillic_IO") ;U+0401 CYRILLIC CAPITAL LETTER IO
          (#x06b4 "Ukrainian_IE") ;U+0404 CYRILLIC CAPITAL LETTER UKRAINIAN IE
          (#x06b4 "Ukranian_JE")    ;deprecated
          (#x06b5 "Macedonia_DSE") ;U+0405 CYRILLIC CAPITAL LETTER DZE
          (#x06b6 "Ukrainian_I") ;U+0406 CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
          (#x06b6 "Ukranian_I")     ;deprecated
          (#x06b7 "Ukrainian_YI") ;U+0407 CYRILLIC CAPITAL LETTER YI
          (#x06b7 "Ukranian_YI")    ;deprecated
          (#x06b8 "Cyrillic_JE") ;U+0408 CYRILLIC CAPITAL LETTER JE
          (#x06b8 "Serbian_JE")     ;deprecated
          (#x06b9 "Cyrillic_LJE") ;U+0409 CYRILLIC CAPITAL LETTER LJE
          (#x06b9 "Serbian_LJE")    ;deprecated
          (#x06ba "Cyrillic_NJE") ;U+040A CYRILLIC CAPITAL LETTER NJE
          (#x06ba "Serbian_NJE")    ;deprecated
          (#x06bb "Serbian_TSHE") ;U+040B CYRILLIC CAPITAL LETTER TSHE
          (#x06bc "Macedonia_KJE") ;U+040C CYRILLIC CAPITAL LETTER KJE
          (#x06bd "Ukrainian_GHE_WITH_UPTURN") ;U+0490 CYRILLIC CAPITAL LETTER GHE WITH UPTURN
          (#x06be "Byelorussian_SHORTU") ;U+040E CYRILLIC CAPITAL LETTER SHORT U
          (#x06bf "Cyrillic_DZHE") ;U+040F CYRILLIC CAPITAL LETTER DZHE
          (#x06bf "Serbian_DZE")    ;deprecated
          (#x06c0 "Cyrillic_yu")  ;U+044E CYRILLIC SMALL LETTER YU
          (#x06c1 "Cyrillic_a")    ;U+0430 CYRILLIC SMALL LETTER A
          (#x06c2 "Cyrillic_be")  ;U+0431 CYRILLIC SMALL LETTER BE
          (#x06c3 "Cyrillic_tse") ;U+0446 CYRILLIC SMALL LETTER TSE
          (#x06c4 "Cyrillic_de")  ;U+0434 CYRILLIC SMALL LETTER DE
          (#x06c5 "Cyrillic_ie")  ;U+0435 CYRILLIC SMALL LETTER IE
          (#x06c6 "Cyrillic_ef")  ;U+0444 CYRILLIC SMALL LETTER EF
          (#x06c7 "Cyrillic_ghe") ;U+0433 CYRILLIC SMALL LETTER GHE
          (#x06c8 "Cyrillic_ha")  ;U+0445 CYRILLIC SMALL LETTER HA
          (#x06c9 "Cyrillic_i")    ;U+0438 CYRILLIC SMALL LETTER I
          (#x06ca "Cyrillic_shorti") ;U+0439 CYRILLIC SMALL LETTER SHORT I
          (#x06cb "Cyrillic_ka")  ;U+043A CYRILLIC SMALL LETTER KA
          (#x06cc "Cyrillic_el")  ;U+043B CYRILLIC SMALL LETTER EL
          (#x06cd "Cyrillic_em")  ;U+043C CYRILLIC SMALL LETTER EM
          (#x06ce "Cyrillic_en")  ;U+043D CYRILLIC SMALL LETTER EN
          (#x06cf "Cyrillic_o")    ;U+043E CYRILLIC SMALL LETTER O
          (#x06d0 "Cyrillic_pe")  ;U+043F CYRILLIC SMALL LETTER PE
          (#x06d1 "Cyrillic_ya")  ;U+044F CYRILLIC SMALL LETTER YA
          (#x06d2 "Cyrillic_er")  ;U+0440 CYRILLIC SMALL LETTER ER
          (#x06d3 "Cyrillic_es")  ;U+0441 CYRILLIC SMALL LETTER ES
          (#x06d4 "Cyrillic_te")  ;U+0442 CYRILLIC SMALL LETTER TE
          (#x06d5 "Cyrillic_u")    ;U+0443 CYRILLIC SMALL LETTER U
          (#x06d6 "Cyrillic_zhe") ;U+0436 CYRILLIC SMALL LETTER ZHE
          (#x06d7 "Cyrillic_ve")  ;U+0432 CYRILLIC SMALL LETTER VE
          (#x06d8 "Cyrillic_softsign") ;U+044C CYRILLIC SMALL LETTER SOFT SIGN
          (#x06d9 "Cyrillic_yeru") ;U+044B CYRILLIC SMALL LETTER YERU
          (#x06da "Cyrillic_ze")  ;U+0437 CYRILLIC SMALL LETTER ZE
          (#x06db "Cyrillic_sha") ;U+0448 CYRILLIC SMALL LETTER SHA
          (#x06dc "Cyrillic_e")    ;U+044D CYRILLIC SMALL LETTER E
          (#x06dd "Cyrillic_shcha") ;U+0449 CYRILLIC SMALL LETTER SHCHA
          (#x06de "Cyrillic_che") ;U+0447 CYRILLIC SMALL LETTER CHE
          (#x06df "Cyrillic_hardsign") ;U+044A CYRILLIC SMALL LETTER HARD SIGN
          (#x06e0 "Cyrillic_YU") ;U+042E CYRILLIC CAPITAL LETTER YU
          (#x06e1 "Cyrillic_A")  ;U+0410 CYRILLIC CAPITAL LETTER A
          (#x06e2 "Cyrillic_BE") ;U+0411 CYRILLIC CAPITAL LETTER BE
          (#x06e3 "Cyrillic_TSE") ;U+0426 CYRILLIC CAPITAL LETTER TSE
          (#x06e4 "Cyrillic_DE") ;U+0414 CYRILLIC CAPITAL LETTER DE
          (#x06e5 "Cyrillic_IE") ;U+0415 CYRILLIC CAPITAL LETTER IE
          (#x06e6 "Cyrillic_EF") ;U+0424 CYRILLIC CAPITAL LETTER EF
          (#x06e7 "Cyrillic_GHE") ;U+0413 CYRILLIC CAPITAL LETTER GHE
          (#x06e8 "Cyrillic_HA") ;U+0425 CYRILLIC CAPITAL LETTER HA
          (#x06e9 "Cyrillic_I")  ;U+0418 CYRILLIC CAPITAL LETTER I
          (#x06ea "Cyrillic_SHORTI") ;U+0419 CYRILLIC CAPITAL LETTER SHORT I
          (#x06eb "Cyrillic_KA") ;U+041A CYRILLIC CAPITAL LETTER KA
          (#x06ec "Cyrillic_EL") ;U+041B CYRILLIC CAPITAL LETTER EL
          (#x06ed "Cyrillic_EM") ;U+041C CYRILLIC CAPITAL LETTER EM
          (#x06ee "Cyrillic_EN") ;U+041D CYRILLIC CAPITAL LETTER EN
          (#x06ef "Cyrillic_O")  ;U+041E CYRILLIC CAPITAL LETTER O
          (#x06f0 "Cyrillic_PE") ;U+041F CYRILLIC CAPITAL LETTER PE
          (#x06f1 "Cyrillic_YA") ;U+042F CYRILLIC CAPITAL LETTER YA
          (#x06f2 "Cyrillic_ER") ;U+0420 CYRILLIC CAPITAL LETTER ER
          (#x06f3 "Cyrillic_ES") ;U+0421 CYRILLIC CAPITAL LETTER ES
          (#x06f4 "Cyrillic_TE") ;U+0422 CYRILLIC CAPITAL LETTER TE
          (#x06f5 "Cyrillic_U")  ;U+0423 CYRILLIC CAPITAL LETTER U
          (#x06f6 "Cyrillic_ZHE") ;U+0416 CYRILLIC CAPITAL LETTER ZHE
          (#x06f7 "Cyrillic_VE") ;U+0412 CYRILLIC CAPITAL LETTER VE
          (#x06f8 "Cyrillic_SOFTSIGN") ;U+042C CYRILLIC CAPITAL LETTER SOFT SIGN
          (#x06f9 "Cyrillic_YERU") ;U+042B CYRILLIC CAPITAL LETTER YERU
          (#x06fa "Cyrillic_ZE") ;U+0417 CYRILLIC CAPITAL LETTER ZE
          (#x06fb "Cyrillic_SHA") ;U+0428 CYRILLIC CAPITAL LETTER SHA
          (#x06fc "Cyrillic_E")  ;U+042D CYRILLIC CAPITAL LETTER E
          (#x06fd "Cyrillic_SHCHA") ;U+0429 CYRILLIC CAPITAL LETTER SHCHA
          (#x06fe "Cyrillic_CHE") ;U+0427 CYRILLIC CAPITAL LETTER CHE
          (#x06ff "Cyrillic_HARDSIGN") ;U+042A CYRILLIC CAPITAL LETTER HARD SIGN
          (#x07a1 "Greek_ALPHAaccent") ;U+0386 GREEK CAPITAL LETTER ALPHA WITH TONOS
          (#x07a2 "Greek_EPSILONaccent") ;U+0388 GREEK CAPITAL LETTER EPSILON WITH TONOS
          (#x07a3 "Greek_ETAaccent") ;U+0389 GREEK CAPITAL LETTER ETA WITH TONOS
          (#x07a4 "Greek_IOTAaccent") ;U+038A GREEK CAPITAL LETTER IOTA WITH TONOS
          (#x07a5 "Greek_IOTAdieresis") ;U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
          (#x07a5 "Greek_IOTAdiaeresis") ;old typo
          (#x07a7 "Greek_OMICRONaccent") ;U+038C GREEK CAPITAL LETTER OMICRON WITH TONOS
          (#x07a8 "Greek_UPSILONaccent") ;U+038E GREEK CAPITAL LETTER UPSILON WITH TONOS
          (#x07a9 "Greek_UPSILONdieresis") ;U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
          (#x07ab "Greek_OMEGAaccent") ;U+038F GREEK CAPITAL LETTER OMEGA WITH TONOS
          (#x07ae "Greek_accentdieresis") ;U+0385 GREEK DIALYTIKA TONOS
          (#x07af "Greek_horizbar") ;U+2015 HORIZONTAL BAR
          (#x07b1 "Greek_alphaaccent") ;U+03AC GREEK SMALL LETTER ALPHA WITH TONOS
          (#x07b2 "Greek_epsilonaccent") ;U+03AD GREEK SMALL LETTER EPSILON WITH TONOS
          (#x07b3 "Greek_etaaccent") ;U+03AE GREEK SMALL LETTER ETA WITH TONOS
          (#x07b4 "Greek_iotaaccent") ;U+03AF GREEK SMALL LETTER IOTA WITH TONOS
          (#x07b5 "Greek_iotadieresis") ;U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA
          (#x07b6 "Greek_iotaaccentdieresis") ;U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
          (#x07b7 "Greek_omicronaccent") ;U+03CC GREEK SMALL LETTER OMICRON WITH TONOS
          (#x07b8 "Greek_upsilonaccent") ;U+03CD GREEK SMALL LETTER UPSILON WITH TONOS
          (#x07b9 "Greek_upsilondieresis") ;U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA
          (#x07ba "Greek_upsilonaccentdieresis") ;U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
          (#x07bb "Greek_omegaaccent") ;U+03CE GREEK SMALL LETTER OMEGA WITH TONOS
          (#x07c1 "Greek_ALPHA") ;U+0391 GREEK CAPITAL LETTER ALPHA
          (#x07c2 "Greek_BETA")  ;U+0392 GREEK CAPITAL LETTER BETA
          (#x07c3 "Greek_GAMMA") ;U+0393 GREEK CAPITAL LETTER GAMMA
          (#x07c4 "Greek_DELTA") ;U+0394 GREEK CAPITAL LETTER DELTA
          (#x07c5 "Greek_EPSILON") ;U+0395 GREEK CAPITAL LETTER EPSILON
          (#x07c6 "Greek_ZETA")  ;U+0396 GREEK CAPITAL LETTER ZETA
          (#x07c7 "Greek_ETA")    ;U+0397 GREEK CAPITAL LETTER ETA
          (#x07c8 "Greek_THETA") ;U+0398 GREEK CAPITAL LETTER THETA
          (#x07c9 "Greek_IOTA")  ;U+0399 GREEK CAPITAL LETTER IOTA
          (#x07ca "Greek_KAPPA") ;U+039A GREEK CAPITAL LETTER KAPPA
          (#x07cb "Greek_LAMDA") ;U+039B GREEK CAPITAL LETTER LAMDA
          (#x07cb "Greek_LAMBDA") ;U+039B GREEK CAPITAL LETTER LAMDA
          (#x07cc "Greek_MU")      ;U+039C GREEK CAPITAL LETTER MU
          (#x07cd "Greek_NU")      ;U+039D GREEK CAPITAL LETTER NU
          (#x07ce "Greek_XI")      ;U+039E GREEK CAPITAL LETTER XI
          (#x07cf "Greek_OMICRON") ;U+039F GREEK CAPITAL LETTER OMICRON
          (#x07d0 "Greek_PI")      ;U+03A0 GREEK CAPITAL LETTER PI
          (#x07d1 "Greek_RHO")    ;U+03A1 GREEK CAPITAL LETTER RHO
          (#x07d2 "Greek_SIGMA") ;U+03A3 GREEK CAPITAL LETTER SIGMA
          (#x07d4 "Greek_TAU")    ;U+03A4 GREEK CAPITAL LETTER TAU
          (#x07d5 "Greek_UPSILON") ;U+03A5 GREEK CAPITAL LETTER UPSILON
          (#x07d6 "Greek_PHI")    ;U+03A6 GREEK CAPITAL LETTER PHI
          (#x07d7 "Greek_CHI")    ;U+03A7 GREEK CAPITAL LETTER CHI
          (#x07d8 "Greek_PSI")    ;U+03A8 GREEK CAPITAL LETTER PSI
          (#x07d9 "Greek_OMEGA") ;U+03A9 GREEK CAPITAL LETTER OMEGA
          (#x07e1 "Greek_alpha")  ;U+03B1 GREEK SMALL LETTER ALPHA
          (#x07e2 "Greek_beta")    ;U+03B2 GREEK SMALL LETTER BETA
          (#x07e3 "Greek_gamma")  ;U+03B3 GREEK SMALL LETTER GAMMA
          (#x07e4 "Greek_delta")  ;U+03B4 GREEK SMALL LETTER DELTA
          (#x07e5 "Greek_epsilon") ;U+03B5 GREEK SMALL LETTER EPSILON
          (#x07e6 "Greek_zeta")    ;U+03B6 GREEK SMALL LETTER ZETA
          (#x07e7 "Greek_eta")      ;U+03B7 GREEK SMALL LETTER ETA
          (#x07e8 "Greek_theta")  ;U+03B8 GREEK SMALL LETTER THETA
          (#x07e9 "Greek_iota")    ;U+03B9 GREEK SMALL LETTER IOTA
          (#x07ea "Greek_kappa")  ;U+03BA GREEK SMALL LETTER KAPPA
          (#x07eb "Greek_lamda")  ;U+03BB GREEK SMALL LETTER LAMDA
          (#x07eb "Greek_lambda") ;U+03BB GREEK SMALL LETTER LAMDA
          (#x07ec "Greek_mu")       ;U+03BC GREEK SMALL LETTER MU
          (#x07ed "Greek_nu")       ;U+03BD GREEK SMALL LETTER NU
          (#x07ee "Greek_xi")       ;U+03BE GREEK SMALL LETTER XI
          (#x07ef "Greek_omicron") ;U+03BF GREEK SMALL LETTER OMICRON
          (#x07f0 "Greek_pi")       ;U+03C0 GREEK SMALL LETTER PI
          (#x07f1 "Greek_rho")      ;U+03C1 GREEK SMALL LETTER RHO
          (#x07f2 "Greek_sigma")  ;U+03C3 GREEK SMALL LETTER SIGMA
          (#x07f3 "Greek_finalsmallsigma") ;U+03C2 GREEK SMALL LETTER FINAL SIGMA
          (#x07f4 "Greek_tau")      ;U+03C4 GREEK SMALL LETTER TAU
          (#x07f5 "Greek_upsilon") ;U+03C5 GREEK SMALL LETTER UPSILON
          (#x07f6 "Greek_phi")      ;U+03C6 GREEK SMALL LETTER PHI
          (#x07f7 "Greek_chi")      ;U+03C7 GREEK SMALL LETTER CHI
          (#x07f8 "Greek_psi")      ;U+03C8 GREEK SMALL LETTER PSI
          (#x07f9 "Greek_omega")  ;U+03C9 GREEK SMALL LETTER OMEGA
          (#xff7e "Greek_switch")   ;Alias for mode_switch
          (#x08a1 "leftradical")    ;U+23B7 RADICAL SYMBOL BOTTOM
          (#x08a2 "topleftradical") ;(U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT)
          (#x08a3 "horizconnector") ;(U+2500 BOX DRAWINGS LIGHT HORIZONTAL)
          (#x08a4 "topintegral")    ;U+2320 TOP HALF INTEGRAL
          (#x08a5 "botintegral")    ;U+2321 BOTTOM HALF INTEGRAL
          (#x08a6 "vertconnector") ;(U+2502 BOX DRAWINGS LIGHT VERTICAL)
          (#x08a7 "topleftsqbracket") ;U+23A1 LEFT SQUARE BRACKET UPPER CORNER
          (#x08a8 "botleftsqbracket") ;U+23A3 LEFT SQUARE BRACKET LOWER CORNER
          (#x08a9 "toprightsqbracket") ;U+23A4 RIGHT SQUARE BRACKET UPPER CORNER
          (#x08aa "botrightsqbracket") ;U+23A6 RIGHT SQUARE BRACKET LOWER CORNER
          (#x08ab "topleftparens") ;U+239B LEFT PARENTHESIS UPPER HOOK
          (#x08ac "botleftparens") ;U+239D LEFT PARENTHESIS LOWER HOOK
          (#x08ad "toprightparens") ;U+239E RIGHT PARENTHESIS UPPER HOOK
          (#x08ae "botrightparens") ;U+23A0 RIGHT PARENTHESIS LOWER HOOK
          (#x08af "leftmiddlecurlybrace") ;U+23A8 LEFT CURLY BRACKET MIDDLE PIECE
          (#x08b0 "rightmiddlecurlybrace") ;U+23AC RIGHT CURLY BRACKET MIDDLE PIECE
          (#x08b1 "topleftsummation")
          (#x08b2 "botleftsummation")
          (#x08b3 "topvertsummationconnector")
          (#x08b4 "botvertsummationconnector")
          (#x08b5 "toprightsummation")
          (#x08b6 "botrightsummation")
          (#x08b7 "rightmiddlesummation")
          (#x08bc "lessthanequal")  ;U+2264 LESS-THAN OR EQUAL TO
          (#x08bd "notequal")       ;U+2260 NOT EQUAL TO
          (#x08be "greaterthanequal") ;U+2265 GREATER-THAN OR EQUAL TO
          (#x08bf "integral")       ;U+222B INTEGRAL
          (#x08c0 "therefore")      ;U+2234 THEREFORE
          (#x08c1 "variation")      ;U+221D PROPORTIONAL TO
          (#x08c2 "infinity")       ;U+221E INFINITY
          (#x08c5 "nabla")          ;U+2207 NABLA
          (#x08c8 "approximate")    ;U+223C TILDE OPERATOR
          (#x08c9 "similarequal")  ;U+2243 ASYMPTOTICALLY EQUAL TO
          (#x08cd "ifonlyif")      ;U+21D4 LEFT RIGHT DOUBLE ARROW
          (#x08ce "implies")       ;U+21D2 RIGHTWARDS DOUBLE ARROW
          (#x08cf "identical")      ;U+2261 IDENTICAL TO
          (#x08d6 "radical")        ;U+221A SQUARE ROOT
          (#x08da "includedin")     ;U+2282 SUBSET OF
          (#x08db "includes")       ;U+2283 SUPERSET OF
          (#x08dc "intersection")   ;U+2229 INTERSECTION
          (#x08dd "union")          ;U+222A UNION
          (#x08de "logicaland")     ;U+2227 LOGICAL AND
          (#x08df "logicalor")      ;U+2228 LOGICAL OR
          (#x08ef "partialderivative") ;U+2202 PARTIAL DIFFERENTIAL
          (#x08f6 "function") ;U+0192 LATIN SMALL LETTER F WITH HOOK
          (#x08fb "leftarrow")      ;U+2190 LEFTWARDS ARROW
          (#x08fc "uparrow")        ;U+2191 UPWARDS ARROW
          (#x08fd "rightarrow")     ;U+2192 RIGHTWARDS ARROW
          (#x08fe "downarrow")      ;U+2193 DOWNWARDS ARROW
          (#x09df "blank")
          (#x09e0 "soliddiamond")   ;U+25C6 BLACK DIAMOND
          (#x09e1 "checkerboard")   ;U+2592 MEDIUM SHADE
          (#x09e2 "ht")   ;U+2409 SYMBOL FOR HORIZONTAL TABULATION
          (#x09e3 "ff")             ;U+240C SYMBOL FOR FORM FEED
          (#x09e4 "cr")         ;U+240D SYMBOL FOR CARRIAGE RETURN
          (#x09e5 "lf")             ;U+240A SYMBOL FOR LINE FEED
          (#x09e8 "nl")             ;U+2424 SYMBOL FOR NEWLINE
          (#x09e9 "vt")     ;U+240B SYMBOL FOR VERTICAL TABULATION
          (#x09ea "lowrightcorner") ;U+2518 BOX DRAWINGS LIGHT UP AND LEFT
          (#x09eb "uprightcorner") ;U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT
          (#x09ec "upleftcorner") ;U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT
          (#x09ed "lowleftcorner") ;U+2514 BOX DRAWINGS LIGHT UP AND RIGHT
          (#x09ee "crossinglines") ;U+253C BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
          (#x09ef "horizlinescan1") ;U+23BA HORIZONTAL SCAN LINE-1
          (#x09f0 "horizlinescan3") ;U+23BB HORIZONTAL SCAN LINE-3
          (#x09f1 "horizlinescan5") ;U+2500 BOX DRAWINGS LIGHT HORIZONTAL
          (#x09f2 "horizlinescan7") ;U+23BC HORIZONTAL SCAN LINE-7
          (#x09f3 "horizlinescan9") ;U+23BD HORIZONTAL SCAN LINE-9
          (#x09f4 "leftt") ;U+251C BOX DRAWINGS LIGHT VERTICAL AND RIGHT
          (#x09f5 "rightt") ;U+2524 BOX DRAWINGS LIGHT VERTICAL AND LEFT
          (#x09f6 "bott") ;U+2534 BOX DRAWINGS LIGHT UP AND HORIZONTAL
          (#x09f7 "topt") ;U+252C BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
          (#x09f8 "vertbar")   ;U+2502 BOX DRAWINGS LIGHT VERTICAL
          (#x0aa1 "emspace")        ;U+2003 EM SPACE
          (#x0aa2 "enspace")        ;U+2002 EN SPACE
          (#x0aa3 "em3space")       ;U+2004 THREE-PER-EM SPACE
          (#x0aa4 "em4space")       ;U+2005 FOUR-PER-EM SPACE
          (#x0aa5 "digitspace")     ;U+2007 FIGURE SPACE
          (#x0aa6 "punctspace")     ;U+2008 PUNCTUATION SPACE
          (#x0aa7 "thinspace")      ;U+2009 THIN SPACE
          (#x0aa8 "hairspace")      ;U+200A HAIR SPACE
          (#x0aa9 "emdash")         ;U+2014 EM DASH
          (#x0aaa "endash")         ;U+2013 EN DASH
          (#x0aac "signifblank")    ;(U+2423 OPEN BOX)
          (#x0aae "ellipsis")       ;U+2026 HORIZONTAL ELLIPSIS
          (#x0aaf "doubbaselinedot") ;U+2025 TWO DOT LEADER
          (#x0ab0 "onethird")    ;U+2153 VULGAR FRACTION ONE THIRD
          (#x0ab1 "twothirds")  ;U+2154 VULGAR FRACTION TWO THIRDS
          (#x0ab2 "onefifth")    ;U+2155 VULGAR FRACTION ONE FIFTH
          (#x0ab3 "twofifths")  ;U+2156 VULGAR FRACTION TWO FIFTHS
          (#x0ab4 "threefifths") ;U+2157 VULGAR FRACTION THREE FIFTHS
          (#x0ab5 "fourfifths") ;U+2158 VULGAR FRACTION FOUR FIFTHS
          (#x0ab6 "onesixth")    ;U+2159 VULGAR FRACTION ONE SIXTH
          (#x0ab7 "fivesixths") ;U+215A VULGAR FRACTION FIVE SIXTHS
          (#x0ab8 "careof")         ;U+2105 CARE OF
          (#x0abb "figdash")        ;U+2012 FIGURE DASH
          (#x0abc "leftanglebracket") ;(U+27E8 MATHEMATICAL LEFT ANGLE BRACKET)
          (#x0abd "decimalpoint")   ;(U+002E FULL STOP)
          (#x0abe "rightanglebracket") ;(U+27E9 MATHEMATICAL RIGHT ANGLE BRACKET)
          (#x0abf "marker")
          (#x0ac3 "oneeighth")  ;U+215B VULGAR FRACTION ONE EIGHTH
          (#x0ac4 "threeeighths") ;U+215C VULGAR FRACTION THREE EIGHTHS
          (#x0ac5 "fiveeighths") ;U+215D VULGAR FRACTION FIVE EIGHTHS
          (#x0ac6 "seveneighths") ;U+215E VULGAR FRACTION SEVEN EIGHTHS
          (#x0ac9 "trademark")      ;U+2122 TRADE MARK SIGN
          (#x0aca "signaturemark")  ;(U+2613 SALTIRE)
          (#x0acb "trademarkincircle")
          (#x0acc "leftopentriangle") ;(U+25C1 WHITE LEFT-POINTING TRIANGLE)
          (#x0acd "rightopentriangle") ;(U+25B7 WHITE RIGHT-POINTING TRIANGLE)
          (#x0ace "emopencircle")   ;(U+25CB WHITE CIRCLE)
          (#x0acf "emopenrectangle") ;(U+25AF WHITE VERTICAL RECTANGLE)
          (#x0ad0 "leftsinglequotemark") ;U+2018 LEFT SINGLE QUOTATION MARK
          (#x0ad1 "rightsinglequotemark") ;U+2019 RIGHT SINGLE QUOTATION MARK
          (#x0ad2 "leftdoublequotemark") ;U+201C LEFT DOUBLE QUOTATION MARK
          (#x0ad3 "rightdoublequotemark") ;U+201D RIGHT DOUBLE QUOTATION MARK
          (#x0ad4 "prescription")   ;U+211E PRESCRIPTION TAKE
          (#x0ad6 "minutes")        ;U+2032 PRIME
          (#x0ad7 "seconds")        ;U+2033 DOUBLE PRIME
          (#x0ad9 "latincross")     ;U+271D LATIN CROSS
          (#x0ada "hexagram")
          (#x0adb "filledrectbullet") ;(U+25AC BLACK RECTANGLE)
          (#x0adc "filledlefttribullet") ;(U+25C0 BLACK LEFT-POINTING TRIANGLE)
          (#x0add "filledrighttribullet") ;(U+25B6 BLACK RIGHT-POINTING TRIANGLE)
          (#x0ade "emfilledcircle") ;(U+25CF BLACK CIRCLE)
          (#x0adf "emfilledrect") ;(U+25AE BLACK VERTICAL RECTANGLE)
          (#x0ae0 "enopencircbullet") ;(U+25E6 WHITE BULLET)
          (#x0ae1 "enopensquarebullet") ;(U+25AB WHITE SMALL SQUARE)
          (#x0ae2 "openrectbullet") ;(U+25AD WHITE RECTANGLE)
          (#x0ae3 "opentribulletup") ;(U+25B3 WHITE UP-POINTING TRIANGLE)
          (#x0ae4 "opentribulletdown") ;(U+25BD WHITE DOWN-POINTING TRIANGLE)
          (#x0ae5 "openstar")       ;(U+2606 WHITE STAR)
          (#x0ae6 "enfilledcircbullet") ;(U+2022 BULLET)
          (#x0ae7 "enfilledsqbullet") ;(U+25AA BLACK SMALL SQUARE)
          (#x0ae8 "filledtribulletup") ;(U+25B2 BLACK UP-POINTING TRIANGLE)
          (#x0ae9 "filledtribulletdown") ;(U+25BC BLACK DOWN-POINTING TRIANGLE)
          (#x0aea "leftpointer") ;(U+261C WHITE LEFT POINTING INDEX)
          (#x0aeb "rightpointer") ;(U+261E WHITE RIGHT POINTING INDEX)
          (#x0aec "club")           ;U+2663 BLACK CLUB SUIT
          (#x0aed "diamond")        ;U+2666 BLACK DIAMOND SUIT
          (#x0aee "heart")          ;U+2665 BLACK HEART SUIT
          (#x0af0 "maltesecross")   ;U+2720 MALTESE CROSS
          (#x0af1 "dagger")         ;U+2020 DAGGER
          (#x0af2 "doubledagger")   ;U+2021 DOUBLE DAGGER
          (#x0af3 "checkmark")      ;U+2713 CHECK MARK
          (#x0af4 "ballotcross")    ;U+2717 BALLOT X
          (#x0af5 "musicalsharp")   ;U+266F MUSIC SHARP SIGN
          (#x0af6 "musicalflat")    ;U+266D MUSIC FLAT SIGN
          (#x0af7 "malesymbol")     ;U+2642 MALE SIGN
          (#x0af8 "femalesymbol")   ;U+2640 FEMALE SIGN
          (#x0af9 "telephone")      ;U+260E BLACK TELEPHONE
          (#x0afa "telephonerecorder") ;U+2315 TELEPHONE RECORDER
          (#x0afb "phonographcopyright") ;U+2117 SOUND RECORDING COPYRIGHT
          (#x0afc "caret")          ;U+2038 CARET
          (#x0afd "singlelowquotemark") ;U+201A SINGLE LOW-9 QUOTATION MARK
          (#x0afe "doublelowquotemark") ;U+201E DOUBLE LOW-9 QUOTATION MARK
          (#x0aff "cursor")
          (#x0ba3 "leftcaret")      ;(U+003C LESS-THAN SIGN)
          (#x0ba6 "rightcaret")     ;(U+003E GREATER-THAN SIGN)
          (#x0ba8 "downcaret")      ;(U+2228 LOGICAL OR)
          (#x0ba9 "upcaret")        ;(U+2227 LOGICAL AND)
          (#x0bc0 "overbar")        ;(U+00AF MACRON)
          (#x0bc2 "downtack")       ;U+22A5 UP TACK
          (#x0bc3 "upshoe")         ;(U+2229 INTERSECTION)
          (#x0bc4 "downstile")      ;U+230A LEFT FLOOR
          (#x0bc6 "underbar")       ;(U+005F LOW LINE)
          (#x0bca "jot")            ;U+2218 RING OPERATOR
          (#x0bcc "quad")       ;U+2395 APL FUNCTIONAL SYMBOL QUAD
          (#x0bce "uptack")         ;U+22A4 DOWN TACK
          (#x0bcf "circle")         ;U+25CB WHITE CIRCLE
          (#x0bd3 "upstile")        ;U+2308 LEFT CEILING
          (#x0bd6 "downshoe")       ;(U+222A UNION)
          (#x0bd8 "rightshoe")      ;(U+2283 SUPERSET OF)
          (#x0bda "leftshoe")       ;(U+2282 SUBSET OF)
          (#x0bdc "lefttack")       ;U+22A2 RIGHT TACK
          (#x0bfc "righttack")      ;U+22A3 LEFT TACK
          (#x0cdf "hebrew_doublelowline") ;U+2017 DOUBLE LOW LINE
          (#x0ce0 "hebrew_aleph")   ;U+05D0 HEBREW LETTER ALEF
          (#x0ce1 "hebrew_bet")     ;U+05D1 HEBREW LETTER BET
          (#x0ce1 "hebrew_beth")    ;deprecated
          (#x0ce2 "hebrew_gimel")   ;U+05D2 HEBREW LETTER GIMEL
          (#x0ce2 "hebrew_gimmel")  ;deprecated
          (#x0ce3 "hebrew_dalet")   ;U+05D3 HEBREW LETTER DALET
          (#x0ce3 "hebrew_daleth")  ;deprecated
          (#x0ce4 "hebrew_he")      ;U+05D4 HEBREW LETTER HE
          (#x0ce5 "hebrew_waw")     ;U+05D5 HEBREW LETTER VAV
          (#x0ce6 "hebrew_zain")    ;U+05D6 HEBREW LETTER ZAYIN
          (#x0ce6 "hebrew_zayin")   ;deprecated
          (#x0ce7 "hebrew_chet")    ;U+05D7 HEBREW LETTER HET
          (#x0ce7 "hebrew_het")     ;deprecated
          (#x0ce8 "hebrew_tet")     ;U+05D8 HEBREW LETTER TET
          (#x0ce8 "hebrew_teth")    ;deprecated
          (#x0ce9 "hebrew_yod")     ;U+05D9 HEBREW LETTER YOD
          (#x0cea "hebrew_finalkaph") ;U+05DA HEBREW LETTER FINAL KAF
          (#x0ceb "hebrew_kaph")    ;U+05DB HEBREW LETTER KAF
          (#x0cec "hebrew_lamed")   ;U+05DC HEBREW LETTER LAMED
          (#x0ced "hebrew_finalmem") ;U+05DD HEBREW LETTER FINAL MEM
          (#x0cee "hebrew_mem")     ;U+05DE HEBREW LETTER MEM
          (#x0cef "hebrew_finalnun") ;U+05DF HEBREW LETTER FINAL NUN
          (#x0cf0 "hebrew_nun")     ;U+05E0 HEBREW LETTER NUN
          (#x0cf1 "hebrew_samech")  ;U+05E1 HEBREW LETTER SAMEKH
          (#x0cf1 "hebrew_samekh")  ;deprecated
          (#x0cf2 "hebrew_ayin")    ;U+05E2 HEBREW LETTER AYIN
          (#x0cf3 "hebrew_finalpe") ;U+05E3 HEBREW LETTER FINAL PE
          (#x0cf4 "hebrew_pe")      ;U+05E4 HEBREW LETTER PE
          (#x0cf5 "hebrew_finalzade") ;U+05E5 HEBREW LETTER FINAL TSADI
          (#x0cf5 "hebrew_finalzadi") ;deprecated
          (#x0cf6 "hebrew_zade")    ;U+05E6 HEBREW LETTER TSADI
          (#x0cf6 "hebrew_zadi")    ;deprecated
          (#x0cf7 "hebrew_qoph")    ;U+05E7 HEBREW LETTER QOF
          (#x0cf7 "hebrew_kuf")     ;deprecated
          (#x0cf8 "hebrew_resh")    ;U+05E8 HEBREW LETTER RESH
          (#x0cf9 "hebrew_shin")    ;U+05E9 HEBREW LETTER SHIN
          (#x0cfa "hebrew_taw")     ;U+05EA HEBREW LETTER TAV
          (#x0cfa "hebrew_taf")     ;deprecated
          (#xff7e "Hebrew_switch")  ;Alias for mode_switch
          (#x0da1 "Thai_kokai")     ;U+0E01 THAI CHARACTER KO KAI
          (#x0da2 "Thai_khokhai")  ;U+0E02 THAI CHARACTER KHO KHAI
          (#x0da3 "Thai_khokhuat") ;U+0E03 THAI CHARACTER KHO KHUAT
          (#x0da4 "Thai_khokhwai") ;U+0E04 THAI CHARACTER KHO KHWAI
          (#x0da5 "Thai_khokhon")  ;U+0E05 THAI CHARACTER KHO KHON
          (#x0da6 "Thai_khorakhang") ;U+0E06 THAI CHARACTER KHO RAKHANG
          (#x0da7 "Thai_ngongu")    ;U+0E07 THAI CHARACTER NGO NGU
          (#x0da8 "Thai_chochan")  ;U+0E08 THAI CHARACTER CHO CHAN
          (#x0da9 "Thai_choching") ;U+0E09 THAI CHARACTER CHO CHING
          (#x0daa "Thai_chochang") ;U+0E0A THAI CHARACTER CHO CHANG
          (#x0dab "Thai_soso")      ;U+0E0B THAI CHARACTER SO SO
          (#x0dac "Thai_chochoe")  ;U+0E0C THAI CHARACTER CHO CHOE
          (#x0dad "Thai_yoying")    ;U+0E0D THAI CHARACTER YO YING
          (#x0dae "Thai_dochada")  ;U+0E0E THAI CHARACTER DO CHADA
          (#x0daf "Thai_topatak")  ;U+0E0F THAI CHARACTER TO PATAK
          (#x0db0 "Thai_thothan")  ;U+0E10 THAI CHARACTER THO THAN
          (#x0db1 "Thai_thonangmontho") ;U+0E11 THAI CHARACTER THO NANGMONTHO
          (#x0db2 "Thai_thophuthao") ;U+0E12 THAI CHARACTER THO PHUTHAO
          (#x0db3 "Thai_nonen")     ;U+0E13 THAI CHARACTER NO NEN
          (#x0db4 "Thai_dodek")     ;U+0E14 THAI CHARACTER DO DEK
          (#x0db5 "Thai_totao")     ;U+0E15 THAI CHARACTER TO TAO
          (#x0db6 "Thai_thothung") ;U+0E16 THAI CHARACTER THO THUNG
          (#x0db7 "Thai_thothahan") ;U+0E17 THAI CHARACTER THO THAHAN
          (#x0db8 "Thai_thothong") ;U+0E18 THAI CHARACTER THO THONG
          (#x0db9 "Thai_nonu")      ;U+0E19 THAI CHARACTER NO NU
          (#x0dba "Thai_bobaimai") ;U+0E1A THAI CHARACTER BO BAIMAI
          (#x0dbb "Thai_popla")     ;U+0E1B THAI CHARACTER PO PLA
          (#x0dbc "Thai_phophung") ;U+0E1C THAI CHARACTER PHO PHUNG
          (#x0dbd "Thai_fofa")      ;U+0E1D THAI CHARACTER FO FA
          (#x0dbe "Thai_phophan")  ;U+0E1E THAI CHARACTER PHO PHAN
          (#x0dbf "Thai_fofan")     ;U+0E1F THAI CHARACTER FO FAN
          (#x0dc0 "Thai_phosamphao") ;U+0E20 THAI CHARACTER PHO SAMPHAO
          (#x0dc1 "Thai_moma")      ;U+0E21 THAI CHARACTER MO MA
          (#x0dc2 "Thai_yoyak")     ;U+0E22 THAI CHARACTER YO YAK
          (#x0dc3 "Thai_rorua")     ;U+0E23 THAI CHARACTER RO RUA
          (#x0dc4 "Thai_ru")        ;U+0E24 THAI CHARACTER RU
          (#x0dc5 "Thai_loling")    ;U+0E25 THAI CHARACTER LO LING
          (#x0dc6 "Thai_lu")        ;U+0E26 THAI CHARACTER LU
          (#x0dc7 "Thai_wowaen")    ;U+0E27 THAI CHARACTER WO WAEN
          (#x0dc8 "Thai_sosala")    ;U+0E28 THAI CHARACTER SO SALA
          (#x0dc9 "Thai_sorusi")    ;U+0E29 THAI CHARACTER SO RUSI
          (#x0dca "Thai_sosua")     ;U+0E2A THAI CHARACTER SO SUA
          (#x0dcb "Thai_hohip")     ;U+0E2B THAI CHARACTER HO HIP
          (#x0dcc "Thai_lochula")  ;U+0E2C THAI CHARACTER LO CHULA
          (#x0dcd "Thai_oang")      ;U+0E2D THAI CHARACTER O ANG
          (#x0dce "Thai_honokhuk") ;U+0E2E THAI CHARACTER HO NOKHUK
          (#x0dcf "Thai_paiyannoi") ;U+0E2F THAI CHARACTER PAIYANNOI
          (#x0dd0 "Thai_saraa")     ;U+0E30 THAI CHARACTER SARA A
          (#x0dd1 "Thai_maihanakat") ;U+0E31 THAI CHARACTER MAI HAN-AKAT
          (#x0dd2 "Thai_saraaa")    ;U+0E32 THAI CHARACTER SARA AA
          (#x0dd3 "Thai_saraam")    ;U+0E33 THAI CHARACTER SARA AM
          (#x0dd4 "Thai_sarai")     ;U+0E34 THAI CHARACTER SARA I
          (#x0dd5 "Thai_saraii")    ;U+0E35 THAI CHARACTER SARA II
          (#x0dd6 "Thai_saraue")    ;U+0E36 THAI CHARACTER SARA UE
          (#x0dd7 "Thai_sarauee")  ;U+0E37 THAI CHARACTER SARA UEE
          (#x0dd8 "Thai_sarau")     ;U+0E38 THAI CHARACTER SARA U
          (#x0dd9 "Thai_sarauu")    ;U+0E39 THAI CHARACTER SARA UU
          (#x0dda "Thai_phinthu")   ;U+0E3A THAI CHARACTER PHINTHU
          (#x0dde "Thai_maihanakat_maitho")
          (#x0ddf "Thai_baht")   ;U+0E3F THAI CURRENCY SYMBOL BAHT
          (#x0de0 "Thai_sarae")     ;U+0E40 THAI CHARACTER SARA E
          (#x0de1 "Thai_saraae")    ;U+0E41 THAI CHARACTER SARA AE
          (#x0de2 "Thai_sarao")     ;U+0E42 THAI CHARACTER SARA O
          (#x0de3 "Thai_saraaimaimuan") ;U+0E43 THAI CHARACTER SARA AI MAIMUAN
          (#x0de4 "Thai_saraaimaimalai") ;U+0E44 THAI CHARACTER SARA AI MAIMALAI
          (#x0de5 "Thai_lakkhangyao") ;U+0E45 THAI CHARACTER LAKKHANGYAO
          (#x0de6 "Thai_maiyamok") ;U+0E46 THAI CHARACTER MAIYAMOK
          (#x0de7 "Thai_maitaikhu") ;U+0E47 THAI CHARACTER MAITAIKHU
          (#x0de8 "Thai_maiek")     ;U+0E48 THAI CHARACTER MAI EK
          (#x0de9 "Thai_maitho")    ;U+0E49 THAI CHARACTER MAI THO
          (#x0dea "Thai_maitri")    ;U+0E4A THAI CHARACTER MAI TRI
          (#x0deb "Thai_maichattawa") ;U+0E4B THAI CHARACTER MAI CHATTAWA
          (#x0dec "Thai_thanthakhat") ;U+0E4C THAI CHARACTER THANTHAKHAT
          (#x0ded "Thai_nikhahit") ;U+0E4D THAI CHARACTER NIKHAHIT
          (#x0df0 "Thai_leksun")    ;U+0E50 THAI DIGIT ZERO
          (#x0df1 "Thai_leknung")   ;U+0E51 THAI DIGIT ONE
          (#x0df2 "Thai_leksong")   ;U+0E52 THAI DIGIT TWO
          (#x0df3 "Thai_leksam")    ;U+0E53 THAI DIGIT THREE
          (#x0df4 "Thai_leksi")     ;U+0E54 THAI DIGIT FOUR
          (#x0df5 "Thai_lekha")     ;U+0E55 THAI DIGIT FIVE
          (#x0df6 "Thai_lekhok")    ;U+0E56 THAI DIGIT SIX
          (#x0df7 "Thai_lekchet")   ;U+0E57 THAI DIGIT SEVEN
          (#x0df8 "Thai_lekpaet")   ;U+0E58 THAI DIGIT EIGHT
          (#x0df9 "Thai_lekkao")    ;U+0E59 THAI DIGIT NINE
          (#xff31 "Hangul")         ;Hangul start/stop(toggle)
          (#xff32 "Hangul_Start")   ;Hangul start
          (#xff33 "Hangul_End")     ;Hangul end, English start
          (#xff34 "Hangul_Hanja")  ;Start Hangul->Hanja Conversion
          (#xff35 "Hangul_Jamo")    ;Hangul Jamo mode
          (#xff36 "Hangul_Romaja")  ;Hangul Romaja mode
          (#xff37 "Hangul_Codeinput") ;Hangul code input mode
          (#xff38 "Hangul_Jeonja")  ;Jeonja mode
          (#xff39 "Hangul_Banja")   ;Banja mode
          (#xff3a "Hangul_PreHanja") ;Pre Hanja conversion
          (#xff3b "Hangul_PostHanja") ;Post Hanja conversion
          (#xff3c "Hangul_SingleCandidate") ;Single candidate
          (#xff3d "Hangul_MultipleCandidate") ;Multiple candidate
          (#xff3e "Hangul_PreviousCandidate") ;Previous candidate
          (#xff3f "Hangul_Special") ;Special symbols
          (#xff7e "Hangul_switch")  ;Alias for mode_switch
          (#x0ea1 "Hangul_Kiyeog")
          (#x0ea2 "Hangul_SsangKiyeog")
          (#x0ea3 "Hangul_KiyeogSios")
          (#x0ea4 "Hangul_Nieun")
          (#x0ea5 "Hangul_NieunJieuj")
          (#x0ea6 "Hangul_NieunHieuh")
          (#x0ea7 "Hangul_Dikeud")
          (#x0ea8 "Hangul_SsangDikeud")
          (#x0ea9 "Hangul_Rieul")
          (#x0eaa "Hangul_RieulKiyeog")
          (#x0eab "Hangul_RieulMieum")
          (#x0eac "Hangul_RieulPieub")
          (#x0ead "Hangul_RieulSios")
          (#x0eae "Hangul_RieulTieut")
          (#x0eaf "Hangul_RieulPhieuf")
          (#x0eb0 "Hangul_RieulHieuh")
          (#x0eb1 "Hangul_Mieum")
          (#x0eb2 "Hangul_Pieub")
          (#x0eb3 "Hangul_SsangPieub")
          (#x0eb4 "Hangul_PieubSios")
          (#x0eb5 "Hangul_Sios")
          (#x0eb6 "Hangul_SsangSios")
          (#x0eb7 "Hangul_Ieung")
          (#x0eb8 "Hangul_Jieuj")
          (#x0eb9 "Hangul_SsangJieuj")
          (#x0eba "Hangul_Cieuc")
          (#x0ebb "Hangul_Khieuq")
          (#x0ebc "Hangul_Tieut")
          (#x0ebd "Hangul_Phieuf")
          (#x0ebe "Hangul_Hieuh")
          (#x0ebf "Hangul_A")
          (#x0ec0 "Hangul_AE")
          (#x0ec1 "Hangul_YA")
          (#x0ec2 "Hangul_YAE")
          (#x0ec3 "Hangul_EO")
          (#x0ec4 "Hangul_E")
          (#x0ec5 "Hangul_YEO")
          (#x0ec6 "Hangul_YE")
          (#x0ec7 "Hangul_O")
          (#x0ec8 "Hangul_WA")
          (#x0ec9 "Hangul_WAE")
          (#x0eca "Hangul_OE")
          (#x0ecb "Hangul_YO")
          (#x0ecc "Hangul_U")
          (#x0ecd "Hangul_WEO")
          (#x0ece "Hangul_WE")
          (#x0ecf "Hangul_WI")
          (#x0ed0 "Hangul_YU")
          (#x0ed1 "Hangul_EU")
          (#x0ed2 "Hangul_YI")
          (#x0ed3 "Hangul_I")
          (#x0ed4 "Hangul_J_Kiyeog")
          (#x0ed5 "Hangul_J_SsangKiyeog")
          (#x0ed6 "Hangul_J_KiyeogSios")
          (#x0ed7 "Hangul_J_Nieun")
          (#x0ed8 "Hangul_J_NieunJieuj")
          (#x0ed9 "Hangul_J_NieunHieuh")
          (#x0eda "Hangul_J_Dikeud")
          (#x0edb "Hangul_J_Rieul")
          (#x0edc "Hangul_J_RieulKiyeog")
          (#x0edd "Hangul_J_RieulMieum")
          (#x0ede "Hangul_J_RieulPieub")
          (#x0edf "Hangul_J_RieulSios")
          (#x0ee0 "Hangul_J_RieulTieut")
          (#x0ee1 "Hangul_J_RieulPhieuf")
          (#x0ee2 "Hangul_J_RieulHieuh")
          (#x0ee3 "Hangul_J_Mieum")
          (#x0ee4 "Hangul_J_Pieub")
          (#x0ee5 "Hangul_J_PieubSios")
          (#x0ee6 "Hangul_J_Sios")
          (#x0ee7 "Hangul_J_SsangSios")
          (#x0ee8 "Hangul_J_Ieung")
          (#x0ee9 "Hangul_J_Jieuj")
          (#x0eea "Hangul_J_Cieuc")
          (#x0eeb "Hangul_J_Khieuq")
          (#x0eec "Hangul_J_Tieut")
          (#x0eed "Hangul_J_Phieuf")
          (#x0eee "Hangul_J_Hieuh")
          (#x0eef "Hangul_RieulYeorinHieuh")
          (#x0ef0 "Hangul_SunkyeongeumMieum")
          (#x0ef1 "Hangul_SunkyeongeumPieub")
          (#x0ef2 "Hangul_PanSios")
          (#x0ef3 "Hangul_KkogjiDalrinIeung")
          (#x0ef4 "Hangul_SunkyeongeumPhieuf")
          (#x0ef5 "Hangul_YeorinHieuh")
          (#x0ef6 "Hangul_AraeA")
          (#x0ef7 "Hangul_AraeAE")
          (#x0ef8 "Hangul_J_PanSios")
          (#x0ef9 "Hangul_J_KkogjiDalrinIeung")
          (#x0efa "Hangul_J_YeorinHieuh")
          (#x0eff "Korean_Won")     ;(U+20A9 WON SIGN)
          (#x1000587 "Armenian_ligature_ew") ;U+0587 ARMENIAN SMALL LIGATURE ECH YIWN
          (#x1000589 "Armenian_full_stop") ;U+0589 ARMENIAN FULL STOP
          (#x1000589 "Armenian_verjaket") ;U+0589 ARMENIAN FULL STOP
          (#x100055d "Armenian_separation_mark") ;U+055D ARMENIAN COMMA
          (#x100055d "Armenian_but") ;U+055D ARMENIAN COMMA
          (#x100058a "Armenian_hyphen") ;U+058A ARMENIAN HYPHEN
          (#x100058a "Armenian_yentamna") ;U+058A ARMENIAN HYPHEN
          (#x100055c "Armenian_exclam") ;U+055C ARMENIAN EXCLAMATION MARK
          (#x100055c "Armenian_amanak") ;U+055C ARMENIAN EXCLAMATION MARK
          (#x100055b "Armenian_accent") ;U+055B ARMENIAN EMPHASIS MARK
          (#x100055b "Armenian_shesht") ;U+055B ARMENIAN EMPHASIS MARK
          (#x100055e "Armenian_question") ;U+055E ARMENIAN QUESTION MARK
          (#x100055e "Armenian_paruyk") ;U+055E ARMENIAN QUESTION MARK
          (#x1000531 "Armenian_AYB") ;U+0531 ARMENIAN CAPITAL LETTER AYB
          (#x1000561 "Armenian_ayb") ;U+0561 ARMENIAN SMALL LETTER AYB
          (#x1000532 "Armenian_BEN") ;U+0532 ARMENIAN CAPITAL LETTER BEN
          (#x1000562 "Armenian_ben") ;U+0562 ARMENIAN SMALL LETTER BEN
          (#x1000533 "Armenian_GIM") ;U+0533 ARMENIAN CAPITAL LETTER GIM
          (#x1000563 "Armenian_gim") ;U+0563 ARMENIAN SMALL LETTER GIM
          (#x1000534 "Armenian_DA") ;U+0534 ARMENIAN CAPITAL LETTER DA
          (#x1000564 "Armenian_da") ;U+0564 ARMENIAN SMALL LETTER DA
          (#x1000535 "Armenian_YECH") ;U+0535 ARMENIAN CAPITAL LETTER ECH
          (#x1000565 "Armenian_yech") ;U+0565 ARMENIAN SMALL LETTER ECH
          (#x1000536 "Armenian_ZA") ;U+0536 ARMENIAN CAPITAL LETTER ZA
          (#x1000566 "Armenian_za") ;U+0566 ARMENIAN SMALL LETTER ZA
          (#x1000537 "Armenian_E") ;U+0537 ARMENIAN CAPITAL LETTER EH
          (#x1000567 "Armenian_e") ;U+0567 ARMENIAN SMALL LETTER EH
          (#x1000538 "Armenian_AT") ;U+0538 ARMENIAN CAPITAL LETTER ET
          (#x1000568 "Armenian_at") ;U+0568 ARMENIAN SMALL LETTER ET
          (#x1000539 "Armenian_TO") ;U+0539 ARMENIAN CAPITAL LETTER TO
          (#x1000569 "Armenian_to") ;U+0569 ARMENIAN SMALL LETTER TO
          (#x100053a "Armenian_ZHE") ;U+053A ARMENIAN CAPITAL LETTER ZHE
          (#x100056a "Armenian_zhe") ;U+056A ARMENIAN SMALL LETTER ZHE
          (#x100053b "Armenian_INI") ;U+053B ARMENIAN CAPITAL LETTER INI
          (#x100056b "Armenian_ini") ;U+056B ARMENIAN SMALL LETTER INI
          (#x100053c "Armenian_LYUN") ;U+053C ARMENIAN CAPITAL LETTER LIWN
          (#x100056c "Armenian_lyun") ;U+056C ARMENIAN SMALL LETTER LIWN
          (#x100053d "Armenian_KHE") ;U+053D ARMENIAN CAPITAL LETTER XEH
          (#x100056d "Armenian_khe") ;U+056D ARMENIAN SMALL LETTER XEH
          (#x100053e "Armenian_TSA") ;U+053E ARMENIAN CAPITAL LETTER CA
          (#x100056e "Armenian_tsa") ;U+056E ARMENIAN SMALL LETTER CA
          (#x100053f "Armenian_KEN") ;U+053F ARMENIAN CAPITAL LETTER KEN
          (#x100056f "Armenian_ken") ;U+056F ARMENIAN SMALL LETTER KEN
          (#x1000540 "Armenian_HO") ;U+0540 ARMENIAN CAPITAL LETTER HO
          (#x1000570 "Armenian_ho") ;U+0570 ARMENIAN SMALL LETTER HO
          (#x1000541 "Armenian_DZA") ;U+0541 ARMENIAN CAPITAL LETTER JA
          (#x1000571 "Armenian_dza") ;U+0571 ARMENIAN SMALL LETTER JA
          (#x1000542 "Armenian_GHAT") ;U+0542 ARMENIAN CAPITAL LETTER GHAD
          (#x1000572 "Armenian_ghat") ;U+0572 ARMENIAN SMALL LETTER GHAD
          (#x1000543 "Armenian_TCHE") ;U+0543 ARMENIAN CAPITAL LETTER CHEH
          (#x1000573 "Armenian_tche") ;U+0573 ARMENIAN SMALL LETTER CHEH
          (#x1000544 "Armenian_MEN") ;U+0544 ARMENIAN CAPITAL LETTER MEN
          (#x1000574 "Armenian_men") ;U+0574 ARMENIAN SMALL LETTER MEN
          (#x1000545 "Armenian_HI") ;U+0545 ARMENIAN CAPITAL LETTER YI
          (#x1000575 "Armenian_hi") ;U+0575 ARMENIAN SMALL LETTER YI
          (#x1000546 "Armenian_NU") ;U+0546 ARMENIAN CAPITAL LETTER NOW
          (#x1000576 "Armenian_nu") ;U+0576 ARMENIAN SMALL LETTER NOW
          (#x1000547 "Armenian_SHA") ;U+0547 ARMENIAN CAPITAL LETTER SHA
          (#x1000577 "Armenian_sha") ;U+0577 ARMENIAN SMALL LETTER SHA
          (#x1000548 "Armenian_VO") ;U+0548 ARMENIAN CAPITAL LETTER VO
          (#x1000578 "Armenian_vo") ;U+0578 ARMENIAN SMALL LETTER VO
          (#x1000549 "Armenian_CHA") ;U+0549 ARMENIAN CAPITAL LETTER CHA
          (#x1000579 "Armenian_cha") ;U+0579 ARMENIAN SMALL LETTER CHA
          (#x100054a "Armenian_PE") ;U+054A ARMENIAN CAPITAL LETTER PEH
          (#x100057a "Armenian_pe") ;U+057A ARMENIAN SMALL LETTER PEH
          (#x100054b "Armenian_JE") ;U+054B ARMENIAN CAPITAL LETTER JHEH
          (#x100057b "Armenian_je") ;U+057B ARMENIAN SMALL LETTER JHEH
          (#x100054c "Armenian_RA") ;U+054C ARMENIAN CAPITAL LETTER RA
          (#x100057c "Armenian_ra") ;U+057C ARMENIAN SMALL LETTER RA
          (#x100054d "Armenian_SE") ;U+054D ARMENIAN CAPITAL LETTER SEH
          (#x100057d "Armenian_se") ;U+057D ARMENIAN SMALL LETTER SEH
          (#x100054e "Armenian_VEV") ;U+054E ARMENIAN CAPITAL LETTER VEW
          (#x100057e "Armenian_vev") ;U+057E ARMENIAN SMALL LETTER VEW
          (#x100054f "Armenian_TYUN") ;U+054F ARMENIAN CAPITAL LETTER TIWN
          (#x100057f "Armenian_tyun") ;U+057F ARMENIAN SMALL LETTER TIWN
          (#x1000550 "Armenian_RE") ;U+0550 ARMENIAN CAPITAL LETTER REH
          (#x1000580 "Armenian_re") ;U+0580 ARMENIAN SMALL LETTER REH
          (#x1000551 "Armenian_TSO") ;U+0551 ARMENIAN CAPITAL LETTER CO
          (#x1000581 "Armenian_tso") ;U+0581 ARMENIAN SMALL LETTER CO
          (#x1000552 "Armenian_VYUN") ;U+0552 ARMENIAN CAPITAL LETTER YIWN
          (#x1000582 "Armenian_vyun") ;U+0582 ARMENIAN SMALL LETTER YIWN
          (#x1000553 "Armenian_PYUR") ;U+0553 ARMENIAN CAPITAL LETTER PIWR
          (#x1000583 "Armenian_pyur") ;U+0583 ARMENIAN SMALL LETTER PIWR
          (#x1000554 "Armenian_KE") ;U+0554 ARMENIAN CAPITAL LETTER KEH
          (#x1000584 "Armenian_ke") ;U+0584 ARMENIAN SMALL LETTER KEH
          (#x1000555 "Armenian_O") ;U+0555 ARMENIAN CAPITAL LETTER OH
          (#x1000585 "Armenian_o") ;U+0585 ARMENIAN SMALL LETTER OH
          (#x1000556 "Armenian_FE") ;U+0556 ARMENIAN CAPITAL LETTER FEH
          (#x1000586 "Armenian_fe") ;U+0586 ARMENIAN SMALL LETTER FEH
          (#x100055a "Armenian_apostrophe") ;U+055A ARMENIAN APOSTROPHE
          (#x10010d0 "Georgian_an") ;U+10D0 GEORGIAN LETTER AN
          (#x10010d1 "Georgian_ban") ;U+10D1 GEORGIAN LETTER BAN
          (#x10010d2 "Georgian_gan") ;U+10D2 GEORGIAN LETTER GAN
          (#x10010d3 "Georgian_don") ;U+10D3 GEORGIAN LETTER DON
          (#x10010d4 "Georgian_en") ;U+10D4 GEORGIAN LETTER EN
          (#x10010d5 "Georgian_vin") ;U+10D5 GEORGIAN LETTER VIN
          (#x10010d6 "Georgian_zen") ;U+10D6 GEORGIAN LETTER ZEN
          (#x10010d7 "Georgian_tan") ;U+10D7 GEORGIAN LETTER TAN
          (#x10010d8 "Georgian_in") ;U+10D8 GEORGIAN LETTER IN
          (#x10010d9 "Georgian_kan") ;U+10D9 GEORGIAN LETTER KAN
          (#x10010da "Georgian_las") ;U+10DA GEORGIAN LETTER LAS
          (#x10010db "Georgian_man") ;U+10DB GEORGIAN LETTER MAN
          (#x10010dc "Georgian_nar") ;U+10DC GEORGIAN LETTER NAR
          (#x10010dd "Georgian_on") ;U+10DD GEORGIAN LETTER ON
          (#x10010de "Georgian_par") ;U+10DE GEORGIAN LETTER PAR
          (#x10010df "Georgian_zhar") ;U+10DF GEORGIAN LETTER ZHAR
          (#x10010e0 "Georgian_rae") ;U+10E0 GEORGIAN LETTER RAE
          (#x10010e1 "Georgian_san") ;U+10E1 GEORGIAN LETTER SAN
          (#x10010e2 "Georgian_tar") ;U+10E2 GEORGIAN LETTER TAR
          (#x10010e3 "Georgian_un") ;U+10E3 GEORGIAN LETTER UN
          (#x10010e4 "Georgian_phar") ;U+10E4 GEORGIAN LETTER PHAR
          (#x10010e5 "Georgian_khar") ;U+10E5 GEORGIAN LETTER KHAR
          (#x10010e6 "Georgian_ghan") ;U+10E6 GEORGIAN LETTER GHAN
          (#x10010e7 "Georgian_qar") ;U+10E7 GEORGIAN LETTER QAR
          (#x10010e8 "Georgian_shin") ;U+10E8 GEORGIAN LETTER SHIN
          (#x10010e9 "Georgian_chin") ;U+10E9 GEORGIAN LETTER CHIN
          (#x10010ea "Georgian_can") ;U+10EA GEORGIAN LETTER CAN
          (#x10010eb "Georgian_jil") ;U+10EB GEORGIAN LETTER JIL
          (#x10010ec "Georgian_cil") ;U+10EC GEORGIAN LETTER CIL
          (#x10010ed "Georgian_char") ;U+10ED GEORGIAN LETTER CHAR
          (#x10010ee "Georgian_xan") ;U+10EE GEORGIAN LETTER XAN
          (#x10010ef "Georgian_jhan") ;U+10EF GEORGIAN LETTER JHAN
          (#x10010f0 "Georgian_hae") ;U+10F0 GEORGIAN LETTER HAE
          (#x10010f1 "Georgian_he") ;U+10F1 GEORGIAN LETTER HE
          (#x10010f2 "Georgian_hie") ;U+10F2 GEORGIAN LETTER HIE
          (#x10010f3 "Georgian_we") ;U+10F3 GEORGIAN LETTER WE
          (#x10010f4 "Georgian_har") ;U+10F4 GEORGIAN LETTER HAR
          (#x10010f5 "Georgian_hoe") ;U+10F5 GEORGIAN LETTER HOE
          (#x10010f6 "Georgian_fi") ;U+10F6 GEORGIAN LETTER FI
          (#x1001e8a "Xabovedot") ;U+1E8A LATIN CAPITAL LETTER X WITH DOT ABOVE
          (#x100012c "Ibreve") ;U+012C LATIN CAPITAL LETTER I WITH BREVE
          (#x10001b5 "Zstroke") ;U+01B5 LATIN CAPITAL LETTER Z WITH STROKE
          (#x10001e6 "Gcaron") ;U+01E6 LATIN CAPITAL LETTER G WITH CARON
          (#x10001d1 "Ocaron") ;U+01D2 LATIN CAPITAL LETTER O WITH CARON
          (#x100019f "Obarred") ;U+019F LATIN CAPITAL LETTER O WITH MIDDLE TILDE
          (#x1001e8b "xabovedot") ;U+1E8B LATIN SMALL LETTER X WITH DOT ABOVE
          (#x100012d "ibreve") ;U+012D LATIN SMALL LETTER I WITH BREVE
          (#x10001b6 "zstroke") ;U+01B6 LATIN SMALL LETTER Z WITH STROKE
          (#x10001e7 "gcaron") ;U+01E7 LATIN SMALL LETTER G WITH CARON
          (#x10001d2 "ocaron") ;U+01D2 LATIN SMALL LETTER O WITH CARON
          (#x1000275 "obarred") ;U+0275 LATIN SMALL LETTER BARRED O
          (#x100018f "SCHWA")   ;U+018F LATIN CAPITAL LETTER SCHWA
          (#x1000259 "schwa")     ;U+0259 LATIN SMALL LETTER SCHWA
          (#x1001e36 "Lbelowdot") ;U+1E36 LATIN CAPITAL LETTER L WITH DOT BELOW
          (#x1001e37 "lbelowdot") ;U+1E37 LATIN SMALL LETTER L WITH DOT BELOW
          (#x1001ea0 "Abelowdot") ;U+1EA0 LATIN CAPITAL LETTER A WITH DOT BELOW
          (#x1001ea1 "abelowdot") ;U+1EA1 LATIN SMALL LETTER A WITH DOT BELOW
          (#x1001ea2 "Ahook") ;U+1EA2 LATIN CAPITAL LETTER A WITH HOOK ABOVE
          (#x1001ea3 "ahook") ;U+1EA3 LATIN SMALL LETTER A WITH HOOK ABOVE
          (#x1001ea4 "Acircumflexacute") ;U+1EA4 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
          (#x1001ea5 "acircumflexacute") ;U+1EA5 LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
          (#x1001ea6 "Acircumflexgrave") ;U+1EA6 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
          (#x1001ea7 "acircumflexgrave") ;U+1EA7 LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
          (#x1001ea8 "Acircumflexhook") ;U+1EA8 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
          (#x1001ea9 "acircumflexhook") ;U+1EA9 LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
          (#x1001eaa "Acircumflextilde") ;U+1EAA LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
          (#x1001eab "acircumflextilde") ;U+1EAB LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE
          (#x1001eac "Acircumflexbelowdot") ;U+1EAC LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
          (#x1001ead "acircumflexbelowdot") ;U+1EAD LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
          (#x1001eae "Abreveacute") ;U+1EAE LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
          (#x1001eaf "abreveacute") ;U+1EAF LATIN SMALL LETTER A WITH BREVE AND ACUTE
          (#x1001eb0 "Abrevegrave") ;U+1EB0 LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
          (#x1001eb1 "abrevegrave") ;U+1EB1 LATIN SMALL LETTER A WITH BREVE AND GRAVE
          (#x1001eb2 "Abrevehook") ;U+1EB2 LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
          (#x1001eb3 "abrevehook") ;U+1EB3 LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE
          (#x1001eb4 "Abrevetilde") ;U+1EB4 LATIN CAPITAL LETTER A WITH BREVE AND TILDE
          (#x1001eb5 "abrevetilde") ;U+1EB5 LATIN SMALL LETTER A WITH BREVE AND TILDE
          (#x1001eb6 "Abrevebelowdot") ;U+1EB6 LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
          (#x1001eb7 "abrevebelowdot") ;U+1EB7 LATIN SMALL LETTER A WITH BREVE AND DOT BELOW
          (#x1001eb8 "Ebelowdot") ;U+1EB8 LATIN CAPITAL LETTER E WITH DOT BELOW
          (#x1001eb9 "ebelowdot") ;U+1EB9 LATIN SMALL LETTER E WITH DOT BELOW
          (#x1001eba "Ehook") ;U+1EBA LATIN CAPITAL LETTER E WITH HOOK ABOVE
          (#x1001ebb "ehook") ;U+1EBB LATIN SMALL LETTER E WITH HOOK ABOVE
          (#x1001ebc "Etilde") ;U+1EBC LATIN CAPITAL LETTER E WITH TILDE
          (#x1001ebd "etilde") ;U+1EBD LATIN SMALL LETTER E WITH TILDE
          (#x1001ebe "Ecircumflexacute") ;U+1EBE LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
          (#x1001ebf "ecircumflexacute") ;U+1EBF LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
          (#x1001ec0 "Ecircumflexgrave") ;U+1EC0 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
          (#x1001ec1 "ecircumflexgrave") ;U+1EC1 LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
          (#x1001ec2 "Ecircumflexhook") ;U+1EC2 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
          (#x1001ec3 "ecircumflexhook") ;U+1EC3 LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
          (#x1001ec4 "Ecircumflextilde") ;U+1EC4 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
          (#x1001ec5 "ecircumflextilde") ;U+1EC5 LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE
          (#x1001ec6 "Ecircumflexbelowdot") ;U+1EC6 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
          (#x1001ec7 "ecircumflexbelowdot") ;U+1EC7 LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
          (#x1001ec8 "Ihook") ;U+1EC8 LATIN CAPITAL LETTER I WITH HOOK ABOVE
          (#x1001ec9 "ihook") ;U+1EC9 LATIN SMALL LETTER I WITH HOOK ABOVE
          (#x1001eca "Ibelowdot") ;U+1ECA LATIN CAPITAL LETTER I WITH DOT BELOW
          (#x1001ecb "ibelowdot") ;U+1ECB LATIN SMALL LETTER I WITH DOT BELOW
          (#x1001ecc "Obelowdot") ;U+1ECC LATIN CAPITAL LETTER O WITH DOT BELOW
          (#x1001ecd "obelowdot") ;U+1ECD LATIN SMALL LETTER O WITH DOT BELOW
          (#x1001ece "Ohook") ;U+1ECE LATIN CAPITAL LETTER O WITH HOOK ABOVE
          (#x1001ecf "ohook") ;U+1ECF LATIN SMALL LETTER O WITH HOOK ABOVE
          (#x1001ed0 "Ocircumflexacute") ;U+1ED0 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
          (#x1001ed1 "ocircumflexacute") ;U+1ED1 LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
          (#x1001ed2 "Ocircumflexgrave") ;U+1ED2 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
          (#x1001ed3 "ocircumflexgrave") ;U+1ED3 LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
          (#x1001ed4 "Ocircumflexhook") ;U+1ED4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
          (#x1001ed5 "ocircumflexhook") ;U+1ED5 LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
          (#x1001ed6 "Ocircumflextilde") ;U+1ED6 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
          (#x1001ed7 "ocircumflextilde") ;U+1ED7 LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE
          (#x1001ed8 "Ocircumflexbelowdot") ;U+1ED8 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
          (#x1001ed9 "ocircumflexbelowdot") ;U+1ED9 LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
          (#x1001eda "Ohornacute") ;U+1EDA LATIN CAPITAL LETTER O WITH HORN AND ACUTE
          (#x1001edb "ohornacute") ;U+1EDB LATIN SMALL LETTER O WITH HORN AND ACUTE
          (#x1001edc "Ohorngrave") ;U+1EDC LATIN CAPITAL LETTER O WITH HORN AND GRAVE
          (#x1001edd "ohorngrave") ;U+1EDD LATIN SMALL LETTER O WITH HORN AND GRAVE
          (#x1001ede "Ohornhook") ;U+1EDE LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
          (#x1001edf "ohornhook") ;U+1EDF LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE
          (#x1001ee0 "Ohorntilde") ;U+1EE0 LATIN CAPITAL LETTER O WITH HORN AND TILDE
          (#x1001ee1 "ohorntilde") ;U+1EE1 LATIN SMALL LETTER O WITH HORN AND TILDE
          (#x1001ee2 "Ohornbelowdot") ;U+1EE2 LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
          (#x1001ee3 "ohornbelowdot") ;U+1EE3 LATIN SMALL LETTER O WITH HORN AND DOT BELOW
          (#x1001ee4 "Ubelowdot") ;U+1EE4 LATIN CAPITAL LETTER U WITH DOT BELOW
          (#x1001ee5 "ubelowdot") ;U+1EE5 LATIN SMALL LETTER U WITH DOT BELOW
          (#x1001ee6 "Uhook") ;U+1EE6 LATIN CAPITAL LETTER U WITH HOOK ABOVE
          (#x1001ee7 "uhook") ;U+1EE7 LATIN SMALL LETTER U WITH HOOK ABOVE
          (#x1001ee8 "Uhornacute") ;U+1EE8 LATIN CAPITAL LETTER U WITH HORN AND ACUTE
          (#x1001ee9 "uhornacute") ;U+1EE9 LATIN SMALL LETTER U WITH HORN AND ACUTE
          (#x1001eea "Uhorngrave") ;U+1EEA LATIN CAPITAL LETTER U WITH HORN AND GRAVE
          (#x1001eeb "uhorngrave") ;U+1EEB LATIN SMALL LETTER U WITH HORN AND GRAVE
          (#x1001eec "Uhornhook") ;U+1EEC LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
          (#x1001eed "uhornhook") ;U+1EED LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE
          (#x1001eee "Uhorntilde") ;U+1EEE LATIN CAPITAL LETTER U WITH HORN AND TILDE
          (#x1001eef "uhorntilde") ;U+1EEF LATIN SMALL LETTER U WITH HORN AND TILDE
          (#x1001ef0 "Uhornbelowdot") ;U+1EF0 LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
          (#x1001ef1 "uhornbelowdot") ;U+1EF1 LATIN SMALL LETTER U WITH HORN AND DOT BELOW
          (#x1001ef4 "Ybelowdot") ;U+1EF4 LATIN CAPITAL LETTER Y WITH DOT BELOW
          (#x1001ef5 "ybelowdot") ;U+1EF5 LATIN SMALL LETTER Y WITH DOT BELOW
          (#x1001ef6 "Yhook") ;U+1EF6 LATIN CAPITAL LETTER Y WITH HOOK ABOVE
          (#x1001ef7 "yhook") ;U+1EF7 LATIN SMALL LETTER Y WITH HOOK ABOVE
          (#x1001ef8 "Ytilde") ;U+1EF8 LATIN CAPITAL LETTER Y WITH TILDE
          (#x1001ef9 "ytilde") ;U+1EF9 LATIN SMALL LETTER Y WITH TILDE
          (#x10001a0 "Ohorn") ;U+01A0 LATIN CAPITAL LETTER O WITH HORN
          (#x10001a1 "ohorn") ;U+01A1 LATIN SMALL LETTER O WITH HORN
          (#x10001af "Uhorn") ;U+01AF LATIN CAPITAL LETTER U WITH HORN
          (#x10001b0 "uhorn") ;U+01B0 LATIN SMALL LETTER U WITH HORN
          (#x10020a0 "EcuSign")     ;U+20A0 EURO-CURRENCY SIGN
          (#x10020a1 "ColonSign")   ;U+20A1 COLON SIGN
          (#x10020a2 "CruzeiroSign") ;U+20A2 CRUZEIRO SIGN
          (#x10020a3 "FFrancSign")  ;U+20A3 FRENCH FRANC SIGN
          (#x10020a4 "LiraSign")    ;U+20A4 LIRA SIGN
          (#x10020a5 "MillSign")    ;U+20A5 MILL SIGN
          (#x10020a6 "NairaSign")   ;U+20A6 NAIRA SIGN
          (#x10020a7 "PesetaSign")  ;U+20A7 PESETA SIGN
          (#x10020a8 "RupeeSign")   ;U+20A8 RUPEE SIGN
          (#x10020a9 "WonSign")     ;U+20A9 WON SIGN
          (#x10020aa "NewSheqelSign") ;U+20AA NEW SHEQEL SIGN
          (#x10020ab "DongSign")    ;U+20AB DONG SIGN
          (#x20ac "EuroSign")       ;U+20AC EURO SIGN
          (#x1002070 "zerosuperior") ;U+2070 SUPERSCRIPT ZERO
          (#x1002074 "foursuperior") ;U+2074 SUPERSCRIPT FOUR
          (#x1002075 "fivesuperior") ;U+2075 SUPERSCRIPT FIVE
          (#x1002076 "sixsuperior") ;U+2076 SUPERSCRIPT SIX
          (#x1002077 "sevensuperior") ;U+2077 SUPERSCRIPT SEVEN
          (#x1002078 "eightsuperior") ;U+2078 SUPERSCRIPT EIGHT
          (#x1002079 "ninesuperior") ;U+2079 SUPERSCRIPT NINE
          (#x1002080 "zerosubscript") ;U+2080 SUBSCRIPT ZERO
          (#x1002081 "onesubscript") ;U+2081 SUBSCRIPT ONE
          (#x1002082 "twosubscript") ;U+2082 SUBSCRIPT TWO
          (#x1002083 "threesubscript") ;U+2083 SUBSCRIPT THREE
          (#x1002084 "foursubscript") ;U+2084 SUBSCRIPT FOUR
          (#x1002085 "fivesubscript") ;U+2085 SUBSCRIPT FIVE
          (#x1002086 "sixsubscript") ;U+2086 SUBSCRIPT SIX
          (#x1002087 "sevensubscript") ;U+2087 SUBSCRIPT SEVEN
          (#x1002088 "eightsubscript") ;U+2088 SUBSCRIPT EIGHT
          (#x1002089 "ninesubscript") ;U+2089 SUBSCRIPT NINE
          (#x1002202 "partdifferential") ;U+2202 PARTIAL DIFFERENTIAL
          (#x1002205 "emptyset")    ;U+2205 NULL SET
          (#x1002208 "elementof")   ;U+2208 ELEMENT OF
          (#x1002209 "notelementof") ;U+2209 NOT AN ELEMENT OF
          (#x100220B "containsas")  ;U+220B CONTAINS AS MEMBER
          (#x100221A "squareroot")  ;U+221A SQUARE ROOT
          (#x100221B "cuberoot")    ;U+221B CUBE ROOT
          (#x100221C "fourthroot")  ;U+221C FOURTH ROOT
          (#x100222C "dintegral")   ;U+222C DOUBLE INTEGRAL
          (#x100222D "tintegral")   ;U+222D TRIPLE INTEGRAL
          (#x1002235 "because")     ;U+2235 BECAUSE
          (#x1002248 "approxeq")    ;U+2245 ALMOST EQUAL TO
          (#x1002247 "notapproxeq") ;U+2247 NOT ALMOST EQUAL TO
          (#x1002262 "notidentical") ;U+2262 NOT IDENTICAL TO
          (#x1002263 "stricteq")    ;U+2263 STRICTLY EQUIVALENT TO
          ;; extended keysyms
          ;; (#x100000A8 "hpmute_acute")
          ;; (#x100000A9 "hpmute_grave")
          ;; (#x100000AA "hpmute_asciicircum")
          ;; (#x100000AB "hpmute_diaeresis")
          ;; (#x100000AC "hpmute_asciitilde")
          ;; (#x100000AF "hplira")
          ;; (#x100000BE "hpguilder")
          ;; (#x100000EE "hpYdiaeresis")
          ;; (#x100000EE "hpIO")
          ;; (#x100000F6 "hplongminus")
          ;; (#x100000FC "hpblock")
          ;; (#x1000FF00 "apLineDel")
          ;; (#x1000FF01 "apCharDel")
          ;; (#x1000FF02 "apCopy")
          ;; (#x1000FF03 "apCut")
          ;; (#x1000FF04 "apPaste")
          ;; (#x1000FF05 "apMove")
          ;; (#x1000FF06 "apGrow")
          ;; (#x1000FF07 "apCmd")
          ;; (#x1000FF08 "apShell")
          ;; (#x1000FF09 "apLeftBar")
          ;; (#x1000FF0A "apRightBar")
          ;; (#x1000FF0B "apLeftBox")
          ;; (#x1000FF0C "apRightBox")
          ;; (#x1000FF0D "apUpBox")
          ;; (#x1000FF0E "apDownBox")
          ;; (#x1000FF0F "apPop")
          ;; (#x1000FF10 "apRead")
          ;; (#x1000FF11 "apEdit")
          ;; (#x1000FF12 "apSave")
          ;; (#x1000FF13 "apExit")
          ;; (#x1000FF14 "apRepeat")
          ;; (#x1000FF48 "hpModelock1")
          ;; (#x1000FF49 "hpModelock2")
          ;; (#x1000FF6C "hpReset")
          ;; (#x1000FF6D "hpSystem")
          ;; (#x1000FF6E "hpUser")
          ;; (#x1000FF6F "hpClearLine")
          ;; (#x1000FF70 "hpInsertLine")
          ;; (#x1000FF71 "hpDeleteLine")
          ;; (#x1000FF72 "hpInsertChar")
          ;; (#x1000FF73 "hpDeleteChar")
          ;; (#x1000FF74 "hpBackTab")
          ;; (#x1000FF75 "hpKP_BackTab")
          ;; (#x1000FFA8 "apKP_parenleft")
          ;; (#x1000FFA9 "apKP_parenright")
          ;; (#x10004001 "I2ND_FUNC_L")
          ;; (#x10004002 "I2ND_FUNC_R")
          ;; (#x10004003 "IREMOVE")
          ;; (#x10004004 "IREPEAT")
          ;; (#x10004101 "IA1")
          ;; (#x10004102 "IA2")
          ;; (#x10004103 "IA3")
          ;; (#x10004104 "IA4")
          ;; (#x10004105 "IA5")
          ;; (#x10004106 "IA6")
          ;; (#x10004107 "IA7")
          ;; (#x10004108 "IA8")
          ;; (#x10004109 "IA9")
          ;; (#x1000410A "IA10")
          ;; (#x1000410B "IA11")
          ;; (#x1000410C "IA12")
          ;; (#x1000410D "IA13")
          ;; (#x1000410E "IA14")
          ;; (#x1000410F "IA15")
          ;; (#x10004201 "IB1")
          ;; (#x10004202 "IB2")
          ;; (#x10004203 "IB3")
          ;; (#x10004204 "IB4")
          ;; (#x10004205 "IB5")
          ;; (#x10004206 "IB6")
          ;; (#x10004207 "IB7")
          ;; (#x10004208 "IB8")
          ;; (#x10004209 "IB9")
          ;; (#x1000420A "IB10")
          ;; (#x1000420B "IB11")
          ;; (#x1000420C "IB12")
          ;; (#x1000420D "IB13")
          ;; (#x1000420E "IB14")
          ;; (#x1000420F "IB15")
          ;; (#x10004210 "IB16")
          ;; (#x1000FF00 "DRemove")
          ;; (#x1000FEB0 "Dring_accent")
          ;; (#x1000FE5E "Dcircumflex_accent")
          ;; (#x1000FE2C "Dcedilla_accent")
          ;; (#x1000FE27 "Dacute_accent")
          ;; (#x1000FE60 "Dgrave_accent")
          ;; (#x1000FE7E "Dtilde")
          ;; (#x1000FE22 "Ddiaeresis")
          ;; (#x1004FF02 "osfCopy")
          ;; (#x1004FF03 "osfCut")
          ;; (#x1004FF04 "osfPaste")
          ;; (#x1004FF07 "osfBackTab")
          ;; (#x1004FF08 "osfBackSpace")
          ;; (#x1004FF0B "osfClear")
          ;; (#x1004FF1B "osfEscape")
          ;; (#x1004FF31 "osfAddMode")
          ;; (#x1004FF32 "osfPrimaryPaste")
          ;; (#x1004FF33 "osfQuickPaste")
          ;; (#x1004FF40 "osfPageLeft")
          ;; (#x1004FF41 "osfPageUp")
          ;; (#x1004FF42 "osfPageDown")
          ;; (#x1004FF43 "osfPageRight")
          ;; (#x1004FF44 "osfActivate")
          ;; (#x1004FF45 "osfMenuBar")
          ;; (#x1004FF51 "osfLeft")
          ;; (#x1004FF52 "osfUp")
          ;; (#x1004FF53 "osfRight")
          ;; (#x1004FF54 "osfDown")
          ;; (#x1004FF55 "osfPrior")
          ;; (#x1004FF56 "osfNext")
          ;; (#x1004FF57 "osfEndLine")
          ;; (#x1004FF58 "osfBeginLine")
          ;; (#x1004FF59 "osfEndData")
          ;; (#x1004FF5A "osfBeginData")
          ;; (#x1004FF5B "osfPrevMenu")
          ;; (#x1004FF5C "osfNextMenu")
          ;; (#x1004FF5D "osfPrevField")
          ;; (#x1004FF5E "osfNextField")
          ;; (#x1004FF60 "osfSelect")
          ;; (#x1004FF63 "osfInsert")
          ;; (#x1004FF65 "osfUndo")
          ;; (#x1004FF67 "osfMenu")
          ;; (#x1004FF69 "osfCancel")
          ;; (#x1004FF6A "osfHelp")
          ;; (#x1004FF71 "osfSelectAll")
          ;; (#x1004FF72 "osfDeselectAll")
          ;; (#x1004FF73 "osfReselect")
          ;; (#x1004FF74 "osfExtend")
          ;; (#x1004FF78 "osfRestore")
          ;; (#x1004FF7E "osfSwitchDirection")
          ;; (#x1004FFF5 "osfPriorMinor")
          ;; (#x1004FFF6 "osfNextMinor")
          ;; (#x1004FFF7 "osfRightLine")
          ;; (#x1004FFF8 "osfLeftLine")
          ;; (#x1004FFFF "osfDelete")
          ;; (#x1005FF00 "SunFA_Grave")
          ;; (#x1005FF01 "SunFA_Circum")
          ;; (#x1005FF02 "SunFA_Tilde")
          ;; (#x1005FF03 "SunFA_Acute")
          ;; (#x1005FF04 "SunFA_Diaeresis")
          ;; (#x1005FF05 "SunFA_Cedilla")
          ;; (#x1005FF10 "SunF36")
          ;; (#x1005FF11 "SunF37")
          ;; (#x1005FF60 "SunSys_Req")
          ;; (#x1005FF70 "SunProps")
          ;; (#x1005FF71 "SunFront")
          ;; (#x1005FF72 "SunCopy")
          ;; (#x1005FF73 "SunOpen")
          ;; (#x1005FF74 "SunPaste")
          ;; (#x1005FF75 "SunCut")
          ;; (#x1005FF76 "SunPowerSwitch")
          ;; (#x1005FF77 "SunAudioLowerVolume")
          ;; (#x1005FF78 "SunAudioMute")
          ;; (#x1005FF79 "SunAudioRaiseVolume")
          ;; (#x1005FF7A "SunVideoDegauss")
          ;; (#x1005FF7B "SunVideoLowerBrightness")
          ;; (#x1005FF7C "SunVideoRaiseBrightness")
          ;; (#x1005FF7D "SunPowerSwitchShift")
          ;; (#xFF20 "SunCompose")
          ;; (#xFF55 "SunPageUp")
          ;; (#xFF56 "SunPageDown")
          ;; (#xFF61 "SunPrint_Screen")
          ;; (#xFF65 "SunUndo")
          ;; (#xFF66 "SunAgain")
          ;; (#xFF68 "SunFind")
          ;; (#xFF69 "SunStop")
          ;; (#xFF7E "SunAltGraph")
          ;; (#x1006FF00 "WYSetup")
          ;; (#x1006FF00 "ncdSetup")
          ;; (#x10070001 "XeroxPointerButton1")
          ;; (#x10070002 "XeroxPointerButton2")
          ;; (#x10070003 "XeroxPointerButton3")
          ;; (#x10070004 "XeroxPointerButton4")
          ;; (#x10070005 "XeroxPointerButton5")
          ;; (#x1008FF01 "XF86ModeLock")
          ;; (#x1008FF10 "XF86Standby")
          ;; (#x1008FF11 "XF86AudioLowerVolume")
          ;; (#x1008FF12 "XF86AudioMute")
          ;; (#x1008FF13 "XF86AudioRaiseVolume")
          ;; (#x1008FF14 "XF86AudioPlay")
          ;; (#x1008FF15 "XF86AudioStop")
          ;; (#x1008FF16 "XF86AudioPrev")
          ;; (#x1008FF17 "XF86AudioNext")
          ;; (#x1008FF18 "XF86HomePage")
          ;; (#x1008FF19 "XF86Mail")
          ;; (#x1008FF1A "XF86Start")
          ;; (#x1008FF1B "XF86Search")
          ;; (#x1008FF1C "XF86AudioRecord")
          ;; (#x1008FF1D "XF86Calculator")
          ;; (#x1008FF1E "XF86Memo")
          ;; (#x1008FF1F "XF86ToDoList")
          ;; (#x1008FF20 "XF86Calendar")
          ;; (#x1008FF21 "XF86PowerDown")
          ;; (#x1008FF22 "XF86ContrastAdjust")
          ;; (#x1008FF23 "XF86RockerUp")
          ;; (#x1008FF24 "XF86RockerDown")
          ;; (#x1008FF25 "XF86RockerEnter")
          ;; (#x1008FF26 "XF86Back")
          ;; (#x1008FF27 "XF86Forward")
          ;; (#x1008FF28 "XF86Stop")
          ;; (#x1008FF29 "XF86Refresh")
          ;; (#x1008FF2A "XF86PowerOff")
          ;; (#x1008FF2B "XF86WakeUp")
          ;; (#x1008FF2C "XF86Eject")
          ;; (#x1008FF2D "XF86ScreenSaver")
          ;; (#x1008FF2E "XF86WWW")
          ;; (#x1008FF2F "XF86Sleep")
          ;; (#x1008FF30 "XF86Favorites")
          ;; (#x1008FF31 "XF86AudioPause")
          ;; (#x1008FF32 "XF86AudioMedia")
          ;; (#x1008FF33 "XF86MyComputer")
          ;; (#x1008FF34 "XF86VendorHome")
          ;; (#x1008FF35 "XF86LightBulb")
          ;; (#x1008FF36 "XF86Shop")
          ;; (#x1008FF37 "XF86History")
          ;; (#x1008FF38 "XF86OpenURL")
          ;; (#x1008FF39 "XF86AddFavorite")
          ;; (#x1008FF3A "XF86HotLinks")
          ;; (#x1008FF3B "XF86BrightnessAdjust")
          ;; (#x1008FF3C "XF86Finance")
          ;; (#x1008FF3D "XF86Community")
          ;; (#x1008FF3E "XF86AudioRewind")
          ;; (#x1008FF3F "XF86BackForward")
          ;; (#x1008FF40 "XF86Launch0")
          ;; (#x1008FF41 "XF86Launch1")
          ;; (#x1008FF42 "XF86Launch2")
          ;; (#x1008FF43 "XF86Launch3")
          ;; (#x1008FF44 "XF86Launch4")
          ;; (#x1008FF45 "XF86Launch5")
          ;; (#x1008FF46 "XF86Launch6")
          ;; (#x1008FF47 "XF86Launch7")
          ;; (#x1008FF48 "XF86Launch8")
          ;; (#x1008FF49 "XF86Launch9")
          ;; (#x1008FF4A "XF86LaunchA")
          ;; (#x1008FF4B "XF86LaunchB")
          ;; (#x1008FF4C "XF86LaunchC")
          ;; (#x1008FF4D "XF86LaunchD")
          ;; (#x1008FF4E "XF86LaunchE")
          ;; (#x1008FF4F "XF86LaunchF")
          ;; (#x1008FF50 "XF86ApplicationLeft")
          ;; (#x1008FF51 "XF86ApplicationRight")
          ;; (#x1008FF52 "XF86Book")
          ;; (#x1008FF53 "XF86CD")
          ;; (#x1008FF54 "XF86Calculater")
          ;; (#x1008FF55 "XF86Clear")
          ;; (#x1008FF56 "XF86Close")
          ;; (#x1008FF57 "XF86Copy")
          ;; (#x1008FF58 "XF86Cut")
          ;; (#x1008FF59 "XF86Display")
          ;; (#x1008FF5A "XF86DOS")
          ;; (#x1008FF5B "XF86Documents")
          ;; (#x1008FF5C "XF86Excel")
          ;; (#x1008FF5D "XF86Explorer")
          ;; (#x1008FF5E "XF86Game")
          ;; (#x1008FF5F "XF86Go")
          ;; (#x1008FF60 "XF86iTouch")
          ;; (#x1008FF61 "XF86LogOff")
          ;; (#x1008FF62 "XF86Market")
          ;; (#x1008FF63 "XF86Meeting")
          ;; (#x1008FF65 "XF86MenuKB")
          ;; (#x1008FF66 "XF86MenuPB")
          ;; (#x1008FF67 "XF86MySites")
          ;; (#x1008FF68 "XF86New")
          ;; (#x1008FF69 "XF86News")
          ;; (#x1008FF6A "XF86OfficeHome")
          ;; (#x1008FF6B "XF86Open")
          ;; (#x1008FF6C "XF86Option")
          ;; (#x1008FF6D "XF86Paste")
          ;; (#x1008FF6E "XF86Phone")
          ;; (#x1008FF70 "XF86Q")
          ;; (#x1008FF72 "XF86Reply")
          ;; (#x1008FF73 "XF86Reload")
          ;; (#x1008FF74 "XF86RotateWindows")
          ;; (#x1008FF75 "XF86RotationPB")
          ;; (#x1008FF76 "XF86RotationKB")
          ;; (#x1008FF77 "XF86Save")
          ;; (#x1008FF78 "XF86ScrollUp")
          ;; (#x1008FF79 "XF86ScrollDown")
          ;; (#x1008FF7A "XF86ScrollClick")
          ;; (#x1008FF7B "XF86Send")
          ;; (#x1008FF7C "XF86Spell")
          ;; (#x1008FF7D "XF86SplitScreen")
          ;; (#x1008FF7E "XF86Support")
          ;; (#x1008FF7F "XF86TaskPane")
          ;; (#x1008FF80 "XF86Terminal")
          ;; (#x1008FF81 "XF86Tools")
          ;; (#x1008FF82 "XF86Travel")
          ;; (#x1008FF84 "XF86UserPB")
          ;; (#x1008FF85 "XF86User1KB")
          ;; (#x1008FF86 "XF86User2KB")
          ;; (#x1008FF87 "XF86Video")
          ;; (#x1008FF88 "XF86WheelButton")
          ;; (#x1008FF89 "XF86Word")
          ;; (#x1008FF8A "XF86Xfer")
          ;; (#x1008FF8B "XF86ZoomIn")
          ;; (#x1008FF8C "XF86ZoomOut")
          ;; (#x1008FF8D "XF86Away")
          ;; (#x1008FF8E "XF86Messenger")
          ;; (#x1008FF8F "XF86WebCam")
          ;; (#x1008FF90 "XF86MailForward")
          ;; (#x1008FF91 "XF86Pictures")
          ;; (#x1008FF92 "XF86Music")
          ;; (#x1008FE01 "XF86_Switch_VT_1")
          ;; (#x1008FE02 "XF86_Switch_VT_2")
          ;; (#x1008FE03 "XF86_Switch_VT_3")
          ;; (#x1008FE04 "XF86_Switch_VT_4")
          ;; (#x1008FE05 "XF86_Switch_VT_5")
          ;; (#x1008FE06 "XF86_Switch_VT_6")
          ;; (#x1008FE07 "XF86_Switch_VT_7")
          ;; (#x1008FE08 "XF86_Switch_VT_8")
          ;; (#x1008FE09 "XF86_Switch_VT_9")
          ;; (#x1008FE0A "XF86_Switch_VT_10")
          ;; (#x1008FE0B "XF86_Switch_VT_11")
          ;; (#x1008FE0C "XF86_Switch_VT_12")
          ;; (#x1008FE20 "XF86_Ungrab")
          ;; (#x1008FE21 "XF86_ClearGrab")
          ;; (#x1008FE22 "XF86_Next_VMode")
          ;; (#x1008FE23 "XF86_Prev_VMode")
          ;; (#x100000A8 "usldead_acute")
          ;; (#x100000A9 "usldead_grave")
          ;; (#x100000AB "usldead_diaeresis")
          ;; (#x100000AA "usldead_asciicircum")
          ;; (#x100000AC "usldead_asciitilde")
          ;; (#x1000FE2C "usldead_cedilla")
          ;; (#x1000FEB0 "usldead_ring")
          ))

;; Commentary:
;;
;; Translate between stumpwm key names and keysym names.
;;
;; Code:

(defvar *stumpwm-name->keysym-name-translations* (make-hash-table :test #'equal)
  "Hashtable mapping from stumpwm key names to keysym names.")

(defun define-keysym-name (stumpwm-name keysym-name)
  "Define a mapping from a STUMPWM-NAME to KEYSYM-NAME.
This function is used to translate Emacs-like names to keysym
names."
  (setf (gethash stumpwm-name *stumpwm-name->keysym-name-translations*)
        keysym-name))

(defun stumpwm-name->keysym-name (stumpwm-name)
  (multiple-value-bind (value present-p)
      (gethash stumpwm-name *stumpwm-name->keysym-name-translations*)
    (declare (ignore present-p))
    value))

(defun keysym-name->stumpwm-name (keysym-name)
  (maphash (lambda (k v)
             (when (equal v keysym-name)
               (return-from keysym-name->stumpwm-name k)))
           *stumpwm-name->keysym-name-translations*))

(defun stumpwm-name->keysym (stumpwm-name)
  "Return the keysym corresponding to STUMPWM-NAME.
If no mapping for STUMPWM-NAME exists, then fallback by calling
KEYSYM-NAME->KEYSYM."
  (let ((keysym-name (stumpwm-name->keysym-name stumpwm-name)))
    (keysym-name->keysym (or keysym-name stumpwm-name))))

(defun keysym->stumpwm-name (keysym)
  "Return the stumpwm key name corresponding to KEYSYM.
If no mapping for the stumpwm key name exists, then fall back by
calling KEYSYM->KEYSYM-NAME."
  (let ((keysym-name (keysym->keysym-name keysym)))
    (or (keysym-name->stumpwm-name keysym-name)
        keysym-name)))

(mapcar (lambda (args) (apply #'define-keysym-name args))
        '(("RET" "Return")
          ("ESC" "Escape")
          ("TAB" "Tab")
          ("DEL" "BackSpace")
          ("SPC" "space")
          ("!" "exclam")
          ("\"" "quotedbl")
          ("$" "dollar")
          ("%" "percent")
          ("&" "ampersand")
          ("'" "quoteright")   ;deprecated
          ("'" "apostrophe")
          ("`" "quoteleft")    ;deprecated
          ("`" "grave")
          ("&" "ampersand")
          ("(" "parenleft")
          (")" "parenright")
          ("*" "asterisk")
          ("+" "plus")
          ("," "comma")
          ("-" "minus")
          ("." "period")
          ("/" "slash")
          (":" "colon")
          (";" "semicolon")
          ("<" "less")
          ("=" "equal")
          (">" "greater")
          ("?" "question")
          ("@" "at")
          ("[" "bracketleft")
          ("\\" "backslash")
          ("]" "bracketright")
          ("^" "asciicircum")
          ("_" "underscore")
          ("#" "numbersign")
          ("{" "braceleft")
          ("|" "bar")
          ("}" "braceright")
          ("~" "asciitilde")))
