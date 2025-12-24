# SSY3825I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3825I.COB`

## ソースコード

```cobol
****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　出荷
*    業務名　　　　　　　：　ナフコ出荷支援
*    モジュール名　　　　：　箱数訂正入力
*    作成日／更新日　　　：　2015/06/15
*    作成者／更新者　　　：　ＮＡＶ
*    処理概要　　　　　　：　オンライン/本発共通。
*                            店舗毎に、計算（想定）箱数から
*                            実出荷箱数に訂正する。
*    修正履歴　　　　　　：
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY3825I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/06/15.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 表示ファイル >>--*
     SELECT   DSPFILE   ASSIGN         01-GS-DSPF
                        FORMAT         DSP-FMT
                        GROUP          DSP-GRP
                        PROCESSING     DSP-PRO
                        UNIT CONTROL   DSP-CON
                        FUNCTION       DSP-FNC
                        STATUS         DSP-ST.
*----<< 箱数ファイル >>--*
     SELECT   NFHAKOL4  ASSIGN         DA-01-VI-NFHAKOL4
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  HAK-F01   HAK-F05
                                       HAK-F08   HAK-F06
                                       HAK-F07
                        STATUS         NFHAKOL4-ST.
*---<<  担当者マスタ  >>---*
     SELECT   TANMS1    ASSIGN    TO        DA-01-VI-TANMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TAN-F01
                                                 TAN-F02
                        FILE      STATUS    IS   TANMS1-ST.
*---<<  条件ファイル  >>---*
     SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYK-F01
                                                 JYK-F02
                        FILE      STATUS    IS   JYOKEN1-ST.
*----<< 作場マスタ >>--*
     SELECT   SAKUBAL1  ASSIGN         DA-01-VI-SAKUBAL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SAK-F01
                        STATUS         SAKUBAL1-ST.
*----<< ナフコ店舗マスタ >>-*
     SELECT   NFTENMS1  ASSIGN    TO        DA-01-VI-NFTENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F01   TEN-F02
                        FILE      STATUS    NFTENMS1-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSY38251  OF        XMDLIB.
*----<< 箱数ファイル >>--*
 FD  NFHAKOL4           LABEL     RECORD   IS   STANDARD.
     COPY     NFHAKOF   OF        XFDLIB
              JOINING   HAK       PREFIX.
*---<<  担当者マスタ  >>---*
 FD  TANMS1.
     COPY     TANMS1    OF        XFDLIB
              JOINING   TAN       PREFIX.
*---<<  条件マスタ  >>---*
 FD  JYOKEN1.
     COPY     JYOKEN1   OF        XFDLIB
              JOINING   JYK       PREFIX.
*----<< 作場マスタ >>--*
 FD  SAKUBAL1            LABEL RECORD   IS   STANDARD.
     COPY     SAKUBAL1   OF        XFDLIB
              JOINING   SAK       PREFIX.
*----<< ナフコ店舗マスタ >>--*
 FD  NFTENMS1             LABEL RECORD   IS   STANDARD.
     COPY     NFTENMS1  OF        XFDLIB
              JOINING   TEN       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  KEP-FLG             PIC  X(01)  VALUE  SPACE.
     03  MEIMST-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG             PIC  X(03)  VALUE  SPACE.
     03  WK-B-KANRNO         PIC  9(08)  VALUE  ZERO.
     03  WK-B-SKBCD          PIC  X(02)  VALUE  SPACE.
     03  ERK-FLG             PIC  9(01)  VALUE  ZERO.
     03  WK-NOUHIN           PIC  9(08)  VALUE  ZERO.
     03  LNKSAIF-INV-FLG     PIC  X(03)  VALUE SPACE.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  NFHAKOL4-ST       PIC  X(02).
 01  TANMS1-ST         PIC  X(02).
 01  JYOKEN1-ST        PIC  X(02).
 01  SAKUBAL1-ST       PIC  X(02).
 01  NFTENMS1-ST       PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
 01  WK-GRP             PIC  X(08).
 01  DSP-CNTL.
     03  DSP-ST         PIC  X(02).
     03  DSP-ST2        PIC  X(04).
     03  DSP-FMT        PIC  X(08).
     03  DSP-GRP        PIC  X(08).
     03  DSP-PRO        PIC  X(02).
     03  DSP-FNC        PIC  X(04).
     03  DSP-CON        PIC  X(06).
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ﾃ-ﾌﾞﾙ >>-*
 01  FNC-TABLE.
     03  ENT            PIC  X(04)     VALUE     "E000".
     03  PF04           PIC  X(04)     VALUE     "F004".
     03  PF05           PIC  X(04)     VALUE     "F005".
     03  PF06           PIC  X(04)     VALUE     "F006".
     03  PF11           PIC  X(04)     VALUE     "F011".
     03  PF12           PIC  X(04)     VALUE     "F012".
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE01       PIC  N(40)  VALUE   NC"_取　消　_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
     NC"_取　消　_終　了　_項目戻り".
 01  GUIDE03       PIC  N(40)  VALUE
     NC"_取　消　_終　了　_項目戻り　_次　頁".
 01  GUIDE04       PIC  N(40)  VALUE
     NC"_取　消　_終　了　_項目戻り　_前　頁".
 01  GUIDE05       PIC  N(40)  VALUE
     NC"_取　消　_終　了　_項目戻り　_前　頁　_次　頁".
*
 01  MSG-AREA.
     03  MSG01               PIC  N(30)  VALUE
              NC"ＰＦキーが違います".
     03  MSG02               PIC  N(30)  VALUE
              NC"管理番号を入力して下さい。".
     03  MSG03               PIC  N(30)  VALUE
              NC"管理番号に誤りがあります。".
     03  MSG04               PIC  N(30)  VALUE
              NC"ＴＲＡＮＴＲＡＮ連携済です。".
     03  MSG05               PIC  N(30)  VALUE
              NC"箱数ファイルに存在しません。".
     03  MSG06               PIC  N(30)  VALUE
              NC"取引先コードを入力して下さい。".
     03  MSG07               PIC  N(30)  VALUE
              NC"訂正商品が違います。".
     03  MSG08               PIC  N(30)  VALUE
              NC"作場が違います。".
     03  MSG09               PIC  N(30)  VALUE
              NC"作場を入力して下さい。".
     03  MSG10               PIC  N(30)  VALUE
              NC"前頁はありません。".
     03  MSG11               PIC  N(30)  VALUE
              NC"次頁はありません。".
     03  MSG12               PIC  N(30)  VALUE
              NC"実納品数と訂正後発注数が一致してません。".
     03  MSG13               PIC  N(30)  VALUE
              NC"Ｙを入力".
     03  MSG14               PIC  N(30)  VALUE
              NC"訂正後発注数が発注数を超えています。".
     03  MSG15               PIC  N(30)  VALUE
              NC"実納品数が発注数量を超えています。".
     03  MSG16               PIC  N(30)  VALUE
     NC"全ての発注数量を”０”にしました。確認して下さい。".
     03  MSG17               PIC  N(30)  VALUE
              NC"納品日に誤りがあります。".
     03  MSG18               PIC  N(30)  VALUE
     NC"一致するデータが箱数ファイルに存在しません。".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(30)  OCCURS      18.
*
 01  WRK-AREA.
     03  WRK-MEISAI          OCCURS    999.
       05  WRK-TENPO         PIC  9(05).
       05  WRK-NOUBCD        PIC  X(01).
       05  WRK-AREACD        PIC  X(01).
       05  WRK-MAESU         PIC  9(06).
       05  WRK-ATOSU         PIC  9(06).
       05  WRK-ITEMSU        PIC  9(06).
       05  WRK-TENDT         PIC  9(08).
       05  WRK-ERR           PIC  9(01).
*
 01  WRK-NAME.
     03  WRK-KANA            PIC  X(15)        VALUE SPACE.
*
*計算領域
 01  WRK-AREA2.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*
 01  CNT-AREA.
     03  IX                  PIC  9(04)  VALUE  ZERO.
     03  IY                  PIC  9(04)  VALUE  ZERO.
     03  IZ                  PIC  9(04)  VALUE  ZERO.
     03  CNT-MEISAI          PIC  9(04)  VALUE  ZERO.
     03  CNT-PAGE            PIC  9(04)  VALUE  ZERO.
     03  CNT-MAXPAGE         PIC  9(04)  VALUE  ZERO.
     03  AMARI               PIC  9(04)  VALUE  ZERO.
*
 01  SEC-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
****  リンク領域  ***
 LINKAGE               SECTION.
 01  PARA-IN-BUMONCD           PIC  X(04).
 01  PARA-IN-TANCD             PIC  X(02).
*
****************************************************************
 PROCEDURE              DIVISION
              USING     PARA-IN-BUMONCD  PARA-IN-TANCD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 表示ファイル >>--*
 DSPFILE-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSPFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY3825I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     4000       TO          PROGRAM-STATUS.
*    CLOSE    NFHAKOL4   SAKUBAL1    DSPFILE
*             TANMS1     JYOKEN1     NFTENMS1.
     STOP     RUN.
*----<< 箱数ファイル >>--*
 NFHAKOL4-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFHAKOL4.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY3825I NFHAKOL4 ERROR " NFHAKOL4-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     4000       TO          PROGRAM-STATUS.
*    CLOSE    NFHAKOL4   SAKUBAL1    DSPFILE
*             TANMS1     JYOKEN1     NFTENMS1.
     STOP     RUN.
*----<< 担当者マスタ >>--*
 TANMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TANMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY3825I TANMS1   ERROR " TANMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     4000       TO          PROGRAM-STATUS.
*    CLOSE    NFHAKOL4   SAKUBAL1    DSPFILE
*             TANMS1     JYOKEN1     NFTENMS1.
     STOP     RUN.
*----<< 条件ファイル >>--*
 JYOKEN1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JYOKEN1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY3825I JYOKEN1  ERROR " JYOKEN1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     4000       TO          PROGRAM-STATUS.
*    CLOSE    NFHAKOL4   SAKUBAL1    DSPFILE
*             TANMS1     JYOKEN1     NFTENMS1.
     STOP     RUN.
*----<< 作場マスタ >>--*
 SAKUBAL1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SAKUBAL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY3825I SAKUBAL1 ERROR " SAKUBAL1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     4000       TO          PROGRAM-STATUS.
*    CLOSE    NFHAKOL4   SAKUBAL1    DSPFILE
*             TANMS1     JYOKEN1     NFTENMS1.
     STOP     RUN.
*----<< ナフコ店舗マスタ >>--*
 NFTENMS1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFTENMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY3825I NFTENMS1 ERROR " NFTENMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     4000       TO          PROGRAM-STATUS.
*    CLOSE    NFHAKOL4   SAKUBAL1    DSPFILE
*             TANMS1     JYOKEN1     NFTENMS1.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO    =    99.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
*
     INITIALIZE                   WRK-AREA.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     SYS-YYW        TO   WK-SYSYY.
     MOVE     SYS-MMW        TO   WK-SYSMM.
     MOVE     SYS-DDW        TO   WK-SYSDD.
*
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY3825I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     TANMS1.
     OPEN     INPUT     JYOKEN1.
     OPEN     INPUT     SAKUBAL1.
     OPEN     INPUT     NFTENMS1.
     OPEN     I-O       NFHAKOL4.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   GR-NO.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*----<< ﾊﾝｲ ｼﾃｲ ｶﾞﾒﾝ ｸﾘｱ >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< KEY     ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-KEYGRP UNTIL     GR-NO    NOT  =    1.
*    同一管理番号・別作場の入力を可とするため一旦開放
     CLOSE    NFHAKOL4.
     OPEN I-O NFHAKOL4.
*----<< ﾒｲｻｲ    ﾆｭｳﾘｮｸ >>-*
*****PERFORM  220-INP-GRP02  UNTIL     GR-NO    NOT  =    2.
*----<< ﾒｲｻｲ    ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-MEIGRP UNTIL     GR-NO    NOT  =    2.
*----<< ｶｸﾆﾝ    ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ﾌｧｲﾙ    ｺｳｼﾝ ｼｮﾘ >>-*
     PERFORM  240-UPDATE-SEC UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    NFHAKOL4.
     CLOSE    SAKUBAL1.
     CLOSE    NFTENMS1.
     CLOSE    TANMS1.
     CLOSE    JYOKEN1.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY3825I END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE    "210-DSP-INIT"       TO   S-NAME.
     MOVE     SPACE          TO   FSY38251.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-BODY1-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SSY3825I"      TO   PGID.
     MOVE    "FSY38251"      TO   FORM.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSY38251"     TO   DSP-FMT.
     MOVE     "SCREFX"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*毎回KEY部を入力させない。
*    IF        CHK-FLG  =  SPACE
*              MOVE   " "    TO   EDIT-STATUS OF KANRNO
*              MOVE   " "    TO   EDIT-STATUS OF SKBCD
*    ELSE
*              MOVE   "X"    TO   EDIT-STATUS OF KANRNO
*              MOVE   "X"    TO   EDIT-STATUS OF SKBCD
*              MOVE WK-B-KANRNO TO KANRNO
*              MOVE WK-B-SKBCD  TO SKBCD
*    END-IF.
*    MOVE     WK-B-KANRNO    TO   KANRNO.
*    MOVE     WK-B-SKBCD     TO   SKBCD.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      KEY      ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-KEYGRP          SECTION.
     MOVE     "220-INP-KEYGRP"     TO   S-NAME.
     MOVE     "KEYGRP"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF04
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN ENT
              INITIALIZE               WRK-AREA
              INITIALIZE               CNT-AREA
              PERFORM   CLR-HEAD-RTN
              PERFORM   CLR-BODY1-RTN
              PERFORM   220-KEYGRP-CHECK-SEC
              IF        ERR-FLG   =    ZERO
***                     画面セット処理
                        MOVE      1    TO   CNT-PAGE
                        PERFORM   GAMEN-SET-SEC
                        MOVE      2    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-KEYGRP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      KEY     ﾁｪｯｸ                                *
*--------------------------------------------------------------*
 220-KEYGRP-CHECK-SEC    SECTION.
     MOVE     "220-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*    MOVE     SPACE     TO        MEIMST-INV-FLG.
*
 220-KEYGRP-CHECK-01.
*    管理番号入力チェック
     IF       KANRNO    =    ZERO
              MOVE      2         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF KANRNO
              MOVE     "R"        TO   EDIT-OPTION OF KANRNO
              GO                  TO   220-KEYGRP-CHECK-EXIT
     END-IF.
*
 220-KEYGRP-CHECK-02.
     IF       KANRNO(1:1)    =     9
              MOVE      SPACE      TO  SKBCD
              GO        TO         220-KEYGRP-CHECK-03
     END-IF.
*    作場コードチェック
     IF       SKBCD      NOT =     SPACE
              MOVE      SKBCD      TO   SAK-F01
              READ      SAKUBAL1
                  INVALID  KEY
                        MOVE      SPACE     TO   SKBNM
                        IF   ERR-FLG   =    ZERO
                             MOVE      8    TO   ERR-FLG
                        END-IF
                        MOVE     "C"   TO   EDIT-CURSOR OF SKBCD
                        MOVE     "R"   TO   EDIT-OPTION OF SKBCD
                  NOT INVALID  KEY
                        MOVE      SAK-F02   TO   SKBNM
              END-READ
     ELSE
              MOVE      SPACE     TO   SKBNM
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF SKBCD
              MOVE     "R"   TO   EDIT-OPTION OF SKBCD
     END-IF.
*
 220-KEYGRP-CHECK-03.
*    箱数ファイル存在チェック
     IF  ERR-FLG        =    ZERO
         PERFORM   220-SONZAI-CHECK
         IF   ERR-FLG   =    ZERO
              MOVE      HAK-F05    TO   SKBCD
                                        SAK-F01
              READ      SAKUBAL1
                  INVALID  KEY
                        MOVE      SPACE     TO   SKBNM
                  NOT INVALID  KEY
                        MOVE      SAK-F02   TO   SKBNM
              END-READ
              MOVE      ZERO      TO   IX
*             対象データ退避処理
              PERFORM   220-TAIHI-SEC
***
              IF   CNT-MEISAI     =    ZERO
                   MOVE      18   TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF SKBCD
                   MOVE     "R"   TO   EDIT-OPTION OF SKBCD
                   GO   TO   220-KEYGRP-CHECK-EXIT
              END-IF
***
              IF        CNT-MEISAI     <=   17
                   MOVE      1         TO   CNT-MAXPAGE
              ELSE
                   DIVIDE    CNT-MEISAI     BY   17
                             GIVING    CNT-MAXPAGE
                             REMAINDER      AMARI
                   IF   AMARI     NOT =     ZERO
                        ADD  1    TO   CNT-MAXPAGE
                   END-IF
              END-IF
         END-IF
     END-IF.
*
 220-KEYGRP-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶﾞﾒﾝ ﾒｲｻｲ ｾｯﾄ                               *
*--------------------------------------------------------------*
 GAMEN-SET-SEC          SECTION.
     MOVE    "GAMEN-SET-SEC"      TO   S-NAME.
*
     COMPUTE  IY   =    CNT-PAGE  *    17   -    17.
     PERFORM  VARYING   IZ   FROM  1   BY   1    UNTIL IZ > 17
         COMPUTE   IX   =    IY   +    IZ
         IF   WRK-TENPO(IX)  NOT =     ZERO
              MOVE      SPACE          TO   MAS001(IZ)
              MOVE      WRK-TENPO(IX)  TO   TENPO(IZ)
              MOVE      137607         TO   TEN-F01
              MOVE      WRK-TENPO(IX)  TO   TEN-F02
              READ      NFTENMS1
                   INVALID   KEY
                        MOVE      SPACE     TO   TENPNM(IZ)
                   NOT INVALID
                        MOVE      TEN-F05   TO   TENPNM(IZ)
              END-READ
              IF   TEN-F13         NOT =    SPACE
                   MOVE 96             TO   JYK-F01
                   MOVE TEN-F13        TO   AREACD(IZ)
                                            JYK-F02
                   READ JYOKEN1
                        INVALID   KEY
                             MOVE      SPACE      TO  AREANM(IZ)
                        NOT INVALID
                             MOVE      JYK-F03    TO  AREANM(IZ)
                   END-READ
              END-IF
              MOVE WRK-NOUBCD(IX)       TO   NOUBCD(IZ)
              MOVE WRK-MAESU (IX)       TO   MAESU (IZ)
              MOVE WRK-ATOSU (IX)       TO   ATOSU (IZ)
              MOVE WRK-ITEMSU(IX)       TO   ITEMSU(IZ)
              MOVE WRK-TENDT (IX)       TO   TENDT (IZ)
         ELSE
              MOVE      SPACE          TO   MAS001(IZ)
              MOVE      "X"  TO   EDIT-STATUS OF ATOSU(IZ)
         END-IF
     END-PERFORM.
*
 GAMEN-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾌｧｲﾙ         ｿﾝｻﾞｲ ﾁｪｯｸ                     *
*--------------------------------------------------------------*
 220-SONZAI-CHECK       SECTION.
     MOVE    "220-SONZAI-CHECK"  TO   S-NAME.
*
     MOVE     SPACE          TO   HAK-REC.
     INITIALIZE                   HAK-REC.
     MOVE     KANRNO         TO   HAK-F01.
     MOVE     SKBCD          TO   HAK-F05.
     MOVE     ZERO           TO   HAK-F08.
     MOVE     ZERO           TO   HAK-F06.
     MOVE     SPACE          TO   HAK-F07.
     START    NFHAKOL4  KEY  >=   HAK-F01   HAK-F05
                                  HAK-F08   HAK-F06
                                  HAK-F07
         INVALID   KEY
              MOVE      5    TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF KANRNO
              MOVE     "R"        TO   EDIT-OPTION OF KANRNO
              MOVE     "R"        TO   EDIT-OPTION OF SKBCD
              GO   TO   220-SONZAI-CHECK-EXIT
         NOT INVALID
*TEST
*    DISPLAY "SKBCD START =" SKBCD UPON CONS
*TEST
              READ      NFHAKOL4
                AT END
                   MOVE      5    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF KANRNO
                   MOVE     "R"   TO   EDIT-OPTION OF KANRNO
                   MOVE     "R"   TO   EDIT-OPTION OF SKBCD
                   GO   TO   220-SONZAI-CHECK-EXIT
                NOT AT END
*TEST
*    DISPLAY "SKBCD READ  =" SKBCD UPON CONS
*TEST
                   IF ( KANRNO    =    HAK-F01 )
                        CONTINUE
                   ELSE
                        MOVE      5    TO   ERR-FLG
                        MOVE "C"  TO   EDIT-CURSOR OF KANRNO
                        MOVE "R"  TO   EDIT-OPTION OF KANRNO
                        GO   TO   220-SONZAI-CHECK-EXIT
                   END-IF
                   IF ( KANRNO(1:1)    =  9   )
                        CONTINUE
                   ELSE
                        IF ( SKBCD     =    HAK-F05 )
                             CONTINUE
                        ELSE
                             MOVE      5    TO   ERR-FLG
                             MOVE "C"  TO   EDIT-CURSOR OF SKBCD
                             MOVE "R"  TO   EDIT-OPTION OF SKBCD
                             GO   TO   220-SONZAI-CHECK-EXIT
                        END-IF
                   END-IF
                   IF ( HAK-F98   =    " "     )
                        CONTINUE
                   ELSE
                        MOVE      4    TO   ERR-FLG
                        MOVE "C"  TO   EDIT-CURSOR OF KANRNO
                        MOVE "R"  TO   EDIT-OPTION OF KANRNO
                        MOVE "R"  TO   EDIT-OPTION OF SKBCD
                        GO   TO   220-SONZAI-CHECK-EXIT
                   END-IF
              END-READ
     END-START.
 220-SONZAI-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｰﾀﾀｲﾋ ｼﾖﾘ                                 *
*--------------------------------------------------------------*
 220-TAIHI-SEC          SECTION.
     MOVE     "220-TAIHI-SEC"     TO   S-NAME.
*
     IF     ( KANRNO    =    HAK-F01 ) AND
            ( SKBCD     =    HAK-F05 ) AND
            ( HAK-F98   =    " "     )
              ADD       1         TO   IX
              MOVE      HAK-F06   TO   WRK-TENPO  (IX)
              MOVE      HAK-F07   TO   WRK-NOUBCD (IX)
              MOVE      HAK-F09   TO   WRK-MAESU  (IX)
                                       WRK-ATOSU  (IX)
              MOVE      HAK-F10   TO   WRK-ITEMSU (IX)
              MOVE      HAK-F08   TO   WRK-TENDT  (IX)
              GO        TO        220-TAIHI-010
     ELSE
              MOVE      IX        TO   CNT-MEISAI
              GO        TO        220-TAIHI-EXIT
     END-IF.
*
 220-TAIHI-010.
*
     READ     NFHAKOL4  NEXT
         AT   END
              MOVE      IX        TO   CNT-MEISAI
         NOT AT END
              GO   TO   220-TAIHI-SEC
     END-READ.
*
 220-TAIHI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      箱数訂正入力　　　　                        *
*--------------------------------------------------------------*
 220-INP-MEIGRP          SECTION.
     MOVE     "220-INP-MEIGRP"     TO   S-NAME.
*
     PERFORM   GAMEN-SET-SEC.
     MOVE     "MEIGRP"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
     MOVE     ZERO      TO        ERR-FLG.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF06
***         項目戻り時、現在の訂正後数をワークへ書き出し
              COMPUTE  IY    =    CNT-MEISAI * 17 - 17
              PERFORM   VARYING   IZ  FROM  1  BY  1
                                  UNTIL     IZ > 17
                   COMPUTE  IX    =  IY  +  IZ
                   PERFORM  VARYING  IX FROM 1 BY 1
                                  UNTIL     IX > 17
                        MOVE  ATOSU (IX)    TO   WRK-ATOSU(IX)
                   END-PERFORM
              END-PERFORM
*
              PERFORM   CLR-BODY1-RTN
              MOVE      1         TO   GR-NO
         WHEN PF11
*             PERFORM   MEISAI-CHK-SEC
*             IF   ERR-FLG  =  ZERO
              PERFORM 900-DSP-WRITE2
*             MOVE     "MEIGRP"       TO   WK-GRP
*             PERFORM  900-DSP-READ
                   PERFORM   CLR-BODY1-RTN
                   IF   CNT-PAGE  <=   1
                        PERFORM   GAMEN-TAIHI-SEC
                        MOVE      10   TO   ERR-FLG
                   ELSE
                        PERFORM   GAMEN-TAIHI-SEC
                        IF   ERR-FLG   =    ZERO
                             ADD       -1   TO   CNT-PAGE
                             PERFORM   GAMEN-SET-SEC
                        END-IF
                   END-IF
*             END-IF
         WHEN PF12
**            PERFORM   MEISAI-CHK-SEC
**            IF   ERR-FLG  =  ZERO
*TEST↓
**************PERFORM 900-DSP-WRITE2
**            DISPLAY "ATOSU(1)= " ATOSU(1) UPON CONS
**************MOVE     "MEIGRP"       TO   WK-GRP
**************PERFORM  900-DSP-READ
*TEST↑
*                  PERFORM   CLR-BODY1-RTN
*                  IF   CNT-PAGE  =    CNT-MAXPAGE
*                       PERFORM   GAMEN-TAIHI-SEC
*                       MOVE      11   TO   ERR-FLG
*                  ELSE
*                       PERFORM   GAMEN-TAIHI-SEC
*                       IF   ERR-FLG   =    ZERO
*                            ADD       1    TO   CNT-PAGE
*                            PERFORM   GAMEN-SET-SEC
*                       END-IF
*                  END-IF
**            END-IF
         WHEN ENT
              PERFORM   CLR-BODY1-RTN
              PERFORM   GAMEN-TAIHI-SEC
              PERFORM   220-MEIGRP-CHECK-SEC
              IF        ERR-FLG   =    ZERO
*TEST
                        MOVE      9    TO   GR-NO
*                       MOVE      2    TO   GR-NO
*TEST
              END-IF
         WHEN OTHER
              MOVE           1    TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-MEIGRP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾒｲｻｲ    ﾁｪｯｸ                                *
*--------------------------------------------------------------*
 220-MEIGRP-CHECK-SEC    SECTION.
     MOVE     "220-CRP03-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
 220-MEIGRP-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶﾞﾒﾝ ﾀｲﾋ ｼｮﾘ                                *
*--------------------------------------------------------------*
 GAMEN-TAIHI-SEC        SECTION.
     MOVE     "GAMEN-TAIHI-SEC"   TO   S-NAME.
*
     PERFORM  900-DSP-WRITE2.
     COMPUTE  IY   =    CNT-PAGE  *    17   -    17.
*TEST
*    DISPLAY "IY=" IY UPON CONS.
*TEST
     PERFORM  VARYING   IZ   FROM  1  BY  1  UNTIL  IZ  >  17
         COMPUTE   IX   =    IY   +    IZ
              IF   ATOSU (IZ)     IS NUMERIC
                   MOVE      ATOSU (IZ)     TO   WRK-ATOSU(IX)
*TEST
**   DISPLAY "IZ=" IZ UPON CONS
**   DISPLAY "IX=" IX UPON CONS
**   DISPLAY "  ATOSU=" ATOSU(IZ) UPON CONS
*    DISPLAY "WRK-ATOSU=" WRK-ATOSU(IX) UPON CONS
*TEST
              ELSE
                   MOVE      "X"  TO   EDIT-STATUS OF ATOSU(IZ)
              END-IF
     END-PERFORM.
*
 GAMEN-TAIHI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-WRITE2         SECTION.
*
     MOVE     "SCREEN"       TO   DSP-GRP.
     MOVE     WK-SYSYMD           TO   SDATE.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW           TO   STIME.
*
     PERFORM  900-DSP-WRITE.
*
 900-DSP-WRITE2-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "230-INP-KKNN"      TO   S-NAME.
     MOVE     "KKNN"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF06
              MOVE      2         TO   GR-NO
         WHEN PF11
*             PERFORM  MEISAI-CHK-SEC
*             IF   ERR-FLG  =  ZERO
                   IF   CNT-PAGE  <=   1
                        MOVE      10   TO   ERR-FLG
                   ELSE
                        ADD       -1   TO   CNT-PAGE
                        PERFORM   GAMEN-SET-SEC
                   END-IF
*             END-IF
         WHEN PF12
*             PERFORM  MEISAI-CHK-SEC
*             IF   ERR-FLG  =  ZERO
                   IF   CNT-PAGE  =    CNT-MAXPAGE
                        MOVE      11   TO   ERR-FLG
                   ELSE
                        ADD       1    TO   CNT-PAGE
                        PERFORM   GAMEN-SET-SEC
                   END-IF
*             END-IF
         WHEN ENT
              MOVE      ZERO      TO   ERR-FLG
              PERFORM   CLR-TAIL-RTN
              IF        KKNN      NOT =    "Y"
                        MOVE      13   TO   ERR-FLG
              ELSE
                        MOVE      10   TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾌｧｲﾙｺｳｼﾝ                                    *
*--------------------------------------------------------------*
 240-UPDATE-SEC           SECTION.
     MOVE     "240-UPDATE-SEC"    TO   S-NAME.
*
*****指定キーを表示する
     DISPLAY NC"＃管理番号＝" KANRNO NC"　作場＝" SKBCD
             NC"　＃"  UPON CONS.
*訂正後箱数更新
     PERFORM  VARYING   IX   FROM      1    BY   1
                             UNTIL     IX   >    CNT-MEISAI
         IF   WRK-ATOSU(IX)   NOT =     WRK-MAESU(IX)
              PERFORM   KOUSIN1-SEC
         END-IF
     END-PERFORM.
*取消を押さない限り、キーは固定表示
     MOVE    "CHK"                TO   CHK-FLG.
     MOVE     KANRNO              TO   WK-B-KANRNO.
     MOVE     SKBCD               TO   WK-B-SKBCD.
*画面初期化ＳＥＣへ
     MOVE     0                   TO   GR-NO.
*
 240-UPDATE-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL         ｺｳｼﾝ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 KOUSIN1-SEC             SECTION.
     MOVE    "KOUSIN1-SEC"   TO   S-NAME.
*
     MOVE     SPACE          TO   HAK-REC.
     INITIALIZE                   HAK-REC  WRK-ERR(IX).
     MOVE     KANRNO         TO   HAK-F01.
     MOVE     SKBCD          TO   HAK-F05.
     MOVE     WRK-TENDT(IX)  TO   HAK-F08.
     MOVE     WRK-TENPO(IX)  TO   HAK-F06.
     MOVE     WRK-NOUBCD(IX) TO   HAK-F07.
     START    NFHAKOL4  KEY  >=   HAK-F01   HAK-F05
                                  HAK-F08   HAK-F06
                                  HAK-F07
         INVALID   KEY
              DISPLAY  "SSY3825I  NFHAKOL4 START INV 1"
                                       UPON CONS
              GO   TO   KOUSIN1-SEC-EXIT
     END-START.
*
 KOUSIN1-010.
*
     READ     NFHAKOL4
         AT END
              GO   TO   KOUSIN1-SEC-EXIT
         NOT AT END
              IF ( KANRNO         =    HAK-F01 ) AND
                 ( SKBCD          =    HAK-F05 ) AND
                 ( WRK-TENDT(IX)  =    HAK-F08 ) AND
                 ( WRK-TENPO(IX)  =    HAK-F06 ) AND
                 ( WRK-NOUBCD(IX) =    HAK-F07 ) AND
                 ( HAK-F98        =    " "     )
                   MOVE WRK-ATOSU(IX)  TO   HAK-F09
                   REWRITE  HAK-REC
              END-IF
     END-READ.
*
 KOUSIN1-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "900-DSP-READ"      TO   S-NAME.
     MOVE     "SCREEN"       TO   DSP-GRP.
     IF       ERR-FLG   =    0
              MOVE      SPACE               TO   MSG
              MOVE      "D"      TO   EDIT-OPTION OF MSG
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   MSG
     END-IF.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   GUIDE
     ELSE
*        IF   GR-NO     =    2
*             MOVE      GUIDE02   TO   GUIDE
*        ELSE
           IF   CNT-MEISAI     <=   17
                MOVE      GUIDE02   TO   GUIDE
           ELSE
                IF   CNT-PAGE  <=   1
                     MOVE      GUIDE03   TO   GUIDE
                ELSE
                     IF   CNT-PAGE  =    CNT-MAXPAGE
                          MOVE      GUIDE04   TO   GUIDE
                     ELSE
                          MOVE      GUIDE05   TO   GUIDE
                     END-IF
                END-IF
           END-IF
*        END-IF
     END-IF.
*    MOVE     WK-SYSYMD           TO   SDATE.
     MOVE     SYS-DATEW           TO   SDATE.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW           TO   STIME.
     MOVE     PARA-IN-BUMONCD     TO   TAN-F01.
     MOVE     PARA-IN-BUMONCD     TO   BUMTAN(1:4).
     MOVE     "-"                 TO   BUMTAN(5:1).
     MOVE     PARA-IN-TANCD       TO   TAN-F02.
     MOVE     PARA-IN-TANCD       TO   BUMTAN(6:2).
     READ     TANMS1
              INVALID
                MOVE  ALL NC"？"     TO  TANNM
              NOT INVALID
                MOVE  TAN-F03        TO  TANNM
     END-READ.
*
     PERFORM  900-DSP-WRITE.
*
     IF       MSG  NOT  =    SPACE
              MOVE "AL"      TO   DSP-PRO
     ELSE
              MOVE "NE"      TO   DSP-PRO
     END-IF.
     MOVE     SPACE          TO   MSG.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
*TEST
     IF  WK-GRP = "MEIGRP"
         DISPLAY "ATOSU(1)=" ATOSU(1) UPON CONS
     END-IF.
*TEST
     MOVE     SPACE          TO   DSP-PRO.
 900-DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
     MOVE     "900-DSP-WRITE"     TO   S-NAME.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FSY38251.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF KANRNO
                                  EDIT-CURSOR OF SKBCD.
     MOVE     "M"            TO   EDIT-OPTION OF KANRNO
                                  EDIT-OPTION OF SKBCD.
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY1-RTN          SECTION.
*
     PERFORM  VARYING   IX   FROM  1   BY   1    UNTIL  IX > 17
         MOVE     " "        TO   EDIT-CURSOR OF ATOSU(IX)
         MOVE     "M"        TO   EDIT-OPTION OF ATOSU(IX)
         MOVE     " "        TO   EDIT-STATUS OF ATOSU(IX)
     END-PERFORM.
*
 CLR-BODY1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF KKNN.
     MOVE     "M"        TO   EDIT-OPTION OF KKNN.
 CLR-TAIL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細情報　発注数　訂正数チェック                          *
*--------------------------------------------------------------*
 MEISAI-CHK-SEC         SECTION.
     MOVE     "MEISAI-CHK-SEC"         TO   S-NAME.
     MOVE      ZERO                    TO   ERR-FLG.
*
 MEISAI-CHK-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
