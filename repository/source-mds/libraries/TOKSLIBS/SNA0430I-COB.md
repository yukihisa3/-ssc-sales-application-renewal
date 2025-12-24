# SNA0430I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNA0430I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　苗業務システム　ＨＧ連携          *
*    業務名　　　　　　　：　サカタ商品ＣＤ再取得              *
*    モジュール名　　　　：　サカタ商品ＣＤ再取得指示          *
*    作成日／更新日　　　：　12/08/16                          *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　画面より、サカタ商品ＣＤを再取得　*
*                            する条件を入力する。　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SNA0430I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/08/16.
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
*----<< 商品変換テーブル>>--*
     SELECT   HSHOTBL   ASSIGN         DA-01-VI-SHOTBL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TBL-F01   TBL-F02
                        STATUS         HSHOTBL-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 倉庫マスタ >>--*
     SELECT   ZSOKMS   ASSIGN         DA-01-VI-ZSOKMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SOK-F01
                        STATUS         ZSOKMS-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FNA04301  OF        XMDLIB
              JOINING   DSP       PREFIX.
*----<< 商品変換テーブル >>--*
 FD  HSHOTBL            LABEL     RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 倉庫マスタ >>--*
 FD  ZSOKMS            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS   OF        XFDLIB
              JOINING   SOK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  HSHOTBL-ST       PIC  X(02).
 01  HTOKMS-ST        PIC  X(02).
 01  ZSOKMS-ST        PIC  X(02).
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
 01  WK-SOKCD           PIC  X(02)  VALUE  SPACE.
*
 01  WK-TIME.
     03  WK-TIME-1      PIC  9(02).
     03  WK-TIME-2      PIC  9(02).
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
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE01       PIC  N(40)  VALUE   NC"_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
         NC"_取　消　_終　了　_項目戻り".
*
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
              NC"ＰＦキーが違います".
     03  MSG02               PIC  N(20)  VALUE
              NC"バッチ_を入力して下さい。".
     03  MSG03               PIC  N(20)  VALUE
              NC"バッチ_に誤りがあります。".
     03  MSG04               PIC  N(20)  VALUE
              NC"取引先コードが違います。".
     03  MSG05               PIC  N(20)  VALUE
              NC"商品変換テーブルに存在しません。".
     03  MSG06               PIC  N(20)  VALUE
              NC"倉庫が違います。".
     03  MSG07               PIC  N(20)  VALUE
              NC"倉庫を入力して下さい。".
     03  MSG08               PIC  N(20)  VALUE
              NC"区分が違います。".
     03  MSG09               PIC  N(20)  VALUE
              NC"開始が終了を超えています。".
     03  MSG10               PIC  N(20)  VALUE
              NC"相手商品ＣＤを入力して下さい。".
     03  MSG11               PIC  N(20)  VALUE
              NC"伝票番号開始を入力して下さい。".
     03  MSG12               PIC  N(20)  VALUE
              NC"伝票番号終了を入力して下さい。".
     03  MSG13               PIC  N(20)  VALUE
              NC"小売連携対象の倉庫ではありません。".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS      13.
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
 LINKAGE                SECTION.
 01  PARA-KBN               PIC   X(01).
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-JTORCD            PIC   9(08).
 01  PARA-JSOKO             PIC   X(02).
 01  PARA-JAITE             PIC   X(13).
 01  PARA-TTORCD            PIC   9(08).
 01  PARA-TSOKO             PIC   X(02).
 01  PARA-TAITE             PIC   X(13).
 01  PARA-TDENST            PIC   9(09).
 01  PARA-TDENED            PIC   9(09).
*
****************************************************************
 PROCEDURE              DIVISION USING PARA-KBN
                                       PARA-JDATE
                                       PARA-JTIME
                                       PARA-JTORCD
                                       PARA-JSOKO
                                       PARA-JAITE
                                       PARA-TTORCD
                                       PARA-TSOKO
                                       PARA-TAITE
                                       PARA-TDENST
                                       PARA-TDENED.
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
     DISPLAY  "### SNA0430I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HSHOTBL   HTOKMS    ZSOKMS    DSPFILE.
     STOP     RUN.
*----<< 商品変換テーブル >>--*
 HSHOTBL-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHOTBL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SNA0430I HSHOTBL ERROR " HSHOTBL-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HSHOTBL   HTOKMS    ZSOKMS    DSPFILE.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SNA0430I HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HSHOTBL   HTOKMS    ZSOKMS    DSPFILE.
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 ZSOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SNA0430I ZSOKMS ERROR " ZSOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HSHOTBL   HTOKMS    ZSOKMS    DSPFILE.
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
     DISPLAY  "*** SNA0430I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     HSHOTBL.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     ZSOKMS.
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
*----<< ｸﾌﾞﾝ    ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-KBN    UNTIL     GR-NO    NOT  =    1.
*----<< ﾊﾞｯﾁNO. ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-GRP01  UNTIL     GR-NO    NOT  =    2.
*----<< ﾃｶﾞｷ ｼﾞｮｳｹﾝ  ﾆｭｳﾘｮｸ >>-*
     PERFORM  240-INP-GRP02  UNTIL     GR-NO    NOT  =    3.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  250-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ﾊﾟﾗﾒﾀ ｼｭﾂﾘｮｸ >>-*
     PERFORM  260-PARA-SEC   UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    HSHOTBL.
     CLOSE    HTOKMS.
     CLOSE    ZSOKMS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY00013I END *** "
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
     MOVE     SPACE          TO   DSP-FNA04301.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-BDY01-RTN.
     PERFORM  CLR-BDY02-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SNA0430I"      TO   DSP-PGID.
     MOVE    "FNA04301"      TO   DSP-FORM.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FNA04301"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｸﾌﾞﾝ     ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-KBN            SECTION.
     MOVE     "230-INP-GRP01"     TO   S-NAME.
     MOVE     "HEAD"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      4010      TO   PROGRAM-STATUS
              DISPLAY NC"＃＃取消終了＃＃" UPON CONS
              STOP  RUN
         WHEN ENT
              PERFORM   CLR-HEAD-RTN
              PERFORM   220-KBN-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                   IF  DSP-KBN = "1"
                        MOVE      2    TO   GR-NO
                   ELSE
                        MOVE      3    TO   GR-NO
                   END-IF
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-KBN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｸﾌﾞﾝ     ﾆｭｳﾘｮｸ  ﾁｪｯｸ                       *
*--------------------------------------------------------------*
 220-KBN-CHECK-SEC      SECTION.
*
     MOVE     "220-KBN-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO                   TO   ERR-FLG.
*
     IF       DSP-KBN  =  "1"  OR  "2"
              CONTINUE
     ELSE
              MOVE      8         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF DSP-KBN
              MOVE     "R"        TO   EDIT-OPTION OF DSP-KBN
     END-IF.
*
 220-KBN-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO.  ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 230-INP-GRP01          SECTION.
     MOVE     "230-INP-GRP01"     TO   S-NAME.
     MOVE     "BODY01"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      4010      TO   PROGRAM-STATUS
              DISPLAY NC"＃＃取消終了＃＃" UPON CONS
              STOP  RUN
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      SPACE     TO   DSP-MAS001
              MOVE      1         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-BDY01-RTN
              PERFORM   230-GRP01-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 230-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO. ﾁｪｯｸ                                *
*--------------------------------------------------------------*
 230-GRP01-CHECK-SEC    SECTION.
     MOVE     "230-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
     IF     ( DSP-JDATE     =    ZERO ) AND
            ( DSP-JTIME     =    ZERO ) AND
            ( DSP-JTORCD    =    ZERO )
              MOVE      2         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF DSP-JDATE
              MOVE     "R"        TO   EDIT-OPTION OF DSP-JDATE
              MOVE     "R"        TO   EDIT-OPTION OF DSP-JTIME
              MOVE     "R"        TO   EDIT-OPTION OF DSP-JTORCD
              GO   TO   230-GRP01-CHECK-EXIT
     END-IF.
*    受信日付チェック
     IF  DSP-JDATE     NOT =     ZERO
         MOVE     "2"        TO        LINK-IN-KBN
         MOVE      DSP-JDATE TO        LINK-IN-YMD8
         CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8
         IF   LINK-OUT-RET   NOT =     ZERO
              IF        ERR-FLG   =    ZERO
                        MOVE      3    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF DSP-JDATE
              MOVE     "R"        TO   EDIT-OPTION OF DSP-JDATE
              GO   TO   230-GRP01-CHECK-EXIT
         END-IF
     END-IF.
*    受信時間
     IF  DSP-JTIME     NOT NUMERIC
         MOVE      ZERO      TO   DSP-JTIME
     END-IF.
*
     MOVE    DSP-JTIME       TO   WK-TIME.
     IF  WK-TIME-1  >  24
         IF        ERR-FLG   =    ZERO
                   MOVE      3    TO   ERR-FLG
         END-IF
         MOVE     "C"        TO  EDIT-CURSOR OF DSP-JTIME
         MOVE     "R"        TO  EDIT-OPTION OF DSP-JTIME
         GO   TO   230-GRP01-CHECK-EXIT
     END-IF.
     IF  WK-TIME-2  >  59
         IF        ERR-FLG   =    ZERO
                   MOVE      3    TO   ERR-FLG
         END-IF
         MOVE     "C"        TO  EDIT-CURSOR OF DSP-JTIME
         MOVE     "R"        TO  EDIT-OPTION OF DSP-JTIME
         GO   TO   230-GRP01-CHECK-EXIT
     END-IF.
*    取引先チェック
     IF  DSP-JTORCD    NOT =     ZERO
         MOVE      SPACE     TO   TOK-REC
         INITIALIZE               TOK-REC
         MOVE      DSP-JTORCD TO  TOK-F01
         READ      HTOKMS
             INVALID
                   MOVE      SPACE     TO   DSP-JTORNM
                   IF   ERR-FLG   =    ZERO
                        MOVE      4    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF DSP-JTORCD
                   MOVE     "R"   TO   EDIT-OPTION OF DSP-JTORCD
             NOT INVALID
                   MOVE      TOK-F02   TO   DSP-JTORNM
         END-READ
     END-IF.
*    倉庫コードチェック
     IF     ( DSP-JSOKO      NOT =     SPACE )
     AND    ( DSP-JSOKO      NOT =     "00"  )
              MOVE      DSP-JSOKO      TO   SOK-F01
              READ      ZSOKMS
                  INVALID  KEY
                        MOVE      SPACE     TO   DSP-JSOKON
                        IF   ERR-FLG   =    ZERO
                             MOVE      6    TO   ERR-FLG
                        END-IF
                        MOVE "C"   TO   EDIT-CURSOR OF DSP-JSOKO
                        MOVE "R"   TO   EDIT-OPTION OF DSP-JSOKO
              NOT INVALID  KEY
                        MOVE      SOK-F02   TO   DSP-JSOKON
                        MOVE      DSP-JSOKO TO   WK-SOKCD
                  IF  SOK-F12  NOT =  "1"
                        IF   ERR-FLG   =    ZERO
                             MOVE     13    TO   ERR-FLG
                        END-IF
                        MOVE "C"   TO   EDIT-CURSOR OF DSP-JSOKO
                        MOVE "R"   TO   EDIT-OPTION OF DSP-JSOKO
                  END-IF
              END-READ
     ELSE
              MOVE      SPACE     TO   DSP-JSOKON
              IF   ERR-FLG   =    ZERO
                   MOVE      7    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF DSP-JSOKO
              MOVE     "R"   TO   EDIT-OPTION OF DSP-JSOKO
     END-IF.
*    相手商品コードチェック
     IF       DSP-JAITE      NOT =     SPACE
              MOVE      DSP-JTORCD     TO   TBL-F01
              MOVE      DSP-JAITE      TO   TBL-F02
              READ      HSHOTBL
                  INVALID  KEY
                        IF   ERR-FLG   =    ZERO
                             MOVE      5    TO   ERR-FLG
                        END-IF
                        MOVE "C"   TO   EDIT-CURSOR OF DSP-JAITE
                        MOVE "R"   TO   EDIT-OPTION OF DSP-JAITE
              END-READ
     ELSE
              IF   ERR-FLG   =    ZERO
                   MOVE      10   TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF DSP-JAITE
              MOVE     "R"   TO   EDIT-OPTION OF DSP-JAITE
     END-IF.
*
 230-GRP01-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO.  ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 240-INP-GRP02          SECTION.
     MOVE     "240-INP-GRP02"     TO   S-NAME.
     MOVE     "BODY02"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      4010      TO   PROGRAM-STATUS
              DISPLAY NC"＃＃取消終了＃＃" UPON CONS
              STOP  RUN
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      SPACE     TO   DSP-MAS002
              MOVE      1         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-BDY02-RTN
              PERFORM   240-GRP02-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 240-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO. ﾁｪｯｸ                                *
*--------------------------------------------------------------*
 240-GRP02-CHECK-SEC    SECTION.
     MOVE     "240-CRP02-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*    取引先チェック
     IF  DSP-TTORCD    NOT =     ZERO
         MOVE      SPACE     TO   TOK-REC
         INITIALIZE               TOK-REC
         MOVE      DSP-TTORCD TO  TOK-F01
         READ      HTOKMS
             INVALID
                   MOVE      SPACE     TO   DSP-TTORNM
                   IF   ERR-FLG   =    ZERO
                        MOVE      4    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF DSP-TTORCD
                   MOVE     "R"   TO   EDIT-OPTION OF DSP-TTORCD
             NOT INVALID
                   MOVE      TOK-F02   TO   DSP-TTORNM
         END-READ
     END-IF.
*    倉庫コードチェック
     IF     ( DSP-TSOKO      NOT =     SPACE )
     AND    ( DSP-TSOKO      NOT =     "00"  )
              MOVE      DSP-TSOKO      TO   SOK-F01
              READ      ZSOKMS
                  INVALID  KEY
                        MOVE      SPACE     TO   DSP-TSOKON
                        IF   ERR-FLG   =    ZERO
                             MOVE      6    TO   ERR-FLG
                        END-IF
                        MOVE "C"   TO   EDIT-CURSOR OF DSP-TSOKO
                        MOVE "R"   TO   EDIT-OPTION OF DSP-TSOKO
              NOT INVALID  KEY
                        MOVE      SOK-F02   TO   DSP-TSOKON
                        MOVE      DSP-TSOKO TO   WK-SOKCD
                  IF  SOK-F12  NOT =  "1"
                        IF   ERR-FLG   =    ZERO
                             MOVE     13    TO   ERR-FLG
                        END-IF
                        MOVE "C"   TO   EDIT-CURSOR OF DSP-TSOKO
                        MOVE "R"   TO   EDIT-OPTION OF DSP-TSOKO
                  END-IF
              END-READ
     ELSE
              MOVE      SPACE     TO   DSP-TSOKON
              IF   ERR-FLG   =    ZERO
                   MOVE      7    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF DSP-TSOKO
              MOVE     "R"   TO   EDIT-OPTION OF DSP-TSOKO
     END-IF.
*    相手商品コードチェック
     IF       DSP-TAITE      NOT =     SPACE
              MOVE      DSP-TTORCD     TO   TBL-F01
              MOVE      DSP-TAITE      TO   TBL-F02
              READ      HSHOTBL
                  INVALID  KEY
                        IF   ERR-FLG   =    ZERO
                             MOVE      5    TO   ERR-FLG
                        END-IF
                        MOVE "C"   TO   EDIT-CURSOR OF DSP-TAITE
                        MOVE "R"   TO   EDIT-OPTION OF DSP-TAITE
              END-READ
     ELSE
              IF   ERR-FLG   =    ZERO
                   MOVE      10   TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF DSP-TAITE
              MOVE     "R"   TO   EDIT-OPTION OF DSP-TAITE
     END-IF.
*    伝票番号チェック
*    （開始）
     IF       DSP-TDENST  NOT  NUMERIC
     OR       DSP-TDENST  =    ZERO
              MOVE ZERO      TO   DSP-TDENST
     END-IF.
*　　（終了）
     IF       DSP-TDENED  NOT  NUMERIC
     OR       DSP-TDENED  =    ZERO
              MOVE 999999999 TO   DSP-TDENED
     END-IF.
*　　（大小チェック）
     IF       DSP-TDENST  >  DSP-TDENED
              IF   ERR-FLG   =    ZERO
                   MOVE      9    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF DSP-TDENST
              MOVE     "R"   TO   EDIT-OPTION OF DSP-TDENST
              MOVE     "R"   TO   EDIT-OPTION OF DSP-TDENED
     END-IF.
*
 240-GRP02-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 250-INP-KKNN           SECTION.
     MOVE     "250-INP-KKNN"      TO   S-NAME.
     MOVE     "KKNN"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      4010      TO   PROGRAM-STATUS
              DISPLAY NC"＃＃取消終了＃＃" UPON CONS
              STOP  RUN
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              IF  DSP-KBN  = "1"
                  MOVE      2         TO   GR-NO
              ELSE
                  MOVE      3         TO   GR-NO
              END-IF
         WHEN ENT
              PERFORM   CLR-TAIL-RTN
              MOVE      10        TO   GR-NO
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 250-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾟﾗﾒﾀ ｼｭﾂﾘｮｸ                                *
*--------------------------------------------------------------*
 260-PARA-SEC           SECTION.
     MOVE     "260-PARA-SEC"      TO   S-NAME.
*
     IF  DSP-KBN = "1"
         MOVE DSP-JDATE           TO   PARA-JDATE
         MOVE DSP-JTIME           TO   PARA-JTIME
         MOVE DSP-JTORCD          TO   PARA-JTORCD
         MOVE DSP-JSOKO           TO   PARA-JSOKO
         MOVE DSP-JAITE           TO   PARA-JAITE
         MOVE DSP-KBN             TO   PARA-KBN
         MOVE ZERO                TO   PARA-TTORCD
         MOVE SPACE               TO   PARA-TSOKO
         MOVE SPACE               TO   PARA-TAITE
         MOVE ZERO                TO   PARA-TDENST
         MOVE ZERO                TO   PARA-TDENED
     ELSE
         MOVE ZERO                TO   PARA-JDATE
         MOVE ZERO                TO   PARA-JTIME
         MOVE ZERO                TO   PARA-JTORCD
         MOVE SPACE               TO   PARA-JSOKO
         MOVE SPACE               TO   PARA-JAITE
         MOVE DSP-KBN             TO   PARA-KBN
         MOVE DSP-TTORCD          TO   PARA-TTORCD
         MOVE DSP-TSOKO           TO   PARA-TSOKO
         MOVE DSP-TAITE           TO   PARA-TAITE
         MOVE DSP-TDENST          TO   PARA-TDENST
         MOVE DSP-TDENED          TO   PARA-TDENED
     END-IF.
*
     MOVE     99                  TO   GR-NO.
*
 260-PARA-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "900-DSP-READ"      TO   S-NAME.
     MOVE     "SCREEN"       TO   DSP-GRP.
     IF       ERR-FLG   =    0
              MOVE      SPACE               TO   DSP-MSG
              MOVE      "D"      TO   EDIT-OPTION OF DSP-MSG
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   DSP-MSG
     END-IF.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   DSP-GUIDE
     ELSE
              MOVE      GUIDE02   TO   DSP-GUIDE
     END-IF.
     MOVE     WK-SYSYMD           TO   DSP-SYSYMD.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW           TO   DSP-SYSTIM.
*
     PERFORM  900-DSP-WRITE.
*
     IF       DSP-MSG  NOT  =    SPACE
              MOVE "AL"      TO   DSP-PRO
     ELSE
              MOVE "NE"      TO   DSP-PRO
     END-IF.
     MOVE     SPACE          TO   DSP-MSG.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE          TO   DSP-PRO.
 900-DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
     MOVE     "900-DSP-WRITE"     TO   S-NAME.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    DSP-FNA04301.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF DSP-KBN.
     MOVE     "M"            TO   EDIT-OPTION OF DSP-KBN.
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＤＹ１　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BDY01-RTN          SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF DSP-JDATE
                                  EDIT-CURSOR OF DSP-JTIME
                                  EDIT-CURSOR OF DSP-JTORCD
                                  EDIT-CURSOR OF DSP-JSOKO
                                  EDIT-CURSOR OF DSP-JAITE.
     MOVE     "M"            TO   EDIT-OPTION OF DSP-JDATE
                                  EDIT-OPTION OF DSP-JTIME
                                  EDIT-OPTION OF DSP-JTORCD
                                  EDIT-OPTION OF DSP-JSOKO
                                  EDIT-OPTION OF DSP-JAITE.
 CLR-BDY01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＤＹ２　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BDY02-RTN          SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF DSP-TTORCD
                                  EDIT-CURSOR OF DSP-TSOKO
                                  EDIT-CURSOR OF DSP-TAITE
                                  EDIT-CURSOR OF DSP-TDENST
                                  EDIT-CURSOR OF DSP-TDENED.
     MOVE     "M"            TO   EDIT-OPTION OF DSP-TTORCD
                                  EDIT-OPTION OF DSP-TSOKO
                                  EDIT-OPTION OF DSP-TAITE
                                  EDIT-OPTION OF DSP-TDENST
                                  EDIT-OPTION OF DSP-TDENED.
 CLR-BDY02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF DSP-KKNN.
     MOVE     "M"        TO   EDIT-OPTION OF DSP-KKNN.
 CLR-TAIL-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
