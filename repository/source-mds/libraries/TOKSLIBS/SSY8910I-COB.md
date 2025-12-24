# SSY8910I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY8910I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　リックＷＥＢＥＤＩ対応            *
*    業務名　　　　　　　：　リックＷＥＢＥＤＩ対応            *
*    モジュール名　　　　：　納品予定データ作成指示            *
*    作成日／更新日　　　：　2008/04/11                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　画面より、バッチ_、出力倉庫、    *
*                            納品日の範囲を入力し、チェック    *
*                            後、パラメタへ渡す。              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY8910I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/17.
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
*----<< リック基本情報データ >>--*
     SELECT   RCJOHOF   ASSIGN         DA-01-VI-RCJOHOL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  RCJ-K01   RCJ-K02
                                       RCJ-K03   RCJ-K04
                                       RCJ-K05   RCJ-K06
                                       RCJ-K07   RCJ-K08
                        STATUS         RCJOHOF-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 倉庫マスタ >>--*
     SELECT   ZSOKMS1   ASSIGN         DA-01-VI-ZSOKMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SOK-F01
                        STATUS         ZSOKMS1-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSY8910I  OF        XMDLIB.
*----<< 伝票データ >>--*
 FD  RCJOHOF            LABEL     RECORD   IS   STANDARD.
     COPY     RCJOHOF   OF        XFDLIB
              JOINING   RCJ       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 倉庫マスタ >>--*
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1   OF        XFDLIB
              JOINING   SOK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  RCJOHOF-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  ZSOKMS1-ST        PIC  X(02).
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
 01  WK-HCSZHOK         PIC  9(08)  VALUE  ZERO.
 01  WK-HCSZTOH         PIC  9(08)  VALUE  ZERO.
 01  WK-HCSZKAN         PIC  9(08)  VALUE  ZERO.
 01  WK-KAHMASZ         PIC  9(08)  VALUE  ZERO.
 01  WK-HCSKHOK         PIC  9(08)  VALUE  ZERO.
 01  WK-HCSKTOH         PIC  9(08)  VALUE  ZERO.
 01  WK-HCSKKAN         PIC  9(08)  VALUE  ZERO.
 01  WK-KAHMASK         PIC  9(08)  VALUE  ZERO.
 01  WK-DAIK1           PIC  9(08)  VALUE  ZERO.
 01  WK-DAIK2           PIC  9(08)  VALUE  ZERO.
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
              NC"ＲＣ基本情報ＤＴに存在しません。".
     03  MSG06               PIC  N(20)  VALUE
              NC"倉庫が違います。".
     03  MSG07               PIC  N(20)  VALUE
              NC"対象データがありません。".
     03  MSG08               PIC  N(20)  VALUE
              NC"既に確定データ作成済みです。".
     03  MSG09               PIC  N(20)  VALUE
              NC"既にデータ送信済みです。".
     03  MSG10               PIC  N(20)  VALUE
              NC"納品日を正しく入力して下さい。".
     03  MSG11               PIC  N(20)  VALUE
              NC"取引先ＣＤは、資材ＣＤを入力して下さい。".
     03  MSG12               PIC  N(20)  VALUE
              NC"取引先ＣＤは、植物ＣＤを入力して下さい。".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS      12.
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
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
 01  PARA-DSOKO             PIC   X(02).
*
****************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUDT
                                       PARA-DSOKO.
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
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY8910I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    RCJOHOF   HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< 伝票データ >>--*
 RCJOHOF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCJOHOF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY8910I RCJOHOF ERROR " RCJOHOF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    RCJOHOF   HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY8910I HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    RCJOHOF   HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 ZSOKMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY8910I ZSOKMS1 ERROR " ZSOKMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    RCJOHOF   HTOKMS    ZSOKMS1    DSPFILE.
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
*ﾁｪｯｸ取引先CDｾｯﾄ
     MOVE     883                 TO   WK-HCSZHOK.
     MOVE     882                 TO   WK-HCSZTOH.
     MOVE     880                 TO   WK-HCSZKAN.
     MOVE     13938               TO   WK-KAHMASZ.
     MOVE     14273               TO   WK-HCSKHOK.
     MOVE     14272               TO   WK-HCSKTOH.
     MOVE     1427                TO   WK-HCSKKAN.
     MOVE     17137               TO   WK-KAHMASK.
     MOVE     100403              TO   WK-DAIK1.
     MOVE     1004031             TO   WK-DAIK2.

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
     DISPLAY  "*** SSY8910I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     RCJOHOF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     ZSOKMS1.
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
*----<< ﾊﾞｯﾁNO. ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ﾊﾟﾗﾒﾀ ｼｭﾂﾘｮｸ >>-*
     PERFORM  240-PARA-SEC   UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    RCJOHOF.
     CLOSE    HTOKMS.
     CLOSE    ZSOKMS1.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY8910I END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE    "210-DSP-INIT"  TO   S-NAME.
     MOVE     SPACE          TO   FSY8910I.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SSY8910I"      TO   PGID.
     MOVE    "FSY8910I"      TO   FORM.
*倉庫コードパラメタより倉庫コード入力チェック
     IF       PARA-DSOKO  =  "01"
              MOVE    " "    TO   EDIT-STATUS OF SOKO
     ELSE
              MOVE      PARA-SOKO TO   SOK-F01 SOKO
              READ      ZSOKMS1
                  INVALID  KEY
                        MOVE      ALL NC"＊" TO   SOKONM
              NOT INVALID  KEY
                        MOVE      SOK-F02    TO   SOKONM
              END-READ
              MOVE    "X"    TO   EDIT-STATUS OF SOKO
     END-IF.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSY8910I"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO.  ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "GRP01"        TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      4010      TO   PROGRAM-STATUS
              MOVE      99        TO   GR-NO
         WHEN ENT
              PERFORM   CLR-HEAD-RTN
              PERFORM   220-GRP01-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO. ﾁｪｯｸ                                *
*--------------------------------------------------------------*
 220-GRP01-CHECK-SEC    SECTION.
     MOVE     "220-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
     IF     ( JDATE     =    ZERO ) AND
            ( JTIME     =    ZERO ) AND
            ( TORICD    =    ZERO )
              MOVE      2         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JTIME
              MOVE     "R"        TO   EDIT-OPTION OF TORICD
              GO   TO   220-GRP01-CHECK-EXIT
     END-IF.
*    受信日付チェック
     IF  JDATE     NOT =     ZERO
         MOVE     "2"        TO        LINK-IN-KBN
         MOVE      JDATE     TO        LINK-IN-YMD8
         CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8
         IF   LINK-OUT-RET   NOT =     ZERO
              IF        ERR-FLG   =    ZERO
                        MOVE      3    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JDATE
              GO   TO   220-GRP01-CHECK-EXIT
         END-IF
     END-IF.
*    受信時間
     IF  JTIME     NOT NUMERIC
         MOVE      ZERO      TO   JTIME
     END-IF.
*    取引先チェック
     IF  TORICD  NOT  NUMERIC
     AND TORICD  =  ZERO
         IF   ERR-FLG   =    ZERO
              MOVE      4    TO   ERR-FLG
         END-IF
         MOVE     "C"   TO   EDIT-CURSOR OF TORICD
         MOVE     "R"   TO   EDIT-OPTION OF TORICD
     END-IF.
     IF  TORICD    NOT =     ZERO
         MOVE      SPACE     TO   TOK-REC
         INITIALIZE               TOK-REC
         MOVE      TORICD    TO   TOK-F01
         READ      HTOKMS
             INVALID
                   MOVE      SPACE     TO   TORINM
                   IF   ERR-FLG   =    ZERO
                        MOVE      4    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF TORICD
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
             NOT INVALID
                   MOVE      TOK-F02   TO   TORINM
         END-READ
     END-IF.
*    倉庫コードチェック
     IF     ( SOKO      NOT =     SPACE )
     AND    ( SOKO      NOT =     "00"  )
              MOVE      SOKO      TO   SOK-F01
              READ      ZSOKMS1
                  INVALID  KEY
                        MOVE      SPACE     TO   SOKONM
                        IF   ERR-FLG   =    ZERO
                             MOVE      6    TO   ERR-FLG
                        END-IF
                        MOVE     "C"   TO   EDIT-CURSOR OF SOKO
                        MOVE     "R"   TO   EDIT-OPTION OF SOKO
              NOT INVALID  KEY
                        MOVE      SOK-F02   TO   SOKONM
                        MOVE      SOKO      TO   WK-SOKCD
              END-READ
     ELSE
              MOVE      SPACE          TO   WK-SOKCD
              MOVE      NC"全倉庫"     TO   SOKONM
     END-IF.
*    納品日チェック
     IF       NOUDT NOT NUMERIC
              MOVE      ZERO      TO   NOUDT
     ELSE
              IF   NOUDT  NOT =  ZERO
                   MOVE    "2"        TO        LINK-IN-KBN
                   MOVE     NOUDT     TO        LINK-IN-YMD8
                   CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                                LINK-IN-YMD6
                                                LINK-IN-YMD8
                                                LINK-OUT-RET
                                                LINK-OUT-YMD8
                   IF       LINK-OUT-RET   NOT =    ZERO
                            IF   ERR-FLG   =    ZERO
                                 MOVE      10   TO   ERR-FLG
                            END-IF
                            MOVE  "C"   TO  EDIT-CURSOR OF NOUDT
                            MOVE  "R"   TO  EDIT-OPTION OF NOUDT
                   END-IF
              END-IF
     END-IF.
*    売上伝票データ存在チェック
     IF  ERR-FLG        =    ZERO
         MOVE      SPACE          TO   RCJ-REC
         INITIALIZE                    RCJ-REC
         MOVE      JDATE          TO   RCJ-K01
         MOVE      JTIME          TO   RCJ-K02
         MOVE      TORICD         TO   RCJ-K03
         MOVE      WK-SOKCD       TO   RCJ-K04
         MOVE      NOUDT          TO   RCJ-K08
         START     RCJOHOF   KEY  >=   RCJ-K01   RCJ-K02
                                       RCJ-K03   RCJ-K04
                                       RCJ-K05   RCJ-K06
                                       RCJ-K07   RCJ-K08
              INVALID   KEY
                   MOVE      5    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JTIME
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
                   IF   WK-SOKCD  NOT =     SPACE
                        MOVE     "R"   TO   EDIT-OPTION OF SOKO
                   END-IF
                   IF   NOUDT     NOT =    ZERO
                        MOVE     "R"   TO   EDIT-OPTION OF NOUDT
                   END-IF
                   GO   TO   220-GRP01-CHECK-EXIT
              NOT INVALID
                   READ      RCJOHOF
                     AT END
                        MOVE      5    TO   ERR-FLG
                        MOVE     "C"   TO   EDIT-CURSOR OF JDATE
                        MOVE     "R"   TO   EDIT-OPTION OF JDATE
                        MOVE     "R"   TO   EDIT-OPTION OF JTIME
                        MOVE     "R"   TO   EDIT-OPTION OF TORICD
                        MOVE     "R"   TO   EDIT-OPTION OF SOKO
                        MOVE     "R"   TO   EDIT-OPTION OF NOUDT
                        GO   TO   220-GRP01-CHECK-EXIT
                     NOT AT END
                        IF ( JDATE     =    RCJ-K01 ) AND
                           ( JTIME     =    RCJ-K02 ) AND
                           ( TORICD    =    RCJ-K03 )
                             IF ( WK-SOKCD  NOT =    SPACE )
                                  IF   SOKO     =     RCJ-K04
                                       CONTINUE
                                  ELSE
                                       MOVE  5   TO   ERR-FLG
                                       MOVE "C"  TO
                                            EDIT-CURSOR OF JDATE
                                       MOVE "R"  TO
                                            EDIT-OPTION OF JDATE
                                            EDIT-OPTION OF JTIME
                                            EDIT-OPTION OF TORICD
                                            EDIT-OPTION OF SOKO
                                            EDIT-OPTION OF NOUDT
                                  END-IF
                             ELSE
                                  CONTINUE
                             END-IF
                             IF   NOUDT  =  RCJ-K08
                             OR   NOUDT  =  ZERO
                                  CONTINUE
                             ELSE
                                  MOVE  5   TO   ERR-FLG
                                  MOVE "C"  TO
                                        EDIT-CURSOR OF JDATE
                                  MOVE "R"  TO
                                        EDIT-OPTION OF JDATE
                                        EDIT-OPTION OF JTIME
                                        EDIT-OPTION OF TORICD
                                        EDIT-OPTION OF SOKO
                                        EDIT-OPTION OF NOUDT
                             END-IF
                        ELSE
                             MOVE      5    TO   ERR-FLG
                             MOVE "C"  TO   EDIT-CURSOR OF JDATE
                             MOVE "R"  TO   EDIT-OPTION OF JDATE
                             MOVE "R"  TO   EDIT-OPTION OF JTIME
                             MOVE "R"  TO   EDIT-OPTION OF TORICD
                             IF    WK-SOKCD NOT =    SPACE
                                   MOVE    "R"   TO
                                            EDIT-OPTION OF SOKO
                             END-IF
                             GO   TO   220-GRP01-CHECK-EXIT
                        END-IF
                   END-READ
         END-START
     END-IF.
*    チェックＯＫ時
     IF      ERR-FLG  =  ZERO
             IF    RCJ-K11 =  1
                   IF   ERR-FLG   =    ZERO
                        MOVE      9    TO   ERR-FLG
                   END-IF
                   MOVE "C"  TO   EDIT-CURSOR OF JDATE
                   MOVE "R"  TO   EDIT-OPTION OF JDATE
                                  EDIT-OPTION OF JTIME
                                  EDIT-OPTION OF TORICD
                                  EDIT-OPTION OF SOKO
                                  EDIT-OPTION OF NOUDT
             END-IF
     END-IF.
*
 220-GRP01-CHECK-EXIT.
     EXIT.
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
              MOVE      4010      TO   PROGRAM-STATUS
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      1         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-TAIL-RTN
              MOVE      10        TO   GR-NO
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾟﾗﾒﾀ ｼｭﾂﾘｮｸ                                *
*--------------------------------------------------------------*
 240-PARA-SEC           SECTION.
     MOVE     "240-PARA-SEC"      TO   S-NAME.
*
     MOVE     JDATE               TO   PARA-JDATE.
     MOVE     JTIME               TO   PARA-JTIME.
     MOVE     TORICD              TO   PARA-TORICD.
     MOVE     SOKO                TO   PARA-SOKO.
     IF       WK-SOKCD  =      SPACE
              MOVE      SPACE     TO   PARA-SOKO
     END-IF.
     MOVE     NOUDT               TO   PARA-NOUDT.
*
     MOVE     99                  TO   GR-NO.
*
 240-PARA-SEC-EXIT.
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
              MOVE      GUIDE02   TO   GUIDE
     END-IF.
     MOVE     WK-SYSYMD           TO   SYSYMD.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW           TO   SYSTIM.
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
     MOVE     SPACE          TO   DSP-PRO.
 900-DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
     MOVE     "900-DSP-WRITE"     TO   S-NAME.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FSY8910I.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF JDATE
                                  EDIT-CURSOR OF JTIME
                                  EDIT-CURSOR OF TORICD
                                  EDIT-CURSOR OF NOUDT
                                  EDIT-CURSOR OF SOKO.
     MOVE     "M"            TO   EDIT-OPTION OF JDATE
                                  EDIT-OPTION OF JTIME
                                  EDIT-OPTION OF TORICD
                                  EDIT-OPTION OF NOUDT
                                  EDIT-OPTION OF SOKO.
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF KKNN.
     MOVE     "M"        TO   EDIT-OPTION OF KKNN.
 CLR-TAIL-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
