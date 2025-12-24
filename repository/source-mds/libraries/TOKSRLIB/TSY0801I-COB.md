# TSY0801I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/TSY0801I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｄ３６５連携                      *
*    モジュール名　　　　：　連携店舗変更指示（オンライン）    *
*    作成日／作成者　　　：　2021/05/31 INOUE                  *
*    処理概要　　　　　　：　売上伝票ファイルの需要家ＩＤを    *
*                            変更するための対象条件指示。　    *
*    更新日／更新者　　　：　                                  *
*　                                                            *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            TSY0801I.
*                  流用:SSY0015I.TOKSLIB
 AUTHOR.                NAV Y.YOSHIDA.
 DATE-WRITTEN.          2021/05/31.
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
*----<< 取引先マスタ >>--*
     SELECT   TOKMS2    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOKMS2-ST.
*----<< 倉庫マスタ >>--*
     SELECT   ZSOKMS1   ASSIGN         DA-01-VI-ZSOKMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SOK-F01
                        STATUS         ZSOKMS1-ST.
*----<< 店舗マスタ >>--*
     SELECT   TENMS1    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52
                                       TEN-F011
                        STATUS         TENMS1-ST.
*----<< 需要家ＩＤ管理マスタ >>--*
     SELECT   KYKJYKL1  ASSIGN         DA-01-VI-KYKJYKL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  KY1-F01
                                       KY1-F02
                        STATUS         KYKJYKL1-ST.
*----<< 需要家ＩＤ管理マスタ >>--*
     SELECT   KYKJYKL2  ASSIGN         DA-01-VI-KYKJYKL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  KY2-F03
                                       KY2-F04
                        STATUS         KYKJYKL2-ST.
*----<< 売上伝票ファイル >>--*
     SELECT   SHTDENLA  ASSIGN         DA-01-VI-SHTDENLA
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F46
                                       DEN-F47
                                       DEN-F01
                                       DEN-F48
                                       DEN-F02
                                       DEN-F04
                                       DEN-F051
                                       DEN-F07
                                       DEN-F112
                                       DEN-F03
                        STATUS         SHTDENLA-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSY08011  OF        XMDLIB.
*----<< 取引先マスタ >>--*
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     TOKMS2    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 倉庫マスタ >>--*
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1   OF        XFDLIB
              JOINING   SOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 需要家ＩＤ管理マスタ >>--*
 FD  KYKJYKL1           LABEL RECORD   IS   STANDARD.
     COPY     KYKJYKL1  OF        XFDLIB
              JOINING   KY1       PREFIX.
*----<< 需要家ＩＤ管理マスタ >>--*
 FD  KYKJYKL2           LABEL RECORD   IS   STANDARD.
     COPY     KYKJYKL2  OF        XFDLIB
              JOINING   KY2       PREFIX.
*----<< 売上伝票ファイル >>--*
 FD  SHTDENLA           LABEL RECORD   IS   STANDARD.
     COPY     SHTDENLA  OF        XFDLIB
              JOINING   DEN       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
     03  SONZAI-FLG     PIC  9(01)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  TOKMS2-ST         PIC  X(02).
 01  ZSOKMS1-ST        PIC  X(02).
 01  TENMS1-ST         PIC  X(02).
 01  KYKJYKL1-ST       PIC  X(02).
 01  KYKJYKL2-ST       PIC  X(02).
 01  SHTDENLA-ST       PIC  X(02).
*
 01  IX                PIC  9(02)      VALUE  ZERO.
 01  IY                PIC  9(02)      VALUE  ZERO.
 01  IZ                PIC  9(02)      VALUE  ZERO.
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
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.
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
              NC"必須入力です。".
     03  MSG03               PIC  N(20)  VALUE
              NC"倉庫マスタに存在しません。".
     03  MSG04               PIC  N(20)  VALUE
              NC"取引先マスタに存在しません。".
     03  MSG05               PIC  N(20)  VALUE
              NC"売上伝票ファイルに存在しません。".
     03  MSG06               PIC  N(20)  VALUE
              NC"開始＞終了になっています。".
     03  MSG07               PIC  N(20)  VALUE
              NC"日付・時刻エラーです。".
     03  MSG08               PIC  N(20)  VALUE
              NC"店舗マスタに存在しません。".
     03  MSG09               PIC  N(20)  VALUE
              NC"需要家ＩＤ管理マスタに存在しません。".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       9.
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
 01  PARA-IN-SOKO           PIC   X(02).
 01  PARA-IN-DSOKO          PIC   X(02).
 01  PARA-IN-BUMON          PIC   X(04).
 01  PARA-IN-TANTOU         PIC   X(02).
 01  PARA-OUT-BDATE         PIC   9(08).
 01  PARA-OUT-BTIME         PIC   9(04).
 01  PARA-OUT-BTORI         PIC   9(08).
 01  PARA-OUT-SOKO          PIC   X(02).
 01  PARA-OUT-NDATE         PIC   9(08).
 01  PARA-OUT-CHGTEN        PIC   9(05).
 01  PARA-OUT-CHGJYK        PIC   X(10).
 01  PARA-OUT-SDENNO01      PIC   9(09).
 01  PARA-OUT-EDENNO01      PIC   9(09).
 01  PARA-OUT-SDENNO02      PIC   9(09).
 01  PARA-OUT-EDENNO02      PIC   9(09).
 01  PARA-OUT-SDENNO03      PIC   9(09).
 01  PARA-OUT-EDENNO03      PIC   9(09).
 01  PARA-OUT-SDENNO04      PIC   9(09).
 01  PARA-OUT-EDENNO04      PIC   9(09).
 01  PARA-OUT-SDENNO05      PIC   9(09).
 01  PARA-OUT-EDENNO05      PIC   9(09).
 01  PARA-OUT-SDENNO06      PIC   9(09).
 01  PARA-OUT-EDENNO06      PIC   9(09).
 01  PARA-OUT-SDENNO07      PIC   9(09).
 01  PARA-OUT-EDENNO07      PIC   9(09).
 01  PARA-OUT-SDENNO08      PIC   9(09).
 01  PARA-OUT-EDENNO08      PIC   9(09).
 01  PARA-OUT-SDENNO09      PIC   9(09).
 01  PARA-OUT-EDENNO09      PIC   9(09).
 01  PARA-OUT-SDENNO10      PIC   9(09).
 01  PARA-OUT-EDENNO10      PIC   9(09).
 01  PARA-OUT-SDENNO11      PIC   9(09).
 01  PARA-OUT-EDENNO11      PIC   9(09).
 01  PARA-OUT-SDENNO12      PIC   9(09).
 01  PARA-OUT-EDENNO12      PIC   9(09).
*
****************************************************************
 PROCEDURE              DIVISION
       USING   PARA-IN-SOKO       PARA-IN-DSOKO    PARA-IN-BUMON
               PARA-IN-TANTOU
               PARA-OUT-BDATE     PARA-OUT-BTIME   PARA-OUT-BTORI
               PARA-OUT-SOKO      PARA-OUT-NDATE   PARA-OUT-CHGTEN
               PARA-OUT-CHGJYK
               PARA-OUT-SDENNO01  PARA-OUT-EDENNO01
               PARA-OUT-SDENNO02  PARA-OUT-EDENNO02
               PARA-OUT-SDENNO03  PARA-OUT-EDENNO03
               PARA-OUT-SDENNO04  PARA-OUT-EDENNO04
               PARA-OUT-SDENNO05  PARA-OUT-EDENNO05
               PARA-OUT-SDENNO06  PARA-OUT-EDENNO06
               PARA-OUT-SDENNO07  PARA-OUT-EDENNO07
               PARA-OUT-SDENNO08  PARA-OUT-EDENNO08
               PARA-OUT-SDENNO09  PARA-OUT-EDENNO09
               PARA-OUT-SDENNO10  PARA-OUT-EDENNO10
               PARA-OUT-SDENNO11  PARA-OUT-EDENNO11
               PARA-OUT-SDENNO12  PARA-OUT-EDENNO12.
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
     DISPLAY  "### TSY0801I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"    TO   PROGRAM-STATUS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 TOKMS2-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOKMS2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY0801I TOKMS2 ERROR " TOKMS2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"    TO   PROGRAM-STATUS.
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 ZSOKMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY0801I ZSOKMS1 ERROR " ZSOKMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"    TO   PROGRAM-STATUS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 TENMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TENMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY0801I TENMS1 ERROR " TENMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"    TO   PROGRAM-STATUS.
     STOP     RUN.
*----<< 需要家ＩＤ管理マスタ >>--*
 KYKJYKL1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KYKJYKL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY0801I KYKJYKL1 ERROR " KYKJYKL1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"    TO   PROGRAM-STATUS.
     STOP     RUN.
*----<< 需要家ＩＤ管理マスタ >>--*
 KYKJYKL2-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KYKJYKL2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY0801I KYKJYKL2 ERROR " KYKJYKL2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"    TO   PROGRAM-STATUS.
     STOP     RUN.
*----<< 売上伝票ファイル >>--*
 SHTDENLA-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENLA.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY0801I SHTDENLA ERROR " SHTDENLA-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"    TO   PROGRAM-STATUS.
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
*    LEVEL  2      初期処理                                    *
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
     DISPLAY  "*** TSY0801I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     TOKMS2
                        ZSOKMS1
                        TENMS1
                        KYKJYKL1
                        KYKJYKL2
                        SHTDENLA.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   GR-NO.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      メイン処理                                  *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*
*-<< 画面初期化 >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*
*-<< 条件入力（左側）>>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*
*-<< 伝票_範囲入力（右側）>>-*
     PERFORM  220-INP-GRP02  UNTIL     GR-NO    NOT  =    2.
*
*-<< 確認入力 >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*
*-<< パラメタ出力 >>-*
     PERFORM  240-PARA-SEC   UNTIL     GR-NO    NOT  =    10.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      エンド処理                                  *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE
              TOKMS2
              ZSOKMS1
              TENMS1
              KYKJYKL1
              KYKJYKL2
              SHTDENLA.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** TSY0801I END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      画面初期化                                  *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE    "210-DSP-INIT"       TO   S-NAME.
     MOVE     SPACE          TO   FSY08011.
*
     PERFORM  CLR-GRP001-RTN.
     PERFORM  CLR-GRP002-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "TSY0801I"      TO   PGID.
     MOVE    "FSY08011"      TO   FORM.
*
     IF      PARA-IN-DSOKO  =  "01"
             MOVE      1         TO   GR-NO
     ELSE
             MOVE      PARA-IN-SOKO TO   SOKO
             MOVE      SOKO      TO   SOK-F01
             READ      ZSOKMS1
                 INVALID  KEY
                       MOVE      SPACE     TO   SOKONM
             NOT INVALID  KEY
                       MOVE      SOK-F02   TO   SOKONM
             END-READ
             MOVE     "X"        TO   EDIT-STATUS OF SOKO
             MOVE      1         TO   GR-NO
     END-IF.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSY08011"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
 210-DSP-INIT-EXIT.
     EXIT.

*--------------------------------------------------------------*
*    LEVEL  3      条件入力（左側）                            *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "GRP001"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN ENT
              PERFORM   CLR-GRP001-RTN
              PERFORM   220-GRP01-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      2    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      入力条件チェック                            *
*--------------------------------------------------------------*
 220-GRP01-CHECK-SEC    SECTION.
     MOVE     "220-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
 220-GRP01-CHECK-001.
*バッチ_（日付）チェック
***  バッチ_（日付）未入力チェック
     IF       BDATE  NOT NUMERIC
         OR   BDATE  =  ZERO
              MOVE   2       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  BDATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  BDATE
              GO             TO   220-GRP01-CHECK-EXIT
     ELSE
***           バッチ_（日付）論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     BDATE          TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD8
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD8
              IF   LINK-OUT-RET   = 9
                   MOVE   7       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  BDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  BDATE
                   GO             TO   220-GRP01-CHECK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  BDATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  BDATE
     END-IF.
*
 220-GRP01-CHECK-002.
*バッチ_（時間）チェック
***  バッチ_（時間）未入力チェック
     IF       BTIME  NOT NUMERIC
         OR   BTIME  =   ZERO
                   MOVE   2       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  BTIME
                   MOVE  "C"      TO   EDIT-CURSOR  OF  BTIME
                   GO             TO   220-GRP01-CHECK-EXIT
     ELSE
***           バッチ_（時間）論理チェック
              MOVE     BTIME      TO  WK-JIKAN
              IF       WK-HH  <=  ZERO  OR  WK-HH  >  24 OR
                       WK-MM  <   ZERO  OR  WK-MM  >  59
                       IF     ERR-FLG  =  ZERO
                              MOVE    7    TO    ERR-FLG
                       END-IF
                       MOVE "R"    TO EDIT-OPTION OF BTIME
                       MOVE "C"    TO EDIT-CURSOR OF BTIME
                       GO          TO 220-GRP01-CHECK-EXIT
              ELSE
                       MOVE "M"    TO EDIT-OPTION OF BTIME
                       MOVE SPACE  TO EDIT-CURSOR OF BTIME
              END-IF
     END-IF.
*
 220-GRP01-CHECK-003.
*バッチ_（取引先）チェック
***  バッチ_（取引先）未入力チェック
     IF       TORICD  NOT NUMERIC
         OR   TORICD  =  ZERO
              MOVE   2       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  TORICD
              MOVE  "C"      TO   EDIT-CURSOR  OF  TORICD
              GO             TO   220-GRP01-CHECK-EXIT
     ELSE
***           取引先ＲＥＡＤ
              MOVE     TORICD TO   TOK-F01
              READ     TOKMS2
              INVALID
                     MOVE   4     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  TORICD
                     MOVE  "C"    TO   EDIT-CURSOR  OF  TORICD
                     MOVE   SPACE TO   TORINM
                     GO           TO   220-GRP01-CHECK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  TORICD
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  TORICD
                     MOVE  TOK-F03  TO TORINM
              END-READ
     END-IF.
*
 220-GRP01-CHECK-004.
*倉庫コードチェック
     IF       SOKO      NOT =     SPACE
              MOVE      SOKO      TO   SOK-F01
              READ      ZSOKMS1
                  INVALID  KEY
                        MOVE      SPACE     TO   SOKONM
                        MOVE      3    TO   ERR-FLG
                        MOVE     "C"   TO   EDIT-CURSOR OF SOKO
                        MOVE     "R"   TO   EDIT-OPTION OF SOKO
                        GO             TO   220-GRP01-CHECK-EXIT
                  NOT INVALID  KEY
                        MOVE SOK-F02   TO   SOKONM
                        MOVE     " "   TO   EDIT-CURSOR OF SOKO
                        MOVE     "M"   TO   EDIT-OPTION OF SOKO
              END-READ
     END-IF.
 220-GRP01-CHECK-005.
*納品日チェック
     IF       NDATE  NOT NUMERIC
         OR   NDATE  =  ZERO
*             MOVE   ZERO    TO   NDATE
              CONTINUE
     ELSE
***           バッチ_（日付）論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     NDATE          TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD8
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD8
              IF   LINK-OUT-RET   = 9
                   MOVE   7       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  NDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  NDATE
                   GO             TO   220-GRP01-CHECK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  NDATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  NDATE
     END-IF.
*
 220-GRP01-CHECK-006.
*変更対象店舗チェック
***  未入力チェック
     IF       TENCD   NOT NUMERIC
         OR   TENCD   =  ZERO
              MOVE   2       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  TENCD
              MOVE  "C"      TO   EDIT-CURSOR  OF  TENCD
              GO             TO   220-GRP01-CHECK-EXIT
     ELSE
***           店舗マスタＲＥＡＤ
              MOVE     TORICD     TO   TEN-F52
              MOVE     TENCD      TO   TEN-F011
              READ     TENMS1
              INVALID
                     MOVE   8     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  TENCD
                     MOVE  "C"    TO   EDIT-CURSOR  OF  TENCD
                     MOVE   SPACE TO   TENNM
                     GO           TO   220-GRP01-CHECK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  TENCD
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  TENCD
                     MOVE  TEN-F03  TO TENNM
              END-READ
     END-IF.
*
 220-GRP01-CHECK-007.
*変更後需要家ＩＤチェック
*  未入力チェック
     IF       JYID    =  SPACE
              MOVE   2       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  JYID
              MOVE  "C"      TO   EDIT-CURSOR  OF  JYID
              GO             TO   220-GRP01-CHECK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  JYID
              MOVE  SPACE    TO   EDIT-CURSOR  OF  JYID
     END-IF.
*
*  需要家ＩＤ管理マスタ存在チェック１
***  需要家ＩＤ管理マスタＲＥＡＤ１
     MOVE     TORICD     TO   KY1-F01.
     MOVE     TENCD      TO   KY1-F02.
     READ     KYKJYKL1
     INVALID
              MOVE   9     TO   ERR-FLG
              MOVE  "R"    TO   EDIT-OPTION  OF  TENCD
              MOVE  "C"    TO   EDIT-CURSOR  OF  TENCD
              GO           TO   220-GRP01-CHECK-EXIT
     NOT INVALID
              MOVE  "M"    TO   EDIT-OPTION  OF  TENCD
              MOVE  SPACE  TO   EDIT-CURSOR  OF  TENCD
     END-READ.
*
*  需要家ＩＤ管理マスタ存在チェック２
***  需要家ＩＤ管理マスタＲＥＡＤ２
     MOVE     KY1-F03    TO   KY2-F03.
     MOVE     JYID       TO   KY2-F04.
     READ     KYKJYKL2
     INVALID
              MOVE   9     TO   ERR-FLG
              MOVE  "R"    TO   EDIT-OPTION  OF  JYID
              MOVE  "C"    TO   EDIT-CURSOR  OF  JYID
              GO           TO   220-GRP01-CHECK-EXIT
     NOT INVALID
              MOVE  "M"    TO   EDIT-OPTION  OF  JYID
              MOVE  SPACE  TO   EDIT-CURSOR  OF  JYID
     END-READ.
*
 220-GRP01-CHECK-008.
*売上伝票ファイルＳＴＡＳＲＴ
     MOVE     SPACE     TO   DEN-REC.
     INITIALIZE              DEN-REC.
     MOVE     BDATE     TO   DEN-F46.
     MOVE     BTIME     TO   DEN-F47.
     MOVE     TORICD    TO   DEN-F01.
     IF       SOKO  NOT =    SPACE
              MOVE  SOKO     TO   DEN-F48
     ELSE
              MOVE  SPACE    TO   DEN-F48
     END-IF.
     START    SHTDENLA  KEY  IS   >=   DEN-F46  DEN-F47  DEN-F01
                                       DEN-F48  DEN-F02  DEN-F04
                                       DEN-F051 DEN-F07  DEN-F112
                                       DEN-F03
     INVALID
              MOVE      5    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  BDATE
              MOVE     "R"   TO   EDIT-OPTION  OF  BTIME
              MOVE     "R"   TO   EDIT-OPTION  OF  TORICD
              MOVE     "R"   TO   EDIT-OPTION  OF  SOKO
              MOVE     "R"   TO   EDIT-OPTION  OF  NDATE
              MOVE     "R"   TO   EDIT-OPTION  OF  TENCD
              MOVE     "C"   TO   EDIT-CURSOR  OF  BDATE
              GO             TO   220-GRP01-CHECK-EXIT
     END-START.
*
 220-GRP01-CHECK-009.
*売上ファイル存在チェック
     READ     SHTDENLA
           AT END
                   MOVE   5   TO   ERR-FLG
                   MOVE  "R"  TO   EDIT-OPTION OF BDATE
                   MOVE  "R"  TO   EDIT-OPTION OF BTIME
                   MOVE  "R"  TO   EDIT-OPTION OF TORICD
                   MOVE  "R"  TO   EDIT-OPTION OF SOKO
                   MOVE  "R"  TO   EDIT-OPTION OF NDATE
                   MOVE  "R"  TO   EDIT-OPTION OF TENCD
                   MOVE  "C"  TO   EDIT-CURSOR OF BDATE
                   GO         TO   220-GRP01-CHECK-EXIT
     END-READ.
*
 220-GRP01-CHECK-010.
*   バッチ_チェック
     IF   DEN-F46      =  BDATE   AND
          DEN-F47      =  BTIME   AND
          DEN-F01      =  TORICD
          CONTINUE
     ELSE
          MOVE   5   TO   ERR-FLG
          MOVE  "R"  TO   EDIT-OPTION OF BDATE
          MOVE  "R"  TO   EDIT-OPTION OF BTIME
          MOVE  "R"  TO   EDIT-OPTION OF TORICD
          MOVE  "R"  TO   EDIT-OPTION OF SOKO
          MOVE  "R"  TO   EDIT-OPTION OF NDATE
          MOVE  "R"  TO   EDIT-OPTION OF TENCD
          MOVE  "C"  TO   EDIT-CURSOR OF BDATE
          GO         TO   220-GRP01-CHECK-EXIT
     END-IF.
*
 220-GRP01-CHECK-011.
*   倉庫ＣＤチェック
     IF   SOKO       =    SPACE
          GO         TO   220-GRP01-CHECK-012
     END-IF.
*
     IF   DEN-F48    =  SOKO
          CONTINUE
     ELSE
          MOVE   5   TO   ERR-FLG
          MOVE  "R"  TO   EDIT-OPTION OF BDATE
          MOVE  "R"  TO   EDIT-OPTION OF BTIME
          MOVE  "R"  TO   EDIT-OPTION OF TORICD
          MOVE  "R"  TO   EDIT-OPTION OF SOKO
          MOVE  "R"  TO   EDIT-OPTION OF NDATE
          MOVE  "R"  TO   EDIT-OPTION OF TENCD
          MOVE  "C"  TO   EDIT-CURSOR OF BDATE
          GO         TO   220-GRP01-CHECK-EXIT
     END-IF.
*
 220-GRP01-CHECK-012.
*   相殺区分.伝区ＣＤ.店舗ＣＤ.売上データ作成　チェック
     IF   DEN-F04      =  0       AND
          DEN-F051     =  40      AND
          DEN-F277 NOT =  9       AND
          DEN-F07      =  TENCD
          CONTINUE
     ELSE
          GO         TO   220-GRP01-CHECK-009
     END-IF.
*
 220-GRP01-CHECK-013.
*   納品日チェック
     IF ( NDATE   NOT NUMERIC ) OR ( NDATE   =  ZERO )
*         MOVE    ZERO    TO   NDATE
          GO              TO   220-GRP01-CHECK-014
     END-IF.
*
     IF   DEN-F112   =    NDATE
          CONTINUE
     ELSE
          GO              TO   220-GRP01-CHECK-009
     END-IF.
*
 220-GRP01-CHECK-014.
     MOVE     2         TO        GR-NO.
*
 220-GRP01-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      伝票_範囲入力（右側）　　　　　            *
*--------------------------------------------------------------*
 220-INP-GRP02          SECTION.
     MOVE     "220-INP-GRP02"     TO   S-NAME.
     MOVE     "GRP002"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
     MOVE     ZERO      TO        ERR-FLG.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      1         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-GRP002-RTN
              PERFORM   220-GRP02-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE           1    TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      入力伝票範囲チェック                        *
*--------------------------------------------------------------*
 220-GRP02-CHECK-SEC    SECTION.
     MOVE     "220-CRP02-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
     MOVE     ZERO      TO        SONZAI-FLG.
     PERFORM  VARYING   IX   FROM  1  BY  1  UNTIL  IX  >  12
*        伝票_範囲
         IF ( SDENNO(IX)     IS   NUMERIC   AND
              EDENNO(IX)     IS   NUMERIC )
              IF   SDENNO(IX)     >    EDENNO(IX)
                   IF   ERR-FLG   =    ZERO
                        MOVE      6    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF SDENNO(IX)
                   MOVE     "R"   TO   EDIT-OPTION OF SDENNO(IX)
                   MOVE     "R"   TO   EDIT-OPTION OF EDENNO(IX)
              ELSE
                   MOVE     " "   TO   EDIT-CURSOR OF SDENNO(IX)
                   MOVE     "M"   TO   EDIT-OPTION OF SDENNO(IX)
                   MOVE     "M"   TO   EDIT-OPTION OF EDENNO(IX)
              END-IF
         ELSE
              IF ( SDENNO(IX)     NOT  NUMERIC ) AND
                 ( EDENNO(IX)     IS   NUMERIC )
                   MOVE      2    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF SDENNO(IX)
                   MOVE     "R"   TO   EDIT-OPTION OF SDENNO(IX)
                   MOVE     "M"   TO   EDIT-OPTION OF EDENNO(IX)
              END-IF
         END-IF
     END-PERFORM.
*
 220-GRP02-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認入力                                    *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "230-INP-KKNN"      TO   S-NAME.
     MOVE     "KKNN"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      2         TO   GR-NO
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
*    LEVEL  3      パラメタ出力                                *
*--------------------------------------------------------------*
 240-PARA-SEC           SECTION.
     MOVE     "240-PARA-SEC"      TO   S-NAME.
*
     MOVE     BDATE                    TO   PARA-OUT-BDATE.
     MOVE     BTIME                    TO   PARA-OUT-BTIME.
     MOVE     TORICD                   TO   PARA-OUT-BTORI.
     MOVE     SOKO                     TO   PARA-OUT-SOKO.
     IF       NDATE  NOT NUMERIC
              MOVE   ZERO              TO   PARA-OUT-NDATE
     ELSE
              MOVE   NDATE             TO   PARA-OUT-NDATE
     END-IF.
     MOVE     TENCD                    TO   PARA-OUT-CHGTEN.
     MOVE     JYID                     TO   PARA-OUT-CHGJYK.
     IF       SDENNO(1)  NOT NUMERIC
              MOVE   ZERO              TO   PARA-OUT-SDENNO01
     ELSE
              MOVE   SDENNO(1)         TO   PARA-OUT-SDENNO01
     END-IF.
     IF     ( EDENNO(1)  NOT NUMERIC ) OR
            ( EDENNO(1)  =   ZERO    )
              MOVE   PARA-OUT-SDENNO01 TO   PARA-OUT-EDENNO01
     ELSE
              MOVE   EDENNO(1)         TO   PARA-OUT-EDENNO01
     END-IF.
*
     IF       SDENNO(2)  NOT NUMERIC
              MOVE   ZERO              TO   PARA-OUT-SDENNO02
     ELSE
              MOVE   SDENNO(2)         TO   PARA-OUT-SDENNO02
     END-IF.
     IF     ( EDENNO(2)  NOT NUMERIC ) OR
            ( EDENNO(2)  =   ZERO    )
              MOVE   PARA-OUT-SDENNO02 TO   PARA-OUT-EDENNO02
     ELSE
              MOVE   EDENNO(2)         TO   PARA-OUT-EDENNO02
     END-IF.
*
     IF       SDENNO(3)  NOT NUMERIC
              MOVE   ZERO              TO   PARA-OUT-SDENNO03
     ELSE
              MOVE   SDENNO(3)         TO   PARA-OUT-SDENNO03
     END-IF.
     IF     ( EDENNO(3)  NOT NUMERIC ) OR
            ( EDENNO(3)  =   ZERO    )
              MOVE   PARA-OUT-SDENNO03 TO   PARA-OUT-EDENNO03
     ELSE
              MOVE   EDENNO(3)         TO   PARA-OUT-EDENNO03
     END-IF.
*
     IF       SDENNO(4)  NOT NUMERIC
              MOVE   ZERO              TO   PARA-OUT-SDENNO04
     ELSE
              MOVE   SDENNO(4)         TO   PARA-OUT-SDENNO04
     END-IF.
     IF     ( EDENNO(4)  NOT NUMERIC ) OR
            ( EDENNO(4)  =   ZERO    )
              MOVE   PARA-OUT-SDENNO04 TO   PARA-OUT-EDENNO04
     ELSE
              MOVE   EDENNO(4)         TO   PARA-OUT-EDENNO04
     END-IF.
*
     IF       SDENNO(5)  NOT NUMERIC
              MOVE   ZERO              TO   PARA-OUT-SDENNO05
     ELSE
              MOVE   SDENNO(5)         TO   PARA-OUT-SDENNO05
     END-IF.
     IF     ( EDENNO(5)  NOT NUMERIC ) OR
            ( EDENNO(5)  =   ZERO    )
              MOVE   PARA-OUT-SDENNO05 TO   PARA-OUT-EDENNO05
     ELSE
              MOVE   EDENNO(5)         TO   PARA-OUT-EDENNO05
     END-IF.
*
     IF       SDENNO(6)  NOT NUMERIC
              MOVE   ZERO              TO   PARA-OUT-SDENNO06
     ELSE
              MOVE   SDENNO(6)         TO   PARA-OUT-SDENNO06
     END-IF.
     IF     ( EDENNO(6)  NOT NUMERIC ) OR
            ( EDENNO(6)  =   ZERO    )
              MOVE   PARA-OUT-SDENNO06 TO   PARA-OUT-EDENNO06
     ELSE
              MOVE   EDENNO(6)         TO   PARA-OUT-EDENNO06
     END-IF.
*
     IF       SDENNO(7)  NOT NUMERIC
              MOVE   ZERO              TO   PARA-OUT-SDENNO07
     ELSE
              MOVE   SDENNO(7)         TO   PARA-OUT-SDENNO07
     END-IF.
     IF     ( EDENNO(7)  NOT NUMERIC ) OR
            ( EDENNO(7)  =   ZERO    )
              MOVE   PARA-OUT-SDENNO07 TO   PARA-OUT-EDENNO07
         ELSE
              MOVE   EDENNO(7)         TO   PARA-OUT-EDENNO07
     END-IF.
*
     IF       SDENNO(8)  NOT NUMERIC
              MOVE   ZERO              TO   PARA-OUT-SDENNO08
     ELSE
              MOVE   SDENNO(8)         TO   PARA-OUT-SDENNO08
     END-IF.
     IF     ( EDENNO(8)  NOT NUMERIC ) OR
            ( EDENNO(8)  =   ZERO    )
              MOVE   PARA-OUT-SDENNO08 TO   PARA-OUT-EDENNO08
     ELSE
              MOVE   EDENNO(8)         TO   PARA-OUT-EDENNO08
     END-IF.
*
     IF       SDENNO(9)  NOT NUMERIC
              MOVE   ZERO              TO   PARA-OUT-SDENNO09
     ELSE
              MOVE   SDENNO(9)         TO   PARA-OUT-SDENNO09
     END-IF.
     IF     ( EDENNO(9)  NOT NUMERIC ) OR
            ( EDENNO(9)  =   ZERO    )
              MOVE   PARA-OUT-SDENNO09 TO   PARA-OUT-EDENNO09
     ELSE
              MOVE   EDENNO(9)         TO   PARA-OUT-EDENNO09
     END-IF.
*
     IF       SDENNO(10) NOT NUMERIC
              MOVE   ZERO              TO   PARA-OUT-SDENNO10
     ELSE
              MOVE   SDENNO(10)        TO   PARA-OUT-SDENNO10
     END-IF.
     IF     ( EDENNO(10) NOT NUMERIC ) OR
            ( EDENNO(10) =   ZERO    )
              MOVE   PARA-OUT-SDENNO10 TO   PARA-OUT-EDENNO10
     ELSE
              MOVE   EDENNO(10)        TO   PARA-OUT-EDENNO10
     END-IF.
*
     IF       SDENNO(11) NOT NUMERIC
              MOVE   ZERO              TO   PARA-OUT-SDENNO11
     ELSE
              MOVE   SDENNO(11)        TO   PARA-OUT-SDENNO11
     END-IF.
     IF     ( EDENNO(11) NOT NUMERIC ) OR
            ( EDENNO(11) =   ZERO    )
              MOVE   PARA-OUT-SDENNO11 TO   PARA-OUT-EDENNO11
     ELSE
              MOVE   EDENNO(11)        TO   PARA-OUT-EDENNO11
     END-IF.
*
     IF       SDENNO(12) NOT NUMERIC
              MOVE   ZERO              TO   PARA-OUT-SDENNO12
     ELSE
              MOVE   SDENNO(12)        TO   PARA-OUT-SDENNO12
     END-IF.
     IF     ( EDENNO(12) NOT NUMERIC ) OR
            ( EDENNO(12) =   ZERO    )
              MOVE   PARA-OUT-SDENNO12 TO   PARA-OUT-EDENNO12
         ELSE
              MOVE   EDENNO(12)        TO   PARA-OUT-EDENNO12
     END-IF.
*
     MOVE     99                  TO   GR-NO.
*
 240-PARA-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     画面ＲＥＡＤ                                *
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
*    行_ｾｯﾄ
*    PERFORM  VARYING   IX   FROM  1  BY  1  UNTIL  IX  >  12
*        MOVE      IX        TO   BANGO(IX)
*    END-PERFORM.
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
*    LEVEL ALL     画面ＷＲＩＴＥ                              *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
     MOVE     "900-DSP-WRITE"     TO   S-NAME.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FSY08011.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      条件入力（左側）　属性クリア　　　　　　　　*
*--------------------------------------------------------------*
 CLR-GRP001-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF BDATE
                                  EDIT-CURSOR OF BTIME
                                  EDIT-CURSOR OF TORICD
                                  EDIT-CURSOR OF SOKO
                                  EDIT-CURSOR OF NDATE
                                  EDIT-CURSOR OF TENCD
                                  EDIT-CURSOR OF JYID.
     MOVE     "M"            TO   EDIT-OPTION OF BDATE
                                  EDIT-OPTION OF BTIME
                                  EDIT-OPTION OF TORICD
                                  EDIT-OPTION OF SOKO
                                  EDIT-OPTION OF NDATE
                                  EDIT-OPTION OF TENCD
                                  EDIT-OPTION OF JYID.
 CLR-GRP001-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      伝票_範囲入力（右側）　属性クリア　　　　　*
*--------------------------------------------------------------*
 CLR-GRP002-RTN           SECTION.
*
     PERFORM  VARYING   IX   FROM  1  BY  1  UNTIL  IX  >  12
         MOVE     " "        TO   EDIT-CURSOR OF SDENNO(IX)
                                  EDIT-CURSOR OF EDENNO(IX)
         MOVE     "M"        TO   EDIT-OPTION OF SDENNO(IX)
                                  EDIT-OPTION OF EDENNO(IX)
     END-PERFORM.
*
 CLR-GRP002-EXIT.
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
