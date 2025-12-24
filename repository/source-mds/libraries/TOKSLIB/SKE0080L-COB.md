# SKE0080L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKE0080L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿向け　　　　　*
*    業務名　　　　　　　：　出荷検品業務                      *
*    モジュール名　　　　：　検品グループマスタリスト          *
*    作成日／更新日　　　：　00/10/30                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　検品Ｇマスタリストを出力する。　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKE0080L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/10/30.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 表示ファイル >>--*
     SELECT   DSPFILE   ASSIGN    TO     01-GS-DSPF
                        FORMAT           DSP-FMT
                        GROUP            DSP-GRP
                        PROCESSING       DSP-PRO
                        UNIT CONTROL     DSP-CON
                        FUNCTION         DSP-FNC
                        STATUS           DSP-ST.
*----<< 検品グループマスタ >>--*
     SELECT   SOKKPGF   ASSIGN    TO     DA-01-VI-SOKKPGL1
                        ORGANIZATION     INDEXED
                        ACCESS    MODE   RANDOM
                        RECORD    KEY    KPG-F01   KPG-F02
                        STATUS           KPG-ST.
*----<< 倉庫マスタ >>--*
     SELECT   ZSOKMS    ASSIGN    TO     DA-01-VI-ZSOKMS1
                        ORGANIZATION     INDEXED
                        ACCESS    MODE   RANDOM
                        RECORD    KEY    SOK-F01
                        STATUS           SOK-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FKE00801  OF        XMDLIB.
*----<< 店舗マスタ >>--*
 FD  SOKKPGF            LABEL RECORD   IS   STANDARD.
     COPY     SOKKPGF   OF        XFDLIB
              JOINING   KPG       PREFIX.
*----<< 倉庫マスタ>>-*
 FD  ZSOKMS
                        LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  SKIP-FLG       PIC  9(01).
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
 01  IDX.
     03  I              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  KPG-ST            PIC  X(02).
 01  SOK-ST            PIC  X(02).
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
 01  WK-SYSTIME.
     03  WK-SYSHH       PIC  9(02).
     03  FILLER         PIC  X(01)     VALUE ":".
     03  WK-SYSMN       PIC  9(02).
     03  FILLER         PIC  X(01)     VALUE ":".
     03  WK-SYSSS       PIC  9(02).
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH          PIC   9(02)  VALUE  ZERO.
     03  WK-MM          PIC   9(02)  VALUE  ZERO.
*
 01  WK-SOKOCD          PIC  X(02)   VALUE  SPACE.
*開始，終了倉庫コード
 01  SOKCD1             PIC  X(02)   VALUE  ZERO.
 01  SOKCD2             PIC  X(02)   VALUE  ZERO.
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  WK-32          PIC  X(25).
     03  WK-33          PIC  S9(12)V99   PACKED-DECIMAL.
     03  WK-34          PIC  S9(12)V99   PACKED-DECIMAL.
     03  WK-35          PIC  S9(12)V99   PACKED-DECIMAL.
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-01.
             07  NEW-DAT                    PIC  9(08).
             07  NEW-TIM                    PIC  9(04).
             07  NEW-TOR                    PIC  9(08).
             07  NEW-SOK                    PIC  X(02).
             07  NEW-DEN                    PIC  9(09).
     03  OLD.
         05  OLD-01.
             07  OLD-DAT                    PIC  9(08).
             07  OLD-TIM                    PIC  9(04).
             07  OLD-TOR                    PIC  9(08).
             07  OLD-SOK                    PIC  X(02).
             07  OLD-DEN                    PIC  9(09).
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
 01  GUIDE01       PIC  N(40)  VALUE
         NC"_取　消　_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
         NC"_取　消　_終　了　_戻る".
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  X(08)     VALUE  "SKE0080L".
         05  FILLER     PIC  X(28)     VALUE  SPACE.
         05  FILLER     PIC  N(16)     VALUE
             NC"【　検品グループマスタリスト　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(16)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"処理日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-011     PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-02      PIC  ZZ9.
 01  HEAD02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"倉庫：".
         05  HD-031     PIC  X(02).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-032     PIC  N(10).
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"検品ＧＣＤ".
         05  FILLER     PIC  X(30)     VALUE  SPACE.
         05  FILLER     PIC  X(07)     VALUE  NC"ﾋﾟｯｷﾝｸﾞ".
         05  FILLER     PIC  N(02)     VALUE  NC"種別".
 01  P-LINE1                     CHARACTER  TYPE  MODE-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(02).
         05  ME-03      PIC  9(01).
         05  FILLER     PIC  X(01).
         05  ME-04      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-05      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-06      PIC  X(09).
         05  FILLER     PIC  X(02).
         05  ME-07      PIC  X(02).
         05  FILLER     PIC  X(03).
         05  ME-08      PIC  X(02).
         05  FILLER     PIC  X(02).
         05  ME-09      PIC  9(05).
         05  FILLER     PIC  X(01).
         05  ME-10      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-11      PIC  9(02).
         05  FILLER     PIC  X(01).
         05  ME-12      PIC  N(04).
         05  FILLER     PIC  X(01).
         05  ME-13      PIC  X(07).
         05  FILLER     PIC  X(01).
         05  ME-14      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-15      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-16      PIC  X(04).
         05  FILLER     PIC  X(03).
         05  ME-17      PIC  X(02).
         05  FILLER     PIC  X(02).
         05  ME-18      PIC  X(02).
         05  FILLER     PIC  X(03).
         05  ME-19      PIC  9(01).
         05  FILLER     PIC  X(05).
     03  FILLER         CHARACTER TYPE MODE-2.
         05  ME-20      PIC  N(02).
 01  MEIS02.
     03  FILLER.
         05  FILLER     PIC  X(01).
         05  ME-21      PIC  Z9.
         05  FILLER     PIC  X(01).
         05  ME-22      PIC  X(13).
         05  FILLER     PIC  X(01).
         05  ME-23      PIC  X(30).
         05  ME-241      PIC  --,---,--9.99.
         05  FILLER     PIC  X(02).
         05  ME-242     PIC  X(01)     VALUE  "(".
         05  ME-243     PIC  --,---,--9.99.
         05  ME-244     PIC  X(01)     VALUE  ")".
         05  FILLER     PIC  X(02).
         05  ME-25      PIC  X(01).
         05  ME-26      PIC  --,---,--9.99.
         05  ME-27      PIC  ---,---,--9.
         05  ME-28      PIC  --,---,--9.99.
         05  ME-29      PIC  ---,---,--9.
 01  MEIS03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  ME-32L     PIC  N(03)     VALUE  NC"（備考".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME-32      PIC  X(25).
         05  ME-32R     PIC  N(01)     VALUE  NC"）".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"合　計".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME-33      PIC  ---,---,--9.99.
         05  FILLER     PIC  X(32)     VALUE  SPACE.
         05  ME-34      PIC  ----,---,--9.
         05  FILLER     PIC  X(12)     VALUE  SPACE.
         05  ME-35      PIC  ----,---,--9.
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE2            PIC  X(136)    VALUE  ALL   "-".
*
 01  MSG-AREA.
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
     03  LINK-OUT-YMD       PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-SOKO              PIC   X(02).
 01  PARA-DSOKO             PIC   X(02).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-SOKO
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
     DISPLAY  "### SKE0080L DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    HTENMS    ZSOKMS   DSPFILE.
     STOP     RUN.
*----<< 伝票データ >>--*
 SHTDENF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKE0080L SHTDENF ERROR " SHTDENF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    HTENMS    ZSOKMS   DSPFILE.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKE0080L HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    HTENMS    ZSOKMS   DSPFILE.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKE0080L HTENMS ERROR " HTENMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    HTENMS    ZSOKMS   DSPFILE.
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKE0080L ZSOKMS ERROR " ZSOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    HTENMS    ZSOKMS   DSPFILE.
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
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     SYS-YYW        TO   WK-SYSYY.
     MOVE     SYS-MMW        TO   WK-SYSMM.
     MOVE     SYS-DDW        TO   WK-SYSDD.
*
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     SYS-HH         TO   WK-SYSHH.
     MOVE     SYS-MN         TO   WK-SYSMN.
     MOVE     SYS-SS         TO   WK-SYSSS.
*
     DISPLAY  "*** SKE0080L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     I-O       SHTDENF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
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
*----<< ﾊﾝｲ ｼﾃｲ ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ｲﾝｻﾂ >>-*
     PERFORM  240-PRINT      UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    SHTDENF.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
     CLOSE    ZSOKMS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE0080L END *** "
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
     MOVE     SPACE          TO   FKE00801.
*
*倉庫コードパラメタより倉庫コード入力チェック
*****IF       PARA-SOKO  =  0  OR  1
     IF       PARA-DSOKO  =  "01"
              MOVE      " "       TO   EDIT-STATUS OF SOKCD
     ELSE
              MOVE      PARA-SOKO TO   SOK-F01 SOKCD
              READ      ZSOKMS
                  INVALID  KEY
                        MOVE      ALL NC"＊" TO   SOKNM
              NOT INVALID  KEY
                        MOVE      SOK-F02    TO   SOKNM
              END-READ
              MOVE    "X"    TO   EDIT-STATUS OF SOKCD
     END-IF.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FKE00801"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     PERFORM  215-DSP-SYOKI.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             画面制御項目初期化                               *
*--------------------------------------------------------------*
 215-DSP-SYOKI         SECTION.
     MOVE     "215-DSP-SYOKI"      TO   S-NAME.
*
*リバース，カーソルパーク解除
***  バッチ_（日付）
     MOVE    "M"      TO  EDIT-OPTION  OF  BTDATE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  BTDATE.
***  バッチ_（時間）
     MOVE    "M"      TO  EDIT-OPTION  OF  BTTIME.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  BTTIME.
***  バッチ_（取引先）
     MOVE    "M"      TO  EDIT-OPTION  OF  BTTORI.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  BTTORI.
***  開始倉庫コード
     MOVE    "M"      TO  EDIT-OPTION  OF  SOKCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  SOKCD.
***  終了倉庫コード
***  MOVE    "M"      TO  EDIT-OPTION  OF  SOKCD2.
***  MOVE    SPACE    TO  EDIT-CURSOR  OF  SOKCD2.
***  開始伝票_
     MOVE    "M"      TO  EDIT-OPTION  OF  DENNO1.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DENNO1.
***  終了伝票_
     MOVE    "M"      TO  EDIT-OPTION  OF  DENNO2.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DENNO2.
***  開始納品日
     MOVE    "M"      TO  EDIT-OPTION  OF  SNOYMD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  SNOYMD.
***  終了納品日
     MOVE    "M"      TO  EDIT-OPTION  OF  ENOYMD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  ENOYMD.
***  開始店舗
     MOVE    "M"      TO  EDIT-OPTION  OF  STENCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  STENCD.
***  終了店舗
     MOVE    "M"      TO  EDIT-OPTION  OF  ETENCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  ETENCD.
*
 215-DSP-SYOKI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲ      ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP02"     TO   S-NAME.
     MOVE     "PARGRP"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
******** WHEN PF09
********      MOVE      1         TO   GR-NO
         WHEN ENT
              PERFORM   225-PARA-CHK

         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             パラメタチェック                                 *
*--------------------------------------------------------------*
 225-PARA-CHK                 SECTION.
     MOVE     "PARA-CHK"     TO   S-NAME.
*
*バッチ_（日付）チェック
***  バッチ_（日付）未入力チェック
     IF       BTDATE  NOT NUMERIC
         OR   BTDATE  =  ZERO
              MOVE NC"バッチ_を入力して下さい"    TO  MSG
              MOVE  "R"      TO   EDIT-OPTION  OF  BTDATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  BTDATE
              GO             TO   PARA-CHK-EXIT
     ELSE
***           バッチ_（日付）論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     BTDATE         TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE   NC"バッチ_論理エラー"   TO   MSG
                   MOVE  "R"      TO   EDIT-OPTION  OF  BTDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  BTDATE
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  BTDATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  BTDATE
     END-IF.
*
*バッチ_（時間）チェック
***  バッチ_（時間）未入力チェック
     IF       BTTIME  NOT NUMERIC
         OR   BTTIME  =   ZERO
                   MOVE NC"バッチ_を入力して下さい"    TO  MSG
                   MOVE  "R"      TO   EDIT-OPTION  OF  BTTIME
                   MOVE  "C"      TO   EDIT-CURSOR  OF  BTTIME
                   GO             TO   PARA-CHK-EXIT
     ELSE
***           バッチ_（時間）論理チェック
              MOVE     BTTIME    TO  WK-JIKAN
              IF       WK-HH  <=  ZERO  OR  WK-HH  >  24 OR
                       WK-MM  <   ZERO  OR  WK-MM  >  59
                       MOVE   NC"バッチ_論理エラー"   TO   MSG
                       MOVE "R"    TO EDIT-OPTION OF BTTIME
                       MOVE "C"    TO EDIT-CURSOR OF BTTIME
                       GO          TO PARA-CHK-EXIT
              ELSE
                       MOVE "M"    TO EDIT-OPTION OF BTTIME
                       MOVE SPACE  TO EDIT-CURSOR OF BTTIME
              END-IF
     END-IF.
*
*バッチ_（取引先）チェック
***  バッチ_（取引先）未入力チェック
     IF       BTTORI  NOT NUMERIC
         OR   BTTORI  =  ZERO
              MOVE NC"バッチ_を入力して下さい"    TO  MSG
              MOVE  "R"      TO   EDIT-OPTION  OF  BTTORI
              MOVE  "C"      TO   EDIT-CURSOR  OF  BTTORI
              GO             TO   PARA-CHK-EXIT
     ELSE
***           取引先ＲＥＡＤ
              MOVE     BTTORI TO   TOK-F01
              READ     HTOKMS
              INVALID
                     MOVE
                     NC"取引先マスタに登録されていません" TO MSG
                     MOVE  "R"    TO   EDIT-OPTION  OF  BTTORI
                     MOVE  "C"    TO   EDIT-CURSOR  OF  BTTORI
                     MOVE   SPACE TO   TORINM
                     GO           TO   PARA-CHK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  BTTORI
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  BTTORI
                     MOVE  TOK-F03  TO TORINM
              END-READ
     END-IF.
*
*    倉庫コードチェック
*****IF     ( SOKCD     IS   NUMERIC   )   AND
************( SOKCD     NOT =     ZERO )
     IF     ( SOKCD     NOT =     SPACE )
     AND    ( SOKCD     NOT =     "00"  )
              MOVE      SOKCD     TO   SOK-F01
              READ      ZSOKMS
                  INVALID  KEY
                        MOVE      SPACE     TO   SOKNM
                        MOVE
                       NC"倉庫マスタに登録されていません" TO MSG
                        MOVE     "C"   TO   EDIT-CURSOR OF SOKCD
                        MOVE     "R"   TO   EDIT-OPTION OF SOKCD
                        GO             TO   PARA-CHK-EXIT
              NOT INVALID  KEY
                        MOVE      SOK-F02   TO   SOKNM
                        MOVE      SOKCD     TO   SOKCD1
                                                 SOKCD2
              END-READ
     ELSE
**************MOVE      ZERO           TO   SOKCD
**************MOVE      "00"           TO   SOKCD
              MOVE      NC"全倉庫"     TO   SOKNM
**************MOVE      ZERO           TO   SOKCD1
              MOVE      SPACE          TO   SOKCD1
              MOVE      99             TO   SOKCD2
     END-IF.
*開始倉庫コードチェック
***  未入力時，開始倉庫コード＝０
*    IF       SOKCD1   NOT NUMERIC
*        OR   SOKCD1   =  ZERO
*             MOVE     ZERO       TO   SOKCD1
*    END-IF.
*
*終了倉庫コードチェック
***  未入力時，終了倉庫コード＝９９
*    IF       SOKCD2   NOT NUMERIC
*       OR    SOKCD2   =  ZERO
*             MOVE         99     TO   SOKCD2
*    END-IF.
*
*倉庫コード大小チェック
*    IF       SOKCD1  >  SOKCD2
*             MOVE      NC"範囲指定が違います"   TO   MSG
*             MOVE     "R"   TO   EDIT-OPTION  OF  SOKCD1
*             MOVE     "R"   TO   EDIT-OPTION  OF  SOKCD2
*             MOVE     "C"   TO   EDIT-CURSOR  OF  SOKCD1
*             GO             TO   PARA-CHK-EXIT
*    ELSE
*             MOVE     "M"   TO   EDIT-OPTION  OF  SOKCD1
*             MOVE     "M"   TO   EDIT-OPTION  OF  SOKCD2
*             MOVE     SPACE TO   EDIT-CURSOR  OF  SOKCD1
*    END-IF.
*開始伝票_チェック
***  未入力時，開始伝票_＝０
     IF       DENNO1   NOT NUMERIC
         OR   DENNO1   =  ZERO
              MOVE     ZERO       TO   DENNO1
     END-IF.
*
*終了伝票_チェック
***  未入力時，終了伝票_＝ALL 9
     IF       DENNO2   NOT NUMERIC
        OR    DENNO2   =  ZERO
              MOVE     999999999  TO   DENNO2
     END-IF.
*
*伝票_大小チェック
     IF       SOKCD1  >  SOKCD2
              MOVE      NC"範囲指定が違います"   TO   MSG
              MOVE     "R"   TO   EDIT-OPTION  OF  DENNO1
              MOVE     "R"   TO   EDIT-OPTION  OF  DENNO2
              MOVE     "C"   TO   EDIT-CURSOR  OF  DENNO1
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DENNO1
              MOVE     "M"   TO   EDIT-OPTION  OF  DENNO2
              MOVE     SPACE TO   EDIT-CURSOR  OF  DENNO1
     END-IF.
*納品日範囲チェック
*    （開始納品日チェック）
     IF       SNOYMD   NOT NUMERIC
     OR       SNOYMD   =     ZERO
              MOVE     ZERO  TO   SNOYMD
              MOVE    "M"    TO   EDIT-OPTION  OF  SNOYMD
              MOVE     SPACE TO   EDIT-CURSOR  OF  SNOYMD
     END-IF.
*    （終了納品日チェック）
     IF       ENOYMD   NOT NUMERIC
     OR       ENOYMD   =     ZERO
              MOVE     99999999   TO   ENOYMD
              MOVE    "M"    TO   EDIT-OPTION  OF  ENOYMD
              MOVE     SPACE TO   EDIT-CURSOR  OF  ENOYMD
     END-IF.
*    開始納品日論理チェック
     IF       SNOYMD   NOT =  ZERO
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     SNOYMD         TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  NC"開始納品日を正しく入力して下さい"
                                  TO   MSG
                   MOVE  "R"      TO   EDIT-OPTION  OF  SNOYMD
                   MOVE  "C"      TO   EDIT-CURSOR  OF  SNOYMD
                   GO             TO   PARA-CHK-EXIT
              ELSE
                   MOVE  "M"      TO   EDIT-OPTION  OF  SNOYMD
                   MOVE   SPACE   TO   EDIT-CURSOR  OF  SNOYMD
              END-IF
     END-IF.
*    終了納品日論理チェック
     IF       ENOYMD  NOT =  ZERO     AND  99999999
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     ENOYMD         TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  NC"終了納品日を正しく入力して下さい"
                                  TO   MSG
                   MOVE  "R"      TO   EDIT-OPTION  OF  ENOYMD
                   MOVE  "C"      TO   EDIT-CURSOR  OF  ENOYMD
                   GO             TO   PARA-CHK-EXIT
              ELSE
                   MOVE  "M"      TO   EDIT-OPTION  OF  ENOYMD
                   MOVE   SPACE   TO   EDIT-CURSOR  OF  ENOYMD
              END-IF
     END-IF.
*納品日入力範囲チェック
     IF       SNOYMD  >  ENOYMD
              MOVE  NC"開始が終了を超えています"  TO  MSG
              MOVE  "R"      TO   EDIT-OPTION  OF  SNOYMD
              MOVE  "R"      TO   EDIT-OPTION  OF  ENOYMD
              MOVE  "C"      TO   EDIT-CURSOR  OF  SNOYMD
              GO             TO   PARA-CHK-EXIT
     END-IF.
*店舗ＣＤ範囲チェック
*    （開始店舗チェック）
     IF       STENCD   NOT NUMERIC
     OR       STENCD   =     ZERO
              MOVE     ZERO  TO   STENCD
              MOVE    "M"    TO   EDIT-OPTION  OF  STENCD
              MOVE     SPACE TO   EDIT-CURSOR  OF  STENCD
     END-IF.
*    （終了店舗チェック）
     IF       ETENCD   NOT NUMERIC
     OR       ETENCD   =     ZERO
              MOVE     99999      TO   ETENCD
              MOVE    "M"    TO   EDIT-OPTION  OF  ETENCD
              MOVE     SPACE TO   EDIT-CURSOR  OF  ETENCD
     END-IF.
*納品日入力範囲チェック
     IF       STENCD  >  ETENCD
              MOVE  NC"開始が終了を超えています"  TO  MSG
              MOVE  "R"      TO   EDIT-OPTION  OF  STENCD
              MOVE  "R"      TO   EDIT-OPTION  OF  ETENCD
              MOVE  "C"      TO   EDIT-CURSOR  OF  STENCD
              GO             TO   PARA-CHK-EXIT
     END-IF.
*
*売上伝票ファイル存在チェック
     MOVE     BTDATE     TO   DEN-F46.
     MOVE     BTTIME     TO   DEN-F47.
     MOVE     BTTORI     TO   DEN-F01.
     MOVE     SOKCD1     TO   DEN-F48.
     MOVE     DENNO1     TO   DEN-F02.
     START    SHTDENF   KEY  IS   >=   DEN-F46
                                       DEN-F47
                                       DEN-F01
                                       DEN-F48
                                       DEN-F02
     INVALID
              MOVE     NC"対象データが存在しません"  TO  MSG
              MOVE     "C"   TO   EDIT-CURSOR  OF  BTDATE
              GO             TO   PARA-CHK-EXIT
     NOT INVALID
              READ     SHTDENF
              END-READ
*
              IF       BTDATE =    DEN-F46
                 AND   BTTIME =    DEN-F47
                 AND   BTTORI =    DEN-F01
                 AND ( SOKCD1 <=   DEN-F48 AND
                       SOKCD2 >=   DEN-F48     )
                 AND ( DENNO1  <=  DEN-F02 AND
                       DENNO2  >=  DEN-F02     )
                 AND   DEN-F274 =  1
                 AND   DEN-F277 =  0
*
                       MOVE  SPACE TO  EDIT-CURSOR OF BTDATE
              ELSE
                       MOVE NC"対象データが存在しません" TO MSG
                       MOVE  "C"  TO   EDIT-CURSOR OF BTDATE
                       GO         TO   PARA-CHK-EXIT
              END-IF
     END-START.
*
     MOVE     9        TO    GR-NO.
     CLOSE    SHTDENF.
     OPEN     I-O  SHTDENF.
*
 PARA-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "230-INP-KKNN"      TO   S-NAME.
     MOVE     "CHKGRP"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      1         TO   GR-NO
         WHEN ENT
              MOVE      10        TO   GR-NO
         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSG
     END-EVALUATE.
*
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｲﾝｻﾂ                                        *
*--------------------------------------------------------------*
 240-PRINT              SECTION.
     MOVE     "240-PRINT"    TO   S-NAME.
     OPEN     OUTPUT    PRTF.
     MOVE     99             TO   LINE-CNT.
     MOVE     0              TO   PAGE-CNT.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     INITIALIZE                   GOKEI-AREA.
*
     MOVE     ZERO           TO   DEN-F46.
     MOVE     ZERO           TO   DEN-F47.
     MOVE     ZERO           TO   DEN-F01.
     MOVE     ZERO           TO   DEN-F48.
     MOVE     ZERO           TO   DEN-F02.
     MOVE     BTDATE         TO   DEN-F46.
     MOVE     BTTIME         TO   DEN-F47.
     MOVE     BTTORI         TO   DEN-F01.
     MOVE     SOKCD1         TO   DEN-F48.
     MOVE     DENNO1         TO   DEN-F02.
     PERFORM  900-DEN-START-READ.
*倉庫コードブレイクによる改ページ用
     MOVE     DEN-F48             TO   WK-SOKOCD.
*
     PERFORM  241-LIST-PRINT
                        UNTIL     OLD  =    HIGH-VALUE.
     CLOSE    PRTF.
*
     MOVE     0              TO   GR-NO.
 240-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｲﾝｻﾂ                                        *
*--------------------------------------------------------------*
 241-LIST-PRINT         SECTION.
     MOVE    "241-LIST-PRINT"     TO   S-NAME.
*
*
     IF       OLD  NOT  =    LOW-VALUE
     AND      NEW  NOT  =    OLD
              PERFORM   2413-MEIS03-PRINT
     END-IF.
*
     IF       NEW  NOT  =    HIGH-VALUE
     AND      NEW  NOT  =    OLD
              PERFORM   2411-MEIS01-PRINT
     END-IF.
*
     IF       NEW       NOT  =    HIGH-VALUE
     AND      DEN-F03   NOT  =    80
              PERFORM   2412-MEIS02-PRINT
     END-IF.
*
     MOVE     NEW            TO   OLD.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   2414-SYUKEI
              PERFORM   900-DEN-READ
     END-IF.
 241-LIST-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2411-MEIS01-PRINT      SECTION.
     MOVE    "2411-MEIS01-PRINT"  TO   S-NAME.
     IF       LINE-CNT  >    62
              PERFORM   2415-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
     MOVE     DEN-F04        TO   ME-03.
     MOVE     DEN-F01        TO   ME-04.
     MOVE     DEN-F01        TO   TOK-F01.
     PERFORM  900-TOK-READ.
     MOVE     TOK-F03        TO   ME-05.
     MOVE     DEN-F02        TO   ME-06.
     MOVE     DEN-F08        TO   ME-07.
     MOVE     DEN-F09        TO   ME-08.
     MOVE     DEN-F07        TO   ME-09.
     MOVE     DEN-F01        TO   TEN-F52.
     MOVE     DEN-F07        TO   TEN-F011.
     PERFORM  900-TEN-READ.
     MOVE     TEN-F03        TO   ME-10.
     MOVE     DEN-F051       TO   ME-11.
     MOVE     DEN-F052       TO   ME-12.
     MOVE     DEN-F10        TO   ME-13.
     MOVE     DEN-F111       TO   ME-14.
     MOVE     DEN-F112       TO   ME-15.
     MOVE     DEN-F12        TO   ME-16.
     MOVE     DEN-F131       TO   ME-17.
     MOVE     DEN-F132       TO   ME-18.
     MOVE     DEN-F134       TO   ME-19.
     IF       DEN-F133  =    9
              MOVE      NC"請求"  TO   ME-20
     ELSE
              MOVE      SPACE     TO   ME-20
     END-IF.
*
     WRITE    PRT-REC        FROM MEIS01    AFTER     2.
     WRITE    PRT-REC        FROM P-SPACE   AFTER     1.
     ADD      3              TO   LINE-CNT.
 2411-MEIS01-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2412-MEIS02-PRINT      SECTION.
     MOVE    "2412-MEIS02-PRINT"  TO   S-NAME.
     IF       LINE-CNT  >    63
              PERFORM   2415-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS02.
     MOVE     DEN-F03        TO   ME-21.
     IF       DEN-F25   NOT  =    SPACE
              MOVE      DEN-F25   TO   ME-22
     ELSE
              MOVE      DEN-F141  TO   ME-22
     END-IF.
     MOVE     DEN-F142       TO   ME-23.
     MOVE     DEN-F50        TO   ME-241.
     MOVE     DEN-F16        TO   ME-25.
     MOVE     DEN-F512       TO   ME-26.
     MOVE     DEN-F521       TO   ME-27.
     MOVE     DEN-F513       TO   ME-28.
     MOVE     DEN-F522       TO   ME-29.
*
     MOVE     "("            TO   ME-242.
***  訂正時のみ出力
     IF       DEN-F15  NOT = DEN-F50
              MOVE     DEN-F15        TO   ME-243
     END-IF.
     MOVE      ")"           TO   ME-244.
*
     IF       LINE-CNT  <    7
              WRITE     PRT-REC   FROM P-SPACE   AFTER     1
              ADD       1         TO   LINE-CNT
     END-IF.
     WRITE    PRT-REC        FROM MEIS02    AFTER     1.
     ADD      1              TO   LINE-CNT.
 2412-MEIS02-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2413-MEIS03-PRINT      SECTION.
     MOVE    "2413-MEIS03-PRINT"  TO   S-NAME.
     IF       LINE-CNT  >    62
              PERFORM   2415-HEAD-PRINT
     END-IF.
*
     IF       WK-32     NOT  =    SPACE
              MOVE      WK-32          TO   ME-32
              MOVE      NC"備考（"     TO   ME-32L
              MOVE      NC"）"         TO   ME-32R
     ELSE
              MOVE      SPACE          TO   ME-32
              MOVE      SPACE          TO   ME-32L
              MOVE      SPACE          TO   ME-32R
     END-IF.
     MOVE     WK-33          TO   ME-33.
     MOVE     WK-34          TO   ME-34.
     MOVE     WK-35          TO   ME-35.
*
     IF       LINE-CNT  <    7
              WRITE     PRT-REC   FROM P-SPACE   AFTER     1
              ADD       1         TO   LINE-CNT
     END-IF.
     WRITE    PRT-REC        FROM MEIS03    AFTER     2.
     WRITE    PRT-REC        FROM P-LINE2   AFTER     1.
     ADD      3              TO   LINE-CNT.
     INITIALIZE              GOKEI-AREA.
*倉庫コードブレイク判定
     IF       NEW       NOT =     HIGH-VALUE
          IF       DEN-F48   NOT =     WK-SOKOCD
              PERFORM   2415-HEAD-PRINT
              MOVE      DEN-F48   TO   WK-SOKOCD
          END-IF
     END-IF.
 2413-MEIS03-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ｼｭｳｹｲ                                        *
*--------------------------------------------------------------*
 2414-SYUKEI            SECTION.
     MOVE    "2414-SYUKEI"   TO   S-NAME.
     IF       DEN-F03   =    80
              MOVE      DEN-F142       TO   WK-32
     END-IF.
     ADD      DEN-F50        TO   WK-33.
     ADD      DEN-F521       TO   WK-34.
     ADD      DEN-F522       TO   WK-35.
 2414-SYUKEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 2415-HEAD-PRINT        SECTION.
     MOVE    "2415-HEAD-PRINT"    TO   S-NAME.
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*
     ADD      1                   TO   PAGE-CNT.
     MOVE     SYS-YYW             TO   HD-011.
     MOVE     SYS-MMW             TO   HD-012.
     MOVE     SYS-DDW             TO   HD-013.
     MOVE     PAGE-CNT            TO   HD-02.
*
     MOVE     DEN-F48             TO   HD-031  SOK-F01.
     READ     ZSOKMS
       INVALID
              MOVE      SPACE     TO   HD-032
       NOT INVALID
              MOVE      SOK-F02   TO   HD-032
     END-READ.
     MOVE     DEN-F46             TO   HD-041.
     MOVE     DEN-F47             TO   HD-042.
     MOVE     DEN-F01             TO   HD-043.
     MOVE     SNOYMD              TO   HD-044.
     MOVE     ENOYMD              TO   HD-045.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     2.
     WRITE    PRT-REC   FROM      P-LINE1   AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     WRITE    PRT-REC   FROM      P-LINE1   AFTER     1.
     MOVE     9              TO   LINE-CNT.
 2415-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "900-DSP-READ"      TO   S-NAME.
     MOVE     "SCREEN"       TO   DSP-GRP.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   GUIDE
     ELSE
              MOVE      GUIDE02   TO   GUIDE
     END-IF.
     MOVE     WK-SYSYMD           TO   SDATE.
     MOVE     WK-SYSTIME          TO   STIME.
*****ACCEPT   SYS-TIME2      FROM TIME.
*****MOVE     SYS-TIMEW           TO   SYSTIM.
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
     WRITE    FKE00801.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-DEN-START-READ     SECTION.
     MOVE     "900-DEN-START-READ"     TO   S-NAME.
     START    SHTDENF   KEY  >=   DEN-F46
                                  DEN-F47
                                  DEN-F01
                                  DEN-F48
                                  DEN-F02
              INVALID   KEY
                        MOVE HIGH-VALUE     TO   NEW
     END-START.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   900-DEN-READ
     END-IF.
 900-DEN-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 READ                          *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     MOVE     "900-DEN-READ"      TO   S-NAME.
*
     READ     SHTDENF   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-DEN-READ-EXIT
     END-READ.
*
     IF       BTDATE     IS   NUMERIC
     AND      DEN-F46   >    BTDATE
              MOVE      HIGH-VALUE     TO   NEW
     END-IF.
     IF       BTTIME     IS   NUMERIC
     AND      DEN-F47   >    BTTIME
              MOVE      HIGH-VALUE     TO   NEW
     END-IF.
     IF       BTTORI     IS   NUMERIC
     AND      DEN-F01   >    BTTORI
              MOVE      HIGH-VALUE     TO   NEW
     END-IF.
*****IF       SOKCD2     IS   NUMERIC
*****AND      DEN-F48   >    SOKCD2
     IF       DEN-F48   >    SOKCD2
              MOVE      HIGH-VALUE     TO   NEW
     END-IF.
     IF       SOKCD2    IS   NUMERIC
     AND      DEN-F48   =    SOKCD2
     AND      DENNO2    IS   NUMERIC
     AND      DEN-F02   >    DENNO2
              MOVE      HIGH-VALUE     TO   NEW
     END-IF.
*
     MOVE     0         TO   SKIP-FLG.
*
     IF       DENNO1    IS   NUMERIC
     AND      DEN-F02   <    DENNO1
              MOVE      1         TO   SKIP-FLG
     END-IF.
*
     IF       DENNO2    IS   NUMERIC
     AND      DEN-F02   >    DENNO2
              MOVE      1         TO   SKIP-FLG
     END-IF.
*
     IF       DEN-F274  NOT  =    1
              MOVE      1         TO   SKIP-FLG
     END-IF.
*
     IF       DEN-F277  NOT  =    0
              MOVE      1         TO   SKIP-FLG
     END-IF.
*
*    納品日範囲のみ抽出
     IF     ( DEN-F112  <    SNOYMD )  OR
            ( DEN-F112  >    ENOYMD )
              GO   TO   900-DEN-READ
     END-IF.
*    店舗ＣＤ範囲のみ抽出
     IF     ( DEN-F07  <    STENCD )  OR
            ( DEN-F07  >    ETENCD )
              GO   TO   900-DEN-READ
     END-IF.
*
     IF       NEW       NOT  =    HIGH-VALUE
     AND      SKIP-FLG       =    1
              GO   TO   900-DEN-READ
     END-IF.
*
     IF       NEW       NOT  =    HIGH-VALUE
              MOVE      DEN-F46        TO   NEW-DAT
              MOVE      DEN-F47        TO   NEW-TIM
              MOVE      DEN-F01        TO   NEW-TOR
              MOVE      DEN-F48        TO   NEW-SOK
              MOVE      DEN-F02        TO   NEW-DEN
     END-IF.
 900-DEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     MOVE     "900-TOK-READ"      TO   S-NAME.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     MOVE     "900-TEN-READ"      TO   S-NAME.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
