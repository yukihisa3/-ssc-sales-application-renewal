# SSY8763L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY8763L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　オンライン　　　*
*    モジュール名　　　　：　出荷リスト出力                    *
*    作成日／更新日　　　：　07/06/01                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　ＤＣＭ出荷確定データを読み、出荷　*
*                            リストを発行する。（ダイキ用）    *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY8763L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/06/01.
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
         YB-22     IS   BAIKAKU-2-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< ＤＣＭ出荷確定データ >>--*
     SELECT   DJSYUKF   ASSIGN         DA-01-VI-DJSYUKL8
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  KMK-K01
                                       KMK-K02
                                       KMK-K03
                                       KMK-K04
                                       KMK-F09
                                       KMK-F05
                                       KMK-K08
                                       KMK-F21
                                       KMK-F304
                                       KMK-F264
                                       KMK-F03
                                       KMK-M03
                        STATUS         DJSYUKF-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04-PRTF.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< ＤＣＭ出荷確定データ >>--*
 FD  DJSYUKF            LABEL     RECORD   IS   STANDARD.
     COPY     DJSYUKF   OF        XFDLIB
              JOINING   KMK       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03)   VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)   VALUE  ZERO.
     03  PAGE-CNT2      PIC  9(03)   VALUE  ZERO.
     03  READ-CNT       PIC  9(08)   VALUE  ZERO.
 01  FLGS.
     03  END-FLG        PIC  X(03)   VALUE  SPACE.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  DJSYUKF-ST        PIC  X(02).
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
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*
 01  WK-SURYO           PIC  9(05)V9   VALUE  ZERO.
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  WK-GK-GENKA    PIC  9(09)V99.
     03  WK-GK-BAIKA    PIC  9(09).
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  WK-HIZUKE-HENSYU.
     03  WK-H-YYYY      PIC  9(04).
     03  WK-H-KU1       PIC  X(01).
     03  WK-H-MM        PIC  9(02).
     03  WK-H-KU2       PIC  X(01).
     03  WK-H-DD        PIC  9(02).
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  WK-KMK-K01     PIC  9(08).
     03  WK-KMK-K02     PIC  9(04).
     03  WK-KMK-K03     PIC  9(08).
     03  WK-KMK-K04     PIC  X(02).
     03  WK-KMK-F09     PIC  9(02).
     03  WK-KMK-F05     PIC  9(08).
     03  WK-KMK-K08     PIC  9(08).
     03  WK-KMK-F21     PIC  9(04).
     03  WK-KMK-F264    PIC  X(02).
     03  WK-KMK-F03     PIC  9(09).
     03  WK-KMK-M03     PIC  9(03).
     03  WK-KMK-F304    PIC  X(06).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD00.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(114)    VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"日付".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD00-SDATE PIC  X(10).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD00-PCNT  PIC  ZZ9.
 01  HD001.
     03  FILLER         PIC  X(104)    VALUE  SPACE.
     03  FILLER         PIC  X(31)     VALUE
        "-------------------------------".
 01  HD002.
     03  FILLER         PIC  X(104)    VALUE  SPACE.
     03  FILLER         PIC  X(31)     VALUE
        "!               !             !".
 01  HD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(07)     VALUE
             NC"ダイキ株式会社".
         05  FILLER     PIC  X(32)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE BAIKAKU-2-5.
         05  FILLER     PIC  N(05)     VALUE
             NC"出荷リスト".
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(32)     VALUE  SPACE.
         05  FILLER     PIC  X(04)     VALUE  "!   ".
         05  FILLER     PIC  N(02)     VALUE
             NC"検品".
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  FILLER     PIC  N(03)     VALUE
             NC"検品者".
         05  FILLER     PIC  X(09)     VALUE  "   !     ".
         05  FILLER     PIC  N(03)     VALUE
             NC"検収印".
         05  FILLER     PIC  X(05)     VALUE  "    !".
 01  HD011.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(50)     VALUE  SPACE.
         05  FILLER     PIC  X(01)     VALUE  "(".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD011-1    PIC  X(02).
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD011-2    PIC  N(10).
         05  FILLER     PIC  X(01)     VALUE  ")".
 01  HD02.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  X(10)     VALUE  "ﾍﾞﾝﾀﾞｰｺｰﾄﾞ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  X(06)     VALUE  "ﾍﾞﾝﾀﾞｰ".
         05  FILLER     PIC  N(02)     VALUE  NC"名称".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"店".
         05  FILLER     PIC  X(04)     VALUE  "ｺｰﾄﾞ".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗名".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"館名".
         05  FILLER     PIC  X(59)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"発注日".
         05  FILLER     PIC  X(10)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"出荷日".
 01  HD03.
     03  FILLER.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD03-TOKCD PIC  999999.
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  HD03-JISYA PIC  X(10).
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD03-TENCD PIC  9999.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD03-TENNM PIC  X(10).
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD03-KANNM PIC  X(10).
         05  FILLER     PIC  X(49)     VALUE  SPACE.
         05  HD03-HACD  PIC  X(10).
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  HD03-NOUD  PIC  X(10).
 01  HD04.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"納品番号".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"部門".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票区分".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"備考".
         05  FILLER     PIC  X(97)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"発注伝票番号".
 01  HD05.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"行".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"商品".
         05  FILLER     PIC  X(04)     VALUE  "ｺｰﾄﾞ".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(07)     VALUE  "JANｺｰﾄﾞ".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"品名".
         05  FILLER     PIC  X(18)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"規格".
         05  FILLER     PIC  X(18)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"特売".
         05  FILLER     PIC  X(04)     VALUE  "ｺｰﾄﾞ".
         05  FILLER     PIC  N(03)     VALUE  NC"申込表".
         05  FILLER     PIC  X(03)     VALUE  "NO.".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"出荷数".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"引合".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"原価".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"売価".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価金額".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価金額".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"行".
 01  SEN                         CHARACTER  TYPE  MODE-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
*
 01  MS01.
     03  FILLER                  CHARACTER  TYPE  MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-DENNO PIC  999999999.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-BUMON PIC  999.
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  MS01-DENK  PIC  99.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"備考：".
         05  MS01-BIKO  PIC  X(20).
         05  FILLER     PIC  X(75)     VALUE  SPACE.
         05  MS01-DENNO2 PIC 999999999.
 01  MS02.
     03  FILLER.
         05  MS02-GYO   PIC  999.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-SYOCD PIC  X(08).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-JANCD PIC  X(13).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-SYONM1 PIC X(20).
         05  MS02-SYONM2 PIC X(20).
         05  MS02-TOK   PIC  X(06).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS02-MOS   PIC  X(06).
         05  MS02-SURYO PIC  ---,--9.9.
         05  FILLER     PIC  X(05)     VALUE  "(   )".
         05  MS02-GENTAN PIC  ---,--9.99.
         05  MS02-BAITAN PIC  ---,--9.
         05  MS02-GENKIN PIC  -,---,--9.99.
         05  MS02-BAIKIN PIC  -,---,--9.
         05  FILLER      PIC  X(01)     VALUE  SPACE.
         05  MS02-GYO2   PIC  999.
 01  MS03.
     03  FILLER.
         05  FILLER     PIC  X(111)    VALUE  SPACE.
         05  MS03-GENGOK PIC  -,---,--9.99.
         05  MS03-BAIGOK PIC  -,---,--9.
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
 LINKAGE     SECTION.
 01  LINK-JDATE             PIC  9(08).
 01  LINK-JTIME             PIC  9(04).
 01  LINK-TORICD            PIC  9(08).
 01  LINK-SOKO              PIC  X(02).
 01  LINK-STENCD            PIC  9(05).
 01  LINK-ETENCD            PIC  9(05).
 01  LINK-SNOUDT            PIC  9(08).
 01  LINK-ENOUDT            PIC  9(08).
****************************************************************
 PROCEDURE              DIVISION  USING  LINK-JDATE
                                         LINK-JTIME
                                         LINK-TORICD
                                         LINK-SOKO
                                         LINK-STENCD
                                         LINK-ETENCD
                                         LINK-SNOUDT
                                         LINK-ENOUDT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< カーマ出荷確定データ >>--*
 DJSYUKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DJSYUKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY8763L DJSYUKF ERROR " DJSYUKF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    DJSYUKF.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG = "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
     ACCEPT   SYS-TIME       FROM TIME.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD   TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     DISPLAY  "*** SSY8763L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     DJSYUKF.
     OPEN     OUTPUT    PRTF.
*
     INITIALIZE  BREAK-KEY.
*カーマ出荷確定データＲＥＡＤ
     MOVE     SPACE               TO   KMK-REC.
     INITIALIZE                        KMK-REC.
     MOVE     LINK-JDATE          TO   KMK-K01.
     MOVE     LINK-JTIME          TO   KMK-K02.
     MOVE     LINK-TORICD         TO   KMK-K03.
     MOVE     LINK-SOKO           TO   KMK-K04.
     MOVE     LINK-STENCD         TO   KMK-K05.
     DISPLAY  "LINK-JDATE = " LINK-JDATE UPON CONS.
     DISPLAY  "LINK-JTIME = " LINK-JTIME UPON CONS.
     DISPLAY  "LINK-TORICD= " LINK-TORICD UPON CONS.
     DISPLAY  "LINK-SOKO  = " LINK-SOKO  UPON CONS.
     DISPLAY  "LINK-STENCD= " LINK-STENCD UPON CONS.
     PERFORM  900-KMK-START-READ.
*
     IF       END-FLG  =  "END"
              DISPLAY NC"＃＃出力対象無し＃＃" UPON CONS
              STOP  RUN
     ELSE
              MOVE     KMK-K01    TO   WK-KMK-K01
              MOVE     KMK-K02    TO   WK-KMK-K02
              MOVE     KMK-K03    TO   WK-KMK-K03
              MOVE     KMK-K04    TO   WK-KMK-K04
              MOVE     KMK-F09    TO   WK-KMK-F09
              MOVE     KMK-F05    TO   WK-KMK-F05
              MOVE     KMK-K08    TO   WK-KMK-K08
              MOVE     KMK-F21    TO   WK-KMK-F21
              MOVE     KMK-F264   TO   WK-KMK-F264
              MOVE     KMK-F304   TO  WK-KMK-F304
              MOVE     99         TO   LINE-CNT
              MOVE     ZERO       TO   PAGE-CNT
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*    発注伝票区分／発注日／出荷日／店ＣＤ／館ＣＤブレイク時
*    改ページ
     IF       KMK-F09   NOT =  WK-KMK-F09
     OR       KMK-F05   NOT =  WK-KMK-F05
     OR       KMK-K08   NOT =  WK-KMK-K08
     OR       KMK-F21   NOT =  WK-KMK-F21
     OR       KMK-F264  NOT =  WK-KMK-F264
     OR       KMK-F304  NOT =  WK-KMK-F304
              PERFORM   GOKEI-WT-SEC
              MOVE      ZERO      TO  PAGE-CNT2
              PERFORM   HEAD-WT-SEC
              PERFORM   MEISAI-HEAD-SEC
              MOVE      KMK-F09   TO  WK-KMK-F09
              MOVE      KMK-F05   TO  WK-KMK-F05
              MOVE      KMK-K08   TO  WK-KMK-K08
              MOVE      KMK-F21   TO  WK-KMK-F21
              MOVE      KMK-F264  TO  WK-KMK-F264
              MOVE      KMK-F03   TO  WK-KMK-F03
              MOVE      KMK-F304  TO  WK-KMK-F304
              MOVE      ZERO      TO  WK-GK-GENKA WK-GK-BAIKA
     END-IF.
*    伝票番号がブレイク
     IF       KMK-F03   NOT =  WK-KMK-F03
              IF  PAGE-CNT > ZERO
                  PERFORM  GOKEI-WT-SEC
              END-IF
              PERFORM   MEISAI-HEAD-SEC
              MOVE      KMK-F03   TO  WK-KMK-F03
              MOVE      ZERO      TO  WK-GK-GENKA WK-GK-BAIKA
     END-IF.
*    明細行セット
     PERFORM  MEISAI-BODY-SEC.
*    カーマ出荷確定データ読込み
     PERFORM  900-KMK-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     DISPLAY "ﾀｲｼｮｳDT CNT = " READ-CNT  UPON CONS.
     DISPLAY NC"＃総出力枚数⇒" " " PAGE-CNT NC"枚" UPON CONS.
*
     IF  READ-CNT  >  ZERO
         PERFORM  GOKEI-WT-SEC
     END-IF.
*
     CLOSE    DJSYUKF  PRTF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY8763L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ヘッダ印字
*--------------------------------------------------------------*
 HEAD-WT-SEC            SECTION.
     MOVE    "HEAD-WT-SEC"        TO   S-NAME.
*    改頁判定
     IF       PAGE-CNT  >   ZERO
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
     END-IF.
*    行カウンター初期化
     MOVE     ZERO      TO        LINE-CNT.
*    頁カウンター
     ADD      1         TO        PAGE-CNT  PAGE-CNT2.
     MOVE     PAGE-CNT2 TO        HD00-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4) TO   WK-H-YYYY.
     MOVE     SYS-DATEW(5:2) TO   WK-H-MM.
     MOVE     SYS-DATEW(7:2) TO   WK-H-DD.
     MOVE     "/"            TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-HIZUKE-HENSYU TO HD00-SDATE.
*    発注種別区分
     MOVE     KMK-F09        TO   HD011-1.
     EVALUATE KMK-F09
         WHEN 0
              MOVE NC"　定　期　　　　　　" TO HD011-2
         WHEN 1
              MOVE NC"　特　売　　　　　　" TO HD011-2
         WHEN 2
              MOVE NC"　新店改装　　　　　" TO HD011-2
         WHEN 3
              MOVE NC"　本部発注（投入）　" TO HD011-2
         WHEN 4
              MOVE NC"　ダイレクト　　　　" TO HD011-2
         WHEN 5
              MOVE NC"　客注ダイレクト　　" TO HD011-2
         WHEN 6
              MOVE NC"　新規初回　　　　　" TO HD011-2
         WHEN 51
              MOVE NC"　本部在庫補充　　　" TO HD011-2
         WHEN 52
              MOVE NC"　商管在庫補充　　　" TO HD011-2
         WHEN 71
              MOVE NC"　備品（用度品）　　" TO HD011-2
         WHEN 81
              MOVE NC"　プロモーション　　" TO HD011-2
         WHEN 82
              MOVE NC"　改　廃　　　　　　" TO HD011-2
         WHEN 93
              MOVE NC"　ＢＹ改廃　　　　　" TO HD011-2
     END-EVALUATE.
*    ベンダーＣＤ
     MOVE     KMK-F304       TO   HD03-TOKCD.
*    ベンダー名称
     MOVE     KMK-F29        TO   HD03-JISYA.
*    店ＣＤ
     MOVE     KMK-F21        TO   HD03-TENCD.
*    店舗名称
     MOVE     KMK-F22        TO   HD03-TENNM.
*    館名称
     EVALUATE KMK-F264
         WHEN "01"
              MOVE "ﾎｰﾑｾﾝﾀｰ"        TO  HD03-KANNM
         WHEN "02"
              MOVE "ｼｻﾞｲｾﾝﾀｰ"       TO  HD03-KANNM
         WHEN "03"
              MOVE "ﾎｰﾑｲﾝﾃﾘｱｾﾝﾀｰ"   TO  HD03-KANNM
         WHEN "04"
              MOVE "ｶﾞｰﾃﾞﾝｾﾝﾀｰ"     TO  HD03-KANNM
         WHEN "05"
              MOVE "ﾍﾟｯﾄ&ｶﾞｰﾃﾞﾝ"    TO  HD03-KANNM
         WHEN "06"
              MOVE "ｼｻﾞｲ&ｶﾞｰﾃﾞﾝ"    TO  HD03-KANNM
         WHEN "07"
              MOVE "ﾍﾟｯﾄ"           TO  HD03-KANNM
     END-EVALUATE.
*    発注日
     MOVE     KMK-F05(1:4)   TO   WK-H-YYYY.
     MOVE     KMK-F05(5:2)   TO   WK-H-MM.
     MOVE     KMK-F05(7:2)   TO   WK-H-DD.
     MOVE     "/"            TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-HIZUKE-HENSYU TO HD03-HACD.
*    出荷日
     MOVE     KMK-K08(1:4)   TO   WK-H-YYYY.
     MOVE     KMK-K08(5:2)   TO   WK-H-MM.
     MOVE     KMK-K08(7:2)   TO   WK-H-DD.
     MOVE     "/"            TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-HIZUKE-HENSYU TO HD03-NOUD.
*    ヘッダ印刷
     WRITE    PRT-REC  FROM  HD00  AFTER  0.
     WRITE    PRT-REC  FROM  HD001 AFTER  1.
     WRITE    PRT-REC  FROM  HD01  AFTER  1.
     WRITE    PRT-REC  FROM  HD001 AFTER  1.
     WRITE    PRT-REC  FROM  HD002 AFTER  1.
     WRITE    PRT-REC  FROM  HD002 AFTER  1.
     WRITE    PRT-REC  FROM  HD011 AFTER  0.
     WRITE    PRT-REC  FROM  HD001 AFTER  1.
     WRITE    PRT-REC  FROM  HD02  AFTER  1.
     WRITE    PRT-REC  FROM  HD03  AFTER  1.
     WRITE    PRT-REC  FROM  HD04  AFTER  2.
     WRITE    PRT-REC  FROM  HD05  AFTER  1.
     WRITE    PRT-REC  FROM  SEN1  AFTER  1.
*行カウント
     MOVE     12             TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細ヘッダー印字
*--------------------------------------------------------------*
 MEISAI-HEAD-SEC       SECTION.
     MOVE     "MEISAI-HEAD-SEC"    TO   S-NAME.
*    ヘッダ出力判定
     IF     LINE-CNT  >  50
            PERFORM   HEAD-WT-SEC
     END-IF.
*    納品番号（伝票番号）
     MOVE      KMK-F03     TO   MS01-DENNO.
*    部門
     MOVE      KMK-F14     TO   MS01-BUMON.
*    伝区
     MOVE      KMK-F081    TO   MS01-DENK.
*    備考
     MOVE      SPACE       TO   MS01-BIKO.
*    発注伝票番号
     MOVE      KMK-F03     TO   MS01-DENNO2.
*
     WRITE     PRT-REC   FROM  MS01  AFTER  1.
*
     ADD       1           TO   LINE-CNT.
*
 MEISAI-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細ボディー印字
*--------------------------------------------------------------*
 MEISAI-BODY-SEC        SECTION.
     MOVE     "MEISAI-BODY-SEC"   TO   S-NAME.
*    ヘッダ出力判定
     IF     LINE-CNT  >  50
            PERFORM   HEAD-WT-SEC
     END-IF.
*    行
     MOVE   KMK-M03               TO   MS02-GYO.
*    商品ＣＤ
     MOVE   KMK-M04               TO   MS02-SYOCD.
*    ＪＡＮＣＤ
     MOVE   KMK-M02               TO   MS02-JANCD.
*    品名
     MOVE   KMK-M07               TO   MS02-SYONM1.
*    規格
     MOVE   KMK-M084              TO   MS02-SYONM2.
*    特売ＣＤ
     MOVE   KMK-F061              TO   MS02-TOK.
*    申込票
     MOVE   KMK-F311              TO   MS02-MOS.
*    出荷数
     MOVE   KMK-A36               TO   MS02-SURYO WK-SURYO.
*****IF     KMK-F15  >  ZERO
************COMPUTE  MS02-SURYO  =  KMK-F15  /  10
************COMPUTE  WK-SURYO    =  KMK-F15  /  10
*****ELSE
************MOVE     ZERO         TO   MS02-SURYO  WK-SURYO
*****END-IF.
*    引合
*    原価
*****COMPUTE  MS02-GENTAN  =  KMK-F10C  /  100.
     COMPUTE  MS02-GENTAN  =  KMK-M16  /  100.
*    売価
*****MOVE   KMK-F10D              TO   MS02-BAITAN.
     MOVE   KMK-M17               TO   MS02-BAITAN.
*    原価金額
     COMPUTE  MS02-GENKIN  =  ( WK-SURYO * KMK-M16  )  /  100.
     COMPUTE  WK-GK-GENKA  =  WK-GK-GENKA +
                              ( WK-SURYO * KMK-M16  )  /  100.
*    売価金額
     COMPUTE  MS02-BAIKIN  =  ( WK-SURYO * KMK-M17  ).
     COMPUTE  WK-GK-BAIKA  =  WK-GK-BAIKA +
                              ( WK-SURYO * KMK-M17  ).
*    発注伝票行番号
     MOVE     KMK-M03         TO    MS02-GYO2.
*    〇以上の場合に出力対象
*----< 2007/06/07 修正開始 >----
**   ０以上じゃなくても出力するよう修正
*****IF   WK-SURYO  >  ZERO
**********WRITE     PRT-REC   FROM  MS02  AFTER  1
**********ADD       1           TO   LINE-CNT
*****END-IF.
     WRITE     PRT-REC   FROM  MS02  AFTER  1.
     ADD       1           TO   LINE-CNT.
*----< 2007/06/07 修正終了 >----
*
 MEISAI-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細合計印字
*--------------------------------------------------------------*
 GOKEI-WT-SEC                 SECTION.
     MOVE     "GOKEI-WT-SEC" TO   S-NAME.
*    原価金額合計
     MOVE     WK-GK-GENKA       TO    MS03-GENGOK.
*    売価金額合計
     MOVE     WK-GK-BAIKA       TO    MS03-BAIGOK.
*
     WRITE     PRT-REC   FROM  MS03  AFTER  1.
     WRITE     PRT-REC   FROM  SEN1  AFTER  1.
*
     MOVE     ZERO              TO    WK-GK-GENKA WK-GK-BAIKA.
*
     ADD       2           TO   LINE-CNT.
*
 GOKEI-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-KMK-START-READ     SECTION.
     MOVE     "900-KMK-START-READ"     TO   S-NAME.
     START    DJSYUKF   KEY  >=   KMK-K01  KMK-K02
                                  KMK-K03  KMK-K04
                                  KMK-F09  KMK-F05
                                  KMK-K08  KMK-F21
                                  KMK-F304 KMK-F264
                                  KMK-F03  KMK-M03
              INVALID   KEY
**************DISPLAY "AAA" UPON CONS
              MOVE     "END"      TO   END-FLG
              GO                  TO   900-KMK-START-READ-EXIT
     END-START.
*
     PERFORM   900-KMK-READ.
*
 900-KMK-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    カーマ出荷確定ファイル　 READ                *
*--------------------------------------------------------------*
 900-KMK-READ           SECTION.
     MOVE     "900-KMK-READ"      TO   S-NAME.
*
     READ     DJSYUKF   AT   END
**************DISPLAY "DDD" UPON CONS
              MOVE     "END"      TO   END-FLG
              GO        TO        900-KMK-READ-EXIT
     END-READ.
*
*ブレイクチェック
     IF   LINK-SOKO = SPACE THEN
          IF       KMK-K01   NOT =  LINK-JDATE       OR
                   KMK-K02   NOT =  LINK-JTIME       OR
                   KMK-K03   NOT =  LINK-TORICD
**************DISPLAY "BBB" UPON CONS
                   MOVE     "END"      TO   END-FLG
                   GO        TO        900-KMK-READ-EXIT
          END-IF
     ELSE
          IF       KMK-K01   NOT =  LINK-JDATE       OR
                   KMK-K02   NOT =  LINK-JTIME       OR
                   KMK-K03   NOT =  LINK-TORICD      OR
                   KMK-K04   NOT =  LINK-SOKO
**************DISPLAY "CCC" UPON CONS
                   MOVE     "END"      TO   END-FLG
                   GO        TO        900-KMK-READ-EXIT
          END-IF
     END-IF.
*店舗ＣＤ範囲チェック
     IF       LINK-STENCD  <=  KMK-K05
     AND      LINK-ETENCD  >=  KMK-K05
              CONTINUE
     ELSE
**************DISPLAY "EEE" UPON CONS
              GO        TO        900-KMK-READ
     END-IF.
*
*納品日範囲チェック
     IF       LINK-SNOUDT  <=  KMK-K08
     AND      LINK-ENOUDT  >=  KMK-K08
              CONTINUE
     ELSE
**************DISPLAY "FFF" UPON CONS
              GO        TO        900-KMK-READ
     END-IF.
*
     ADD      1         TO        READ-CNT.
*
 900-KMK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
