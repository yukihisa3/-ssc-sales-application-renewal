# SSY7825L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY7825L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ロイヤルHC 新EDIシステム
*    業務名　　　　　　　：　ロイヤルHC 新EDIシステム
*    モジュール名　　　　：　ロイヤルHC納品伝票発行
*    作成日／更新日　　　：　08/10/31                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY7825L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/10/31.
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
*----<< ロイヤルHC出荷情報ファイル>>--*
     SELECT   RHSYUKF   ASSIGN         DA-01-VI-RHSYUKL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  RHC-F01   RHC-F02
                                       RHC-F03   RHC-F04
                                       RHC-B07   RHC-F08
                                       RHC-F05   RHC-B03
                                       RHC-B04   RHC-B10
                                       RHC-F06   RHC-F07
                        STATUS         RHSYUKF-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04-PRTF.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 出荷データファイル>>--*
 FD  RHSYUKF            LABEL     RECORD   IS   STANDARD.
     COPY     RHSYUKF   OF        XFDLIB
              JOINING   RHC       PREFIX.
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
 01  RHSYUKF-ST         PIC  X(02).
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
 01  WK-GENKA           PIC  9(09)V9   VALUE  ZERO.
 01  WK-KEPPIN          PIC  9(05)     VALUE  ZERO.
 01  WK-DENNO           PIC  9(09)     VALUE  ZERO.
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  WK-GK-GENKA    PIC  9(11)     VALUE  ZERO.
     03  WK-GK-BAIKA    PIC  9(11)     VALUE  ZERO.
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
     03  WK-RHC-F01     PIC  9(08).
     03  WK-RHC-F02     PIC  9(04).
     03  WK-RHC-F03     PIC  9(08).
     03  WK-RHC-F04     PIC  X(02).
     03  WK-RHC-B07     PIC  9(08).
     03  WK-RHC-F08     PIC  9(08).
     03  WK-RHC-F05     PIC  9(05).
     03  WK-RHC-B03     PIC  9(03).
     03  WK-RHC-B04     PIC  X(02).
     03  WK-RHC-B10     PIC  9(01).
     03  WK-RHC-F06     PIC  9(09).
     03  WK-RHC-F07     PIC  9(02).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD000.
     03  FILLER         CHARACTER TYPE BAIKAKU.
         05  FILLER     PIC  X(56)     VALUE  SPACE.
         05  FILLER     PIC  N(07)     VALUE
                                           NC"＜納品明細書＞".
 01  HD00.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(20)     VALUE
                         NC"ロイヤルホームセンター".
         05  FILLER     PIC  X(68)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"作成日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD00-YYYY  PIC  9999.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD00-MM    PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD00-DD    PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  X(05)     VALUE  "PAGE:".
         05  HD00-PAGE  PIC  ZZ9.
 01  HD01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"発注日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD02-HACYY PIC  9999.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD02-HACMM PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD02-HACDD PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"店舗".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD02-TENCD PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD02-TENME PIC  X(28).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票区分".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD02-DENKB PIC  99.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD02-DENME PIC  N(04).
         05  FILLER     PIC  X(26)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD02-TORCD PIC  999999.
 01  HD02.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"納品日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD03-NOHYY PIC  9999.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD03-NOHMM PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD03-NOHDD PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"部門".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD03-BUMCD PIC  99.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(29)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"ルート".
***********位置あわせ用空白
         05  FILLER     PIC  N(01)     VALUE  NC"　".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD03-RUTO  PIC  99.
         05  FILLER     PIC  X(33)     VALUE  SPACE.
         05  HD03-TORME PIC  N(20).
 01  HD03.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(104)    VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(03)     VALUE  "TEL".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD04-TEL   PIC  X(13).
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  HD04-SOKCD PIC  X(02).
 01  HD04.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票番号".
 01  HD05.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"行".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(07)     VALUE  "JANｺｰﾄﾞ".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"商品名".
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  FILLER     PIC  N(03)     VALUE  NC"規格名".
         05  FILLER     PIC  X(42)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"発注数量".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"納品数量".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"訂正".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"原単価".
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価金額".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"売単価".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価金額".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
 01  SEN                         CHARACTER  TYPE  MODE-2.
     03  FILLER                  PIC  X(100) VALUE  SPACE.
     03  FILLER                  PIC  N(08)  VALUE
         NC"───────".
     03  FILLER                  PIC  X(12)  VALUE  SPACE.
     03  FILLER                  PIC  N(08)  VALUE
         NC"───────".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
 01  SEN2.
     03  FILLER                  PIC  X(50)  VALUE
         "==================================================".
     03  FILLER                  PIC  X(50)  VALUE
         "==================================================".
     03  FILLER                  PIC  X(36)  VALUE
         "====================================".
*
 01  MS01.
     03  FILLER.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-DENNO PIC  ZZZZZZZ9.
 01  MS02.
     03  FILLER.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-GYONO PIC  99.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-JANCD PIC  X(13)     VALUE  SPACE.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-SHOME PIC  X(50).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-HACSU PIC  ZZ,ZZ9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-NOHSU PIC  ZZ,ZZ9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-TESEI PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-GENTAN PIC  Z,ZZZ,ZZ9.99.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-GENKIN PIC  ZZZ,ZZZ,ZZ9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-BAITAN PIC  Z,ZZZ,ZZ9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-BAIKIN PIC  ZZZ,ZZZ,ZZ9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-RIYUU PIC  X(01)     VALUE  SPACE.
 01  MS04.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(93)     VALUE  SPACE.
***********位置あわせ用空白
         05  FILLER     PIC  N(01)     VALUE  NC"　".
         05  FILLER     PIC  N(05)     VALUE  NC"原価金額計".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS03-GENGOK PIC  ZZZ,ZZZ,ZZ9.
***********位置あわせ用空白
         05  FILLER     PIC  N(01)     VALUE  NC"　".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"売価金額計".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS03-BAIGOK PIC  ZZZ,ZZZ,ZZ9.
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
 01  PARA-PRTCNT            PIC  9(03).
****************************************************************
 PROCEDURE              DIVISION  USING  LINK-JDATE
                                         LINK-JTIME
                                         LINK-TORICD
                                         LINK-SOKO
                                         LINK-STENCD
                                         LINK-ETENCD
                                         LINK-SNOUDT
                                         LINK-ENOUDT
                                         PARA-PRTCNT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ロイヤルHC出荷情報ファイル >>--*
 RHSYUKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RHSYUKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY7825L RHSYUKF ERROR " RHSYUKF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    RHSYUKF.
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
     DISPLAY  "*** SSY7825L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     RHSYUKF.
     OPEN     OUTPUT    PRTF.
*
     INITIALIZE  BREAK-KEY.
*ロイヤルHC出荷確定データＲＥＡＤ
     MOVE     SPACE               TO   RHC-REC.
     INITIALIZE                        RHC-REC.
     MOVE     LINK-JDATE          TO   RHC-F01.
     MOVE     LINK-JTIME          TO   RHC-F02.
     MOVE     LINK-TORICD         TO   RHC-F03.
     MOVE     LINK-SOKO           TO   RHC-F04.
     MOVE     LINK-STENCD         TO   RHC-F05.
     MOVE     LINK-SNOUDT         TO   RHC-F08.
     DISPLAY  "LINK-JDATE = " LINK-JDATE UPON CONS.
     DISPLAY  "LINK-JTIME = " LINK-JTIME UPON CONS.
     DISPLAY  "LINK-TORICD= " LINK-TORICD UPON CONS.
     DISPLAY  "LINK-SOKO  = " LINK-SOKO  UPON CONS.
     DISPLAY  "LINK-STENCD= " LINK-STENCD UPON CONS.
     DISPLAY  "LINK-SNOUDT= " LINK-SNOUDT UPON CONS.
     PERFORM  900-RHC-START-READ.
*
     IF       END-FLG  =  "END"
              DISPLAY NC"＃＃出力対象無し＃＃" UPON CONS
              STOP  RUN
     ELSE
              MOVE     RHC-F01    TO   WK-RHC-F01
              MOVE     RHC-F02    TO   WK-RHC-F02
              MOVE     RHC-F03    TO   WK-RHC-F03
              MOVE     RHC-F04    TO   WK-RHC-F04
              MOVE     RHC-B07    TO   WK-RHC-B07
              MOVE     RHC-F08    TO   WK-RHC-F08
              MOVE     RHC-F05    TO   WK-RHC-F05
              MOVE     RHC-B03    TO   WK-RHC-B03
              MOVE     RHC-B04    TO   WK-RHC-B04
              MOVE     RHC-B10    TO   WK-RHC-B10
              MOVE     RHC-F06    TO   WK-RHC-F06
              MOVE     RHC-F07    TO   WK-RHC-F07
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
*    バッチ日付／バッチ時刻／バッチ取引先／倉庫ＣＤ／発注日／
*    納品日／店舗ＣＤ／部門ＣＤ／伝票区分／ルート
*    ブレイク時改ページ
     IF       RHC-F01   NOT =  WK-RHC-F01
     OR       RHC-F02   NOT =  WK-RHC-F02
     OR       RHC-F03   NOT =  WK-RHC-F03
     OR       RHC-F04   NOT =  WK-RHC-F04
     OR       RHC-B07   NOT =  WK-RHC-B07
     OR       RHC-F08   NOT =  WK-RHC-F08
     OR       RHC-F05   NOT =  WK-RHC-F05
     OR       RHC-B03   NOT =  WK-RHC-B03
     OR       RHC-B04   NOT =  WK-RHC-B04
     OR       RHC-B10   NOT =  WK-RHC-B10
              PERFORM   GOKEI-WT-SEC
              MOVE      ZERO      TO  PAGE-CNT2
              PERFORM   HEAD-WT-SEC
              MOVE      RHC-F01   TO  WK-RHC-F01
              MOVE      RHC-F02   TO  WK-RHC-F02
              MOVE      RHC-F03   TO  WK-RHC-F03
              MOVE      RHC-F04   TO  WK-RHC-F04
              MOVE      RHC-B07   TO  WK-RHC-B07
              MOVE      RHC-F08   TO  WK-RHC-F08
              MOVE      RHC-F05   TO  WK-RHC-F05
              MOVE      RHC-B03   TO  WK-RHC-B03
              MOVE      RHC-B04   TO  WK-RHC-B04
              MOVE      RHC-B10   TO  WK-RHC-B10
              MOVE      RHC-F06   TO  WK-RHC-F06
              MOVE      RHC-F07   TO  WK-RHC-F07
              MOVE      ZERO      TO  WK-GK-GENKA WK-GK-BAIKA
     END-IF.
*    伝票番号がブレイク
     IF       RHC-F06   NOT =  WK-RHC-F06
              IF  PAGE-CNT > ZERO
                  PERFORM  GOKEI-WT-SEC
              END-IF
              MOVE      RHC-F01   TO  WK-RHC-F01
              MOVE      RHC-F02   TO  WK-RHC-F02
              MOVE      RHC-F03   TO  WK-RHC-F03
              MOVE      RHC-F04   TO  WK-RHC-F04
              MOVE      RHC-B07   TO  WK-RHC-B07
              MOVE      RHC-F08   TO  WK-RHC-F08
              MOVE      RHC-F05   TO  WK-RHC-F05
              MOVE      RHC-B03   TO  WK-RHC-B03
              MOVE      RHC-B04   TO  WK-RHC-B04
              MOVE      RHC-B10   TO  WK-RHC-B10
              MOVE      RHC-F06   TO  WK-RHC-F06
              MOVE      RHC-F07   TO  WK-RHC-F07
              MOVE      ZERO      TO  WK-GK-GENKA WK-GK-BAIKA
     END-IF.
*    明細行セット
     PERFORM  MEISAI-BODY-SEC.
*    ロイヤルHC出荷確定データ読込み
     PERFORM  900-RHC-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
****出力件数にパラメータを渡す。
*    MOVE     PAGE-CNT            TO   PARA-PRTCNT.
*
     DISPLAY "ﾀｲｼｮｳDT CNT = " READ-CNT  UPON CONS.
     DISPLAY NC"＃総出力枚数⇒" " " PAGE-CNT NC"枚" UPON CONS.
*
     IF  READ-CNT  >  ZERO
         PERFORM  GOKEI-WT-SEC
     END-IF.
*
     CLOSE    RHSYUKF  PRTF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY7825L END *** "
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
*    システム日付セット
     MOVE     SYS-DATEW(1:4) TO   WK-H-YYYY.
     MOVE     SYS-DATEW(5:2) TO   WK-H-MM.
     MOVE     SYS-DATEW(7:2) TO   WK-H-DD.
     MOVE     "/"            TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-H-YYYY      TO   HD00-YYYY.
     MOVE     WK-H-MM        TO   HD00-MM.
     MOVE     WK-H-DD        TO   HD00-DD.
     MOVE     PAGE-CNT       TO   HD00-PAGE.
*    発注日
     MOVE     RHC-B07(1:4)   TO   HD02-HACYY.
     MOVE     RHC-B07(5:2)   TO   HD02-HACMM.
     MOVE     RHC-B07(7:2)   TO   HD02-HACDD.
*    店ＣＤ
     MOVE     RHC-F05        TO   HD02-TENCD.
*    店舗名称
     MOVE     RHC-B09        TO   HD02-TENME.
*    伝票区分
     MOVE     RHC-B04        TO   HD02-DENKB.
     EVALUATE RHC-B04
         WHEN "01"
              MOVE NC"定番"  TO  HD02-DENME
         WHEN "02"
              MOVE NC"特売"  TO  HD02-DENME
     END-EVALUATE.
*    取引先
     MOVE     00051649       TO   HD02-TORCD.
*    納品日
     MOVE     RHC-F08(1:4)   TO   HD03-NOHYY.
     MOVE     RHC-F08(5:2)   TO   HD03-NOHMM.
     MOVE     RHC-F08(7:2)   TO   HD03-NOHDD.
*    部門　
     MOVE     RHC-B03        TO   HD03-BUMCD.
*ルート
     MOVE     RHC-B10        TO   HD03-RUTO.
*取引先名
     MOVE     NC"株式会社　サカタのタネ"
                             TO   HD03-TORME.
*取引先名ＴＥＬ
     MOVE     "045-945-8816" TO   HD04-TEL.
*出荷倉庫
     MOVE     RHC-F04        TO   HD04-SOKCD.
*    ヘッダ印刷
     WRITE    PRT-REC  FROM  HD000 AFTER  1.
     WRITE    PRT-REC  FROM  HD00  AFTER  1.
     WRITE    PRT-REC  FROM  HD01  AFTER  2.
     WRITE    PRT-REC  FROM  HD02  AFTER  1.
     WRITE    PRT-REC  FROM  HD03  AFTER  1.
     WRITE    PRT-REC  FROM  SEN2  AFTER  1.
     WRITE    PRT-REC  FROM  HD04  AFTER  1.
     WRITE    PRT-REC  FROM  HD05  AFTER  1.
     WRITE    PRT-REC  FROM  SEN2  AFTER  1.
*行カウント
     MOVE      11             TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細ボディー印字
*--------------------------------------------------------------*
 MEISAI-BODY-SEC        SECTION.
     MOVE     "MEISAI-BODY-SEC"   TO   S-NAME.
*    ヘッダ出力判定
     IF     LINE-CNT  >  53
            PERFORM   HEAD-WT-SEC
     END-IF.
*    伝票番号
     MOVE   RHC-F06               TO   MS01-DENNO.
*    行
     MOVE   RHC-F07               TO   MS01-GYONO.
*    ＪＡＮＣＤ
     MOVE   RHC-C05               TO   MS01-JANCD.
*    商品名
     MOVE   RHC-C23(1:50)         TO   MS01-SHOME.
*    発注数量
     MOVE   RHC-C09               TO   MS01-HACSU.
*    納品数量
     MOVE   RHC-C251              TO   MS01-NOHSU.
*    訂正数量
     MOVE   "(  )"                TO   MS01-TESEI.
*    原単価
     MOVE   RHC-C11               TO   MS01-GENTAN.
*    売単価
     MOVE   RHC-C13               TO   MS01-BAITAN.
*    原価金額
     MOVE   RHC-C15               TO   MS01-GENKIN.
*    売価金額
     MOVE   RHC-C17               TO   MS01-BAIKIN.
*
     ADD    RHC-C15         TO    WK-GK-GENKA.
     ADD    RHC-C17         TO    WK-GK-BAIKA.
*
     IF       MS01-DENNO  NOT =   WK-DENNO
     IF       RHC-F06  NOT =   WK-DENNO
              WRITE     PRT-REC   FROM  MS01  AFTER  1
              ADD       1         TO    LINE-CNT
              MOVE    RHC-F06     TO    WK-DENNO
     END-IF.
     WRITE     PRT-REC   FROM  MS02  AFTER  1.
*    WRITE     PRT-REC   FROM  MS03  AFTER  1.
     ADD       2           TO   LINE-CNT.
*****MOVE   MS01-DENNO            TO   WK-DENNO.
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
     WRITE     PRT-REC   FROM  MS04  AFTER  2.
     WRITE     PRT-REC   FROM  SEN1  AFTER  1.
     MOVE      SPACE       TO   PRT-REC.
*
     MOVE     ZERO              TO    WK-GK-GENKA WK-GK-BAIKA.
*
     ADD       3           TO   LINE-CNT.
*
 GOKEI-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-RHC-START-READ     SECTION.
     MOVE     "900-RHC-START-READ"     TO   S-NAME.
     START    RHSYUKF   KEY  >=   RHC-F01   RHC-F02
                                  RHC-F03   RHC-F04
                                  RHC-B07   RHC-F08
                                  RHC-F05   RHC-B03
                                  RHC-B04   RHC-B10
                                  RHC-F06   RHC-F07
              INVALID   KEY
              MOVE     "END"      TO   END-FLG
              GO                  TO   900-RHC-START-READ-EXIT
     END-START.
*
     PERFORM   900-RHC-READ.
*
 900-RHC-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    ロイヤルHC出荷確定ファイル　 READ
*--------------------------------------------------------------*
 900-RHC-READ           SECTION.
     MOVE     "900-RHC-READ"      TO   S-NAME.
*
     READ     RHSYUKF   AT   END
              MOVE     "END"      TO   END-FLG
              GO        TO        900-RHC-READ-EXIT
     END-READ.
*
*ブレイクチェック
     IF   LINK-SOKO = SPACE THEN
          IF       RHC-F01   NOT =  LINK-JDATE       OR
                   RHC-F02   NOT =  LINK-JTIME       OR
                   RHC-F03   NOT =  LINK-TORICD
                   MOVE     "END"      TO   END-FLG
                   GO        TO        900-RHC-READ-EXIT
          END-IF
     ELSE
          IF       RHC-F01   NOT =  LINK-JDATE       OR
                   RHC-F02   NOT =  LINK-JTIME       OR
                   RHC-F03   NOT =  LINK-TORICD      OR
                   RHC-F04   NOT =  LINK-SOKO
                   MOVE     "END"      TO   END-FLG
                   GO        TO        900-RHC-READ-EXIT
          END-IF
     END-IF.
*店舗ＣＤ範囲チェック
     IF       LINK-STENCD  <=  RHC-F05
     AND      LINK-ETENCD  >=  RHC-F05
              CONTINUE
     ELSE
              GO        TO        900-RHC-READ
     END-IF.
*
*納品日範囲チェック
     IF       LINK-SNOUDT  <=  RHC-F08
     AND      LINK-ENOUDT  >=  RHC-F08
              CONTINUE
     ELSE
              GO        TO        900-RHC-READ
     END-IF.
*
     ADD      1         TO        READ-CNT.
*
 900-RHC-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
