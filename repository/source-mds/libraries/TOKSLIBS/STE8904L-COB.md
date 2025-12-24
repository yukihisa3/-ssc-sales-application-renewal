# STE8904L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/STE8904L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　新規取引先オンライン　　　　　　　*
*    業務名　　　　　　　：　リック　　　　                    *
*    モジュール名　　　　：　リック手書納品伝票発行　　　      *
*    作成日／更新日　　　：　08/04/22                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            STE8904L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/04/22.
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
*----<< リック出荷情報ファイル>>--*
     SELECT   RCSYUKF   ASSIGN         DA-01-VI-RCSYUKL4
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  RCK-K01
                                       RCK-K02
                                       RCK-K03
                                       RCK-K04
                                       RCK-K08
                                       RCK-F03
                                       RCK-K05
                                       RCK-F13
                                       RCK-F05
                                       RCK-F06
                                       RCK-F07
                                       RCK-K06
                                       RCK-K07
                        STATUS         RCSYUKF-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04-PRTF.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< リック出荷情報ファイル>>--*
 FD  RCSYUKF            LABEL     RECORD   IS   STANDARD.
     COPY     RCSYUKF   OF        XFDLIB
              JOINING   RCK       PREFIX.
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
 01  RCSYUKF-ST         PIC  X(02).
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
     03  WK-RCK-K01     PIC  9(08).
     03  WK-RCK-K02     PIC  9(04).
     03  WK-RCK-K03     PIC  9(08).
     03  WK-RCK-K04     PIC  X(02).
     03  WK-RCK-K08     PIC  9(08).
     03  WK-RCK-F03     PIC  9(08).
     03  WK-RCK-K05     PIC  9(05).
     03  WK-RCK-F13     PIC  X(04).
     03  WK-RCK-F05     PIC  X(02).
     03  WK-RCK-F06     PIC  X(02).
     03  WK-RCK-F07     PIC  X(02).
     03  WK-RCK-K06     PIC  9(09).
     03  WK-RCK-K07     PIC  9(02).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD000.
     03  FILLER         CHARACTER TYPE BAIKAKU.
         05  FILLER     PIC  X(56)     VALUE  SPACE.
         05  FILLER     PIC  N(07)     VALUE
                                           NC"店別納品一覧表".
 01  HD00.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD00-KKME  PIC  N(20).
         05  FILLER     PIC  X(68)    VALUE  SPACE.
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
**       05  FILLER     PIC  X(01)     VALUE  "-".
**       05  HD00-PCNT  PIC  ZZ9.
 01  HD01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝票".
         05  FILLER     PIC  X(04)     VALUE  "ﾀｲﾌﾟ".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD01-DENTYPE    PIC  N(03).
 01  HD02.
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
         05  HD02-TENME PIC  N(20).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票区分".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD02-DENKB PIC  99.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD02-DENME PIC  N(04).
         05  FILLER     PIC  X(22)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD02-TORCD PIC  999999.
 01  HD03.
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
         05  HD03-BUMME PIC  N(15).
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"ルート".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD03-RUTO  PIC  99.
         05  FILLER     PIC  X(29)     VALUE  SPACE.
         05  HD03-TORME PIC  N(20).
 01  HD04.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(102)    VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(03)     VALUE  "TEL".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD04-TEL   PIC  X(13).
 01  HD05.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票番号".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  X(07)     VALUE  "JANｺｰﾄﾞ".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"商品名".
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  FILLER     PIC  N(03)     VALUE  NC"規格名".
         05  FILLER     PIC  X(20)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"発注数量".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"納品数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"欠品".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"訂正".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"原単価".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価金額".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"売単価".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価金額".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"欠".
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
*
 01  MS01.
     03  FILLER                  CHARACTER  TYPE  PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-DENNO PIC  ZZZZZZ9.
         05  FILLER     PIC  X(01)     VALUE  "-".
         05  MS01-GYONO PIC  99.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-JANCD PIC  X(13)     VALUE  SPACE.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-SHOME PIC  N(20).
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-HACSU PIC  Z,ZZ9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-NOHSU PIC  Z,ZZ9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-KEPIN PIC  Z,ZZ9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
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
 01  MS02.
     03  FILLER.
         05  FILLER     PIC  X(28)     VALUE  SPACE.
         05  MS02-KIKAKU PIC  X(10)    VALUE  SPACE.
 01  MS03.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(87)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"原価金額計".
         05  MS03-GENGOK PIC  ZZ,ZZZ,ZZZ,ZZ9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"売価金額計".
         05  MS03-BAIGOK PIC  ZZ,ZZZ,ZZZ,ZZ9.
 01  MS04.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"欠品理由".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(02)     VALUE  "1:".
         05  FILLER     PIC  N(06)     VALUE  NC"取引先側理由".
         05  FILLER     PIC  X(03)     VALUE  "､2:".
         05  FILLER     PIC  N(05)     VALUE  NC"小売側理由".
         05  FILLER     PIC  X(03)     VALUE  "､3:".
         05  FILLER     PIC  N(12)     VALUE
                                 NC"新規商品手配前・終売商品".
         05  FILLER     PIC  X(03)     VALUE  "､4:".
         05  FILLER     PIC  N(03)     VALUE  NC"その他".
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
 01  LINK-TORICD            PIC  9(08).
 01  LINK-SOKO              PIC  X(02).
 01  LINK-DENST             PIC  9(09).
 01  LINK-DENED             PIC  9(09).
 01  LINK-STENCD            PIC  9(05).
 01  LINK-ETENCD            PIC  9(05).
 01  LINK-SNOUDT            PIC  9(08).
 01  LINK-ENOUDT            PIC  9(08).
****************************************************************
 PROCEDURE              DIVISION  USING  LINK-TORICD
                                         LINK-SOKO
                                         LINK-DENST
                                         LINK-DENED
                                         LINK-STENCD
                                         LINK-ETENCD
                                         LINK-SNOUDT
                                         LINK-ENOUDT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< リック出荷情報ファイル >>--*
 RCSYUKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCSYUKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### STE8904L RCSYUKF ERROR " RCSYUKF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    RCSYUKF.
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
     DISPLAY  "*** STE8904L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     RCSYUKF.
     OPEN     OUTPUT    PRTF.
*
     INITIALIZE  BREAK-KEY.
*リック出荷確定データＲＥＡＤ
     MOVE     SPACE               TO   RCK-REC.
     INITIALIZE                        RCK-REC.
     MOVE     99999999            TO   RCK-K01.
     MOVE     9999                TO   RCK-K02.
     MOVE     LINK-TORICD         TO   RCK-K03.
     MOVE     LINK-SOKO           TO   RCK-K04.
     MOVE     LINK-STENCD         TO   RCK-K05.
     MOVE     LINK-DENST          TO   RCK-K06.
     MOVE     LINK-SNOUDT         TO   RCK-K08.
     PERFORM  900-RCK-START-READ.
*
     IF       END-FLG  =  "END"
              DISPLAY NC"＃＃出力対象無し＃＃" UPON CONS
              STOP  RUN
     ELSE
              MOVE     RCK-K01    TO   WK-RCK-K01
              MOVE     RCK-K02    TO   WK-RCK-K02
              MOVE     RCK-K03    TO   WK-RCK-K03
              MOVE     RCK-K04    TO   WK-RCK-K04
              MOVE     RCK-K08    TO   WK-RCK-K08
              MOVE     RCK-F03    TO   WK-RCK-F03
              MOVE     RCK-K05    TO   WK-RCK-K05
              MOVE     RCK-F13    TO   WK-RCK-F13
              MOVE     RCK-F05    TO   WK-RCK-F05
              MOVE     RCK-F06    TO   WK-RCK-F06
              MOVE     RCK-F07    TO   WK-RCK-F07
              MOVE     RCK-K06    TO   WK-RCK-K06
              MOVE     RCK-K07    TO   WK-RCK-K07
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
*    バッチ日付／バッチ時刻／バッチ取引先／倉庫ＣＤ／納品日／
*    発注日／店舗ＣＤ／部門ＣＤ／伝票タイプ／伝票区分／ルート
*    ブレイク時改ページ
     IF       RCK-K01   NOT =  WK-RCK-K01
     OR       RCK-K02   NOT =  WK-RCK-K02
     OR       RCK-K03   NOT =  WK-RCK-K03
     OR       RCK-K04   NOT =  WK-RCK-K04
     OR       RCK-K08   NOT =  WK-RCK-K08
     OR       RCK-F03   NOT =  WK-RCK-F03
     OR       RCK-K05   NOT =  WK-RCK-K05
     OR       RCK-F13   NOT =  WK-RCK-F13
     OR       RCK-F05   NOT =  WK-RCK-F05
     OR       RCK-F06   NOT =  WK-RCK-F06
     OR       RCK-F07   NOT =  WK-RCK-F07
              PERFORM   GOKEI-WT-SEC
              MOVE      ZERO      TO  PAGE-CNT2
              PERFORM   HEAD-WT-SEC
              MOVE      RCK-K01   TO  WK-RCK-K01
              MOVE      RCK-K02   TO  WK-RCK-K02
              MOVE      RCK-K03   TO  WK-RCK-K03
              MOVE      RCK-K04   TO  WK-RCK-K04
              MOVE      RCK-K08   TO  WK-RCK-K08
              MOVE      RCK-F03   TO  WK-RCK-F03
              MOVE      RCK-K05   TO  WK-RCK-K05
              MOVE      RCK-F13   TO  WK-RCK-F13
              MOVE      RCK-F05   TO  WK-RCK-F05
              MOVE      RCK-F06   TO  WK-RCK-F06
              MOVE      RCK-F07   TO  WK-RCK-F07
              MOVE      RCK-K06   TO  WK-RCK-K06
              MOVE      RCK-K07   TO  WK-RCK-K07
              MOVE      ZERO      TO  WK-GK-GENKA WK-GK-BAIKA
     END-IF.
*    伝票番号がブレイク
     IF       RCK-K06   NOT =  WK-RCK-K06
              IF  PAGE-CNT > ZERO
                  PERFORM  GOKEI-WT-SEC
              END-IF
*             PERFORM   MEISAI-HEAD-SEC
              MOVE      RCK-K01   TO  WK-RCK-K01
              MOVE      RCK-K02   TO  WK-RCK-K02
              MOVE      RCK-K03   TO  WK-RCK-K03
              MOVE      RCK-K04   TO  WK-RCK-K04
              MOVE      RCK-K08   TO  WK-RCK-K08
              MOVE      RCK-F03   TO  WK-RCK-F03
              MOVE      RCK-K05   TO  WK-RCK-K05
              MOVE      RCK-F13   TO  WK-RCK-F13
              MOVE      RCK-F05   TO  WK-RCK-F05
              MOVE      RCK-F06   TO  WK-RCK-F06
              MOVE      RCK-F07   TO  WK-RCK-F07
              MOVE      RCK-K06   TO  WK-RCK-K06
              MOVE      RCK-K07   TO  WK-RCK-K07
              MOVE      ZERO      TO  WK-GK-GENKA WK-GK-BAIKA
     END-IF.
*    明細行セット
     PERFORM  MEISAI-BODY-SEC.
*    リック出荷確定データ読込み
     PERFORM  900-RCK-READ.
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
     CLOSE    RCSYUKF  PRTF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** STE8904L END *** "
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
*    MOVE     PAGE-CNT  TO        HD00-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4) TO   WK-H-YYYY.
     MOVE     SYS-DATEW(5:2) TO   WK-H-MM.
     MOVE     SYS-DATEW(7:2) TO   WK-H-DD.
     MOVE     "/"            TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-H-YYYY      TO   HD00-YYYY.
     MOVE     WK-H-MM        TO   HD00-MM.
     MOVE     WK-H-DD        TO   HD00-DD.
     MOVE     PAGE-CNT       TO   HD00-PAGE.
*    小売企業名称（漢字）
     MOVE     RCK-F102        TO  HD00-KKME.
*    伝票タイプ　
     EVALUATE RCK-F05
         WHEN "11"
              MOVE NC"ＥＯＳ" TO  HD01-DENTYPE
         WHEN "21"
              MOVE NC"手書"   TO  HD01-DENTYPE
     END-EVALUATE.
*    発注日
     MOVE     RCK-F03(1:4)   TO   HD02-HACYY.
     MOVE     RCK-F03(5:2)   TO   HD02-HACMM.
     MOVE     RCK-F03(7:2)   TO   HD02-HACDD.
*    店ＣＤ
     MOVE     RCK-K05        TO   HD02-TENCD.
*    店舗名称
     MOVE     RCK-F192       TO   HD02-TENME.
*    伝票区分
     MOVE     RCK-F06        TO   HD02-DENKB.
     EVALUATE RCK-F06
         WHEN "10"
              MOVE NC"ＴＡ"   TO  HD02-DENME
         WHEN "11"
              MOVE NC"本発"  TO  HD02-DENME
         WHEN "12"
              MOVE NC"客注"  TO  HD02-DENME
         WHEN "20"
              MOVE NC"手書"  TO  HD02-DENME
     END-EVALUATE.
*    取引先
     MOVE     RCK-F15        TO   HD02-TORCD.
*    納品日
     MOVE     RCK-K08(1:4)   TO   HD03-NOHYY.
     MOVE     RCK-K08(5:2)   TO   HD03-NOHMM.
     MOVE     RCK-K08(7:2)   TO   HD03-NOHDD.
*    部門　
     MOVE     RCK-F13        TO   HD03-BUMCD.
     MOVE     RCK-F122       TO   HD03-BUMME.
*ルート
     MOVE     RCK-F07        TO   HD03-RUTO.
*取引先名
     MOVE     RCK-F172       TO   HD03-TORME.
*取引先名ＴＥＬ
     MOVE     RCK-F21        TO   HD04-TEL.
*    ヘッダ印刷
     WRITE    PRT-REC  FROM  HD000 AFTER  1.
     WRITE    PRT-REC  FROM  HD00  AFTER  1.
     WRITE    PRT-REC  FROM  HD01  AFTER  1.
     WRITE    PRT-REC  FROM  HD02  AFTER  1.
     WRITE    PRT-REC  FROM  HD03  AFTER  1.
     WRITE    PRT-REC  FROM  HD04  AFTER  1.
     WRITE    PRT-REC  FROM  HD05  AFTER  2.
*    WRITE    PRT-REC  FROM  MS04  AFTER 39.
*行カウント
     MOVE      9             TO    LINE-CNT.
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
     MOVE   RCK-K06               TO   MS01-DENNO.
*    行
     MOVE   RCK-M03               TO   MS01-GYONO.
*    ＪＡＮＣＤ
     MOVE   RCK-M02               TO   MS01-JANCD.
*    商品名
     MOVE   RCK-M052              TO   MS01-SHOME.
*    規格名
     MOVE   RCK-M06               TO   MS02-KIKAKU.
*    発注数量
     MOVE   RCK-M10               TO   MS01-HACSU.
*    納品数量
     MOVE   RCK-M12               TO   MS01-NOHSU.
*    欠品数量
     COMPUTE    WK-KEPPIN   =  RCK-M10 - RCK-M12.
     MOVE   WK-KEPPIN             TO   MS01-KEPIN.
*    訂正数量
     MOVE   "(  )"                TO   MS01-TESEI.
*    原価
     COMPUTE   WK-GENKA     =    RCK-M16  /  100.
     MOVE   WK-GENKA              TO   MS01-GENTAN.
*    売価
     MOVE   RCK-M17               TO   MS01-BAITAN.
*    原価金額
     MOVE   RCK-M14               TO   MS01-GENKIN.
*    売価金額
     MOVE   RCK-M15               TO   MS01-BAIKIN.
*    欠品理由
     MOVE   RCK-M09               TO   MS01-RIYUU.
*
     ADD    RCK-M14         TO    WK-GK-GENKA.
*    DISPLAY  "RCK-M14    =" RCK-M14     UPON CONS.
*    DISPLAY  "WK-GK-GENKA=" WK-GK-GENKA UPON CONS.
     ADD    RCK-M15         TO    WK-GK-BAIKA.
*    DISPLAY  "RCK-M15    =" RCK-M15     UPON CONS.
*    DISPLAY  "WK-GK-BAIKA=" WK-GK-BAIKA UPON CONS.
*
     WRITE     PRT-REC   FROM  MS01  AFTER  1.
     WRITE     PRT-REC   FROM  MS02  AFTER  1.
     ADD       2           TO   LINE-CNT.
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
     WRITE     PRT-REC   FROM  SEN   AFTER  1.
     MOVE      SPACE       TO   PRT-REC.
     WRITE     PRT-REC     AFTER  1.
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
 900-RCK-START-READ     SECTION.
     MOVE     "900-RCK-START-READ"     TO   S-NAME.
     START    RCSYUKF   KEY  >=   RCK-K01  RCK-K02
                                  RCK-K03  RCK-K04
                                  RCK-K08  RCK-F03
                                  RCK-K05  RCK-F13
                                  RCK-F05  RCK-F06
                                  RCK-F07  RCK-K06
                                  RCK-K07
              INVALID   KEY
**************DISPLAY "AAA" UPON CONS
              MOVE     "END"      TO   END-FLG
              GO                  TO   900-RCK-START-READ-EXIT
     END-START.
*
     PERFORM   900-RCK-READ.
*
 900-RCK-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    リック出荷確定ファイル　 READ                *
*--------------------------------------------------------------*
 900-RCK-READ           SECTION.
     MOVE     "900-RCK-READ"      TO   S-NAME.
*
     READ     RCSYUKF   AT   END
**************DISPLAY "DDD" UPON CONS
              MOVE     "END"      TO   END-FLG
              GO        TO        900-RCK-READ-EXIT
     END-READ.
*
*ブレイクチェック
     IF   LINK-SOKO = SPACE THEN
*         IF       RCK-K01   NOT =  LINK-JDATE       OR
*                  RCK-K02   NOT =  LINK-JTIME       OR
          IF       RCK-K03   NOT =  LINK-TORICD
**************DISPLAY "BBB" UPON CONS
                   MOVE     "END"      TO   END-FLG
                   GO        TO        900-RCK-READ-EXIT
          END-IF
     ELSE
*         IF       RCK-K01   NOT =  LINK-JDATE       OR
*                  RCK-K02   NOT =  LINK-JTIME       OR
          IF       RCK-K03   NOT =  LINK-TORICD      OR
                   RCK-K04   NOT =  LINK-SOKO
**************DISPLAY "CCC" UPON CONS
                   MOVE     "END"      TO   END-FLG
                   GO        TO        900-RCK-READ-EXIT
          END-IF
     END-IF.
*店舗ＣＤ範囲チェック
     IF       LINK-STENCD  <=  RCK-K05
     AND      LINK-ETENCD  >=  RCK-K05
              CONTINUE
     ELSE
**************DISPLAY "EEE" UPON CONS
              GO        TO        900-RCK-READ
     END-IF.
*
*伝票番号範囲チェック
     IF       LINK-DENST   <=  RCK-K06
     AND      LINK-DENED   >=  RCK-K06
              CONTINUE
     ELSE
              GO        TO        900-RCK-READ
     END-IF.
*
*納品日範囲チェック
     IF       LINK-SNOUDT  <=  RCK-K08
     AND      LINK-ENOUDT  >=  RCK-K08
              CONTINUE
     ELSE
**************DISPLAY "FFF" UPON CONS
              GO        TO        900-RCK-READ
     END-IF.
*
     ADD      1         TO        READ-CNT.
*
 900-RCK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
