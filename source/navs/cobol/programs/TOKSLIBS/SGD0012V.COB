****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　グッデイ　手書発注データ　　　　　*
*    業務名　　　　　　　：　手書発注データＣＳＶ              *
*    モジュール名　　　　：　支払明細書データＣＳＶ            *
*    作成日／更新日　　　：　08/06/04                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　手書発注データをＣＳＶファイルに  *
*    　　　　　　　　　　　　出力する。　　　　　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SGD0012V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/06/04.
*
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
*
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<ウタネ手書発注データ >>*********************************
     SELECT   UTAHACF            ASSIGN    TO     JHTDENF
                                 ACCESS    MODE   SEQUENTIAL
                                 STATUS           UTA-STATUS.
*
*****<<  発注ﾃﾞｰﾀCSV   >>**************************************
     SELECT   UTACSV             ASSIGN    TO     UTACSV
                                 ACCESS    MODE   SEQUENTIAL
                                 STATUS           CSV-STATUS.
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ＤＣＭＪＡＰＡＮ　物品受領書                       *
*--------------------------------------------------------------*
 FD  UTAHACF            BLOCK CONTAINS 1    RECORDS.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   UTA       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ＤＣＭＪＡＰＡＮ　物品ＣＳＶデータ                 *
*--------------------------------------------------------------*
 FD  UTACSV             BLOCK CONTAINS 1    RECORDS.
     COPY     UTACSV    OF        XFDLIB
              JOINING   CSV       PREFIX.
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  CHK-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  UTA-STATUS                   PIC       X(02).
 01  CSV-STATUS                   PIC       X(02).
*
***** ワーク　エリア
 01  WK-AREA.
     03  WK-KUBUN                 PIC   X(01)   VALUE SPACE.
     03  WK-SEQ                   PIC   9(03)   VALUE ZERO.
     03  WK-TENCD                 PIC   9(05)   VALUE ZERO.
     03  WK-SEIKIN                PIC  S9(11)   VALUE ZERO.
     03  WK-SIHKIN                PIC  S9(11)   VALUE ZERO.
     03  IX                       PIC   9(02)   VALUE ZERO.
     03  WK-GENKA1                PIC   9(13)   VALUE ZERO.
     03  WK-HIDUKE                PIC   9(08)   VALUE ZERO.
 01  WK-DATE.
     03  WK-NEN                   PIC   9(04)   VALUE ZERO.
     03  FILLER                   PIC   X(01)   VALUE "/".
     03  WK-TUKI                  PIC   9(02)   VALUE ZERO.
     03  FILLER                   PIC   X(01)   VALUE "/".
     03  WK-HI                    PIC   9(02)   VALUE ZERO.
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC       9(06)  VALUE  ZERO.
     03  SYS-DATEW                PIC       9(08)  VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES SYS-DATEW.
         05  SYS-YY               PIC       9(04).
         05  SYS-MM               PIC       9(02).
         05  SYS-DD               PIC       9(02).
*
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
*タイトルエリア
 01  WK-TAITL.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"受信日付".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"伝票番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"行番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"品番ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"ＪＡＮＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"発注数量".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"販売単位単価".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"原価単価".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"売価単価".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"原価複価".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"売価複価".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"商品名上段".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"商品名下段".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"店舗ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"発注日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"納品日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"店舗名カナ".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SGD0012V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SGD0012V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SGD0012V".
         05  FILLER               PIC       X(10)  VALUE
                       " ABEND ###".
*
     03  MSG-ABEND2.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-FL-ID            PIC       X(08).
         05  FILLER               PIC       X(04)  VALUE
                       " ST-".
         05  ERR-STCD             PIC       X(02).
         05  FILLER               PIC       X(04)  VALUE
                       " ###".
*
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPG= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*LINKAGE                SECTION.
*01  PARA-KENSU             PIC   9(08).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
*PROCEDURE              DIVISION    USING   PARA-KENSU.
 PROCEDURE              DIVISION.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE UTAHACF.
     MOVE     "UTAHACF  "          TO        ERR-FL-ID.
     MOVE     UTA-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE UTACSV.
     MOVE     "UTACSV"          TO        ERR-FL-ID.
     MOVE     CSV-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SGD0012V-START         SECTION.
*
     MOVE   "SGD0012V-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SGD0012V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     UTAHACF.
     OPEN     OUTPUT    UTACSV.
     DISPLAY  MSG-START UPON CONS.
*
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
*
     PERFORM  UTAHACF-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ウタネ手書発注データ読込み
****************************************************************
 UTAHACF-RD-SEC             SECTION.
*
     MOVE    "UTAHACF-RD-SEC"    TO   S-NAME.
*
     READ     UTAHACF
          AT END
              MOVE     "END"      TO        END-FLG
          NOT  AT  END
*             MOVE      UTA-REC   TO        MEI-REC
              ADD       1         TO        READ-CNT
     END-READ.
*
 UTAHACF-RD-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
*    明細データ転送
     PERFORM  MEI-TENSO-SEC.
*    次レコード読込み
     PERFORM  UTAHACF-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*             明細データ転送                2.0                *
****************************************************************
 MEI-TENSO-SEC          SECTION.
*
     MOVE    "MEI-TENSO-SEC"           TO   S-NAME.
*初期化
     MOVE     SPACE               TO   CSV-REC.
     INITIALIZE                        CSV-REC.
*項目転送
     MOVE     ","                 TO   CSV-A01 CSV-A02 CSV-A03.
     MOVE     ","                 TO   CSV-A04 CSV-A05 CSV-A06.
     MOVE     ","                 TO   CSV-A07 CSV-A08 CSV-A09.
     MOVE     ","                 TO   CSV-A10 CSV-A11 CSV-A12.
     MOVE     ","                 TO   CSV-A13 CSV-A14 CSV-A15.
     MOVE     ","                 TO   CSV-A16.
*
*    MOVE    X"28"                TO   CSV-F031 CSV-F051.
*    MOVE    X"29"                TO   CSV-F032 CSV-F052.
*受信日付
     MOVE     SYS-DATEW           TO   WK-HIDUKE.
     MOVE     WK-HIDUKE(1:4)      TO   WK-NEN.
     MOVE     WK-HIDUKE(5:2)      TO   WK-TUKI.
     MOVE     WK-HIDUKE(7:2)      TO   WK-HI.
     MOVE     WK-DATE             TO   CSV-F01.
*伝票番号
     MOVE     UTA-F02             TO   CSV-F02.
*行番号
     MOVE     UTA-F03             TO   CSV-F03.
*品番ＣＤ
*    MOVE     UTA-F               TO   CSV-F04.
*ＪＡＮＣＤ
     MOVE     UTA-F25             TO   CSV-F05.
*発注数量
     COMPUTE  CSV-F06  =  UTA-F15 * 100.
*販売単位単価
*    MOVE     UTA-F               TO   CSV-F07.
*原価単価
     COMPUTE  WK-GENKA1 = UTA-F172 * 100.
*    COMPUTE  CSV-F08   = WK-GENKA1 / 100.
     MOVE     WK-GENKA1           TO   CSV-F08.
*売価単価
     MOVE     UTA-F173            TO   CSV-F09.
*原価複価　
     MOVE     UTA-F181            TO   CSV-F10.
*売価複価
     MOVE     UTA-F182            TO   CSV-F11.
*商品名上段
     MOVE     UTA-F1421           TO   CSV-F12.
*商品名下段
     MOVE     UTA-F1422           TO   CSV-F13.
*店舗ＣＤ
     MOVE     UTA-F07             TO   CSV-F14.
*発注日　
     MOVE     UTA-F111            TO   WK-HIDUKE.
     MOVE     WK-HIDUKE(1:4)      TO   WK-NEN.
     MOVE     WK-HIDUKE(5:2)      TO   WK-TUKI.
     MOVE     WK-HIDUKE(7:2)      TO   WK-HI.
     MOVE     WK-DATE             TO   CSV-F15.
*納品日　
     MOVE     UTA-F112            TO   WK-HIDUKE.
     MOVE     WK-HIDUKE(1:4)      TO   WK-NEN.
     MOVE     WK-HIDUKE(5:2)      TO   WK-TUKI.
     MOVE     WK-HIDUKE(7:2)      TO   WK-HI.
     MOVE     WK-DATE             TO   CSV-F16.
*店舗名カナ
     MOVE     UTA-F30             TO   CSV-F17.
*
     WRITE    CSV-REC.
*
     ADD      1                   TO   OUTPUT-CNT.
 MEI-TENSO-EXIT.
     EXIT.
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
*****MOVE    OUTPUT-CNT    TO     PARA-KENSU.
*
     CLOSE    UTAHACF   UTACSV.
*
 END-EXIT.
     EXIT.
********************<<  PUROGRAM  END  >>*************************
