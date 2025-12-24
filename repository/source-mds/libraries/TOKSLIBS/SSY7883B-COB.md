# SSY7883B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY7883B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ロイヤルＨＣ　物品受領ＣＳＶ　　　*
*    業務名　　　　　　　：　物品受領書                        *
*    モジュール名　　　　：　物品受領書                        *
*    作成日／更新日　　　：　08/11/13                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　ロイヤルＨＣ物品受領ＣＳＶデータ　*
*    　　　　　　　　　　　　を出力する。　　　　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY7883B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/11/13.
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
****<<DCMJAPAN物品受領データ >>*********************************
     SELECT   ROYJYRF            ASSIGN    TO   DA-01-VI-ROYJYRL3
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JYU-F17  JYU-F00
                                                JYU-F04  JYU-F03
                                                JYU-F06  JYU-F07
                                 STATUS         JYU-STATUS.
*
*****<<  受領ﾃﾞｰﾀCSV   >>**************************************
     SELECT   ROYJYRDT           ASSIGN    TO   ROYJYRDT
                                 STATUS         DCM-STATUS.
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
 FD  ROYJYRF            LABEL RECORD   IS   STANDARD.
     COPY     ROYJYRF   OF        XFDLIB
              JOINING   JYU       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ＤＣＭＪＡＰＡＮ　物品ＣＳＶデータ                 *
*--------------------------------------------------------------*
 FD  ROYJYRDT           BLOCK CONTAINS 1    RECORDS.
 01  DCM-REC.
     03  FILLER         PIC       X(500).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*ヘッダ情報格納領域
     COPY   ROYJYRDT OF XFDLIB  JOINING   DJR  AS   PREFIX.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  CHK-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  JYU-STATUS                   PIC       X(02).
 01  DCM-STATUS                   PIC       X(02).
*
 01  NENGETUDO                    PIC       9(06).
*
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC       9(06)  VALUE  ZERO.
     03  SYS-DATEW                PIC       9(08)  VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES SYS-DATEW.
         05  SYS-YY               PIC       9(04).
         05  SYS-MM               PIC       9(02).
         05  SYS-DD               PIC       9(02).
***** 合計エリア
 01  WK-TEN-GOKEI.
     03  WK-TEN-HATYU             PIC      S9(07)  VALUE  ZERO.
     03  WK-TEN-KENSYU            PIC      S9(07)  VALUE  ZERO.
     03  WK-TEN-GENKIN            PIC      S9(10)  VALUE  ZERO.
 01  WK-GOKEI.
     03  WK-GENKIN                PIC      S9(10)  VALUE  ZERO.
***** ブレイク
 01  WK-KEY.
     03  WK-KEY-F06               PIC       9(09)  VALUE  ZERO.
*
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
*タイトルエリア
 01  WK-TAITL0.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  TAITL0-01     PIC N(20).
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"年月度".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ":".
     03  TAITL0-02     PIC 9(06).
 01  WK-TAITL.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"取引先".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(02)  VALUE  "CD".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"発注日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"納品日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"店舗".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(02)  VALUE  "CD".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"店舗名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"伝票".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(02)  VALUE  "NO".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(01)  VALUE  NC"行".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"伝区".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"商品".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(02)  VALUE  "CD".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"ＪＡＮＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"商品名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"発注数".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"検収数".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"原単価".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"原価金額".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"納品書番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
*タイトルエリア
 01  GOK-REC.
     03  FILLER        PIC X(10)  VALUE  ",,,,,,,,,,".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  GOK-01        PIC N(04).
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  GOK-021       PIC X(01).
     03  GOK-02        PIC 9(07).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  GOK-031       PIC X(01).
     03  GOK-03        PIC 9(07).
     03  FILLER        PIC X(02)  VALUE  ",,".
     03  GOK-041       PIC X(01).
     03  GOK-04        PIC 9(10).
*タイトルエリア
 01  GGK-REC.
     03  FILLER        PIC X(10)  VALUE  ",,,,,,,,,,".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  GGK-01        PIC N(04).
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(02)  VALUE  ",,".
     03  GGK-041       PIC X(01).
     03  GGK-04        PIC 9(10).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7883B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7883B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY7883B".
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
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE ROYJYRF.
     MOVE     "ROYJYRF "          TO        ERR-FL-ID.
     MOVE     JYU-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE ROYJYRDT.
     MOVE     "ROYJYRDT"          TO        ERR-FL-ID.
     MOVE     DCM-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY7883B-START         SECTION.
*
     MOVE   "SSY7883B-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSY7883B-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       ROYJYRF.
     OPEN     OUTPUT    ROYJYRDT.
     DISPLAY  MSG-START UPON CONS.
*
     DISPLAY NC"（受領ＣＳＶ）出力年月度を入力（６桁）"
             UPON CONS.
     ACCEPT   NENGETUDO FROM CONS.
*
     IF       NENGETUDO  NOT NUMERIC
              DISPLAY NC"＃＃年月度を入力して下さい。＃＃"
                      UPON CONS
              STOP  RUN
     END-IF.
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
     PERFORM  ROYJYRF-START-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
              STOP  RUN
     END-IF.
     PERFORM  ROYJYRF-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     ELSE
              MOVE NC"ロイヤルＨＣ受領ＤＴ"
                                          TO  TAITL0-01
              MOVE   NENGETUDO     TO        TAITL0-02
              MOVE   SPACE         TO        DCM-REC
              MOVE   WK-TAITL0     TO        DCM-REC
              WRITE  DCM-REC
              MOVE   SPACE         TO        DCM-REC
              MOVE   WK-TAITL      TO        DCM-REC
              WRITE  DCM-REC
              MOVE   JYU-F06       TO        WK-KEY-F06
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ホーマック物品受領書データ読込み
****************************************************************
 ROYJYRF-RD-SEC             SECTION.
*
     MOVE    "ROYJYRF-RD-SEC"    TO   S-NAME.
*
     READ     ROYJYRF
          AT END
              MOVE     "END"      TO        END-FLG
          NOT  AT  END
              ADD       1         TO        READ-CNT
     END-READ.
*指定された年月度を超えた場合、終了
     IF       NENGETUDO  <  JYU-F17
              MOVE     "END"      TO        END-FLG
              GO                  TO        ROYJYRF-RD-EXIT
     END-IF.
*
 ROYJYRF-RD-EXIT.
     EXIT.
****************************************************************
*    ロイヤルＨＣ物品受領データスタート
****************************************************************
 ROYJYRF-START-SEC          SECTION.
*
     MOVE    "ROYJYRF-START-SEC" TO   S-NAME.
*
     MOVE     SPACE              TO   JYU-REC.
     INITIALIZE                       JYU-REC.
     MOVE     NENGETUDO          TO   JYU-F17.
     MOVE     51649              TO   JYU-F00.
     START    ROYJYRF  KEY  IS  >=    JYU-F17  JYU-F00  JYU-F04
                                      JYU-F03  JYU-F06  JYU-F07
              INVALID
              MOVE    "END"      TO   END-FLG
     END-START.
*
 ROYJYRF-START-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     IF       JYU-F06  NOT =  WK-KEY-F06
              MOVE    SPACE       TO   DCM-REC
              MOVE NC"店舗合計"   TO   GOK-01
              IF      WK-TEN-HATYU < ZERO
                      MOVE "-"         TO   GOK-021
                      COMPUTE GOK-02 = WK-TEN-HATYU * -1
              ELSE
                      MOVE "0"         TO   GOK-021
                      MOVE WK-TEN-HATYU TO  GOK-02
              END-IF
**************MOVE    WK-TEN-KENSYU    TO   GOK-03
              IF      WK-TEN-KENSYU < ZERO
                      MOVE "-"         TO   GOK-031
                      COMPUTE GOK-03 = WK-TEN-KENSYU * -1
              ELSE
                      MOVE "0"         TO   GOK-031
                      MOVE WK-TEN-KENSYU TO GOK-03
              END-IF
              IF      WK-TEN-GENKIN < ZERO
                      MOVE "-"         TO   GOK-041
                      COMPUTE GOK-04 = WK-TEN-GENKIN * -1
              ELSE
                      MOVE "0"         TO   GOK-041
                      MOVE WK-TEN-GENKIN TO GOK-04
              END-IF
              MOVE    GOK-REC     TO   DCM-REC
              WRITE   DCM-REC
              ADD     1           TO   OUTPUT-CNT
              MOVE    ZERO        TO   WK-TEN-GOKEI
              MOVE    JYU-F06     TO   WK-KEY-F06
     END-IF.
*初期化
     MOVE     SPACE               TO   DJR-REC.
     INITIALIZE                        DJR-REC.
*項目転送
     MOVE     ","                 TO   DJR-A01 DJR-A02 DJR-A03.
     MOVE     ","                 TO   DJR-A04 DJR-A05 DJR-A06.
     MOVE     ","                 TO   DJR-A07 DJR-A08 DJR-A09.
     MOVE     ","                 TO   DJR-A10.
     MOVE     ","                 TO   DJR-A13 DJR-A15.
*
     MOVE     JYU-F00             TO   DJR-F01.
     MOVE     JYU-F02             TO   DJR-F02.
     MOVE     JYU-F03             TO   DJR-F03.
     MOVE     JYU-F04             TO   DJR-F04.
     MOVE     JYU-F05             TO   DJR-F05.
     MOVE     JYU-F06             TO   DJR-F06.
     MOVE     JYU-F07             TO   DJR-F07.
     MOVE     JYU-F08             TO   DJR-F08.
     MOVE     JYU-F09             TO   DJR-F09.
     MOVE     JYU-F15             TO   DJR-F10.
     MOVE     JYU-F10             TO   DJR-F11.
     MOVE     JYU-F11             TO   DJR-F12.
     IF       JYU-F16(1:1) = "1"
              MOVE  ",-"          TO   DJR-A11
              COMPUTE WK-TEN-HATYU = WK-TEN-HATYU + JYU-F11 * -1
     ELSE
              MOVE  ",0"          TO   DJR-A11
              COMPUTE WK-TEN-HATYU = WK-TEN-HATYU + JYU-F11
     END-IF.
     MOVE     JYU-F12             TO   DJR-F13.
     IF       JYU-F16(1:1) = "1"
              MOVE  ",-"          TO   DJR-A12
              COMPUTE WK-TEN-KENSYU = WK-TEN-KENSYU + JYU-F12 * -1
     ELSE
              MOVE  ",0"          TO   DJR-A12
              COMPUTE WK-TEN-KENSYU = WK-TEN-KENSYU + JYU-F12
     END-IF.
     MOVE     JYU-F13             TO   DJR-F14.
     MOVE     JYU-F14             TO   DJR-F15
     IF       JYU-F16(1:1) = "1"
              MOVE  ",-"          TO   DJR-A14
           COMPUTE WK-TEN-GENKIN = WK-TEN-GENKIN + JYU-F14 * -1
           COMPUTE WK-GENKIN = WK-GENKIN + JYU-F14 * -1
     ELSE
              MOVE  ",0"          TO   DJR-A14
           COMPUTE WK-TEN-GENKIN = WK-TEN-GENKIN + JYU-F14
           COMPUTE WK-GENKIN = WK-GENKIN + JYU-F14
     END-IF.
     MOVE     JYU-F16             TO   DJR-F16
     MOVE     DJR-REC             TO   DCM-REC.
     WRITE    DCM-REC.
     ADD      1                   TO   OUTPUT-CNT.
*    次レコード読込み
     PERFORM  ROYJYRF-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
     IF       OUTPUT-CNT  >  ZERO
              MOVE    SPACE       TO   DCM-REC
              MOVE NC"店舗合計"   TO   GOK-01
              IF      WK-TEN-HATYU < ZERO
                      MOVE "-"         TO   GOK-021
                      COMPUTE GOK-02 = WK-TEN-HATYU * -1
              ELSE
                      MOVE "0"         TO   GOK-021
                      MOVE WK-TEN-HATYU TO  GOK-02
              END-IF
**************MOVE    WK-TEN-KENSYU    TO   GOK-03
              IF      WK-TEN-KENSYU < ZERO
                      MOVE "-"         TO   GOK-031
                      COMPUTE GOK-03 = WK-TEN-KENSYU * -1
              ELSE
                      MOVE "0"         TO   GOK-031
                      MOVE WK-TEN-KENSYU TO GOK-03
              END-IF
              IF      WK-TEN-GENKIN < ZERO
                      MOVE "-"         TO   GOK-041
                      COMPUTE GOK-04 = WK-TEN-GENKIN * -1
              ELSE
                      MOVE "0"         TO   GOK-041
                      MOVE WK-TEN-GENKIN TO GOK-04
              END-IF
              MOVE    GOK-REC     TO   DCM-REC
              WRITE   DCM-REC
              ADD     1           TO   OUTPUT-CNT
              MOVE    ZERO        TO   WK-TEN-GOKEI
**************MOVE    SPACE       TO   GGK-REC
              MOVE NC"総合計　"   TO   GGK-01
              IF      WK-GENKIN < ZERO
                      MOVE "-"         TO   GGK-041
                      COMPUTE GGK-04 = WK-GENKIN * -1
              ELSE
                      MOVE "0"         TO   GGK-041
                      MOVE WK-GENKIN   TO   GGK-04
              END-IF
              MOVE    GGK-REC     TO   DCM-REC
              WRITE   DCM-REC
              ADD     1           TO   OUTPUT-CNT
     END-IF.
*
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
*
     CLOSE    ROYJYRF   ROYJYRDT.
*
 END-EXIT.
     EXIT.
********************<<  PUROGRAM  END  >>*************************

```
