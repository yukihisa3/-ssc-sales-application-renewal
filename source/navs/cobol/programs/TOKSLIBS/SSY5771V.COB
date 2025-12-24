****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　グッデイＥＤＩ                    *
*    業務名　　　　　　　：　受注・売上　　　　　　　          *
*    モジュール名　　　　：　受領書ＣＳＶデータ出力            *
*    作成日／更新日　　　：　14/07/03                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取りパラメタに合致する　　    *
*    　　　　　　　　　　　　レコードをＣＳＶファイルに　　　  *
*    　　　　　　　　　　　　出力する。　　　　　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY5771V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/07/03.
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
****<<受領累積データ >>*********************************
     SELECT   GDJYURL2           ASSIGN    TO   DA-01-VI-GDJYURL2
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    GDJ-F04  GDJ-F05
                                                GDJ-F06  GDJ-F07
                                                GDJ-F08
                                 STATUS         GDJ-STATUS.
*
*****<<受領データＣＳＶ >>*******************************
     SELECT   GDJYRCSV           ASSIGN    TO   GDJYRCSV
                                 STATUS         CSV-STATUS.
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = グッデイ受領累積データ　　　                       *
*--------------------------------------------------------------*
 FD  GDJYURL2            LABEL RECORD   IS   STANDARD.
     COPY     GDJYURL2   OF        XFDLIB
              JOINING    GDJ       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = グッデイ受領ＣＳＶデータ                           *
*--------------------------------------------------------------*
 FD  GDJYRCSV           BLOCK CONTAINS 1    RECORDS.
 01  OUT-REC.
     03  FILLER         PIC       X(350).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*ヘッダ情報格納領域
     COPY   GDJYRCSV OF XFDLIB  JOINING   CSV  AS   PREFIX.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  CHK-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  GDJ-STATUS                   PIC       X(02).
 01  CSV-STATUS                   PIC       X(02).
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
***** 変換ワーク
 01  HENKAN1                      PIC       9(08)V99.
 01  HENKAN1R        REDEFINES    HENKAN1.
     03  HENKAN1-1                PIC       9(08).
     03  HENKAN1-2                PIC       9(02).
 01  HENKAN1-DATA.
     03  HENKAN1-DATA1            PIC       X(08).
     03  HENKAN1-DATA2            PIC       X(01).
     03  HENKAN1-DATA3            PIC       X(02).
***** ブレイク
 01  WK-KEY.
     03  WK-KEY-F04               PIC       9(08)  VALUE  ZERO.
     03  WK-KEY-F05               PIC       9(03)  VALUE  ZERO.
     03  WK-KEY-F06               PIC       X(02)  VALUE  "  ".
*
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
*タイトルエリア
 01  WK-TAITL.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"店舗".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(02)  VALUE  "CD".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"店舗名".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"伝票区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"伝票区分名".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"伝票番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"行番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"検品日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  "/".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"返品日".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"ＪＡＮＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"符号".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"検品数".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"原価単価".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"売価単価".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"符号".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"原価金額".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"符号".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"売価金額".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"商品名（上段）".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"商品名（下段）".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"返品理由".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
     03  FILLER        PIC X(01)  VALUE  ",".
*
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"返品理由名".
     03  FILLER        PIC X(01)  VALUE  X"29".
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY5771V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY5771V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY5771V".
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
         05  FILLER         PIC   X(09)  VALUE " OUTPUT=".
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
 LINKAGE                SECTION.
 01  PARA-KENDTS            PIC   9(08).
 01  PARA-KENDTE            PIC   9(08).
 01  PARA-TENCDS            PIC   9(03).
 01  PARA-TENCDE            PIC   9(03).
 01  PARA-DENKU             PIC   X(02).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING PARA-KENDTS
                                        PARA-KENDTE
                                        PARA-TENCDS
                                        PARA-TENCDE
                                        PARA-DENKU.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE GDJYURL2.
     MOVE     "GDJYURL2 "         TO        ERR-FL-ID.
     MOVE     GDJ-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO        PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE GDJYRCSV.
     MOVE     "GDJYRCSV"          TO        ERR-FL-ID.
     MOVE     CSV-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO        PROGRAM-STATUS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY5771V-START         SECTION.
*
     MOVE   "SSY5771V-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSY5771V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     GDJYURL2.
     OPEN     OUTPUT    GDJYRCSV.
     DISPLAY  MSG-START UPON CONS.
*
*    DISPLAY NC"（受領ＣＳＶ）出力年月度を入力（６桁）"
*            UPON CONS.
*    ACCEPT   NENGETUDO FROM CONS.
*
*    IF       NENGETUDO  NOT NUMERIC
*             DISPLAY NC"＃＃年月度を入力して下さい。＃＃"
*                     UPON CONS
*             STOP  RUN
*    END-IF.
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
     PERFORM  GDJYURL2-START-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
              STOP  RUN
     END-IF.
     PERFORM  GDJYURL2-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     ELSE
*             MOVE NC"グッデイ受領ＤＴ"
*                                         TO  TAITL0-01
*             MOVE   NENGETUDO     TO        TAITL0-02
              MOVE   SPACE         TO        OUT-REC
              MOVE   WK-TAITL      TO        OUT-REC
              WRITE  OUT-REC
*             MOVE   GDJ-F04       TO        WK-KEY-F04
*                                            WK-KEY-F05
*                                            WK-KEY-F06
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    グッデイ受領累積データスタート
****************************************************************
 GDJYURL2-START-SEC          SECTION.
*
     MOVE    "GDJYURL2-START-SEC" TO   S-NAME.
*
     MOVE     SPACE              TO   GDJ-REC.
     INITIALIZE                       GDJ-REC.
     MOVE     PARA-KENDTS        TO   GDJ-F04.
     MOVE     PARA-TENCDS        TO   GDJ-F05.
     MOVE     PARA-DENKU         TO   GDJ-F06.
     MOVE     ZERO               TO   GDJ-F07.
     MOVE     ZERO               TO   GDJ-F08.
     START    GDJYURL2  KEY  IS  >=   GDJ-F04  GDJ-F05  GDJ-F06
                                      GDJ-F07  GDJ-F08
              INVALID
              MOVE    "END"      TO   END-FLG
     END-START.
*
 GDJYURL2-START-EXIT.
     EXIT.
*
****************************************************************
*    受領累積データ読込み
****************************************************************
 GDJYURL2-RD-SEC             SECTION.
*
     MOVE    "GDJYURL2-RD-SEC"    TO   S-NAME.
*
     READ     GDJYURL2
          AT END
              MOVE     "END"      TO        END-FLG
              GO                  TO        GDJYURL2-RD-EXIT
          NOT  AT  END
              ADD       1         TO        READ-CNT
     END-READ.
*検品日範囲チェック
     IF     ( PARA-KENDTS >  GDJ-F04 )
        OR  ( PARA-KENDTE <  GDJ-F04 )
              MOVE     "END"      TO        END-FLG
              GO                  TO        GDJYURL2-RD-EXIT
     END-IF.
*
*店舗範囲チェック
     IF     ( PARA-TENCDS >  GDJ-F05 )
        OR  ( PARA-TENCDE <  GDJ-F05 )
*             MOVE     "END"      TO        END-FLG
              GO                  TO        GDJYURL2-RD-SEC
     END-IF.
*
*伝票区分チェック
     IF       PARA-DENKU   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-DENKU =  GDJ-F06
                   CONTINUE
              ELSE
*                  MOVE   "END"   TO        END-FLG
                   GO             TO        GDJYURL2-RD-SEC
              END-IF
     END-IF.
*
 GDJYURL2-RD-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
*初期化
     MOVE     SPACE               TO   CSV-REC.
     INITIALIZE                        CSV-REC.
*固定項目転送
     MOVE     ","                 TO   CSV-A01 CSV-A02 CSV-A03
                                       CSV-A04 CSV-A05 CSV-A06
                                       CSV-A07 CSV-A08 CSV-A09
                                       CSV-A10 CSV-A11 CSV-A12
                                       CSV-A13 CSV-A14 CSV-A15
                                       CSV-A16 CSV-A17 CSV-A18
                                       CSV-A19.
*
     MOVE     X"28"               TO   CSV-A021 CSV-A041 CSV-A171
                                       CSV-A181 CSV-A201.
     MOVE     X"29"               TO   CSV-A022 CSV-A042 CSV-A172
                                       CSV-A182 CSV-A202.
*レコード項目転送
*   店舗CD
     MOVE     GDJ-F05             TO   CSV-F01.
*   店舗名
     MOVE     GDJ-A12             TO   CSV-F02.
*   伝票区分
     MOVE     GDJ-F06             TO   CSV-F03.
*   伝票区分名
     EVALUATE GDJ-F06
        WHEN  "01"
              MOVE  NC"定番発注"  TO   CSV-F04
        WHEN  "02"
              MOVE  NC"特売発注"  TO   CSV-F04
        WHEN  "03"
              MOVE  NC"新店定番"  TO   CSV-F04
        WHEN  "04"
              MOVE  NC"新店特売"  TO   CSV-F04
        WHEN  "05"
              MOVE  NC"手書発注"  TO   CSV-F04
        WHEN  "07"
              MOVE  NC"返品　　"  TO   CSV-F04
        WHEN  "08"
              MOVE  NC"値引　　"  TO   CSV-F04
        WHEN  "09"
              MOVE  NC"経費　　"  TO   CSV-F04
        WHEN  OTHER
              MOVE  NC"？？？？"  TO   CSV-F04
     END-EVALUATE.
*   伝票番号
     MOVE     GDJ-F07             TO   CSV-F05.
*   行番号
     MOVE     GDJ-F08             TO   CSV-F06.
*   検品日／返品日
     MOVE     GDJ-F04             TO   CSV-F07.
*   ＪＡＮＣＤ
     MOVE     GDJ-B04             TO   CSV-F08.
*   符号(検品数)
     MOVE     GDJ-B05             TO   CSV-F09.
*   検品数
     MOVE     GDJ-B06             TO   CSV-F10.
*   原価単価
*    MOVE     GDJ-B07             TO   CSV-F11.
     MOVE     GDJ-B07             TO   HENKAN1.
     MOVE     HENKAN1-1           TO   HENKAN1-DATA1.
     MOVE     "."                 TO   HENKAN1-DATA2.
     MOVE     HENKAN1-2           TO   HENKAN1-DATA3.
     MOVE     HENKAN1-DATA        TO   CSV-F11.
*   売価単価
     MOVE     GDJ-B08             TO   CSV-F12.
*   符号(原価金額)
     MOVE     GDJ-B09             TO   CSV-F13.
*   原価金額
     MOVE     GDJ-B10             TO   CSV-F14.
*   符号(売価金額)
     MOVE     GDJ-B11             TO   CSV-F15.
*   売価金額
     MOVE     GDJ-B12             TO   CSV-F16.
*   商品名（上段）
     MOVE     GDJ-B13             TO   CSV-F17.
*   商品名（下段）
     MOVE     GDJ-B14             TO   CSV-F18.
*   返品理由
     MOVE     GDJ-B15             TO   CSV-F19.
*   返品理由名
     EVALUATE GDJ-B15
        WHEN  "01"
              MOVE  NC"不良品　　　　　　　"  TO   CSV-F20
        WHEN  "02"
              MOVE  NC"発注量超え　　　　　"  TO   CSV-F20
        WHEN  "03"
              MOVE  NC"商品違い　　　　　　"  TO   CSV-F20
        WHEN  "04"
              MOVE  NC"返品確認　　　　　　"  TO   CSV-F20
        WHEN  "05"
              MOVE  NC"その他（仕入違原価違"  TO   CSV-F20
        WHEN  OTHER
              MOVE  NC"　　　　　　　　　　"  TO   CSV-F20
     END-EVALUATE.
*
     MOVE     CSV-REC             TO   OUT-REC.
     WRITE    OUT-REC.
     ADD      1                   TO   OUTPUT-CNT.
*    次レコード読込み
     PERFORM  GDJYURL2-RD-SEC.
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
*    IF       OUTPUT-CNT  >  ZERO
*             MOVE    SPACE       TO   CSV-REC
*             MOVE NC"店舗合計"   TO   GOK-01
*             IF      WK-TEN-HATYU < ZERO
*                     MOVE "-"         TO   GOK-021
*                     COMPUTE GOK-02 = WK-TEN-HATYU * -1
*             ELSE
*                     MOVE "0"         TO   GOK-021
*                     MOVE WK-TEN-HATYU TO  GOK-02
*             END-IF
**************MOVE    WK-TEN-KENSYU    TO   GOK-03
*             IF      WK-TEN-KENSYU < ZERO
*                     MOVE "-"         TO   GOK-031
*                     COMPUTE GOK-03 = WK-TEN-KENSYU * -1
*             ELSE
*                     MOVE "0"         TO   GOK-031
*                     MOVE WK-TEN-KENSYU TO GOK-03
*             END-IF
*             IF      WK-TEN-GENKIN < ZERO
*                     MOVE "-"         TO   GOK-041
*                     COMPUTE GOK-04 = WK-TEN-GENKIN * -1
*             ELSE
*                     MOVE "0"         TO   GOK-041
*                     MOVE WK-TEN-GENKIN TO GOK-04
*             END-IF
*             MOVE    GOK-REC     TO   CSV-REC
*             WRITE   CSV-REC
*             ADD     1           TO   OUTPUT-CNT
*             MOVE    ZERO        TO   WK-TEN-GOKEI
**************MOVE    SPACE       TO   GGK-REC
*             MOVE NC"総合計　"   TO   GGK-01
*             IF      WK-GENKIN < ZERO
*                     MOVE "-"         TO   GGK-041
*                     COMPUTE GGK-04 = WK-GENKIN * -1
*             ELSE
*                     MOVE "0"         TO   GGK-041
*                     MOVE WK-GENKIN   TO   GGK-04
*             END-IF
*             MOVE    GGK-REC     TO   CSV-REC
*             WRITE   CSV-REC
*             ADD     1           TO   OUTPUT-CNT
*    END-IF.
*
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
*
     CLOSE    GDJYURL2   GDJYRCSV.
*
 END-EXIT.
     EXIT.
********************<<  PUROGRAM  END  >>*************************
