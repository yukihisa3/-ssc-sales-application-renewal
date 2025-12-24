# SSY7510V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY7510V.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭサンワ　　　　　　　　　　　*
*    モジュール名　　　　：　受領書発行　　　　　　　　　　　　*
*    作成日／更新日　　　：　2015/12/07                        *
*    作成者／更新者　　　：　ＮＡＶ宮下　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＤＣＭサンワ受領よりＤＣＭサンワ  *
*                        ：　受領書のＣＳＶを出力する　　　　  *
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.               SSY7510V.
 AUTHOR.                   NAV.
 DATE-WRITTEN.             2015/12/07.
****************************************************************
 ENVIRONMENT               DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
****************************************************************
 INPUT-OUTPUT           SECTION.
****************************************************************
 FILE-CONTROL.
*----<< ＤＣＭサンワ受領ワーク >>--*
     SELECT     SWJYRWF    ASSIGN    TO        DA-01-VI-SWJYRWL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       WK-F01
                                               WK-F03
                                               WK-F05
                                               WK-F06
                                               WK-F07
                           FILE      STATUS    WK-ST.
*----<< ＣＳＶワーク >>--*
     SELECT   SWJYURWK     ASSIGN    TO  DA-01-S-SWJYURWK
                           FILE      STATUS    CWK-ST.
*
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< ＤＣＭサンワ受領ワーク >>--*
 FD  SWJYRWF.
     COPY        SWJYRWF   OF        XFDLIB
     JOINING     WK        AS        PREFIX.
*
*----<< ＣＳＶワーク >>--*
 FD  SWJYURWK           BLOCK        CONTAINS  1    RECORDS.
*
 01 CSV-REC.
    03  FILLER                    PIC  X(1000).
******************************************************************
 WORKING-STORAGE        SECTION.
*
*----<< ファイルステータス >>--*
 01  FILE-STATUS.
     03  WK-ST                    PIC  X(02).
     03  CWK-ST                   PIC  X(02).
*
*----<< 変数 >>--*
 01  WK-AREA.
     03  END-FLG                  PIC  X(03)     VALUE  SPACE.
     03  RD-CNT                   PIC  9(08)     VALUE  ZERO.
     03  WRT-CNT                  PIC  9(08)     VALUE  ZERO.
*
*----<< システム日付 >>--*
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC  9(06)     VALUE  ZERO.
     03  SYS-DATEW                PIC  9(08)     VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES      SYS-DATEW.
         05  SYS-YY               PIC  9(04).
         05  SYS-MM               PIC  9(02).
         05  SYS-DD               PIC  9(02).
*
*----<< システム時刻 >>--*
 01  SYS-TIME                     PIC  9(08).
 01  FILLER                       REDEFINES      SYS-TIME.
     03  SYS-HHMMSS               PIC  9(06).
     03  SYS-MS                   PIC  9(02).
*
*----<< 日付変換サブルーチン用 >>--*
 01  LINK-AREA.
     03  LINK-IN-KBN              PIC  X(01).
     03  LINK-IN-YMD6             PIC  9(06).
     03  LINK-IN-YMD8             PIC  9(08).
     03  LINK-OUT-RET             PIC  X(01).
     03  LINK-OUT-YMD8            PIC  9(08).
*
*----<< ＣＳＶ小数点編集用 >>--*
 01  WK-HENSYU1.
     03  WK-6-KETA                PIC  9(05)V9(01).
     03  WK-6-KETA-R              REDEFINES      WK-6-KETA.
         05  WK-5-KETA            PIC  9(05).
         05  WK-1-KETA            PIC  9(01).
 01  WK-HENSYU2.
     03  WK-9-KETA                PIC  9(07)V9(02).
     03  WK-9-KETA-R              REDEFINES      WK-9-KETA.
         05  WK-7-KETA            PIC  9(07).
         05  WK-2-KETA            PIC  9(02).
*
*----<< ＣＳＶファイル >>--*
 01  WK-CSV-HEAD0.
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(13)
                     VALUE NC"＜ＤＣＭサンワ受領データ＞".
     03  FILLER PIC  X(01)  VALUE X"29".
*
 01  WK-CSV-HEAD1.
     03  FILLER PIC  X(01)  VALUE X"28".
     03  TAITL1 PIC  N(03).
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  TAITL2 PIC  X(10).
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  TAITL3 PIC  N(01).
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  TAITL4 PIC  X(10).
*
 01  WK-CSV-HEAD2.
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(06)  VALUE NC"取引先コード".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"取引先名".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(05)  VALUE NC"店舗コード".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(03)  VALUE NC"店舗名".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(03)  VALUE NC"検収日".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(06)  VALUE NC"納品伝票番号".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(07)  VALUE NC"納品伝票行番号".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(06)  VALUE NC"ＪＡＮコード".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"商品名１".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"商品名２".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"発注数量".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"検収数量".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"欠品数量".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"原価単価".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"売価単価".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(06)  VALUE NC"発注伝票番号".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(07)  VALUE NC"発注伝票行番号".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"伝票区分".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(05)  VALUE NC"伝票区分名".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"商品種別".
     03  FILLER PIC  X(01)  VALUE X"29".
*
 01  WK-CSV-MEISAI.
     03  CSV-F01                  PIC  9(08).
     03  CSV-A01                  PIC  X(01)     VALUE  ",".
     03  CSV-C01                  PIC  X(01)     VALUE X"28".
     03  CSV-F02                  PIC  N(15).
     03  CSV-D01                  PIC  X(01)     VALUE X"29".
     03  CSV-A02                  PIC  X(01)     VALUE  ",".
     03  CSV-F03                  PIC  9(05).
     03  CSV-A03                  PIC  X(01)     VALUE  ",".
     03  CSV-C02                  PIC  X(01)     VALUE X"28".
     03  CSV-F04                  PIC  N(15).
     03  CSV-D02                  PIC  X(01)     VALUE X"29".
     03  CSV-A04                  PIC  X(01)     VALUE  ",".
     03  CSV-F05                  PIC  9(08).
     03  CSV-A05                  PIC  X(01)     VALUE  ",".
     03  CSV-F06                  PIC  9(08).
     03  CSV-A06                  PIC  X(01)     VALUE  ",".
     03  CSV-F07                  PIC  9(02).
     03  CSV-A07                  PIC  X(01)     VALUE  ",".
     03  CSV-F08                  PIC  X(13).
     03  CSV-A08                  PIC  X(01)     VALUE  ",".
     03  CSV-F09                  PIC  X(20).
     03  CSV-A09                  PIC  X(01)     VALUE  ",".
     03  CSV-F10                  PIC  X(20).
     03  CSV-A10                  PIC  X(01)     VALUE  ",".
     03  CSV-F111                 PIC  9(05).
     03  CSV-B01                  PIC  X(01)     VALUE  ".".
     03  CSV-F112                 PIC  9(01).
     03  CSV-A11                  PIC  X(01)     VALUE  ",".
     03  CSV-F121                 PIC  9(05).
     03  CSV-B02                  PIC  X(01)     VALUE  ".".
     03  CSV-F122                 PIC  9(01).
     03  CSV-A12                  PIC  X(01)     VALUE  ",".
     03  CSV-F131                 PIC  9(05).
     03  CSV-B03                  PIC  X(01)     VALUE  ".".
     03  CSV-F132                 PIC  9(01).
     03  CSV-A13                  PIC  X(01)     VALUE  ",".
     03  CSV-F141                 PIC  9(07).
     03  CSV-B04                  PIC  X(01)     VALUE  ".".
     03  CSV-F142                 PIC  9(02).
     03  CSV-A14                  PIC  X(01)     VALUE  ",".
     03  CSV-F15                  PIC  9(07).
     03  CSV-A15                  PIC  X(01)     VALUE  ",".
     03  CSV-F16                  PIC  9(08).
     03  CSV-A16                  PIC  X(01)     VALUE  ",".
     03  CSV-F17                  PIC  9(02).
     03  CSV-A17                  PIC  X(01)     VALUE  ",".
     03  CSV-F18                  PIC  9(02).
     03  CSV-A18                  PIC  X(01)     VALUE  ",".
     03  CSV-C03                  PIC  X(01)     VALUE X"28".
     03  CSV-F19                  PIC  N(04).
     03  CSV-D03                  PIC  X(01)     VALUE X"29".
     03  CSV-A19                  PIC  X(01)     VALUE  ",".
     03  CSV-F20                  PIC  9(01).
*
***  セクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*メッセージ出力
 01  FILE-ERR.
     03  WK-ERR                   PIC  N(20)     VALUE
         NC"受領書ワークエラ－".
     03  CWK-ERR                  PIC  N(20)     VALUE
         NC"受領書ＣＳＶワークエラ－".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SSY7510V".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SSY7510V".
         05  FILLER               PIC  X(11)     VALUE
                                        " END   *** ".
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION.
 DECLARATIVES.
 WK-ERR                     SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  SWJYRWF.
     DISPLAY       WK-ERR   UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       WK-ST    UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 CWK-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  SWJYURWK.
     DISPLAY       CWK-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       CWK-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 END       DECLARATIVES.
******************************************************************
*                                                                *
******************************************************************
 CONTROL-SUB           SECTION.
*
     MOVE     "CONTROL-SUB"       TO    S-NAME.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =    "END".
     PERFORM  END-SEC.
 CONTROL-EXIT.
     EXIT.
*
******************************************************************
*             初期処理　　　　　　　　　　　　　　　　　　　　 *
******************************************************************
 INIT-SEC               SECTION.
*
*----<< 処理名セット >>--*
     MOVE    "INIT-SEC"           TO    S-NAME.
*
*----<< 初期メッセージ出力 >>--*
     DISPLAY  MSG-START UPON CONS.
*
*----<<FILE OPEN >>--*
     OPEN     INPUT     SWJYRWF.
     OPEN     OUTPUT    SWJYURWK.
*
*----<< 変数クリア >>-*
     MOVE     SPACE               TO        END-FLG.
     MOVE     ZERO                TO        RD-CNT
                                            WRT-CNT.
*
*----<< システム日付取得 >>--*
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
              MOVE      ZERO           TO   SYS-DATEW
     END-IF.
*
*----<< システム時刻取得 >>--*
     ACCEPT   SYS-TIME  FROM      TIME.
*
*----<< １レコード目読込 >>--*
     PERFORM  WK-READ-SEC.
*
*終了チェック（存在した場合は、タイトル行出力）
     IF  END-FLG  NOT =  "END"
*********ヘッダタイトルセット
         MOVE   SPACE             TO        CSV-REC
         MOVE   WK-CSV-HEAD0      TO        CSV-REC
         WRITE  CSV-REC
         ADD    1                 TO        WRT-CNT
*********受信日／検収日範囲セット
         MOVE   SPACE             TO        TAITL1  TAITL2
                                            TAITL3  TAITL4
         IF  WK-F96  =  1
             MOVE NC"受信日"      TO        TAITL1
             MOVE WK-F97(1:4)     TO        TAITL2(1:4)
             MOVE "/"             TO        TAITL2(5:1)
             MOVE WK-F97(5:2)     TO        TAITL2(6:2)
             MOVE "/"             TO        TAITL2(8:1)
             MOVE WK-F97(7:2)     TO        TAITL2(9:2)
         ELSE
             IF  WK-F96  =  2
                 MOVE NC"検収日"  TO        TAITL1
                 MOVE WK-F98(1:4) TO        TAITL2(1:4)
                 MOVE "/"         TO        TAITL2(5:1)
                 MOVE WK-F98(5:2) TO        TAITL2(6:2)
                 MOVE "/"         TO        TAITL2(8:1)
                 MOVE WK-F98(7:2) TO        TAITL2(9:2)
                 MOVE NC"～"      TO        TAITL3
                 MOVE WK-F99(1:4) TO        TAITL4(1:4)
                 MOVE "/"         TO        TAITL4(5:1)
                 MOVE WK-F99(5:2) TO        TAITL4(6:2)
                 MOVE "/"         TO        TAITL4(8:1)
                 MOVE WK-F99(7:2) TO        TAITL4(9:2)
             ELSE
                 MOVE ALL NC"＊"  TO        TAITL1
                 MOVE ALL "*"     TO        TAITL2
                 MOVE ALL NC"＊"  TO        TAITL3
                 MOVE ALL "*"     TO        TAITL4
             END-IF
         END-IF
         MOVE   SPACE             TO        CSV-REC
         MOVE   WK-CSV-HEAD1      TO        CSV-REC
         WRITE  CSV-REC
         ADD    1                 TO        WRT-CNT
*********ヘッダタイトルセット
         MOVE   SPACE             TO        CSV-REC
         MOVE   WK-CSV-HEAD2      TO        CSV-REC
         WRITE  CSV-REC
         ADD    1                 TO        WRT-CNT
     END-IF.
*
 INIT-EXIT.
     EXIT.
******************************************************************
*              メイン処理　　　　　　　　　　　　　　　　　　　*
******************************************************************
 MAIN-SEC     SECTION.
*
*----<< 処理名セット >>--*
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
*----<< ＣＳＶワーククリア >>--*
     MOVE      SPACE              TO   WK-CSV-MEISAI.
     INITIALIZE                        WK-CSV-MEISAI.
*
*----<< 区切文字（カンマ）セット >>--*
     MOVE      ","                TO   CSV-A01  CSV-A02  CSV-A03
                                       CSV-A04  CSV-A05  CSV-A06
                                       CSV-A07  CSV-A08  CSV-A09
                                       CSV-A10  CSV-A11  CSV-A12
                                       CSV-A13  CSV-A14  CSV-A15
                                       CSV-A16  CSV-A17  CSV-A18
                                       CSV-A19.
*
*----<< 制御文字セット >>--*
     MOVE      X"28"              TO   CSV-C01  CSV-C02  CSV-C03.
     MOVE      X"29"              TO   CSV-D01  CSV-D02  CSV-D03.
*
*----<< 明細セット >>--*
     MOVE      WK-F01             TO   CSV-F01.
     MOVE      WK-F02             TO   CSV-F02.
     MOVE      WK-F03             TO   CSV-F03.
     MOVE      WK-F04             TO   CSV-F04.
     MOVE      WK-F05             TO   CSV-F05.
     MOVE      WK-F06             TO   CSV-F06.
     MOVE      WK-F07             TO   CSV-F07.
     MOVE      WK-F08             TO   CSV-F08.
     MOVE      WK-F09             TO   CSV-F09.
     MOVE      WK-F10             TO   CSV-F10.
     MOVE      WK-F11             TO   WK-6-KETA.
     MOVE      WK-5-KETA          TO   CSV-F111.
     MOVE      WK-1-KETA          TO   CSV-F112.
     MOVE      WK-F12             TO   WK-6-KETA.
     MOVE      WK-5-KETA          TO   CSV-F121.
     MOVE      WK-1-KETA          TO   CSV-F122.
     MOVE      WK-F13             TO   WK-6-KETA.
     MOVE      WK-5-KETA          TO   CSV-F131.
     MOVE      WK-1-KETA          TO   CSV-F132.
     MOVE      WK-F14             TO   WK-9-KETA.
     MOVE      WK-7-KETA          TO   CSV-F141.
     MOVE      WK-2-KETA          TO   CSV-F142.
     MOVE      "."                TO   CSV-B01  CSV-B02  CSV-B03
                                       CSV-B04.
     MOVE      WK-F15             TO   CSV-F15.
     MOVE      WK-F16             TO   CSV-F16.
     MOVE      WK-F17             TO   CSV-F17.
     MOVE      WK-F18             TO   CSV-F18.
     MOVE      WK-F19             TO   CSV-F19.
     MOVE      WK-F20             TO   CSV-F20.
*
*----<< ワーク書込 >>--*
     MOVE      SPACE              TO   CSV-REC.
     MOVE      WK-CSV-MEISAI      TO   CSV-REC.
     WRITE  CSV-REC.
     ADD       1                  TO   WRT-CNT.
*
*----<< 次レコード読込 >>--*
     PERFORM  WK-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
******************************************************************
*              終了処理　　　　　　　　　　　　　　　　　　　　*
******************************************************************
 END-SEC       SECTION.
*
*----<< 処理名セット >>--*
     MOVE     "END-SEC"           TO   S-NAME.
*
*----<<FILE CLOSE >>--*
     CLOSE     SWJYRWF
               SWJYURWK.
*
     DISPLAY   "## RD-CNT  = " RD-CNT  " ##" UPON CONS.
     DISPLAY   "## WRT-CNT = " WRT-CNT " ##" UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*            ワーク読込
****************************************************************
 WK-READ-SEC             SECTION.
*
*----<< 処理名セット >>--*
     MOVE    "WK-READ-SEC"       TO     S-NAME.
*
*----<< 読込 >>--*
     READ     SWJYRWF  AT  END
              MOVE    "END"      TO   END-FLG
              GO                 TO   WK-READ-EXIT
     END-READ.
*
     ADD      1              TO   RD-CNT.
*
 WK-READ-EXIT.
     EXIT.

```
