# SSY0807V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY0807V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＷｉｎＡｃｔｏｒ導入　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ受領　　　        *
*    モジュール名　　　　：　オンラインデータ欠品ＣＳＶ出力　　*
*    作成日／更新日　　　：　20/03/18                          *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　累積欠品データより欠品データをＣ　*
*                            ＳＶ出力する。　　　　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY0807V.
 AUTHOR.               NAV.
 DATE-WRITTEN.         20/03/18.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     YA           IS   CHR-2
     YB-21        IS   CHR-21
     YB           IS   CHR-15
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*欠品ファイル
     SELECT  ONLKEPF   ASSIGN    TO        ONLKEPL2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KPN-F08
                                           KPN-F01
                                           KPN-F02
                                           KPN-F03
                                           KPN-F04
                                           KPN-F05
                                           KPN-F06
                                           KPN-F07
                       FILE      STATUS    KPN-ST.
*条件ファイル
     SELECT  HJYOKEN   ASSIGN    TO        JYOKEN1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       JYO-F01  JYO-F02
                       FILE      STATUS    JYO-ST.
*
*累積欠品ＣＳＶデータ
     SELECT  ONLKPCSV  ASSIGN    TO        ONLKPCSV
                       STATUS              CSV-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 欠品ファイル                     *
****************************************************************
 FD  ONLKEPF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ONLKEPF   OF   XFDLIB
                       JOINING   KPN       AS   PREFIX.
****************************************************************
*    FILE = 条件ファイル                                       *
****************************************************************
 FD  HJYOKEN
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HJYOKEN   OF   XFDLIB
                       JOINING   JYO       AS   PREFIX.
****************************************************************
*    FILE = 累積欠品ＣＳＶデータ                               *
****************************************************************
 FD  ONLKPCSV           BLOCK CONTAINS 1    RECORDS.
 01  KED-REC.
     03  FILLER         PIC       X(500).
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*欠品ＣＳＶデータ
     COPY   ONLKPCSV OF XFDLIB  JOINING   CSV  AS   PREFIX.
*ステータス領域
 01  STATUS-AREA.
     03  KPN-ST                   PIC  X(02).
     03  JYO-ST                   PIC  X(02).
     03  CSV-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
 01  READ-CNT                     PIC  9(08)  VALUE  0.
 01  DTWT-CNT                     PIC  9(08)  VALUE  0.
 01  HJYOKEN-INV-FLG              PIC  X(03)  VALUE  SPACE.
 01  HTENMS-INV-FLG               PIC  X(03)  VALUE  SPACE.
 01  ZSOKMS-INV-FLG               PIC  X(03)  VALUE  SPACE.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*
 01  WK-KEIRI                     PIC  9(02)  VALUE  ZERO.
 01  WK-KEIRI-ST                  PIC  9(08)  VALUE  ZERO.
 01  WK-KEIRI-ED                  PIC  9(08)  VALUE  ZERO.
 01  WK-SIMEBI-KEISAN             PIC S9(02)  VALUE  ZERO.
 01  WK-SIMEBI.
     03  WK-SIMEBI-S              PIC  9(08).
     03  WK-SIMEBI-SR             REDEFINES  WK-SIMEBI-S.
         05  WK-SIMEBIS-YY        PIC  9(04).
         05  WK-SIMEBIS-MM        PIC  9(02).
         05  WK-SIMEBIS-DD        PIC  9(02).
     03  WK-SIMEBI-E              PIC  9(08).
     03  WK-SIMEBI-ER             REDEFINES  WK-SIMEBI-E.
         05  WK-SIMEBIE-YY        PIC  9(04).
         05  WK-SIMEBIE-MM        PIC  9(02).
         05  WK-SIMEBIE-DD        PIC  9(02).
*
 01  WK-TAITL.
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(05)  VALUE  NC"バッチ日付".
     03 FILLER         PIC  X(01)  VALUE  X"29".
     03 FILLER         PIC  X(01)  VALUE  ",".
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(05)  VALUE  NC"バッチ時刻".
     03 FILLER         PIC  X(01)  VALUE  X"29".
     03 FILLER         PIC  X(01)  VALUE  ",".
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(06)  VALUE  NC"バッチ取引先".
     03 FILLER         PIC  X(01)  VALUE  X"29".
     03 FILLER         PIC  X(01)  VALUE  ",".
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(04)  VALUE  NC"倉庫ＣＤ".
     03 FILLER         PIC  X(01)  VALUE  X"29".
     03 FILLER         PIC  X(01)  VALUE  ",".
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(04)  VALUE  NC"店舗ＣＤ".
     03 FILLER         PIC  X(01)  VALUE  X"29".
     03 FILLER         PIC  X(01)  VALUE  ",".
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(04)  VALUE  NC"伝票番号".
     03 FILLER         PIC  X(01)  VALUE  X"29".
     03 FILLER         PIC  X(01)  VALUE  ",".
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(03)  VALUE  NC"行番号".
     03 FILLER         PIC  X(01)  VALUE  X"29".
     03 FILLER         PIC  X(01)  VALUE  ",".
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(03)  VALUE  NC"納品日".
     03 FILLER         PIC  X(01)  VALUE  X"29".
     03 FILLER         PIC  X(01)  VALUE  ",".
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(05)  VALUE  NC"ＪＡＮＣＤ".
     03 FILLER         PIC  X(01)  VALUE  X"29".
     03 FILLER         PIC  X(01)  VALUE  ",".
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(07)  VALUE  NC"サカタ商品ＣＤ".
     03 FILLER         PIC  X(01)  VALUE  X"29".
     03 FILLER         PIC  X(01)  VALUE  ",".
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(07)  VALUE  NC"サカタ品単ＣＤ".
     03 FILLER         PIC  X(01)  VALUE  X"29".
     03 FILLER         PIC  X(01)  VALUE  ",".
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(04)  VALUE  NC"商品名１".
     03 FILLER         PIC  X(01)  VALUE  X"29".
     03 FILLER         PIC  X(01)  VALUE  ",".
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(04)  VALUE  NC"商品名２".
     03 FILLER         PIC  X(01)  VALUE  X"29".
     03 FILLER         PIC  X(01)  VALUE  ",".
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(03)  VALUE  NC"発注数".
     03 FILLER         PIC  X(01)  VALUE  X"29".
     03 FILLER         PIC  X(01)  VALUE  ",".
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(03)  VALUE  NC"出荷数".
     03 FILLER         PIC  X(01)  VALUE  X"29".
     03 FILLER         PIC  X(01)  VALUE  ",".
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(03)  VALUE  NC"欠品数".
     03 FILLER         PIC  X(01)  VALUE  X"29".
     03 FILLER         PIC  X(01)  VALUE  ",".
     03 FILLER         PIC  X(01)  VALUE  X"28".
     03 FILLER         PIC  N(07)  VALUE  NC"ＤＣＭ伝票番号".
     03 FILLER         PIC  X(01)  VALUE  X"29".
*
 01  FILE-ERR.
     03  KPN-ERR           PIC N(20) VALUE
                        NC"累積欠品ファイルエラー".
     03  JYO-ERR           PIC N(20) VALUE
                        NC"条件ファイルエラー".
     03  CSV-ERR           PIC N(20) VALUE
                        NC"累積欠品ＣＳＶエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 KPN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ONLKEPF.
     DISPLAY     KPN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KPN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     JYO-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 CSV-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ONLKPCSV.
     DISPLAY     CSV-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     CSV-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*                   MAIN       MODULE                3.0       *
****************************************************************
 PROCESS-SEC             SECTION.
     MOVE     "PROCESS-SEC" TO   S-NAME.
     DISPLAY  "***   SSY0807V   START    ***"  UPON CONS.
*
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC   UNTIL     END-FLG  =   "END".
     PERFORM   END-SEC.
     STOP      RUN.
*
 PROCESS-EXIT.
     EXIT.
****************************************************************
*             ＰＲＴファイル初期処理                 4.0       *
****************************************************************
 INIT-SEC                SECTION.
     MOVE     "INIT-SEC"    TO   S-NAME.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
*ファイルのＯＰＥＮ
     OPEN     INPUT     ONLKEPF  HJYOKEN.
     OPEN     OUTPUT    ONLKPCSV.
*ワークの初期化
     INITIALIZE         FLG-AREA  WK-SIMEBI.
*条件Ｆ（経理月）
     MOVE     "58"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       HJYOKEN-INV-FLG  =  "INV"
              DISPLAY   "HJYOKEN INV KEY=58"  UPON CONS
              MOVE      "END"     TO   END-FLG
              GO   TO   INIT-EXIT
     END-IF.
     MOVE     JYO-F04        TO   WK-KEIRI.
*条件Ｆ（ＡＣＯＳ用締日）
     MOVE     "99"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       HJYOKEN-INV-FLG  =  "INV"
              DISPLAY   "HJYOKEN INV KEY=99"  UPON CONS
              MOVE      "END"     TO   END-FLG
              GO   TO   INIT-EXIT
     END-IF.
     EVALUATE WK-KEIRI
         WHEN 1
              MOVE JYO-F04   TO   WK-SIMEBI-S  WK-SIMEBI-E
         WHEN 2
              MOVE JYO-F05   TO   WK-SIMEBI-S  WK-SIMEBI-E
         WHEN 3
              MOVE JYO-F06   TO   WK-SIMEBI-S  WK-SIMEBI-E
         WHEN 4
              MOVE JYO-F07   TO   WK-SIMEBI-S  WK-SIMEBI-E
         WHEN 5
              MOVE JYO-F08   TO   WK-SIMEBI-S  WK-SIMEBI-E
         WHEN 6
              MOVE JYO-F09   TO   WK-SIMEBI-S  WK-SIMEBI-E
         WHEN 7
              MOVE JYO-F10   TO   WK-SIMEBI-S  WK-SIMEBI-E
         WHEN 8
              MOVE JYO-F11   TO   WK-SIMEBI-S  WK-SIMEBI-E
         WHEN 9
              MOVE JYO-F12   TO   WK-SIMEBI-S  WK-SIMEBI-E
         WHEN 10
              MOVE JYO-F12A  TO   WK-SIMEBI-S  WK-SIMEBI-E
         WHEN 11
              MOVE JYO-F12B  TO   WK-SIMEBI-S  WK-SIMEBI-E
         WHEN 12
              MOVE JYO-F12C  TO   WK-SIMEBI-S  WK-SIMEBI-E
         WHEN OTHER
              DISPLAY  "### ｹﾞﾂﾄﾞ ｲｼﾞｮｳ  ｹﾞﾂﾄﾞ= " WK-KEIRI " ###"
                                       UPON CONS
              MOVE      "END"     TO   END-FLG
              GO   TO   INIT-EXIT
     END-EVALUATE.
*
     DISPLAY NC"＃経理月" " = " WK-KEIRI     UPON  CONS.
     DISPLAY NC"＃締基準" " = " WK-SIMEBI-S  UPON  CONS.
*
     MOVE    01                 TO     WK-SIMEBIS-DD.
     MOVE    99                 TO     WK-SIMEBIE-DD.
     MOVE    WK-SIMEBIS-MM      TO     WK-SIMEBI-KEISAN.
     COMPUTE WK-SIMEBI-KEISAN = WK-SIMEBI-KEISAN  -  3.
     IF  WK-SIMEBI-KEISAN   <  0
         COMPUTE  WK-SIMEBIS-YY = WK-SIMEBIS-YY - 1
         COMPUTE  WK-SIMEBIS-MM = 12 + WK-SIMEBI-KEISAN
     ELSE
         COMPUTE WK-SIMEBIS-MM = WK-SIMEBI-KEISAN + 1
     END-IF.
     MOVE    WK-SIMEBI-S        TO     WK-KEIRI-ST.
     MOVE    WK-SIMEBI-E        TO     WK-KEIRI-ED.
     DISPLAY "HANI = " WK-KEIRI-ST " - " WK-KEIRI-ED UPON CONS.
*
     MOVE    SPACE              TO     KPN-REC.
     INITIALIZE                        KPN-REC.
     MOVE    WK-KEIRI-ST        TO     KPN-F08.
     START ONLKEPF KEY IS >=  KPN-F08  KPN-F01  KPN-F02  KPN-F03
                              KPN-F04  KPN-F05  KPN-F06  KPN-F07
     INVALID
             MOVE  "END"        TO     END-FLG
             DISPLAY NC"＃対象データ無し！　ＳＴ" UPON CONS
             GO                 TO     INIT-EXIT
     END-START.
*
     PERFORM  KPN-READ-SEC.
     IF  END-FLG  =  "END"
         MOVE  "END"        TO     END-FLG
         DISPLAY NC"＃対象データ無し！　初Ｒ" UPON CONS
     ELSE
         MOVE   SPACE       TO     CSV-REC
         INITIALIZE                CSV-REC
         MOVE   WK-TAITL    TO     KED-REC
         WRITE  KED-REC
         ADD    1           TO     DTWT-CNT
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             ＰＲＴファイルメイン処理               5.0       *
****************************************************************
 MAIN-SEC                SECTION.
     MOVE     "MAIN-SEC"    TO   S-NAME.
*欠品出力用ファイルを出力する。
     MOVE     SPACE         TO   CSV-REC
     INITIALIZE                  CSV-REC.
*項目セット
     MOVE     KPN-F01       TO   CSV-F01.
     MOVE     KPN-F02       TO   CSV-F02.
     MOVE     KPN-F03       TO   CSV-F03.
     MOVE     KPN-F04       TO   CSV-F04.
     MOVE     KPN-F05       TO   CSV-F05.
     MOVE     KPN-F06       TO   CSV-F06.
     MOVE     KPN-F07       TO   CSV-F07.
     MOVE     KPN-F08       TO   CSV-F08.
     MOVE     KPN-F09       TO   CSV-F09.
     MOVE     KPN-F10       TO   CSV-F10.
     MOVE     KPN-F11       TO   CSV-F11.
     MOVE     KPN-F12       TO   CSV-F12.
     MOVE     KPN-F13       TO   CSV-F13.
     MOVE     KPN-F14       TO   CSV-F14.
     MOVE     KPN-F15       TO   CSV-F15.
     MOVE     KPN-F16       TO   CSV-F16.
     MOVE     KPN-F17       TO   CSV-F17.
*カンマセット
     MOVE    "," TO CSV-A01 CSV-A02 CSV-A03 CSV-A04 CSV-A05
                    CSV-A06 CSV-A07 CSV-A08 CSV-A09 CSV-A10
                    CSV-A11 CSV-A12 CSV-A13 CSV-A14 CSV-A15
                    CSV-A16.
*
     MOVE     CSV-REC       TO   KED-REC.
     WRITE    KED-REC.
     ADD      1             TO   DTWT-CNT.
*
     PERFORM  KPN-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             　欠品ファイルＲＥＡＤ　                       *
****************************************************************
 KPN-READ-SEC          SECTION.
     MOVE     "KPN-READ-SEC"      TO   S-NAME.
*
     READ     ONLKEPF       AT    END
              MOVE         "END"  TO   END-FLG
              GO   TO   KPN-READ-EXIT
         NOT  AT   END
              ADD        1  TO    READ-CNT
     END-READ.
*
     IF       READ-CNT(6:3)  =  "000"  OR "500"
              DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*
     IF  WK-KEIRI-ED  <  KPN-F08
         MOVE "END"               TO   END-FLG
     END-IF.
*
 KPN-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               6.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             ONLKEPF  HJYOKEN  ONLKPCSV.
     DISPLAY  "*** READ-CNT  = " READ-CNT " *** " UPON CONS.
     DISPLAY  "*** DTWT-CNT  = " DTWT-CNT " *** " UPON CONS.
     DISPLAY  "***   SSY0807V   END      ***"  UPON CONS.
**
 END-EXIT.
     EXIT.
****************************************************************
*    LEVEL  ALL   条件ファイル　　　ＲＥＡＤ　　　　　　　　　 *
****************************************************************
 JYO-RD-RTN           SECTION.
*
     READ     HJYOKEN
              INVALID     MOVE "INV"  TO  HJYOKEN-INV-FLG
              NOT INVALID MOVE SPACE  TO  HJYOKEN-INV-FLG
     END-READ.
*
 JYO-RD-EXIT.
     EXIT.
*****************<<  SSY0807V   END PROGRAM  >>******************

```
