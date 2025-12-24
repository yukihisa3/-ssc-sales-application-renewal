# SBI0030B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SBI0030B.COB`

## ソースコード

```cobol
**************************************************************
*                                                            *
*    顧客名　　　　　：　（株）サカタのタネ殿　　　　　      *
*    業務名　　　　　：　BIツール連携　　　　　　　　　　　  *
*    モジュール名　　：　朝一：ﾏｽﾀﾃﾞｰﾀ抽出（在庫マスタ）     *
*    作成日／更新日　：　2018/12/14                          *
*    作成者／更新者　：　INOUE     　　　　　　　　　　　　  *
*    処理概要　　　　：　連携在庫マスタデータを出力する　    *
*                                                            *
**************************************************************
 IDENTIFICATION         DIVISION.
**************************************************************
 PROGRAM-ID.            SBI0030B.
 AUTHOR.                NAV.
**************************************************************
 ENVIRONMENT            DIVISION.
**************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PRIMERGY6000.
 OBJECT-COMPUTER.       PRIMERGY6000.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 在庫マスタ                 >>****************************
     SELECT   ZAMZAIL1  ASSIGN  TO   DA-01-VI-ZAMZAIL1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  SEQUENTIAL
                        RECORD  KEY          IS  ZAI-F01
                                                 ZAI-F021
                                                 ZAI-F022
                                                 ZAI-F031
                                                 ZAI-F032
                                                 ZAI-F033
                        FILE    STATUS       IS  ZAI-STATUS.
****<< 連携在庫マスタデータ  >>******************************
     SELECT   BIMSTZAI  ASSIGN  TO   DA-01-S-BIMSTZAI
                        ORGANIZATION         IS  SEQUENTIAL
                        ACCESS  MODE         IS  SEQUENTIAL
                        FILE STATUS          IS  BIM-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< 在庫マスタ　　　　　　　　 >>**************************
 FD    ZAMZAIL1
                LABEL     RECORD    IS   STANDARD.
       COPY     ZAMZAIL1  OF        XFDLIB
                JOINING   ZAI       PREFIX.
****<< 連携在庫マスタデータ　 >>******************************
 FD    BIMSTZAI BLOCK     CONTAINS  11   RECORDS
                LABEL     RECORD    IS   STANDARD.
       COPY     BIMSTZAI  OF        XFDLIB
                JOINING   BIM       PREFIX.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 ZAI-STATUS           PIC  X(2).
     02 BIM-STATUS           PIC  X(2).
****  ワークエリア　          ****
 01  WK-AREA.
     02 WK-STAN.
         03  WK-STAN1        PIC  X(05)  VALUE  SPACE.
         03  WK-STAN2        PIC  X(02)  VALUE  SPACE.
         03  WK-STAN3        PIC  X(01)  VALUE  SPACE.
     02 WK-ETAN.
         03  WK-ETAN1        PIC  X(05)  VALUE  SPACE.
         03  WK-ETAN2        PIC  X(02)  VALUE  SPACE.
         03  WK-ETAN3        PIC  X(01)  VALUE  SPACE.
     02 WK-STANA.
         03  WK-STANA1       PIC  X(03)  VALUE  SPACE.
         03  WK-STANA2       PIC  X(01)  VALUE  SPACE.
         03  WK-STANA3       PIC  X(02)  VALUE  SPACE.
     02 WK-ETANA.
         03  WK-ETANA1       PIC  X(03)  VALUE  SPACE.
         03  WK-ETANA2       PIC  X(01)  VALUE  SPACE.
         03  WK-ETANA3       PIC  X(02)  VALUE  SPACE.
****  カウンタ               ****
 01  ZAMZAIL1-CNT            PIC  9(6)   VALUE  ZERO.
 01  BIMSTZAI-CNT            PIC  9(6)   VALUE  ZERO.
****  フラグ                 ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
*
*変換
*　数量変換
 01  WK-SURYO                PIC  9(08)V9(02).
 01  WK-SURYO-R              REDEFINES   WK-SURYO.
     03  WK-SURYO-1          PIC  9(08).
     03  WK-SURYO-2          PIC  9(02).
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
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBI0030B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                    SECTION.
 01  PAR-OUT-KENSU         PIC 9(06).
*
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION  USING  PAR-OUT-KENSU.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZAMZAIL1.
     MOVE   "ZAMZAIL1"        TO    ERR-FL-ID.
     MOVE    ZAI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  BIMSTZAI.
     MOVE   "BIMSTZAI"        TO    ERR-FL-ID.
     MOVE    BIM-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
 SBI0030B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 SBI0030B-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
*
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
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     OPEN     INPUT     ZAMZAIL1.
     OPEN     OUTPUT    BIMSTZAI.
 010-INIT.
     PERFORM  ZAMZAIL1-READ-SEC.
*****READ     ZAMZAIL1
*             AT END    MOVE  "END"  TO  END-FLG
*                       GO   TO   INIT-END
*****END-READ.
 INIT-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 ZAMZAIL1-READ-SEC      SECTION.
*
     READ     ZAMZAIL1
              AT END   MOVE  "END"  TO  END-FLG
              GO                    TO  ZAMZAIL1-READ-EXIT
     END-READ.
*倉庫ＣＤが空白の場合、対象外とする。
     IF  ZAI-F01 =  SPACE
         GO   TO    ZAMZAIL1-READ-SEC
     END-IF.
*
 ZAMZAIL1-READ-EXIT.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
*カウントアップ
     ADD        1           TO      ZAMZAIL1-CNT.
     PERFORM  BIMSTZAI-WRITE-SEC.
 010-ZAI.
     PERFORM  ZAMZAIL1-READ-SEC.
*****READ     ZAMZAIL1
*             AT END   MOVE  "END"  TO  END-FLG
*             GO TO MAIN-END
*****END-READ.
 MAIN-END.
     EXIT.
************************************************************
*      _１      在庫マスタ　　　ＣＳＶ出力                *
************************************************************
 BIMSTZAI-WRITE-SEC       SECTION.
*レコード初期化　　　　　　　　　　
     MOVE     SPACE         TO      BIM-REC.
     INITIALIZE                     BIM-REC.
*カウントアップ
     ADD       1             TO     BIMSTZAI-CNT.
*倉庫ＣＤ
     MOVE      ZAI-F01       TO     BIM-F01.
*サカタ商品ＣＤ
     MOVE      ZAI-F021      TO     BIM-F02.
*品単ＣＤ１　　　　　　
     MOVE      ZAI-F022(1:5) TO     BIM-F03.
*品単ＣＤ２
     MOVE      ZAI-F022(6:2) TO     BIM-F04.
*品単ＣＤ３
     MOVE      ZAI-F022(8:1) TO     BIM-F05.
*棚番
     MOVE      ZAI-F03       TO     BIM-F06.
*現在庫数
     IF        ZAI-F04       <      0
               MOVE   "-"    TO     BIM-F07(1:1)
     ELSE
               MOVE   "0"    TO     BIM-F07(1:1)
     END-IF.
     MOVE      ZAI-F04       TO     WK-SURYO.
     MOVE      WK-SURYO-1    TO     BIM-F07(2:8).
     MOVE      "."           TO     BIM-F07(10:1).
     MOVE      WK-SURYO-2    TO     BIM-F07(11:2).
*前月末在庫数
     IF        ZAI-F05       <      0
               MOVE   "-"    TO     BIM-F08(1:1)
     ELSE
               MOVE   "0"    TO     BIM-F08(1:1)
     END-IF.
     MOVE      ZAI-F05       TO     WK-SURYO.
     MOVE      WK-SURYO-1    TO     BIM-F08(2:8).
     MOVE      "."           TO     BIM-F08(10:1).
     MOVE      WK-SURYO-2    TO     BIM-F08(11:2).
*当月入出庫数
     IF        ZAI-F06       <      0
               MOVE   "-"    TO     BIM-F09(1:1)
     ELSE
               MOVE   "0"    TO     BIM-F09(1:1)
     END-IF.
     MOVE      ZAI-F06       TO     WK-SURYO.
     MOVE      WK-SURYO-1    TO     BIM-F09(2:8).
     MOVE      "."           TO     BIM-F09(10:1).
     MOVE      WK-SURYO-2    TO     BIM-F09(11:2).
*当月入庫数　
     IF        ZAI-F07       <      0
               MOVE   "-"    TO     BIM-F10(1:1)
     ELSE
               MOVE   "0"    TO     BIM-F10(1:1)
     END-IF.
     MOVE      ZAI-F07       TO     WK-SURYO.
     MOVE      WK-SURYO-1    TO     BIM-F10(2:8).
     MOVE      "."           TO     BIM-F10(10:1).
     MOVE      WK-SURYO-2    TO     BIM-F10(11:2).
*当月出庫数　
     IF        ZAI-F08       <      0
               MOVE   "-"    TO     BIM-F11(1:1)
     ELSE
               MOVE   "0"    TO     BIM-F11(1:1)
     END-IF.
     MOVE      ZAI-F08       TO     WK-SURYO.
     MOVE      WK-SURYO-1    TO     BIM-F11(2:8).
     MOVE      "."           TO     BIM-F11(10:1).
     MOVE      WK-SURYO-2    TO     BIM-F11(11:2).
*当月売上数
     IF        ZAI-F09       <      0
               MOVE   "-"    TO     BIM-F12(1:1)
     ELSE
               MOVE   "0"    TO     BIM-F12(1:1)
     END-IF.
     MOVE      ZAI-F09       TO     WK-SURYO.
     MOVE      WK-SURYO-1    TO     BIM-F12(2:8).
     MOVE      "."           TO     BIM-F12(10:1).
     MOVE      WK-SURYO-2    TO     BIM-F12(11:2).
*当月返品数　　　　　　
     IF        ZAI-F10       <      0
               MOVE   "-"    TO     BIM-F13(1:1)
     ELSE
               MOVE   "0"    TO     BIM-F13(1:1)
     END-IF.
     MOVE      ZAI-F10       TO     WK-SURYO.
     MOVE      WK-SURYO-1    TO     BIM-F13(2:8).
     MOVE      "."           TO     BIM-F13(10:1).
     MOVE      WK-SURYO-2    TO     BIM-F13(11:2).
*次月入庫数
     IF        ZAI-F11       <      0
               MOVE   "-"    TO     BIM-F14(1:1)
     ELSE
               MOVE   "0"    TO     BIM-F14(1:1)
     END-IF.
     MOVE      ZAI-F11       TO     WK-SURYO.
     MOVE      WK-SURYO-1    TO     BIM-F14(2:8).
     MOVE      "."           TO     BIM-F14(10:1).
     MOVE      WK-SURYO-2    TO     BIM-F14(11:2).
*次月出庫数
     IF        ZAI-F12       <      0
               MOVE   "-"    TO     BIM-F15(1:1)
     ELSE
               MOVE   "0"    TO     BIM-F15(1:1)
     END-IF.
     MOVE      ZAI-F12       TO     WK-SURYO.
     MOVE      WK-SURYO-1    TO     BIM-F15(2:8).
     MOVE      "."           TO     BIM-F15(10:1).
     MOVE      WK-SURYO-2    TO     BIM-F15(11:2).
*次月売上数
     IF        ZAI-F13       <      0
               MOVE   "-"    TO     BIM-F16(1:1)
     ELSE
               MOVE   "0"    TO     BIM-F16(1:1)
     END-IF.
     MOVE      ZAI-F13       TO     WK-SURYO.
     MOVE      WK-SURYO-1    TO     BIM-F16(2:8).
     MOVE      "."           TO     BIM-F16(10:1).
     MOVE      WK-SURYO-2    TO     BIM-F16(11:2).
*次月返品数
     IF        ZAI-F14       <      0
               MOVE   "-"    TO     BIM-F17(1:1)
     ELSE
               MOVE   "0"    TO     BIM-F17(1:1)
     END-IF.
     MOVE      ZAI-F14       TO     WK-SURYO.
     MOVE      WK-SURYO-1    TO     BIM-F17(2:8).
     MOVE      "."           TO     BIM-F17(10:1).
     MOVE      WK-SURYO-2    TO     BIM-F17(11:2).
*前月入庫数　
     IF        ZAI-F15       <      0
               MOVE   "-"    TO     BIM-F18(1:1)
     ELSE
               MOVE   "0"    TO     BIM-F18(1:1)
     END-IF.
     MOVE      ZAI-F15       TO     WK-SURYO.
     MOVE      WK-SURYO-1    TO     BIM-F18(2:8).
     MOVE      "."           TO     BIM-F18(10:1).
     MOVE      WK-SURYO-2    TO     BIM-F18(11:2).
*前月出庫数　
     IF        ZAI-F16       <      0
               MOVE   "-"    TO     BIM-F19(1:1)
     ELSE
               MOVE   "0"    TO     BIM-F19(1:1)
     END-IF.
     MOVE      ZAI-F16       TO     WK-SURYO.
     MOVE      WK-SURYO-1    TO     BIM-F19(2:8).
     MOVE      "."           TO     BIM-F19(10:1).
     MOVE      WK-SURYO-2    TO     BIM-F19(11:2).
*前月末在庫　
     IF        ZAI-F17       <      0
               MOVE   "-"    TO     BIM-F20(1:1)
     ELSE
               MOVE   "0"    TO     BIM-F20(1:1)
     END-IF.
     MOVE      ZAI-F17       TO     WK-SURYO.
     MOVE      WK-SURYO-1    TO     BIM-F20(2:8).
     MOVE      "."           TO     BIM-F20(10:1).
     MOVE      WK-SURYO-2    TO     BIM-F20(11:2).
*未入庫数
     IF        ZAI-F26       <      0
               MOVE   "-"    TO     BIM-F21(1:1)
     ELSE
               MOVE   "0"    TO     BIM-F21(1:1)
     END-IF.
     MOVE      ZAI-F26       TO     WK-SURYO.
     MOVE      WK-SURYO-1    TO     BIM-F21(2:8).
     MOVE      "."           TO     BIM-F21(10:1).
     MOVE      WK-SURYO-2    TO     BIM-F21(11:2).
*未出庫数
     IF        ZAI-F27       <      0
               MOVE   "-"    TO     BIM-F22(1:1)
     ELSE
               MOVE   "0"    TO     BIM-F22(1:1)
     END-IF.
     MOVE      ZAI-F27       TO     WK-SURYO.
     MOVE      WK-SURYO-1    TO     BIM-F22(2:8).
     MOVE      "."           TO     BIM-F22(10:1).
     MOVE      WK-SURYO-2    TO     BIM-F22(11:2).
*取引先ＣＤ　　　　　　
     MOVE      ZAI-F29      TO      BIM-F23.
*商品名カナ
     MOVE      ZAI-F30      TO      BIM-F24.
*廃棄数
*****IF        ZAI-F31 NOT  NUMERIC
*              MOVE   "0"           TO     BIM-F25(1:1)
*              MOVE   "00000000"    TO     BIM-F25(2:8)
*              MOVE   "."           TO     BIM-F25(10:1)
*              MOVE   "00"          TO     BIM-F25(11:2)
*    ELSE
*       IF     ZAI-F31       <      0
*              MOVE   "-"    TO     BIM-F25(1:1)
*       ELSE
*              MOVE   "0"    TO     BIM-F25(1:1)
*       END-IF
*       MOVE   ZAI-F31       TO     WK-SURYO
*       MOVE   WK-SURYO-1    TO     BIM-F25(2:8)
*       MOVE   "."           TO     BIM-F25(10:1)
*       MOVE   WK-SURYO-2    TO     BIM-F25(11:2)
*****END-IF.
*廃棄数（サカタ様側定義無し　固定値で送る）
        MOVE  "000000000.00" TO     BIM-F25.
*レコード出力
     WRITE     BIM-REC.

 BIMSTZAI-WRITE-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE      ZAMZAIL1  BIMSTZAI.

*    IF    BIMSTZAI-CNT NOT = ZERO
     DISPLAY "* ﾏｽﾀ" NC"抽出" ":" NC"在庫" "ﾏｽﾀ *"  UPON CONS.
     DISPLAY "* READ-CNT = " ZAMZAIL1-CNT     " *"  UPON CONS.
     DISPLAY "* WT-CNT   = " BIMSTZAI-CNT     " *"  UPON CONS.
     MOVE       BIMSTZAI-CNT       TO           PAR-OUT-KENSU.
*    END-IF.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
