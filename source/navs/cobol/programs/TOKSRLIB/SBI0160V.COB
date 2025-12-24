**************************************************************
*                                                            *
*    顧客名　　　　　：　（株）サカタのタネ殿　　　　　      *
*    業務名　　　　　：　BIツール連携　　　　　　　　　　　  *
*    モジュール名　　：　BI連携用データ作成：在庫マスタ(CSV) *
*    作成日／更新日　：　2018/12/11                          *
*    作成者／更新者　：　INOUE     　　　　　　　　　　　　  *
*    処理概要　　　　：　在庫マスタＣＳＶ出力を行う。　　    *
*                                                            *
**************************************************************
 IDENTIFICATION         DIVISION.
**************************************************************
 PROGRAM-ID.            SBI0160V.
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
****<< 連携在庫マスタデータ  >>******************************
     SELECT   BIMSTZAI  ASSIGN  TO   DA-01-S-BIMSTZAI
                        ORGANIZATION         IS  SEQUENTIAL
                        ACCESS  MODE         IS  SEQUENTIAL
                        FILE STATUS          IS  ZAI-STATUS.
****<< 連携データ：在庫　　ＣＳＶ >>****************************
     SELECT   SNDZAIKO  ASSIGN  TO   DA-01-S-SNDZAIKO
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   CSV-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< 連携在庫マスタデータ　 >>******************************
 FD    BIMSTZAI BLOCK     CONTAINS  11   RECORDS
                LABEL     RECORD    IS   STANDARD.
       COPY     BIMSTZAI  OF        XFDLIB
                JOINING   ZAI       PREFIX.
****<< 連携データ：在庫　　ＣＳＶ >>**************************
 FD    SNDZAIKO BLOCK     CONTAINS  1    RECORDS
                LABEL     RECORD    IS   STANDARD.
       COPY     SNDZAIKO  OF        XFDLIB
                JOINING   CSV       PREFIX.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 CSV-STATUS           PIC  X(2).
     02 ZAI-STATUS           PIC  X(2).
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
 01  BIMSTZAI-CNT            PIC  9(6)   VALUE  ZERO.
 01  SNDZAIKO-CNT            PIC  9(6)   VALUE  ZERO.
****  フラグ                 ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBI0160V".
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
                   PROCEDURE  SNDZAIKO.
     MOVE   "SNDZAIKO"        TO    ERR-FL-ID.
     MOVE    CSV-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  BIMSTZAI.
     MOVE   "BIMSTZAI"        TO    ERR-FL-ID.
     MOVE    ZAI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
 SBI0160V-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 SBI0160V-END.
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
     OPEN     INPUT     BIMSTZAI.
     OPEN     OUTPUT    SNDZAIKO.
 010-INIT.
     READ     BIMSTZAI
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END
     END-READ.
 INIT-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
*カウントアップ
     ADD        1           TO      BIMSTZAI-CNT.
     PERFORM  SNDZAIKO-WRITE-SEC.
 010-ZAI.
     READ     BIMSTZAI
              AT END   MOVE  "END"  TO  END-FLG
              GO TO MAIN-END
     END-READ.
 MAIN-END.
     EXIT.
************************************************************
*      _１      在庫マスタ　　　ＣＳＶ出力                *
************************************************************
 SNDZAIKO-WRITE-SEC       SECTION.
*レコード初期化　　　　　　　　　　
     MOVE     SPACE         TO      CSV-REC.
     INITIALIZE                     CSV-REC.
*カンマセット
     MOVE     ","           TO      CSV-A01  CSV-A02  CSV-A03
                                    CSV-A04  CSV-A05  CSV-A06
                                    CSV-A07  CSV-A08  CSV-A09
                                    CSV-A10  CSV-A11  CSV-A12
                                    CSV-A13  CSV-A14  CSV-A15
                                    CSV-A16  CSV-A17  CSV-A18
                                    CSV-A19  CSV-A20  CSV-A21
                                    CSV-A22  CSV-A23  CSV-A24
                                    CSV-A25.
*カウントアップ
     ADD       1            TO      SNDZAIKO-CNT.
*倉庫ＣＤ
     MOVE      ZAI-F01      TO      CSV-F01.
*サカタ商品ＣＤ
     MOVE      ZAI-F02      TO      CSV-F02.
*品単ＣＤ１　　　　　　
     MOVE      ZAI-F03      TO      CSV-F03.
*品単ＣＤ２
     MOVE      ZAI-F04      TO      CSV-F04.
*品単ＣＤ３
     MOVE      ZAI-F05      TO      CSV-F05.
*棚番
     MOVE      ZAI-F06      TO      CSV-F06.
*現在庫数
     MOVE      ZAI-F07      TO      CSV-F07.
*前月末在庫数
     MOVE      ZAI-F08      TO      CSV-F08.
*当月入出庫数
     MOVE      ZAI-F09      TO      CSV-F09.
*当月入庫数　
     MOVE      ZAI-F10      TO      CSV-F10.
*当月出庫数　
     MOVE      ZAI-F11      TO      CSV-F11.
*当月売上数
     MOVE      ZAI-F12      TO      CSV-F12.
*当月返品数　　　　　　
     MOVE      ZAI-F13      TO      CSV-F13.
*次月入庫数
     MOVE      ZAI-F14      TO      CSV-F14.
*次月出庫数
     MOVE      ZAI-F15      TO      CSV-F15.
*次月売上数
     MOVE      ZAI-F16      TO      CSV-F16.
*次月返品数
     MOVE      ZAI-F17      TO      CSV-F17.
*前月入庫数　
     MOVE      ZAI-F18      TO      CSV-F18.
*前月出庫数　
     MOVE      ZAI-F19      TO      CSV-F19.
*前月末在庫　
     MOVE      ZAI-F20      TO      CSV-F20.
*未入庫数
     MOVE      ZAI-F21      TO      CSV-F21.
*未出庫数
     MOVE      ZAI-F22      TO      CSV-F22.
*取引先ＣＤ　　　　　　
     MOVE      ZAI-F23      TO      CSV-F23.
*商品名カナ
     MOVE      ZAI-F24      TO      CSV-F24.
*廃棄数
     MOVE      ZAI-F25      TO      CSV-F25.
*レコード出力
     WRITE     CSV-REC.

 SNDZAIKO-WRITE-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE      SNDZAIKO  BIMSTZAI.

*    IF    BIMSTZAI-CNT NOT = ZERO
     DISPLAY  " BIMSTZAI-READ-CNT  = " BIMSTZAI-CNT UPON CONS.
     DISPLAY  " SNDZAIKO-WRITE-CNT = " SNDZAIKO-CNT UPON CONS.
     MOVE       SNDZAIKO-CNT       TO           PAR-OUT-KENSU.
*    END-IF.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
