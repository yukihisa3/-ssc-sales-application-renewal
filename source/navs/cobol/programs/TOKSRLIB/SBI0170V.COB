**************************************************************
*                                                            *
*    顧客名　　　　　：　（株）サカタのタネ殿　　　　　      *
*    業務名　　　　　：　BIツール連携　　　　　　　　　　　  *
*    モジュール名　　：　BI連携用データ作成：商品変換TBL(CSV)*
*    作成日／更新日　：　2018/12/11                          *
*    作成者／更新者　：　INOUE     　　　　　　　　　　　　  *
*    処理概要　　　　：　商品変換ＴＢＬのＣＳＶ出力を行う    *
*                                                            *
**************************************************************
 IDENTIFICATION         DIVISION.
**************************************************************
 PROGRAM-ID.            SBI0170V.
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
****<< 連携商品変換ＴＢＬ  >>******************************
     SELECT   BIMSTTBL  ASSIGN  TO   DA-01-S-BIMSTTBL
                        ORGANIZATION         IS  SEQUENTIAL
                        ACCESS  MODE         IS  SEQUENTIAL
                        FILE STATUS          IS  TBL-STATUS.
****<< 連携データ：商品変換ＴＢＬ　ＣＳＶ >>*******************
     SELECT   SNDSYOTB  ASSIGN  TO   DA-01-S-SNDSYOTB
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   CSV-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< 連携商品変換ＴＢＬ　 >>******************************
 FD    BIMSTTBL BLOCK     CONTAINS  20   RECORDS
                LABEL     RECORD    IS   STANDARD.
       COPY     BIMSTTBL  OF        XFDLIB
                JOINING   TBL       PREFIX.
****<< 連携データ：商品変換ＴＢＬ　　ＣＳＶ >>**************
 FD    SNDSYOTB BLOCK     CONTAINS  1    RECORDS
                LABEL     RECORD    IS   STANDARD.
       COPY     SNDSYOTB  OF        XFDLIB
                JOINING   CSV       PREFIX.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 CSV-STATUS           PIC  X(2).
     02 TBL-STATUS           PIC  X(2).
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
 01  BIMSTTBL-CNT            PIC  9(6)   VALUE  ZERO.
 01  SNDSYOTB-CNT            PIC  9(6)   VALUE  ZERO.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBI0170V".
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
                   PROCEDURE  SNDSYOTB.
     MOVE   "SNDSYOTB"        TO    ERR-FL-ID.
     MOVE    CSV-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  BIMSTTBL.
     MOVE   "BIMSTTBL"        TO    ERR-FL-ID.
     MOVE    TBL-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
 SBI0170V-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 SBI0170V-END.
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
     OPEN     INPUT     BIMSTTBL.
     OPEN     OUTPUT    SNDSYOTB.
 010-INIT.
     READ     BIMSTTBL
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
     ADD        1           TO      BIMSTTBL-CNT.
     PERFORM  SNDSYOTB-WRITE-SEC.
 010-ZAI.
     READ     BIMSTTBL
              AT END   MOVE  "END"  TO  END-FLG
              GO TO MAIN-END
     END-READ.
 MAIN-END.
     EXIT.
************************************************************
*      _１      ＣＳＶ出力                *
************************************************************
 SNDSYOTB-WRITE-SEC       SECTION.
*レコード初期化　　　　　　　　　　
     MOVE     SPACE         TO      CSV-REC.
     INITIALIZE                     CSV-REC.
*カンマセット
     MOVE     ","           TO      CSV-A01  CSV-A02  CSV-A03
                                    CSV-A04  CSV-A05  CSV-A06
                                    CSV-A07  CSV-A08  CSV-A09
                                    CSV-A10  CSV-A11  CSV-A12
                                    CSV-A13  CSV-A14.
*カウントアップ
     ADD       1            TO      SNDSYOTB-CNT.
*取引先ＣＤ
     MOVE      TBL-F01      TO      CSV-F01.
*相手商品ＣＤ
     MOVE      TBL-F02      TO      CSV-F02.
*サカタ商品ＣＤ　　　　　　
     MOVE      TBL-F03      TO      CSV-F03.
*品単１
     MOVE      TBL-F04      TO      CSV-F04.
*品単２
     MOVE      TBL-F05      TO      CSV-F05.
*品単３
     MOVE      TBL-F06      TO      CSV-F06.
*出荷場所
     MOVE      TBL-F07      TO      CSV-F07.
*仕入単価
     MOVE      TBL-F08      TO      CSV-F08.
*原価単価
     MOVE      TBL-F09      TO      CSV-F09.
*売価単価
     MOVE      TBL-F10      TO      CSV-F10.
*棚番
     MOVE      TBL-F11      TO      CSV-F11.
*予備商品情報（JANCD)
     MOVE      TBL-F12      TO      CSV-F12.
*予備商品情報（インストアＣＤ)
     MOVE      TBL-F13      TO      CSV-F13.
*入数
     MOVE      TBL-F14      TO      CSV-F14.
*レコード出力
     WRITE     CSV-REC.

 SNDSYOTB-WRITE-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE      SNDSYOTB  BIMSTTBL.

*    IF    BIMSTTBL-CNT NOT = ZERO
     DISPLAY  " BIMSTTBL-READ-CNT  = " BIMSTTBL-CNT UPON CONS.
     DISPLAY  " SNDSYOTB-WRITE-CNT = " SNDSYOTB-CNT UPON CONS.
     MOVE       SNDSYOTB-CNT       TO           PAR-OUT-KENSU.
*    END-IF.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
