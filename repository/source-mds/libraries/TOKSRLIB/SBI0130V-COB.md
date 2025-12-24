# SBI0130V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SBI0130V.COB`

## ソースコード

```cobol
**************************************************************
*                                                            *
*    顧客名　　　　　：　（株）サカタのタネ殿　　　　　      *
*    業務名　　　　　：　BIツール連携　　　　　　　　　　　  *
*    モジュール名　　：　BI連携用データ作成：日次計上ﾃﾞｰﾀ(CSV*
*    作成日／更新日　：　2018/12/13                          *
*    作成者／更新者　：　INOUE     　　　　　　　　　　　　  *
*    処理概要　　　　：　日次計上データのＣＳＶ出力を行う    *
*                                                            *
**************************************************************
 IDENTIFICATION         DIVISION.
**************************************************************
 PROGRAM-ID.            SBI0130V.
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
****<< BI連携用ファイル　  >>******************************
     SELECT   BITRNSET  ASSIGN  TO   DA-01-S-BITRNSET
                        ORGANIZATION         IS  SEQUENTIAL
                        ACCESS  MODE         IS  SEQUENTIAL
                        FILE STATUS          IS  SET-STATUS.
****<< 連携データ：日次計上　　　　　ＣＳＶ >>******************
     SELECT   SNDTRNDT  ASSIGN  TO   DA-01-S-SNDTRNDT
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   CSV-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< BI連携用ファイル　　 >>******************************
 FD    BITRNSET BLOCK     CONTAINS  8    RECORDS
                LABEL     RECORD    IS   STANDARD.
       COPY     BITRNSET  OF        XFDLIB
                JOINING   SET       PREFIX.
****<< 連携データ：日次計上ＣＳＶ >>**************************
 FD    SNDTRNDT BLOCK     CONTAINS  1    RECORDS
                LABEL     RECORD    IS   STANDARD.
       COPY     SNDTRNDT  OF        XFDLIB
                JOINING   CSV       PREFIX.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 CSV-STATUS           PIC  X(2).
     02 SET-STATUS           PIC  X(2).
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
 01  BITRNSET-CNT            PIC  9(6)   VALUE  ZERO.
 01  SNDTRNDT-CNT            PIC  9(6)   VALUE  ZERO.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBI0130V".
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
                   PROCEDURE  SNDTRNDT.
     MOVE   "SNDTRNDT"        TO    ERR-FL-ID.
     MOVE    CSV-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  BITRNSET.
     MOVE   "BITRNSET"        TO    ERR-FL-ID.
     MOVE    SET-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
 SBI0130V-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 SBI0130V-END.
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
     OPEN     INPUT     BITRNSET.
     OPEN     OUTPUT    SNDTRNDT.
 010-INIT.
     READ     BITRNSET
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
     ADD        1           TO      BITRNSET-CNT.
     IF  BITRNSET-CNT(3:4) = "0000" OR "5000"
         DISPLAY "READ-CNT = " BITRNSET-CNT UPON CONS
     END-IF.
     PERFORM  SNDTRNDT-WRITE-SEC.
 010-ZAI.
     READ     BITRNSET
              AT END   MOVE  "END"  TO  END-FLG
              GO TO MAIN-END
     END-READ.
 MAIN-END.
     EXIT.
************************************************************
*      _１      ＣＳＶ出力                *
************************************************************
 SNDTRNDT-WRITE-SEC       SECTION.
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
                                    CSV-A25  CSV-A26  CSV-A27
                                    CSV-A28  CSV-A29  CSV-A30
                                    CSV-A31  CSV-A32  CSV-A33
                                    CSV-A34  CSV-A35  CSV-A36
                                    CSV-A37  CSV-A38  CSV-A39
                                    CSV-A40  CSV-A41  CSV-A42
                                    CSV-A43  CSV-A44  CSV-A45
                                    CSV-A46  CSV-A47  CSV-A48
                                    CSV-A49  CSV-A50  CSV-A51
                                    CSV-A52  CSV-A53  CSV-A54
                                    CSV-A55  CSV-A56  CSV-A57.
*カウントアップ
     ADD       1            TO      SNDTRNDT-CNT.
*部門CD
     MOVE      SET-F01      TO      CSV-F01.
*伝票区分
     MOVE      SET-F02      TO      CSV-F02.
*伝票_
     MOVE      SET-F03      TO      CSV-F03.
*行_
     MOVE      SET-F04      TO      CSV-F04.
*赤黒区分
     MOVE      SET-F05      TO      CSV-F05.
*得意先／仕入先CD
     MOVE      SET-F06      TO      CSV-F06.
*納入先CD
     MOVE      SET-F07      TO      CSV-F07.
*発注日
     MOVE      SET-F08      TO      CSV-F08.
*納品日
     MOVE      SET-F09      TO      CSV-F09.
*出荷日
     MOVE      SET-F10      TO      CSV-F10.
*サカタ商品CD
     MOVE      SET-F11      TO      CSV-F11.
*サカタ品単１
     MOVE      SET-F12      TO      CSV-F12.
*サカタ品単２
     MOVE      SET-F13      TO      CSV-F13.
*サカタ品単３
     MOVE      SET-F14      TO      CSV-F14.
*ストック_
     MOVE      SET-F15      TO      CSV-F15.
*数量
     MOVE      SET-F16      TO      CSV-F16.
*単価区分
     MOVE      SET-F17      TO      CSV-F17.
*単価
     MOVE      SET-F18      TO      CSV-F18.
*金額
     MOVE      SET-F19      TO      CSV-F19.
*場所CD（出荷場所）
     MOVE      SET-F20      TO      CSV-F20.
*入出庫区分
     MOVE      SET-F21      TO      CSV-F21.
*税区分
     MOVE      SET-F22      TO      CSV-F22.
*作業明細区分（入庫場所）
     MOVE      SET-F23      TO      CSV-F23.
*備考
     MOVE      SET-F24      TO      CSV-F24.
*注文_
     MOVE      SET-F25      TO      CSV-F25.
*量販店商品CD
     MOVE      SET-F26      TO      CSV-F26.
*担当者CD
     MOVE      SET-F27      TO      CSV-F27.
*請求区分
     MOVE      SET-F28      TO      CSV-F28.
*処理日
     MOVE      SET-F29      TO      CSV-F29.
*量販店_
     MOVE      SET-F30      TO      CSV-F30.
*支払締日
     MOVE      SET-F31      TO      CSV-F31.
*単価１(仕入単価）
     MOVE      SET-F32      TO      CSV-F32.
*単価２（売価単価）
     MOVE      SET-F33      TO      CSV-F33.
*HG部取引先CD
     MOVE      SET-F34      TO      CSV-F34.
*受注数量（売上時）
     MOVE      SET-F35      TO      CSV-F35.
*JANCD
     MOVE      SET-F36      TO      CSV-F36.
*インストアCD
     MOVE      SET-F37      TO      CSV-F37.
*訂正数（欠品数）
     MOVE      SET-F38      TO      CSV-F38.
*生産者（ナフコ）
     MOVE      SET-F39      TO      CSV-F39.
*物流区分
     MOVE      SET-F40      TO      CSV-F40.
*商品計上区分
     MOVE      SET-F41      TO      CSV-F41.
*発注区分
     MOVE      SET-F42      TO      CSV-F42.
*小売連携区分
     MOVE      SET-F43      TO      CSV-F43.
*計上年月
     MOVE      SET-F44      TO      CSV-F44.
*計上週
     MOVE      SET-F45      TO      CSV-F45.
*未計上データ区分
     MOVE      SET-F46      TO      CSV-F46.
*手書オンライン区分
     MOVE      SET-F47      TO      CSV-F47.
*数値予備項目1
     MOVE      SET-F48      TO      CSV-F48.
*数値予備項目2
     MOVE      SET-F49      TO      CSV-F49.
*数値予備項目3
     MOVE      SET-F50      TO      CSV-F50.
*数値予備項目4
     MOVE      SET-F51      TO      CSV-F51.
*数値予備項目5
     MOVE      SET-F52      TO      CSV-F52.
*文字予備項目1
     MOVE      SET-F53      TO      CSV-F53.
*文字予備項目2
     MOVE      SET-F54      TO      CSV-F54.
*文字予備項目3
     MOVE      SET-F55      TO      CSV-F55.
*文字予備項目4
     MOVE      SET-F56      TO      CSV-F56.
*文字予備項目5
     MOVE      SET-F57      TO      CSV-F57.
*レコード出力
     WRITE     CSV-REC.

 SNDTRNDT-WRITE-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE      SNDTRNDT  BITRNSET.

*    IF    BITRNSET-CNT NOT = ZERO
     DISPLAY  " BITRNSET-READ-CNT  = " BITRNSET-CNT UPON CONS.
     DISPLAY  " SNDTRNDT-WRITE-CNT = " SNDTRNDT-CNT UPON CONS.
     MOVE       SNDTRNDT-CNT       TO           PAR-OUT-KENSU.
*    END-IF.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
