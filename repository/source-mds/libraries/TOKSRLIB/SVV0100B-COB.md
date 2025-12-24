# SVV0100B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SVV0100B.COB`

## ソースコード

```cobol
****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿
*    業務名　　　　　　　：　ＣＳＶ出力　　　　　
*    モジュール名　　　　：　在庫マスタＣＳＶ出力
*    作成日／作成者　　　：　2022/07/04 INOUE
*    処理概要　　　　　　：　在庫マスタＣＳＶ出力を行う。
*    更新履歴　　　　　　：
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　
*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SVV0100B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PRIMERGY6000.
 OBJECT-COMPUTER.       PRIMERGY6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         YA        IS   NIHONGO
         YB        IS   YB
         YB-21     IS   YB-21
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<  画面ファイル        >>******************************
     SELECT   DSPF      ASSIGN  TO   GS-DSPF
                        ORGANIZATION         IS  SEQUENTIAL
                        ACCESS  MODE         IS  SEQUENTIAL
                        SYMBOLIC DESTINATION IS  "DSP"
                        PROCESSING MODE      IS   DSP-PROC
                        GROUP                IS   DSP-GROUP
                        FORMAT               IS   DSP-FORMAT
                        SELECTED FUNCTION    IS   DSP-FUNC
                        FILE STATUS          IS   DSP-STATUS.
****<< 在庫マスタ　　　　   >>******************************
     SELECT   ZAMZAIF    ASSIGN  TO   DA-01-VI-ZAMZAIL1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  SEQUENTIAL
                        RECORD  KEY          IS  ZAI-F01
                                                 ZAI-F021
                                                 ZAI-F022
                                                 ZAI-F031
                                                 ZAI-F032
                                                 ZAI-F033
                        FILE STATUS          IS  ZAI-STATUS.
****<< 商品名称マスタ   >>******************************
     SELECT   SUBMEIF   ASSIGN  TO   DA-01-VI-SUBMEIL1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS
                                MEI-F011  *> 商品コード
                                MEI-F0121 *> 品単１
                                MEI-F0122 *> 品単２
                                MEI-F0123 *> 品単３
                        FILE STATUS          IS  MEI-STATUS.
****<< 在庫マスタ　　　ＣＳＶ >>******************************
     SELECT   SVVZAICS  ASSIGN  TO   DA-01-S-SVVZAICS
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   CSV-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     FVV00501           OF   XMDLIB.
****<< 在庫マスタ　　　　　　 >>******************************
 FD    ZAMZAIF.
       COPY     ZAMZAIF    OF        XFDLIB
                JOINING   ZAI       PREFIX.
****<< 商品名称マスタ　　　　 >>******************************
 FD    SUBMEIF.
       COPY     SUBMEIL1    OF        XFDLIB
                JOINING   MEI       PREFIX.
****<< 在庫マスタ　　　ＣＳＶ >>******************************
 FD  SVVZAICS            LABEL RECORD   IS   STANDARD
                         BLOCK CONTAINS  1   RECORDS.

 01  SVVZAICS-REC.
     03  FILLER                   PIC X(1000).
*
*FD  SVVZAICS.
*      COPY     SVVZAIC2    OF        XFDLIB
*               JOINING   CSV       PREFIX.
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ＣＳＶ タイトル行       ****
 COPY   SVVZAIC1   OF        XFDLIB
        JOINING    CSV1      PREFIX.
****  ＣＳＶ 明細行　　       ****
 COPY   SVVZAIC2   OF        XFDLIB
        JOINING    CSV2      PREFIX.
****  画面制御項目            ****
 01  DSP-CONTROL.
     02 DSP-PROC             PIC  X(2).
     02 DSP-GROUP            PIC  X(8).
     02 DSP-FORMAT           PIC  X(8).
     02 DSP-STATUS           PIC  X(2).
     02 DSP-FUNC             PIC  X(4).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 CSV-STATUS           PIC  X(2).
     02 ZAI-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
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
****  カウンタ                ****
 01  ZAMZAIF-CNT              PIC  9(8)   VALUE  ZERO.
 01  SVVZAICS-CNT              PIC  9(8)   VALUE  ZERO.
****  フラグ                  ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
 01  FG-SUBMEIF-INV           PIC  9(01).
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
**** エラーメッセージ         ****
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(30)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(30)  VALUE
            NC"開始・終了コードの関係に誤りがあります。".
     02  PMSG01              PIC N(20) VALUE
            NC"_取消　_終了".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SVV0100B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*ヘッダ行
 01  WK-HEAD.
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(04)  VALUE NC"倉庫ＣＤ".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(04)  VALUE NC"商品ＣＤ".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(02)  VALUE NC"品単".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(03)  VALUE NC"_番１".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(03)  VALUE NC"_番２".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(03)  VALUE NC"_番３".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(09)  VALUE NC"たねまるＪＡＮＣＤ".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(04)  VALUE NC"現在庫数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(06)  VALUE NC"前月末在庫数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(06)  VALUE NC"当月入出庫数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(05)  VALUE NC"当月入庫数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(05)  VALUE NC"当月出庫数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(05)  VALUE NC"当月売上数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(05)  VALUE NC"当月返品数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(05)  VALUE NC"次月入庫数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(05)  VALUE NC"次月出庫数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(05)  VALUE NC"次月売上数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(05)  VALUE NC"次月返品数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(03)  VALUE NC"入庫数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(03)  VALUE NC"出庫数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(06)  VALUE NC"前月末在庫数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(05)  VALUE NC"社内発注数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC N(07) VALUE NC"社内発注消込数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(05)  VALUE NC"当月発注数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(05)  VALUE NC"今回発注数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(03)  VALUE NC"予備３".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(04)  VALUE NC"発注数１".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(04)  VALUE NC"発注数２".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(04)  VALUE NC"発注数３".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(04)  VALUE NC"未入庫数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(04)  VALUE NC"未出庫数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(04)  VALUE NC"引当済数".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(05)  VALUE NC"取引先ＣＤ".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(04)  VALUE NC"カナ名称".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(03)  VALUE NC"登録日".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(03)  VALUE NC"修正日".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(06)  VALUE NC"商品名日本語".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(07)  VALUE NC"引当可能在庫数".
     03  FILLER   PIC  X(01)  VALUE X"29".
*
*レコード格納領域
     COPY   SVVZAMN  OF XFDLIB  JOINING   CSV  AS   PREFIX.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SVVZAICS.
     MOVE   "SVVZAICS  "        TO    ERR-FL-ID.
     MOVE    CSV-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DSPF.
     MOVE   "DSPF    "        TO    ERR-FL-ID.
     MOVE    DSP-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZAMZAIF.
     MOVE   "ZAMZAIF  "        TO    ERR-FL-ID.
     MOVE    ZAI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
 SVV0100B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 SVV0100B-END.
     EXIT.
************************************************************
*      １．０     初期処理                                 *
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
     OPEN     I-O       DSPF.
     OPEN     INPUT     ZAMZAIF.
     OPEN     INPUT     SUBMEIF.
     OPEN     OUTPUT    SVVZAICS.
*
     MOVE    "FVV00501"  TO     DSP-FORMAT.
     MOVE     SPACE     TO     FVV00501.
     MOVE     SPACE     TO     MSG1.
     MOVE     PMSG01    TO     MSG2.
     MOVE    "0"        TO     DSP-FLG.
     PERFORM  DSP-SEC.
     IF       END-FLG   NOT =  SPACE
              GO   TO   INIT-END
     END-IF.
     MOVE     SSOKCD    TO     ZAI-F01.
     MOVE     SSYOCD    TO     ZAI-F021.
     MOVE     WK-STAN   TO     ZAI-F022.
     MOVE     STANA1    TO     ZAI-F031.
     MOVE     STANA2    TO     ZAI-F032.
     MOVE     STANA3    TO     ZAI-F033.
     START    ZAMZAIF    KEY  IS  >=  ZAI-F01   ZAI-F021
                                      ZAI-F022  ZAI-F031
                                      ZAI-F032  ZAI-F033
              INVALID   MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END.
 010-INIT.
     READ     ZAMZAIF
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END
     END-READ.
     IF       ZAI-F01          >    ESOKCD
              MOVE  "END"    TO   END-FLG
              GO TO  INIT-END
     ELSE
              IF     ZAI-F021     >     ESYOCD
              OR     ZAI-F021     <     SSYOCD
                     GO TO  010-INIT
              ELSE
                     IF     ZAI-F022    >    WK-ETAN
                     OR     ZAI-F022    <    WK-STAN
                            GO TO  010-INIT
                     ELSE
                            IF     ZAI-F03     >    WK-ETANA
                            OR     ZAI-F03     <    WK-STANA
                                   GO TO  010-INIT
                            ELSE
                                   IF  ZAI-F01 = SPACE
                                   OR  ZAI-F021 = SPACE
                                   OR  ZAI-F022 = SPACE
                                       GO  TO     010-INIT
                                   END-IF
                                   MOVE SPACE   TO SVVZAICS-REC
********************************** INITIALIZE      SVVZAICS-REC
                                   MOVE WK-HEAD TO SVVZAICS-REC
                                   WRITE SVVZAICS-REC
                            END-IF
                     END-IF
              END-IF
     END-IF.
 INIT-END.
     EXIT.
************************************************************
*      ２．１     画面処理                                 *
************************************************************
 DSP-SEC                SECTION.
*画面表示
     MOVE     SPACE     TO     DSP-PROC.
     MOVE    "GPALL "   TO     DSP-GROUP.
     MOVE     HEN-DATE  TO     SDATE.
     MOVE     HEN-TIME  TO     STIME.
     WRITE    FVV00501.
*画面入力
     MOVE     SPACE     TO     MSG1.
     MOVE     "NE"      TO     DSP-PROC.
     IF   DSP-FLG   =   "0"
        MOVE    "GPHEAD"   TO     DSP-GROUP
     ELSE
        MOVE    "KAKNIN"   TO     DSP-GROUP
     END-IF.
     READ     DSPF.
*アテンション判定
     EVALUATE   DSP-FUNC
         WHEN   "F004"
                IF   DSP-FLG   =   "1"
                     MOVE   "0"       TO  DSP-FLG
                     MOVE   SPACE     TO  HEAD
                ELSE
                     MOVE   SPACE     TO  HEAD
                     MOVE   MSG-ERR1  TO  MSG1
                END-IF
                MOVE   "M"       TO  EDIT-OPTION  OF  SSOKCD
                MOVE   "M"       TO  EDIT-OPTION  OF  ESOKCD
                MOVE   "M"       TO  EDIT-OPTION  OF  SSYOCD
                MOVE   "M"       TO  EDIT-OPTION  OF  ESYOCD
                MOVE   "M"       TO  EDIT-OPTION  OF  STAN1
                MOVE   "M"       TO  EDIT-OPTION  OF  STAN2
                MOVE   "M"       TO  EDIT-OPTION  OF  STAN3
                MOVE   "M"       TO  EDIT-OPTION  OF  ETAN1
                MOVE   "M"       TO  EDIT-OPTION  OF  ETAN2
                MOVE   "M"       TO  EDIT-OPTION  OF  ETAN3
                MOVE   "M"       TO  EDIT-OPTION  OF  STANA1
                MOVE   "M"       TO  EDIT-OPTION  OF  STANA2
                MOVE   "M"       TO  EDIT-OPTION  OF  STANA3
                MOVE   "M"       TO  EDIT-OPTION  OF  ETANA1
                MOVE   "M"       TO  EDIT-OPTION  OF  ETANA2
                MOVE   "M"       TO  EDIT-OPTION  OF  ETANA3
                MOVE   " "       TO  EDIT-CURSOR  OF  SSOKCD
                MOVE   " "       TO  EDIT-CURSOR  OF  ESOKCD
                MOVE   " "       TO  EDIT-CURSOR  OF  SSYOCD
                MOVE   " "       TO  EDIT-CURSOR  OF  ESYOCD
                MOVE   " "       TO  EDIT-CURSOR  OF  STAN1
                MOVE   " "       TO  EDIT-CURSOR  OF  STAN2
                MOVE   " "       TO  EDIT-CURSOR  OF  STAN3
                MOVE   " "       TO  EDIT-CURSOR  OF  ETAN1
                MOVE   " "       TO  EDIT-CURSOR  OF  ETAN2
                MOVE   " "       TO  EDIT-CURSOR  OF  ETAN3
                MOVE   " "       TO  EDIT-CURSOR  OF  STANA1
                MOVE   " "       TO  EDIT-CURSOR  OF  STANA2
                MOVE   " "       TO  EDIT-CURSOR  OF  STANA3
                MOVE   " "       TO  EDIT-CURSOR  OF  ETANA1
                MOVE   " "       TO  EDIT-CURSOR  OF  ETANA2
                MOVE   " "       TO  EDIT-CURSOR  OF  ETANA3
                MOVE   SPACE     TO  MSG1
                GO TO  DSP-SEC
         WHEN   "F005"
                MOVE    "END"   TO    END-FLG
                MOVE    "END"   TO    PG-END
                GO TO  DSP-END
         WHEN   "E000"
*倉庫コード
                IF   SSOKCD =    SPACE
                     MOVE  "  " TO  SSOKCD
                END-IF
                IF   ESOKCD =    SPACE
                     MOVE  "99" TO  ESOKCD
                END-IF
*商品コード
                IF   SSYOCD =    SPACE
                     MOVE  "        " TO  SSYOCD
                END-IF
                IF   ESYOCD =    SPACE
                     MOVE  "99999999" TO  ESYOCD
                END-IF
*品単コード
                IF   STAN1  =    SPACE
                AND  STAN2  =    SPACE
                AND  STAN3  =    SPACE
                     MOVE  "     "    TO  STAN1
                     MOVE  "  "       TO  STAN2
                     MOVE  " "        TO  STAN3
                END-IF
                IF   ETAN1  =    SPACE
                AND  ETAN2  =    SPACE
                AND  ETAN3  =    SPACE
                     MOVE  "99999"    TO  ETAN1
                     MOVE  "99"       TO  ETAN2
                     MOVE  "9"        TO  ETAN3
                END-IF
                MOVE        STAN1     TO  WK-STAN1
                MOVE        STAN2     TO  WK-STAN2
                MOVE        STAN3     TO  WK-STAN3
                MOVE        ETAN1     TO  WK-ETAN1
                MOVE        ETAN2     TO  WK-ETAN2
                MOVE        ETAN3     TO  WK-ETAN3
*_番
                IF   ETANA1 =    SPACE
                AND  ETANA2 =    SPACE
                AND  ETANA3 =    SPACE
                     MOVE  "99999"    TO  ETANA1
                     MOVE  "9"        TO  ETANA2
                     MOVE  "99"       TO  ETANA3
                END-IF
                MOVE        STANA1    TO  WK-STANA1
                MOVE        STANA2    TO  WK-STANA2
                MOVE        STANA3    TO  WK-STANA3
                MOVE        ETANA1    TO  WK-ETANA1
                MOVE        ETANA2    TO  WK-ETANA2
                MOVE        ETANA3    TO  WK-ETANA3
*大小関係チェック
***倉庫コード
                IF   SSOKCD >   ESOKCD
                     IF     MSG1        =   SPACE
                            MOVE   MSG-ERR2  TO  MSG1
                     END-IF
                     MOVE   "R"       TO  EDIT-OPTION  OF  SSOKCD
                     MOVE   "R"       TO  EDIT-OPTION  OF  ESOKCD
                     MOVE   "C"       TO  EDIT-CURSOR  OF  SSOKCD
                     GO TO  DSP-SEC
                ELSE
                     MOVE   "M"       TO  EDIT-OPTION  OF  SSOKCD
                     MOVE   "M"       TO  EDIT-OPTION  OF  ESOKCD
                     MOVE   " "       TO  EDIT-CURSOR  OF  SSOKCD
                     MOVE   " "       TO  EDIT-CURSOR  OF  ESOKCD
                END-IF
***商品コード
                IF   SSYOCD >   ESYOCD
                     IF     MSG1        =   SPACE
                            MOVE   MSG-ERR2  TO  MSG1
                     END-IF
                     MOVE   "R"       TO  EDIT-OPTION  OF  SSYOCD
                     MOVE   "R"       TO  EDIT-OPTION  OF  ESYOCD
                     MOVE   "C"       TO  EDIT-CURSOR  OF  SSYOCD
                     GO TO  DSP-SEC
                ELSE
                     MOVE   "M"       TO  EDIT-OPTION  OF  SSYOCD
                     MOVE   "M"       TO  EDIT-OPTION  OF  ESYOCD
                     MOVE   " "       TO  EDIT-CURSOR  OF  SSYOCD
                     MOVE   " "       TO  EDIT-CURSOR  OF  ESYOCD
                END-IF
***品単コード
                IF   WK-STAN >   WK-ETAN
                     IF     MSG1        =   SPACE
                            MOVE   MSG-ERR2  TO  MSG1
                     END-IF
                     MOVE   "R"       TO  EDIT-OPTION  OF  STAN1
                     MOVE   "R"       TO  EDIT-OPTION  OF  STAN2
                     MOVE   "R"       TO  EDIT-OPTION  OF  STAN3
                     MOVE   "R"       TO  EDIT-OPTION  OF  ETAN1
                     MOVE   "R"       TO  EDIT-OPTION  OF  ETAN2
                     MOVE   "R"       TO  EDIT-OPTION  OF  ETAN3
                     MOVE   "C"       TO  EDIT-CURSOR  OF  STAN1
                     GO TO  DSP-SEC
                ELSE
                     MOVE   "M"       TO  EDIT-OPTION  OF  STAN1
                     MOVE   "M"       TO  EDIT-OPTION  OF  STAN2
                     MOVE   "M"       TO  EDIT-OPTION  OF  STAN3
                     MOVE   "M"       TO  EDIT-OPTION  OF  ETAN1
                     MOVE   "M"       TO  EDIT-OPTION  OF  ETAN2
                     MOVE   "M"       TO  EDIT-OPTION  OF  ETAN3
                     MOVE   " "       TO  EDIT-CURSOR  OF  STAN1
                     MOVE   " "       TO  EDIT-CURSOR  OF  STAN2
                     MOVE   " "       TO  EDIT-CURSOR  OF  STAN3
                     MOVE   " "       TO  EDIT-CURSOR  OF  ETAN1
                     MOVE   " "       TO  EDIT-CURSOR  OF  ETAN2
                     MOVE   " "       TO  EDIT-CURSOR  OF  ETAN3
                END-IF
***_番
                IF   WK-STANA >   WK-ETANA
                     IF     MSG1        =   SPACE
                            MOVE   MSG-ERR2  TO  MSG1
                     END-IF
                     MOVE   "R"       TO  EDIT-OPTION  OF  STANA1
                     MOVE   "R"       TO  EDIT-OPTION  OF  STANA2
                     MOVE   "R"       TO  EDIT-OPTION  OF  STANA3
                     MOVE   "R"       TO  EDIT-OPTION  OF  ETANA1
                     MOVE   "R"       TO  EDIT-OPTION  OF  ETANA2
                     MOVE   "R"       TO  EDIT-OPTION  OF  ETANA3
                     MOVE   "C"       TO  EDIT-CURSOR  OF  STANA1
                     GO TO  DSP-SEC
                ELSE
                     MOVE   "M"       TO  EDIT-OPTION  OF  STANA1
                     MOVE   "M"       TO  EDIT-OPTION  OF  STANA2
                     MOVE   "M"       TO  EDIT-OPTION  OF  STANA3
                     MOVE   "M"       TO  EDIT-OPTION  OF  ETANA1
                     MOVE   "M"       TO  EDIT-OPTION  OF  ETANA2
                     MOVE   "M"       TO  EDIT-OPTION  OF  ETANA3
                     MOVE   " "       TO  EDIT-CURSOR  OF  STANA1
                     MOVE   " "       TO  EDIT-CURSOR  OF  STANA2
                     MOVE   " "       TO  EDIT-CURSOR  OF  STANA3
                     MOVE   " "       TO  EDIT-CURSOR  OF  ETANA1
                     MOVE   " "       TO  EDIT-CURSOR  OF  ETANA2
                     MOVE   " "       TO  EDIT-CURSOR  OF  ETANA3
                END-IF
                IF   DSP-FLG   =   "0"
                     MOVE   "1"       TO  DSP-FLG
                     GO TO  DSP-SEC
                END-IF
         WHEN   OTHER
                MOVE   MSG-ERR1  TO  MSG1
                GO TO  DSP-SEC
     END-EVALUATE.
 DSP-END.
     EXIT.
************************************************************
*      ２．０      メイン処理                              *
************************************************************
 MAIN-SEC               SECTION.
*在庫マスタカウントアップ
     ADD        1           TO      ZAMZAIF-CNT.
     PERFORM  SVVZAICS-WRITE-SEC.
 010-ZAI.
     READ     ZAMZAIF
              AT END   MOVE  "END"  TO  END-FLG
              GO TO MAIN-END
     END-READ.
     IF       ZAI-F01          >    ESOKCD
              MOVE  "END"    TO   END-FLG
              GO TO  MAIN-END
     ELSE
              IF     ZAI-F021     >     ESYOCD
              OR     ZAI-F021     <     SSYOCD
                     GO TO  010-ZAI
              ELSE
                     IF     ZAI-F022    >    WK-ETAN
                     OR     ZAI-F022    <    WK-STAN
                            GO TO  010-ZAI
                     ELSE
                            IF     ZAI-F03     >    WK-ETANA
                            OR     ZAI-F03     <    WK-STANA
                                   GO TO  010-ZAI
                            ELSE
                                   IF  ZAI-F01 = SPACE
                                   OR  ZAI-F021 = SPACE
                                   OR  ZAI-F022 = SPACE
                                       GO  TO     010-ZAI
                                   END-IF
                            END-IF
                     END-IF
              END-IF
     END-IF.
 MAIN-END.
     EXIT.
************************************************************
*      ２．１      在庫マスタ　　　ＣＳＶ出力              *
************************************************************
 SVVZAICS-WRITE-SEC       SECTION.
* 2011/10/11,S  S.I/NAV
     MOVE  ZAI-F021         TO  MEI-F011  *> 商品コード
     MOVE  ZAI-F022         TO  MEI-F012  *> 品単
     READ  SUBMEIF
       INVALID
         MOVE  1            TO  FG-SUBMEIF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-SUBMEIF-INV
     END-READ.
* 2011/10/11,E  S.I/NAV
*
*レコード初期化　　　　　　　　　　
     MOVE     SPACE         TO      CSV2-REC.
     INITIALIZE                     CSV2-REC.
*カンマセット
     MOVE     ","           TO      CSV2-MK01 CSV2-MK02 CSV2-MK03
                                    CSV2-MK04 CSV2-MK05 CSV2-MK06
                                    CSV2-MK07 CSV2-MK08 CSV2-MK09
                                    CSV2-MK10 CSV2-MK11 CSV2-MK12
                                    CSV2-MK13 CSV2-MK14 CSV2-MK15
                                    CSV2-MK16 CSV2-MK17 CSV2-MK18
                                    CSV2-MK19 CSV2-MK20 CSV2-MK21
                                    CSV2-MK22 CSV2-MK23 CSV2-MK24
                                    CSV2-MK25 CSV2-MK26 CSV2-MK27
                                    CSV2-MK28 CSV2-MK29 CSV2-MK30
                                    CSV2-MK31 CSV2-MK32 CSV2-MK33
                                    CSV2-MK34 CSV2-MK35 CSV2-MK36
                                    CSV2-MK37.
*カウントアップ
     ADD       1            TO      SVVZAICS-CNT.
*倉庫コード
     MOVE      ZAI-F01      TO      CSV2-M01.
*商品コード
     MOVE      ZAI-F021     TO      CSV2-M02.
*品単コード　　　　　　
     MOVE      ZAI-F022     TO      CSV2-M03.
*_番１
     MOVE      ZAI-F031     TO      CSV2-M04.
*_番２
     MOVE      ZAI-F032     TO      CSV2-M05.
*_番３
     MOVE      ZAI-F033     TO      CSV2-M06.
*たねまるＪＡＮＣＤ　　
     IF        FG-SUBMEIF-INV = ZERO
               MOVE     MEI-D01     TO      CSV2-M07
     ELSE
               MOVE     SPACE       TO      CSV2-M07
     END-IF.
*現在庫数
     MOVE      ZAI-F04      TO      CSV2-M08.
     IF        ZAI-F04 < ZERO
               MOVE  "-"    TO      CSV2-MA08
     ELSE
               MOVE  "0"    TO      CSV2-MA08
     END-IF.
*前月末在庫数
     MOVE      ZAI-F05      TO      CSV2-M09.
     IF        ZAI-F05 < ZERO
               MOVE  "-"    TO      CSV2-MA09
     ELSE
               MOVE  "0"    TO      CSV2-MA09
     END-IF.
*当月入出庫数
     MOVE      ZAI-F06      TO      CSV2-M10.
     IF        ZAI-F06 < ZERO
               MOVE  "-"    TO      CSV2-MA10
     ELSE
               MOVE  "0"    TO      CSV2-MA10
     END-IF.
*当月入庫数　
     MOVE      ZAI-F07      TO      CSV2-M11.
     IF        ZAI-F07 < ZERO
               MOVE  "-"    TO      CSV2-MA11
     ELSE
               MOVE  "0"    TO      CSV2-MA11
     END-IF.
*当月出庫数　
     MOVE      ZAI-F08      TO      CSV2-M12.
     IF        ZAI-F08 < ZERO
               MOVE  "-"    TO      CSV2-MA12
     ELSE
               MOVE  "0"    TO      CSV2-MA12
     END-IF.
*当月売上数
     MOVE      ZAI-F09      TO      CSV2-M13.
     IF        ZAI-F09 < ZERO
               MOVE  "-"    TO      CSV2-MA13
     ELSE
               MOVE  "0"    TO      CSV2-MA13
     END-IF.
*当月返品数　　　　　　
     MOVE      ZAI-F10      TO      CSV2-M14.
     IF        ZAI-F10 < ZERO
               MOVE  "-"    TO      CSV2-MA14
     ELSE
               MOVE  "0"    TO      CSV2-MA14
     END-IF.
*次月入庫数
     MOVE      ZAI-F11      TO      CSV2-M15.
     IF        ZAI-F11 < ZERO
               MOVE  "-"    TO      CSV2-MA15
     ELSE
               MOVE  "0"    TO      CSV2-MA15
     END-IF.
*次月出庫数
     MOVE      ZAI-F12      TO      CSV2-M16.
     IF        ZAI-F12 < ZERO
               MOVE  "-"    TO      CSV2-MA16
     ELSE
               MOVE  "0"    TO      CSV2-MA16
     END-IF.
*次月売上数
     MOVE      ZAI-F13      TO      CSV2-M17.
     IF        ZAI-F13 < ZERO
               MOVE  "-"    TO      CSV2-MA17
     ELSE
               MOVE  "0"    TO      CSV2-MA17
     END-IF.
*次月返品数
     MOVE      ZAI-F14      TO      CSV2-M18.
     IF        ZAI-F14 < ZERO
               MOVE  "-"    TO      CSV2-MA18
     ELSE
               MOVE  "0"    TO      CSV2-MA18
     END-IF.
*入庫数　　　
     MOVE      ZAI-F15      TO      CSV2-M19.
     IF        ZAI-F15 < ZERO
               MOVE  "-"    TO      CSV2-MA19
     ELSE
               MOVE  "0"    TO      CSV2-MA19
     END-IF.
*出庫数　　　
     MOVE      ZAI-F16      TO      CSV2-M20.
     IF        ZAI-F16 < ZERO
               MOVE  "-"    TO      CSV2-MA20
     ELSE
               MOVE  "0"    TO      CSV2-MA20
     END-IF.
*前月末在庫　
     MOVE      ZAI-F17      TO      CSV2-M21.
     IF        ZAI-F17 < ZERO
               MOVE  "-"    TO      CSV2-MA21
     ELSE
               MOVE  "0"    TO      CSV2-MA21
     END-IF.
*社内発注数　
     MOVE      ZAI-F18      TO      CSV2-M22.
     IF        ZAI-F18 < ZERO
               MOVE  "-"    TO      CSV2-MA22
     ELSE
               MOVE  "0"    TO      CSV2-MA22
     END-IF.
*社内発注消込数
     MOVE      ZAI-F19      TO      CSV2-M23.
     IF        ZAI-F19 < ZERO
               MOVE  "-"    TO      CSV2-MA23
     ELSE
               MOVE  "0"    TO      CSV2-MA23
     END-IF.
*当月発注数　　　　　　
     MOVE      ZAI-F20      TO      CSV2-M24.
     IF        ZAI-F20 < ZERO
               MOVE  "-"    TO      CSV2-MA24
     ELSE
               MOVE  "0"    TO      CSV2-MA24
     END-IF.
*今回発注数
     MOVE      ZAI-F21      TO      CSV2-M25.
     IF        ZAI-F21 < ZERO
               MOVE  "-"    TO      CSV2-MA25
     ELSE
               MOVE  "0"    TO      CSV2-MA25
     END-IF.
*予備３
     MOVE      ZAI-F22      TO      CSV2-M26.
     IF        ZAI-F22 < ZERO
               MOVE  "-"    TO      CSV2-MA26
     ELSE
               MOVE  "0"    TO      CSV2-MA26
     END-IF.
*発注数量１
     MOVE      ZAI-F23      TO      CSV2-M27.
     IF        ZAI-F23 < ZERO
               MOVE  "-"    TO      CSV2-MA27
     ELSE
               MOVE  "0"    TO      CSV2-MA27
     END-IF.
*発注数量２
     MOVE      ZAI-F24      TO      CSV2-M28.
     IF        ZAI-F24 < ZERO
               MOVE  "-"    TO      CSV2-MA28
     ELSE
               MOVE  "0"    TO      CSV2-MA28
     END-IF.
*発注数量３　
     MOVE      ZAI-F25      TO      CSV2-M29.
     IF        ZAI-F25 < ZERO
               MOVE  "-"    TO      CSV2-MA29
     ELSE
               MOVE  "0"    TO      CSV2-MA29
     END-IF.
*未入庫数　　
     MOVE      ZAI-F26      TO      CSV2-M30.
     IF        ZAI-F26 < ZERO
               MOVE  "-"    TO      CSV2-MA30
     ELSE
               MOVE  "0"    TO      CSV2-MA30
     END-IF.
*未出庫数　　
     MOVE      ZAI-F27      TO      CSV2-M31.
     IF        ZAI-F27 < ZERO
               MOVE  "-"    TO      CSV2-MA31
     ELSE
               MOVE  "0"    TO      CSV2-MA31
     END-IF.
*引当済数　　
     MOVE      ZAI-F28      TO      CSV2-M32.
     IF        ZAI-F27 < ZERO
               MOVE  "-"    TO      CSV2-MA32
     ELSE
               MOVE  "0"    TO      CSV2-MA32
     END-IF.
*取引先コード　
     MOVE      ZAI-F29      TO      CSV2-M33.
*カナ名称　　
     MOVE      ZAI-F30      TO      CSV2-M34.
*登録日　　　
     MOVE      ZAI-F98      TO      CSV2-M35.
*更新日
     MOVE      ZAI-F99      TO      CSV2-M36.
* 2011/10/11,S  S.I/NAV
*商品名日本語
*
     IF  FG-SUBMEIF-INV = ZERO
         MOVE  SPACE        TO  CSV2-M37
         STRING  X"28" MEI-F02 X"29"
           DELIMITED BY SIZE  INTO  CSV2-M37
     ELSE
         MOVE  SPACE        TO  CSV2-M37
     END-IF.
*引当可能在庫数
**   ZAI-F04  現在庫数
**   ZAI-F27  未出庫数
*
     COMPUTE  CSV2-M38  =  ZAI-F04  -  ZAI-F27.
     IF  ZAI-F04  -  ZAI-F27  <  ZERO *> マイナス
         MOVE  "-"          TO  CSV2-MA38
     ELSE
         MOVE  "0"          TO  CSV2-MA38
     END-IF.
* 2011/10/11,E  S.I/NAV
*レコード転送
     MOVE      CSV2-REC     TO      SVVZAICS-REC.
*レコード出力
     WRITE     SVVZAICS-REC.
*
 SVVZAICS-WRITE-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF   SVVZAICS  ZAMZAIF  SUBMEIF.
*
     IF    ZAMZAIF-CNT NOT = ZERO
     DISPLAY  " ZAMZAIF-READ-CNT = " ZAMZAIF-CNT UPON CONS
     DISPLAY  " SVVZAICS-WRITE-CNT = " SVVZAICS-CNT UPON CONS
     ELSE
     DISPLAY  " ZAMZAIF-READ-CNT = " ZAMZAIF-CNT UPON CONS
     DISPLAY  " SVVZAICS-WRITE-CNT = " SVVZAICS-CNT UPON CONS
     MOVE     4010       TO     PROGRAM-STATUS
     END-IF.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
