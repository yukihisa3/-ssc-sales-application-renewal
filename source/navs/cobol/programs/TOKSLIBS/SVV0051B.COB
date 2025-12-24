****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿
*    業務名　　　　　　　：　ＣＳＶ出力　　　　　
*    モジュール名　　　　：　在庫マスタＣＳＶ出力
*    作成日／更新日　　　：　06/01/11
*    作成者／更新者　　　：　ＮＡＶ松野　　　　　
*    処理概要　　　　　　：　在庫マスタＣＳＶ出力を行う。
*    更新履歴　　　　　　：
*      2011/10/11 飯田/NAV 基幹サーバ統合・業務改善
*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SVV0051B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
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
****<< 在庫マスタ　　　ＣＳＶ >>******************************
     SELECT   SVVZAMNF   ASSIGN  TO   DA-01-S-SVVZAMNF
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   CSV-STATUS.
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
* 2011/10/11,S  S.I/NAV
****<< 商品名称マスタ   >>******************************
     SELECT   MEIMSF    ASSIGN  TO   DA-01-VI-MEIMS1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS
                                MEI-F011  *> 商品コード
                                MEI-F0121 *> 品単１
                                MEI-F0122 *> 品単２
                                MEI-F0123 *> 品単３
                        FILE STATUS          IS  MEI-STATUS.
* 2011/10/11,E  S.I/NAV
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     FVV00501           OF   XMDLIB.
****<< 在庫マスタ　　　ＣＳＶ >>******************************
 FD  SVVZAMNF            LABEL RECORD   IS   STANDARD.
*
 01  SVVZAMNF-REC.
     03  FILLER                   PIC X(1000).
****<< 在庫マスタ　　　　　　 >>******************************
 FD    ZAMZAIF.
       COPY     ZAMZAIF    OF        XFDLIB
                JOINING   ZAI       PREFIX.
* 2011/10/11,S  S.I/NAV
****<< 商品名称マスタ　　　　 >>******************************
 FD    MEIMSF.
       COPY     MEIMS1    OF        XFDLIB
                JOINING   MEI       PREFIX.
* 2011/10/11,E  S.I/NAV
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
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
* 2011/10/11,S  S.I/NAV
     02 MEI-STATUS           PIC  X(2).
* 2011/10/11,E  S.I/NAV
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
 01  SVVZAMNF-CNT              PIC  9(8)   VALUE  ZERO.
****  フラグ                  ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
* 2011/10/11,S  S.I/NAV
 01  FG-MEIMSF-INV           PIC  9(01).
* 2011/10/11,E  S.I/NAV
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SVV0051B".
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
* 2011/10/11,S  S.I/NAV
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(06)  VALUE NC"商品名日本語".
     03  FILLER   PIC  X(01)  VALUE X"29".
     03  FILLER   PIC  X(01)  VALUE ",".
     03  FILLER   PIC  X(01)  VALUE X"28".
     03  FILLER   PIC  N(07)  VALUE NC"引当可能在庫数".
     03  FILLER   PIC  X(01)  VALUE X"29".
* 2011/10/11,E  S.I/NAV
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
                   PROCEDURE  SVVZAMNF.
     MOVE   "SVVZAMNF  "        TO    ERR-FL-ID.
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
 SVV0051B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 SVV0051B-END.
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
* 2011/10/11,S  S.I/NAV
     OPEN     INPUT     MEIMSF.
* 2011/10/11,E  S.I/NAV

     OPEN     OUTPUT    SVVZAMNF.
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
                                   MOVE SPACE   TO SVVZAMNF-REC
********************************** INITIALIZE      SVVZAMNF-REC
                                   MOVE WK-HEAD TO SVVZAMNF-REC
                                   WRITE SVVZAMNF-REC
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
     PERFORM  SVVZAMNF-WRITE-SEC.
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
 SVVZAMNF-WRITE-SEC       SECTION.
* 2011/10/11,S  S.I/NAV
     MOVE  ZAI-F021         TO  MEI-F011  *> 商品コード
     MOVE  ZAI-F022         TO  MEI-F012  *> 品単
     READ  MEIMSF
       INVALID
         MOVE  1            TO  FG-MEIMSF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-MEIMSF-INV
     END-READ.
* 2011/10/11,E  S.I/NAV

*レコード初期化　　　　　　　　　　
     MOVE     SPACE         TO      CSV-REC.
     INITIALIZE                     CSV-REC.
*カンマセット
* 2011/10/11,S  S.I/NAV
**     MOVE     ","           TO      CSV-A01  CSV-A02  CSV-A03
**                                    CSV-A04  CSV-A05  CSV-A06
**                                    CSV-A07  CSV-A08  CSV-A09
**                                    CSV-A10  CSV-A11  CSV-A12
**                                    CSV-A13  CSV-A14  CSV-A15
**                                    CSV-A16  CSV-A17  CSV-A18
**                                    CSV-A19  CSV-A20  CSV-A21
**                                    CSV-A22  CSV-A23  CSV-A24
**                                    CSV-A25  CSV-A26  CSV-A27
**                                    CSV-A28  CSV-A29  CSV-A30
**                                    CSV-A31  CSV-A32  CSV-A33
**                                    CSV-A34.
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
                                    CSV-A34  CSV-A35  CSV-A36.
* 2011/10/11,E  S.I/NAV
*カウントアップ
     ADD       1            TO      SVVZAMNF-CNT.
*倉庫コード
     MOVE      ZAI-F01      TO      CSV-F01.
*商品コード
     MOVE      ZAI-F021     TO      CSV-F02.
*品単コード　　　　　　
     MOVE      ZAI-F022     TO      CSV-F03.
*_番１
     MOVE      ZAI-F031     TO      CSV-F04.
*_番２
     MOVE      ZAI-F032     TO      CSV-F05.
*_番３
     MOVE      ZAI-F033     TO      CSV-F06.
*現在庫数
     MOVE      ZAI-F04      TO      CSV-F07.
     IF        ZAI-F04 < ZERO
               MOVE  "-"    TO      CSV-F07A
     ELSE
               MOVE  "0"    TO      CSV-F07A
     END-IF.
*前月末在庫数
     MOVE      ZAI-F05      TO      CSV-F08.
     IF        ZAI-F05 < ZERO
               MOVE  "-"    TO      CSV-F08A
     ELSE
               MOVE  "0"    TO      CSV-F08A
     END-IF.
*当月入出庫数
     MOVE      ZAI-F06      TO      CSV-F09.
     IF        ZAI-F06 < ZERO
               MOVE  "-"    TO      CSV-F09A
     ELSE
               MOVE  "0"    TO      CSV-F09A
     END-IF.
*当月入庫数　
     MOVE      ZAI-F07      TO      CSV-F10.
     IF        ZAI-F07 < ZERO
               MOVE  "-"    TO      CSV-F10A
     ELSE
               MOVE  "0"    TO      CSV-F10A
     END-IF.
*当月出庫数　
     MOVE      ZAI-F08      TO      CSV-F11.
     IF        ZAI-F08 < ZERO
               MOVE  "-"    TO      CSV-F11A
     ELSE
               MOVE  "0"    TO      CSV-F11A
     END-IF.
*当月売上数
     MOVE      ZAI-F09      TO      CSV-F12.
     IF        ZAI-F09 < ZERO
               MOVE  "-"    TO      CSV-F12A
     ELSE
               MOVE  "0"    TO      CSV-F12A
     END-IF.
*当月返品数　　　　　　
     MOVE      ZAI-F10      TO      CSV-F13.
     IF        ZAI-F10 < ZERO
               MOVE  "-"    TO      CSV-F13A
     ELSE
               MOVE  "0"    TO      CSV-F13A
     END-IF.
*次月入庫数
     MOVE      ZAI-F11      TO      CSV-F14.
     IF        ZAI-F11 < ZERO
               MOVE  "-"    TO      CSV-F14A
     ELSE
               MOVE  "0"    TO      CSV-F14A
     END-IF.
*次月出庫数
     MOVE      ZAI-F12      TO      CSV-F15.
     IF        ZAI-F12 < ZERO
               MOVE  "-"    TO      CSV-F15A
     ELSE
               MOVE  "0"    TO      CSV-F15A
     END-IF.
*次月売上数
     MOVE      ZAI-F13      TO      CSV-F16.
     IF        ZAI-F13 < ZERO
               MOVE  "-"    TO      CSV-F16A
     ELSE
               MOVE  "0"    TO      CSV-F16A
     END-IF.
*次月返品数
     MOVE      ZAI-F14      TO      CSV-F17.
     IF        ZAI-F14 < ZERO
               MOVE  "-"    TO      CSV-F17A
     ELSE
               MOVE  "0"    TO      CSV-F17A
     END-IF.
*入庫数　　　
     MOVE      ZAI-F15      TO      CSV-F18.
     IF        ZAI-F15 < ZERO
               MOVE  "-"    TO      CSV-F18A
     ELSE
               MOVE  "0"    TO      CSV-F18A
     END-IF.
*出庫数　　　
     MOVE      ZAI-F16      TO      CSV-F19.
     IF        ZAI-F16 < ZERO
               MOVE  "-"    TO      CSV-F19A
     ELSE
               MOVE  "0"    TO      CSV-F19A
     END-IF.
*前月末在庫　
     MOVE      ZAI-F17      TO      CSV-F20.
     IF        ZAI-F17 < ZERO
               MOVE  "-"    TO      CSV-F20A
     ELSE
               MOVE  "0"    TO      CSV-F20A
     END-IF.
*社内発注数　
     MOVE      ZAI-F18      TO      CSV-F21.
     IF        ZAI-F18 < ZERO
               MOVE  "-"    TO      CSV-F21A
     ELSE
               MOVE  "0"    TO      CSV-F21A
     END-IF.
*社内発注消込数
     MOVE      ZAI-F19      TO      CSV-F22.
     IF        ZAI-F19 < ZERO
               MOVE  "-"    TO      CSV-F22A
     ELSE
               MOVE  "0"    TO      CSV-F22A
     END-IF.
*当月発注数　　　　　　
     MOVE      ZAI-F20      TO      CSV-F23.
     IF        ZAI-F20 < ZERO
               MOVE  "-"    TO      CSV-F23A
     ELSE
               MOVE  "0"    TO      CSV-F23A
     END-IF.
*今回発注数
     MOVE      ZAI-F21      TO      CSV-F24.
     IF        ZAI-F21 < ZERO
               MOVE  "-"    TO      CSV-F24A
     ELSE
               MOVE  "0"    TO      CSV-F24A
     END-IF.
*予備３
     MOVE      ZAI-F22      TO      CSV-F25.
     IF        ZAI-F22 < ZERO
               MOVE  "-"    TO      CSV-F25A
     ELSE
               MOVE  "0"    TO      CSV-F25A
     END-IF.
*発注数量１
     MOVE      ZAI-F23      TO      CSV-F26.
     IF        ZAI-F23 < ZERO
               MOVE  "-"    TO      CSV-F26A
     ELSE
               MOVE  "0"    TO      CSV-F26A
     END-IF.
*発注数量２
     MOVE      ZAI-F24      TO      CSV-F27.
     IF        ZAI-F24 < ZERO
               MOVE  "-"    TO      CSV-F27A
     ELSE
               MOVE  "0"    TO      CSV-F27A
     END-IF.
*発注数量３　
     MOVE      ZAI-F25      TO      CSV-F28.
     IF        ZAI-F25 < ZERO
               MOVE  "-"    TO      CSV-F28A
     ELSE
               MOVE  "0"    TO      CSV-F28A
     END-IF.
*未入庫数　　
     MOVE      ZAI-F26      TO      CSV-F29.
     IF        ZAI-F26 < ZERO
               MOVE  "-"    TO      CSV-F29A
     ELSE
               MOVE  "0"    TO      CSV-F29A
     END-IF.
*未出庫数　　
     MOVE      ZAI-F27      TO      CSV-F30.
     IF        ZAI-F27 < ZERO
               MOVE  "-"    TO      CSV-F30A
     ELSE
               MOVE  "0"    TO      CSV-F30A
     END-IF.
*引当済数　　
     MOVE      ZAI-F28      TO      CSV-F31.
     IF        ZAI-F27 < ZERO
               MOVE  "-"    TO      CSV-F30A
     ELSE
               MOVE  "0"    TO      CSV-F30A
     END-IF.
*取引先コード　
     MOVE      ZAI-F29      TO      CSV-F32.
*カナ名称　　
     MOVE      ZAI-F30      TO      CSV-F33.
*登録日　　　
     MOVE      ZAI-F98      TO      CSV-F34.
*更新日
     MOVE      ZAI-F99      TO      CSV-F35.
* 2011/10/11,S  S.I/NAV
*商品名日本語

     IF  FG-MEIMSF-INV = ZERO
         MOVE  SPACE        TO  CSV-F36
         STRING  X"28" MEI-F02 X"29"
           DELIMITED BY SIZE  INTO  CSV-F36
     ELSE
         MOVE  SPACE        TO  CSV-F36
     END-IF.
*引当可能在庫数
**   ZAI-F04  現在庫数
**   ZAI-F27  未出庫数

     COMPUTE  CSV-F37  =  ZAI-F04  -  ZAI-F27.
     IF  ZAI-F04  -  ZAI-F27  <  ZERO *> マイナス
         MOVE  "-"          TO  CSV-F37A
     ELSE
         MOVE  "0"          TO  CSV-F37A
     END-IF.
* 2011/10/11,E  S.I/NAV
*レコード転送
     MOVE      CSV-REC      TO      SVVZAMNF-REC.
*レコード出力
     WRITE     SVVZAMNF-REC.

 SVVZAMNF-WRITE-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF   SVVZAMNF  ZAMZAIF.
* 2011/10/11,S  S.I/NAV
     CLOSE  MEIMSF.
* 2011/10/11,E  S.I/NAV

     IF    ZAMZAIF-CNT NOT = ZERO
     DISPLAY  " ZAMZAIF-READ-CNT = " ZAMZAIF-CNT UPON CONS
     DISPLAY  " SVVZAMNF-WRITE-CNT = " SVVZAMNF-CNT UPON CONS
     END-IF.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
