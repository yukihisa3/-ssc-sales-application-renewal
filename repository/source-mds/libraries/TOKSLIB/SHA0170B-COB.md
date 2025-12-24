# SHA0170B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SHA0170B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　自動発注システム　　　　　　　　　*
*    モジュール名　　　　：　自動発注確定更新                  *
*    作成日／更新日　　　：　00/07/12                          *
*    作成者／更新者　　　：　ＮＡＶ                            *
*    処理概要　　　　　　：　自動発注ヘッダ、明細ワークを読み、*
*                          発注ヘッダ、明細へ出力する。また、  *
*                          自動発注ヘッダワークの発注_より、  *
*                          自動発注ワークを読み削除を行う。    *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SHA0170B.
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
         YA-21     IS   YA-21
         YB        IS   YB
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
***-<<  画面ファイル  >>************************************
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        SYMBOLIC  DESTINATION    IS  "DSP"
                        PROCESSING MODE     IS   DSP-PROC
                        GROUP               IS   DSP-GROUP
                        FORMAT              IS   DSP-FORMAT
                        SELECTED  FUNCTION  IS   DSP-FUNC
                        FILE      STATUS    IS   DSP-STATUS.
*
***-<<  自動発注（ヘッダ）  >>*************************
     SELECT   AUTHEDF   ASSIGN    TO        DA-01-VI-AUTHEDL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   AUH-F02
                        FILE      STATUS    IS   AUH-STATUS.
*
***-<<  自動発注（明細）    >>*************************
     SELECT   AUTMEIF   ASSIGN    TO        DA-01-VI-AUTMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   AUM-F02
                                                 AUM-F03
                        FILE      STATUS    IS   AUM-STATUS.
*
***-<<  発注ファイル（ヘッダ）  >>*************************
     SELECT   HACHEDF   ASSIGN    TO        DA-01-VI-HACHEDL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   HED-F02
                        FILE      STATUS    IS   HED-STATUS.
*
***-<<  発注ファイル（明細）    >>*************************
     SELECT   HACMEIF   ASSIGN    TO        DA-01-VI-HACMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   BDY-F02
                                                 BDY-F03
                        FILE      STATUS    IS   BDY-STATUS.
*
****<<  自動発注ワーク  >>***********************************
     SELECT   AUTHACF   ASSIGN    TO        DA-01-VI-AUTHACL2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   AUT-F15
                                                 AUT-F16
                                            WITH DUPLICATES
                        FILE      STATUS    IS   AUT-STATUS.
*
****<<  条件ファイル    >>**********************************
     SELECT   JYOKEN    ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
****<<  商品在庫マスタ  >>*************************************
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-STATUS.
*
****<<  商品名称マスタ  >>*************************************
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-STATUS.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル  >>************************************
 FD  DSPF.
     COPY     FHA01701  OF        XMDLIB
              JOINING   DSP       PREFIX.
****<< 自動発注（ヘッダ） >>****************************
 FD  AUTHEDF.
     COPY     AUTHEDF   OF        XFDLIB
              JOINING   AUH       PREFIX.
****<< 自動発注（明細）   >>****************************
 FD  AUTMEIF.
     COPY     AUTMEIF   OF        XFDLIB
              JOINING   AUM       PREFIX.
****<< 発注ファイル（ヘッダ） >>****************************
 FD  HACHEDF.
     COPY     HACHEDF   OF        XFDLIB
              JOINING   HED       PREFIX.
****<< 発注ファイル（明細）   >>****************************
 FD  HACMEIF.
     COPY     HACMEIF   OF        XFDLIB
              JOINING   BDY       PREFIX.
****<< 自動発注ワーク   >>**********************************
 FD  AUTHACF.
     COPY     AUTHACF   OF        XFDLIB
              JOINING   AUT       PREFIX.
****<< 条件ファイル         >>******************************
 FD  JYOKEN.
     COPY     JYOKEN1   OF        XFDLIB
              JOINING   JYO       PREFIX.
****<< 商品在庫マスタ   >>**********************************
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
****<< 商品名称マスタ   >>**********************************
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  画面制御項目  ****
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 AUH-STATUS           PIC  X(2).
     02 AUM-STATUS           PIC  X(2).
     02 HED-STATUS           PIC  X(2).
     02 BDY-STATUS           PIC  X(2).
     02 JYO-STATUS           PIC  X(2).
     02 AUT-STATUS           PIC  X(2).
     02 ZAI-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
**** 日付／時刻
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
**** 画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
**** 画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
*
 01  WK-SDATE                PIC  9(08).
 01  WK-SDATE-R   REDEFINES   WK-SDATE.
     03  FILLER              PIC  X(02).
     03  WK-SYMD.
       05  WK-SYY            PIC  9(02).
       05  WK-SMM            PIC  9(02).
       05  WK-SDD            PIC  9(02).
 01  WK-EDATE                PIC  9(08).
 01  WK-EDATE-R   REDEFINES   WK-EDATE.
     03  FILLER              PIC  X(02).
     03  WK-EYMD.
       05  WK-EYY            PIC  9(02).
       05  WK-EMM            PIC  9(02).
       05  WK-EDD            PIC  9(02).
*
****  カウンタ                ****
 01  IN-HED                  PIC  9(7)   VALUE  0.
 01  IN-BDY                  PIC  9(7)   VALUE  0.
 01  OT-HED                  PIC  9(7)   VALUE  0.
 01  OT-BDY                  PIC  9(7)   VALUE  0.
 01  DEL-CNT                 PIC  9(7)   VALUE  0.
 01  ZAI-OUT                 PIC  9(7)   VALUE  0.
 01  ZAI-UPD                 PIC  9(7)   VALUE  0.
****  フラグ                  ****
 01  PFK-FLG                 PIC  9(01)  VALUE  0.
 01  ERR-FLG                 PIC  9(01)  VALUE  0.
 01  MAIN-FLG                PIC  9(02)  VALUE  0.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  SOK-FLG                 PIC  9(01)  VALUE  1.
 01  HAC-FLG                 PIC  9(01)  VALUE  0.
 01  INV-FLG                 PIC  9(01)  VALUE  ZERO.
 01  INVALID-FLG             PIC  9(01)  VALUE  ZERO.
 01  WK-YOTEI                PIC  9(08)  VALUE  ZERO.
****  インデックス            ****
 01  IXA                     PIC  9(02).
 01  IX                      PIC  9(02).
 01  X                       PIC  9(02).
 01  Z                       PIC  9(02).
*
*メッセージテーブル
 01  MSG-TBL.
     03  MSG-NO01            PIC  N(20)  VALUE
            NC"誤ったＰＦキーが押されました".
     03  MSG-NO02            PIC  N(20)  VALUE
            NC"自動発注処理が開始されていません".
     03  MSG-NO03            PIC  N(20)  VALUE
            NC"＊＊＊　自動発注確定更新中です。　＊＊＊".
 01  TBL-MSG-R   REDEFINES    MSG-TBL.
     03  TBL-MSG             PIC  N(20)  OCCURS  3   TIMES.
*ＰＦキー
 01  PFK-TBL.
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_終了".
 01  TBL-PFK-R       REDEFINES    PFK-TBL.
     03  TBL-PFK             PIC  N(30)  OCCURS  1   TIMES.
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(6).
     02  SYSYMD-R            REDEFINES SYSYMD.
       03  SYS-YY            PIC  99.
       03  SYS-MM            PIC  99.
       03  SYS-DD            PIC  99.
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SHA0170B".
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
***************************************************************
 PROCEDURE               DIVISION.
***************************************************************
*
 DECLARATIVES.
 FILEERR-DSP            SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DSPF.
     MOVE   "DSPF    "        TO    ERR-FL-ID.
     MOVE    DSP-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-AUH            SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  AUTHEDF.
     MOVE   "AUTHEDF "        TO    ERR-FL-ID.
     MOVE    AUH-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-AUM            SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  AUTMEIF.
     MOVE   "HACMEIF "        TO    ERR-FL-ID.
     MOVE    AUM-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-HED            SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACHEDF.
     MOVE   "HACHEDF "        TO    ERR-FL-ID.
     MOVE    HED-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-BDY            SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACMEIF.
     MOVE   "HACMEIF "        TO    ERR-FL-ID.
     MOVE    BDY-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-JYO            SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  JYOKEN.
     MOVE   "JYOKEN  "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERR-AUT            SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  AUTHACF.
     MOVE   "AUTHACF "        TO    ERR-FL-ID.
     MOVE    AUT-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERR-ZAI            SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZAMZAIF.
     MOVE   "ZAMZAIF "        TO    ERR-FL-ID.
     MOVE    ZAI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERR-MEI            SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HMEIMS.
     MOVE   "HMEIMS  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
 SHA0170B-START         SECTION.
     DISPLAY  "**  SHA0170B   START  **"   UPON  CONS.
*
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC     UNTIL   MAIN-FLG = 99.
     PERFORM       END-SEC.
*
     DISPLAY  "**  SHA0170B   END    **"   UPON  CONS.
     STOP     RUN.
 SHA0170B-END.
     EXIT.
************************************************************
*      1.0      初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       DSPF.
     OPEN     INPUT     HMEIMS   AUTHEDF  AUTMEIF   JYOKEN.
     OPEN     I-O       HACHEDF  HACMEIF   AUTHACF  ZAMZAIF.
     ACCEPT   SYSYMD    FROM     DATE.
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
*    納入予定日設定（システム日付＋１日）
     MOVE    "5"                  TO   LINK-IN-KBN.
     MOVE     1                   TO   LINK-IN-YMD6.
     MOVE     SYS-DATE            TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   WK-YOTEI.
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
*特販部名称編集
     MOVE    SPACE               TO        JYO-REC.
     INITIALIZE                            JYO-REC.
     MOVE    "99"                TO        JYO-F01.
     MOVE    "BUMON"             TO        JYO-F02.
     READ    JYOKEN
       INVALID KEY
             MOVE NC"＊＊＊＊＊＊"   TO    HEN-TOKHAN
       NOT INVALID KEY
             MOVE JYO-F03            TO    HEN-TOKHAN
     END-READ.
*初期画面表示へ
     MOVE  1              TO      MAIN-FLG.
 INIT-END.
     EXIT.
************************************************************
*      2.0       メイン処理（全体）                        *
************************************************************
 MAIN-SEC               SECTION.
     EVALUATE  MAIN-FLG
*初期画面表示
         WHEN   1      PERFORM  DSP-INIT-SEC
*確認入力
         WHEN   2      PERFORM  DSP-KAKU-SEC
*更新処理
         WHEN   3      PERFORM  KOSHIN-SEC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
**************************************************************
*    2.1         画面初期表示処理
**************************************************************
 DSP-INIT-SEC       SECTION.
*初期画面の処理
     MOVE  SPACE        TO  DSP-CONTROL.
     MOVE  SPACE        TO  DSP-FHA01701.
*確認入力へ
     MOVE  2            TO  MAIN-FLG.
 DSP-INIT-EXIT.
     EXIT.
************************************************************
*    2.2         確認入力処理
************************************************************
 DSP-KAKU-SEC       SECTION.
*画面表示
     MOVE  1                     TO  PFK-FLG.
     PERFORM  DSP-WRITE-SEC.
*画面入力
     MOVE     "KAKU"            TO  DSP-GROUP.
     PERFORM  DSP-READ-SEC.
*ＰＦ判定
     EVALUATE  DSP-FUNC
         WHEN "E000"
                          PERFORM  CHK-KAKU-SEC
         WHEN "F005"
                          MOVE  99      TO  MAIN-FLG
         WHEN OTHER
                          MOVE  1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
************************************************************
*    2.2.1   確認入力チェック                              *
************************************************************
 CHK-KAKU-SEC   SECTION.
     MOVE  ZERO      TO      ERR-FLG.
*    自動発注区分チェック（１になっているか）
     MOVE    SPACE               TO    JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE    "82"                TO    JYO-F01.
     MOVE    "JIDO"              TO    JYO-F02.
     READ    JYOKEN
       INVALID KEY
             DISPLAY "JYOKEN1 INV 82 JIDO" UPON CONS
             MOVE       99        TO   MAIN-FLG
             GO    TO   CHK-KAKU-EXIT
     END-READ.
*
     IF  JYO-F04   NOT =     1
         MOVE      2    TO   ERR-FLG
     END-IF.
*
     IF  ERR-FLG  =  ZERO
         MOVE     3     TO   ERR-FLG
         PERFORM  DSP-WRITE-SEC
         MOVE     3     TO   MAIN-FLG
     END-IF.
 CHK-KAKU-EXIT.
     EXIT.
************************************************************
*    2.3        更新処理                                   *
************************************************************
 KOSHIN-SEC             SECTION.
*
*    発注ヘッダファイルへ追加
     READ     AUTHEDF
         AT END
              GO   TO   KOSHIN-020
     END-READ.
     ADD      1    TO   IN-HED.
     MOVE     AUH-REC        TO   HED-REC.
*    発注日、納入予定日設定
     MOVE     SYS-DATE       TO   HED-F10.
     MOVE     WK-YOTEI       TO   HED-F11.
     WRITE    HED-REC
         INVALID
              DISPLAY  "HACHEDF WRITE INV KEY=" HED-F02
                                  UPON CONS
         NOT INVALID
              ADD       1    TO   OT-HED
     END-WRITE.
     GO  TO   KOSHIN-SEC.
*
 KOSHIN-020.
*
*    発注明細ファイルへ追加
     READ     AUTMEIF
         AT END
              GO   TO   KOSHIN-030
     END-READ.
     ADD      1    TO   IN-BDY.
*
*****在庫更新の為設定した倉庫コードはクリア
*****MOVE     SPACE          TO   AUM-F97.
*
     MOVE     AUM-REC        TO   BDY-REC.
*    発注日設定
     MOVE     SYS-DATE       TO   BDY-F19.
     WRITE    BDY-REC
         INVALID
              DISPLAY  "HACMEIF WRITE INV KEY=" BDY-F02 ":"
                        BDY-F03   UPON CONS
         NOT INVALID
*             商品在庫マスタの更新
              PERFORM   ZAI-UPD-SEC
              ADD       1    TO   OT-BDY
*             自動発注ワークの削除
              PERFORM   AUTHACF-DEL-SEC
     END-WRITE.
     GO  TO   KOSHIN-020.
*
 KOSHIN-030.
*
     MOVE     99        TO   MAIN-FLG.
*
 KOSHIN-END.
     EXIT.
************************************************************
*    2.3.1    自動発注ワーク削除処理                       *
************************************************************
 AUTHACF-DEL-SEC        SECTION.
*
     MOVE     SPACE     TO   AUT-REC.
     INITIALIZE              AUT-REC.
     MOVE     BDY-F02   TO   AUT-F15.
     MOVE     BDY-F03   TO   AUT-F16.
     READ     AUTHACF
         INVALID
              DISPLAY "AUTHACF READ INV KEY="
                   BDY-F02 ":" BDY-F03   UPON CONS
              GO   TO   AUTHACF-DEL-EXIT
     END-READ.
     DELETE   AUTHACF.
     ADD      1    TO   DEL-CNT.
*
 AUTHACF-DEL-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.3.2        在庫マスタ読込／更新                   *
*----------------------------------------------------------*
 ZAI-UPD-SEC            SECTION.
     MOVE     AUM-F97             TO   ZAI-F01.
     MOVE     AUM-F06             TO   ZAI-F021.
     MOVE     AUM-F07             TO   ZAI-F022.
     MOVE     AUM-F08             TO   ZAI-F03.
*****DISPLAY "AUM-F97 = " AUM-F97 UPON CONS.
*****DISPLAY "AUM-F06 = " AUM-F06 UPON CONS.
*****DISPLAY "AUM-F07 = " AUM-F07 UPON CONS.
*****DISPLAY "AUM-F08 = " AUM-F08 UPON CONS.
*
     READ     ZAMZAIF
       INVALID     KEY
         MOVE      SPACE          TO   ZAI-REC
         INITIALIZE                    ZAI-REC
         MOVE      AUM-F97        TO   ZAI-F01
         MOVE      AUM-F06        TO   ZAI-F021
         MOVE      AUM-F07        TO   ZAI-F022
         MOVE      AUM-F08        TO   ZAI-F03
         PERFORM   MEI-READ-SUB
         IF   INVALID-FLG   =   ZERO
              MOVE      MEI-F031  TO   ZAI-F30
         ELSE
              MOVE      SPACE     TO   ZAI-F30
         END-IF
*        未入庫数・当月発注数
         ADD       AUM-F09        TO   ZAI-F26
         ADD       AUM-F09        TO   ZAI-F20
         MOVE      SYS-DATE       TO   ZAI-F98
         MOVE      SYS-DATE       TO   ZAI-F99
         WRITE     ZAI-REC        END-WRITE
         ADD       1              TO   ZAI-OUT
       NOT INVALID  KEY
*        未入庫数・当月発注数
         ADD       AUM-F09        TO   ZAI-F26
         ADD       AUM-F09        TO   ZAI-F20
*****DISPLAY "AUM-F09 = " AUM-F09 UPON CONS
*****DISPLAY "ZAI-F26 = " ZAI-F26 UPON CONS
*****DISPLAY "ZAI-F20 = " ZAI-F20 UPON CONS
         MOVE      SYS-DATE       TO   ZAI-F99
         REWRITE   ZAI-REC        END-REWRITE
         ADD       1              TO   ZAI-UPD
     END-READ.
 ZAI-UPD-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF      AUTHEDF   AUTMEIF   AUTHACF   HMEIMS
              HACHEDF   HACMEIF   JYOKEN    ZAMZAIF.
*
     DISPLAY "* AUTHEDF (IN)=" IN-HED   " *" UPON CONS.
     DISPLAY "* HACHEDF (OT)=" OT-HED   " *" UPON CONS.
     DISPLAY "* AUTMEIF (IN)=" IN-BDY   " *" UPON CONS.
     DISPLAY "* HACMEIF (OT)=" OT-BDY   " *" UPON CONS.
     DISPLAY "* AUTHACF (DL)=" DEL-CNT  " *" UPON CONS.
     DISPLAY "* ZAMZAIF (OT)=" ZAI-OUT  " *" UPON CONS.
     DISPLAY "* ZAMZAIF (UP)=" ZAI-UPD  " *" UPON CONS.
*
 END-END.
     EXIT.
************************************************************
*      画面表示処理
************************************************************
 DSP-WRITE-SEC          SECTION.
*ＰＦキー設定
     MOVE  TBL-PFK(PFK-FLG)  TO   DSP-PFKEY.
*エラー設定
     IF  ERR-FLG        =    ZERO
         MOVE    SPACE       TO   DSP-ERRMSG
     ELSE
         MOVE    TBL-MSG(ERR-FLG)  TO   DSP-ERRMSG
         MOVE    ZERO        TO   ERR-FLG
     END-IF.
*
     MOVE     HEN-DATE       TO   DSP-SDATE.
     MOVE     HEN-TIME       TO   DSP-STIME.
     MOVE     HEN-TOKHAN-AREA     TO   DSP-TOKHAN.
*
     MOVE    "FHA01701"      TO   DSP-FORMAT.
     MOVE    "SCREEN"        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     WRITE    DSP-FHA01701.
 DSP-WRITE-END.
     EXIT.
************************************************************
*      画面データの入力処理
************************************************************
 DSP-READ-SEC           SECTION.
     MOVE  "NE"      TO   DSP-PROC.
     READ   DSPF.
*
 DSP-READ-END.
     EXIT.
*----------------------------------------------------------*
*                商品名の取得                              *
*----------------------------------------------------------*
 MEI-READ-SUB           SECTION.
     MOVE    AUM-F06         TO   MEI-F01.
     MOVE    AUM-F07         TO   MEI-F012.
     READ    HMEIMS
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 MEI-READ-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
