****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　入荷消込データＣＳＶ出力　　　　　*
*    作成日／作成者　　　：　2022/08/22   INOUE                *
*    処理内容　　　　　　：　画面より出力条件を入力して　　　　*
*    　　　　　　　　　　　　入荷消込データＣＳＶを出力　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NVD0215V.
*                  流用:NVD0210L
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
         YB        IS   YB1
         YB-21     IS   YB2
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
****<< 入庫ファイル         >>******************************
     SELECT   NYKFILF   ASSIGN  TO   DA-01-VI-NYKFILL2
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   SEQUENTIAL
                        RECORD  KEY          IS   NYUK-F27
                                                  NYUK-F02
                                                  NYUK-F03
                                                  NYUK-F04
                                                  NYUK-F05
                        FILE    STATUS       IS   NYUK-STATUS.
****<< 発注 ファイル.ヘッダ  >>*****************************
     SELECT   HACHEDF   ASSIGN  TO   DA-01-VI-HACHEDL1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   HED-F02
                        FILE    STATUS       IS   HED-STATUS.
****<< 発注 ファイル.明細　  >>*****************************
     SELECT   HACMEIF   ASSIGN  TO   DA-01-VI-HACMEIL1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   BDY-F02
                                                  BDY-F03
                        FILE    STATUS       IS   BDY-STATUS.
****<< SUB商品名称マスタ       >>***************************
     SELECT   SUBMEIF   ASSIGN  TO   DA-01-VI-SUBMEIL1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   MEI-F011
                                                  MEI-F012
                        FILE    STATUS       IS   MEI-STATUS.
****<< 仕入先マスタ         >>******************************
     SELECT   ZSHIMS    ASSIGN  TO   DA-01-VI-ZSHIMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   SHI-F01
                        FILE    STATUS       IS   SHI-STATUS.
****<< 取引先マスタ         >>******************************
     SELECT   HTOKMS    ASSIGN  TO   DA-01-VI-TOKMS2
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   TOK-F01
                        FILE    STATUS       IS   TOK-STATUS.
****<< 倉庫マスタ           >>******************************
     SELECT   ZSOKMS    ASSIGN  TO   DA-01-VI-ZSOKMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   SOK-F01
                        FILE    STATUS       IS   SOK-STATUS.
****<< 条件ファイル         >>******************************
     SELECT   HJYOKEN   ASSIGN  TO   DA-01-VI-JYOKEN1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   JYO-F01
                                                  JYO-F02
                        FILE    STATUS       IS   JYO-STATUS.
****<< 担当者マスタ         >>******************************
     SELECT   HTANMS    ASSIGN  TO   DA-01-VI-TANMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   TAN-F01
                                                  TAN-F02
                        FILE    STATUS       IS   TAN-STATUS.
****<< 入荷消込データＣＳＶ >>******************************
     SELECT   NYKESICS  ASSIGN  TO   DA-01-S-NYKESICS
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   CSV-STATUS.
************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     FVD02151          OF   XMDLIB.
****<< 入庫ファイル         >>******************************
 FD    NYKFILF
       LABEL     RECORD    IS    STANDARD.
       COPY      NYKFILF   OF    XFDLIB
                 JOINING   NYUK  PREFIX.
****<< 発注ファイル ヘッダ  >>*****************************
 FD    HACHEDF
       LABEL     RECORD    IS    STANDARD.
       COPY      HACHEDF   OF    XFDLIB
                 JOINING   HED   PREFIX.
****<< 発注ファイル 明細　  >>*****************************
 FD    HACMEIF
       LABEL     RECORD    IS    STANDARD.
       COPY      HACMEIF   OF    XFDLIB
                 JOINING   BDY   PREFIX.
****<< 商品名称マスタ       >>******************************
 FD    SUBMEIF
        LABEL     RECORD    IS    STANDARD.
        COPY      SUBMEIF    OF    XFDLIB
                  JOINING   MEI   PREFIX.
****<< 仕入先マスタ         >>******************************
 FD    ZSHIMS
       LABEL     RECORD    IS    STANDARD.
        COPY      ZSHIMS    OF    XFDLIB
                  JOINING   SHI   PREFIX.
****<< 取引先マスタ         >>******************************
 FD    HTOKMS
        LABEL     RECORD    IS    STANDARD.
        COPY      HTOKMS    OF    XFDLIB
                  JOINING   TOK   PREFIX.
****<< 倉庫マスタ           >>******************************
 FD    ZSOKMS
        LABEL     RECORD    IS    STANDARD.
        COPY      ZSOKMS    OF    XFDLIB
                  JOINING   SOK   PREFIX.
****<< 条件ファイル         >>******************************
 FD    HJYOKEN
        LABEL     RECORD    IS    STANDARD.
        COPY      HJYOKEN   OF    XFDLIB
                  JOINING   JYO   PREFIX.
****<<担当者マスタ         >>******************************
 FD    HTANMS
        LABEL     RECORD    IS    STANDARD.
        COPY      HTANMS    OF    XFDLIB
                  JOINING   TAN   PREFIX.
****<<  入荷消込データＣＳＶ  >>****************************
 FD    NYKESICS.
       COPY     NYKESIC4    OF       XFDLIB
       JOINING  CSV         AS       PREFIX.
*
************************************************************
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ＣＳＶ 帳票タイトル行       ****
 COPY   NYKESIC1   OF        XFDLIB
        JOINING    CSV1      PREFIX.
****  ＣＳＶ ヘッダ行　　　       ****
 COPY   NYKESIC2   OF        XFDLIB
        JOINING    CSV2      PREFIX.
****  ＣＳＶ 項目タイトル行　　   ****
 COPY   NYKESIC3   OF        XFDLIB
        JOINING    CSV3      PREFIX.
****  画面制御項目            ****
 01  DSP-CONTROL.
     02 DSP-PROC             PIC  X(02).
     02 DSP-GROUP            PIC  X(08).
     02 DSP-FORMAT           PIC  X(08).
     02 DSP-STATUS           PIC  X(02).
     02 DSP-FUNC             PIC  X(04).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 NYUK-STATUS          PIC  X(02).
     02 HED-STATUS           PIC  X(02).
     02 BDY-STATUS           PIC  X(02).
     02 TEN-STATUS           PIC  X(02).
     02 MEI-STATUS           PIC  X(02).
     02 SHI-STATUS           PIC  X(02).
     02 TOK-STATUS           PIC  X(02).
     02 SOK-STATUS           PIC  X(02).
     02 JYO-STATUS           PIC  X(02).
     02 TAN-STATUS           PIC  X(02).
     02 CSV-STATUS           PIC  X(02).
***  日付／時刻
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
***  画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
***  画面表示時刻編集
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
****  フラグ                  ****
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE  SPACE.
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(06).
     02  SYSYMD-R            REDEFINES SYSYMD.
       03  SYS-YY            PIC  99.
       03  SYS-MM            PIC  99.
       03  SYS-DD            PIC  99.
****  カウンタ                ****
 01  CNT-AREA.
     02  L-CNT               PIC  9(02)  VALUE  90.
     02  P-CNT               PIC  9(07)  VALUE  ZERO.
     02  READ-CNT            PIC  9(07)  VALUE  ZERO.
     02  SELECT-CNT          PIC  9(07)  VALUE  ZERO.
     02  SKIP-CNT            PIC  9(07)  VALUE  ZERO.
     02  TBL-CNT             PIC  9(01)  VALUE  ZERO.
     02  PRINT-CNT           PIC  9(07)  VALUE  ZERO.
     02  NYKESICS-CNT        PIC  9(08)  VALUE  ZERO.
****  ワーク                  ****
 01  WORK-AREA.
     02  WK-SDATE            PIC  9(08).
     02  WK-SDATE-R   REDEFINES  WK-SDATE.
       03  FILLER            PIC  9(02).
       03  WK-SYMD.
         04  WK-SYY          PIC  9(02).
         04  WK-SMM          PIC  9(02).
         04  WK-SDD          PIC  9(02).
     02  WK-EDATE            PIC  9(08).
     02  WK-EDATE-R   REDEFINES  WK-EDATE.
       03  FILLER            PIC  9(02).
       03  WK-EYMD.
         04  WK-EYY          PIC  9(02).
         04  WK-EMM          PIC  9(02).
         04  WK-EDD          PIC  9(02).
     02  WK-HACHZAN          PIC S9(08)V99      VALUE  ZERO.
     02  WK-L-CNT            PIC  9(02)         VALUE  ZERO.
     02  WK-NYUK-KEY                     VALUE  ZERO.
       03  WK-NYUK-F27       PIC  X(02).
       03  WK-NYUK-F01       PIC  9(02).
       03  WK-NYUK-F02       PIC  9(07).
       03  WK-NYUK-F03       PIC  9(02).
       03  WK-NYUK-F04       PIC  9(01).
     02  WK-HIKAKU-KEY.
       03  WK-HIKA-F27       PIC  X(02).
       03  FILLER            PIC  X(12).
****  インデックス            ****
 01  IX                      PIC  9(01).
****  入庫ファイルテーブル    ****
      COPY      NYKFILF   OF    XFDLIB
                JOINING   TBL   PREFIX.
 01  TBL-NYUKF.
     02  TBL-NYUK        OCCURS  6.
       03  FILLER            PIC  X(200).
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     02  PMSG01              PIC N(20) VALUE
                             NC"_取消　_終了".
     02  PMSG02              PIC N(20) VALUE
                             NC"_取消　_終了　_項目戻り".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NVD0215V".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージコード  ***
 01  CODE-AREA.
     02  ERR-CD              PIC  9(02)  VALUE  ZERO.
****  エラーメッセージ        ***
 01  ERR-TAB.
     02  MSG-NYUK            PIC  N(20)  VALUE
            NC"誤ったＰＦキーが押されました。".
     02  MSG-ERR2            PIC  N(20)  VALUE
            NC"倉庫コードを入力して下さい。".
     02  MSG-ERR3            PIC  N(20)  VALUE
            NC"倉庫コードに誤りがあります。".
     02  MSG-ERR4            PIC  N(20)  VALUE
            NC"指定の倉庫コードは扱えません。".
     02  MSG-ERR5            PIC  N(20)  VALUE
            NC"年月日に誤りがあります。".
     02  MSG-ERR6            PIC  N(20)  VALUE
            NC"計上区分に誤りがあります。".
     02  MSG-ERR7            PIC  N(20)  VALUE
            NC"計上区分は入力できません。".
     02  MSG-ERR8            PIC  N(20)  VALUE
            NC"開始が終了を越えています。".
 01  ERR-MSG-ALL             REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(20)
                             OCCURS   8  TIMES.
****  エラーメッセージ　ＦＯＲ　コンソール  ***
 01  FILE-ERROR.
     02  FILE-NYUK           PIC  N(10)  VALUE
            NC"入庫ファイル　異常！".
     02  FILE-ERR2           PIC  N(10)  VALUE
            NC"発注（ヘッダ）異常！".
     02  FILE-ERR3           PIC  N(10)  VALUE
            NC"店舗マスタ　　異常！".
     02  FILE-ERR4           PIC  N(10)  VALUE
            NC"商品名称マスタ異常！".
     02  FILE-ERR5           PIC  N(10)  VALUE
            NC"仕入先マスタ　異常！".
     02  FILE-ERR6           PIC  N(10)  VALUE
            NC"取引先マスタ　異常！".
     02  FILE-ERR7           PIC  N(10)  VALUE
            NC"倉庫マスタ　　異常！".
     02  FILE-ERR8           PIC  N(10)  VALUE
            NC"条件ファイル　異常！".
     02  FILE-ERR9           PIC  N(10)  VALUE
            NC"画面ファイル　異常！".
     02  FILE-NYUK0          PIC  N(10)  VALUE
            NC"担当者マスタ　異常！".
***  日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
**** パラメータ               ****
 LINKAGE                SECTION.
 01  LINK-SOKCD         PIC  X(02).
 01  LINK-DSOKCD        PIC  X(02).
 01  LINK-BUMON         PIC  X(04).
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION      USING   LINK-SOKCD
                                              LINK-DSOKCD
                                              LINK-BUMON.
*
 DECLARATIVES.
**入庫ファイル
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NYKFILF.
     MOVE   "NYKFILF "        TO    ERR-FL-ID.
     MOVE    NYUK-STATUS      TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**発注ファイル（ヘッダ）
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACHEDF.
     MOVE   "HACHEDF "        TO    ERR-FL-ID.
     MOVE    HED-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**発注ファイル（明細）
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACMEIF.
     MOVE   "HACHEDF "        TO    ERR-FL-ID.
     MOVE    BDY-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**商品名称マスタ
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBMEIF.
     MOVE   "SUBMEIF  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**仕入先マスタ
 FILEERR-SEC6           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSHIMS.
     MOVE   "ZSHIMS  "        TO    ERR-FL-ID.
     MOVE    SHI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**取引先マスタ
 FILEERR-SEC7           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HTOKMS.
     MOVE   "HTOKMS  "        TO    ERR-FL-ID.
     MOVE    TOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**倉庫マスタ
 FILEERR-SEC8           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS  "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**条件ファイル
 FILEERR-SEC9           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HJYOKEN.
     MOVE   "HJYOKEN "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**画面ファイル
 FILEERR-SEC10          SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DSPF.
     MOVE   "DSPF    "        TO    ERR-FL-ID.
     MOVE    DSP-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**担当者マスタ
 FILEERR-SEC11          SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HTANMS.
     MOVE   "HTANMS  "        TO    ERR-FL-ID.
     MOVE    TAN-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
**入荷消込データＣＳＶ
 FILEERR-SEC12          SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NYKESICS.
     MOVE   "NYKESICS"        TO    ERR-FL-ID.
     MOVE    CSV-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
 FNY00201B-START         SECTION.
     DISPLAY  "**  NVD0215V   START  **"   UPON  CONS.
*
*    MOVE  "01"          TO   LINK-SOKCD
*    MOVE  "01"          TO   LINK-DSOKCD
*    MOVE  "01"          TO   LINK-BUMON.
*
*    DISPLAY "倉庫CD    =" LINK-SOKCD
*    DISPLAY "代表倉庫CD=" LINK-DSOKCD
*    DISPLAY "部門      =" LINK-BUMON
*
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC      UNTIL  END-FLG  =  "END".
     PERFORM       END-SEC.
*
     DISPLAY  "**  NVD0215V    END   **"   UPON  CONS.
     STOP     RUN.
 FNY00201B-END.
     EXIT.
************************************************************
*      ■０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       DSPF.
     OPEN     INPUT     NYKFILF.
     OPEN     INPUT     HACHEDF.
     OPEN     INPUT     HACMEIF.
     OPEN     INPUT     SUBMEIF.
     OPEN     INPUT     ZSHIMS.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     ZSOKMS.
     OPEN     INPUT     HJYOKEN.
     OPEN     INPUT     HTANMS.
     OPEN     OUTPUT    NYKESICS.
*---- システム日付・時刻の取得
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
*---- 画面・帳票日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*---- システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*---- 画面時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
*---- システム日付　取得
     ACCEPT   SYSYMD    FROM   DATE.
*特販部名称編集
     MOVE     SPACE               TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE    "99"                 TO   JYO-F01.
     MOVE    "BUMON"              TO   JYO-F02.
     READ     HJYOKEN
       INVALID KEY
              MOVE NC"＊＊＊＊＊＊"    TO    HEN-TOKHAN
       NOT INVALID KEY
              MOVE JYO-F03             TO    HEN-TOKHAN
     END-READ.
*----MOVE     HEN-TOKHAN-AREA     TO   M-TOKHAN.
*
*出力条件指定
     PERFORM     WRITE-SELECT-SEC.
*
*入庫ファイル読み込み
     IF    END-FLG  NOT =   "END"
        PERFORM     NYKFILF-START-SEC
     END-IF.
     IF   END-FLG = "END"
          DISPLAY NC"対象データなし" UPON CONS
          GO                    TO   INIT-SEC-EXIT
     END-IF.
     IF    END-FLG  NOT =   "END"
        PERFORM     NYKFILF-READ-SEC
     END-IF.
     IF   END-FLG = "END"
          DISPLAY NC"対象データなし" UPON CONS
          GO                    TO   INIT-SEC-EXIT
     END-IF.
*
 021-INIT.
*    帳票タイトルレコード出力
     MOVE     SPACE                   TO       CSV1-REC.
     MOVE     X"28"                   TO       CSV1-CS01.
     MOVE     NC"＜入荷消込ＣＳＶ＞"  TO       CSV1-C01.
     MOVE     X"29"                   TO       CSV1-CE01.
     MOVE     ","                     TO       CSV1-CK01
     WRITE    CSV-REC                 FROM     CSV1-REC.
*
 022-INIT.
*    ヘッダレコード出力
     MOVE     SPACE                   TO       CSV2-REC.
     MOVE     X"28"                   TO       CSV2-HS01.
     MOVE     NC"日付："              TO       CSV2-H01.
     MOVE     X"29"                   TO       CSV2-HE01.
     MOVE     ","                     TO       CSV2-HK01.
     MOVE     SYS-DATE(1:4)           TO       CSV2-H02(1:4).
     MOVE     "/"                     TO       CSV2-H02(5:1).
     MOVE     SYS-DATE(5:2)           TO       CSV2-H02(6:2).
     MOVE     "/"                     TO       CSV2-H02(8:1).
     MOVE     SYS-DATE(7:2)           TO       CSV2-H02(9:2).
     MOVE     ","                     TO       CSV2-HK02.
     MOVE     X"28"                   TO       CSV2-HS03.
     MOVE     NC"時刻："              TO       CSV2-H03.
     MOVE     X"29"                   TO       CSV2-HE03.
     MOVE     ","                     TO       CSV2-HK03.
     MOVE     WK-TIME(1:2)            TO       CSV2-H04(1:2).
     MOVE     ":"                     TO       CSV2-H04(3:1).
     MOVE     WK-TIME(3:2)            TO       CSV2-H04(4:2).
     MOVE     ":"                     TO       CSV2-H04(6:1).
     MOVE     WK-TIME(5:2)            TO       CSV2-H04(7:2).
     MOVE     ","                     TO       CSV2-HK04.
     WRITE    CSV-REC                 FROM     CSV2-REC.
*
 023-INIT.
*    項目タイトルレコード出力
     MOVE     SPACE                   TO       CSV3-REC.
     MOVE     X"28"                   TO       CSV3-KS01
                                               CSV3-KS02
                                               CSV3-KS03
                                               CSV3-KS04
                                               CSV3-KS05
                                               CSV3-KS06
                                               CSV3-KS07
                                               CSV3-KS08
                                               CSV3-KS09
                                               CSV3-KS10
                                               CSV3-KS11
                                               CSV3-KS12
                                               CSV3-KS13
                                               CSV3-KS14
                                               CSV3-KS15
                                               CSV3-KS16
                                               CSV3-KS17
                                               CSV3-KS18
                                               CSV3-KS19
                                               CSV3-KS20
                                               CSV3-KS21
                                               CSV3-KS22
                                               CSV3-KS23
                                               CSV3-KS24
                                               CSV3-KS25
                                               CSV3-KS26
                                               CSV3-KS27.
     MOVE     X"29"                   TO       CSV3-KE01
                                               CSV3-KE02
                                               CSV3-KE03
                                               CSV3-KE04
                                               CSV3-KE05
                                               CSV3-KE06
                                               CSV3-KE07
                                               CSV3-KE08
                                               CSV3-KE09
                                               CSV3-KE10
                                               CSV3-KE11
                                               CSV3-KE12
                                               CSV3-KE13
                                               CSV3-KE14
                                               CSV3-KE15
                                               CSV3-KE16
                                               CSV3-KE17
                                               CSV3-KE18
                                               CSV3-KE19
                                               CSV3-KE20
                                               CSV3-KE21
                                               CSV3-KE22
                                               CSV3-KE23
                                               CSV3-KE24
                                               CSV3-KE25
                                               CSV3-KE26
                                               CSV3-KE27.
     MOVE     ","                     TO       CSV3-KK01
                                               CSV3-KK02
                                               CSV3-KK03
                                               CSV3-KK04
                                               CSV3-KK05
                                               CSV3-KK06
                                               CSV3-KK07
                                               CSV3-KK08
                                               CSV3-KK09
                                               CSV3-KK10
                                               CSV3-KK11
                                               CSV3-KK12
                                               CSV3-KK13
                                               CSV3-KK14
                                               CSV3-KK15
                                               CSV3-KK16
                                               CSV3-KK17
                                               CSV3-KK18
                                               CSV3-KK19
                                               CSV3-KK20
                                               CSV3-KK21
                                               CSV3-KK22
                                               CSV3-KK23
                                               CSV3-KK24
                                               CSV3-KK25
                                               CSV3-KK26
                                               CSV3-KK27.
     MOVE   NC"倉庫ＣＤ"               TO      CSV3-K01.
     MOVE   NC"倉庫名"                 TO      CSV3-K02.
     MOVE   NC"伝区"                   TO      CSV3-K03.
     MOVE   NC"伝区名"                 TO      CSV3-K04.
     MOVE   NC"納品日"                 TO      CSV3-K05.
     MOVE   NC"発注ＮＯ"               TO      CSV3-K06.
     MOVE   NC"枝ＮＯ"                 TO      CSV3-K07.
     MOVE   NC"発注日"                 TO      CSV3-K08.
     MOVE   NC"仕入先ＣＤ"             TO      CSV3-K09.
     MOVE   NC"仕入先名"               TO      CSV3-K10.
     MOVE   NC"ＤＴ種別"               TO      CSV3-K11.
     MOVE   NC"ＤＴ種別名"             TO      CSV3-K12.
     MOVE   NC"Ｄ３６５発注ＮＯ"       TO      CSV3-K13.
     MOVE   NC"担当者ＣＤ"             TO      CSV3-K14.
     MOVE   NC"担当者名"               TO      CSV3-K15.
     MOVE   NC"登録日"                 TO      CSV3-K16.
     MOVE   NC"更新日"                 TO      CSV3-K17.
     MOVE   NC"行ＮＯ"                 TO      CSV3-K18.
     MOVE   NC"完納"                   TO      CSV3-K19.
     MOVE   NC"サカタ商品ＣＤ"         TO      CSV3-K20.
     MOVE   NC"サカタ品単ＣＤ"         TO      CSV3-K21.
     MOVE   NC"商品名"                 TO      CSV3-K22.
     MOVE   NC"発注数"                 TO      CSV3-K23.
     MOVE   NC"入荷数"                 TO      CSV3-K24.
     MOVE   NC"発注残数"               TO      CSV3-K25.
     MOVE   NC"_番"                   TO      CSV3-K26.
     MOVE   NC"ＪＡＮＣＤ"             TO      CSV3-K27.
     WRITE  CSV-REC                    FROM    CSV3-REC.
*
 INIT-SEC-EXIT.
     EXIT.
****************************************************************
*      ■１　　出力条件指定　処理                              *
****************************************************************
 WRITE-SELECT-SEC            SECTION.
*----- 初画面出力 ------*
     PERFORM    DSP-INIT-SEC.
*
 WS010.
*----- 倉庫ＣＤプロテクト ----*
     IF    LINK-DSOKCD  NOT =  "01"  AND  "88"
       MOVE  "X"                 TO  EDIT-STATUS  OF  SOKCD
       MOVE  "X"                 TO  EDIT-STATUS  OF  KEIJYO
     END-IF.
*----- 画面ＲＥＡＤ ----*
     MOVE    "BODY"              TO   DSP-GROUP.
     PERFORM    DSP-READ-SEC.
*----- ＰＦキー判定　_１ ----*
     PERFORM    OPT-CLR-SEC.
     EVALUATE      DSP-FUNC
         WHEN     "F004"
             GO                  TO   WRITE-SELECT-SEC
         WHEN     "F005"
             MOVE    "END"       TO   END-FLG
             MOVE    4010        TO   PROGRAM-STATUS
             GO                  TO   WRITE-SELECT-END
         WHEN     "E000"
             CONTINUE
         WHEN      OTHER
             MOVE    1           TO   ERR-CD
             PERFORM    DSP-ERR-SEC
             GO                  TO   WS010
     END-EVALUATE.
 WS111.
*----- 入力項目チェック---*
*（倉庫チェック）*
     IF  LINK-DSOKCD  =   "01"  OR  "88"
         IF   SOKCD   =   SPACE
             MOVE    NC"全倉庫"      TO   SOKNM
         ELSE
             MOVE    SOKCD           TO   SOK-F01
             READ    ZSOKMS
                 INVALID KEY
                   MOVE  SPACE   TO   SOKNM
                   MOVE  "R"     TO   EDIT-OPTION   OF   SOKCD
                   MOVE  "C"     TO   EDIT-CURSOR   OF   SOKCD
                   IF   ERR-CD    =    ZERO
                      MOVE    3      TO   ERR-CD
                   END-IF
               NOT INVALID KEY
                   MOVE    SOK-F02   TO   SOKNM
             END-READ
         END-IF
     END-IF.
 WS112.
*
*（開始年月日チェック）*
     MOVE     SYY                 TO   WK-SYY.
     MOVE     SMM                 TO   WK-SMM.
     MOVE     SDD                 TO   WK-SDD.
     MOVE     "1"                 TO   LINK-IN-KBN.
     MOVE     WK-SYMD             TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET    =  ZERO
           MOVE   "3"             TO   LINK-IN-KBN
           MOVE   WK-SYMD         TO   LINK-IN-YMD6
           CALL   "SKYDTCKB"   USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
           MOVE   LINK-OUT-YMD    TO   WK-SDATE
     ELSE
           MOVE   ZERO            TO   WK-SDATE
           MOVE    "R"       TO   EDIT-OPTION   OF   SYY
                                  EDIT-OPTION   OF   SMM
                                  EDIT-OPTION   OF   SDD
           MOVE    "C"       TO   EDIT-CURSOR   OF   SYY
           IF  ERR-CD    =    ZERO
               MOVE   5      TO    ERR-CD
           END-IF
     END-IF.
*（終了年月日チェック）*
     MOVE     EYY                 TO   WK-EYY.
     MOVE     EMM                 TO   WK-EMM.
     MOVE     EDD                 TO   WK-EDD.
     MOVE     "1"                 TO   LINK-IN-KBN.
     MOVE     WK-EYMD             TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET    =  ZERO
           MOVE   "3"             TO   LINK-IN-KBN
           MOVE   WK-EYMD         TO   LINK-IN-YMD6
           CALL   "SKYDTCKB"   USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
           MOVE   LINK-OUT-YMD    TO   WK-EDATE
     ELSE
           MOVE   ZERO            TO   WK-EDATE
           MOVE    "R"       TO   EDIT-OPTION   OF   EYY
                                  EDIT-OPTION   OF   EMM
                                  EDIT-OPTION   OF   EDD
           MOVE    "C"       TO   EDIT-CURSOR   OF   EYY
           IF  ERR-CD    =    ZERO
               MOVE   5      TO    ERR-CD
           END-IF
     END-IF.
*（開始・終了の関連チェック）
     IF       WK-SDATE  NOT =  ZERO
         AND  WK-EDATE  NOT =  ZERO
         AND  WK-SDATE    >    WK-EDATE
           MOVE    "R"       TO   EDIT-OPTION   OF   SYY
                                  EDIT-OPTION   OF   SMM
                                  EDIT-OPTION   OF   SDD
                                  EDIT-OPTION   OF   EYY
                                  EDIT-OPTION   OF   EMM
                                  EDIT-OPTION   OF   EDD
           MOVE    "C"       TO   EDIT-CURSOR   OF   SYY
           IF  ERR-CD    =    ZERO
               MOVE   8      TO    ERR-CD
           END-IF
     END-IF.
*
*（計上のチェック）*
     IF  KEIJYO      NOT    NUMERIC
         IF    LINK-DSOKCD   =   "01"  OR  "88"
               MOVE     ZERO      TO   KEIJYO
         ELSE
               MOVE     9         TO   KEIJYO
         END-IF
     END-IF.
     IF  LINK-DSOKCD    =   "01"  OR  "88"
         IF  KEIJYO      NOT =       ZERO AND 1 AND 9
             MOVE    "R"         TO   EDIT-OPTION   OF   KEIJYO
             MOVE    "C"         TO   EDIT-CURSOR   OF   KEIJYO
             IF       ERR-CD    =    ZERO
                      MOVE    6      TO   ERR-CD
             END-IF
         END-IF
     END-IF.
     IF (LINK-DSOKCD  NOT =       "01"  AND  "88" )  AND
         KEIJYO       NOT =       9
         MOVE    "R"         TO   EDIT-OPTION   OF   KEIJYO
         MOVE    "C"         TO   EDIT-CURSOR   OF   KEIJYO
         IF       ERR-CD    =    ZERO
                  MOVE    7      TO   ERR-CD
         END-IF
     END-IF.
*
*----- エラーが有った時 ----*
     IF   ERR-CD  NOT =   ZERO
       MOVE    "BODY"        TO   DSP-GROUP
       PERFORM    DSP-ERR-SEC
       GO                    TO   WS010
     END-IF.
*
*----- ボディー部の出力 ----*
     PERFORM    OPT-CLR-SEC.
     MOVE    PMSG02          TO   PFKGID.
     MOVE    "BODY"          TO   DSP-GROUP.
     MOVE    SPACE           TO   ERRMSG.
     PERFORM    DSP-WRITE-SEC.
*----- 確認画面の出力-----*
     MOVE    "TAIL"              TO   DSP-GROUP.
     PERFORM    DSP-WRITE-SEC.
*----- 確認画面の入力-----*
 WS020.
     MOVE    "TAIL"              TO   DSP-GROUP.
     PERFORM    DSP-READ-SEC.
*----- ＰＦキー判定　_２ ----*
     EVALUATE      DSP-FUNC
         WHEN     "F004"
             GO                  TO   WRITE-SELECT-SEC
         WHEN     "F005"
             MOVE    "END"       TO   END-FLG
             GO                  TO   WRITE-SELECT-END
         WHEN     "F006"
             GO                  TO   WS010
         WHEN     "E000"
             CONTINUE
         WHEN      OTHER
             MOVE    1           TO   ERR-CD
             MOVE    "TAIL"      TO   DSP-GROUP
             PERFORM    DSP-ERR-SEC
             GO                  TO   WS020
     END-EVALUATE.
 WRITE-SELECT-END.
     EXIT.
****************************************************************
*      ■■１　初期画面　出力　処理                            *
****************************************************************
 DSP-INIT-SEC           SECTION.
     MOVE    SPACE               TO   FVD02151.
     MOVE    SPACE               TO   DSP-CONTROL.
     MOVE    "CL"                TO   DSP-PROC.
     MOVE    "FVD02151"          TO   DSP-FORMAT.
     MOVE    "SCREEN"            TO   DSP-GROUP.
*
     MOVE    SYS-YY              TO   SYY  EYY.
     MOVE    SYS-MM              TO   SMM  EMM.
     MOVE    SYS-DD              TO   SDD  EDD.
     MOVE    PMSG01              TO   PFKGID.
*
     IF      LINK-DSOKCD    NOT =    "01"  AND  "88"
         MOVE    LINK-SOKCD      TO   SOK-F01   SOKCD
         READ    ZSOKMS
             INVALID KEY
               CONTINUE
             NOT INVALID KEY
               MOVE    SOK-F02   TO   SOKNM
         END-READ
         MOVE    9               TO   KEIJYO
     END-IF.
     MOVE    HEN-DATE            TO   SDATE.
     MOVE    HEN-TIME            TO   STIME.
     MOVE    HEN-TOKHAN-AREA     TO   TOKHAN.
*
     WRITE                  FVD02151.
*
 DSP-INIT-END.
     EXIT.
****************************************************************
*      ■■２　画面ＲＥＡＤ　処理                              *
****************************************************************
 DSP-READ-SEC           SECTION.
     MOVE    "NE"                TO   DSP-PROC.
     READ    DSPF
         AT   END
             GO                  TO   DSP-READ-END
     END-READ.
     IF   DSP-STATUS   NOT =   ZERO
         DISPLAY   FILE-ERR9   UPON   CONS
     END-IF.
 DSP-READ-END.
     EXIT.
****************************************************************
*      ■■３　画面ＷＲＩＴＥ　処理                            *
****************************************************************
 DSP-WRITE-SEC          SECTION.
     MOVE    SPACE               TO   DSP-PROC.
     WRITE                  FVD02151.
 DSP-WRITE-END.
     EXIT.
****************************************************************
*      1.1.4     エラーメッセージセット　処理                  *
****************************************************************
 DSP-ERR-SEC                 SECTION.
*---- エラー メッセージ セット ----*
     MOVE    ERR-MSG(ERR-CD)     TO   ERRMSG.
     MOVE    ZERO                TO   ERR-CD.
     PERFORM    DSP-WRITE-SEC.
 DSP-ERR-END.
     EXIT.
****************************************************************
*      1.2       入庫ファイル　ＳＴＡＲＴ処理                *
****************************************************************
 NYKFILF-START-SEC      SECTION.
     INITIALIZE                  NYUK-REC.
     MOVE  SOKCD             TO  NYUK-F27.
*
     START NYKFILF   KEY >=      NYUK-F27  NYUK-F02  NYUK-F03
                                 NYUK-F04  NYUK-F05
         INVALID
            MOVE   "END"     TO  END-FLG
     END-START.
 NYKFILF-START-EXIT.
     EXIT.
************************************************************
*      1.3       入庫ファイル　ＲＥＡＤ処理                *
************************************************************
 NYKFILF-READ-SEC       SECTION.
     READ    NYKFILF
       AT  END
           MOVE     "END"        TO   END-FLG
           MOVE     HIGH-VALUE   TO   WK-NYUK-KEY
           GO       TO   NYKFILF-READ-EXIT
     END-READ.
     ADD      1          TO   READ-CNT.
*
 NYKFILF-010.
     IF   SOKCD    NOT =   SPACE     AND
          SOKCD    NOT =   NYUK-F27
           MOVE     "END"        TO   END-FLG
           MOVE     HIGH-VALUE   TO   WK-NYUK-KEY
           GO       TO   NYKFILF-READ-EXIT
     END-IF.
 NYKFILF-020.
*伝票区分
     IF   LINK-DSOKCD     =   "01"  OR  "88"
          IF    NYUK-F01   NOT =   "50"
               GO          TO   NYKFILF-READ-SEC
          END-IF
     END-IF.
 NYKFILF-030.
*計上
     EVALUATE   KEIJYO
       WHEN   1
          IF    NYUK-F30   =       ZERO
               GO          TO   NYKFILF-READ-SEC
          END-IF
       WHEN   9
          IF    NYUK-F30   NOT =   ZERO
               GO          TO   NYKFILF-READ-SEC
          END-IF
     END-EVALUATE.
*
 NYKFILF-040.
     IF  KEIJYO  =  9
         IF ( WK-SDATE    <=    NYUK-F98 ) AND
            ( WK-EDATE    >=    NYUK-F98 ) AND
            ( NYUK-F01     =    50 )
                ADD      1             TO   SELECT-CNT
                MOVE     NYUK-F27      TO   WK-NYUK-F27
                MOVE     NYUK-F01      TO   WK-NYUK-F01
                MOVE     NYUK-F02      TO   WK-NYUK-F02
                MOVE     NYUK-F03      TO   WK-NYUK-F03
                MOVE     NYUK-F04      TO   WK-NYUK-F04
                GO       TO                 NYKFILF-READ-EXIT
            ELSE
                ADD  1          TO   SKIP-CNT
                GO              TO   NYKFILF-READ-SEC
         END-IF
     END-IF.
*
 NYKFILF-050.
**** DISPLAY "WK-SDATE = " WK-SDATE UPON CONS.
**** DISPLAY "WK-EDATE = " WK-EDATE UPON CONS.
**** DISPLAY "NYUK-F98 = " NYUK-F98 UPON CONS.
**** DISPLAY "NYUK-F99 = " NYUK-F99 UPON CONS.
     IF ((WK-SDATE <= NYUK-F98  AND  WK-EDATE >= NYUK-F98) OR
         (WK-SDATE <= NYUK-F99  AND  WK-EDATE >= NYUK-F99))
       THEN
           ADD      1             TO   SELECT-CNT
           MOVE     NYUK-F27      TO   WK-NYUK-F27
           MOVE     NYUK-F01      TO   WK-NYUK-F01
           MOVE     NYUK-F02      TO   WK-NYUK-F02
           MOVE     NYUK-F03      TO   WK-NYUK-F03
           MOVE     NYUK-F04      TO   WK-NYUK-F04
       ELSE
         ADD  1          TO   SKIP-CNT
         GO              TO   NYKFILF-READ-SEC
     END-IF.
 NYKFILF-READ-EXIT.
     EXIT.
************************************************************
*      ■０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
     MOVE     WK-NYUK-KEY         TO   WK-HIKAKU-KEY.
     PERFORM  TBL-SET-SEC     VARYING  IX FROM   1 BY 1
              UNTIL   IX  >   6
                OR    WK-NYUK-KEY   NOT =  WK-HIKAKU-KEY.
     PERFORM  HENSYU-SEC.
 MAIN-SEC-EXIT.
     EXIT.
************************************************************
*      ■1       テーブル格納処理                          *
************************************************************
 TBL-SET-SEC            SECTION.
     MOVE     NYUK-REC      TO       TBL-NYUK(IX).
     ADD      1             TO       TBL-CNT.
     PERFORM  NYKFILF-READ-SEC.
  TBL-SET-EXIT.
************************************************************
*      ■２      編集処理                                  *
************************************************************
 HENSYU-SEC             SECTION.
*    EVALUATE     TBL-CNT
*        WHEN  1
*            MOVE     4     TO        PRINT-CNT
*        WHEN  2
*            MOVE     5     TO        PRINT-CNT
*        WHEN  3
*            MOVE     6     TO        PRINT-CNT
*        WHEN  4
*            MOVE     7     TO        PRINT-CNT
*        WHEN  5
*            MOVE     8     TO        PRINT-CNT
*        WHEN  6
*            MOVE     9     TO        PRINT-CNT
*    END-EVALUATE.
*
*    COMPUTE  WK-L-CNT  =  L-CNT   +   PRINT-CNT
*    IF       WK-L-CNT     >       52
*       OR    H1-SOKCD     NOT =   WK-HIKA-F27
*        PERFORM       MIDASI-SEC
*    END-IF.
*
     PERFORM   PRINT-SEC    VARYING   IX  FROM  1  BY  1
               UNTIL        IX   >    TBL-CNT.
*
*    MOVE     SPACE         TO        P-REC.
*    WRITE    P-REC         FROM      MEISAI-0    AFTER 1
*    ADD      1             TO        L-CNT.
*
     MOVE     ZERO          TO        TBL-CNT.
 HNSYU-SEC-EXIT.
     EXIT.
************************************************************
*      ■■２    CSV出力処理                              *
************************************************************
 PRINT-SEC              SECTION.
*
*レコード初期化　　　　　　　　　　
     MOVE     SPACE         TO      CSV-REC.
     INITIALIZE                     CSV-REC.
*カンマセット
     MOVE     ","           TO      CSV-MK01 CSV-MK02 CSV-MK03
                                    CSV-MK04 CSV-MK05 CSV-MK06
                                    CSV-MK07 CSV-MK08 CSV-MK09
                                    CSV-MK10 CSV-MK11 CSV-MK12
                                    CSV-MK13 CSV-MK14 CSV-MK15
                                    CSV-MK16 CSV-MK17 CSV-MK18
                                    CSV-MK19 CSV-MK20 CSV-MK21
                                    CSV-MK22 CSV-MK23 CSV-MK24
                                    CSV-MK25 CSV-MK26 CSV-MK27.
*制御バイトセット
     MOVE    X"28"          TO      CSV-MS02 CSV-MS04 CSV-MS10
                                    CSV-MS12 CSV-MS15 CSV-MS22.
     MOVE    X"29"          TO      CSV-ME02 CSV-ME04 CSV-ME10
                                    CSV-ME12 CSV-ME15 CSV-ME22.
*
     MOVE     TBL-NYUK(IX)  TO      TBL-REC.
*
*倉庫ＣＤ
     MOVE     TBL-F27               TO      CSV-M01 SOK-F01.
*倉庫名
     READ     ZSOKMS
              INVALID KEY
                  MOVE   ALL NC"＊" TO      CSV-M02
              NOT INVALID
                  MOVE   SOK-F02    TO      CSV-M02
     END-READ.
*伝区
     MOVE     TBL-F01               TO      CSV-M03
                                            JYO-F02.
*伝区名
     MOVE     1                     TO      JYO-F01.
     READ     HJYOKEN
       INVALID
         MOVE     ALL NC"＊"        TO      CSV-M04
       NOT  INVALID
         MOVE     JYO-F03           TO      CSV-M04
     END-READ.
*納品日
     MOVE     TBL-F26(1:4)          TO      CSV-M05(1:4).
     MOVE     "/"                   TO      CSV-M05(5:1).
     MOVE     TBL-F26(5:2)          TO      CSV-M05(6:2).
     MOVE     "/"                   TO      CSV-M05(8:1).
     MOVE     TBL-F26(7:2)          TO      CSV-M05(9:2).
*発注ＮＯ
     MOVE     TBL-F02               TO      CSV-M06.
*枝ＮＯ
     MOVE     TBL-F03               TO      CSV-M07.
*発注日
     MOVE     TBL-F13(1:4)          TO      CSV-M08(1:4).
     MOVE     "/"                   TO      CSV-M08(5:1).
     MOVE     TBL-F13(5:2)          TO      CSV-M08(6:2).
     MOVE     "/"                   TO      CSV-M08(8:1).
     MOVE     TBL-F13(7:2)          TO      CSV-M08(9:2).
*仕入先ＣＤ
     MOVE     TBL-F08               TO      CSV-M09  SHI-F01.
*仕入先名
     READ     ZSHIMS
       INVALID
         MOVE     ALL NC"＊"        TO      CSV-M10
       NOT  INVALID
         MOVE     SHI-F02           TO      CSV-M10
     END-READ.
*ＤＴ種別
     MOVE     TBL-F91               TO      CSV-M11  JYO-F02.
*ＤＴ種別名
     MOVE     87                    TO      JYO-F01.
     READ     HJYOKEN
       INVALID
         MOVE     ALL NC"＊"        TO      CSV-M12
       NOT  INVALID
         MOVE     JYO-F03           TO      CSV-M12
     END-READ.
*Ｄ３６５発注ＮＯ
     MOVE     TBL-F02               TO      HED-F02.
     READ     HACHEDF
       INVALID
         MOVE     ALL "*"           TO      CSV-M13
       NOT INVALID KEY
         MOVE     HED-F313          TO      CSV-M13
     END-READ.
*担当者ＣＤ
     MOVE     TBL-F33               TO      CSV-M14  TAN-F02.
*担当者名
     MOVE     LINK-BUMON            TO      TAN-F01.
     READ     HTANMS
              INVALID
                  MOVE     ALL NC"＊"     TO      CSV-M15
              NOT  INVALID
                  MOVE     TAN-F03        TO      CSV-M15
     END-READ.
*登録日
     MOVE     TBL-F98(1:4)      TO        CSV-M16(1:4).
     MOVE     "/"               TO        CSV-M16(5:1).
     MOVE     TBL-F98(5:2)      TO        CSV-M16(6:2).
     MOVE     "/"               TO        CSV-M16(8:1).
     MOVE     TBL-F98(7:2)      TO        CSV-M16(9:2).
*更新日
     MOVE     TBL-F99(1:4)      TO        CSV-M17(1:4).
     MOVE     "/"               TO        CSV-M17(5:1).
     MOVE     TBL-F99(5:2)      TO        CSV-M17(6:2).
     MOVE     "/"               TO        CSV-M17(8:1).
     MOVE     TBL-F99(7:2)      TO        CSV-M17(9:2).
*行ＮＯ
     MOVE     TBL-F05           TO        CSV-M18.
*完納区分
     MOVE     TBL-F06           TO        CSV-M19.
*サカタ商品ＣＤ
     MOVE     TBL-F15           TO        CSV-M20 MEI-F011.
*サカタ品単ＣＤ
     MOVE     TBL-F16           TO        CSV-M21 MEI-F012.
*商品名
     READ     SUBMEIF
       INVALID
         MOVE     ALL NC"＊"    TO        CSV-M22
       NOT  INVALID
         MOVE     MEI-F021      TO        CSV-M22(01:15)
         MOVE     MEI-F022      TO        CSV-M22(16:15)
     END-READ.
*発注数
     MOVE     TBL-F25           TO        CSV-M23.
*入荷数
     MOVE     TBL-F18           TO        CSV-M24.
*発注残数
     COMPUTE  CSV-M25   =   CSV-M23   -   CSV-M24.
*_番
     MOVE     TBL-F17           TO        CSV-M26.
*Ｄ３６５ＪＡＮＣＤ
     MOVE     TBL-F15           TO        MEI-F011.
     MOVE     TBL-F16           TO        MEI-F012.
     READ     SUBMEIF
       INVALID
         MOVE     SPACE         TO        CSV-M27
       NOT  INVALID
         MOVE     MEI-D01       TO        CSV-M27
     END-READ.
*
     WRITE    CSV-REC.
     ADD       1            TO        NYKESICS-CNT.
*
 PRINT-SEC-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF     HTANMS
              NYKFILF  HACHEDF  HACMEIF  SUBMEIF
              ZSHIMS   HTOKMS   ZSOKMS   HJYOKEN  NYKESICS.
*
     DISPLAY "* NYKFILF (IN)=" READ-CNT   " *" UPON CONS.
     DISPLAY "* NYKESICS(OT)=" SELECT-CNT " *" UPON CONS.
*
     IF       SELECT-CNT = 0
              MOVE    4010        TO   PROGRAM-STATUS
     END-IF.
*
 END-SEC-EXIT.
     EXIT.
************************************************************
*                 項目属性初期化                           *
************************************************************
 OPT-CLR-SEC            SECTION.
     MOVE    "M"                 TO   EDIT-OPTION   OF   SOKCD.
     MOVE    " "                 TO   EDIT-CURSOR   OF   SOKCD.
     MOVE    "M"                 TO   EDIT-OPTION   OF   SYY.
     MOVE    " "                 TO   EDIT-CURSOR   OF   SYY.
     MOVE    "M"                 TO   EDIT-OPTION   OF   SMM.
     MOVE    " "                 TO   EDIT-CURSOR   OF   SMM.
     MOVE    "M"                 TO   EDIT-OPTION   OF   SDD.
     MOVE    " "                 TO   EDIT-CURSOR   OF   SDD.
     MOVE    "M"                 TO   EDIT-OPTION   OF   EYY.
     MOVE    " "                 TO   EDIT-CURSOR   OF   EYY.
     MOVE    "M"                 TO   EDIT-OPTION   OF   EMM.
     MOVE    " "                 TO   EDIT-CURSOR   OF   EMM.
     MOVE    "M"                 TO   EDIT-OPTION   OF   EDD.
     MOVE    " "                 TO   EDIT-CURSOR   OF   EDD.
     MOVE    "M"                 TO   EDIT-OPTION   OF   KEIJYO.
     MOVE    " "                 TO   EDIT-CURSOR   OF   KEIJYO.
 OPT-CLR-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
