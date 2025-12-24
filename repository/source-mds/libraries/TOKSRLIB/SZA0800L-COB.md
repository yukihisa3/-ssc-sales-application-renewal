# SZA0800L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SZA0800L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　商品在庫マスタリスト              *
*    作成日／更新日　　　：　2022/07/12                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　商品在庫マスタリストの出力を行う　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0800L.
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
                        DESTINATION-1        IS   DSP-WSNO
                        FILE STATUS          IS   DSP-STATUS.
****<< 商品在庫マスタファイル >>****************************
     SELECT   ZAMZAIF    ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-STATUS.
****<< 商品在庫マスタ（実績） >>****************************
     SELECT   ZAMJISF    ASSIGN    TO        DA-01-VI-ZAMJISL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAJ-F01
                                                 ZAJ-F02
                                                 ZAJ-F03
                        FILE      STATUS    IS   ZAJ-STATUS.
****<< 倉庫マスタファイル >>********************************
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
*
*20220712↓
****<< 商品名称マスタファイル >>****************************
*-   SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
     SELECT   SUBMEIF   ASSIGN    TO        DA-01-VI-SUBMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
*
*20220712↓
*---<<  条件ファイル  >>---*
*    SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
*                       ORGANIZATION        IS   INDEXED
*                       ACCESS    MODE      IS   RANDOM
*                       RECORD    KEY       IS   JYO-F01
*                                                JYO-F02
*                       FILE      STATUS    IS   JYO-STATUS.
*
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル        >>******************************
 FD    DSPF.
     COPY     FZA01301   OF       XMDLIB.
****<< 商品在庫マスタファイル >>****************************
 FD  ZAMZAIF.
     COPY     ZAMZAIF    OF       XFDLIB
              JOINING   ZAI       PREFIX.
****<< 商品在庫マスタ（実績） >>****************************
 FD  ZAMJISF.
     COPY     ZAMJISF    OF       XFDLIB
              JOINING   ZAJ       PREFIX.
****<< 倉庫マスタファイル >>********************************
 FD  ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*20220712↓
****<< 商品名称マスタファイル >>****************************
 FD  SUBMEIF.
     COPY     SUBMEIF    OF        XFDLIB
              JOINING   MEI       PREFIX.
*20220712↓
****<< 条件ファイル　　　　　 >>****************************
*FD  HJYOKEN.
*    COPY     HJYOKEN   OF        XFDLIB
*             JOINING   JYO       PREFIX.
****<<  プリントファイル  >>********************************
 FD    PRINTF    LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
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
     02 DSP-WSNO             PIC  X(8).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 ZAI-STATUS           PIC  X(2).
     02 ZAJ-STATUS           PIC  X(2).
     02 SOK-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
     02 JYO-STATUS           PIC  X(2).
****  カウンタ                ****
 01  L-CNT                   PIC  9(2).
 01  P-CNT                   PIC  9(3).
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
 01  INV-FLG                 PIC  9(01)  VALUE  ZERO.
 01  ZAJ-FLG                 PIC  9(01)  VALUE  ZERO.
****  インデックス            ****
 01  IXA                     PIC  9(02).
 01  IXB                     PIC  9(02).
****  品単右詰め用エリア      ****
 01  WK-HINTAN                  PIC X(05).
 01  WK-MIGI.
     03  WK-CNT1                PIC 9(01).
     03  WK-CNT2                PIC 9(01).
     03  MIG-SW                 PIC 9(01).
     03  WK1-HIN                PIC X(05).
     03  WK1-HIN1               REDEFINES   WK1-HIN.
         05  WK1-HIN2               PIC X(01)  OCCURS 5.
     03  WK2-HIN                PIC X(05).
     03  WK2-HIN1               REDEFINES   WK2-HIN.
         05  WK2-HIN2               PIC X(01)  OCCURS 5.
****  商品右詰め用エリア      ****
 01  WK-SHOCD.
     03  WK-SHO              PIC  X(01)   OCCURS  8.
****  ワークエリア            ****
 01  WRK-AREA.
     02  WK-SHTAN.
         03  WK-SHTAN1      PIC  X(05).
         03  WK-SHTAN2      PIC  X(02).
         03  WK-SHTAN3      PIC  X(01).
     02  WK-EHTAN.
         03  WK-EHTAN1      PIC  X(05).
         03  WK-EHTAN2      PIC  X(02).
         03  WK-EHTAN3      PIC  X(01).
     02  WK-STANA.
         03  WK-STANA1      PIC  X(01).
         03  WK-STANA2      PIC  X(03).
         03  WK-STANA3      PIC  X(02).
     02  WK-ETANA.
         03  WK-ETANA1      PIC  X(01).
         03  WK-ETANA2      PIC  X(03).
         03  WK-ETANA3      PIC  X(02).
****  日付保存                ****
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
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
*
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SZA0800L".
     02  FILLER              PIC  X(15)  VALUE  SPACE.
     02  FILLER              PIC  N(18)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＊＊＊　商品在庫マスタリスト　＊＊＊".
     02  H-TOKHAN            PIC  N(08)  VALUE  SPACE
         CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"処理日".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-YY                PIC  9(04).
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-MM                PIC  Z9.
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-DD                PIC  Z9.
     02  FILLER              PIC  X(3)   VALUE  SPACE.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"頁".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  PAGE-SUU            PIC  ZZ9.
****  見出し行２             ****
 01  MIDASI-2.
   02  MIDASI-2-1     CHARACTER     TYPE   IS   NIHONGO.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(02)  VALUE
         NC"倉庫".
     03  FILLER              PIC  X(06)  VALUE  SPACE.
     03  FILLER              PIC  N(02)  VALUE
         NC"商品".
     03  FILLER              PIC  X(04)  VALUE  "ｺｰﾄﾞ".
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE
         NC"品　単".
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE
         NC"棚　番".
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"カナ名".
     03  FILLER              PIC  X(10)  VALUE  SPACE.
   02  MIDASI-2-2     CHARACTER     TYPE   IS   YB.
     03  FILLER              PIC  N(08)
                             VALUE  NC"たねまるＪＡＮ　".
     03  FILLER              PIC  X(13)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE
         NC"現在庫数".
     03  FILLER              PIC  X(04)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE
         NC"引当済数".
     03  FILLER              PIC  X(04)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE
         NC"未入庫数".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(06)  VALUE
         NC"　当月売上数".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(06)  VALUE
         NC"　当月入庫数".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(06)  VALUE
         NC"　次月入庫数".
****  見出し行３             ****
 01  MIDASI-3.
   02  MIDASI-3-1     CHARACTER     TYPE   IS   NIHONGO.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE
         NC"取引先".
     03  FILLER              PIC  X(04)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE
         NC"商品名称".
     03  FILLER              PIC  X(35)  VALUE  SPACE.
   02  MIDASI-3-2     CHARACTER     TYPE   IS   YB.
     03  FILLER              PIC  N(08)
                             VALUE  NC"たねまる商品ＣＤ".
     03  FILLER              PIC  X(10)  VALUE  SPACE.
     03  FILLER              PIC  N(06)  VALUE
         NC"引当可能在庫".
     03  FILLER              PIC  X(04)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE
         NC"未出庫数".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(06)  VALUE
         NC"　前月末在庫".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(06)  VALUE
         NC"　次月売上数".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(06)  VALUE
         NC"　当月出庫数".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(06)  VALUE
         NC"　次月出庫数".
****  見出し行４             ****
 01  MIDASI-4       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"当年度実績".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"売上数".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"入庫数".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"出庫数".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"前年度実績".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"売上数".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"入庫数".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"出庫数".
****  見出し行５             ****
 01  MIDASI-5       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(67)  VALUE  ALL NC"─".
****  明細行１               ****
 01  MEISAI-1       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-SOKCD           PIC  X(02)B.
     02  PRT-SOKNM           PIC  N(04).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-SHOCD           PIC  X(08)B.
     02  PRT-HTAN            PIC  X(08)B.
     02  PRT-TANA            PIC  X(08)B.
     02  PRT-KANA            PIC  X(15)B.
     02  PRT-TANEJAN         PIC  X(20).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-GEN             PIC  --,---,--9.
     02  PRT-SUMI            PIC  --,---,--9.
     02  PRT-MINYK           PIC  --,---,--9.
     02  PRT-TOURI           PIC  --,---,--9.
     02  PRT-TONYK           PIC  --,---,--9.
     02  PRT-JINYK           PIC  --,---,--9.
****  明細行２               ****
 01  MEISAI-2       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-TORCD           PIC  9(08)B.
     02  PRT-SHONM.
       03  PRT-SHONM1        PIC  N(15).
       03  PRT-SHONM2        PIC  N(13).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  PRT-TANESHO         PIC  X(20).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-KANO            PIC  --,---,--9.
     02  PRT-MISYK           PIC  --,---,--9.
     02  PRT-ZEN             PIC  --,---,--9.
     02  PRT-JIURI           PIC  --,---,--9.
     02  PRT-TOSYK           PIC  --,---,--9.
     02  PRT-JISYK           PIC  --,---,--9.
****  明細行３               ****
 01  MEISAI-3       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(15)  VALUE  SPACE.
     02  TUKI1               PIC  X(02).
     02  FILLER              PIC  N(01)  VALUE  NC"月".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-051             PIC  ---,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-052             PIC  ---,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-053             PIC  ---,---,--9.99.
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  TUKI2               PIC  X(02).
     02  FILLER              PIC  N(01)  VALUE  NC"月".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-041             PIC  ---,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-042             PIC  ---,---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  PRT-043             PIC  ---,---,--9.99.
****  明細行４               ****
 01  MEISAI-4.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(134) VALUE  ALL "-".
**** エラーメッセージ         ****
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(30)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(30)  VALUE
            NC"開始・終了コードの関係に誤りがあります。".
     02  MSG-ERR3            PIC  N(30)  VALUE
            NC"実績出力指示エラー".
     02  MSG-ERR4            PIC  N(30)  VALUE
            NC"倉庫コードに誤りがあります".
     02  MSG-ERR5            PIC  N(30)  VALUE
            NC"出力区分に誤りがあります".
     02  PMSG01              PIC N(20) VALUE
            NC"_取消　_終了".
     02  PMSG02              PIC N(20) VALUE
            NC"_取消　_終了　_項目戻り".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SZA0800L".
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
************************************************************
 LINKAGE                SECTION.
 01  LINK-SOKCD            PIC X(02).
 01  LINK-DSOKCD           PIC X(02).
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
 PROCEDURE              DIVISION    USING LINK-SOKCD LINK-DSOKCD.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZAMZAIF.
     MOVE   "ZAMZAIF "        TO    ERR-FL-ID.
     MOVE    ZAI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZAMJISF.
     MOVE   "ZAMJISF "        TO    ERR-FL-ID.
     MOVE    ZAJ-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS  "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBMEIF.
     MOVE   "SUBMEIF  "        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DSPF.
     MOVE   "DSPF    "        TO    ERR-FL-ID.
     MOVE    DSP-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
*20220712↓
*FILEERR-SEC6           SECTION.
*    USE AFTER     EXCEPTION
*                  PROCEDURE  HJYOKEN.
*    MOVE   "HJYOKEN "        TO    ERR-FL-ID.
*    MOVE    JYO-STATUS       TO    ERR-STCD.
*    DISPLAY MSG-ABEND1       UPON  CONS.
*    DISPLAY MSG-ABEND2       UPON  CONS.
*    MOVE    4000             TO    PROGRAM-STATUS.
*    STOP     RUN.
 END     DECLARATIVES.
************************************************************
*      0.0       全体処理                                  *
************************************************************
 SZA0800L-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC        UNTIL  END-FLG = "END".
     PERFORM       END-SEC.
     IF   PG-END   NOT =  "END"
          MOVE     SPACE   TO   END-FLG
          GO  TO   SZA0800L-START
     END-IF.
     STOP     RUN.
 SZA0800L-END.
     EXIT.
************************************************************
*      1.0      初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       DSPF.
     OPEN     INPUT     ZAMZAIF   ZAMJISF   ZSOKMS   SUBMEIF.
*20220712↓
*             HJYOKEN.
*20220712↑
     OPEN     OUTPUT    PRINTF.
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
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY H-YY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM   H-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD   H-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*20220712↓
*特販部名称編集
*    MOVE     SPACE               TO   JYO-REC.
*    INITIALIZE                        JYO-REC.
*    MOVE    "99"                 TO   JYO-F01.
*    MOVE    "BUMON"              TO   JYO-F02.
*    READ     HJYOKEN
*      INVALID KEY
*             MOVE NC"＊＊＊＊＊＊"   TO   HEN-TOKHAN
*      NOT INVALID KEY
*             MOVE JYO-F03            TO   HEN-TOKHAN
*    END-READ.
*    MOVE     HEN-TOKHAN-AREA         TO   H-TOKHAN.
*
     MOVE    "FZA01301" TO     DSP-FORMAT.
     MOVE     SPACE     TO     FZA01301.
     MOVE     SPACE     TO     MSG1.
     MOVE     PMSG01    TO     MSG2.
     MOVE     62        TO     L-CNT.
     MOVE     0         TO     P-CNT.
*
     IF       LINK-DSOKCD   =  "01"  OR  "88"
              CONTINUE
     ELSE
              MOVE   LINK-SOKCD   TO  SSOKCD
                                      ESOKCD
                                      SOK-F01
              PERFORM  SOK-READ-SEC
              IF     INV-FLG   =   ZERO
                     MOVE  SOK-F02    TO   SSOKNM
                                           ESOKNM
              END-IF
     END-IF.
*
*画面（入力処理）
     PERFORM  DSP-SEC.
     IF       END-FLG   NOT =  SPACE
              GO   TO   INIT-END
     END-IF.
*
*データ検索
     MOVE     SSOKCD    TO     ZAI-F01.
     MOVE     SSHOCD    TO     ZAI-F021.
     MOVE     SHTAN1    TO     ZAI-F022(1:5).
     MOVE     SHTAN2    TO     ZAI-F022(6:2).
     MOVE     SHTAN3    TO     ZAI-F022(8:1).
     MOVE     STANA1    TO     ZAI-F03(1:1).
     MOVE     STANA2    TO     ZAI-F03(2:3).
     MOVE     STANA3    TO     ZAI-F03(5:2).
     START    ZAMZAIF    KEY  IS  >=  ZAI-F01
                                      ZAI-F02
                                      ZAI-F03
              INVALID      MOVE  "END"    TO  END-FLG
              NOT INVALID  PERFORM  ZAMZAIF-READ-SEC
     END-START.
*
 INIT-END.
     EXIT.
************************************************************
*      １．１   画面処理                                   *
************************************************************
 DSP-SEC                SECTION.
*画面表示
     MOVE     PMSG01    TO     MSG2.
     PERFORM   DSP-WRITE-SEC.
*倉庫ＣＤプロテクト
     IF       LINK-DSOKCD   =   "01"  OR  "88"
              CONTINUE
     ELSE
              MOVE   "X"       TO   EDIT-STATUS  OF  SSOKCD
                                    EDIT-STATUS  OF  ESOKCD
     END-IF.
*画面入力
     MOVE     SPACE     TO     MSG1.
     MOVE     "NE"      TO     DSP-PROC.
     MOVE     "GPHEAD"  TO     DSP-GROUP.
     READ     DSPF.
     PERFORM  OPT-CLR-SEC.
*アテンション判定
     EVALUATE   DSP-FUNC
         WHEN   "F004"
                MOVE   SPACE    TO    HEAD
                GO TO  DSP-SEC
         WHEN   "F005"
                MOVE    "END"   TO    END-FLG
                MOVE    "END"   TO    PG-END
                GO TO  DSP-END
         WHEN   "E000"
                PERFORM   DSP-CHK-SEC
                IF   MSG1  NOT =  SPACE
                     GO TO  DSP-SEC
                END-IF
         WHEN   OTHER
                MOVE   MSG-ERR1  TO  MSG1
                GO TO  DSP-SEC
     END-EVALUATE.
*
 DSP-500.
*画面表示
     MOVE     PMSG02    TO     MSG2.
     PERFORM   DSP-WRITE-SEC.
*画面入力
     MOVE     SPACE     TO     MSG1.
     MOVE     "NE"      TO     DSP-PROC.
     MOVE    "KAKNIN"   TO     DSP-GROUP.
     READ     DSPF.
     PERFORM  OPT-CLR-SEC.
*アテンション判定
     EVALUATE   DSP-FUNC
         WHEN   "F004"
                MOVE   SPACE    TO    HEAD
                GO TO  DSP-SEC
         WHEN   "F005"
                MOVE    "END"   TO    END-FLG
                MOVE    "END"   TO    PG-END
                GO TO  DSP-END
         WHEN   "F006"
                GO TO  DSP-SEC
         WHEN   "E000"
                CONTINUE
         WHEN   OTHER
                MOVE   MSG-ERR1  TO  MSG1
                GO TO  DSP-SEC
     END-EVALUATE.
 DSP-END.
     EXIT.
************************************************************
*      1.1.2      入力内容チェック                         *
************************************************************
 DSP-CHK-SEC           SECTION.
*倉庫コード
     IF       LINK-DSOKCD   =  "01"  OR  "88"
              CONTINUE
     ELSE
              GO           TO         DSP-CHK-100
     END-IF.
     MOVE     SPACE           TO   SSOKNM.
     IF       SSOKCD       NOT =   SPACE
              MOVE   SSOKCD        TO   SOK-F01
              PERFORM   SOK-READ-SEC
              IF     INV-FLG   =   ZERO
                     MOVE   SOK-F02     TO SSOKNM
              ELSE
                     MOVE   MSG-ERR4    TO MSG1
                     MOVE   "R"         TO EDIT-OPTION OF SSOKCD
                     MOVE   "C"         TO EDIT-CURSOR OF SSOKCD
              END-IF
     END-IF.
*
     MOVE     SPACE           TO   ESOKNM.
     IF       ESOKCD           =   SPACE
              MOVE   "99"          TO   ESOKCD
     END-IF.
     IF       ESOKCD       NOT =   "99"
              MOVE   ESOKCD        TO   SOK-F01
              PERFORM   SOK-READ-SEC
              IF     INV-FLG   =   ZERO
                     MOVE   SOK-F02     TO ESOKNM
              ELSE
                     MOVE   MSG-ERR4    TO MSG1
                     MOVE   "R"         TO EDIT-OPTION OF ESOKCD
                     MOVE   "C"         TO EDIT-CURSOR OF ESOKCD
              END-IF
     END-IF.
*
     IF       MSG1      =   SPACE      AND
              SSOKCD    >   ESOKCD
              MOVE   MSG-ERR2     TO  MSG1
              MOVE   "R"          TO  EDIT-OPTION OF SSOKCD
              MOVE   "R"          TO  EDIT-OPTION OF ESOKCD
              MOVE   "C"          TO  EDIT-CURSOR OF ESOKCD
     END-IF.
*
 DSP-CHK-100.
*商品コード
     IF       SSHOCD    NOT =  SPACE
              MOVE   SSHOCD       TO  WK-SHOCD
              PERFORM          UNTIL  WK-SHO(8) NOT = SPACE
                 PERFORM VARYING IXA FROM 7 BY -1 UNTIL IXA = 0
                    MOVE   WK-SHO(IXA)      TO  WK-SHO(IXA + 1)
                 END-PERFORM
                 MOVE      ZERO             TO  WK-SHO(1)
              END-PERFORM
              MOVE  WK-SHOCD      TO  SSHOCD
     END-IF.
     IF       ESHOCD    =   SPACE
              MOVE   "99999999"   TO  ESHOCD
     ELSE
              MOVE   ESHOCD       TO  WK-SHOCD
              PERFORM          UNTIL  WK-SHO(8) NOT = SPACE
                 PERFORM VARYING IXA FROM 7 BY -1 UNTIL IXA = 0
                    MOVE   WK-SHO(IXA)      TO  WK-SHO(IXA + 1)
                 END-PERFORM
                 MOVE      ZERO             TO  WK-SHO(1)
              END-PERFORM
              MOVE  WK-SHOCD      TO  ESHOCD
     END-IF.
*品単
     IF       EHTAN1    =   SPACE   AND
              EHTAN2    =   SPACE   AND
              EHTAN3    =   SPACE
              MOVE   "99999"      TO  EHTAN1
              MOVE   "99"         TO  EHTAN2
              MOVE   "9"          TO  EHTAN3
     END-IF.
     IF       SHTAN1    NOT =   SPACE
              MOVE     SHTAN1         TO   WK-HINTAN
              PERFORM  HINTAN-SEC
              MOVE     WK-HINTAN      TO   SHTAN1
     END-IF.
     IF       EHTAN1    NOT =   SPACE
              MOVE     EHTAN1         TO   WK-HINTAN
              PERFORM  HINTAN-SEC
              MOVE     WK-HINTAN      TO   EHTAN1
     END-IF.
     MOVE     SHTAN1              TO  WK-SHTAN1.
     MOVE     SHTAN2              TO  WK-SHTAN2.
     MOVE     SHTAN3              TO  WK-SHTAN3.
     MOVE     EHTAN1              TO  WK-EHTAN1.
     MOVE     EHTAN2              TO  WK-EHTAN2.
     MOVE     EHTAN3              TO  WK-EHTAN3.
     IF       WK-SHTAN   >   WK-EHTAN
              IF     MSG1   =   SPACE
                     MOVE   MSG-ERR2     TO  MSG1
              END-IF
              MOVE   "R"          TO  EDIT-OPTION OF SHTAN1
              MOVE   "R"          TO  EDIT-OPTION OF SHTAN2
              MOVE   "R"          TO  EDIT-OPTION OF SHTAN3
              MOVE   "R"          TO  EDIT-OPTION OF EHTAN1
              MOVE   "R"          TO  EDIT-OPTION OF EHTAN2
              MOVE   "R"          TO  EDIT-OPTION OF EHTAN3
              MOVE   "C"          TO  EDIT-CURSOR OF SHTAN1
     END-IF.
*棚番
     IF       ETANA1    =   SPACE     AND
              ETANA2    =   SPACE     AND
              ETANA3    =   SPACE
              MOVE   "9"          TO  ETANA1
              MOVE   "999"        TO  ETANA2
              MOVE   "99"         TO  ETANA3
     END-IF.
     MOVE     STANA1              TO  WK-STANA1.
     MOVE     STANA2              TO  WK-STANA2.
     MOVE     STANA3              TO  WK-STANA3.
     MOVE     ETANA1              TO  WK-ETANA1.
     MOVE     ETANA2              TO  WK-ETANA2.
     MOVE     ETANA3              TO  WK-ETANA3.
     IF       WK-STANA   >   WK-ETANA
              IF     MSG1   =   SPACE
                     MOVE   MSG-ERR2     TO  MSG1
              END-IF
              MOVE   "R"          TO  EDIT-OPTION OF STANA1
              MOVE   "R"          TO  EDIT-OPTION OF STANA2
              MOVE   "R"          TO  EDIT-OPTION OF STANA3
              MOVE   "R"          TO  EDIT-OPTION OF ETANA1
              MOVE   "R"          TO  EDIT-OPTION OF ETANA2
              MOVE   "R"          TO  EDIT-OPTION OF ETANA3
              MOVE   "C"          TO  EDIT-CURSOR OF STANA1
     END-IF.
*実績出力
     IF       ZOUTF  NOT =  SPACE     AND
              ZOUTF  NOT =  "Y"
              IF     MSG1   =   SPACE
                     MOVE   MSG-ERR3     TO  MSG1
              END-IF
              MOVE   "R"          TO  EDIT-OPTION OF ZOUTF
              MOVE   "C"          TO  EDIT-CURSOR OF ZOUTF
     END-IF.
*出力区分
     IF       OUTKB     NOT NUMERIC
              MOVE      ZERO      TO   OUTKB
     END-IF.
     IF       OUTKB     =   0  OR  1  OR  2
              CONTINUE
     ELSE
              IF   MSG1     =     SPACE
                   MOVE     MSG-ERR5   TO   MSG1
              END-IF
              MOVE   "R"          TO  EDIT-OPTION OF OUTKB
              MOVE   "C"          TO  EDIT-CURSOR OF OUTKB
     END-IF.
 DSP-CHK-END.
     EXIT.
************************************************************
*      1.1.2      品単右づめ処理                           *
************************************************************
 HINTAN-SEC            SECTION.
     MOVE     SPACE          TO        WK1-HIN   WK2-HIN.
     MOVE     WK-HINTAN      TO        WK1-HIN.
     MOVE     0              TO        MIG-SW.
     PERFORM     VARYING   IXA       FROM    5  BY  -1
                 UNTIL     IXA <  1   OR  MIG-SW = 1
         IF  ( WK1-HIN2(IXA) NOT  =  SPACE  AND  MIG-SW = 0 )
             MOVE    IXA    TO    WK-CNT1
             MOVE    1      TO    MIG-SW
         END-IF
     END-PERFORM.
     IF   WK-CNT1  =  5
          GO  TO  HINTAN-EXIT
     END-IF.
     COMPUTE   WK-CNT2  =  5 - WK-CNT1.
     PERFORM     VARYING   IXB       FROM    1  BY  1
                 UNTIL     IXB >  WK-CNT1
         ADD       1                TO    WK-CNT2
         MOVE      WK1-HIN2(IXB)    TO    WK2-HIN2(WK-CNT2)
     END-PERFORM.
     MOVE     WK2-HIN        TO        WK-HINTAN.
 HINTAN-EXIT.
     EXIT.
************************************************************
*      3.0       メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
     MOVE     SPACE          TO        MEISAI-1   MEISAI-2.
*
     IF     ( ZAI-F04   =    ZERO ) AND
            ( ZAI-F05   =    ZERO ) AND
            ( ZAI-F07   =    ZERO ) AND
            ( ZAI-F08   =    ZERO ) AND
            ( ZAI-F09   =    ZERO ) AND
            ( ZAI-F10   =    ZERO ) AND
            ( ZAI-F11   =    ZERO ) AND
            ( ZAI-F12   =    ZERO ) AND
            ( ZAI-F13   =    ZERO ) AND
            ( ZAI-F14   =    ZERO ) AND
            ( ZAI-F26   =    ZERO ) AND
            ( ZAI-F27   =    ZERO ) AND
            ( ZAI-F28   =    ZERO )
*             ゼロは除く
              IF  OUTKB      =    1
                  GO    TO   MAIN-010
              ELSE
                  CONTINUE
              END-IF
     ELSE
*             ゼロのみ
              IF  OUTKB      =    2
                  GO    TO   MAIN-010
              ELSE
                  CONTINUE
              END-IF
     END-IF.
*
*在庫マスタ（実績）読み込み
     IF       ZOUTF  =  "Y"
              PERFORM  ZAMJISF-READ-SEC
     ELSE
              MOVE    1         TO    ZAJ-FLG
     END-IF.
*
     IF       ZAJ-FLG   =  ZERO
         IF       L-CNT         >=      50
                  PERFORM       MIDASI-SEC
         END-IF
     ELSE
         IF       L-CNT         >=      62
                  PERFORM       MIDASI-SEC
         END-IF
     END-IF.
*
     MOVE     ZAI-F01       TO      PRT-SOKCD.
     MOVE     ZAI-F021      TO      PRT-SHOCD.
     MOVE     ZAI-F022      TO      PRT-HTAN.
     IF    ZAI-F03  NOT =  SPACE
           MOVE   ZAI-F03(1:1)      TO    PRT-TANA(1:1)
           MOVE   "-"               TO    PRT-TANA(2:1)
           MOVE   ZAI-F03(2:3)      TO    PRT-TANA(3:3)
           MOVE   "-"               TO    PRT-TANA(6:1)
           MOVE   ZAI-F03(5:2)      TO    PRT-TANA(7:2)
     END-IF.
     MOVE     ZAI-F04       TO      PRT-GEN.
     MOVE     ZAI-F09       TO      PRT-TOURI.
*****MOVE     ZAI-F10       TO      PRT-TOHEP.
     MOVE     ZAI-F07       TO      PRT-TONYK.
     MOVE     ZAI-F26       TO      PRT-MINYK.
     MOVE     ZAI-F28       TO      PRT-SUMI.
     MOVE     ZAI-F11       TO      PRT-JINYK.
*
     MOVE     ZAI-F29       TO      PRT-TORCD.
*-   MOVE     ZAI-F30       TO      PRT-KANA.
*
     COMPUTE  PRT-KANO  =  ZAI-F04  -  ZAI-F27.
     MOVE     ZAI-F13       TO      PRT-JIURI.
     MOVE     ZAI-F08       TO      PRT-TOSYK.
     MOVE     ZAI-F27       TO      PRT-MISYK.
     MOVE     ZAI-F05       TO      PRT-ZEN.
     MOVE     ZAI-F12       TO      PRT-JISYK.
*
     MOVE     ZAI-F01       TO      SOK-F01.
     PERFORM     SOK-READ-SEC.
     IF       INV-FLG    =   ZERO
              MOVE     SOK-F02      TO    PRT-SOKNM
     END-IF.
     MOVE     ZAI-F021      TO      MEI-F011.
     MOVE     ZAI-F022      TO      MEI-F012.
     PERFORM     SHO-READ-SEC.
     IF       INV-FLG    =   ZERO
              MOVE     MEI-F021     TO    PRT-SHONM1
              MOVE     MEI-F022     TO    PRT-SHONM2
*20220712↓
              MOVE     MEI-F031     TO    PRT-KANA
              MOVE     MEI-D01      TO    PRT-TANEJAN
              MOVE     MEI-D02      TO    PRT-TANESHO
*20220712↑
     END-IF.
*
     WRITE    P-REC         FROM    MEISAI-1    AFTER  1.
     WRITE    P-REC         FROM    MEISAI-2    AFTER  1.
     IF       ZAJ-FLG   =  ZERO
         MOVE       SPACE   TO      P-REC
         WRITE      P-REC   AFTER   1
         PERFORM    VARYING IXA FROM 1 BY 1   UNTIL IXA > 12
             EVALUATE    IXA
                WHEN   1   MOVE   " 6"     TO   TUKI1  TUKI2
                WHEN   2   MOVE   " 7"     TO   TUKI1  TUKI2
                WHEN   3   MOVE   " 8"     TO   TUKI1  TUKI2
                WHEN   4   MOVE   " 9"     TO   TUKI1  TUKI2
                WHEN   5   MOVE   "10"     TO   TUKI1  TUKI2
                WHEN   6   MOVE   "11"     TO   TUKI1  TUKI2
                WHEN   7   MOVE   "12"     TO   TUKI1  TUKI2
                WHEN   8   MOVE   " 1"     TO   TUKI1  TUKI2
                WHEN   9   MOVE   " 2"     TO   TUKI1  TUKI2
                WHEN  10   MOVE   " 3"     TO   TUKI1  TUKI2
                WHEN  11   MOVE   " 4"     TO   TUKI1  TUKI2
                WHEN  12   MOVE   " 5"     TO   TUKI1  TUKI2
             END-EVALUATE
             MOVE    ZAJ-F041(IXA)    TO   PRT-041
             MOVE    ZAJ-F042(IXA)    TO   PRT-042
             MOVE    ZAJ-F043(IXA)    TO   PRT-043
             MOVE    ZAJ-F051(IXA)    TO   PRT-051
             MOVE    ZAJ-F052(IXA)    TO   PRT-052
             MOVE    ZAJ-F053(IXA)    TO   PRT-053
             WRITE    P-REC   FROM    MEISAI-3    AFTER  1
         END-PERFORM
         WRITE    P-REC       FROM    MEISAI-4    AFTER  1
         ADD      16            TO      L-CNT
     ELSE
         WRITE    P-REC       FROM    MEISAI-4    AFTER  1
         ADD      3             TO      L-CNT
     END-IF.
*
 MAIN-010.
*
*次の在庫マスタ読み込み
     PERFORM  ZAMZAIF-READ-SEC.
*
 MAIN-END.
     EXIT.
************************************************************
*      2.1       見出し処理                                *
************************************************************
 MIDASI-SEC             SECTION.
     ADD      1          TO      P-CNT.
     MOVE     P-CNT      TO      PAGE-SUU.
*
     IF       P-CNT   >   1
              MOVE       SPACE   TO      P-REC
              WRITE      P-REC   AFTER   PAGE
     END-IF.
     WRITE      P-REC   FROM    MIDASI-1   AFTER  2.
     WRITE      P-REC   FROM    MIDASI-2   AFTER  2.
     WRITE      P-REC   FROM    MIDASI-3   AFTER  1.
     IF       ZOUTF   =  "Y"
              WRITE      P-REC   FROM    MIDASI-4   AFTER  1
              WRITE      P-REC   FROM    MIDASI-5   AFTER  1
              MOVE       7               TO         L-CNT
     ELSE
              WRITE      P-REC   FROM    MIDASI-5   AFTER  1
              MOVE       6               TO         L-CNT
     END-IF.
 MIDASI-END.
     EXIT.
************************************************************
*          在庫マスタ・読み込み
************************************************************
 ZAMZAIF-READ-SEC       SECTION.
     READ     ZAMZAIF
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   ZAMZAIF-READ-END
     END-READ.
*倉庫ＢＲＥＡＫ？
     IF       ZAI-F01   >    ESOKCD
              MOVE  "END"    TO   END-FLG
              GO             TO   ZAMZAIF-READ-END
     END-IF.
*商品コードＢＲＥＡＫ？
     IF       ZAI-F01   =    ESOKCD       AND
              ZAI-F021  >    ESHOCD
              MOVE  "END"    TO   END-FLG
              GO             TO   ZAMZAIF-READ-END
     END-IF.
     IF       ZAI-F021  <    SSHOCD       OR
              ZAI-F021  >    ESHOCD
              GO             TO   ZAMZAIF-READ-SEC
     END-IF.
*品単ＢＲＥＡＫ？
     IF       ZAI-F01   =    ESOKCD       AND
              ZAI-F021  =    ESHOCD       AND
              ZAI-F022  >    WK-EHTAN
              MOVE  "END"    TO   END-FLG
              GO             TO   ZAMZAIF-READ-END
     END-IF.
     IF       ZAI-F022  <    WK-SHTAN     OR
              ZAI-F022  >    WK-EHTAN
              GO             TO   ZAMZAIF-READ-SEC
     END-IF.
*棚番ＢＲＥＡＫ？
     IF       ZAI-F01   =    ESOKCD       AND
              ZAI-F021  =    ESHOCD       AND
              ZAI-F022  =    WK-EHTAN     AND
              ZAI-F03   >    WK-ETANA
              MOVE  "END"    TO   END-FLG
              GO             TO   ZAMZAIF-READ-END
     END-IF.
     IF       ZAI-F03   <    WK-STANA     OR
              ZAI-F03   >    WK-ETANA
              GO             TO   ZAMZAIF-READ-SEC
     END-IF.
 ZAMZAIF-READ-END.
     EXIT.
************************************************************
*          在庫マスタ（実績）・読み込み
************************************************************
 ZAMJISF-READ-SEC       SECTION.
     MOVE    0               TO   ZAJ-FLG.
*
     MOVE    ZAI-F01         TO   ZAJ-F01.
     MOVE    ZAI-F02         TO   ZAJ-F02.
     MOVE    ZAI-F03         TO   ZAJ-F03.
     READ    ZAMJISF
       INVALID    MOVE  1    TO   ZAJ-FLG
     END-READ.
 ZAMJISF-READ-END.
     EXIT.
************************************************************
*      2.2       倉庫名の取得                              *
************************************************************
 SOK-READ-SEC           SECTION.
     MOVE    ZERO            TO   INV-FLG.
     READ    ZSOKMS
       INVALID    MOVE  1    TO   INV-FLG
     END-READ.
 SOK-READ-END.
     EXIT.
************************************************************
*      2.3       商品名の取得                              *
************************************************************
 SHO-READ-SEC           SECTION.
     MOVE    ZERO            TO   INV-FLG.
     READ    SUBMEIF
       INVALID    MOVE  1    TO   INV-FLG
     END-READ.
 SHO-READ-END.
     EXIT.
************************************************************
*                画面・出力                                *
************************************************************
 DSP-WRITE-SEC          SECTION.
     MOVE     SPACE     TO     DSP-PROC.
     MOVE    "GPALL "   TO     DSP-GROUP.
     MOVE     HEN-DATE  TO     SDATE.
     MOVE     HEN-TIME  TO     STIME.
     MOVE     HEN-TOKHAN-AREA  TO     TOKHAN.
     WRITE    FZA01301.
 DSP-WRITE-END.
     EXIT.
************************************************************
*                項目属性初期化
************************************************************
 OPT-CLR-SEC        SECTION.
     MOVE  SPACE         TO  EDIT-CURSOR  OF  SSOKCD
                             EDIT-CURSOR  OF  ESOKCD
                             EDIT-CURSOR  OF  SSHOCD
                             EDIT-CURSOR  OF  ESHOCD
                             EDIT-CURSOR  OF  SHTAN1
                             EDIT-CURSOR  OF  SHTAN2
                             EDIT-CURSOR  OF  SHTAN3
                             EDIT-CURSOR  OF  EHTAN1
                             EDIT-CURSOR  OF  EHTAN2
                             EDIT-CURSOR  OF  EHTAN3
                             EDIT-CURSOR  OF  ETANA1
                             EDIT-CURSOR  OF  ETANA2
                             EDIT-CURSOR  OF  ETANA3
                             EDIT-CURSOR  OF  ZOUTF
                             EDIT-CURSOR  OF  OUTKB.
     MOVE "M"            TO  EDIT-OPTION  OF  SSOKCD
                             EDIT-OPTION  OF  ESOKCD
                             EDIT-OPTION  OF  SSHOCD
                             EDIT-OPTION  OF  ESHOCD
                             EDIT-OPTION  OF  SHTAN1
                             EDIT-OPTION  OF  SHTAN2
                             EDIT-OPTION  OF  SHTAN3
                             EDIT-OPTION  OF  EHTAN1
                             EDIT-OPTION  OF  EHTAN2
                             EDIT-OPTION  OF  EHTAN3
                             EDIT-OPTION  OF  ETANA1
                             EDIT-OPTION  OF  ETANA2
                             EDIT-OPTION  OF  ETANA3
                             EDIT-OPTION  OF  ZOUTF
                             EDIT-OPTION  OF  OUTKB.
 OPT-CLR-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF  PRINTF  ZAMZAIF  ZAMJISF  ZSOKMS  SUBMEIF.
*20220712↓
*             HJYOKEN.
*20220712↑
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
