# SPD9992B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SPD9992B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫システム改善　　　　　　　　　*
*    モジュール名　　　　：　実績系Ｆ制御ファイル出力　　　　　*
*    作成日／更新日　　　：　09/10/29                          *
*    作成者／更新者　　　：　ＮＡＶ　大野　　　　　　　　　　　*
*    処理概要　　　　　　：　実績系Ｆ制御ファイル（ＰＣ→Ｋ）、*
*　　　　　　　　　　　　　　実績系Ｆ制御ファイル（Ｋ→ＰＣ）の*
*　　　　　　　　　　　　　　１件目を読み、判断基準に従い処理結*
*　　　　　　　　　　　　　　果を判定して、パラメタに結果区分を*
*　　　　　　　　　　　　　　返す。　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SPD9992B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<　　　　　実績系Ｆ制御ファイル（ＰＣ→Ｋ）****************
     SELECT   JISSVRL2  ASSIGN    TO        DA-01-VI-JISSVRL2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   SVR-F01
                                                 SVR-F02
                        FILE      STATUS    IS   SVR-STATUS.
****<<　　　　　実績系Ｆ制御ファイル（Ｋ→ＰＣ）****************
     SELECT   JISPCCL2  ASSIGN    TO        DA-01-VI-JISPCCL2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   PCC-F01
                                                 PCC-F02
                        FILE      STATUS    IS   PCC-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 実績系Ｆ制御ファイル（ＰＣ→Ｋ）　>>********************
 FD  JISSVRL2.
     COPY     JISSVRL2  OF        XFDLIB
              JOINING   SVR       PREFIX.
****<< 実績系Ｆ制御ファイル（Ｋ→ＰＣ）　>>********************
 FD  JISPCCL2.
     COPY     JISPCCL2  OF        XFDLIB
              JOINING   PCC       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 SVR-STATUS           PIC  X(02).
     02 PCC-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SPD9992B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****システム日付・時刻　      ****
 01  SYSDATE                 PIC  9(08)     VALUE  ZERO.
 01  SYSTIME.
     03  SYS-HHMMSS              PIC  9(06)     VALUE  ZERO.
     03  SYS-MS                  PIC  9(02)     VALUE  ZERO.
****日付変換サブルーチン用ワーク*****
 01  LINK-IN-KBN             PIC  X(01).
 01  LINK-IN-YMD6            PIC  9(06).
 01  LINK-IN-YMD8            PIC  9(08).
 01  LINK-OUT-RET            PIC  X(01).
 01  LINK-OUT-YMD            PIC  9(08).
****  日付・時刻取得            ****
 01  WK-DATE8                PIC  9(08).
 01  WK-TIME6                PIC  9(06).
*
 LINKAGE                            SECTION.
 01  PARA-KEKKA              PIC X(02).
************************************************************
 PROCEDURE            DIVISION  USING     PARA-KEKKA.
************************************************************
 DECLARATIVES.
 SVR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  JISSVRL2.
     MOVE   "JISSVRL2"        TO    ERR-FL-ID.
     MOVE    SVR-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
*
 PCC-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  JISPCCL2.
     MOVE   "JISPCCL2"        TO    ERR-FL-ID.
     MOVE    PCC-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           100-INIT-SEC.
     PERFORM           200-MAIN-SEC.
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*     100     初期処理
************************************************************
 100-INIT-SEC                     SECTION.
*
     ACCEPT   SYSDATE        FROM      DATE.
     ACCEPT   SYSTIME        FROM      TIME.
*
     MOVE     "3"            TO        LINK-IN-KBN.
     MOVE     SYSDATE        TO        LINK-IN-YMD6.
     MOVE     ZERO           TO        LINK-IN-YMD8.
     MOVE     ZERO           TO        LINK-OUT-RET.
     MOVE     ZERO           TO        LINK-OUT-YMD.
     CALL     "SKYDTCKB"     USING     LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD   TO        WK-DATE8.
*
     MOVE     SYS-HHMMSS     TO        WK-TIME6.
*
     OPEN     I-O       JISSVRL2  JISPCCL2.
*
*    INITIALIZE         END-FLG.
*
*実績系Ｆ制御ファイル（ＰＣ→Ｋ）読み込み
     READ     JISSVRL2
     END-READ.
*
*実績系Ｆ制御ファイル（Ｋ→ＰＣ）読み込み
     READ     JISPCCL2
     END-READ.
*
 100-INIT-END.
     EXIT.
************************************************************
*     200     主処理
************************************************************
 200-MAIN-SEC                     SECTION.
*
* 判定
      IF      PCC-F06  = "E"
      AND     SVR-F01  =  PCC-F01
      AND     SVR-F02  =  PCC-F02
      AND     SVR-F03  =  PCC-F03
          MOVE      "OK"           TO    PARA-KEKKA
      ELSE
          DISPLAY "# ｹｯｶﾊﾝﾃｲ   = " PCC-F06       UPON CONS
          DISPLAY "# ｼﾞｯｺｳﾋﾂﾞｹ = " PCC-F01 ":"SVR-F01 UPON CONS
          DISPLAY "# ｼﾞｯｺｳｼﾞｺｸ = " PCC-F02 ":"SVR-F02 UPON CONS
          DISPLAY "# ｼﾞｯｺｳｹﾝｽｳ = " PCC-F03 ":"SVR-F03 UPON CONS
          MOVE      "NG"           TO    PARA-KEKKA
      END-IF.
*
 200-MAIN-SEC-EXT.
     EXIT.
***********************************************************
*      300    終了処理
***********************************************************
 300-END-SEC                     SECTION.
*
      CLOSE                      JISSVRL2  JISPCCL2.
*
 300-END-SEC-EXT.
     EXIT.
*
*****************<<  PROGRAM  END  >>***********************

```
