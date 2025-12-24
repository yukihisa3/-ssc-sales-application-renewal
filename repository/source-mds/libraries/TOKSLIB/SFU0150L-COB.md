# SFU0150L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SFU0150L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　在庫管理システム                  *
*    業務名　　　　　　　：　マスタメンテ　　　　　　　        *
*    モジュール名　　　　：　部門取引先マスタリスト            *
*    作成日／作成者　　　：　00/06/29  Y.YOSHIDA.              *
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SFU0150L.
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
****<< 部門取引先マスタファイル >>******************************
     SELECT   BUTOKMF    ASSIGN  TO   DA-01-VI-BUTOKML1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   SEQUENTIAL
                        RECORD  KEY          IS   BUT-F01
                        FILE    STATUS       IS   BUT-STATUS.
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     FFU01501          OF   XMDLIB.
****<< 部門取引先マスタファイル >>******************************
 FD    BUTOKMF.
       COPY      BUTOKMF    OF  XFDLIB
       JOINING   BUT        AS  PREFIX.
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
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 BUT-STATUS           PIC  X(2).
****  カウンタ                ****
 01  L-CNT                   PIC  9(2).
 01  P-CNT                   PIC  9(3)   VALUE  ZERO.
 01  IN-CNT                  PIC  9(7)   VALUE  ZERO.
 01  OUT-CNT                 PIC  9(7)   VALUE  ZERO.
****  フラグ                  ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08)  VALUE  ZERO.
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
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SFU0150L".
     02  FILLER              PIC  X(21)  VALUE  SPACE.
     02  FILLER              PIC  N(19)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＊＊＊　部門取引先マスタリスト　＊＊＊".
     02  FILLER              PIC  X(15)  VALUE  SPACE.
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
 01  MIDASI-2       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(11)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"取引先".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"取引先名称".
     02  FILLER              PIC  X(27)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"部門".
****  見出し行３             ****
 01  MIDASI-3       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  N(68)  VALUE  ALL NC"─".
 01  SEN1.
     03  FILLER              PIC  X(136)  VALUE
         ALL "-".
****  明細行１               ****
 01  MEISAI-1       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(11)  VALUE  SPACE.
     02  BUT-01              PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  BUT-02              PIC  N(18).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  BUT-03              PIC  X(04).
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SFU0150L".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*---------------- 99/09/28追加 --------------------------*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*--------------------------------------------------------*
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  BUTOKMF.
     MOVE   "BUTOKMF  "        TO    ERR-FL-ID.
     MOVE    BUT-STATUS       TO    ERR-STCD.
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
 END     DECLARATIVES.
************************************************************
 SFU0150L-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     IF   PG-END   NOT =  "END"
          MOVE     SPACE   TO   END-FLG
          GO  TO   SFU0150L-START
     END-IF.
     STOP     RUN.
 SFU0150L-END.
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
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
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
     OPEN     INPUT     BUTOKMF.
     OPEN     OUTPUT    PRINTF.
     MOVE    "FFU01501" TO     DSP-FORMAT.
     MOVE     SPACE     TO     FFU01501.
     MOVE     SPACE     TO     MSG1.
     MOVE     PMSG01    TO     MSG2.
     MOVE    "0"        TO     DSP-FLG.
     MOVE     62        TO     L-CNT.
     PERFORM  DSP-SEC.
     IF       END-FLG   NOT =  SPACE
              GO   TO   INIT-END
     END-IF.
     MOVE     SCODE     TO     BUT-F01.
     START    BUTOKMF    KEY  IS  >=  BUT-F01
              INVALID   MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END.
     READ     BUTOKMF
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END
     END-READ.
     IF       BUT-F01     >    ECODE
              MOVE  "END"    TO   END-FLG
     END-IF.
     ADD      1    TO   IN-CNT.
*
 INIT-END.
     EXIT.
************************************************************
*      _１     画面処理                                   *
************************************************************
 DSP-SEC                SECTION.
*画面表示
     MOVE     SPACE     TO     DSP-PROC.
     MOVE    "GPALL "   TO     DSP-GROUP.
     MOVE     HEN-DATE  TO     SDATE.
     MOVE     HEN-TIME  TO     STIME.
     WRITE    FFU01501.
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
                GO TO  DSP-SEC
         WHEN   "F005"
                MOVE    "END"   TO    END-FLG
                MOVE    "END"   TO    PG-END
                GO TO  DSP-END
         WHEN   "E000"
                IF   SCODE  =    SPACE
                     MOVE   "        "   TO  SCODE
                END-IF
                IF   ECODE  =    SPACE
                     MOVE   "99999999"   TO  ECODE
                END-IF
                IF   SCODE >   ECODE
                     MOVE   MSG-ERR2  TO  MSG1
                     GO TO  DSP-SEC
                END-IF
                IF   DSP-FLG   =   "0"
                     MOVE   "1"       TO  DSP-FLG
                     GO TO  DSP-SEC
                ELSE
                     MOVE   "END"     TO  PG-END
                END-IF
         WHEN   OTHER
                MOVE   MSG-ERR1  TO  MSG1
                GO TO  DSP-SEC
     END-EVALUATE.
 DSP-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
     IF       L-CNT         >=      62
              PERFORM       MIDASI-SEC
     END-IF.
     MOVE     BUT-F01         TO      BUT-01.
     MOVE     BUT-F02         TO      BUT-02.
     MOVE     BUT-F03         TO      BUT-03.
     WRITE    P-REC         FROM    MEISAI-1    AFTER  1.
     WRITE    P-REC         FROM    SEN1        AFTER  1.
     ADD      2             TO      L-CNT.
     ADD      1             TO      OUT-CNT.
     READ     BUTOKMF
              AT END   MOVE  "END"  TO  END-FLG
                       GO     TO    MAIN-END
     END-READ.
     IF       BUT-F01         >       ECODE
                       MOVE  "END"  TO  END-FLG
                       GO     TO    MAIN-END
     END-IF.
     ADD      1    TO      IN-CNT.
 MAIN-END.
     EXIT.
************************************************************
*      2.1       見出し処理                                *
************************************************************
 MIDASI-SEC             SECTION.
     MOVE     SYS-DATE(1:4)      TO      H-YY.
     MOVE     SYS-DATE(5:2)      TO      H-MM.
     MOVE     SYS-DATE(7:2)      TO      H-DD.
     ADD      1          TO      P-CNT.
     MOVE     P-CNT      TO      PAGE-SUU.
     IF       P-CNT      =       1
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    MIDASI-3   AFTER  2
              WRITE      P-REC   FROM    MIDASI-2   AFTER  1
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
     ELSE
              MOVE       SPACE   TO      P-REC
              WRITE      P-REC   AFTER   PAGE
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    MIDASI-3   AFTER  2
              WRITE      P-REC   FROM    MIDASI-2   AFTER  1
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
     END-IF.
     MOVE     6  TO      L-CNT.
 MIDASI-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF   PRINTF   BUTOKMF.
*
     DISPLAY "* BUTOKMF (IN)=" IN-CNT   " *" UPON CONS.
     DISPLAY "* BUTOKMF (OT)=" OUT-CNT  " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" P-CNT    " *" UPON CONS.
*
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
