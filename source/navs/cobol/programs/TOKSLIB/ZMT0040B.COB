****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　在庫管理システム                  *
*    業務名　　　　　　　：　仕入先マスタ　　　　　　　        *
*    モジュール名　　　　：　仕入先マスタリスト                *
*    作成日／作成者　　　：　93/04/14  /NAV                    *
*    更新日／更新者　　　：　99/09/28  /HAGIWARA               *
*          ・新郵便番号追加                                    *
*          ・仕入先マスタをＦＤＧより参照するよう修正          *
*          ・日付８桁変換ＰＧ（ＳＫＹＤＴＣＫＢ）使用          *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZMT0040B.
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
****<< 仕入先マスタファイル >>******************************
     SELECT   ZSHIMS    ASSIGN  TO   DA-01-VI-ZSHIMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   SEQUENTIAL
                        RECORD  KEY          IS   SHI-F01
                        FILE    STATUS       IS   SHI-STATUS.
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     ZMT0040           OF   XMDLIB.
****<< 仕入先マスタファイル >>******************************
*-------- 99/09/28修正 -------------------*
 FD    ZSHIMS
       BLOCK     CONTAINS   2   RECORDS.
       COPY      ZSHIMS     OF  XFDLIB
       JOINING   SHI        AS  PREFIX.
*01  SHIIRE-R.
*    02 SHI01                PIC  X(08).
*    02 SHI02                PIC  N(18).
*    02 SHI03                PIC  X(25).
*    02 SHI04                PIC  N(10).
*    02 SHI05                PIC  X(15).
*    02 SHI06                PIC  X(05).
*    02 SHI07                PIC  N(18).
*    02 SHI08                PIC  N(18).
*    02 SHI09                PIC  X(15).
*    02 SHI10                PIC  X(15).
*    02 SHI11                PIC  9(02).
*    02 FILLER               PIC  X(37).
*-----------------------------------------*
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
     02 SHI-STATUS           PIC  X(2).
****  カウンタ                ****
 01  L-CNT                   PIC  9(2).
 01  P-CNT                   PIC  9(3).
****  フラグ                  ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
*----------- 99/09/28修正 ---------------------*
****  日付保存                ****
*01  SYS-DATE                PIC  9(06).
*01  SYS-DATE8.
*    03  SYS-YYYY            PIC  9(04).
*    03  SYS-MM              PIC  9(02).
*    03  SYS-DD              PIC  9(02).
*----------------------------------------------*
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
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(34)  VALUE  SPACE.
     02  FILLER              PIC  N(17)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＊＊＊　仕入先マスタリスト　＊＊＊".
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
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(12)  VALUE
         NC"仕入先　仕入先名（漢字）".
     02  FILLER              PIC  X(12)  VALUE  SPACE.
     02  FILLER              PIC  N(08)  VALUE
         NC"担当部課（漢字）".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE
         NC"〒".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"住　所　１".
     02  FILLER              PIC  X(19)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"住　所　２".
     02  FILLER              PIC  X(19)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"ＴＥＬ".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
****  見出し行３             ****
 01  MIDASI-3       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(17)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"（カナ）".
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"（カナ）".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"県コード".
     02  FILLER              PIC  X(50)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"ＦＡＸ".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
****  明細行１               ****
 01  MEISAI-1       CHARACTER     TYPE   IS   YB.
     02  SHI-01              PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHI-02              PIC  N(18).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHI-04              PIC  N(10).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHI-121             PIC  X(03).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  SHI-122             PIC  X(04).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHI-07              PIC  N(18).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  SHI-08              PIC  N(18).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  SHI-09              PIC  X(15).
****  明細行２               ****
 01  MEISAI-2.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  SHI-03              PIC  X(25).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  SHI-05              PIC  X(15).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHI-06              PIC  X(05).
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  SHI-11              PIC  9(02).
     02  FILLER              PIC  X(56)  VALUE  SPACE.
     02  SHI-10              PIC  X(15).
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZMT0040B".
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
                   PROCEDURE  ZSHIMS.
     MOVE   "ZSHIMS  "        TO    ERR-FL-ID.
     MOVE    SHI-STATUS       TO    ERR-STCD.
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
 ZMT0040B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     IF   PG-END   NOT =  "END"
          MOVE     SPACE   TO   END-FLG
          GO  TO   ZMT0040B-START
     END-IF.
     STOP     RUN.
 ZMT0040B-END.
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
     OPEN     I-O       DSPF.
     OPEN     INPUT     ZSHIMS.
     OPEN     OUTPUT    PRINTF.
     MOVE    "ZMT0040"  TO     DSP-FORMAT.
     MOVE     SPACE     TO     ZMT0040.
     MOVE     SPACE     TO     MSG1.
     MOVE     PMSG01    TO     MSG2.
     MOVE    "0"        TO     DSP-FLG.
     MOVE     62        TO     L-CNT.
     MOVE     1         TO     P-CNT.
     PERFORM  DSP-SEC.
     IF       END-FLG   NOT =  SPACE
              GO   TO   INIT-END
     END-IF.
     MOVE     SCODE     TO     SHI-F01.
     START    ZSHIMS    KEY  IS  >=  SHI-F01
              INVALID   MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END.
     READ     ZSHIMS
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END
     END-READ.
     IF       SHI-F01     >    ECODE
              MOVE  "END"    TO   END-FLG
     END-IF.
*----------------- 99/09/28追加 ---------------------*
*システム日付取得
*    ACCEPT   SYS-DATE          FROM   DATE.
*    MOVE     "3"                 TO   LINK-IN-KBN.
*    MOVE     SYS-DATE            TO   LINK-IN-YMD6.
*    MOVE     ZERO                TO   LINK-IN-YMD8.
*    MOVE     ZERO                TO   LINK-OUT-RET.
*    MOVE     ZERO                TO   LINK-OUT-YMD.
*    CALL     "SKYDTCKB"       USING   LINK-IN-KBN
*                                      LINK-IN-YMD6
*                                      LINK-IN-YMD8
*                                      LINK-OUT-RET
*                                      LINK-OUT-YMD.
*    MOVE      LINK-OUT-YMD       TO   SYS-DATE8.
*----------------------------------------------------*
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
     WRITE    ZMT0040.
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
     MOVE     SHI-F01         TO      SHI-01.
     MOVE     SHI-F02         TO      SHI-02.
     MOVE     SHI-F03         TO      SHI-03.
     MOVE     SHI-F04         TO      SHI-04.
     MOVE     SHI-F05         TO      SHI-05.
     MOVE     SHI-F121        TO      SHI-121.
     MOVE     SHI-F122        TO      SHI-122.
     MOVE     SHI-F06         TO      SHI-06.
     MOVE     SHI-F07         TO      SHI-07.
     MOVE     SHI-F08         TO      SHI-08.
     MOVE     SHI-F09         TO      SHI-09.
     MOVE     SHI-F10         TO      SHI-10.
     MOVE     SHI-F11         TO      SHI-11.
     WRITE    P-REC         FROM    MEISAI-1    AFTER  1.
     WRITE    P-REC         FROM    MEISAI-2    AFTER  1.
     WRITE    P-REC         FROM    SEN1        AFTER  1.
     ADD      3             TO      L-CNT.
     READ     ZSHIMS
              AT END   MOVE  "END"  TO  END-FLG
     END-READ.
     IF       SHI-F01         >       ECODE
                       MOVE  "END"  TO  END-FLG
     END-IF.
 MAIN-END.
     EXIT.
************************************************************
*      2.1       見出し処理                                *
************************************************************
 MIDASI-SEC             SECTION.
     MOVE     WK-Y       TO      H-YY.
     MOVE     WK-M       TO      H-MM.
     MOVE     WK-D       TO      H-DD.
     MOVE     P-CNT      TO      PAGE-SUU.
     IF       P-CNT  = 1
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    MIDASI-2   AFTER  2
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
              WRITE      P-REC   FROM    SEN1       AFTER  1
        ELSE
              MOVE       SPACE   TO      P-REC
              WRITE      P-REC   AFTER   PAGE
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    MIDASI-2   AFTER  2
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
              WRITE      P-REC   FROM    SEN1       AFTER  1
     END-IF.
     ADD      1  TO      P-CNT.
     MOVE     6  TO      L-CNT.
 MIDASI-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF   PRINTF   ZSHIMS.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
