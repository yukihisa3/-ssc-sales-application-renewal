****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷検品システム　　　　　　　　　*
*    モジュール名　　　　：　検品グループマスタリスト          *
*    作成日／更新日　　　：　01/05/09                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　検品グループマスタリストの出力    *
*    更新日　　　　　　　：　                                  *
*    　　　　　　　　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKE0071L.
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
****<< 検品グループマスタ >>******************************
     SELECT   SOKKPGF   ASSIGN  TO    DA-01-VI-SOKKPGL1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   SEQUENTIAL
                        RECORD  KEY          KPG-F01
                                             KPG-F02
                        FILE    STATUS       KPG-STATUS.
****<< 倉庫マスタファイル >>******************************
     SELECT   ZSOKMS    ASSIGN  TO   DA-01-VI-ZSOKMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   SOK-F01
                        FILE    STATUS       IS   SOK-STATUS.
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     FKE00711          OF   XMDLIB.
****<< 検品グループマスタ >>******************************
 FD  SOKKPGF
     LABEL       RECORD    IS        STANDARD.
     COPY        SOKKPGF   OF        XFDLIB
     JOINING     KPG       AS        PREFIX.
****<< 倉庫マスタファイル >>******************************
 FD  ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
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
     02 KPG-STATUS           PIC  X(2).
     02 SOK-STATUS           PIC  X(2).
****  キー情報          ****
 01  KEY-AREA.
     03  CUR-SOKO            PIC  X(02)   VALUE SPACE.
     03  BRK-SOKO            PIC  X(02)   VALUE SPACE.
     03  BRK-FLG             PIC  X(01)   VALUE SPACE.
****  カウンタ                ****
 01  L-CNT                   PIC  9(2).
 01  P-CNT                   PIC  9(3).
****  フラグ                  ****
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
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(30)  VALUE  SPACE.
     02  FILLER              PIC  N(20)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＊＊＊　検品グループマスタリスト　＊＊＊".
     02  FILLER              PIC  X(18)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"処理日".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-YY                PIC  99.
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-MM                PIC  Z9.
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-DD                PIC  Z9.
     02  FILLER              PIC  X(3)   VALUE  SPACE.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"頁".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  PAGE-SUU            PIC  ZZ9.
     02  FILLER              PIC  X(8)   VALUE  SPACE.
****  見出し行２             ****
 01  MIDASI-2       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(10)  VALUE
         NC"出荷場所　　　　　　".
     02  FILLER              PIC  X(23)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"検品Ｇ".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"検品Ｇ名称".
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  FILLER              PIC  X(07)  VALUE  "ﾋﾟｯｷﾝｸﾞ".
     02  FILLER              PIC  N(05)  VALUE
         NC"種別".
****  線  ****
 01  SEN            CHARACTER     TYPE   IS   NIHONGO.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  SEN1.
     03  FILLER              PIC  X(136) VALUE ALL "-".
****  明細行１               ****
 01  MEISAI-1       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  MEI-01              PIC  X(02).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  MEI-011             PIC  N(18).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-02              PIC  9(05).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-03              PIC  N(10).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  MEI-04              PIC  X(01).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEI-041             PIC  N(03).
**** エラーメッセージ         ****
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(30)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(30)  VALUE
            NC"開始・終了コードの関係に誤りがあります。".
     02  PMSG01              PIC N(20) VALUE
            NC"_取消　_終了".
     02  PMSG02              PIC N(20) VALUE
            NC"_終了".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SKE0071L".
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
*=============================================================
 LINKAGE             SECTION.
*=============================================================
   01  LINK-SOKCD            PIC  X(02).
   01  LINK-DSOKCD           PIC  X(02).
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
 PROCEDURE               DIVISION      USING    LINK-SOKCD
                                                LINK-DSOKCD.
*
 DECLARATIVES.
 FILEERR-SEC0           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SOKKPGF.
     MOVE   "SOKKPGF "        TO    ERR-FL-ID.
     MOVE    KPG-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS  "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
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
 SKE0071L-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
*****IF   PG-END   NOT =  "END"
*****     MOVE     SPACE   TO   END-FLG
*****     GO  TO   SKE0071L-START
*****END-IF.
     STOP     RUN.
 SKE0071L-END.
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
     OPEN     INPUT     SOKKPGF   ZSOKMS.
     OPEN     OUTPUT    PRINTF.
     MOVE    "FKE00711" TO     DSP-FORMAT.
     MOVE     SPACE     TO     FKE00711.
     MOVE     SPACE     TO     MSG1.
     MOVE     PMSG01    TO     MSG2.
     MOVE    "0"        TO     DSP-FLG.
*
     IF       LINK-DSOKCD      NOT =  "01"  AND "88"
              MOVE     PMSG02  TO      MSG2
              MOVE     "1"     TO      DSP-FLG
              MOVE      LINK-SOKCD     TO   SCODE
                                            ECODE
     END-IF.
*
     MOVE     62        TO     L-CNT.
     MOVE     1         TO     P-CNT.
     PERFORM  DSP-SEC.
     IF       END-FLG   NOT =  SPACE
              GO   TO   INIT-END
     END-IF.
     MOVE     SCODE     TO     KPG-F01.
     MOVE     ZERO      TO     KPG-F02.
     START    SOKKPGF   KEY  IS  >=  KPG-F01
                                     KPG-F02
              INVALID   MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END.
     READ     SOKKPGF
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END
     END-READ.
     IF       KPG-F01   >    ECODE
              MOVE  "END"    TO   END-FLG
     END-IF.
     MOVE     KPG-F01   TO   CUR-SOKO.
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
     WRITE    FKE00711.
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
                     IF    LINK-DSOKCD    NOT =  "01" AND "88"
                           MOVE   MSG-ERR1  TO  MSG1
                     ELSE
                           MOVE   "0"       TO  DSP-FLG
                           MOVE   SPACE     TO  HEAD
                     END-IF
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
                     MOVE   "  "  TO  SCODE
                END-IF
                IF   ECODE  =    SPACE
                     MOVE   "99"   TO  ECODE
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
     IF       CUR-SOKO       NOT =     BRK-SOKO
              MOVE      KPG-F01        TO   MEI-01
              PERFORM   SOK-READ
              MOVE      CUR-SOKO       TO   BRK-SOKO
              MOVE     "1"             TO   BRK-FLG
     ELSE
              MOVE      SPACE          TO   MEI-01
                                            MEI-011
                                            BRK-FLG
     END-IF.
     MOVE     KPG-F02       TO      MEI-02.
     MOVE     KPG-F03       TO      MEI-03.
     MOVE     KPG-F04       TO      MEI-04.
     EVALUATE KPG-F04
         WHEN "1"
              MOVE  NC"ケース"   TO  MEI-041
         WHEN "2"
              MOVE  NC"ピース"   TO  MEI-041
         WHEN  OTHER
              MOVE  SPACE        TO  MEI-041
     END-EVALUATE.
     IF       BRK-FLG   =   "1"  AND
              L-CNT     >    6
              WRITE     P-REC    FROM  SEN1     AFTER  1
              ADD       1        TO    L-CNT
     END-IF.
     WRITE    P-REC         FROM    MEISAI-1    AFTER  1.
     ADD      1             TO      L-CNT.
     READ     SOKKPGF
              AT END   MOVE  "END"  TO  END-FLG
      NOT AT END
         IF   KPG-F01       >       ECODE
              MOVE    "END"      TO   END-FLG
         ELSE
              MOVE     KPG-F01   TO   CUR-SOKO
         END-IF
     END-READ.
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
              WRITE      P-REC   FROM    SEN        AFTER  2
              WRITE      P-REC   FROM    MIDASI-2   AFTER  1
              WRITE      P-REC   FROM    SEN        AFTER  1
        ELSE
              MOVE       SPACE   TO      P-REC
              WRITE      P-REC   AFTER   PAGE
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    SEN        AFTER  2
              WRITE      P-REC   FROM    MIDASI-2   AFTER  1
              WRITE      P-REC   FROM    SEN        AFTER  1
     END-IF.
     ADD      1  TO      P-CNT.
     MOVE     6  TO      L-CNT.
     MOVE     SPACE      TO       BRK-SOKO.
 MIDASI-END.
     EXIT.
************************************************************
*    2.1.2        倉庫名の取得                             *
************************************************************
 SOK-READ                        SECTION.
     MOVE    KPG-F01        TO   SOK-F01.
     READ    ZSOKMS
       INVALID      KEY
          MOVE      SPACE   TO   MEI-011
       NOT INVALID  KEY
          MOVE      SOK-F02 TO   MEI-011
     END-READ.
 SOK-READ-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF   PRINTF   SOKKPGF  ZSOKMS.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
