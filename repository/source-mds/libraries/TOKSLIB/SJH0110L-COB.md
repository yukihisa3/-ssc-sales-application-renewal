# SJH0110L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJH0110L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　在庫管理システム                  *
*    業務名　　　　　　　：　日次更新条件マスタ　　　　        *
*    モジュール名　　　　：　日次更新条件マスタリスト          *
*    作成日／作成者　　　：　05/12/19   NAV松野              *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJH0110L.
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
****<< 日次更新条件マスタ >>******************************
     SELECT   JHMNITF   ASSIGN  TO   DA-01-VI-JHMNITL1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   SEQUENTIAL
                        RECORD  KEY          IS   NIT-F01
                        FILE    STATUS       IS   NIT-STATUS.
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     FJH01101          OF   XMDLIB.
****<< 仕入先マスタファイル >>******************************
 FD    JHMNITF
       BLOCK     CONTAINS   2   RECORDS.
       COPY      JHMNITF     OF  XFDLIB
       JOINING   NIT        AS  PREFIX.
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
     02 NIT-STATUS           PIC  X(2).
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
     03  SYS-DATE1                PIC  9(08).
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
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SJH0110L".
     02  FILLER              PIC  X(22)  VALUE  SPACE.
     02  FILLER              PIC  N(16)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＜　日次更新条件マスタリスト　＞".
     02  FILLER              PIC  X(16)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"処理日".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-YY                PIC  9(04).
     02  FILLER              PIC  N(1)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"年".
     02  H-MM                PIC  Z9.
     02  FILLER              PIC  N(1)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"月".
     02  H-DD                PIC  Z9.
     02  FILLER              PIC  N(1)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"日".
     02  FILLER              PIC  X(3)   VALUE  SPACE.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"頁".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  PAGE-SUU            PIC  ZZ9.
****  見出し行２             ****
 01  MIDASI-2       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(07)  VALUE
         NC"帳票出力範囲：".
     02  MSDATE-YYYY         PIC  9(04)  VALUE  ZERO.
     02  FILLER              PIC  X(01)  VALUE  "/".
     02  MSDATE-MM           PIC  9(02)  VALUE  ZERO.
     02  FILLER              PIC  X(01)  VALUE  "/".
     02  MSDATE-DD           PIC  9(02)  VALUE  ZERO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE  NC"～".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MEDATE-YYYY         PIC  9(04)  VALUE  ZERO.
     02  FILLER              PIC  X(01)  VALUE  "/".
     02  MEDATE-MM           PIC  9(02)  VALUE  ZERO.
     02  FILLER              PIC  X(01)  VALUE  "/".
     02  MEDATE-DD           PIC  9(02)  VALUE  ZERO.
****  見出し行３             ****
 01  MIDASI-3       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(53)  VALUE  SPACE.
     02  FILLER              PIC  N(15)  VALUE
         NC"＜　日　次　更　新　条　件　＞".
****  見出し行４             ****
 01  MIDASI-4       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"日付".
     02  FILLER              PIC  X(12)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"実行区分".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"月次区分".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"実行時間".
     02  FILLER              PIC  X(14)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"発注日".
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"納品日".
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"出荷日".
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"実行結果".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
 01  SEN2       CHARACTER     TYPE   IS   NIHONGO.
     03  FILLER                  PIC  N(25)  VALUE
       NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
       NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
       NC"──────────────────".
****  明細行１               ****
 01  MEISAI-1       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  MEI-01.
         03  MEI-011         PIC  9(04).
         03  SURA1           PIC  X(01)  VALUE  "/".
         03  MEI-012         PIC  9(02).
         03  SURA2           PIC  X(01)  VALUE  "/".
         03  MEI-013         PIC  9(02).
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  MEI-02.
         03  MEI-021         PIC  X(01).
         03  FILLER          PIC  X(01)  VALUE  SPACE.
         03  MEI-022         PIC  N(05).
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  MEI-08.
         03  MEI-081         PIC  X(01).
         03  FILLER          PIC  X(01)  VALUE  SPACE.
         03  MEI-082         PIC  N(05).
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  MEI-03              PIC  9(04).
     02  FILLER              PIC  X(14)  VALUE  SPACE.
     02  MEI-04.
         03  MEI-041         PIC  9(04).
         03  SURA3           PIC  X(01)  VALUE  "/".
         03  MEI-042         PIC  9(02).
         03  SURA4           PIC  X(01)  VALUE  "/".
         03  MEI-043         PIC  9(02).
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  MEI-05.
         03  MEI-051         PIC  9(04).
         03  SURA5           PIC  X(01)  VALUE  "/".
         03  MEI-052         PIC  9(02).
         03  SURA6           PIC  X(01)  VALUE  "/".
         03  MEI-053         PIC  9(02).
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  MEI-06.
         03  MEI-061         PIC  9(04).
         03  SURA7           PIC  X(01)  VALUE  "/".
         03  MEI-062         PIC  9(02).
         03  SURA8           PIC  X(01)  VALUE  "/".
         03  MEI-063         PIC  9(02).
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  MEI-07.
         03  MEI-071         PIC  X(01).
         03  FILLER          PIC  X(01)  VALUE  SPACE.
         03  MEI-072         PIC  N(05).
**** エラーメッセージ         ****
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(30)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(30)  VALUE
            NC"開始・終了コードの関係に誤りがあります。".
     02  MSG-ERR3            PIC  N(30)  VALUE
            NC"三年以上先のデータは印刷できません。".
     02  MSG-ERR4            PIC  N(30)  VALUE
            NC"開始日付に誤りがあります。".
     02  MSG-ERR5            PIC  N(30)  VALUE
            NC"終了日付に誤りがあります。".
     02  PMSG01              PIC N(20) VALUE
            NC"_取消　_終了".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SJH0110L".
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
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  JHMNITF.
     MOVE   "JHMNITF  "        TO    ERR-FL-ID.
     MOVE    NIT-STATUS       TO    ERR-STCD.
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
 SJH0110L-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     IF   PG-END   NOT =  "END"
          MOVE     SPACE   TO   END-FLG
          GO  TO   SJH0110L-START
     END-IF.
     STOP     RUN.
 SJH0110L-END.
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
     OPEN     INPUT     JHMNITF.
     OPEN     OUTPUT    PRINTF.
     MOVE    "FJH01101"  TO     DSP-FORMAT.
     MOVE     SPACE     TO     FJH01101.
     MOVE     SPACE     TO     MSG1.
     MOVE     PMSG01    TO     MSG2.
     MOVE     SYS-DATE  TO     STDATE  EDDATE.
     MOVE    "0"        TO     DSP-FLG.
     MOVE     62        TO     L-CNT.
     MOVE     1         TO     P-CNT.
     PERFORM  DSP-SEC.
     IF       END-FLG   NOT =  SPACE
              GO   TO   INIT-END
     END-IF.
     MOVE     STDATE    TO     NIT-F01.
     START    JHMNITF    KEY  IS  >=  NIT-F01
              INVALID   MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END.
     READ     JHMNITF
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END
     END-READ.
     IF       NIT-F01     >    EDDATE
              MOVE  "END"    TO   END-FLG
     END-IF.
*
 INIT-END.
     EXIT.
************************************************************
*      _１     画面処理                                   *
************************************************************
 DSP-SEC                SECTION.
*ボディ部
*画面表示
     MOVE     SPACE     TO     DSP-PROC.
     MOVE    "SCREEN"   TO     DSP-GROUP.
     MOVE     HEN-DATE  TO     SDATE.
     MOVE     HEN-TIME  TO     STIME.
     WRITE    FJH01101.
*画面入力
     MOVE     SPACE     TO     MSG1.
     MOVE     "NE"      TO     DSP-PROC.
     MOVE    "GPHEAD"   TO     DSP-GROUP.
     READ     DSPF.
     COMPUTE  SYS-DATE1 = SYS-DATE + 30000.
*アテンション判定
     EVALUATE   DSP-FUNC
         WHEN   "F004"
                MOVE   SYS-DATE TO    STDATE  EDDATE
                MOVE   "M"   TO EDIT-OPTION OF STDATE
                MOVE   SPACE TO EDIT-CURSOR OF STDATE
                MOVE   "M"   TO EDIT-OPTION OF EDDATE
                MOVE   SPACE TO EDIT-CURSOR OF EDDATE
                GO TO  DSP-SEC
         WHEN   "F005"
                MOVE    "END"   TO    END-FLG
                MOVE    "END"   TO    PG-END
                GO TO  DSP-END
         WHEN   "E000"
                MOVE     "2"                 TO   LINK-IN-KBN
                MOVE     ZERO                TO   LINK-IN-YMD6
                MOVE     STDATE              TO   LINK-IN-YMD8
                MOVE     ZERO                TO   LINK-OUT-RET
                MOVE     ZERO                TO   LINK-OUT-YMD
                CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                                  LINK-IN-YMD6
                                                  LINK-IN-YMD8
                                                  LINK-OUT-RET
                                                  LINK-OUT-YMD
                IF   LINK-OUT-RET   NOT = ZERO
                     MOVE   MSG-ERR4  TO  MSG1
                     MOVE  "R"   TO   EDIT-OPTION  OF  STDATE
                     MOVE  "C"   TO   EDIT-CURSOR  OF  STDATE
                     GO TO  DSP-SEC
                END-IF
                MOVE     "2"                 TO   LINK-IN-KBN
                MOVE     ZERO                TO   LINK-IN-YMD6
                MOVE     EDDATE              TO   LINK-IN-YMD8
                MOVE     ZERO                TO   LINK-OUT-RET
                MOVE     ZERO                TO   LINK-OUT-YMD
                CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                                  LINK-IN-YMD6
                                                  LINK-IN-YMD8
                                                  LINK-OUT-RET
                                                  LINK-OUT-YMD
                IF   LINK-OUT-RET   NOT = ZERO
                     MOVE   MSG-ERR5  TO  MSG1
                     MOVE  "R"   TO   EDIT-OPTION  OF  EDDATE
                     MOVE  "C"   TO   EDIT-CURSOR  OF  EDDATE
                     GO TO  DSP-SEC
                END-IF
                IF   STDATE >  EDDATE
                     MOVE   MSG-ERR2  TO  MSG1
                     MOVE  "R"   TO   EDIT-OPTION  OF  STDATE
                     MOVE  "C"   TO   EDIT-CURSOR  OF  STDATE
                     MOVE  "R"   TO   EDIT-OPTION  OF  EDDATE
                     MOVE  "C"   TO   EDIT-CURSOR  OF  EDDATE
                     GO TO  DSP-SEC
                END-IF
                IF   STDATE >  SYS-DATE1
                     MOVE   MSG-ERR3  TO  MSG1
                     MOVE  "R"   TO   EDIT-OPTION  OF  STDATE
                     MOVE  "C"   TO   EDIT-CURSOR  OF  STDATE
                     GO TO  DSP-SEC
                END-IF
                IF   EDDATE >  SYS-DATE1
                     MOVE   MSG-ERR3  TO  MSG1
                     MOVE  "R"   TO   EDIT-OPTION  OF  EDDATE
                     MOVE  "C"   TO   EDIT-CURSOR  OF  EDDATE
                     GO TO  DSP-SEC
                END-IF
         WHEN   OTHER
                MOVE   MSG-ERR1  TO  MSG1
                GO TO  DSP-SEC
     END-EVALUATE.
     MOVE   "M"   TO EDIT-OPTION OF STDATE.
     MOVE   SPACE TO EDIT-CURSOR OF STDATE.
     MOVE   "M"   TO EDIT-OPTION OF EDDATE.
     MOVE   SPACE TO EDIT-CURSOR OF EDDATE.
*確認部
*画面表示
     MOVE     SPACE     TO     DSP-PROC.
     MOVE    "SCREEN"   TO     DSP-GROUP.
     MOVE     HEN-DATE  TO     SDATE.
     MOVE     HEN-TIME  TO     STIME.
     WRITE    FJH01101.
*画面入力
     MOVE     SPACE     TO     MSG1.
     MOVE     "NE"      TO     DSP-PROC.
     MOVE    "GPKAKU"   TO     DSP-GROUP.
     READ     DSPF.
*アテンション判定
     EVALUATE   DSP-FUNC
         WHEN   "F004"
                MOVE   SYS-DATE TO    STDATE  EDDATE
                MOVE   "M"   TO EDIT-OPTION OF STDATE
                MOVE   SPACE TO EDIT-CURSOR OF STDATE
                MOVE   "M"   TO EDIT-OPTION OF EDDATE
                MOVE   SPACE TO EDIT-CURSOR OF EDDATE
                GO TO  DSP-SEC
         WHEN   "F005"
                MOVE    "END"   TO    END-FLG
                MOVE    "END"   TO    PG-END
                GO TO  DSP-END
         WHEN   "E000"
                CONTINUE
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
     MOVE     NIT-F01(1:4)    TO      MEI-011.
     MOVE     NIT-F01(5:2)    TO      MEI-012.
     MOVE     NIT-F01(7:2)    TO      MEI-013.
     MOVE     "/"             TO      SURA1 SURA2.
     MOVE     NIT-F02         TO      MEI-021.
     EVALUATE NIT-F02
        WHEN  "1"
              MOVE NC"実行　有　" TO  MEI-022
        WHEN  "9"
              MOVE NC"実行　無　" TO  MEI-022
        WHEN  " "
              MOVE NC"未設定　　" TO  MEI-022
     END-EVALUATE.
     MOVE     NIT-F10         TO      MEI-081.
     EVALUATE NIT-F10
        WHEN  "1"
              MOVE NC"実行　有　" TO  MEI-082
        WHEN  "9"
              MOVE NC"実行　無　" TO  MEI-082
        WHEN  " "
              MOVE NC"未設定　　" TO  MEI-082
     END-EVALUATE.
     MOVE     NIT-F03         TO      MEI-03.
     IF       NIT-F05         NOT  =  SPACE  AND  ZERO
              MOVE  NIT-F05(1:4)   TO      MEI-041
              MOVE  NIT-F05(5:2)   TO      MEI-042
              MOVE  NIT-F05(7:2)   TO      MEI-043
              MOVE  "/"            TO      SURA3 SURA4
     ELSE
              MOVE     SPACE       TO      MEI-04
                                           SURA3 SURA4
     END-IF.
     MOVE     NIT-F06(1:4)    TO      MEI-051.
     MOVE     NIT-F06(5:2)    TO      MEI-052.
     MOVE     NIT-F06(7:2)    TO      MEI-053.
     MOVE     "/"             TO      SURA5 SURA6.
     IF       NIT-F07         NOT  =  SPACE  AND  ZERO
              MOVE  NIT-F07(1:4)   TO      MEI-061
              MOVE  NIT-F07(5:2)   TO      MEI-062
              MOVE  NIT-F07(7:2)   TO      MEI-063
              MOVE  "/"            TO      SURA7 SURA8
     ELSE
              MOVE     SPACE       TO      MEI-06
                                           SURA7 SURA8
     END-IF.
     MOVE     NIT-F08         TO      MEI-071.
     EVALUATE NIT-F08
        WHEN  "1"
              MOVE NC"正常終了　" TO  MEI-072
        WHEN  "9"
              MOVE NC"異常終了　" TO  MEI-072
        WHEN  SPACE
              MOVE NC"未実行　　" TO  MEI-072
     END-EVALUATE.
     WRITE    P-REC         FROM    MEISAI-1    AFTER  1.
     WRITE    P-REC         FROM    SEN1        AFTER  1.
     ADD      2             TO      L-CNT.
     READ     JHMNITF
              AT END   MOVE  "END"  TO  END-FLG
     END-READ.
     IF       NIT-F01         >       EDDATE
                       MOVE  "END"  TO  END-FLG
     END-IF.
 MAIN-END.
     EXIT.
************************************************************
*      2.1       見出し処理                                *
************************************************************
 MIDASI-SEC             SECTION.
     MOVE     SYS-DATE(1:4)   TO      H-YY.
     MOVE     SYS-DATE(5:2)   TO      H-MM.
     MOVE     SYS-DATE(7:2)   TO      H-DD.
     MOVE     STDATE  (1:4)   TO      MSDATE-YYYY.
     MOVE     STDATE  (5:2)   TO      MSDATE-MM.
     MOVE     STDATE  (7:2)   TO      MSDATE-DD.
     MOVE     EDDATE  (1:4)   TO      MEDATE-YYYY.
     MOVE     EDDATE  (5:2)   TO      MEDATE-MM.
     MOVE     EDDATE  (7:2)   TO      MEDATE-DD.
     MOVE     P-CNT      TO      PAGE-SUU.
     IF       P-CNT  = 1
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    MIDASI-2   AFTER  2
              WRITE      P-REC   FROM    SEN2       AFTER  1
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
              WRITE      P-REC   FROM    MIDASI-4   AFTER  1
              WRITE      P-REC   FROM    SEN2       AFTER  1
        ELSE
              MOVE       SPACE   TO      P-REC
              WRITE      P-REC   AFTER   PAGE
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    MIDASI-2   AFTER  2
              WRITE      P-REC   FROM    SEN2       AFTER  1
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
              WRITE      P-REC   FROM    MIDASI-4   AFTER  1
              WRITE      P-REC   FROM    SEN2       AFTER  1
     END-IF.
     ADD      1  TO      P-CNT.
     MOVE     8  TO      L-CNT.
 MIDASI-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF   PRINTF   JHMNITF.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
