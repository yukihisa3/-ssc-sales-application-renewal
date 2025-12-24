# SJH0120I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJH0120I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　基幹システム　　                  *
*    業務名　　　　　　　：　日次更新条件マスタ　　　　        *
*    モジュール名　　　　：　日次更新条件マスタ日付作成処理    *
*　　作成日／作成者　　　：　05/12/19　ＮＡＶ松野
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJH0120I.
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
                        ACCESS  MODE         IS   DYNAMIC
                        RECORD  KEY          IS   NIT-F01
                        FILE    STATUS       IS   NIT-STATUS.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル        >>******************************
 FD    DSPF.
 COPY     FJH01201          OF   XMDLIB.
****<< 日次更新条件マスタ >>******************************
 FD    JHMNITF
       BLOCK     CONTAINS   2   RECORDS.
       COPY      JHMNITF     OF  XFDLIB
       JOINING   NIT        AS  PREFIX.
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
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
 01  INV-FLG                 PIC  9(01)  VALUE  ZERO.
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
*末日チェック
 01  CHK-DATE-WK.
     03  CHK-01                   PIC  9(02).
     03  CHK-02                   PIC  9(02).
     03  MATUBI                   PIC  X(24)  VALUE
         "312831303130313130313031".
     03  FILLER                   REDEFINES   MATUBI.
         05  WK-MATUBI            PIC  9(02)  OCCURS  12.
 01  WK-DATE1.
         05  WK1-YYYY             PIC  9(04)  VALUE  ZERO.
         05  WK1-MM               PIC  9(02)  VALUE  ZERO.
         05  WK1-DD               PIC  9(02)  VALUE  ZERO.
 01  WK-DATE2.
         05  WK2-YYYY             PIC  9(04)  VALUE  ZERO.
         05  WK2-MM               PIC  9(02)  VALUE  ZERO.
         05  WK2-DD               PIC  9(02)  VALUE  ZERO.
 01  WK-DATE3.
         05  WK3-YYYY             PIC  9(04)  VALUE  ZERO.
         05  WK3-MM               PIC  9(02)  VALUE  ZERO.
         05  WK3-DD               PIC  9(02)  VALUE  ZERO.
 01  WK-YOUBI                     PIC  9(01)  VALUE  ZERO.
**** エラーメッセージ         ****
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(30)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(30)  VALUE
            NC"開始が終了を超えています。".
     02  MSG-ERR3            PIC  N(30)  VALUE
            NC"３年以上先のデータは印刷できません。".
     02  MSG-ERR4            PIC  N(30)  VALUE
            NC"開始日付論理エラー".
     02  MSG-ERR5            PIC  N(30)  VALUE
            NC"終了日付論理エラー".
     02  MSG-ERR6            PIC  N(30)  VALUE
            NC"該当データがありません".
     02  MSG-ERR7            PIC  N(30)  VALUE
            NC"曜日を入力してください".
     02  MSG-ERR8            PIC  N(30)  VALUE
            NC"曜日を正しく入力してください".
     02  PMSG01              PIC N(20) VALUE
            NC"_取消　_終了".
     02  PMSG02              PIC N(20) VALUE
            NC"_取消　_終了　_項目戻し".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SJH0120I".
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
*　　　　　　プログラムコントロール
************************************************************
 SJH0120I-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     IF   PG-END   NOT =  "END"
          MOVE     SPACE   TO   END-FLG
          GO  TO   SJH0120I-START
     END-IF.
     STOP     RUN.
 SJH0120I-END.
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
     OPEN     I-O       JHMNITF.
     MOVE    "FJH01201" TO     DSP-FORMAT.
     MOVE     SPACE     TO     FJH01201.
     MOVE     SYS-DATE  TO     STDATE   EDDATE.
     MOVE     SPACE     TO     MSG1.
     MOVE     PMSG01    TO     MSG2.
     PERFORM  DSP-SEC.
     IF       END-FLG   NOT =  SPACE
              GO   TO   INIT-END
     END-IF.
*
 INIT-END.
     EXIT.
************************************************************
*      _１     画面処理                                   *
************************************************************
 DSP-SEC                SECTION.
***ボディ部
*画面表示
     MOVE     SPACE     TO     DSP-PROC.
     MOVE    "SCREEN"   TO     DSP-GROUP.
     MOVE     HEN-DATE  TO     SDATE.
     MOVE     HEN-TIME  TO     STIME.
     WRITE    FJH01201.
*画面入力
     MOVE     SPACE     TO     MSG1.
     MOVE     "NE"      TO     DSP-PROC.
     MOVE    "GPHEAD"   TO     DSP-GROUP.
     READ     DSPF.
*アテンション判定
     EVALUATE   DSP-FUNC
         WHEN   "F004"
                MOVE  SYS-DATE TO   STDATE  EDDATE
                MOVE  "M"      TO   EDIT-OPTION  OF  STDATE
                MOVE  SPACE    TO   EDIT-CURSOR  OF  STDATE
                MOVE  "M"      TO   EDIT-OPTION  OF  EDDATE
                MOVE  SPACE    TO   EDIT-CURSOR  OF  EDDATE
                MOVE  "M"      TO   EDIT-OPTION  OF  YOUBI
                MOVE  SPACE    TO   EDIT-CURSOR  OF  YOUBI
                GO TO  DSP-SEC
         WHEN   "F005"
                MOVE    "END"   TO    END-FLG
                MOVE    "END"   TO    PG-END
                GO TO  DSP-END
         WHEN   "E000"
*エラーチェック
*開始日付
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
*終了日付
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
*範囲指定チェック
              IF   STDATE >   EDDATE
                   MOVE   MSG-ERR2  TO  MSG1
                   MOVE  "R"   TO   EDIT-OPTION  OF  STDATE
                   MOVE  "R"   TO   EDIT-OPTION  OF  EDDATE
                   MOVE  "C"   TO   EDIT-CURSOR  OF  STDATE
                   GO TO  DSP-SEC
              END-IF
*曜日入力チェック
              IF   YOUBI  = SPACE   OR  ZERO
                   MOVE   MSG-ERR7  TO  MSG1
                   MOVE  "R"   TO   EDIT-OPTION  OF  YOUBI
                   MOVE  "C"   TO   EDIT-CURSOR  OF  YOUBI
                   GO TO  DSP-SEC
              END-IF
              IF   YOUBI NOT = 1 AND 2 AND 3 AND 4 AND 5
                   MOVE   MSG-ERR8  TO  MSG1
                   MOVE  "R"   TO   EDIT-OPTION  OF  YOUBI
                   MOVE  "C"   TO   EDIT-CURSOR  OF  YOUBI
                   GO TO  DSP-SEC
              END-IF
         WHEN   OTHER
                MOVE   MSG-ERR1  TO  MSG1
                GO TO  DSP-SEC
     END-EVALUATE.
     MOVE  "M"      TO   EDIT-OPTION  OF  STDATE.
     MOVE  SPACE    TO   EDIT-CURSOR  OF  STDATE.
     MOVE  "M"      TO   EDIT-OPTION  OF  EDDATE.
     MOVE  SPACE    TO   EDIT-CURSOR  OF  EDDATE.
     MOVE  "M"      TO   EDIT-OPTION  OF  YOUBI.
     MOVE  SPACE    TO   EDIT-CURSOR  OF  YOUBI.
***確認部
     MOVE     PMSG02    TO     MSG2.
*画面表示
     MOVE     SPACE     TO     DSP-PROC.
     MOVE    "SCREEN"   TO     DSP-GROUP.
     MOVE     HEN-DATE  TO     SDATE.
     MOVE     HEN-TIME  TO     STIME.
     WRITE    FJH01201.
*画面入力
     MOVE     SPACE     TO     MSG1.
     MOVE     "NE"      TO     DSP-PROC.
     MOVE    "GPKAKU"   TO     DSP-GROUP.
     READ     DSPF.
*アテンション判定
     EVALUATE   DSP-FUNC
         WHEN   "F004"
                MOVE  SYS-DATE TO   STDATE  EDDATE
                MOVE  "M"      TO   EDIT-OPTION  OF  STDATE
                MOVE  SPACE    TO   EDIT-CURSOR  OF  STDATE
                MOVE  "M"      TO   EDIT-OPTION  OF  EDDATE
                MOVE  SPACE    TO   EDIT-CURSOR  OF  EDDATE
                MOVE  "M"      TO   EDIT-OPTION  OF  YOUBI
                MOVE  SPACE    TO   EDIT-CURSOR  OF  YOUBI
                GO TO  DSP-SEC
         WHEN   "F005"
                MOVE    "END"   TO    END-FLG
                MOVE    "END"   TO    PG-END
                GO TO  DSP-END
         WHEN   "F006"
                MOVE  "M"      TO   EDIT-OPTION  OF  STDATE
                MOVE  "C"      TO   EDIT-CURSOR  OF  STDATE
                MOVE  "M"      TO   EDIT-OPTION  OF  EDDATE
                MOVE  SPACE    TO   EDIT-CURSOR  OF  EDDATE
                MOVE  "M"      TO   EDIT-OPTION  OF  YOUBI
                MOVE  SPACE    TO   EDIT-CURSOR  OF  YOUBI
                GO TO  DSP-SEC
         WHEN   "E000"
*               基準日付退避
                MOVE   STDATE    TO    WK-DATE1   WK-DATE3
                MOVE   EDDATE    TO    WK-DATE2
                MOVE   YOUBI     TO    WK-YOUBI
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
*レコード存在チェック
     MOVE     WK-DATE3           TO  NIT-F01.
     READ     JHMNITF
        INVALID KEY
              MOVE    1          TO  INV-FLG
        NOT INVALID
              MOVE    ZERO       TO  INV-FLG
     END-READ.
     IF       WK-YOUBI        =  1 OR 2 OR 3 OR 4 OR 5
              IF       INV-FLG         =       1
                       MOVE  SPACE   TO   NIT-REC
                       INITIALIZE         NIT-REC
                       MOVE  WK-DATE3 TO  NIT-F01
                       MOVE  WK-YOUBI TO  NIT-F09
                       WRITE              NIT-REC
              ELSE
                       REWRITE            NIT-REC
              END-IF
     END-IF.
*終了チェック
     IF       WK-DATE3        =       EDDATE
              MOVE  "END"  TO  END-FLG
              GO TO  MAIN-END
     END-IF.
*閏年チェック
     DIVIDE   WK3-YYYY       BY  4  GIVING    CHK-01
                                    REMAINDER CHK-02.
     IF       CHK-02  =  0
              MOVE    29        TO   WK-MATUBI(2)
     ELSE
              MOVE    28        TO   WK-MATUBI(2)
     END-IF.
*日付カウントアップ
     ADD      1                 TO   WK3-DD    WK-YOUBI.
     IF       WK3-DD         >  WK-MATUBI(WK3-MM)
              ADD     1         TO   WK3-MM
              IF      WK3-MM >  12
                      ADD    1  TO   WK3-YYYY
                      MOVE   1  TO   WK3-MM
              END-IF
              MOVE    1         TO   WK3-DD
     END-IF.
     IF       WK-YOUBI       =  8
              MOVE           1  TO   WK-YOUBI
     END-IF.
 MAIN-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF      JHMNITF.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
