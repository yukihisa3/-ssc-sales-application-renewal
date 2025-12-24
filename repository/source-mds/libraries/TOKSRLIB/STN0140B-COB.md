# STN0140B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/STN0140B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　_卸ローカル運用　　　　　　　　  *
*    モジュール名　　　　：　_卸確定リストデータ抽出　　　　　*
*    作成日／作成者　　　：　2021/03/18 INOUE                  *
*    処理概要　　　　　　：　条件に従い、　　　　　　　　　　　*
*                            _卸確定リストデータ抽出　　　　　*
*    更新履歴                                                  *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            STN0140B.
*                  流用:STN0100B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/03/18.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< _卸確定データ >>--*
     SELECT   KKTANAF   ASSIGN    TO        DA-01-VI-KKTANAL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KKT-F04
                                            KKT-F05
                                            KKT-F06X
                                            KKT-F15
                        FILE  STATUS   IS   KKT-STATUS.
*----<< _卸確定リストデータ >>----*
     SELECT   KKTANAW  ASSIGN    TO        DA-01-VS-KKTANAW
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    OUT-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    _卸確定データ
******************************************************************
 FD  KKTANAF            LABEL     RECORD   IS   STANDARD.
     COPY     KKTANAF   OF        XFDLIB
              JOINING   KKT       PREFIX.
******************************************************************
*    確定リストデータ
******************************************************************
 FD  KKTANAW           LABEL     RECORD   IS   STANDARD.
     COPY     KKTANAW  OF        XFDLIB
              JOINING  OUT  AS   PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  WRT-CNT             PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  KKTANAW-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  SHTDENLA-INV-FLG    PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
*退避
 01  BK-C128-REC             PIC  X(128)    VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME           PIC   9(08)  VALUE  ZERO.
*日付の編集
 01  WK-HDATE.
     03  WK-HDATE1         PIC 9(02).
     03  WK-HDATE2         PIC 9(06).
 01  WK-NDATE.
     03  WK-NDATE1         PIC 9(02).
     03  WK-NDATE2         PIC 9(06).
*
 01  WK-ST.
     03  KKT-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  IN-STATUS         PIC  X(02).
     03  OUT-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "STN0140B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STN0140B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STN0140B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-SOKCDS            PIC   X(02).
 01  PARA-IN-SOKCDE            PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING
                                  PARA-IN-SOKCDS
                                  PARA-IN-SOKCDE.
*
******************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KKTANAF.
     MOVE      "KKTANAL3"    TO   AB-FILE.
     MOVE      KKT-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KKTANAW.
     MOVE      "KKTANAW "   TO   AB-FILE.
     MOVE      OUT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     KKTANAF.
     OPEN     OUTPUT    KKTANAW.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG SHTDENLA-INV-FLG.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
*    _卸確定データ　スタート
*T
*    DISPLAY  "PARA-IN-SOKCDS =" PARA-IN-SOKCDS UPON CONS.
*    DISPLAY  "PARA-IN-SOKCDE =" PARA-IN-SOKCDE UPON CONS.
*T
     MOVE      SPACE           TO   KKT-REC.
     INITIALIZE                     KKT-REC.
     MOVE      PARA-IN-SOKCDS  TO   KKT-F04.
     START     KKTANAF   KEY   >=   KKT-F04 KKT-F05 KKT-F06X
                                    KKT-F15
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*
*    _卸確定データ読込み
     PERFORM KKTANAF-READ-SEC.
     IF      END-FLG  NOT = "END"
             CONTINUE
     ELSE
             MOVE     4010          TO  PROGRAM-STATUS
             STOP     RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 KKTANAF-READ-SEC    SECTION.
*
     MOVE    "KKTANAF-READ-SEC"    TO  S-NAME.
*
     READ     KKTANAF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KKTANAF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 KKTANAF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"              TO   S-NAME.
*
*対象データチェック
 MAIN011.
     IF ( PARA-IN-SOKCDS    <=   KKT-F04 ) AND
        ( PARA-IN-SOKCDE    >=   KKT-F04 )
        CONTINUE
     ELSE
        MOVE  "END"      TO   END-FLG
        GO               TO   MAIN-EXIT
     END-IF.
*
     MOVE    KKT-REC                 TO  OUT-REC.
     WRITE   OUT-REC.
     ADD     1                       TO  WRT-CNT.
*
 MAIN999.
*    _卸確定データ読込み
     PERFORM KKTANAF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY   NC"_卸確定データ" "IN  = " READ-CNT  UPON CONS.
     DISPLAY   NC"抽出データ　　" "OUT = " WRT-CNT   UPON CONS.
*    DISPLAY   NC"抽出対象外" "SKIP= " SKIP-CNT  UPON CONS.
*
     IF        WRT-CNT  =  ZERO
               MOVE    4010    TO    PROGRAM-STATUS
               STOP    RUN
     END-IF.
*
     CLOSE     KKTANAF  KKTANAW.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
