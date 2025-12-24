# SSY8798B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY8798B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　ＤＣＭＪＡＰＡＮ　ＷＥＢＥＤＩ　　
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　ＷＥＢＥＤＩ　　
*    モジュール名　　　　：　商管データ削除処理　　
*    作成日／更新日　　　：　2007/07/30
*    作成者／更新者　　　：　NAV
*    処理概要　　　　　　：　商管データの売上データからの削除
*                            を行なう。　　　　　　　　　　　　
*    更新履歴            ：
*      2011/10/07 飯田/NAV 基幹サーバ統合
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY8798B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/17.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                       *> 2011/10/07,S  S.I/NAV
                                            DEN-F07   DEN-F112
                                       *> 2011/10/07,E  S.I/NAV
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*ＤＣＭＪＡＰＡＮ出荷情報修正
     SELECT   DJSYUKF   ASSIGN    TO        DA-01-VI-DJSYUKL5
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DJS-K03   DJS-K11
                                            DJS-F09   DJS-A43
                        FILE      STATUS    DJS-STATUS.
*ＤＣＭＪＡＰＡＮ出荷情報ワーク
     SELECT   DJSYUKWK  ASSIGN    TO        DA-01-S-DJSYUKWK
                        FILE      STATUS    DWK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    ＤＣＭＪＡＰＡＮ出荷情報
******************************************************************
 FD  DJSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     DJSYUKF   OF        XFDLIB
              JOINING   DJS       PREFIX.
*
******************************************************************
*    ＤＣＭＪＡＰＡＮ出荷情報ワーク
******************************************************************
 FD  DJSYUKWK
              BLOCK  CONTAINS  1  RECORDS.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DWK       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  CRT-CNT             PIC  9(08)     VALUE  ZERO.
     03  DEL-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  DJSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-END-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  DJS-STATUS        PIC  X(02).
     03  DWK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY8798B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8798B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8798B".
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
*納品予定数量
 01  WK-NOUHIN.
     03  FILLER            PIC  X(08)   VALUE  SPACE.
     03  NOU-F01           PIC  9(07)   VALUE  ZERO.
     03  FILLER            PIC  X(08)   VALUE  SPACE.
*****03  NOU-F02           PIC  9(09)   VALUE  ZERO.
     03  NOU-F02           PIC  9(07)   VALUE  ZERO.
     03  FILLER            PIC  X(11)   VALUE  SPACE.
*納品予定原価金額
 01  WK-GENKA.
     03  GEN-F01           PIC  9(09)   VALUE  ZERO.
     03  FILLER            PIC  X(02)   VALUE  SPACE.
*納品予定原価金額
 01  WK-BAIKA              PIC  9(09)   VALUE  ZERO.
*納品予定日項目セット
 01  WK-A05.
     03  WK-A051           PIC  X(04)   VALUE  SPACE.
     03  WK-A052           PIC  9(08)   VALUE  ZERO.
     03  WK-A053           PIC  9(08)   VALUE  ZERO.
     03  WK-A054           PIC  X(24)   VALUE  ZERO.
     03  WK-A055           PIC  X(09)   VALUE  ZERO.
     03  WK-A056           PIC  X(230)  VALUE  ZERO.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENF.
     MOVE      "SHTDENL1"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DJSYUKWK.
     MOVE      "DJSYUKWK"    TO   AB-FILE.
     MOVE      DWK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DJSYUKF.
     MOVE      "DJSYUKL5 "   TO   AB-FILE.
     MOVE      DJS-STATUS   TO   AB-STS.
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
     OPEN     I-O       DJSYUKF  SHTDENF.
     OPEN     EXTEND    DJSYUKWK.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
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
*    ＤＣＭＪＡＰＡＮ出荷情報データスタート
     MOVE     SPACE          TO   DJS-REC.
     INITIALIZE                   DJS-REC.
     MOVE     13938          TO   DJS-K03.
     MOVE     "1"            TO   DJS-K11.
     MOVE     52             TO   DJS-F09.
     MOVE     "000000"       TO   DJS-A43.
     START    DJSYUKF   KEY  >=   DJS-K03   DJS-K11
                                  DJS-F09   DJS-A43
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    ＤＣＭＪＡＰＡＮ出荷情報データ読込み
     PERFORM DJSYUKF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 DJSYUKF-READ-SEC    SECTION.
*
     READ     DJSYUKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  DJSYUKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    取引先ＣＤチェック
     IF       DJS-K03  NOT =  13938
        DISPLAY "AAA" UPON CONS
              MOVE     "END"        TO  END-FLG
              GO                    TO  DJSYUKF-READ-EXIT
     END-IF.
*    納品予定送信区分
     IF       DJS-K11  NOT =  "1"
        DISPLAY "BBB" UPON CONS
              MOVE     "END"        TO  END-FLG
              GO                    TO  DJSYUKF-READ-EXIT
     END-IF.
*    発注種別区分
     IF       DJS-F09  NOT =  52
        DISPLAY "CCC" UPON CONS
              MOVE     "END"        TO  END-FLG
              GO                    TO  DJSYUKF-READ-EXIT
     END-IF.
*    削除区分
     IF       DJS-A43  NOT =  "000000"
        DISPLAY "DDD" UPON CONS
              MOVE     "END"        TO  END-FLG
              GO                    TO  DJSYUKF-READ-EXIT
     END-IF.
*
 DJSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    売上伝票ファイル読込処理
     PERFORM  SHTDENF-DELETE-SEC.
*    ＤＣＭＪＡＰＡＮ発注情報保存データ読込み
     PERFORM DJSYUKF-READ-SEC.
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
     DISPLAY "DCMJAPAN F      READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ｳﾘｱｹﾞDT ﾜｰｸ          CNT = " CRT-CNT   UPON CONS.
     DISPLAY "ｳﾘｱｹﾞDELETE          CNT = " DEL-CNT   UPON CONS.
*
     CLOSE     SHTDENF  DJSYUKF  DJSYUKWK.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上データ読込処理
****************************************************************
 SHTDENF-READ-SEC   SECTION.
*
     MOVE     "SHTDENF-READ-SEC"  TO   S-NAME.
*    売上伝票ファイル検索
     MOVE     DJS-K03             TO   DEN-F01.
     MOVE     DJS-K06             TO   DEN-F02.
     MOVE     0                   TO   DEN-F04.
     MOVE     "40"                TO   DEN-F051.
     MOVE     DJS-K07             TO   DEN-F03.
* 2011/10/07,S  S.I/NAV
     MOVE  DJS-K05          TO  DEN-F07.  *> 店舗CD
     MOVE  DJS-K08          TO  DEN-F112. *> 納入日
* 2011/10/07,E  S.I/NAV
     READ     SHTDENF  INVALID
              MOVE    "INV"       TO   SHTDENF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   SHTDENF-INV-FLG
     END-READ.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上データ削除処理
****************************************************************
 SHTDENF-DELETE-SEC SECTION.
*
     MOVE     "SHTDENF-DELETE-SEC" TO   S-NAME.
*    売上伝票ファイル検索
     PERFORM  SHTDENF-READ-SEC.
*
     IF       SHTDENF-INV-FLG = "INV"
              GO                  TO   SHTDENF-DELETE-EXIT
     END-IF.
*
     MOVE     SPACE               TO   DWK-REC.
     INITIALIZE                        DWK-REC.
     MOVE     DEN-REC             TO   DWK-REC.
     WRITE    DWK-REC.
     ADD      1                   TO   CRT-CNT.
     DELETE   SHTDENF.
     ADD      1                   TO   DEL-CNT.
     MOVE     "999999"            TO   DJS-A43.
     REWRITE   DJS-REC.
*
 SHTDENF-DELETE-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
