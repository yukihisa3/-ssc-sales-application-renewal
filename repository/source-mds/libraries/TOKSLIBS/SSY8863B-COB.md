# SSY8863B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY8863B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＤＣＭＪＡＰＡＮ　ＷＥＢＥＤＩ　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　ＷＥＢＥＤＩ　　*
*    モジュール名　　　　：　欠品明細書データ抽出              *
*    作成日／更新日　　　：　2014/08/20                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を読み、欠品分を欠品明細に出力　　*
**履歴**********************************************************
*    2017/03/21  高橋　　ケーヨー対応　　　　　　　　　　　　　*
*    0000/00/00　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY8863B.
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
*ＤＣＭＪＡＰＡＮ欠品明細データ
     SELECT   KPSYUKF   ASSIGN    TO        DA-01-VI-KPSYUKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DJJ-K01   DJJ-K02
                                            DJJ-K03   DJJ-K04
                                            DJJ-K08   DJJ-K05
                                            DJJ-K06   DJJ-K07
                        FILE      STATUS    DJJ-STATUS.
*ＤＣＭＪＡＰＡＮ出荷情報修正
     SELECT   DJSYUKF   ASSIGN    TO        DA-01-VI-DJSYUKL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DJS-K01   DJS-K02
                                            DJS-K03   DJS-K04
                                            DJS-K08   DJS-K05
                                            DJS-K06   DJS-K07
                        FILE      STATUS    DJS-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    ＤＣＭＪＡＰＡＮ欠品明細データ
******************************************************************
 FD  KPSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     KPSYUKF   OF        XFDLIB
              JOINING   DJJ       PREFIX.
******************************************************************
*    ＤＣＭＪＡＰＡＮ出荷確定データ
******************************************************************
 FD  DJSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     DJSYUKF   OF        XFDLIB
              JOINING   DJS       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  OUTPUT-CNT          PIC  9(08)     VALUE  ZERO.
 01  WK-KEPPIN               PIC  9(07)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DJJ-STATUS        PIC  X(02).
     03  DJS-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY8863B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8863B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8863B".
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
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUDT.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KPSYUKF.
     MOVE      "KPSYUKL3 "   TO   AB-FILE.
     MOVE      DJJ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DJSYUKF.
     MOVE      "DJSYUKL3 "   TO   AB-FILE.
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
     OPEN     I-O       KPSYUKF.
     OPEN     INPUT     DJSYUKF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
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
*    ＤＣＭＪＡＰＡＮ出荷確定データ
     MOVE     SPACE          TO   DJS-REC.
     INITIALIZE                   DJS-REC.
     MOVE     PARA-JDATE     TO   DJS-K01.
     MOVE     PARA-JTIME     TO   DJS-K02.
     MOVE     PARA-TORICD    TO   DJS-K03.
     MOVE     PARA-SOKO      TO   DJS-K04.
     MOVE     PARA-NOUDT     TO   DJS-K08.
     MOVE     ZERO           TO   DJS-K05.
     MOVE     ZERO           TO   DJS-K06.
     MOVE     ZERO           TO   DJS-K07.
     START    DJSYUKF   KEY  >=   DJS-K01   DJS-K02
                                  DJS-K03   DJS-K04
                                  DJS-K08   DJS-K05
                                  DJS-K06   DJS-K07
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    ＤＣＭＪＡＰＡＮ出荷確定データ読込み
     PERFORM DJSYUKF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　ＤＣＭＪＡＰＡＮ出荷確定データ読込
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
*    バッチ番号のチェック
     IF       PARA-JDATE  =  DJS-K01
     AND      PARA-JTIME  =  DJS-K02
     AND      PARA-TORICD =  DJS-K03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  DJSYUKF-READ-EXIT
     END-IF.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  DJS-K03
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  DJSYUKF-READ-EXIT
              END-IF
     END-IF.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  DJS-K04
                   CONTINUE
              ELSE
                   GO            TO  DJSYUKF-READ-SEC
              END-IF
     END-IF.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  DJS-K08
                  CONTINUE
              ELSE
                  GO        TO   DJSYUKF-READ-SEC
              END-IF
     END-IF.
*    欠品のデータが対象
     COMPUTE  WK-KEPPIN  =  ( DJS-M11 / 100 ) - DJS-A36.
     IF       WK-KEPPIN  >  ZERO
              CONTINUE
     ELSE
              GO            TO  DJSYUKF-READ-SEC
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
*    欠品明細データ出力
     MOVE     SPACE               TO   DJJ-REC.
     INITIALIZE                        DJJ-REC.
     MOVE     DJS-REC             TO   DJJ-REC.
     IF       DJJ-K03 = 13938  OR  17137  OR  139381  OR  171371
     OR                 1731   OR  1732   OR  7601   OR   7602
              MOVE  WK-KEPPIN     TO   DJJ-M10
     END-IF.
     WRITE    DJJ-REC.
     ADD      1                   TO   OUTPUT-CNT.
 MAIN010.
*    ＤＣＭＪＡＰＡＮ出荷確定データ読込み
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
     DISPLAY "DCMJAPAN SYUKA  READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "DCMJAPAN KEPPIN      CNT = " OUTPUT-CNT   UPON CONS.
*
     CLOSE     KPSYUKF  DJSYUKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
