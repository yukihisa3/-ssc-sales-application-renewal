# SSY5761B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY5761B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　グッデイＥＤＩ　　　　　　　　　　*
*    業務名　　　　　　　：　出荷業務　　　　　　　　　　　　　*
*    モジュール名　　　　：　未送信出荷情報初期化　　　　　　　*
*    作成日／更新日　　　：　2014/06/16                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより　　　　　  *
*                            未送信状態を解除する。　　　　　　*
*                            　基本情報：ＦＬＧ解除　　　　　　*
*                            　出荷情報：レコード削除　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY5761B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2014/06/16.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*グッデイ　　　　　     基本情報ファイル
     SELECT   GDJOHOL2  ASSIGN    TO        DA-01-VI-GDJOHOL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JOH-F01   JOH-F02
                                            JOH-F03   JOH-F04
                                            JOH-F08   JOH-F05
                                            JOH-F06   JOH-F07
                        FILE      STATUS    JOH-STATUS.
*********
*グッデイ　　　　　     出荷情報ファイル
     SELECT   GDSYUKL2  ASSIGN    TO        DA-01-VI-GDSYUKL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SYU-F01   SYU-F02
                                            SYU-F03   SYU-F04
                                            SYU-F08   SYU-F05
                                            SYU-F06   SYU-F07
                        FILE      STATUS    SYU-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    グッデイ　　　　　基本情報ファイル
******************************************************************
 FD  GDJOHOL2            LABEL RECORD   IS   STANDARD.
     COPY     GDJOHOL2   OF        XFDLIB
              JOINING    JOH       PREFIX.
*
******************************************************************
*    グッデイ　　　　　出荷情報ファイル
******************************************************************
 FD  GDSYUKL2            LABEL RECORD   IS   STANDARD.
     COPY     GDSYUKL2   OF        XFDLIB
              JOINING    SYU       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.

*STATUS
 01  WK-ST.
     03  JOH-STATUS        PIC  X(02).
     03  SYU-STATUS        PIC  X(02).
*
*FLG
 01  END-FLG-JOH             PIC  X(03)     VALUE  SPACE.
 01  END-FLG-SYU             PIC  X(03)     VALUE  SPACE.
 01  WK-INV-FLG.
     03  GDJOHOL2-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  GDSYUKL2-INV-FLG    PIC  X(03)     VALUE  SPACE.
*
*COUNT
 01  WK-CNT.
     03  READ-CNT-JOH        PIC  9(08)     VALUE  ZERO.
     03  READ-CNT-SYU        PIC  9(08)     VALUE  ZERO.
     03  UPD-CNT-JOH         PIC  9(08)     VALUE  ZERO.
     03  DEL-CNT-SYU         PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY5761B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY5761B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY5761B".
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
                        PROCEDURE   GDJOHOL2.
     MOVE      "GDJOHOL2 "  TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   GDSYUKL2.
     MOVE      "GDSYUKL2 "  TO   AB-FILE.
     MOVE      SYU-STATUS   TO   AB-STS.
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
     PERFORM  MAIN-SEC-JOH
              UNTIL     END-FLG-JOH   =  "END".
     PERFORM  MAIN-SEC-SYU
              UNTIL     END-FLG-SYU   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       GDJOHOL2  GDSYUKL2.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG-JOH END-FLG-SYU  WK-CNT.
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
*    グッデイ　　　　　基本情報ファイルスタート
     MOVE     SPACE          TO   JOH-REC.
     INITIALIZE                   JOH-REC.
     MOVE     PARA-JDATE     TO   JOH-F01.
     MOVE     PARA-JTIME     TO   JOH-F02.
     MOVE     PARA-TORICD    TO   JOH-F03.
     MOVE     SPACE          TO   JOH-F04.
     MOVE     ZERO           TO   JOH-F08.
     MOVE     ZERO           TO   JOH-F05.
     MOVE     ZERO           TO   JOH-F06.
     MOVE     ZERO           TO   JOH-F07.
     START    GDJOHOL2   KEY  >=  JOH-F01   JOH-F02
                                  JOH-F03   JOH-F04
                                  JOH-F08   JOH-F05
                                  JOH-F06   JOH-F07
              INVALID   KEY
                   MOVE    "END"  TO   END-FLG-JOH
     END-START.
*    グッデイ　　　　　基本情報ファイル読込み
     PERFORM GDJOHOL2-READ-SEC.
*
*    グッデイ　　　　　出荷確定ファイルスタート
     MOVE     SPACE          TO   SYU-REC.
     INITIALIZE                   SYU-REC.
     MOVE     PARA-JDATE     TO   SYU-F01.
     MOVE     PARA-JTIME     TO   SYU-F02.
     MOVE     PARA-TORICD    TO   SYU-F03.
     MOVE     SPACE          TO   SYU-F04.
     MOVE     ZERO           TO   SYU-F08.
     MOVE     ZERO           TO   SYU-F05.
     MOVE     ZERO           TO   SYU-F06.
     MOVE     ZERO           TO   SYU-F07.
     START    GDSYUKL2   KEY  >=  SYU-F01   SYU-F02
                                  SYU-F03   SYU-F04
                                  SYU-F08   SYU-F05
                                  SYU-F06   SYU-F07
              INVALID   KEY
                   MOVE    "END"  TO   END-FLG-SYU
     END-START.
*    グッデイ　　　　　出荷情報ファイル読込み
     PERFORM GDSYUKL2-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　基本情報ファイル読込み　　　　　　　　　　　　　*
****************************************************************
 GDJOHOL2-READ-SEC    SECTION.
*
     READ     GDJOHOL2
              AT  END
                  MOVE     "END"    TO  END-FLG-JOH
                  GO                TO  GDJOHOL2-READ-EXIT
*             NOT AT END
*                 ADD       1       TO  READ-CNT-JOH
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  JOH-F01
     AND      PARA-JTIME  =  JOH-F02
     AND      PARA-TORICD =  JOH-F03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG-JOH
              GO                    TO  GDJOHOL2-READ-EXIT
     END-IF.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  JOH-F03
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG-JOH
                   GO                    TO  GDJOHOL2-READ-EXIT
              END-IF
     END-IF.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  JOH-F04
                   CONTINUE
              ELSE
                   GO            TO  GDJOHOL2-READ-SEC
              END-IF
     END-IF.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  JOH-F08
                  CONTINUE
              ELSE
                  GO        TO   GDJOHOL2-READ-SEC
              END-IF
     END-IF.
*
     ADD      1             TO   READ-CNT-JOH.
*
*    送信ＦＬＧのチェック
     IF       JOH-F12  =  "1"
              GO            TO   GDJOHOL2-READ-SEC
     END-IF.
*
 GDJOHOL2-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　基本情報　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC-JOH     SECTION.
*
*   グッデイ　　　　　基本情報ファイルＦＬＧ解除
     MOVE    "MAIN-SEC-JOH"       TO   S-NAME.
*
*   出荷確定データ作成ＦＬＧ
     MOVE    "0"                  TO   JOH-F10.
*   出荷確定データ作成日
     MOVE    ZERO                 TO   JOH-F11.
     REWRITE JOH-REC.
     ADD      1                   TO   UPD-CNT-JOH.
 MAIN010.
*    グッデイ　　　　　基本情報ファイル読込み
     PERFORM GDJOHOL2-READ-SEC.
*
 MAIN-EXIT-JOH.
     EXIT.
****************************************************************
*　　　　　　　出荷情報ファイル読込み　　　　　　　　　　　　　*
****************************************************************
 GDSYUKL2-READ-SEC    SECTION.
*
     READ     GDSYUKL2
              AT  END
                  MOVE     "END"    TO  END-FLG-SYU
                  GO                TO  GDSYUKL2-READ-EXIT
*             NOT AT END
*                 ADD       1       TO  READ-CNT-SYU
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  SYU-F01
     AND      PARA-JTIME  =  SYU-F02
     AND      PARA-TORICD =  SYU-F03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG-SYU
              GO                    TO  GDSYUKL2-READ-EXIT
     END-IF.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  SYU-F03
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG-SYU
                   GO                    TO  GDSYUKL2-READ-EXIT
              END-IF
     END-IF.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  SYU-F04
                   CONTINUE
              ELSE
                   GO            TO  GDSYUKL2-READ-SEC
              END-IF
     END-IF.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  SYU-F08
                  CONTINUE
              ELSE
                  GO        TO   GDSYUKL2-READ-SEC
              END-IF
     END-IF.
*
     ADD      1             TO   READ-CNT-SYU.
*
*    送信ＦＬＧのチェック
     IF       SYU-F12  =  "1"
              GO            TO   GDSYUKL2-READ-SEC
     END-IF.
*
 GDSYUKL2-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　出荷情報　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC-SYU     SECTION.
*
*   グッデイ　　　　　出荷情報ファイル削除
     MOVE    "MAIN-SEC-SYU"       TO   S-NAME.
*
     DELETE   GDSYUKL2.
     ADD      1                   TO   DEL-CNT-SYU.
 MAIN010.
*    グッデイ　　　　　出荷情報ファイル読込み
     PERFORM GDSYUKL2-READ-SEC.
*
 MAIN-EXIT-SYU.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY  NC"基本情報ファイル　読込＝"
              READ-CNT-JOH NC"件" UPON CONS.
     DISPLAY  NC"　　　　　　　　　解除＝"
              UPD-CNT-JOH  NC"件" UPON CONS.
     DISPLAY  NC"出荷情報ファイル　読込＝"
              READ-CNT-SYU NC"件" UPON CONS.
     DISPLAY  NC"　　　　　　　　　削除＝"
              DEL-CNT-SYU  NC"件" UPON CONS.
*
     CLOSE    GDJOHOL2  GDSYUKL2.
*
     DISPLAY  MSG-END             UPON CONS.
*
     STOP     RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
