# SSY5762B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY5762B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　：　グッデイＥＤＩ
*    業務名　　　　　　：　出荷業務
*    モジュール名　　　：　出荷確定データ抽出
*    作成日／更新日　　：　2014/06/16
*    作成者／更新者　　：　NAV
*    処理概要　　　　　：　受け取ったパラメタと合致するデータを
*    　　　　　　　　　　　基本情報ファイルより、
*                          出荷情報ファイルへ抽出する。
*    更新履歴          ：
*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY5762B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/06/16.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*グッデイ　基本情報ファイル
     SELECT   GDJOHOL2  ASSIGN    TO        DA-01-VI-GDJOHOL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JOH-F01   JOH-F02
                                            JOH-F03   JOH-F04
                                            JOH-F08   JOH-F05
                                            JOH-F06   JOH-F07
                        FILE      STATUS    JOH-STATUS.
*売上伝票データ
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*グッデイ　出荷情報ファイル
     SELECT   GDSYUKL1  ASSIGN    TO        DA-01-VI-GDSYUKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SYU-F01   SYU-F02
                                            SYU-F03   SYU-F04
                                            SYU-F05   SYU-F06
                                            SYU-F07   SYU-F08
                        FILE      STATUS    SYU-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    グッデイ　　　　　基本情報ファイル
******************************************************************
 FD  GDJOHOL2            LABEL RECORD   IS   STANDARD.
     COPY     GDJOHOL2   OF       XFDLIB
              JOINING    JOH       PREFIX.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENF            LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    グッデイ　　　　　出荷情報ファイル
******************************************************************
 FD  GDSYUKL1           LABEL RECORD   IS   STANDARD.
     COPY     GDSYUKL1  OF        XFDLIB
              JOINING   SYU       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP1-CNT           PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  KOS1-CNT            PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  GDSYUKL1-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  SYU-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY5762B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY5762B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY5762B".
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
*****03  FILLER            PIC  X(02)   VALUE  SPACE.
*納品予定原価金額
 01  WK-BAIKA              PIC  9(09)   VALUE  ZERO.
*納品予定日項目セット
 01  WK-C25.
     03  WK-C251           PIC  9(05)   VALUE  ZERO.
     03  WK-C252           PIC  X(02)   VALUE  SPACE.
     03  WK-C253           PIC  X(02)   VALUE  SPACE.
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
 01  PARA-KENSUU            PIC   9(07).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUDT
                                       PARA-KENSUU.
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
                        PROCEDURE   GDJOHOL2.
     MOVE      "GDJOHOL2 "  TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   GDSYUKL1.
     MOVE      "SYUYUKL1 "  TO   AB-FILE.
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
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       GDJOHOL2  SHTDENF.
     OPEN     I-O       GDSYUKL1.
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
*    グッデイ　　　　　基本情報ファイルスタート
     MOVE     SPACE          TO   JOH-REC.
     INITIALIZE                   JOH-REC.
     MOVE     PARA-JDATE     TO   JOH-F01.
     MOVE     PARA-JTIME     TO   JOH-F02.
     MOVE     PARA-TORICD    TO   JOH-F03.
     MOVE     PARA-SOKO      TO   JOH-F04.
     MOVE     PARA-NOUDT     TO   JOH-F08.
     MOVE     ZERO           TO   JOH-F05.
     MOVE     ZERO           TO   JOH-F06.
     MOVE     ZERO           TO   JOH-F07.
     START    GDJOHOL2   KEY  >=  JOH-F01   JOH-F02
                                  JOH-F03   JOH-F04
                                  JOH-F08   JOH-F05
                                  JOH-F06   JOH-F07
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    グッデイ　　　　　基本情報ファイル読込み
     PERFORM GDJOHOL2-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　基本情報　　　　　　　　　　　　　　*
****************************************************************
 GDJOHOL2-READ-SEC    SECTION.
*
     READ     GDJOHOL2
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  GDJOHOL2-READ-EXIT
*             NOT AT END
*                 ADD       1       TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  JOH-F01
     AND      PARA-JTIME  =  JOH-F02
     AND      PARA-TORICD =  JOH-F03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  GDJOHOL2-READ-EXIT
     END-IF.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  JOH-F03
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
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
*    出荷確定データ作成ＦＬＧのチェック
     IF       JOH-F10  =  "1"
              GO                 TO   GDJOHOL2-READ-SEC
     END-IF.
*
     ADD      1                  TO   READ-CNT.
*
 GDJOHOL2-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    売上伝票ファイル検索
     MOVE     JOH-F03             TO   DEN-F01.  *> 取引先
     MOVE     JOH-F06             TO   DEN-F02.  *> 伝票番号
     MOVE     0                   TO   DEN-F04.  *> 相殺区分
     MOVE     "40"                TO   DEN-F051. *> 伝区
     MOVE     JOH-F07             TO   DEN-F03.  *> 行
     MOVE     JOH-F05             TO   DEN-F07.  *> 店舗CD
     MOVE     JOH-F08             TO   DEN-F112. *> 納品日
     READ     SHTDENF    INVALID
              MOVE    "INV"       TO   SHTDENF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   SHTDENF-INV-FLG
     END-READ.
*
     PERFORM  GDSYUKL1-WRITE-SEC.
     MOVE     "1"                 TO   JOH-F10.
     MOVE     SYS-DATEW           TO   JOH-F11.
     REWRITE  JOH-REC.
*
 MAIN010.
*    グッデイ 基本情報ファイル読込み
     PERFORM GDJOHOL2-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　グッデイ　発注確定データ作成処理
****************************************************************
 GDSYUKL1-WRITE-SEC     SECTION.
*
     MOVE     "GDSYUKL1-WRITE-SEC"  TO  S-NAME.
*
     MOVE      SPACE               TO  SYU-REC.
     INITIALIZE                        SYU-REC.
     MOVE      JOH-REC             TO  SYU-REC.
     IF        SHTDENF-INV-FLG     =  "   "
               MOVE    DEN-F15     TO  SYU-F09
     ELSE
               MOVE    JOH-B05     TO  SYU-F09
     END-IF.
     MOVE      "1"                 TO  SYU-F10.
     MOVE      SYS-DATEW           TO  SYU-F11.
*
     WRITE     SYU-REC.
     ADD       1                   TO  KOS1-CNT.
*
 GDSYUKL1-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE     KOS1-CNT   TO      PARA-KENSUU.
*
     DISPLAY  NC"基本情報ファイル　読込＝"  READ-CNT  UPON CONS.
     DISPLAY  NC"出荷情報ファイル　抽出＝"  KOS1-CNT  UPON CONS.
*
     CLOSE    SHTDENF  GDJOHOL2   GDSYUKL1.
*
     DISPLAY  MSG-END             UPON CONS.
*
     STOP     RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
