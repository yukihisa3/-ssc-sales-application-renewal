# STE7399B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/STE7399B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ダイユーエイト　ＷｅｂＥＤＩ　　　*
*    業務名　　　　　　　：　ダイユーエイト　ＷｅｂＥＤＩ      *
*    モジュール名　　　　：　手書納品予定データＦＬＧ解除　　　*
*    作成日／更新日　　　：　2010/08/19                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を削除する。　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            STE7399B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/08/19.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*リック　　　　　出荷確定データ
     SELECT   DYSYUKF   ASSIGN    TO        DA-01-VI-DYSYUKL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       RCS-F01   RCS-F02
                                            RCS-F03   RCS-F04
                                            RCS-F08   RCS-F06
                                            RCS-F07
                        FILE      STATUS    RCS-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    リック　　　　　出荷確定データ
******************************************************************
 FD  DYSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     DYSYUKF   OF        XFDLIB
              JOINING   RCS       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  DEL-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  DYSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  RCS-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "STE7399B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STE7399B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STE7399B".
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
 01  PARA-DENNOS            PIC   9(09).
 01  PARA-DENNOE            PIC   9(09).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-DENNOS
                                       PARA-DENNOE
                                       PARA-SOKO
                                       PARA-NOUDT.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DYSYUKF.
     MOVE      "DYSYUKF "   TO   AB-FILE.
     MOVE      RCS-STATUS   TO   AB-STS.
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
     OPEN     I-O       DYSYUKF.
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
*    リック　　　　　出荷確定ファイルスタート
     MOVE     SPACE          TO   RCS-REC.
     INITIALIZE                   RCS-REC.
     MOVE     PARA-JDATE     TO   RCS-F01.
     MOVE     PARA-JTIME     TO   RCS-F02.
     MOVE     PARA-TORICD    TO   RCS-F03.
     MOVE     PARA-SOKO      TO   RCS-F04.
     MOVE     PARA-DENNOS    TO   RCS-F06.
     MOVE     ZERO           TO   RCS-F07.
     MOVE     PARA-NOUDT     TO   RCS-F08.
     START    DYSYUKF   KEY  >=   RCS-F01   RCS-F02
                                  RCS-F03   RCS-F04
                                  RCS-F08   RCS-F06
                                  RCS-F07
         INVALID   KEY
              DISPLAY NC"＃対象データがありません＃" UPON CONS
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    リック　　　　　出荷確定データ読込み
     PERFORM DYSYUKF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 DYSYUKF-READ-SEC    SECTION.
*
     READ     DYSYUKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  DYSYUKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       99999999    =  RCS-F01
     AND      9999        =  RCS-F02
     AND      PARA-TORICD =  RCS-F03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  DYSYUKF-READ-EXIT
     END-IF.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  RCS-F03
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  DYSYUKF-READ-SEC
              END-IF
     END-IF.
*    伝票番号のチェック
     IF       PARA-DENNOS  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-DENNOS  <=  RCS-F06    AND
                   RCS-F06      <=  PARA-DENNOE
                   CONTINUE
              ELSE
                   GO                    TO  DYSYUKF-READ-SEC
              END-IF
     END-IF.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  RCS-F04
                   CONTINUE
              ELSE
                   GO            TO  DYSYUKF-READ-SEC
              END-IF
     END-IF.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  RCS-F08
                  CONTINUE
              ELSE
                  GO        TO   DYSYUKF-READ-SEC
              END-IF
     END-IF.
*
 DYSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    リックＦＬＧ解除
     MOVE    "0"                  TO   RCS-F11.
     MOVE    ZERO                 TO   RCS-F12.
     REWRITE RCS-REC.
     ADD      1                   TO   DEL-CNT.
 MAIN010.
*    リック　　　　　出荷確定データ読込み
     PERFORM DYSYUKF-READ-SEC.
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
     DISPLAY "ﾀﾞｲﾕｰｴｲﾄ ｶｸﾃｲDT READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ﾀﾞｲﾕｰｴｲﾄ ｶｸﾃｲDT DEL  CNT = " DEL-CNT   UPON CONS.
*
     CLOSE     DYSYUKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
