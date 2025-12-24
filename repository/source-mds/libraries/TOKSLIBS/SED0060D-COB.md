# SED0060D

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SED0060D.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＥＤＩＣシステム構築　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩＣシステム出荷業務　　　　　*
*    モジュール名　　　　：　出荷確定データ削除（ＯＮＬ）　　　*
*    作成日／更新日　　　：　2015/09/16                        *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を削除する。　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SED0060D.
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/09/16.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ＥＤＩＣ出荷ＭＳＧデータ
     SELECT   EDSYUKF   ASSIGN    TO        DA-01-VI-EDSYUKL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SYK-F011  SYK-F012
                                            SYK-F013  SYK-F02
                                            SYK-F04   SYK-F03
                                            SYK-F05   SYK-F06
                        FILE      STATUS    SYK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    ＥＤＩＣ出荷ＭＳＧデータ
******************************************************************
 FD  EDSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     EDSYUKF   OF        XFDLIB
              JOINING   SYK       PREFIX.
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
     03  EDSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  SYK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SED0060D".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0060D".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0060D".
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
                        PROCEDURE   EDSYUKF.
     MOVE      "EDSYUKF "   TO   AB-FILE.
     MOVE      SYK-STATUS   TO   AB-STS.
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
     OPEN     I-O       EDSYUKF.
     DISPLAY  MSG-START UPON CONS.
*
     DISPLAY "PARA-JDATE  = " PARA-JDATE   UPON CONS.
     DISPLAY "PARA-JTIME  = " PARA-JTIME   UPON CONS.
     DISPLAY "PARA-TORICD = " PARA-TORICD  UPON CONS.
     DISPLAY "PARA-SOKO   = " PARA-SOKO    UPON CONS.
     DISPLAY "PARA-NOUDT  = " PARA-NOUDT   UPON CONS.
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
*    ＥＤＩＣ出荷確定ファイルスタート
     MOVE     SPACE          TO   SYK-REC.
     INITIALIZE                   SYK-REC.
     MOVE     PARA-JDATE     TO   SYK-F011.
     MOVE     PARA-JTIME     TO   SYK-F012.
     MOVE     PARA-TORICD    TO   SYK-F013.
     MOVE     SPACE          TO   SYK-F02.
     MOVE     ZERO           TO   SYK-F04.
     MOVE     ZERO           TO   SYK-F03.
     MOVE     ZERO           TO   SYK-F05.
     MOVE     ZERO           TO   SYK-F06.
     START    EDSYUKF   KEY  >=   SYK-F011  SYK-F012  SYK-F013
                                  SYK-F02   SYK-F04   SYK-F03
                                  SYK-F05   SYK-F06
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    ＥＤＩＣ出荷ＭＳＧデータ読込み
     PERFORM EDSYUKF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDSYUKF-READ-SEC    SECTION.
*
     READ     EDSYUKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  EDSYUKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
 EDSYUKF-010.
     IF       PARA-JDATE  =  SYK-F011
     AND      PARA-JTIME  =  SYK-F012
     AND      PARA-TORICD =  SYK-F013
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  EDSYUKF-READ-EXIT
     END-IF.
 EDSYUKF-020.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  SYK-F013
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  EDSYUKF-READ-EXIT
              END-IF
     END-IF.
 EDSYUKF-030.
*    送信ＦＬＧのチェック
     IF       SYK-F801  =  "1"
              GO                 TO   EDSYUKF-READ-SEC
     END-IF.
 EDSYUKF-040.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  SYK-F02
                   CONTINUE
              ELSE
                   GO            TO  EDSYUKF-READ-SEC
              END-IF
     END-IF.
 EDSYUKF-050.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  SYK-F04
                  CONTINUE
              ELSE
                  GO        TO   EDSYUKF-READ-SEC
              END-IF
     END-IF.
*
 EDSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    ＥＤＩＣ出荷ＭＳＧデータ削除
     DELETE   EDSYUKF.
     ADD      1                   TO   DEL-CNT.
 MAIN010.
*    ＥＤＩＣ出荷ＭＳＧデータ読込み
     PERFORM EDSYUKF-READ-SEC.
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
     DISPLAY "EDIC ｶｸﾃｲDT READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "EDIC ｶｸﾃｲDT DELE CNT = " DEL-CNT   UPON CONS.
*
     CLOSE     EDSYUKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
