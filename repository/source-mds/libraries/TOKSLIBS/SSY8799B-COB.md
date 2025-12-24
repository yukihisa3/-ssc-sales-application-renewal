# SSY8799B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY8799B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＤＣＭＪＡＰＡＮ　ＷＥＢＥＤＩ　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　ＷＥＢＥＤＩ　　*
*    モジュール名　　　　：　基本情報データ変更　　　　　　　　*
*    作成日／更新日　　　：　2007/07/30                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　基本データの倉庫ＣＤをＢＫ後削除し*
*                            倉庫ＣＤを変更し、データを戻す。　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY8799B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/07/30.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ＤＣＭＪＡＰＡＮ出荷確定データ
     SELECT   DJJOHOF   ASSIGN    TO        DA-01-VI-DJJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DJS-K01   DJS-K02
                                            DJS-K03   DJS-K04
                                            DJS-K05   DJS-K06
                                            DJS-K07   DJS-K08
                        FILE      STATUS    DJS-STATUS.
*ＤＣＭＪＡＰＡＮ出荷確定データワーク
     SELECT   DJJOHOWK  ASSIGN    TO        DA-01-S-DJJOHOWK
                        FILE      STATUS    DWK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    ＤＣＭＪＡＰＡＮ出荷確定データ
******************************************************************
 FD  DJJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     DJJOHOF   OF        XFDLIB
              JOINING   DJS       PREFIX.
******************************************************************
*    ＤＣＭＪＡＰＡＮ出荷確定データ
******************************************************************
 FD  DJJOHOWK
              BLOCK CONTAINS  1   RECORDS.
     COPY     DJJOHOF   OF        XFDLIB
              JOINING   DWK       PREFIX.
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
     03  DJJOHOF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DJS-STATUS        PIC  X(02).
     03  DWK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY8799B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8799B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8799B".
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
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DJJOHOF.
     MOVE      "DJJOHOF "   TO   AB-FILE.
     MOVE      DJS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DJJOHOWK.
     MOVE      "DJJOHOWK"   TO   AB-FILE.
     MOVE      DWK-STATUS   TO   AB-STS.
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
     OPEN     I-O       DJJOHOF.
     OPEN     OUTPUT    DJJOHOWK.
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
*    ＤＣＭＪＡＰＡＮ出荷確定ファイルスタート
     MOVE     SPACE          TO   DJS-REC.
     INITIALIZE                   DJS-REC.
     MOVE     PARA-JDATE     TO   DJS-K01.
     MOVE     PARA-JTIME     TO   DJS-K02.
     MOVE     PARA-TORICD    TO   DJS-K03.
     MOVE     SPACE          TO   DJS-K04.
     MOVE     ZERO           TO   DJS-K05.
     MOVE     ZERO           TO   DJS-K06.
     MOVE     ZERO           TO   DJS-K07.
     MOVE     ZERO           TO   DJS-K08.
     START    DJJOHOF   KEY  >=   DJS-K01   DJS-K02   DJS-K03
                                  DJS-K04   DJS-K05   DJS-K06
                                  DJS-K07   DJS-K08
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    ＤＣＭＪＡＰＡＮ出荷確定データ読込み
     PERFORM DJJOHOF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 DJJOHOF-READ-SEC    SECTION.
*
     READ     DJJOHOF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  DJJOHOF-READ-EXIT
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
              GO                    TO  DJJOHOF-READ-EXIT
     END-IF.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  DJS-K03
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  DJJOHOF-READ-EXIT
              END-IF
     END-IF.
*
 DJJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    ＤＣＭＪＡＰＡＮ基本情報データコピー
     MOVE     SPACE               TO   DWK-REC.
     INITIALIZE                        DWK-REC.
     MOVE     DJS-REC             TO   DWK-REC.
     MOVE     "6A"                TO   DWK-K04.
     WRITE    DWK-REC.
*    ＤＣＭＪＡＰＡＮ出荷確定データ削除
     DELETE   DJJOHOF.
     ADD      1                   TO   DEL-CNT.
 MAIN010.
*    ＤＣＭＪＡＰＡＮ出荷確定データ読込み
     PERFORM DJJOHOF-READ-SEC.
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
     DISPLAY "DCMJAPAN ｶｸﾃｲDT READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "DCMJAPAN ｶｸﾃｲDT DELE CNT = " DEL-CNT   UPON CONS.
*
     CLOSE     DJJOHOF  DJJOHOWK.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
