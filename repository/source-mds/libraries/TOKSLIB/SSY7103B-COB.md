# SSY7103B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY7103B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｊ本田オンラインシステム　　　　　*
*    業務名　　　　　　　：　Ｊ本田オンラインシステム　　　　　*
*    モジュール名　　　　：　出荷確定データ作成（削除）        *
*    作成日／更新日　　　：　2005/05/18                        *
*    作成者／更新者　　　：　C FUJIWARA                        *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を出荷確定データより削除する。    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY7103B.
 AUTHOR.                C FUJIWARA.
 DATE-WRITTEN.          05/05/18.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*Ｊ本田出荷確定データ
     SELECT   HDSYUKF   ASSIGN    TO        DA-01-VI-HDSYUKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HDK-F01   HDK-F02
                                            HDK-F04   HDK-F06
                        FILE      STATUS    HDK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    Ｊ本田出荷確定データ
******************************************************************
 FD  HDSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     HDSYUKF   OF        XFDLIB
              JOINING   HDK       PREFIX.
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
     03  HDSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  HDK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7103B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7103B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7103B".
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
                        PROCEDURE   HDSYUKF.
     MOVE      "HDSYUKF "   TO   AB-FILE.
     MOVE      HDK-STATUS   TO   AB-STS.
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
     OPEN     I-O       HDSYUKF.
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
*    Ｊ本田出荷確定ファイルスタート
     MOVE     SPACE          TO   HDK-REC.
     INITIALIZE                   HDK-REC.
     MOVE     PARA-JDATE     TO   HDK-F011.
     MOVE     PARA-JTIME     TO   HDK-F012.
     MOVE     PARA-TORICD    TO   HDK-F013.
     MOVE     SPACE          TO   HDK-F02.
     MOVE     ZERO           TO   HDK-F04.
     MOVE     ZERO           TO   HDK-F06.
     START    HDSYUKF   KEY  >=   HDK-F01   HDK-F02
                                  HDK-F04   HDK-F06
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    Ｊ本田出荷確定データ読込み
     PERFORM HDSYUKF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 HDSYUKF-READ-SEC    SECTION.
*
     READ     HDSYUKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  HDSYUKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  HDK-F011
     AND      PARA-JTIME  =  HDK-F012
     AND      PARA-TORICD =  HDK-F013
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  HDSYUKF-READ-EXIT
     END-IF.
*    送信ＦＬＧのチェック
     IF       HDK-F091  =  "1"
              GO                 TO   HDSYUKF-READ-SEC
     END-IF.
*
*    抽出条件のチェック
*-----------（ 倉庫 ）------------*
     IF       PARA-SOKO   =  SPACE
              CONTINUE
*             GO                 TO   HDSYUKF-READ-EXIT
     ELSE
              IF       PARA-SOKO   NOT =   HDK-F02
                       GO        TO   HDSYUKF-READ-SEC
              END-IF
     END-IF.
*
*-----------（ 納品日 ）----------*
     IF       PARA-NOUDT   =   ZERO
              CONTINUE
     ELSE
              IF       PARA-NOUDT   NOT =   HDK-F07
                       GO        TO   HDSYUKF-READ-SEC
              END-IF
     END-IF.
*
 HDSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    Ｊ本田出荷確定データ削除
     DELETE   HDSYUKF.
     ADD      1                   TO   DEL-CNT.
 MAIN010.
*    Ｊ本田出荷確定データ読込み
     PERFORM HDSYUKF-READ-SEC.
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
     DISPLAY "Ｊ本田 確定DT READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "Ｊ本田 確定DT DELE CNT = " DEL-CNT   UPON CONS.
*
     CLOSE     HDSYUKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
