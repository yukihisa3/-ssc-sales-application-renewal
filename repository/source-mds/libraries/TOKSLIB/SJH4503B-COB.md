# SJH4503B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJH4503B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　変換伝票データ作成（支払データ）　*
*    作成日／更新日　　　：　2002/11/22                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　オンラインデータより支払データの　*
*                            編集を行なう。　　　　　　　　　　*
*                            イオン（大阪）                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJH4503B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          02/11/22.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*支払明細データ
     SELECT   IONSHI    ASSIGN    TO        DA-01-S-IONSHI
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    ION-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*支払伝票データ
     SELECT   SITGKFL   ASSIGN    TO        DA-01-S-SITGKFL
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    SIT-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    分割データ　ＲＬ＝　１２８　ＢＦ＝　１
******************************************************************
 FD  IONSHI
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
     COPY     IONSHI    OF        XFDLIB
              JOINING   ION       PREFIX.
*
******************************************************************
*    支払伝票ＤＴＲＬ＝　１２８　ＢＦ＝　８１
******************************************************************
 FD  SITGKFL
                        BLOCK CONTAINS      81   RECORDS
                        LABEL RECORD   IS   STANDARD.
*
     COPY     SITGKFL   OF        XFDLIB
              JOINING   SIT       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IX                      PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WT-CNT                  PIC  9(08)     VALUE  ZERO.
 01  CHK-FLG                 PIC  X(03)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  ION-STATUS        PIC  X(02).
     03  SIT-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJH4503B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH4503B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH4503B".
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
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   IONSHI.
     MOVE      "IONSHI  "   TO   AB-FILE.
     MOVE      ION-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SITGKFL.
     MOVE      "SITGKFL "   TO   AB-FILE.
     MOVE      SIT-STATUS   TO   AB-STS.
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
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
     STOP     RUN.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     IONSHI.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WT-CNT.
*
     PERFORM   IONSHI-READ-SEC.
     IF  END-FG  =  9
         MOVE     "CHK"     TO    CHK-FLG
     ELSE
         MOVE     SPACE     TO    CHK-FLG
         OPEN     OUTPUT    SITGKFL
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IONSHI-READ-SEC     SECTION.
*
     MOVE    "IONSHI-READ-SEC"   TO   S-NAME.
*
     READ     IONSHI
              AT END
              MOVE      9                   TO  END-FG
              NOT AT END
              ADD       1                   TO  RD-CNT
     END-READ.
*
 IONSHI-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 3
         IF  ION-F074(IX)  NOT =  SPACE
             MOVE  SPACE        TO  SIT-REC
             INITIALIZE             SIT-REC
             MOVE  ION-F072(IX) TO  SIT-F01
             MOVE  ION-F074(IX) TO  SIT-F02
             MOVE  ION-F076(IX) TO  SIT-F03
             MOVE  ION-F078(IX) TO  SIT-F04
             MOVE  ION-F07A(IX) TO  SIT-F05
             MOVE  ION-F07C(IX) TO  SIT-F06
             MOVE  ION-F07D(IX) TO  SIT-F07
             MOVE  ION-F07E(IX) TO  SIT-F08
             WRITE SIT-REC
             ADD   1            TO  WT-CNT
         END-IF
     END-PERFORM.
*
     PERFORM  IONSHI-READ-SEC.
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
     CLOSE     IONSHI.
*
     IF  CHK-FLG  =  SPACE
         CLOSE       SITGKFL
     ELSE
         DISPLAY "READ-CNT  = "  RD-CNT  UPON CONS
         DISPLAY "WRITE-CNT = "  WT-CNT  UPON CONS
     END-IF.
*
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
