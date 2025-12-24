# SJH4504B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJH4504B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　変換伝票データ作成（支払データ）　*
*    作成日／更新日　　　：　2002/12/05                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　伝票データの存在チェックを行なう。*
*                            イオン（大阪）                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJH4504B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          02/12/05.
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
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJH4504B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH4504B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH4504B".
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
 LINKAGE                SECTION.
 01  PARA-KEKA              PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING  PARA-KEKA.
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
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
*
     OPEN     INPUT     IONSHI.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WT-CNT.
*
     READ     IONSHI
              AT END
              MOVE      9     TO  END-FG
     END-READ.
     IF  END-FG  =  9
         MOVE     "NG"      TO    PARA-KEKA
     ELSE
         MOVE     "OK"      TO    PARA-KEKA
     END-IF.
*
     CLOSE     IONSHI.
*
 GENERAL-PROCESS-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
