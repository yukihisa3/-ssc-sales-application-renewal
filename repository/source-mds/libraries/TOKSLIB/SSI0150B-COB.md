# SSI0150B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSI0150B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　支払合計ファイル作成　　　　　　　*
*    作成日／更新日　　　：　92/12/01                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　集信ファイルより，支払合計ファイル*
*                        ：　を出力する。　　　　　　　　　　　*
*    作成日／更新日　　　：　08/08/28                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　内部統制対応　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI0150B.
 AUTHOR.                S.K  SANKYO.
 DATE-WRITTEN.          92/12/01.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 集信データ >>--*
     SELECT   CVCSKSHU  ASSIGN         DA-01-S-CVCSKSHU
                        ORGANIZATION   SEQUENTIAL
                        STATUS    CVCSKSHU-ST.
*----<< 支払合計ファイル >>--*
     SELECT   HSHIGKF   ASSIGN         DA-01-S-HSHIGKF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SHI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 集信データ >>--*
 FD  CVCSKSHU           LABEL     RECORD   IS   STANDARD.
 01  ONL-REC.
     03  ONL-RECX       OCCURS    2.
         05  ONL-F.
             07  ONL-F01     PIC  X(01).
             07  ONL-F02     PIC  X(02).
             07  FILLER      PIC  X(42).
             07  ONL-F03     PIC  9(03).
             07  ONL-F04     PIC  9(06).
             07  ONL-F05     PIC  9(05).
             07  ONL-F06     PIC  X(01).
             07  FILLER      PIC  X(68).
         05  ONL-H           REDEFINES ONL-F.
             07  ONL-H01     PIC  X(01).
             07  ONL-H02     PIC  X(02).
             07  ONL-H03     PIC  X(04).
             07  ONL-H04     PIC  X(20).
             07  ONL-H05     PIC  9(06).
             07  ONL-H06     PIC  9(06).
             07  ONL-H07     PIC  X(01).
             07  ONL-H08     PIC  X(20).
             07  ONL-H09     PIC  9(05).
             07  FILLER      PIC  X(63).
         05  FILLER          REDEFINES ONL-F.
             07  ONL-M01     PIC  X(01).
             07  ONL-M02     PIC  X(02).
             07  ONL-M       OCCURS    3.
                 09  ONL-M03      PIC  X(01).
                 09  ONL-M04      PIC  X(05).
                 09  ONL-M05      PIC  9(06).
                 09  ONL-M06      PIC  X(09).
                 09  ONL-M07S     PIC  X(01).
                 09  ONL-M07      PIC  9(09).
                 09  ONL-M08      PIC  X(10).
             07  FILLER      PIC  X(02).
         05  ONL-G           REDEFINES ONL-F.
             07  ONL-G01     PIC  X(01).
             07  ONL-G02     PIC  X(02).
             07  ONL-G03S    PIC  X(01).
             07  ONL-G03     PIC  9(10).
             07  ONL-G04S    PIC  X(01).
             07  ONL-G04     PIC  9(10).
             07  ONL-G05S    PIC  X(01).
             07  ONL-G05     PIC  9(10).
             07  ONL-G06S    PIC  X(01).
             07  ONL-G06     PIC  9(10).
             07  ONL-G07S    PIC  X(01).
             07  ONL-G07     PIC  9(10).
             07  ONL-G08     PIC  9(06).
             07  FILLER      PIC  X(64).
*----<< 支払合計ファイル >>--*
 FD  HSHIGKF            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS 20   RECORDS.
     COPY     SITGKFA   OF        XFDLIB
              JOINING   SHI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  INP-X1         PIC  X(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  FH-CNT         PIC  9(06).
     03  MH-CNT         PIC  9(06).
     03  MS-CNT         PIC  9(06).
     03  GK-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
 01  INDEXES.
     03  I              PIC  9(03).
     03  J              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  CVCSKSHU-ST        PIC  X(02).
 01  SHI-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*
 LINKAGE                SECTION.
 01  PARA-OUT-CNT       PIC  9(07).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-OUT-CNT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 集信データ >>--*
 CVCSKSHU-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CVCSKSHU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0150B CVCSKSHU ERROR " CVCSKSHU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
*****CLOSE    CVCSKSHU  HSHIGKF.
     STOP     RUN.
*----<< 支払合計ファイル >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0150B HSHIGKF ERROR " SHI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
*****CLOSE    CVCSKSHU  HSHIGKF.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG   =    1.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI0150B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     CVCSKSHU.
     OPEN     OUTPUT    HSHIGKF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     SPACE     TO   SHI-REC.
     INITIALIZE              SHI-REC.
*
     PERFORM  900-ONL-READ.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     PERFORM  210-SET   VARYING   I    FROM 1    BY   1
                        UNTIL     I    >    2
                        OR        ONL-F01 (I)    =    SPACE.
*
     PERFORM  900-ONL-READ.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    CVCSKSHU.
     CLOSE    HSHIGKF.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ ﾌｧｲﾙ ﾍｯﾀﾞｰ =" FH-CNT  " +++" UPON CONS.
     DISPLAY  "+++ ﾒｲｻｲ ﾍｯﾀﾞｰ =" MH-CNT  " +++" UPON CONS.
     DISPLAY  "+++ ﾒｲｻｲ  ｹﾝｽｳ =" MS-CNT  " +++" UPON CONS.
     DISPLAY  "+++ ｺﾞｳｹｲ ｹﾝｽｳ =" GK-CNT  " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" OUT-CNT " +++" UPON CONS.
*## 2008/08/28　内部統制対応 NAV
     MOVE     OUT-CNT        TO   PARA-OUT-CNT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI0150B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
**   ACCEPT   INP-X1         FROM CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｼﾊﾗｲｺﾞｳｹｲF ｾｯﾄ                              *
*--------------------------------------------------------------*
 210-SET               SECTION.
*---<< ﾌｧｲﾙ ﾍｯﾀﾞｰ ｾｯﾄ >>---*
     IF       ONL-F01 (I)    =    "A"
              MOVE ONL-F01 (I)    TO   SHI-F01
              MOVE ONL-F02 (I)    TO   SHI-F02
              MOVE ONL-F03 (I)    TO   SHI-F03
              MOVE ONL-F04 (I)    TO   SHI-F04
              MOVE ONL-F05 (I)    TO   SHI-F05
              MOVE ONL-F06 (I)    TO   SHI-F06
              ADD       1         TO   FH-CNT
     END-IF.
*---<< ﾒｲｻｲ ﾍｯﾀﾞｰ ｾｯﾄ >>---*
     IF       ONL-F01 (I)    =    "B"
              MOVE ONL-H01 (I)    TO   SHI-F07
              MOVE ONL-H02 (I)    TO   SHI-F08
              MOVE ONL-H03 (I)    TO   SHI-F09
              MOVE ONL-H04 (I)    TO   SHI-F10
              MOVE ONL-H05 (I)    TO   SHI-F11
              MOVE ONL-H06 (I)    TO   SHI-F12
              MOVE ONL-H07 (I)    TO   SHI-F13
              MOVE ONL-H08 (I)    TO   SHI-F14
              MOVE ONL-H09 (I)    TO   SHI-F15
              ADD       1         TO   MH-CNT
     END-IF.
*---<< ｼﾊﾗｲ ﾒｲｻｲ ｾｯﾄ >>---*
     IF       ONL-F01 (I)    =    "C"
              MOVE SPACE          TO   SHI-F24
              MOVE SPACE          TO   SHI-F25
              MOVE ZERO           TO   SHI-F26
              MOVE ZERO           TO   SHI-F27
              MOVE ZERO           TO   SHI-F28
              MOVE ZERO           TO   SHI-F29
              MOVE ZERO           TO   SHI-F30
              MOVE ZERO           TO   SHI-F31
              PERFORM   211-MEIS-SET
                        VARYING   J    FROM 1    BY   1
                        UNTIL     J    >    3
                        OR        ONL-M (I J)    =    SPACE
     END-IF.
*---<< ｺﾞｳｹｲ ｾｯﾄ >>---*
     IF       ONL-F01 (I)    =    "D"
              MOVE SPACE          TO   SHI-F16
              MOVE SPACE          TO   SHI-F17
              MOVE SPACE          TO   SHI-F18
              MOVE SPACE          TO   SHI-F19
              MOVE ZERO           TO   SHI-F20
              MOVE SPACE          TO   SHI-F21
              MOVE ZERO           TO   SHI-F22
              MOVE SPACE          TO   SHI-F23
              MOVE ONL-G01 (I)    TO   SHI-F24
              MOVE ONL-G02 (I)    TO   SHI-F25
              MOVE ONL-G03 (I)    TO   SHI-F26
              IF   ONL-G03S(I)    =    "-"
                   MULTIPLY       -1   BY   SHI-F26
              END-IF
              MOVE ONL-G04 (I)    TO   SHI-F27
              IF   ONL-G04S(I)    =    "-"
                   MULTIPLY       -1   BY   SHI-F27
              END-IF
              MOVE ONL-G05 (I)    TO   SHI-F28
              IF   ONL-G05S(I)    =    "-"
                   MULTIPLY       -1   BY   SHI-F28
              END-IF
              MOVE ONL-G06 (I)    TO   SHI-F29
              IF   ONL-G06S(I)    =    "-"
                   MULTIPLY       -1   BY   SHI-F29
              END-IF
              MOVE ONL-G07 (I)    TO   SHI-F30
              IF   ONL-G07S(I)    =    "-"
                   MULTIPLY       -1   BY   SHI-F30
              END-IF
              MOVE ONL-G08 (I)    TO   SHI-F31
              ADD       1         TO   GK-CNT
              ADD       1         TO   OUT-CNT
              WRITE     SHI-REC
     END-IF.
 210-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｼﾊﾗｲｺﾞｳｹｲﾒｲｻｲ ｼｭﾂﾘｮｸ                        *
*--------------------------------------------------------------*
 211-MEIS-SET          SECTION.
     MOVE     ONL-M01 (I)    TO   SHI-F16.
     MOVE     ONL-M02 (I)    TO   SHI-F17.
     MOVE     ONL-M03 (I J)  TO   SHI-F18.
     MOVE     ONL-M04 (I J)  TO   SHI-F19.
     MOVE     ONL-M05 (I J)  TO   SHI-F20.
     MOVE     ONL-M06 (I J)  TO   SHI-F21.
     MOVE     ONL-M07 (I J)  TO   SHI-F22.
     IF       ONL-M07S(I J)  =    "-"
              MULTIPLY       -1   BY   SHI-F22
     END-IF.
     MOVE     ONL-M08 (I J)  TO   SHI-F23.
     ADD      1              TO   MS-CNT.
     ADD      1              TO   OUT-CNT.
     WRITE    SHI-REC.
 211-MEIS-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 READ                          *
*--------------------------------------------------------------*
 900-ONL-READ           SECTION.
     READ     CVCSKSHU  AT   END
              MOVE      1         TO   END-FLG
              GO   TO   900-ONL-READ-EXIT
     END-READ.
*
     ADD      1         TO   IN-CNT.
 900-ONL-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
