# SKE9999B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKE9999B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品システム                  *
*　　モジュール名　　　　：　検品明細Ｆ検索　　　　　　　　　　*
*　　作成日／更新日　　　：　00/11/08                          *
*　　作成者／更新者　　　：　NAV                               *
*　　処理概要　　　　　　：　検品明細Ｆの発注数を実際の出荷数　*
*　　　　　　　　　　　　　　の異なるレコード出力する。　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE9999B.
 AUTHOR.                N.KANEKO.
 DATE-WRITTEN.          00/11/08.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
         YA        IS   CHR-2
         YB-21     IS   CHR-21
         YB        IS   CHR-15.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 検品明細Ｆ >>--*
     SELECT   RCVSYUF   ASSIGN         DA-01-S-RCVSYUF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         RCV-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 検品明細Ｆ >>--*
 FD  RCVSYUF            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       42   RECORDS.
     COPY        RCVSYUF     OF      XFDLIB
                 JOINING     RCV     PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)  VALUE SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  RCV-ST             PIC  X(02).
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
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 検品明細Ｆ >>--*
 RCV-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCVSYUF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKE9999B RCVSYUF    ERROR " RCV-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PROG-CNTL              SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     END-FLG   =   "END".
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE9999B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     RCVSYUF.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*
     PERFORM  210-READ-SEC.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*発注数・出荷数をチェックする。
     IF  RCV-F10  NOT =  RCV-F12
         DISPLAY "D = " RCV-F06 " - G = " RCV-F07 " - J = "
                  RCV-F08 " - H = " RCV-F10 " - S = " RCV-F12
         ADD      1      TO   OUT-CNT
     END-IF.
*
     PERFORM  210-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    RCVSYUF.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" OUT-CNT " +++" UPON CONS.
     DISPLAY  "*** SKE9999B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　インプットデータ読む　　　　　　　　　　　　　　　　　*
****************************************************************
 210-READ-SEC           SECTION.
*
     READ     RCVSYUF
        AT    END
              MOVE   "END"   TO   END-FLG
        NOT AT END
              ADD      1     TO   IN-CNT
     END-READ.
*
 210-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
