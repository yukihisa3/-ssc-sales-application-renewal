****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　検品エラーデータ　　　　　　　　　*
*    モジュール名　　　　：　検品エラーデータ重複振分          *
*    作成日／更新日　　　：　2004/01/22                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　検品エラーデータを読み、重複の    *
*                        ：　データを抽出し、正常データのみ    *
*                        ：　にする。　　　　　　              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKE1000B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          04/01/22.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<検品エラーデータ>>----*
     SELECT   RCVERRF1  ASSIGN         DA-01-S-RCVERRF1
                        ORGANIZATION   SEQUENTIAL
                        STATUS         ER1-ST.
*----<<検品エラー正常データ>>----*
     SELECT   RCVERRF2  ASSIGN         DA-01-S-RCVERRF2
                        ORGANIZATION   SEQUENTIAL
                        STATUS         ER2-ST.
*----<<検品エラー異常データ>>----*
     SELECT   RCVERRF3  ASSIGN         DA-01-S-RCVERRF3
                        ORGANIZATION   SEQUENTIAL
                        STATUS         ER3-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<検品エラーデータ>>----*
 FD  RCVERRF1
                        BLOCK CONTAINS 42 RECORDS.
 01  ER1-REC.
     03  ER1-F01        PIC  X(54).
     03  ER1-F02        PIC  X(42).
*----<<検品エラー正常データ>>----*
 FD  RCVERRF2
                        BLOCK CONTAINS 42 RECORDS.
 01  ER2-REC.
     03  ER2-F01        PIC  X(54).
     03  ER2-F02        PIC  X(42).
*----<<検品エラー異常データ>>----*
 FD  RCVERRF3
                        BLOCK CONTAINS 42 RECORDS.
 01  ER3-REC.
     03  ER3-F01        PIC  X(54).
     03  ER3-F02        PIC  X(42).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  FLG-END        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  ER1-CNT        PIC  9(07).
     03  ER2-CNT        PIC  9(07).
     03  ER3-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  ER1-ST         PIC  X(02).
     03  ER2-ST         PIC  X(02).
     03  ER3-ST         PIC  X(02).
*
 01  PG-ID              PIC  X(08)     VALUE  "SKE1000B".
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
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
 01  KEY-AREA.
     03  CUR-KEY        PIC  X(54)  VALUE  SPACE.
     03  BRK-KEY        PIC  X(54)  VALUE  SPACE.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 RCVERRF1               SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCVERRF1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKE1000B RCVERRF1 ERROR " ER1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 RCVERRF2               SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCVERRF2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKE1000B RCVERRF2 ERROR " ER2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 RCVERRF3               SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCVERRF3.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKE1000B RCVERRF3 ERROR " ER3-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL    FLG-END   =    "END".
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
     DISPLAY  "*** SKE1000B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     RCVERRF1.
     OPEN     OUTPUT    RCVERRF2  RCVERRF3.
*クリア
     INITIALIZE    WK-CNT  FLAGS  KEY-AREA.
*
     PERFORM       RCVERRF1-READ-SEC.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     IF  CUR-KEY   NOT =     BRK-KEY
*        正常出力
         MOVE      ER1-REC        TO   ER2-REC
         WRITE     ER2-REC
         ADD       1              TO   ER2-CNT
         MOVE      CUR-KEY        TO   BRK-KEY
         GO   TO   200-MAIN-RTN-010
     END-IF.
*
     IF  CUR-KEY       =     BRK-KEY
*        エラー出力
         MOVE      ER1-REC        TO   ER3-REC
         WRITE     ER3-REC
         ADD       1              TO   ER3-CNT
     END-IF.
*
 200-MAIN-RTN-010.
*
     PERFORM  RCVERRF1-READ-SEC.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    RCVERRF1  RCVERRF2  RCVERRF3.
*
     DISPLAY "* ERROR   (IN)=" ER1-CNT    " *" UPON CONS.
     DISPLAY "* ERROR OK(OT)=" ER2-CNT    " *" UPON CONS.
     DISPLAY "* ERROR NG(OT)=" ER3-CNT    " *" UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE1000B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 RCVERRF1-READ-SEC          SECTION.
     READ     RCVERRF1
         AT END
              MOVE     "END"      TO   FLG-END
         NOT AT END
              ADD       1         TO   ER1-CNT
              MOVE      ER1-F01   TO   CUR-KEY
     END-READ.
 RCVERRF1-READ-EXIT.
     EXIT.
