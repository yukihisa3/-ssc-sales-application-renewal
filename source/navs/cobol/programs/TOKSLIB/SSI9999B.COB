****************************************************************
*    顧客名　　　　　　　：　_サカタのタネ殿　　　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　ケーヨー支払明細削除　　　　　　　*
*    作成日／更新日　　　：　05/12/22                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払明細データを削除する。　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI9999B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          05/12/22.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払合計ファイル >>--*
     SELECT   SITGKFA   ASSIGN         DA-01-S-SITGKFA
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SHI-ST.
*----<< 支払合計ファイル >>--*
     SELECT   SITGKFWK  ASSIGN         DA-01-S-SITGKFWK
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SWK-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払合計ファイル >>--*
 FD  SITGKFA            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS 20   RECORDS.
     COPY     SITGKFA   OF        XFDLIB
              JOINING   SHI       PREFIX.
 FD  SITGKFWK           LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS 20   RECORDS.
     COPY     SITGKFA   OF        XFDLIB
              JOINING   SWK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHI-ST             PIC  X(02).
 01  SWK-ST             PIC  X(02).
*
 01  END-FLG            PIC  X(03)  VALUE  SPACE.
 01  RD-CNT             PIC  9(07)  VALUE  ZERO.
 01  DL-CNT             PIC  9(07)  VALUE  ZERO.
*
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
 01  WK-DATE            PIC  9(06).
 01  FILLER             REDEFINES      WK-DATE.
     03  WK-YY          PIC  9(02).
     03  WK-MM          PIC  9(02).
     03  WK-DD          PIC  9(02).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計ファイル >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SITGKFA.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI9999B SITGKFA ERROR " SHI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 支払合計ファイル >>--*
 SWK-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SITGKFWK.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI9999B SITGKFWK ERROR " SWK-ST " "
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
     PERFORM  200-MAIN-RTN   UNTIL  END-FLG =  "END".
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
     DISPLAY  "*** SSI9999B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     SITGKFA.
     OPEN     OUTPUT    SITGKFWK.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
*
     PERFORM  900-SHI-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     IF    SHI-F20  >=  051101
           MOVE    SPACE         TO   SWK-REC
           INITIALIZE                 SWK-REC
           MOVE    SHI-REC       TO   SWK-REC
           WRITE   SWK-REC
           ADD     1    TO        DL-CNT
     END-IF.
*
     PERFORM   900-SHI-READ
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
     CLOSE    SITGKFA.
*
     DISPLAY "READ-CNT = " RD-CNT UPON CONS.
     DISPLAY "WRIT-CNT = " DL-CNT UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払合計ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SHI-READ           SECTION.
*
     READ     SITGKFA   AT   END
              MOVE      "END"          TO   END-FLG
              NOT  AT  END
              ADD        1             TO   RD-CNT
     END-READ.
*
 900-SHI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
