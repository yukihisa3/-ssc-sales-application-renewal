****************************************************************
*請求データ削除（前月分請求データ削除を行っていない為のＰＧ）  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            DELKEI.
 AUTHOR.                NAV.
 DATE-WRITTEN.          99/12/10.
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
*----<< 支払合計ファイル >>--*
     SELECT   SETGKFB   ASSIGN         DA-01-S-SETGKFB
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SEB-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   SETGKFA   ASSIGN         DA-01-VI-SETGKFA1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SEA-F01   SEA-F05
                        STATUS         SEA-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払合計ファイル >>--*
 FD  SETGKFB            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFB   OF        XFDLIB
              JOINING   SEB       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  SETGKFA            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEA       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  DEL-CNT        PIC  9(06).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SEB-ST             PIC  X(02).
 01  SEA-ST             PIC  X(02).
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
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計ファイル >>--*
 SETGKFB-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGKFB.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### DEKKEI  SETGKFB ERROR " SEB-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    SETGKFB   SETGKFA.
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 SETGKFA-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGKFA.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### DELKEI  SETGKFA ERROR " SEB-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    SETGKFB   SETGKFA.
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
     OPEN     INPUT     SETGKFB.
     OPEN     I-O       SETGKFA.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
*
     PERFORM  900-SEB-READ.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     MOVE     SEB-F01        TO   SEA-F01.
     MOVE     SEB-F05        TO   SEA-F05.
     READ     SETGKFA
         INVALID
              GO   TO   MAIN-900
     END-READ.
*
     DELETE   SETGKFA.
*
     ADD      1         TO   DEL-CNT.
*
 MAIN-900.
     PERFORM  900-SEB-READ.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    SETGKFB.
     CLOSE    SETGKFA.
*
     DISPLAY "IN-CNT  = " IN-CNT   UPON  CONS.
     DISPLAY "DEK-CNT = " DEL-CNT  UPON  CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払合計ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SEB-READ           SECTION.
     READ     SETGKFB   AT   END
              MOVE      1         TO   END-FLG
              GO   TO   900-SEB-READ-EXIT
     END-READ.
*
     ADD      1         TO   IN-CNT.
*
 900-SEB-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
