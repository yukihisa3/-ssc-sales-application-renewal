****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　片倉工業支払照合　　　　　　　　　*
*    モジュール名　　　　：　レコード分割　　　　　　　　　　　*
*    作成日／更新日　　　：　95/11/15                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＣＶＣＳ２５６をＣＶＣＳ１２８に　*
*                        ：　分解する。　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI0170B.
 AUTHOR.                N.T.
 DATE-WRITTEN.          95/11/15.
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
*----<< ＣＶＣＳ２５６ >>--*
     SELECT   CVCS256   ASSIGN         DA-01-S-CVCS256
                        ORGANIZATION   SEQUENTIAL
                        STATUS         CVI-ST.
*----<< ＣＶＣＳ１２８ >>--*
     SELECT   CVCS128   ASSIGN         DA-01-S-CVCS128
                        ORGANIZATION   SEQUENTIAL
                        STATUS         CVO-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< ＣＶＣＳ２５６ >>--*
 FD  CVCS256            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  1   RECORDS.
 01  CVI-REC.
     03  CVI-F01                 PIC  X(128).
     03  CVI-F02                 PIC  X(128).
*----<< ＣＶＣＳ１２８ >>--*
 FD  CVCS128            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  1   RECORDS.
 01  CVO-REC.
     03  CVO-F01                 PIC  X(128).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  CVI-ST             PIC  X(02).
 01  CVO-ST             PIC  X(02).
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
*
 01  END-FLG            PIC  X(03)     VALUE SPACE.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ＣＶＣＳ２５６ >>--*
 CVI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CVCS256.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0170B CVCS256 ERROR " CVI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    CVCS256   CVCS128.
     STOP     RUN.
*----<< ＣＶＣＳ１２８ >>--*
 CVO-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CVCS128.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0170B CVCS128 ERROR " CVO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    CVCS256   CVCS128.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL    END-FLG  NOT =  SPACE.
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
     DISPLAY  "*** SSI0170B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     CVCS256.
     OPEN     OUTPUT    CVCS128.
*
     PERFORM  900-CVI-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE     CVI-F01            TO   CVO-REC.
     WRITE    CVO-REC.
*
     IF       CVI-F02   NOT =   SPACE
              MOVE      CVI-F02  TO   CVO-REC
              WRITE     CVO-REC
     END-IF.
*
     PERFORM  900-CVI-READ.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    CVCS256.
     CLOSE    CVCS128.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI0170B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    ＣＶＣＳ２５６　 READ                        *
*--------------------------------------------------------------*
 900-CVI-READ           SECTION.
     READ     CVCS256
         AT END
              MOVE    "END"      TO   END-FLG
     END-READ.
 900-CVI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
