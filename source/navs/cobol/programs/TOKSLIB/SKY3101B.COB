****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　電算室殿連携システム（計上）　　　*
*    モジュール名　　　　：　電算室計上データ重複振分          *
*    作成日／更新日　　　：　2000/08/15                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　計上データを読み、重複データの    *
*                        ：　チェックを行い正常とエラーの振    *
*                        ：　分けを行い出力する。              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKY3101B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/08/15.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<ＡＣＯＳ計上データＦ（全社）>>----*
     SELECT   ACOS      ASSIGN         DA-01-S-ACOS
                        ORGANIZATION   SEQUENTIAL
                        STATUS         ACS-ST.
*----<<ＡＣＯＳ計上正常Ｆ>>----*
     SELECT   ACOSOK    ASSIGN         DA-01-S-ACOSOK
                        ORGANIZATION   SEQUENTIAL
                        STATUS         ACSOK-ST.
*----<<ＡＣＯＳ計上エラーＦ>>----*
     SELECT   ACOSERR   ASSIGN         DA-01-S-ACOSERR
                        ORGANIZATION   SEQUENTIAL
                        STATUS         ACSERR-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<ＡＣＯＳ計上Ｆ（全社）>>----*
 FD  ACOS
                        BLOCK CONTAINS 1 RECORDS.
     COPY     TOKU      OF        XFDLIB
              JOINING   ACOS      PREFIX.
*----<<ＡＣＯＳ計上正常Ｆ（全社）>>----*
 FD  ACOSOK
                        BLOCK CONTAINS 1 RECORDS.
     COPY     TOKU      OF        XFDLIB
              JOINING   ACOSOK    PREFIX.
*----<<ＡＣＯＳ計上エラーＦ（全社）>>----*
 FD  ACOSERR
                        BLOCK CONTAINS 1 RECORDS.
     COPY     TOKU      OF        XFDLIB
              JOINING   ACOSERR   PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  FLG-END        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  ACS-CNT        PIC  9(07).
     03  ACSOK-CNT      PIC  9(07).
     03  ACSERR-CNT     PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  ACS-ST         PIC  X(02).
     03  ACSOK-ST       PIC  X(02).
     03  ACSERR-ST      PIC  X(02).
*
 01  PG-ID              PIC  X(08)     VALUE  "SKY3101B".
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
     03  CUR-KEY.
       05  CUR-BUMON    PIC  9(04).
       05  CUR-DENKU    PIC  9(02).
       05  CUR-DENNO    PIC  X(09).
       05  CUR-GYONO    PIC  9(02).
       05  CUR-AKAKURO  PIC  9(01).
       05  CUR-TOKCD    PIC  X(08).
       05  CUR-TENCD    PIC  9(05).
       05  CUR-NOUDT    PIC  9(08).
     03  BRK-KEY.
       05  BRK-BUMON    PIC  9(04).
       05  BRK-DENKU    PIC  9(02).
       05  BRK-DENNO    PIC  X(09).
       05  BRK-GYONO    PIC  9(02).
       05  BRK-AKAKURO  PIC  9(01).
       05  BRK-TOKCD    PIC  X(08).
       05  BRK-TENCD    PIC  9(05).
       05  BRK-NOUDT    PIC  9(08).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 ACOS1                  SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ACOS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY3101B ACOS ERROR " ACS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 ACOS2                  SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ACOSOK.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY3101B ACOSOK  ERROR " ACSOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 ACOS3                  SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ACOSERR.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY3101B ACOSERR ERROR " ACSERR-ST " "
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
     DISPLAY  "*** SKY3101B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     ACOS.
     OPEN     OUTPUT    ACOSOK    ACOSERR.
*クリア
     INITIALIZE    WK-CNT  FLAGS  KEY-AREA.
*
     PERFORM       ACOS-READ-SEC.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     IF  CUR-KEY   NOT =     BRK-KEY
*        正常出力
         MOVE      ACOS-REC       TO   ACOSOK-REC
         WRITE     ACOSOK-REC
         ADD       1              TO   ACSOK-CNT
         MOVE      CUR-KEY        TO   BRK-KEY
         GO   TO   200-MAIN-RTN-010
     END-IF.
*
     IF  CUR-KEY       =     BRK-KEY
*        エラー出力
         MOVE      ACOS-REC       TO   ACOSERR-REC
         WRITE     ACOSERR-REC
         ADD       1              TO   ACSERR-CNT
     END-IF.
*
 200-MAIN-RTN-010.
*
     PERFORM  ACOS-READ-SEC.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    ACOS      ACOSOK    ACOSERR.
*
     DISPLAY "* ACOSFILE(IN)=" ACS-CNT    " *" UPON CONS.
     DISPLAY "* ACOSOK  (OT)=" ACSOK-CNT  " *" UPON CONS.
     DISPLAY "* ACOSERR (OT)=" ACSERR-CNT " *" UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKY3101B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 ACOS-READ-SEC          SECTION.
     READ     ACOS
         AT END
              MOVE     "END"      TO   FLG-END
         NOT AT END
              ADD       1         TO   ACS-CNT
              MOVE      ACOS-F01  TO   CUR-BUMON
              MOVE      ACOS-F02  TO   CUR-DENKU
              MOVE      ACOS-F03  TO   CUR-DENNO
              MOVE      ACOS-F04  TO   CUR-GYONO
              MOVE      ACOS-F05  TO   CUR-AKAKURO
              MOVE      ACOS-F06  TO   CUR-TOKCD
              MOVE      ACOS-F07  TO   CUR-TENCD
              MOVE      ACOS-F09  TO   CUR-NOUDT
     END-READ.
 ACOS-READ-EXIT.
     EXIT.
