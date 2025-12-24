****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　出荷検品システム　　　　　　　　　*
*    モジュール名　　　　：　取込／累積件数リスト作成　　　　　*
*    作成日／更新日　　　：　2000/11/15                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　取込／累積件数リストの作成　　　　*
*                        ：　                                  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKE0230L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/11/15.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
     YA            IS        CHR-2
     YB-21         IS        CHR-21
     YB            IS        CHR-15
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN  TO     LP-04-PRTF
                        FILE    STATUS PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<プリントファイル>>----*
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  WK-CNT.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  PRT-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SKE0230L".
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
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
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  HD1-00                  PIC  X(08).
     03  FILLER                  PIC  X(25)  VALUE  SPACE.
     03  FILLER                  PIC  N(20)  VALUE
         NC"※※　　　取込／累積件数リスト　　　※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(11)  VALUE  SPACE.
     03  HD1-01                  PIC  9(02).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-02                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD1-04                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  DT1.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  DT1-01                  PIC  X(25).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-02                  PIC  ZZZ,ZZ9.
*
 LINKAGE                SECTION.
 01  PARA-IN1                PIC  9(06).
 01  PARA-OUT1               PIC  9(06).
 01  PARA-IN2                PIC  9(06).
 01  PARA-OUT2               PIC  9(06).
 01  PARA-IN3                PIC  9(06).
 01  PARA-OUT3               PIC  9(06).
*
****************************************************************
 PROCEDURE              DIVISION  USING     PARA-IN1  PARA-OUT1
                                            PARA-IN2  PARA-OUT2
                                            PARA-IN3  PARA-OUT3.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKE0230L PRTFILE ERROR " PRT-ST " "
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
     PERFORM  200-MAIN-RTN.
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
     DISPLAY  "*** SKE0230L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     OUTPUT    PRTFILE.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     MOVE    "SKE0230L"      TO   HD1-00.
     MOVE     SYS-YY         TO   HD1-01.
     MOVE     SYS-MM         TO   HD1-02.
     MOVE     SYS-DD         TO   HD1-03.
     MOVE     1              TO   HD1-04.
     WRITE    PRT-REC        FROM HD1.
     WRITE    PRT-REC        FROM SEN.
*
     MOVE     "ｹﾝﾋﾟﾝﾒｲｻｲFILE     ﾄﾘｺﾐ = "
                             TO   DT1-01.
     MOVE     PARA-IN1       TO   DT1-02.
     WRITE    PRT-REC        FROM DT1  AFTER    2.
     MOVE     "ｹﾝﾋﾟﾝﾒｲｻｲFILE     ﾙｲｾｷ = "
                             TO   DT1-01.
     MOVE     PARA-OUT1      TO   DT1-02.
     WRITE    PRT-REC        FROM DT1.
*
     MOVE     "ｼｭｯｶﾗﾍﾞﾙﾍｯﾀﾞFILE  ﾄﾘｺﾐ = "
                             TO   DT1-01.
     MOVE     PARA-IN2       TO   DT1-02.
     WRITE    PRT-REC        FROM DT1  AFTER    2.
     MOVE     "ｼｭｯｶﾗﾍﾞﾙﾍｯﾀﾞFILE  ﾙｲｾｷ = "
                             TO   DT1-01.
     MOVE     PARA-OUT2      TO   DT1-02.
     WRITE    PRT-REC        FROM DT1.
*
     MOVE     "ｼｭｯｶﾗﾍﾞﾙﾒｲｻｲFILE  ﾄﾘｺﾐ = "
                             TO   DT1-01.
     MOVE     PARA-IN3       TO   DT1-02.
     WRITE    PRT-REC        FROM DT1  AFTER    2.
     MOVE     "ｼｭｯｶﾗﾍﾞﾙﾒｲｻｲFILE  ﾙｲｾｷ = "
                             TO   DT1-01.
     MOVE     PARA-OUT3      TO   DT1-02.
     WRITE    PRT-REC        FROM DT1.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    PRTFILE.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE0230L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
