****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　電算室殿連携システム（計上）　　　*
*    モジュール名　　　　：　電算室計上データ件数リスト　　　　*
*    作成日／更新日　　　：　2000/05/20                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　各部門毎の売上／仕入計上データの　*
*                        ：　件数をカウントし、リスト出力する。*
*                        ：　岡山追加(2001/02/27)              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKY2301B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/05/20.
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
*----<<部門＝３２６６　売上計上Ｆ（本社）>>----*
     SELECT   HOU       ASSIGN         DA-01-S-HOU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HOU-ST.
*----<<部門＝３２６６　仕入計上Ｆ（本社）>>----*
     SELECT   HOS       ASSIGN         DA-01-S-HOS
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HOS-ST.
*----<<部門＝３４３６　売上計上Ｆ（福岡）>>----*
     SELECT   FKU       ASSIGN         DA-01-S-FKU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         FKU-ST.
*----<<部門＝３４３６　仕入計上Ｆ（福岡）>>----*
     SELECT   FKS       ASSIGN         DA-01-S-FKS
                        ORGANIZATION   SEQUENTIAL
                        STATUS         FKS-ST.
*----<<部門＝３２３６　売上計上Ｆ（仙台）>>----*
     SELECT   SEU       ASSIGN         DA-01-S-SEU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SEU-ST.
*----<<部門＝３２３６　仕入計上Ｆ（仙台）>>----*
     SELECT   SES       ASSIGN         DA-01-S-SES
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SES-ST.
*----<<部門＝３３５６　売上計上Ｆ（岡山）>>----*
     SELECT   OKU       ASSIGN         DA-01-S-OKU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         OKU-ST.
*----<<部門＝３３５６　仕入計上Ｆ（岡山）>>----*
     SELECT   OKS       ASSIGN         DA-01-S-OKS
                        ORGANIZATION   SEQUENTIAL
                        STATUS         OKS-ST.
*----<<部門＝３１３６　売上計上Ｆ（北海道）>>----*
     SELECT   HKU       ASSIGN         DA-01-S-HKU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HKU-ST.
*----<<部門＝３１３６　仕入計上Ｆ（北海道）>>----*
     SELECT   HKS       ASSIGN         DA-01-S-HKS
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HKS-ST.
*----<<部門＝３３４６　売上計上Ｆ（大阪）>>----*
     SELECT   OSU       ASSIGN         DA-01-S-OSU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         OSU-ST.
*----<<部門＝３３４６　仕入計上Ｆ（大阪）>>----*
     SELECT   OSS       ASSIGN         DA-01-S-OSS
                        ORGANIZATION   SEQUENTIAL
                        STATUS         OSS-ST.
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN  TO     LP-04-PRTF
                        FILE    STATUS PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<部門＝３２６６　売上計上Ｆ（本社）>>----*
 FD  HOU
                        BLOCK CONTAINS 4 RECORDS.
 01  HOU-REC            PIC  X(256).
*----<<部門＝３２６６　仕入計上Ｆ（本社）>>----*
 FD  HOS
                        BLOCK CONTAINS 1 RECORDS.
 01  HOU-REC            PIC  X(256).
*----<<部門＝３４３６　売上計上Ｆ（福岡）>>----*
 FD  FKU
                        BLOCK CONTAINS 4 RECORDS.
 01  FKU-REC            PIC  X(256).
*----<<部門＝３４３６　仕入計上Ｆ（福岡）>>----*
 FD  FKS
                        BLOCK CONTAINS 1 RECORDS.
 01  FKS-REC            PIC  X(256).
*----<<部門＝３２３６　売上計上Ｆ（仙台）>>----*
 FD  SEU
                        BLOCK CONTAINS 4 RECORDS.
 01  SEU-REC            PIC  X(256).
*----<<部門＝３２３６　仕入計上Ｆ（仙台）>>----*
 FD  SES
                        BLOCK CONTAINS 1 RECORDS.
 01  SES-REC            PIC  X(256).
*----<<部門＝３１３６　売上計上Ｆ（北海道）>>----*
 FD  HKU
                        BLOCK CONTAINS 4 RECORDS.
 01  HKU-REC            PIC  X(256).
*----<<部門＝３１３６　仕入計上Ｆ（北海道）>>----*
 FD  HKS
                        BLOCK CONTAINS 1 RECORDS.
 01  HKS-REC            PIC  X(256).
*----<<部門＝３３５６　売上計上Ｆ（岡山)----*
 FD  OKU
                        BLOCK CONTAINS 4 RECORDS.
 01  OKU-REC            PIC  X(256).
*----<<部門＝３３５６　仕入計上Ｆ（岡山）>>----*
 FD  OKS
                        BLOCK CONTAINS 1 RECORDS.
 01  OKS-REC            PIC  X(256).
*----<<部門＝３３４６　売上計上Ｆ（大阪)----*
 FD  OSU
                        BLOCK CONTAINS 4 RECORDS.
 01  OSU-REC            PIC  X(256).
*----<<部門＝３３４６　仕入計上Ｆ（大阪）>>----*
 FD  OSS
                        BLOCK CONTAINS 1 RECORDS.
 01  OSS-REC            PIC  X(256).
*----<<プリントファイル>>----*
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  HOU-FLG        PIC  X(03)   VALUE SPACE.
     03  HOS-FLG        PIC  X(03)   VALUE SPACE.
     03  FKU-FLG        PIC  X(03)   VALUE SPACE.
     03  FKS-FLG        PIC  X(03)   VALUE SPACE.
     03  SEU-FLG        PIC  X(03)   VALUE SPACE.
     03  SES-FLG        PIC  X(03)   VALUE SPACE.
     03  HKU-FLG        PIC  X(03)   VALUE SPACE.
     03  HKS-FLG        PIC  X(03)   VALUE SPACE.
     03  OSU-FLG        PIC  X(03)   VALUE SPACE.
     03  OSS-FLG        PIC  X(03)   VALUE SPACE.
     03  OKU-FLG        PIC  X(03)   VALUE SPACE.
     03  OKS-FLG        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  HOU-CNT        PIC  9(07).
     03  HOS-CNT        PIC  9(07).
     03  FKU-CNT        PIC  9(07).
     03  FKS-CNT        PIC  9(07).
     03  SEU-CNT        PIC  9(07).
     03  SES-CNT        PIC  9(07).
     03  HKU-CNT        PIC  9(07).
     03  HKS-CNT        PIC  9(07).
     03  OSU-CNT        PIC  9(07).
     03  OSS-CNT        PIC  9(07).
     03  OKU-CNT        PIC  9(07).
     03  OKS-CNT        PIC  9(07).
     03  URI-CNT        PIC  9(07).
     03  SIR-CNT        PIC  9(07).
     03  GOK-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  HOU-ST         PIC  X(02).
     03  HOS-ST         PIC  X(02).
     03  FKU-ST         PIC  X(02).
     03  FKS-ST         PIC  X(02).
     03  SEU-ST         PIC  X(02).
     03  SES-ST         PIC  X(02).
     03  HKU-ST         PIC  X(02).
     03  HKS-ST         PIC  X(02).
     03  OSU-ST         PIC  X(02).
     03  OSS-ST         PIC  X(02).
     03  OKU-ST         PIC  X(02).
     03  OKS-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SKY2301B".
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
         NC"※※　電算室　計上データ件数リスト　※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(11)  VALUE  SPACE.
     03  HD1-01                  PIC  9(04).
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
 01  HD2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  FILLER                  PIC  N(02)  VALUE
                                 NC"部門"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(19)  VALUE  SPACE.
     03  FILLER                  PIC  N(06)  VALUE
                                 NC"売上計上件数"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  FILLER                  PIC  N(06)  VALUE
                                 NC"仕入計上件数"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"部門総合計"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
 01  DT1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  DT1-01                  PIC  9(04).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-02                  PIC  N(06).
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  DT1-03                  PIC  Z,ZZZ,ZZ9.
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  DT1-04                  PIC  Z,ZZZ,ZZ9.
     03  FILLER                  PIC  X(06)  VALUE  SPACE.
     03  DT1-05                  PIC  Z,ZZZ,ZZ9.
 01  GK1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(41)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE  NC"総合計".
     03  FILLER                  PIC  X(14)  VALUE  SPACE.
     03  GK1-01                  PIC  Z,ZZZ,ZZ9.
     03  GKLLER                  PIC  X(08)  VALUE  SPACE.
     03  GK1-02                  PIC  Z,ZZZ,ZZ9.
     03  FILLER                  PIC  X(06)  VALUE  SPACE.
     03  GK1-03                  PIC  Z,ZZZ,ZZ9.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 HOUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HOU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2301B HOU ERROR " HOU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HOSERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HOS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2301B HOS ERROR " HOS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 FKUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      FKU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2301B FKU ERROR " FKU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 FKSERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      FKS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2301B FKS ERROR " FKS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 SEUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SEU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2301B SEU ERROR " SEU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 SESERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SES.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2301B SES ERROR " SES-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HKUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HKU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2301B SEU ERROR " SEU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HKSERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HKS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2301B HKS ERROR " HKS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 OSUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      OSU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2301B OSU ERROR " OSU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 OSSERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      OSS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2301B OSS ERROR " OSS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 OKUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      OKU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2301B OKU ERROR " OKU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 OKSERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      OKS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2301B OKS ERROR " OKS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2301B PRTFILE ERROR " PRT-ST " "
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
     DISPLAY  "*** SKY2301B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HOU HOS FKU FKS SEU SES HKU HKS OSU OSS
                        OKU OKS.
     OPEN     OUTPUT    PRTFILE.
*クリア
     INITIALIZE    WK-CNT  FLAGS.
*ヘッダ行印字
     PERFORM       HEAD-WT-SEC.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*本社件数カウント
*    売上件数カウント
     PERFORM HOU-RD-SEC  UNTIL  HOU-FLG = "END".
*    仕入件数カウント
     PERFORM HOS-RD-SEC  UNTIL  HOS-FLG = "END".
*    売上合計へ加算
     ADD     HOU-CNT        TO  URI-CNT.
*    仕入合計へ加算
     ADD     HOS-CNT        TO  SIR-CNT.
*    売上／仕入件数＝＞総合計へ加算
     ADD     HOU-CNT        TO  GOK-CNT.
     ADD     HOS-CNT        TO  GOK-CNT.
*    帳票エリアセット
     MOVE    3266           TO  DT1-01.
     MOVE NC"本社　　　　"  TO  DT1-02.
     MOVE    HOU-CNT        TO  DT1-03.
     MOVE    HOS-CNT        TO  DT1-04.
     COMPUTE DT1-05 = HOU-CNT + HOS-CNT.
     WRITE   PRT-REC   FROM   DT1    AFTER  1.
     WRITE   PRT-REC   FROM   SEN1   AFTER  1.
*福岡件数カウント
*    売上件数カウント
     PERFORM FKU-RD-SEC  UNTIL  FKU-FLG = "END".
*    仕入件数カウント
     PERFORM FKS-RD-SEC  UNTIL  FKS-FLG = "END".
*    売上合計へ加算
     ADD     FKU-CNT        TO  URI-CNT.
*    仕入合計へ加算
     ADD     FKS-CNT        TO  SIR-CNT.
*    売上／仕入件数＝＞総合計へ加算
     ADD     FKU-CNT        TO  GOK-CNT.
     ADD     FKS-CNT        TO  GOK-CNT.
*    帳票エリアセット
     MOVE    3436           TO  DT1-01.
     MOVE NC"福岡営業所　"  TO  DT1-02.
     MOVE    FKU-CNT        TO  DT1-03.
     MOVE    FKS-CNT        TO  DT1-04.
     COMPUTE DT1-05 = FKU-CNT + FKS-CNT.
     WRITE   PRT-REC   FROM   DT1    AFTER  1.
     WRITE   PRT-REC   FROM   SEN1   AFTER  1.
*仙台件数カウント
*    売上件数カウント
     PERFORM SEU-RD-SEC  UNTIL  SEU-FLG = "END".
*    仕入件数カウント
     PERFORM SES-RD-SEC  UNTIL  SES-FLG = "END".
*    売上合計へ加算
     ADD     SEU-CNT        TO  URI-CNT.
*    仕入合計へ加算
     ADD     SES-CNT        TO  SIR-CNT.
*    売上／仕入件数＝＞総合計へ加算
     ADD     SEU-CNT        TO  GOK-CNT.
     ADD     SES-CNT        TO  GOK-CNT.
*    帳票エリアセット
     MOVE    3236           TO  DT1-01.
     MOVE NC"仙台営業所　"  TO  DT1-02.
     MOVE    SEU-CNT        TO  DT1-03.
     MOVE    SES-CNT        TO  DT1-04.
     COMPUTE DT1-05 = SEU-CNT + SES-CNT.
     WRITE   PRT-REC   FROM   DT1    AFTER  1.
     WRITE   PRT-REC   FROM   SEN1   AFTER  1.
*岡山件数カウント
*    売上件数カウント
     PERFORM OKU-RD-SEC  UNTIL  OKU-FLG = "END".
*    仕入件数カウント
     PERFORM OKS-RD-SEC  UNTIL  OKS-FLG = "END".
*    売上合計へ加算
     ADD     OKU-CNT        TO  URI-CNT.
*    仕入合計へ加算
     ADD     OKS-CNT        TO  SIR-CNT.
*    売上／仕入件数＝＞総合計へ加算
     ADD     OKU-CNT        TO  GOK-CNT.
     ADD     OKS-CNT        TO  GOK-CNT.
*    帳票エリアセット
     MOVE    3356           TO  DT1-01.
     MOVE NC"岡山営業所　"  TO  DT1-02.
     MOVE    OKU-CNT        TO  DT1-03.
     MOVE    OKS-CNT        TO  DT1-04.
     COMPUTE DT1-05 = OKU-CNT + OKS-CNT.
     WRITE   PRT-REC   FROM   DT1    AFTER  1.
     WRITE   PRT-REC   FROM   SEN1   AFTER  1.
*北海道件数カウント
*    売上件数カウント
     PERFORM HKU-RD-SEC  UNTIL  HKU-FLG = "END".
*    仕入件数カウント
     PERFORM HKS-RD-SEC  UNTIL  HKS-FLG = "END".
*    売上合計へ加算
     ADD     HKU-CNT        TO  URI-CNT.
*    仕入合計へ加算
     ADD     HKS-CNT        TO  SIR-CNT.
*    売上／仕入件数＝＞総合計へ加算
     ADD     HKU-CNT        TO  GOK-CNT.
     ADD     HKS-CNT        TO  GOK-CNT.
*    帳票エリアセット
     MOVE    3136           TO  DT1-01.
     MOVE NC"北海道営業所"  TO  DT1-02.
     MOVE    HKU-CNT        TO  DT1-03.
     MOVE    HKS-CNT        TO  DT1-04.
     COMPUTE DT1-05 = HKU-CNT + HKS-CNT.
     WRITE   PRT-REC   FROM   DT1    AFTER  1.
     WRITE   PRT-REC   FROM   SEN1   AFTER  1.
*大阪件数カウント
*    売上件数カウント
     PERFORM OSU-RD-SEC  UNTIL  OSU-FLG = "END".
*    仕入件数カウント
     PERFORM OSS-RD-SEC  UNTIL  OSS-FLG = "END".
*    売上合計へ加算
     ADD     OSU-CNT        TO  URI-CNT.
*    仕入合計へ加算
     ADD     OSS-CNT        TO  SIR-CNT.
*    売上／仕入件数＝＞総合計へ加算
     ADD     OSU-CNT        TO  GOK-CNT.
     ADD     OSS-CNT        TO  GOK-CNT.
*    帳票エリアセット
     MOVE    3346           TO  DT1-01.
     MOVE NC"大阪営業所　"  TO  DT1-02.
     MOVE    OSU-CNT        TO  DT1-03.
     MOVE    OSS-CNT        TO  DT1-04.
     COMPUTE DT1-05 = OSU-CNT + OSS-CNT.
     WRITE   PRT-REC   FROM   DT1    AFTER  1.
     WRITE   PRT-REC   FROM   SEN1   AFTER  1.
*    総合計行印字
     MOVE    URI-CNT        TO  GK1-01.
     MOVE    SIR-CNT        TO  GK1-02.
     MOVE    GOK-CNT        TO  GK1-03.
     WRITE   PRT-REC   FROM   GK1    AFTER  1.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HOU HOS FKU FKS SEU SES HKU HKS OSU OSS PRTFILE
              OKU OKS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKY2301B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 HOU-RD-SEC             SECTION.
     READ   HOU   AT  END
            MOVE  "END"  TO  HOU-FLG
            NOT   AT  END
            ADD    1     TO  HOU-CNT
     END-READ.
 HOU-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  仕入計上データ読込み                        *
*--------------------------------------------------------------*
 HOS-RD-SEC             SECTION.
     READ   HOS   AT  END
            MOVE  "END"  TO  HOS-FLG
            NOT   AT  END
            ADD    1     TO  HOS-CNT
     END-READ.
 HOS-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 FKU-RD-SEC             SECTION.
     READ   FKU   AT  END
            MOVE  "END"  TO  FKU-FLG
            NOT   AT  END
            ADD    1     TO  FKU-CNT
     END-READ.
 FKU-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  仕入計上データ読込み                        *
*--------------------------------------------------------------*
 FKS-RD-SEC             SECTION.
     READ   FKS   AT  END
            MOVE  "END"  TO  FKS-FLG
            NOT   AT  END
            ADD    1     TO  FKS-CNT
     END-READ.
 FKS-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 SEU-RD-SEC             SECTION.
     READ   SEU   AT  END
            MOVE  "END"  TO  SEU-FLG
            NOT   AT  END
            ADD    1     TO  SEU-CNT
     END-READ.
 SEU-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  仕入計上データ読込み                        *
*--------------------------------------------------------------*
 SES-RD-SEC             SECTION.
     READ   SES   AT  END
            MOVE  "END"  TO  SES-FLG
            NOT   AT  END
            ADD    1     TO  SES-CNT
     END-READ.
 SES-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 HKU-RD-SEC             SECTION.
     READ   HKU   AT  END
            MOVE  "END"  TO  HKU-FLG
            NOT   AT  END
            ADD    1     TO  HKU-CNT
     END-READ.
 HKU-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  仕入計上データ読込み                        *
*--------------------------------------------------------------*
 HKS-RD-SEC             SECTION.
     READ   HKS   AT  END
            MOVE  "END"  TO  HKS-FLG
            NOT   AT  END
            ADD    1     TO  HKS-CNT
     END-READ.
 HKS-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 OSU-RD-SEC             SECTION.
     READ   OSU   AT  END
            MOVE  "END"  TO  OSU-FLG
            NOT   AT  END
            ADD    1     TO  OSU-CNT
     END-READ.
 OSU-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  仕入計上データ読込み                        *
*--------------------------------------------------------------*
 OSS-RD-SEC             SECTION.
     READ   OSS   AT  END
            MOVE  "END"  TO  OSS-FLG
            NOT   AT  END
            ADD    1     TO  OSS-CNT
     END-READ.
 OSS-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 OKU-RD-SEC             SECTION.
     READ   OKU   AT  END
            MOVE  "END"  TO  OKU-FLG
            NOT   AT  END
            ADD    1     TO  OKU-CNT
     END-READ.
 OKU-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  仕入計上データ読込み                        *
*--------------------------------------------------------------*
 OKS-RD-SEC             SECTION.
     READ   OKS   AT  END
            MOVE  "END"  TO  OKS-FLG
            NOT   AT  END
            ADD    1     TO  OKS-CNT
     END-READ.
 OKS-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             ヘッダ部出力処理                                 *
*--------------------------------------------------------------*
 HEAD-WT-SEC                  SECTION.
*項目設定
***  プログラムＩＤ
     MOVE     PG-ID               TO        HD1-00.
***  日付
     MOVE     "3"                 TO        LINK-IN-KBN.
     MOVE     SYS-DATE            TO        LINK-IN-YMD6.
     MOVE     ZERO                TO        LINK-IN-YMD8.
     MOVE     ZERO                TO        LINK-OUT-RET.
     MOVE     ZERO                TO        LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING        LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD(1:4)   TO        HD1-01.
     MOVE     LINK-OUT-YMD(5:2)   TO        HD1-02.
     MOVE     LINK-OUT-YMD(7:2)   TO        HD1-03.
***  ページ_
     MOVE     1                   TO        HD1-04.
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  3.
     WRITE    PRT-REC      FROM   SEN       AFTER  2.
     WRITE    PRT-REC      FROM   HD2       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
 HEAD-WT-EXIT.
     EXIT.
