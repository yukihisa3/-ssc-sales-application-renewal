****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　日次計上処理　　　　　　　　　　　*
*    モジュール名　　　　：　情報ＳＹＳ部計上ＤＴ件数リスト　　*
*    作成日／更新日　　　：　2018/01/30                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　各部門毎の売上／仕入計上データの　*
*                        ：　件数をカウントし、リスト出力する。*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKY2302L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          18/01/30.
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
*----<<売上計上Ｆ（本社）>>----*
     SELECT   HOU       ASSIGN         DA-01-S-TOKUWK
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HOU-ST.
*----<<仕入計上Ｆ（本社）>>----*
     SELECT   HOS       ASSIGN         DA-01-S-TOKUZ
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HOS-ST.
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN  TO     LP-04-PRTF
                        FILE    STATUS PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<売上計上Ｆ（本社）>>----*
 FD  HOU
                        BLOCK CONTAINS 4 RECORDS.
 01  HOU-REC            PIC  X(256).
*----<<仕入計上Ｆ（本社）>>----*
 FD  HOS
                        BLOCK CONTAINS 1 RECORDS.
 01  HOS-REC            PIC  X(256).
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
 01  WK-CNT.
     03  HOU-CNT        PIC  9(07).
     03  HOS-CNT        PIC  9(07).
     03  URI-CNT        PIC  9(07).
     03  SIR-CNT        PIC  9(07).
     03  GOK-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  WK-ST.
     03  HOU-ST         PIC  X(02).
     03  HOS-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SKY2302L".
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
         NC"※※　ＡＣＯＳ計上データ件数リスト　※※"
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
     03  FILLER                  PIC  N(09)  VALUE
                                 NC"仕入／在庫計上件数"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"総合計"
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
     03  FILLER                  PIC  X(14)  VALUE  SPACE.
     03  DT1-04                  PIC  Z,ZZZ,ZZ9.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-05                  PIC  Z,ZZZ,ZZ9.
 01  GK1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(41)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE  NC"総合計".
     03  FILLER                  PIC  X(14)  VALUE  SPACE.
     03  GK1-01                  PIC  Z,ZZZ,ZZ9.
     03  GKLLER                  PIC  X(14)  VALUE  SPACE.
     03  GK1-02                  PIC  Z,ZZZ,ZZ9.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
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
     DISPLAY  "### SKY2302L HOU ERROR " HOU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HOSERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HOS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2302L HOS ERROR " HOS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2302L PRTFILE ERROR " PRT-ST " "
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
     DISPLAY  "*** SKY2302L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HOU HOS.
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
     MOVE    2900           TO  DT1-01.
     MOVE NC"ＨＧ部全体　"  TO  DT1-02.
     MOVE    HOU-CNT        TO  DT1-03.
     MOVE    HOS-CNT        TO  DT1-04.
     COMPUTE DT1-05 = HOU-CNT + HOS-CNT.
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
     CLOSE    HOU HOS PRTFILE.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKY2302L END   *** "
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