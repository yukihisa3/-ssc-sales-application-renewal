# SKY2701L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKY2701L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　電算室殿連携システム（振替）　　　*
*    モジュール名　　　　：　電算室計上データ件数リスト　　　　*
*    作成日／更新日　　　：　2000/05/20                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　各部門毎の売上／仕入計上データの　*
*                        ：　件数をカウントし、リスト出力する。*
*                        ：　                                  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKY2701L.
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
*----<<部門＝３２６６　振替Ｆ（本社）>>----*
     SELECT   HOU       ASSIGN         DA-01-S-HOU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HOU-ST.
*----<<部門＝３４３６　振替Ｆ（福岡）>>----*
     SELECT   FKU       ASSIGN         DA-01-S-FKU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         FKU-ST.
*----<<部門＝３２３６　振替Ｆ（仙台）>>----*
     SELECT   SEU       ASSIGN         DA-01-S-SEU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SEU-ST.
*----<<部門＝３３５６　振替Ｆ（岡山）>>----*
     SELECT   OKU       ASSIGN         DA-01-S-OKU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SEU-ST.
*----<<部門＝３１３６　振替Ｆ（北海道）>>----*
     SELECT   HKU       ASSIGN         DA-01-S-HKU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HKU-ST.
*----<<部門＝３３４６　振替Ｆ（大阪） >>----*
     SELECT   OSU       ASSIGN         DA-01-S-OSU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         OSU-ST.
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN  TO     LP-04-PRTF
                        FILE    STATUS PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<部門＝３２６６　振替Ｆ（本社）>>----*
 FD  HOU
                        BLOCK CONTAINS 1 RECORDS.
 01  HOU-REC            PIC  X(150).
*----<<部門＝３４３６　振替Ｆ（福岡）>>----*
 FD  FKU
                        BLOCK CONTAINS 1 RECORDS.
 01  FKU-REC            PIC  X(150).
*----<<部門＝３２３６　振替Ｆ（仙台）>>----*
 FD  SEU
                        BLOCK CONTAINS 1 RECORDS.
 01  SEU-REC            PIC  X(150).
*----<<部門＝３３５６　振替Ｆ（岡山）>>----*
 FD  OKU
                        BLOCK CONTAINS 1 RECORDS.
 01  OKU-REC            PIC  X(150).
*----<<部門＝３１３６　振替Ｆ（北海道）>>----*
 FD  HKU
                        BLOCK CONTAINS 1 RECORDS.
 01  HKU-REC            PIC  X(150).
*----<<部門＝３３４６　振替Ｆ（大阪)----*
 FD  OSU
                        BLOCK CONTAINS 1 RECORDS.
 01  OSU-REC            PIC  X(150).
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
     03  FKU-FLG        PIC  X(03)   VALUE SPACE.
     03  SEU-FLG        PIC  X(03)   VALUE SPACE.
     03  OKU-FLG        PIC  X(03)   VALUE SPACE.
     03  HKU-FLG        PIC  X(03)   VALUE SPACE.
     03  OSU-FLG        PIC  X(03)   VALUE SPACE.
     03  CHK-FLG        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  HOU-CNT        PIC  9(07).
     03  FKU-CNT        PIC  9(07).
     03  SEU-CNT        PIC  9(07).
     03  OKU-CNT        PIC  9(07).
     03  HKU-CNT        PIC  9(07).
     03  OSU-CNT        PIC  9(07).
     03  URI-CNT        PIC  9(07).
     03  SIR-CNT        PIC  9(07).
     03  GOK-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  HOU-ST         PIC  X(02).
     03  FKU-ST         PIC  X(02).
     03  OKU-ST         PIC  X(02).
     03  SEU-ST         PIC  X(02).
     03  HKU-ST         PIC  X(02).
     03  OSU-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SKY2701L".
 01  WK-MSG1           PIC  N(12)
                       VALUE NC"営業所へ連絡して下さい。".
 01  WK-MSG2           PIC  N(14)
                       VALUE NC"本日の振替データは０件です。".
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
         NC"※※　電算室　振替データ件数リスト　※※"
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
     03  FILLER                  PIC  X(24)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"振替件数"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
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
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-04                  PIC  N(12).
 01  DT2                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  DT2-01                  PIC  N(14).
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
     DISPLAY  "### SKY2701L HOU ERROR " HOU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 FKUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      FKU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2701L FKU ERROR " FKU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 SEUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SEU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2701L SEU ERROR " SEU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HKUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HKU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2701L SEU ERROR " SEU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 OSUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      OSU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2701L OSU ERROR " OSU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2701L PRTFILE ERROR " PRT-ST " "
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
     DISPLAY  "*** SKY2701L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HOU FKU OKU SEU HKU OSU.
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
*    帳票エリアセット
     IF   HOU-CNT  >  ZERO
          MOVE    3266           TO  DT1-01
          MOVE NC"本社　　　　"  TO  DT1-02
          MOVE    HOU-CNT        TO  DT1-03
          MOVE    SPACE          TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*福岡件数カウント
*    売上件数カウント
     PERFORM FKU-RD-SEC  UNTIL  FKU-FLG = "END".
*    帳票エリアセット
     IF   FKU-CNT > ZERO
          MOVE    3436           TO  DT1-01
          MOVE NC"福岡営業所　"  TO  DT1-02
          MOVE    FKU-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*仙台件数カウント
*    売上件数カウント
     PERFORM SEU-RD-SEC  UNTIL  SEU-FLG = "END".
*    帳票エリアセット
     IF   SEU-CNT > ZERO
          MOVE    3236           TO  DT1-01
          MOVE NC"仙台営業所　"  TO  DT1-02
          MOVE    SEU-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*岡山件数カウント
*    売上件数カウント
     PERFORM OKU-RD-SEC  UNTIL  OKU-FLG = "END".
*    帳票エリアセット
     IF   OKU-CNT > ZERO
          MOVE    3356           TO  DT1-01
          MOVE NC"岡山営業所　"  TO  DT1-02
          MOVE    OKU-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*北海道件数カウント
*    売上件数カウント
     PERFORM HKU-RD-SEC  UNTIL  HKU-FLG = "END".
*    帳票エリアセット
     IF   HKU-CNT > ZERO
          MOVE    3136           TO  DT1-01
          MOVE NC"北海道営業所"  TO  DT1-02
          MOVE    HKU-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*大阪件数カウント
*    売上件数カウント
     PERFORM OSU-RD-SEC  UNTIL  OSU-FLG = "END".
*    帳票エリアセット
     IF   OSU-CNT > ZERO
          MOVE    3346           TO  DT1-01
          MOVE NC"大阪営業所　"  TO  DT1-02
          MOVE    OSU-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*    件数チェック
     IF   CHK-FLG = SPACE
          MOVE    WK-MSG2        TO  DT2-01
          WRITE   PRT-REC   FROM   DT2    AFTER  5
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HOU FKU SEU OKU HKU OSU PRTFILE.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKY2701L END *** "
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

```
