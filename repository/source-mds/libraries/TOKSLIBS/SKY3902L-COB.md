# SKY3902L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SKY3902L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　_卸管理システム                  *
*    モジュール名　　　　：　_卸データ件数リスト              *
*    作成日／更新日　　　：　2000/12/07                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　各部門毎の_卸データの件数をカウ　*
*                        ：　ントし、リスト出力する。         *
*                        ：　                                  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKY3902L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/12/07.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
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
*----<<部門＝２９００　振替Ｆ（本社）>>----*
     SELECT   HON       ASSIGN         DA-01-S-HON
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HON-ST.
*----<<部門＝２９１０　振替Ｆ（福岡）>>----*
     SELECT   FUK       ASSIGN         DA-01-S-FUK
                        ORGANIZATION   SEQUENTIAL
                        STATUS         FUK-ST.
*----<<部門＝２９２０　振替Ｆ（仙台）>>----*
     SELECT   SEN       ASSIGN         DA-01-S-SEN
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SEN-ST.
*----<<部門＝２９４０　振替Ｆ（岡山）>>----*
     SELECT   OKA       ASSIGN         DA-01-S-OKA
                        ORGANIZATION   SEQUENTIAL
                        STATUS         OKA-ST.
*----<<部門＝２９５０　振替Ｆ（北海道）>>----*
     SELECT   HOK       ASSIGN         DA-01-S-HOK
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HOK-ST.
*----<<部門＝２９９０　振替Ｆ（大阪）>>----*
     SELECT   OSA       ASSIGN         DA-01-S-OSA
                        ORGANIZATION   SEQUENTIAL
                        STATUS         OSA-ST.
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN  TO     LP-04-PRTF
                        FILE    STATUS PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<部門＝２９００　振替Ｆ（本社）>>----*
 FD  HON
                        BLOCK CONTAINS 3 RECORDS.
 01  HON-REC            PIC  X(80).
*----<<部門＝２９１０　振替Ｆ（福岡）>>----*
 FD  FUK
                        BLOCK CONTAINS 3 RECORDS.
 01  FUK-REC            PIC  X(80).
*----<<部門＝２９２０　振替Ｆ（仙台）>>----*
 FD  SEN
                        BLOCK CONTAINS 3 RECORDS.
 01  SEN-REC            PIC  X(80).
*----<<部門＝２９４０　振替Ｆ（岡山）>>----*
 FD  OKA
                        BLOCK CONTAINS 3 RECORDS.
 01  OKA-REC            PIC  X(80).
*----<<部門＝２９５０　振替Ｆ（北海道）>>----*
 FD  HOK
                        BLOCK CONTAINS 3 RECORDS.
 01  HOK-REC            PIC  X(80).
*----<<部門＝２９９０　振替Ｆ（大阪)----*
 FD  OSA
                        BLOCK CONTAINS 3 RECORDS.
 01  OSA-REC            PIC  X(80).
*----<<プリントファイル>>----*
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  HON-FLG        PIC  X(03)   VALUE SPACE.
     03  FUK-FLG        PIC  X(03)   VALUE SPACE.
     03  SEN-FLG        PIC  X(03)   VALUE SPACE.
     03  OKA-FLG        PIC  X(03)   VALUE SPACE.
     03  HOK-FLG        PIC  X(03)   VALUE SPACE.
     03  OSA-FLG        PIC  X(03)   VALUE SPACE.
     03  CHK-FLG        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  HON-CNT        PIC  9(07).
     03  FUK-CNT        PIC  9(07).
     03  SEN-CNT        PIC  9(07).
     03  OKA-CNT        PIC  9(07).
     03  HOK-CNT        PIC  9(07).
     03  OSA-CNT        PIC  9(07).
     03  URI-CNT        PIC  9(07).
     03  SIR-CNT        PIC  9(07).
     03  GOK-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  HON-ST         PIC  X(02).
     03  FUK-ST         PIC  X(02).
     03  SEN-ST         PIC  X(02).
     03  OKA-ST         PIC  X(02).
     03  HOK-ST         PIC  X(02).
     03  OSA-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SKY3902L".
 01  WK-MSG            PIC  N(12)      VALUE   SPACE.
 01  WK-MSG1           PIC  N(12)
                       VALUE NC"営業所へ連絡して下さい。".
 01  WK-MSG2           PIC  N(14)
                       VALUE NC"_卸の確定データは０件です。".
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
     03  FILLER                  PIC  N(05)  VALUE
         NC"※※　確定"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  HD1-AA                  PIC  N(01)
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  N(14)  VALUE
         NC"　_卸データ件数リスト　※※"
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
                                 NC"_卸件数"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
*
 01  SEN0                        CHARACTER  TYPE  IS  CHR-2.
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
*=============================================================
 LINKAGE             SECTION.
*=============================================================
   01  LINK-KBN              PIC  X(01).
****************************************************************
 PROCEDURE              DIVISION       USING     LINK-KBN.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 HONERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HON.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKY3902L HON ERROR " HON-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 FUKERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      FUK.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKY3902L FUK ERROR " FUK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 SENERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKY3902L SEN ERROR " SEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 OKAERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      OKA.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKY3902L OKA ERROR " OKA-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HOKERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HOK.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKY3902L HOK ERROR " HOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 OSAERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      OSA.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKY3902L OSA ERROR " OSA-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKY3902L PRTFILE ERROR " PRT-ST " "
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
     DISPLAY  "*** SKY3902L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HON FUK SEN OKA HOK OSA.
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
*
     IF   LINK-KBN      =   "1"
          MOVE    SPACE          TO  WK-MSG
     END-IF.
     IF   LINK-KBN      =   "2"
          MOVE    WK-MSG1        TO  WK-MSG
     END-IF.
*本社件数カウント
*    売上件数カウント
     PERFORM HON-RD-SEC  UNTIL  HON-FLG = "END".
*    帳票エリアセット
     IF   HON-CNT  >  ZERO
          MOVE    2930           TO  DT1-01
          MOVE NC"ＨＧ部営業課"  TO  DT1-02
          MOVE    HON-CNT        TO  DT1-03
          MOVE    SPACE          TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*福岡件数カウント
*    売上件数カウント
     PERFORM FUK-RD-SEC  UNTIL  FUK-FLG = "END".
*    帳票エリアセット
     IF   FUK-CNT > ZERO
          MOVE    2940           TO  DT1-01
          MOVE NC"九州営業課　"  TO  DT1-02
          MOVE    FUK-CNT        TO  DT1-03
          MOVE    WK-MSG         TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*仙台件数カウント
*    売上件数カウント
     PERFORM SEN-RD-SEC  UNTIL  SEN-FLG = "END".
*    帳票エリアセット
     IF   SEN-CNT > ZERO
          MOVE    3236           TO  DT1-01
          MOVE NC"東日本仙台　"  TO  DT1-02
          MOVE    SEN-CNT        TO  DT1-03
          MOVE    WK-MSG         TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*岡山件数カウント
*    売上件数カウント
     PERFORM OKA-RD-SEC  UNTIL  OKA-FLG = "END".
*    帳票エリアセット
     IF   OKA-CNT > ZERO
          MOVE    3356           TO  DT1-01
          MOVE NC"西日本岡山　"  TO  DT1-02
          MOVE    OKA-CNT        TO  DT1-03
          MOVE    WK-MSG         TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*北海道件数カウント
*    売上件数カウント
     PERFORM HOK-RD-SEC  UNTIL  HOK-FLG = "END".
*    帳票エリアセット
     IF   HOK-CNT > ZERO
          MOVE    3136           TO  DT1-01
          MOVE NC"北海道支店　"  TO  DT1-02
          MOVE    HOK-CNT        TO  DT1-03
          MOVE    WK-MSG         TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*大阪件数カウント
*    売上件数カウント
     PERFORM OSA-RD-SEC  UNTIL  OSA-FLG = "END".
*    帳票エリアセット
     IF   OSA-CNT > ZERO
          MOVE    3346           TO  DT1-01
          MOVE NC"西日本支店　"  TO  DT1-02
          MOVE    OSA-CNT        TO  DT1-03
          MOVE    WK-MSG         TO  DT1-04
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
     CLOSE    HON FUK SEN OKA HOK OSA PRTFILE.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKY3902L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 HON-RD-SEC             SECTION.
     READ   HON   AT  END
            MOVE  "END"  TO  HON-FLG
            NOT   AT  END
            ADD    1     TO  HON-CNT
     END-READ.
 HON-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 FUK-RD-SEC             SECTION.
     READ   FUK   AT  END
            MOVE  "END"  TO  FUK-FLG
            NOT   AT  END
            ADD    1     TO  FUK-CNT
     END-READ.
 FUK-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 SEN-RD-SEC             SECTION.
     READ   SEN   AT  END
            MOVE  "END"  TO  SEN-FLG
            NOT   AT  END
            ADD    1     TO  SEN-CNT
     END-READ.
 SEN-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 OKA-RD-SEC             SECTION.
     READ   OKA   AT  END
            MOVE  "END"  TO  OKA-FLG
            NOT   AT  END
            ADD    1     TO  OKA-CNT
     END-READ.
 OKA-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 HOK-RD-SEC             SECTION.
     READ   HOK   AT  END
            MOVE  "END"  TO  HOK-FLG
            NOT   AT  END
            ADD    1     TO  HOK-CNT
     END-READ.
 HOK-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 OSA-RD-SEC             SECTION.
     READ   OSA   AT  END
            MOVE  "END"  TO  OSA-FLG
            NOT   AT  END
            ADD    1     TO  OSA-CNT
     END-READ.
 OSA-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             ヘッダ部出力処理                                 *
*--------------------------------------------------------------*
 HEAD-WT-SEC                  SECTION.
*項目設定
***  プログラムＩＤ
     MOVE     PG-ID               TO        HD1-00.
     IF       LINK-KBN       =   "1"
              MOVE     NC"前"     TO        HD1-AA
     END-IF.
     IF       LINK-KBN       =   "2"
              MOVE     NC"済"    TO        HD1-AA
     END-IF.
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
     WRITE    PRT-REC      FROM   SEN0      AFTER  2.
     WRITE    PRT-REC      FROM   HD2       AFTER  1.
     WRITE    PRT-REC      FROM   SEN0      AFTER  1.
 HEAD-WT-EXIT.
     EXIT.

```
