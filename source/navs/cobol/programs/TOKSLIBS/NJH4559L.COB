****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　イオンオンラインシステム　　　　　*
*    モジュール名　　　　：　オンライン件数リスト発行　　　　　*
*    作成日／更新日　　　：　2003/05/06                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　営業所毎の振分けられた受信データ　*
*                        ：　を読み、件数リストを出力する。　　*
*                        ：　                                  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJH4559L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          03/05/06.
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
*----<<東日本第二データ　　　　　　　>>----*
     SELECT   HOU       ASSIGN         DA-01-S-HOU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HOU-ST.
*----<<九州支店データ　　　　　　　　>>----*
     SELECT   FKU       ASSIGN         DA-01-S-FKU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         FKU-ST.
*----<<東日本仙台データ　　　　　　　>>----*
     SELECT   SEU       ASSIGN         DA-01-S-SEU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SEU-ST.
*----<<西日本支店岡山　　　　　　　　>>----*
     SELECT   OKU       ASSIGN         DA-01-S-OKU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SEU-ST.
*----<<北海道支店　　　　　　　　　　　>>----*
     SELECT   HKU       ASSIGN         DA-01-S-HKU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HKU-ST.
*----<<西日本支店データ　　　　　　　>>----*
     SELECT   OSU       ASSIGN         DA-01-S-OSU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         OSU-ST.
*----<<予備１　　　　　　　　　　　　>>----*
     SELECT   YOBI1     ASSIGN         DA-01-S-YOBI1
                        ORGANIZATION   SEQUENTIAL
                        STATUS         YB1-ST.
*----<<予備２　　　　　　　　　　　　>>----*
     SELECT   YOBI2     ASSIGN         DA-01-S-YOBI2
                        ORGANIZATION   SEQUENTIAL
                        STATUS         YB2-ST.
*----<<対象外データ　　　　　　　　　>>----*
     SELECT   YOBI3     ASSIGN         DA-01-S-YOBI3
                        ORGANIZATION   SEQUENTIAL
                        STATUS         YB3-ST.
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN  TO     LP-04-PRTF
                        FILE    STATUS PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<東日本第二データ　　　　　　　>>----*
 FD  HOU
                        BLOCK CONTAINS 1 RECORDS.
 01  HOU-REC            PIC  X(128).
*----<<九州支店データ　　　　　　　　>>----*
 FD  FKU
                        BLOCK CONTAINS 1 RECORDS.
 01  FKU-REC            PIC  X(128).
*----<<東日本仙台データ　　　　　　　>>----*
 FD  SEU
                        BLOCK CONTAINS 1 RECORDS.
 01  SEU-REC            PIC  X(128).
*----<<西日本岡山データ　　　　　　　>>----*
 FD  OKU
                        BLOCK CONTAINS 1 RECORDS.
 01  OKU-REC            PIC  X(128).
*----<<北海道支店データ　　　　　　　>>----*
 FD  HKU
                        BLOCK CONTAINS 1 RECORDS.
 01  HKU-REC            PIC  X(128).
*----<<西日本支店データ　　　　　　　>>----*
 FD  OSU
                        BLOCK CONTAINS 1 RECORDS.
 01  OSU-REC            PIC  X(128).
*----<<予備１　　　　　　　　　　　　>>----*
 FD  YOBI1
                        BLOCK CONTAINS 1 RECORDS.
 01  YB1-REC            PIC  X(128).
*----<<予備２　　　　　　　　　　　　>>----*
 FD  YOBI2
                        BLOCK CONTAINS 1 RECORDS.
 01  YB2-REC            PIC  X(128).
*----<<対象外データ　　　　　　　　　>>----*
 FD  YOBI3
                        BLOCK CONTAINS 1 RECORDS.
 01  YB3-REC            PIC  X(128).
*----<<プリントファイル　　　　　　　>>----*
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
     03  YB1-FLG        PIC  X(03)   VALUE SPACE.
     03  YB2-FLG        PIC  X(03)   VALUE SPACE.
     03  YB3-FLG        PIC  X(03)   VALUE SPACE.
     03  CHK-FLG        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  HOU-CNT        PIC  9(07).
     03  FKU-CNT        PIC  9(07).
     03  SEU-CNT        PIC  9(07).
     03  OKU-CNT        PIC  9(07).
     03  HKU-CNT        PIC  9(07).
     03  OSU-CNT        PIC  9(07).
     03  YB1-CNT        PIC  9(07).
     03  YB2-CNT        PIC  9(07).
     03  YB3-CNT        PIC  9(07).
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
     03  YB1-ST         PIC  X(02).
     03  YB2-ST         PIC  X(02).
     03  YB3-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SJH4559L".
 01  WK-MSG1           PIC  N(14)
                       VALUE NC"発注ＤＴ有り。連絡必要です。".
 01  WK-MSG2           PIC  N(14)
                       VALUE NC"振分不可能データです。確認！".
 01  WK-MSG3           PIC  N(14)
                       VALUE NC"本日の発注ＤＴはありません。".
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
         NC"※※　イオンＧ　発注ＤＴ振分リスト　※※"
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
     03  FILLER                  PIC  N(07)  VALUE
                                 NC"支店／営業所名"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(14)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"振分件数"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
*
 01  HD3                         CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(50)  VALUE  SPACE.
     03  HD3-01                  PIC  N(06).
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
     03  FILLER                  PIC  X(41)  VALUE  SPACE.
     03  DT1-01                  PIC  N(06).
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  DT1-02                  PIC  Z,ZZZ,ZZ9.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-03                  PIC  N(14).
 01  DT2                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  DT2-01                  PIC  N(14).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-KBN                PIC 9(01).
****************************************************************
 PROCEDURE              DIVISION USING PARA-KBN.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 HOUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HOU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4559L HOU ERROR " HOU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 FKUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      FKU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4559L FKU ERROR " FKU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 SEUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SEU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4559L SEU ERROR " SEU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HKUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HKU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4559L SEU ERROR " SEU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 OSUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      OSU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4559L OSU ERROR " OSU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 YB1ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      YOBI1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4559L YB1 ERROR " YB1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 YB2ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      YOBI2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4559L YB2 ERROR " YB2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 YB3ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      YOBI3.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4559L YB3 ERROR " YB3-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4559L PRTFILE ERROR " PRT-ST " "
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
     DISPLAY  "*** SJH4559L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HOU FKU OKU SEU HKU OSU YOBI1 YOBI2 YOBI3.
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
*東日本第二
*    発注件数カウント
     PERFORM HOU-RD-SEC  UNTIL  HOU-FLG = "END".
*    帳票エリアセット
     IF   HOU-CNT  >  ZERO
          MOVE NC"東日本第二　"  TO  DT1-01
          MOVE    HOU-CNT        TO  DT1-02
          MOVE    SPACE          TO  DT1-03
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*九州支店
*    発注件数カウント
     PERFORM FKU-RD-SEC  UNTIL  FKU-FLG = "END".
*    帳票エリアセット
     IF   FKU-CNT > ZERO
          MOVE NC"九州支店　　"  TO  DT1-01
          MOVE    FKU-CNT        TO  DT1-02
          MOVE    WK-MSG1        TO  DT1-03
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*東日本仙台
*    発注件数カウント
     PERFORM SEU-RD-SEC  UNTIL  SEU-FLG = "END".
*    帳票エリアセット
     IF   SEU-CNT > ZERO
          MOVE NC"東日本仙台　"  TO  DT1-01
          MOVE    SEU-CNT        TO  DT1-02
          MOVE    WK-MSG1        TO  DT1-03
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*西日本岡山
*    発注件数カウント
     PERFORM OKU-RD-SEC  UNTIL  OKU-FLG = "END".
*    帳票エリアセット
     IF   OKU-CNT > ZERO
          MOVE NC"西日本岡山　"  TO  DT1-01
          MOVE    OKU-CNT        TO  DT1-02
          MOVE    WK-MSG1        TO  DT1-03
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*北海道支店
*    発注件数カウント
     PERFORM HKU-RD-SEC  UNTIL  HKU-FLG = "END".
*    帳票エリアセット
     IF   HKU-CNT > ZERO
          MOVE NC"北海道支店　"  TO  DT1-01
          MOVE    HKU-CNT        TO  DT1-02
          MOVE    WK-MSG1        TO  DT1-03
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*西日本支店
*    発注件数カウント
     PERFORM OSU-RD-SEC  UNTIL  OSU-FLG = "END".
*    帳票エリアセット
     IF   OSU-CNT > ZERO
          MOVE NC"西日本支店　"  TO  DT1-01
          MOVE    OSU-CNT        TO  DT1-02
          MOVE    WK-MSG1        TO  DT1-03
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*予備１
*    発注件数カウント
     PERFORM YB1-RD-SEC  UNTIL  YB1-FLG = "END".
*    帳票エリアセット
     IF   YB1-CNT > ZERO
          MOVE NC"予備１　　　"  TO  DT1-01
          MOVE    YB1-CNT        TO  DT1-02
          MOVE    WK-MSG1        TO  DT1-03
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*予備２
*    発注件数カウント
     PERFORM YB2-RD-SEC  UNTIL  YB2-FLG = "END".
*    帳票エリアセット
     IF   YB2-CNT > ZERO
          MOVE NC"予備２　　　"  TO  DT1-01
          MOVE    YB2-CNT        TO  DT1-02
          MOVE    WK-MSG1        TO  DT1-03
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*予備３
*    発注件数カウント
     PERFORM YB3-RD-SEC  UNTIL  YB3-FLG = "END".
*    帳票エリアセット
     IF   YB3-CNT > ZERO
          MOVE NC"上記部門以外"  TO  DT1-01
          MOVE    YB3-CNT        TO  DT1-02
          MOVE    WK-MSG1        TO  DT1-03
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*    件数チェック
     IF   CHK-FLG = SPACE
          MOVE    WK-MSG3        TO  DT2-01
          WRITE   PRT-REC   FROM   DT2    AFTER  5
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HOU FKU SEU OKU HKU OSU PRTFILE YOBI1 YOBI2 YOBI3.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SJH4559L END *** "
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
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 YB1-RD-SEC             SECTION.
     READ   YOBI1 AT  END
            MOVE  "END"  TO  YB1-FLG
            NOT   AT  END
            ADD    1     TO  YB1-CNT
     END-READ.
 YB1-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 YB2-RD-SEC             SECTION.
     READ   YOBI2 AT  END
            MOVE  "END"  TO  YB2-FLG
            NOT   AT  END
            ADD    1     TO  YB2-CNT
     END-READ.
 YB2-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 YB3-RD-SEC             SECTION.
     READ   YOBI3 AT  END
            MOVE  "END"  TO  YB3-FLG
            NOT   AT  END
            ADD    1     TO  YB3-CNT
     END-READ.
 YB3-RD-EXIT.
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
*ジャスコ／イオン
     EVALUATE PARA-KBN
         WHEN 1      MOVE NC"（ジャスコ）" TO      HD3-01
         WHEN 2      MOVE NC"（イオン　）" TO      HD3-01
         WHEN OTHER  MOVE NC"（＊＊＊＊）" TO      HD3-01
     END-EVALUATE.
***  ページ_
     MOVE     1                   TO        HD1-04.
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  3.
     WRITE    PRT-REC      FROM   HD3       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  2.
     WRITE    PRT-REC      FROM   HD2       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
 HEAD-WT-EXIT.
     EXIT.
