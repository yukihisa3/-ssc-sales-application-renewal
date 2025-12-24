# SJH4556L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJH4556L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　イオンオンラインシステム　　　　　*
*    モジュール名　　　　：　受信データ種別毎受信件数リスト　　*
*    作成日／更新日　　　：　2003/06/17                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　受信データ種別毎に件数をカウント  *
*                        ：　し、件数リストを出力する。        *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJH4556L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          03/06/17.
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
*----<<受信データ　　　　　　　　　　>>----*
     SELECT   IONHAC       ASSIGN         DA-01-S-IONHAC
                        ORGANIZATION   SEQUENTIAL
                        STATUS         IONHAC-ST.
*----<<運用情報データ　　　　　　　　>>----*
     SELECT   IONHAC1       ASSIGN         DA-01-S-IONHAC1
                        ORGANIZATION   SEQUENTIAL
                        STATUS         IONHAC1-ST.
*----<<発注情報データ（イオン旧）　　>>----*
     SELECT   IONHAC2       ASSIGN         DA-01-S-IONHAC2
                        ORGANIZATION   SEQUENTIAL
                        STATUS         IONHAC2-ST.
*----<<汎用情報データ　　　　　　　　>>----*
     SELECT   IONHAC3       ASSIGN         DA-01-S-IONHAC3
                        ORGANIZATION   SEQUENTIAL
                        STATUS         IONHAC3-ST.
*----<<支払明細情報データ　　　　　　　>>----*
     SELECT   IONHAC4       ASSIGN         DA-01-S-IONHAC4
                        ORGANIZATION   SEQUENTIAL
                        STATUS         IONHAC4-ST.
*----<<支払案内情報データ　　　　　　>>----*
     SELECT   IONHAC5       ASSIGN         DA-01-S-IONHAC5
                        ORGANIZATION   SEQUENTIAL
                        STATUS         IONHAC5-ST.
*----<<発注情報データ（イオン新）　　>>----*
     SELECT   IONHAC6     ASSIGN         DA-01-S-IONHAC6
                        ORGANIZATION   SEQUENTIAL
                        STATUS         IONHAC6-ST.
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN  TO     LP-04-PRTF
                        FILE    STATUS PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<受信データ　　　　　　　　　　>>----*
 FD  IONHAC
                        BLOCK CONTAINS 1 RECORDS.
 01  IONHAC-REC            PIC  X(256).
*----<<運用情報データ　　　　　　　　>>----*
 FD  IONHAC1
                        BLOCK CONTAINS 1 RECORDS.
 01  IONHAC1-REC            PIC  X(128).
*----<<発注情報データ（イオン旧）　　>>----*
 FD  IONHAC2
                        BLOCK CONTAINS 1 RECORDS.
 01  IONHAC2-REC            PIC  X(256).
*----<<汎用情報データ　　　　　　　　>>----*
 FD  IONHAC3
                        BLOCK CONTAINS 1 RECORDS.
 01  IONHAC3-REC            PIC  X(128).
*----<<支払明細情報データ　　　　　　>>----*
 FD  IONHAC4
                        BLOCK CONTAINS 1 RECORDS.
 01  IONHAC4-REC            PIC  X(128).
*----<<支払案内情報データ　　　　　　>>----*
 FD  IONHAC5
                        BLOCK CONTAINS 1 RECORDS.
 01  IONHAC5-REC            PIC  X(128).
*----<<発注情報データ（イオン新）　　>>----*
 FD  IONHAC6
                        BLOCK CONTAINS 1 RECORDS.
 01  IONHAC6-REC            PIC  X(256).
*----<<プリントファイル　　　　　　　>>----*
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  IONHAC-FLG        PIC  X(03)   VALUE SPACE.
     03  IONHAC1-FLG        PIC  X(03)   VALUE SPACE.
     03  IONHAC2-FLG        PIC  X(03)   VALUE SPACE.
     03  IONHAC3-FLG        PIC  X(03)   VALUE SPACE.
     03  IONHAC4-FLG        PIC  X(03)   VALUE SPACE.
     03  IONHAC5-FLG        PIC  X(03)   VALUE SPACE.
     03  IONHAC6-FLG        PIC  X(03)   VALUE SPACE.
     03  CHK-FLG        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  IONHAC-CNT        PIC  9(07).
     03  IONHAC1-CNT        PIC  9(07).
     03  IONHAC2-CNT        PIC  9(07).
     03  IONHAC3-CNT        PIC  9(07).
     03  IONHAC4-CNT        PIC  9(07).
     03  IONHAC5-CNT        PIC  9(07).
     03  IONHAC6-CNT        PIC  9(07).
     03  URI-CNT        PIC  9(07).
     03  SIR-CNT        PIC  9(07).
     03  GOK-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  IONHAC-ST         PIC  X(02).
     03  IONHAC1-ST         PIC  X(02).
     03  IONHAC2-ST         PIC  X(02).
     03  IONHAC3-ST         PIC  X(02).
     03  IONHAC4-ST         PIC  X(02).
     03  IONHAC5-ST         PIC  X(02).
     03  IONHAC6-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SJH4556L".
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
         NC"※※　イオンＧ　　種別別件数リスト　※※"
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
     03  FILLER                  PIC  X(41)  VALUE  SPACE.
     03  FILLER                  PIC  N(07)  VALUE
                                 NC"種別名称　　　"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"件　　数"
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
     03  FILLER                  PIC  X(55)  VALUE  SPACE.
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
 IONHACERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      IONHAC.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4556L IONHAC ERROR " IONHAC-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 IONHAC1ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      IONHAC1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4556L IONHAC1 ERROR " IONHAC1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 IONHAC2ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      IONHAC2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4556L IONHAC2 ERROR " IONHAC2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 IONHAC3ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      IONHAC3.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4556L IONHAC3 ERROR " IONHAC3-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 IONHAC4ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      IONHAC4.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4556L IONHAC4 ERROR " IONHAC4-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 IONHAC5ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      IONHAC5.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4556L IONHAC5 ERROR " IONHAC5-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 IONHAC6ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      IONHAC6.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4556L IONHAC6 ERROR " IONHAC6-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH4556L PRTFILE ERROR " PRT-ST " "
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
     DISPLAY  "*** SJH4556L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     IONHAC  IONHAC1 IONHAC2 IONHAC3 IONHAC4
                        IONHAC5 IONHAC6.
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
*受信データ
*    発注件数カウント
     PERFORM IONHAC-RD-SEC  UNTIL  IONHAC-FLG = "END".
*    帳票エリアセット
     IF   IONHAC-CNT  >  ZERO
          MOVE NC"受信データ　"  TO  DT1-01
          MOVE    IONHAC-CNT     TO  DT1-02
          MOVE    SPACE          TO  DT1-03
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*運用情報データ
*    発注件数カウント
     PERFORM IONHAC1-RD-SEC  UNTIL  IONHAC1-FLG = "END".
*    帳票エリアセット
     IF   IONHAC1-CNT > ZERO
          MOVE NC"運用情報ＤＴ"  TO  DT1-01
          MOVE    IONHAC1-CNT    TO  DT1-02
          MOVE    SPACE          TO  DT1-03
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*発注情報データ（イオン旧）
*    発注件数カウント
     PERFORM IONHAC2-RD-SEC  UNTIL  IONHAC2-FLG = "END".
*    帳票エリアセット
     IF   IONHAC2-CNT > ZERO
          MOVE NC"発注情報　Ｊ" TO  DT1-01
          MOVE    IONHAC2-CNT   TO  DT1-02
          MOVE    SPACE         TO  DT1-03
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*汎用情報データ
*    発注件数カウント
     PERFORM IONHAC3-RD-SEC  UNTIL  IONHAC3-FLG = "END".
*    帳票エリアセット
     IF   IONHAC3-CNT > ZERO
          MOVE NC"汎用情報ＤＴ"  TO  DT1-01
          MOVE    IONHAC3-CNT    TO  DT1-02
          MOVE    SPACE          TO  DT1-03
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*支払明細情報データ
*    発注件数カウント
     PERFORM IONHAC4-RD-SEC  UNTIL  IONHAC4-FLG = "END".
*    帳票エリアセット
     IF   IONHAC4-CNT > ZERO
          MOVE NC"支明情報ＤＴ"  TO  DT1-01
          MOVE    IONHAC4-CNT    TO  DT1-02
          MOVE    SPACE          TO  DT1-03
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*支払案内情報データ
*    発注件数カウント
     PERFORM IONHAC5-RD-SEC  UNTIL  IONHAC5-FLG = "END".
*    帳票エリアセット
     IF   IONHAC5-CNT > ZERO
          MOVE NC"支案情報ＤＴ"  TO  DT1-01
          MOVE    IONHAC5-CNT    TO  DT1-02
          MOVE    SPACE          TO  DT1-03
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*発注情報データ（イオン新）
*    発注件数カウント
     PERFORM IONHAC6-RD-SEC  UNTIL  IONHAC6-FLG = "END".
*    帳票エリアセット
     IF   IONHAC6-CNT > ZERO
          MOVE NC"発注情報　Ｉ"  TO  DT1-01
          MOVE    IONHAC6-CNT    TO  DT1-02
          MOVE    SPACE          TO  DT1-03
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
     CLOSE IONHAC IONHAC1 IONHAC2 IONHAC3 IONHAC4 IONHAC5 IONHAC6
           PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SJH4556L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 IONHAC-RD-SEC             SECTION.
     READ   IONHAC   AT  END
            MOVE  "END"  TO  IONHAC-FLG
            NOT   AT  END
            ADD    1     TO  IONHAC-CNT
     END-READ.
 IONHAC-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 IONHAC1-RD-SEC             SECTION.
     READ   IONHAC1   AT  END
            MOVE  "END"  TO  IONHAC1-FLG
            NOT   AT  END
            ADD    1     TO  IONHAC1-CNT
     END-READ.
 IONHAC1-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 IONHAC2-RD-SEC             SECTION.
     READ   IONHAC2   AT  END
            MOVE  "END"  TO  IONHAC2-FLG
            NOT   AT  END
            ADD    1     TO  IONHAC2-CNT
     END-READ.
 IONHAC2-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 IONHAC3-RD-SEC             SECTION.
     READ   IONHAC3   AT  END
            MOVE  "END"  TO  IONHAC3-FLG
            NOT   AT  END
            ADD    1     TO  IONHAC3-CNT
     END-READ.
 IONHAC3-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 IONHAC4-RD-SEC             SECTION.
     READ   IONHAC4   AT  END
            MOVE  "END"  TO  IONHAC4-FLG
            NOT   AT  END
            ADD    1     TO  IONHAC4-CNT
     END-READ.
 IONHAC4-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 IONHAC5-RD-SEC             SECTION.
     READ   IONHAC5   AT  END
            MOVE  "END"  TO  IONHAC5-FLG
            NOT   AT  END
            ADD    1     TO  IONHAC5-CNT
     END-READ.
 IONHAC5-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 IONHAC6-RD-SEC             SECTION.
     READ   IONHAC6 AT  END
            MOVE  "END"  TO  IONHAC6-FLG
            NOT   AT  END
            ADD    1     TO  IONHAC6-CNT
     END-READ.
 IONHAC6-RD-EXIT.
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

```
