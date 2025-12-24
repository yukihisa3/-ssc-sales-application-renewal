****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品　　　　                  *
*　　モジュール名　　　　：　カインズ検品出荷場所一括変更　　　*
*　　作成日／作成者　　　：　2023/10/03  NAV                   *
*　　処理概要　　　　　　：　ＴＫへ出荷指示されたデータの振分　*
*　　　　　　　　　　　　　　倉庫の一括変更を行なう。　　　　　*
*　　更新日／更新者　　　：　                                  *
*　　処理概要　　　　　　：　　　　　　　　                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE9000B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/06/28.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
         YA        IS   CHR-2
         YB-21     IS   CHR-21
         YB        IS   CHR-15.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  店舗マスタ  >>---*
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F52
                                                 TEN-F011
                        FILE      STATUS    IS   TEN-ST.
*---<<  条件ファイル>>---*
     SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-ST.
*----<< 売上伝票ファイル >>--*
     SELECT   SHTDENLA  ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY  DEN-F46   DEN-F47
                                       DEN-F01   DEN-F48
                                       DEN-F02   DEN-F04
                                       DEN-F051  DEN-F07
                                       DEN-F112  DEN-F03
                                       WITH DUPLICATES
                        FILE      STATUS    IS   DEN-ST.
*---<<  ＢＭＳ発注ＭＳＧ  >>---*
     SELECT   BMSHACL1  ASSIGN    TO        DA-01-VI-BMSHACL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   HAC-F011
                                                 HAC-F012
                                                 HAC-F013
                                                 HAC-F02
                                                 HAC-F308
                                                 HAC-F346
                                                 HAC-F302
                                                 HAC-F402
                                  WITH DUPLICATES
                        FILE      STATUS    IS   HAC-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*---<<  店舗マスタ  >>---*
 FD  TENMS1.
     COPY        TENMS1      OF      XFDLIB
                 JOINING     TEN     PREFIX.
*---<<  条件ファイル>>---*
 FD  JYOKEN1.
     COPY        JYOKEN1     OF      XFDLIB
                 JOINING     JYO     PREFIX.
*----<< 売上伝票ファイル >>--*
 FD  SHTDENLA    LABEL       RECORD  IS   STANDARD.
     COPY        SHTDENLA    OF      XFDLIB
                 JOINING     DEN     PREFIX.
*---<<  ＢＭＳ発注ＭＳＧ  >>---*
 FD  BMSHACL1.
     COPY        BMSHACL1     OF      XFDLIB
                 JOINING     HAC     PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  CHK-FLG        PIC  9(01).
     03  HAC-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  TEN-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  JYO-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-1      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-2      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-3      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-4      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-5      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-6      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-7      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-8      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-9      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-10     PIC  X(03)  VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(07).
     03  RW-CNT1        PIC  9(07).
     03  RW-CNT2        PIC  9(07).
     03  SKIP-CNT       PIC  9(07).
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-GOKEI           PIC  9(10)     VALUE  ZERO.
 01  WK-DEN-F27D        PIC  9(01)     VALUE  ZERO.
 01  ID                 PIC  9(01)     VALUE  ZERO.
 01  WK-DATE            PIC  9(08).
 01  WK-DATE-R          REDEFINES      WK-DATE.
     03  WK-DATE-R1     PIC  X(04).
     03  WK-DATE-R2     PIC  X(02).
     03  WK-DATE-R3     PIC  X(02).
 01  WK-SUM-F10         PIC  S9(9)V99  VALUE  ZERO.
 01  WK-SUM-F12         PIC  S9(9)V99  VALUE  ZERO.
 01  WK-SURYO           PIC  S9(9)V99  VALUE  ZERO.
 01  P-CNT              PIC  9(03)     VALUE  ZERO.
 01  L-CNT              PIC  9(02)     VALUE  99.
 01  HEN-DATE           PIC  9(08)     VALUE ZERO.
 01  WK-CENTER-CD       PIC  9(05)     VALUE ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  TEN-ST             PIC  X(02).
 01  JYO-ST             PIC  X(02).
 01  DEN-ST             PIC  X(02).
 01  HAC-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-HIDUKE                 PIC  9(08).
 01  PARA-JIKAN                  PIC  9(04).
 01  PARA-TOKCD                  PIC  9(08).
 01  PARA-SOKCD                  PIC  X(02).
 01  PARA-TENST                  PIC  9(05).
 01  PARA-TENED                  PIC  9(05).
 01  PARA-CETST                  PIC  9(05).
 01  PARA-CETED                  PIC  9(05).
 01  PARA-BUMST                  PIC  X(04).
 01  PARA-BUMED                  PIC  X(04).
 01  PARA-NOUST                  PIC  9(08).
 01  PARA-NOUED                  PIC  9(08).
 01  PARA-HEN-SOKCD              PIC  X(02).
*
****************************************************************
 PROCEDURE              DIVISION USING  PARA-HIDUKE
                                        PARA-JIKAN
                                        PARA-TOKCD
                                        PARA-SOKCD
                                        PARA-TENST
                                        PARA-TENED
                                        PARA-CETST
                                        PARA-CETED
                                        PARA-BUMST
                                        PARA-BUMED
                                        PARA-NOUST
                                        PARA-NOUED
                                        PARA-HEN-SOKCD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 売上伝票ファイル >>--*
 SHTDENLA-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENLA.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE9000B SHTDENLA   ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< ＢＭＳ発注ＭＳＧ >>--*
 BMSHACL1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      BMSHACL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE9000B BMSHACL1    ERROR " HAC-ST    " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 TENMS1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TENMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE9000B TEMMS1      ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 検品結果集計Ｆ >>--*
 JYOKEN1-ERR          SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JYOKEN1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE9000B JYOKEN1     ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PROG-CNTL              SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     END-FLG   =    1.
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE9000B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   HEN-DATE.
     OPEN     INPUT     TENMS1   JYOKEN1.
     OPEN     I-O       SHTDENLA  BMSHACL1.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*
*売上伝票データ存在チェック
     INITIALIZE                   DEN-REC.
     MOVE     PARA-HIDUKE    TO   DEN-F46.
     MOVE     PARA-JIKAN     TO   DEN-F47.
     MOVE     PARA-TOKCD     TO   DEN-F01.
     MOVE     PARA-SOKCD     TO   DEN-F48.
     MOVE     ZERO           TO   DEN-F02.
     MOVE     0              TO   DEN-F04.
     MOVE     40             TO   DEN-F051.
     MOVE     ZERO           TO   DEN-F07.
     MOVE     ZERO           TO   DEN-F112.
     MOVE     ZERO           TO   DEN-F03.
*****DISPLAY "DEN-F46  = "  DEN-F46      UPON  CONS.
*    DISPLAY "DEN-F47  = "  DEN-F47      UPON  CONS.
*    DISPLAY "DEN-F01  = "  DEN-F01      UPON  CONS.
*    DISPLAY "DEN-F48  = "  DEN-F48      UPON  CONS.
*    DISPLAY "DEN-F02  = "  DEN-F02      UPON  CONS.
*    DISPLAY "DEN-F04  = "  DEN-F04      UPON  CONS.
*    DISPLAY "DEN-F051 = "  DEN-F051     UPON  CONS.
*    DISPLAY "DEN-F07  = "  DEN-F07      UPON  CONS.
*    DISPLAY "DEN-F112 = "  DEN-F112     UPON  CONS.
*****DISPLAY "DEN-F03  = "  DEN-F03      UPON  CONS.
     START  SHTDENLA  KEY  IS  >=  DEN-F46  DEN-F47  DEN-F01
                                   DEN-F48  DEN-F02  DEN-F04
                                   DEN-F051 DEN-F07  DEN-F112
                                   DEN-F03
            INVALID
            DISPLAY NC"＃＃対象データ無し１＃＃" UPON CONS
            MOVE    1        TO    END-FLG
            GO               TO    INIT-RTN-EXIT
     END-START.
     PERFORM  DEN-READ-SEC.
     IF  END-FLG  =  1
         DISPLAY NC"＃＃対象データ無し２＃＃" UPON CONS
     END-IF.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*
*ＢＭＳ発注ＭＳＧ存在チェック
     MOVE     SPACE          TO   HAC-REC.
     INITIALIZE                   HAC-REC.
     MOVE     DEN-F46        TO   HAC-F011.
     MOVE     DEN-F47        TO   HAC-F012.
     MOVE     DEN-F01        TO   HAC-F013.
     MOVE     DEN-F48        TO   HAC-F02.
     MOVE     DEN-F07        TO   HAC-F308(1:5).
     MOVE     DEN-F112       TO   HAC-F346.
     MOVE     DEN-F02        TO   HAC-F302(1:9).
     MOVE     DEN-F03        TO   HAC-F402(1:2).
*
     READ     BMSHACL1
        INVALID
              MOVE   "INV"   TO   HAC-INV-FLG
        NOT INVALID
              MOVE   SPACE   TO   HAC-INV-FLG
     END-READ.
*    BMSHACFが存在する場合
     IF  HAC-INV-FLG  =  "INV"
         ADD    1            TO   SKIP-CNT
         GO                  TO   MAIN-010
     END-IF.
*出荷場所を変更する
     MOVE    PARA-HEN-SOKCD  TO   DEN-F48.
     REWRITE  DEN-REC.
     ADD     1               TO   RW-CNT1.
     MOVE    PARA-HEN-SOKCD  TO   HAC-F02.
     REWRITE  HAC-REC.
     ADD      1              TO   RW-CNT2.
*
 MAIN-010.
     PERFORM  DEN-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    SHTDENLA  BMSHACL1  TENMS1  JYOKEN1.
*
     DISPLAY  "+++ INPUT      =" IN-CNT    " +++" UPON CONS.
     DISPLAY  "+++ SKIP-CNT   =" SKIP-CNT  " +++" UPON CONS.
     DISPLAY  "+++ DEN-RW-CNT =" RW-CNT1   " +++" UPON CONS.
     DISPLAY  "+++ HAC-RW-CNT =" RW-CNT2   " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE9000B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　売上伝票ファイルチェック　　　　　　　　　　　　　　　*
****************************************************************
 DEN-READ-SEC           SECTION.
*
     READ     SHTDENLA
              NEXT  AT  END
                   MOVE      1    TO   END-FLG
                   GO             TO   DEN-READ-EXIT
              NOT   AT  END
                   ADD       1    TO   IN-CNT
     END-READ.
*
     IF  IN-CNT(5:3)  =  "000"  OR  "500"
         DISPLAY "## READ-CNT = " IN-CNT  " ##"  UPON CONS
     END-IF.
*    バッチNOチェック
 DEN-010.
*****DISPLAY "DEN-F46 = " DEN-F46 " - " PARA-HIDUKE     UPON CONS.
*    DISPLAY "DEN-F47 = " DEN-F47 " - " PARA-JIKAN      UPON CONS.
*    DISPLAY "DEN-F01 = " DEN-F01 " - " PARA-TOKCD      UPON CONS.
*****DISPLAY "DEN-F48 = " DEN-F48 " - " PARA-SOKCD      UPON CONS.
     IF    DEN-F46  =  PARA-HIDUKE
     AND   DEN-F47  =  PARA-JIKAN
     AND   DEN-F01  =  PARA-TOKCD
     AND   DEN-F48  =  PARA-SOKCD
           CONTINUE
     ELSE
           MOVE   1         TO   END-FLG
           GO               TO   DEN-READ-EXIT
     END-IF.
*    店舗ＣＤ範囲チェック
 DEN-020.
     IF    DEN-F07  >=  PARA-TENST
     AND   DEN-F07  <=  PARA-TENED
           CONTINUE
     ELSE
           ADD    1         TO   SKIP-CNT
           GO               TO   DEN-READ-SEC
     END-IF.
*　　部門ＣＤ範囲チェック
 DEN-030.
     IF    DEN-F12  >=  PARA-BUMST
     AND   DEN-F12  <=  PARA-BUMED
           CONTINUE
     ELSE
           ADD    1         TO   SKIP-CNT
           GO               TO   DEN-READ-SEC
     END-IF.
*　　納品日範囲チェック
 DEN-030.
     IF    DEN-F112  >=  PARA-NOUST
     AND   DEN-F112  <=  PARA-NOUED
           CONTINUE
     ELSE
           ADD    1         TO   SKIP-CNT
           GO               TO   DEN-READ-SEC
     END-IF.
*　　センターＣＤチェック
 DEN-040.
     PERFORM  TEN-READ-SEC.
     IF    TEN-INV-FLG  =  SPACE
           PERFORM  JYO-READ-SEC
           IF  JYO-INV-FLG  =  SPACE
               MOVE  JYO-F04   TO   WK-CENTER-CD
           ELSE
               ADD    1        TO   SKIP-CNT
               GO              TO   DEN-READ-SEC
           END-IF
     ELSE
           ADD        1        TO   SKIP-CNT
           GO                  TO   DEN-READ-SEC
     END-IF.
*　　センターＣＤチェック
 DEN-050.
*****DISPLAY "WK-CENTER-CD = " WK-CENTER-CD " - " PARA-CETST " - "
*****                          PARA-CETED UPON CONS.
     IF    WK-CENTER-CD  >=  PARA-CETST
     AND   WK-CENTER-CD  <=  PARA-CETED
           CONTINUE
     ELSE
           ADD    1         TO   SKIP-CNT
           GO               TO   DEN-READ-SEC
     END-IF.
*
 DEN-READ-EXIT.
     EXIT.
****************************************************************
*　　　　店舗マスタチェック　　　　　　　　　　　　　　　　　　*
****************************************************************
 TEN-READ-SEC           SECTION.
*
     MOVE     DEN-F01        TO   TEN-F52.
     MOVE     DEN-F07        TO   TEN-F011.
     READ     TENMS1
        INVALID
              MOVE   "INV"   TO   TEN-INV-FLG
        NOT INVALID
              MOVE   SPACE   TO   TEN-INV-FLG
     END-READ.
*
 TEN-READ-EXIT.
     EXIT.
****************************************************************
*　　　　条件ファイルチェック　　　　　　　　　　　　　　　　　*
****************************************************************
 JYO-READ-SEC           SECTION.
*
     MOVE     23             TO   JYO-F01.
     MOVE     TEN-F75        TO   JYO-F02.
     READ     JYOKEN1
        INVALID
              MOVE   "INV"   TO   JYO-INV-FLG
              MOVE   ZERO    TO   WK-CENTER-CD
        NOT INVALID
              MOVE   SPACE   TO   JYO-INV-FLG
              MOVE   JYO-F04 TO   WK-CENTER-CD
     END-READ.
*
 JYO-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
