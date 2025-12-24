# NKE1050B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE1050B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品　　　　                  *
*　　モジュール名　　　　：　出荷検品結果チェック　　　　　　　*
*　　　　　　　　　　　　　　（カインズＴＣ１）　　　　　　　　*
*　　作成日／作成者　　　：　2019/06/28  NAV                   *
*　　処理概要　　　　　　：　データの整合性をチェックする。　　*
*　　　　　　　　　　　　　　後続処理（更新・出荷ＭＳＧ作成等）*
*　　　　　　　　　　　　　　の続行判断のため。　　　　　　　  *
*　　更新日／更新者　　　：　                                  *
*　　処理概要　　　　　　：　　　　　　　　                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE1050B.
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
*----<< 検品結果集計Ｆ >>--*
     SELECT   CZSUMXX1  ASSIGN         DA-01-VI-CZSUMXX1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   SUM-F01
                                                 SUM-F02
                                                 SUM-F03
                                                 SUM-F04
                                                 SUM-F05
                                                 SUM-F06
                                                 SUM-F07
                                                 SUM-F08
                                                 SUM-F09
                                                 SUM-F10
                                                 SUM-F11
                        FILE      STATUS    IS   SUM-ST.
*---<<  在庫マスタ  >>---*
     SELECT   ZAMZAIL1  ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-ST.
*----<< 売上伝票ファイル >>--*
     SELECT   SHTDENLA  ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY  DEN-F46   DEN-F47
                                       DEN-F01   DEN-F48
                                       DEN-F02   DEN-F04
                                       DEN-F051  DEN-F07
                                       DEN-F112  DEN-F03
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
                        FILE      STATUS    IS   HAC-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 検品結果集計Ｆ >>--*
 FD  CZSUMXX1            LABEL RECORD   IS   STANDARD
     BLOCK               CONTAINS       34   RECORDS.
     COPY        CZSUMF      OF      XFDLIB
                 JOINING     SUM     PREFIX.
*---<<  在庫マスタ  >>---*
 FD  ZAMZAIL1.
     COPY        ZAMZAIL1    OF      XFDLIB
                 JOINING     ZAI     PREFIX.
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
     03  DEN-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  ZAI-INV-FLG    PIC  X(03)  VALUE  SPACE.
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
     03  RW-CNT         PIC  9(07).
     03  ERR-CNT        PIC  9(07).
     03  HAC-RWT-CNT    PIC  9(07).
*
*----<< ｴﾗｰﾒｯｾｰｼﾞ >>--*
 01  ERR-TBL.
   03    FILLER         PIC  N(14)     VALUE
                        NC"　　　　　　　　　　　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"売上伝票ファイル未登録　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"ＢＭＳ発注ＭＳＧ未登録　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"在庫マスタ未登録　　　　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"　　　　　　　　　　　　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"検品数量＞発注数量　エラー　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"検品数量が０以下です。確認！".
 01  ERR-TBL-R          REDEFINES  ERR-TBL.
   03  ERR-MSG          PIC  N(14)     OCCURS   7.
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
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SUM-ST             PIC  X(02).
 01  ZAI-ST             PIC  X(02).
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
*LINKAGE                SECTION.
*01  PARA-IN-BUMCD               PIC  X(04).
*01  PARA-IN-TANCD               PIC  X(02).
*01  PARA-IN-SOKCD               PIC  X(02).
*01  PARA-IN-TDATE               PIC  9(08).
*01  PARA-IN-TTIME               PIC  9(06).
*01  PARA-OUT-KENSU              PIC  9(07).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 検品結果集計Ｆ >>--*
 SUM-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CZSUMXX1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1050B CZSUMXX1    ERROR " SUM-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 売上伝票ファイル >>--*
 SHTDENLA-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENLA.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1050B SHTDENLA   ERROR " DEN-ST " "
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
     DISPLAY  "### NKE1050B BMSHACL1    ERROR " HAC-ST    " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 在庫マスタ >>--*
 ZAMZAIL1-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZAMZAIL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1050B ZAMZAIL1    ERROR " ZAI-ST " "
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
*T↓
*    DISPLAY "INIT-RTN" UPON CONS.
*T↑
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE1050B START *** "
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
     OPEN     INPUT     ZAMZAIL1 SHTDENLA BMSHACL1.
     OPEN     I-O       CZSUMXX1.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*
     PERFORM  SUM-READ-SEC.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*T↓
*    DISPLAY "MAIN-RTN" UPON CONS.
*T↑
*
*売上伝票データ存在チェック
     INITIALIZE                   DEN-REC.
     MOVE     SUM-F01        TO   DEN-F46.
     MOVE     SUM-F02        TO   DEN-F47.
     MOVE     SUM-F03        TO   DEN-F01.
     MOVE     SUM-F04        TO   DEN-F48.
     MOVE     SUM-F10        TO   DEN-F02.
     MOVE     0              TO   DEN-F04.
     MOVE     40             TO   DEN-F051.
     MOVE     SUM-F08        TO   DEN-F07.
     MOVE     SUM-F07        TO   DEN-F112.
     MOVE     SUM-F11        TO   DEN-F03.
*T↓
*    DISPLAY "DEN-F46     =" DEN-F46     UPON CONS.
*    DISPLAY "DEN-F47     =" DEN-F47     UPON CONS.
*    DISPLAY "DEN-F01     =" DEN-F01     UPON CONS.
*    DISPLAY "DEN-F048    =" DEN-F048    UPON CONS.
*    DISPLAY "DEN-F02     =" DEN-F02     UPON CONS.
*    DISPLAY "DEN-F04     =" DEN-F04     UPON CONS.
*    DISPLAY "DEN-F051    =" DEN-F051    UPON CONS.
*    DISPLAY "DEN-F07     =" DEN-F07     UPON CONS.
*    DISPLAY "DEN-F112    =" DEN-F112    UPON CONS.
*    DISPLAY "DEN-F03     =" DEN-F03     UPON CONS.
*T↑
     PERFORM  DEN-FIND-SEC.
*T↓
*    DISPLAY "DEN-INV-FLG =" DEN-INV-FLG UPON CONS.
*T↑
*    発注数量と検品数量の比較
     IF       DEN-INV-FLG  = "HIT"
              IF       DEN-F15  NOT = SUM-F16
                       MOVE   "ERR"   TO   ERR-FLG-4
                       MOVE      6    TO   CHK-FLG
              END-IF
     END-IF.
*
*ＢＭＳ発注ＭＳＧ存在チェック
     MOVE     SPACE          TO   HAC-REC.
     INITIALIZE                   HAC-REC.
     MOVE     SUM-F01        TO   HAC-F011.
     MOVE     SUM-F02        TO   HAC-F012.
     MOVE     SUM-F03        TO   HAC-F013.
     MOVE     SUM-F04        TO   HAC-F02.
     MOVE     SUM-F08        TO   HAC-F308(1:5).
     MOVE     SUM-F07        TO   HAC-F346.
     MOVE     SUM-F10        TO   HAC-F302(1:9).
     MOVE     SUM-F11        TO   HAC-F402(1:2).
*T↓
*    DISPLAY "HAC-F011    = " HAC-F011    UPON CONS.
*    DISPLAY "HAC-F012    = " HAC-F012    UPON CONS.
*    DISPLAY "HAC-F013    = " HAC-F013    UPON CONS.
*    DISPLAY "HAC-F02     = " HAC-F02     UPON CONS.
*    DISPLAY "HAC-F308    = " HAC-F308    UPON CONS.
*    DISPLAY "HAC-F346    = " HAC-F346    UPON CONS.
*    DISPLAY "HAC-F302    = " HAC-F302    UPON CONS.
*    DISPLAY "HAC-F402    = " HAC-F402    UPON CONS.
*T↑
     PERFORM  HAC-FIND-SEC.
*T↓
*    DISPLAY "HAC-INV-FLG = " ZAI-INV-FLG UPON CONS.
*T↑
*
*在庫マスタ存在チェック
     INITIALIZE                   ZAI-REC.
     MOVE     SUM-F04        TO   ZAI-F01.
     MOVE     SUM-F12        TO   ZAI-F02.
     MOVE     SUM-F14        TO   ZAI-F03.
     PERFORM  ZAI-FIND-SEC.
*T↓
*    DISPLAY "ZAI-INV-FLG = " ZAI-INV-FLG UPON CONS.
*T↑
*
 MAIN-01.
     PERFORM  SUM-REWRITE-SEC.
*
 MAIN-02.
     INITIALIZE      FLAGS.
     PERFORM  SUM-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*T↓
*    DISPLAY "END-RTN" UPON CONS.
*T↑
*
     CLOSE    CZSUMXX1   SHTDENLA   BMSHACL1   ZAMZAIL1.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ CHECK DATA =" RW-CNT  " +++" UPON CONS.
     DISPLAY  "+++ ERROR DATA =" ERR-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE1050B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　インプットデータ読　　　　　　　　　　　　　　　　　　*
****************************************************************
 SUM-READ-SEC           SECTION.
*T↓
*    DISPLAY "------------"  UPON CONS.
*    DISPLAY "SUM-READ-SEC"  UPON CONS.
*T↑
*
     READ     CZSUMXX1
        AT    END
              MOVE      1    TO   END-FLG
              GO   TO   SUM-READ-EXIT
        NOT AT END
              ADD       1    TO   IN-CNT
*T↓
*             DISPLAY "IN-CNT=" IN-CNT UPON CONS
*T↑
     END-READ.
*
*    検品数がマイナスはエラー
     IF    SUM-F16  <= ZERO
           MOVE   "ERR"     TO   ERR-FLG-5
           MOVE      6      TO   CHK-FLG
     END-IF.
*
 SUM-READ-EXIT.
     EXIT.
****************************************************************
*　　　　売上伝票ファイル存在チェック　　　　　　　　　　　　　*
****************************************************************
 DEN-FIND-SEC           SECTION.
*T↓
*    DISPLAY "DEN-FIND-SEC"  UPON CONS.
*T↑
*
     READ     SHTDENLA
        INVALID
              MOVE   "INV"   TO   DEN-INV-FLG
              MOVE   "ERR"   TO   ERR-FLG-1
*             MOVE     2     TO   CHK-FLG
        NOT INVALID
              MOVE   "HIT"   TO   DEN-INV-FLG
              MOVE   "   "   TO   ERR-FLG-1
*             MOVE     0     TO   CHK-FLG
     END-READ.
*
 DEN-FIND-EXIT.
     EXIT.
****************************************************************
*　　　　在庫マスタ存在チェック　　　　　　　　　　　　　　　　*
****************************************************************
 ZAI-FIND-SEC     SECTION.
*T↓
*    DISPLAY "ZAI-FIND-SEC" UPON CONS.
*T↑
*    在庫マスタ参照
*****DISPLAY "ZAI-F01  = " ZAI-F01  UPON CONS.
*****DISPLAY "ZAI-F021 = " ZAI-F021 UPON CONS.
*****DISPLAY "ZAI-F022 = " ZAI-F022 UPON CONS.
*****DISPLAY "ZAI-F03  = " ZAI-F03  UPON CONS.
     READ     ZAMZAIL1
        INVALID
              MOVE   "INV"   TO   ZAI-INV-FLG
              MOVE   "ERR"   TO   ERR-FLG-2
*             MOVE     4     TO   CHK-FLG
        NOT INVALID
              MOVE   "HIT"   TO   ZAI-INV-FLG
              MOVE   "   "   TO   ERR-FLG-2
*             MOVE     0     TO   CHK-FLG
     END-READ.
*
 ZAI-FIND-EXIT.
     EXIT.
****************************************************************
*　　　　ＢＭＳ発注ＭＳＧ存在チェック　　　　　　　　　　　　　*
****************************************************************
 HAC-FIND-SEC           SECTION.
*T↓
*    DISPLAY "HAC-FIND-SEC"  UPON CONS.
*T↑
*
     READ     BMSHACL1
        INVALID
              MOVE   "INV"   TO   HAC-INV-FLG
              MOVE   "ERR"   TO   ERR-FLG-3
*             MOVE     3     TO   CHK-FLG
        NOT INVALID
              MOVE   "HIT"   TO   HAC-INV-FLG
              MOVE   "   "   TO   ERR-FLG-3
*             MOVE     0     TO   CHK-FLG
     END-READ.
*
 HAC-FIND-EXIT.
     EXIT.
****************************************************************
*    　　検品結果集計Ｆ更新　　　　　　　　　　　　　　　　　　*
****************************************************************
 SUM-REWRITE-SEC      SECTION.
*T↓
*    DISPLAY  "SUM-REWRITE-SEC"  UPON CONS.
*T↑
*項目セット
     MOVE     "0"            TO   SUM-F17.
*エラー内容０１
     IF       ERR-FLG-1  =  "ERR"
              MOVE      "1"  TO   SUM-F17
                                  SUM-F18
     ELSE
              MOVE      " "  TO   SUM-F18
     END-IF.
*エラー内容０２
     IF       ERR-FLG-2  =  "ERR"
              MOVE      "1"  TO   SUM-F17
                                  SUM-F19
     ELSE
              MOVE      " "  TO   SUM-F19
     END-IF.
*エラー内容０３
     IF       ERR-FLG-3  =  "ERR"
              MOVE      "1"  TO   SUM-F17
                                  SUM-F20
     ELSE
              MOVE      " "  TO   SUM-F20
     END-IF.
*エラー内容０４
     IF       ERR-FLG-4  =  "ERR"
              MOVE      "1"  TO   SUM-F17
                                  SUM-F21
     ELSE
              MOVE      " "  TO   SUM-F21
     END-IF.
*エラー内容０５
     IF       ERR-FLG-5  =  "ERR"
              MOVE      "1"  TO   SUM-F17
                                  SUM-F22
     ELSE
              MOVE      " "  TO   SUM-F22
     END-IF.
*エラー内容０６
     IF       ERR-FLG-6  =  "ERR"
              MOVE      "1"  TO   SUM-F17
                                  SUM-F23
     ELSE
              MOVE      " "  TO   SUM-F23
     END-IF.
*エラー内容０７
     IF       ERR-FLG-7  =  "ERR"
              MOVE      "1"  TO   SUM-F17
                                  SUM-F24
     ELSE
              MOVE      " "  TO   SUM-F24
     END-IF.
*エラー内容０８
     IF       ERR-FLG-8  =  "ERR"
              MOVE      "1"  TO   SUM-F17
                                  SUM-F25
     ELSE
              MOVE      " "  TO   SUM-F25
     END-IF.
*エラー内容０９
     IF       ERR-FLG-9  =  "ERR"
              MOVE      "1"  TO   SUM-F17
                                  SUM-F26
     ELSE
              MOVE      " "  TO   SUM-F26
     END-IF.
*エラー内容１０
     IF       ERR-FLG-10 =  "ERR"
              MOVE      "1"  TO   SUM-F17
                                  SUM-F27
     ELSE
              MOVE      " "  TO   SUM-F27
     END-IF.
*エラー件数カウント
     IF       SUM-F17    =  "1"
              ADD        1   TO   ERR-CNT
*T↓
*             DISPLAY "ERR-CNT = " ERR-CNT UPON CONS
*T↑
     END-IF.
*T↓
*             DISPLAY "ERR-FLG-1 = " ERR-FLG-1 UPON CONS
*             DISPLAY "ERR-FLG-2 = " ERR-FLG-2 UPON CONS
*             DISPLAY "ERR-FLG-3 = " ERR-FLG-3 UPON CONS
*             DISPLAY "ERR-FLG-4 = " ERR-FLG-4 UPON CONS
*             DISPLAY "ERR-FLG-5 = " ERR-FLG-5 UPON CONS
*T↑
*
     REWRITE  SUM-REC.
     ADD      1               TO  RW-CNT.
*T↓
*    DISPLAY "RW-CNT      = " RW-CNT UPON CONS.
*T↑
*
 SUM-REWRITE-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
