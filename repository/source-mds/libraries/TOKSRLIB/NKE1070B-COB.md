# NKE1070B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE1070B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品　　　　                  *
*　　モジュール名　　　　：　出荷検品確定データ更新　　　　　　*
*　　　　　　　　　　　　　　（カインズ）　　　　　　　　　　　*
*　　作成日／作成者　　　：　2019/06/24  NAV                   *
*　　処理概要　　　　　　：　検品結果集計ファイルより、売上伝　*
*　　　　　　　　　　　　　　票ファイル、在庫マスタへ実績数値  *
*　　　　　　　　　　　　　　更新を行なう。　　　　　　　　　　*
*　　更新日／更新者　　　：　                                  *
*　　処理概要　　　　　　：　　　　　　　　                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE1070B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/06/24.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
         YA        IS   CHR-2
         YB-21     IS   CHR-21
         YB        IS   CHR-15.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 検品結果集計ファイル >>--*
     SELECT   CZSUML3   ASSIGN              DA-01-VI-CZSUML3
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   SUM-F17
                                                 SUM-F01
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
     SELECT   SHTDENL1  ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY  DEN-F01   DEN-F02
                                       DEN-F04   DEN-F051
                                       DEN-F07   DEN-F112
                                       DEN-F03
                        FILE      STATUS    IS   DEN-ST.
*---<<  商品変換テーブル  >>---*
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F02
                        FILE      STATUS    IS   SHO-ST.
*---<<  商品名称マスタ  >>---*
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 検品結果集計ファイル >>--*
 FD  CZSUML3     LABEL RECORD   IS   STANDARD.
     COPY        CZSUMF      OF      XFDLIB
                 JOINING     SUM     PREFIX.
*---<<  在庫マスタ  >>---*
 FD  ZAMZAIL1.
     COPY        ZAMZAIL1    OF      XFDLIB
                 JOINING     ZAI     PREFIX.
*----<< 売上伝票ファイル >>--*
 FD  SHTDENL1    LABEL       RECORD  IS   STANDARD.
     COPY        SHTDENL1    OF      XFDLIB
                 JOINING     DEN     PREFIX.
*---<<  商品変換テーブル  >>---*
 FD  SHOTBL1.
     COPY        SHOTBL1     OF      XFDLIB
                 JOINING     SHO     PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  MEIMS1.
     COPY     MEIMS1    OF        XFDLIB
              JOINING   MEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)  VALUE  SPACE.
     03  OKNG-FLG       PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG        PIC  X(03)  VALUE  SPACE.
     03  MEI-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  TBL-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  DEN-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  ZAI-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  DEN-UPD-FLG    PIC  X(03)  VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(07).
     03  ERR-CNT        PIC  9(07).
     03  ZAI-OUT-CNT    PIC  9(07).
     03  ZAI-RWT-CNT    PIC  9(07).
     03  DEN-RWT-CNT    PIC  9(07).
*
*----<< ｴﾗｰﾒｯｾｰｼﾞ >>--*
 01  ERR-TBL.
   03    FILLER         PIC  N(14)     VALUE
                        NC"出荷確定データ　　重複".
   03    FILLER         PIC  N(14)     VALUE
                        NC"売上伝票ファイル未登録　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"商品変換テーブル未登録　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"在庫マスタ未登録　　　　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"商品名称マスタ未登録　　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"出荷数量＞発注数量　エラー　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"出荷数量が０以下です。確認！".
 01  ERR-TBL-R          REDEFINES  ERR-TBL.
   03  ERR-MSG          PIC  N(14)     OCCURS   7.
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-GOKEI           PIC  9(10)     VALUE  ZERO.
 01  WK-DEN-F27D        PIC  9(01)     VALUE  ZERO.
 01  WK-DEN-F15         PIC  9(07)     VALUE  ZERO.
 01  ID                 PIC  9(01)     VALUE  ZERO.
 01  WK-DATE            PIC  9(08).
 01  WK-DATE-R          REDEFINES      WK-DATE.
     03  WK-DATE-R1     PIC  X(04).
     03  WK-DATE-R2     PIC  X(02).
     03  WK-DATE-R3     PIC  X(02).
 01  WK-RCV-F10         PIC  S9(9)V99  VALUE  ZERO.
 01  WK-RCV-F12         PIC  S9(9)V99  VALUE  ZERO.
 01  WK-SURYO           PIC  S9(9)V99  VALUE  ZERO.
 01  P-CNT              PIC  9(03)     VALUE  ZERO.
 01  L-CNT              PIC  9(02)     VALUE  99.
 01  HEN-DATE           PIC  9(08)     VALUE ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SUM-ST             PIC  X(02).
 01  ZAI-ST             PIC  X(02).
 01  DEN-ST             PIC  X(02).
 01  SHO-ST             PIC  X(02).
 01  MEI-ST             PIC  X(02).
 01  FIN-ST             PIC  X(02).
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
 01  WK-RCV-F03.
     03  WK-RCV-F031    PIC  9(04).
     03  WK-RCV-F032    PIC  9(02).
     03  WK-RCV-F033    PIC  9(02).
 01  WK-RCV-F05.
     03  WK-RCV-F051    PIC  9(04).
     03  WK-RCV-F052    PIC  9(02).
     03  WK-RCV-F053    PIC  9(02).
 01  WK-RCV-F05R REDEFINES WK-RCV-F05.
     03  WK-RCV-F05RR   PIC  9(08).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-BUMCD               PIC  X(04).
 01  PARA-IN-TANCD               PIC  X(02).
 01  PARA-IN-SOKCD               PIC  X(02).
 01  PARA-IN-TDATE               PIC  9(08).
 01  PARA-IN-TTIME               PIC  9(06).
 01  PARA-OUT-KENSU              PIC  9(07).
*
****************************************************************
 PROCEDURE              DIVISION USING       PARA-IN-BUMCD
                                             PARA-IN-TANCD
                                             PARA-IN-SOKCD
                                             PARA-IN-TDATE
                                             PARA-IN-TTIME
                                             PARA-OUT-KENSU.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 検品結果集計ファイル >>--*
 CZSUML3-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CZSUML3.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1070B CZSUML3     ERROR " SUM-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 売上伝票ファイル >>--*
 SHTDENL1-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1070B SHTDENL1   ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 商品変換テーブル >>--*
 SHOTBL1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHOTBL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1070B SHOTBL1    ERROR " SHO-ST    " "
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
     DISPLAY  "### NKE1070B ZAMZAIL1    ERROR " ZAI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 商品名称マスタ >>--*
 MEIMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      MEIMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1070B MEIMS1  ERROR " MEI-ST " "
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
     PERFORM  MAIN-RTN   UNTIL     END-FLG   =   "END".
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
*システム日付／時刻取得
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE1070B START *** "
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
     MOVE      LINK-OUT-YMD       TO   HEN-DATE.
*ファイルＯＰＥＮ
     OPEN     INPUT     SHOTBL1  MEIMS1.
     OPEN     I-O       CZSUML3  ZAMZAIL1 SHTDENL1.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*エラー存在チェック（エラーがあるか確認する）
     PERFORM  ERR-CHK-SEC.
*ＮＧの場合は、エラーＭＳＧを出力しＰＧ終了
     IF  OKNG-FLG  =  "NG"
         DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
         DISPLAY NC"＃　エラーデータが存在します　＃" UPON CONS
         DISPLAY NC"＃　処理を終了します。　　　　＃" UPON CONS
         DISPLAY NC"＃　エラー内容を確認し、再度　＃" UPON CONS
         DISPLAY NC"＃　処理を行なって下さい。　　＃" UPON CONS
         DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
         MOVE    "4000"          TO    PROGRAM-STATUS
         MOVE    "END"           TO    END-FLG
         STOP  RUN
     END-IF.
*検品結果集計ＦのＯＰＥＮ／ＣＬＯＳＥ
     CLOSE      CZSUML3.
     OPEN  I-O  CZSUML3.
*
     PERFORM  CZSUML3-READ-SEC.
     IF  END-FLG  =  "END"
         DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
         DISPLAY NC"＃　処理対象のデータが存在し　＃" UPON CONS
         DISPLAY NC"＃　ません。再度、データ内容　＃" UPON CONS
         DISPLAY NC"＃　を確認してください。　　　＃" UPON CONS
         DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
         MOVE    "4000"          TO    PROGRAM-STATUS
         MOVE    "END"           TO    END-FLG
         STOP  RUN
     END-IF.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　エラーレコード有無チェック　　　　　　　　　　　　　　　　*
****************************************************************
 ERR-CHK-SEC            SECTION.
*検品結果集計ファイルスタート
     MOVE         SPACE          TO        SUM-REC.
     INITIALIZE                            SUM-REC.
     MOVE         "1"            TO        SUM-F17.
     START  CZSUML3  KEY  IS  >=  SUM-F17  SUM-F01  SUM-F02
                                  SUM-F03  SUM-F04  SUM-F05
                                  SUM-F06  SUM-F07  SUM-F08
                                  SUM-F09  SUM-F10  SUM-F11
            INVALID
            MOVE  "OK"           TO        OKNG-FLG
            GO                   TO        ERR-CHK-EXIT
     END-START.
*
*****IF  OKNG-FLG  =  "OK"
*        GO                      TO        ERR-CHK-EXIT
*****END-IF.
*検品結果集計ファイル読込
     READ  CZSUML3
           AT   END
            MOVE  "OK"           TO        OKNG-FLG
            GO                   TO        ERR-CHK-EXIT
     END-READ.
*チェック結果判定（１のときは、ＮＧとする。以外はＯＫ）
     IF    SUM-F17  =  "1"
           MOVE   "NG"           TO        OKNG-FLG
***********DISPLAY NC"＃エラーデータが存在します！＃＃" UPON
***********        CONS
     END-IF.
*
 ERR-CHK-EXIT.
     EXIT.
****************************************************************
*　　検品結果集計ファイル読込　　　　　　　　　　　　　　　　　*
****************************************************************
 CZSUML3-READ-SEC       SECTION.
*検品結果集計ファイル読込
     READ  CZSUML3
           AT   END
            MOVE  "END"          TO        END-FLG
            GO                   TO        CZSUML3-READ-EXIT
     END-READ.
*読込件数カウント／件数表示
     ADD    1                    TO        IN-CNT.
     IF  IN-CNT(5:3)  =  "000" OR "500"
         DISPLAY "# READ-CNT = " IN-CNT  " #"  UPON CONS
     END-IF.
*
 CZSUML3-READ-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*売上伝票データを参照
     INITIALIZE                   DEN-REC.
     MOVE     SUM-F03        TO   DEN-F01.
     MOVE     SUM-F10        TO   DEN-F02.
     MOVE     0              TO   DEN-F04.
     MOVE     40             TO   DEN-F051.
     MOVE     SUM-F08        TO   DEN-F07.
     MOVE     SUM-F07        TO   DEN-F112.
     MOVE     SUM-F11        TO   DEN-F03.
*
     PERFORM  DEN-FIND-SEC.
*
*    売上伝票Ｆが存在しない場合
     IF  DEN-INV-FLG  = "INV"
         DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
         DISPLAY NC"＃　更新対象の売上伝票Ｆが存　＃" UPON CONS
         DISPLAY NC"＃　在しません。　　　　　　　＃" UPON CONS
         DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
         DISPLAY "INV KEY 1 = " DEN-F01  UPON CONS
         DISPLAY "INV KEY 2 = " DEN-F02  UPON CONS
         DISPLAY "INV KEY 3 = " DEN-F07  UPON CONS
         DISPLAY "INV KEY 4 = " DEN-F112 UPON CONS
         DISPLAY "INV KEY 5 = " DEN-F03  UPON CONS
         MOVE    "4000"          TO    PROGRAM-STATUS
         MOVE    "END"           TO    END-FLG
         STOP  RUN
     END-IF.
*
*商品変換ＴＢＬを参照
     MOVE     SUM-F03        TO   SHO-F01.
     MOVE     SUM-F13        TO   SHO-F02.
     PERFORM  TBL-FIND-SEC.
*
*商品名称マスタを参照
     MOVE     SUM-F12(1:8)   TO   MEI-F011.
     MOVE     SUM-F12(9:8)   TO   MEI-F012.
     PERFORM  MEI-FIND-SEC.
*    商品名称Ｍが存在しない場合
     IF  MEI-INV-FLG  = "INV"
         DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
         DISPLAY NC"＃　商品名称Ｍがマスタに存在　＃" UPON CONS
         DISPLAY NC"＃　しません。確認して下さい　＃" UPON CONS
         DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
         DISPLAY "INV KEY 1 = " DEN-F01  UPON CONS
         DISPLAY "INV KEY 2 = " DEN-F02  UPON CONS
         DISPLAY "INV KEY 3 = " DEN-F07  UPON CONS
         DISPLAY "INV KEY 4 = " DEN-F112 UPON CONS
         DISPLAY "INV KEY 5 = " DEN-F03  UPON CONS
         DISPLAY "INV SYOCD = " MEI-F011 UPON CONS
         DISPLAY "INV HINTN = " MEI-F012 UPON CONS
         MOVE    "4000"          TO    PROGRAM-STATUS
         MOVE    "END"           TO    END-FLG
         STOP  RUN
     END-IF.
*更新判定（出荷指示数より多かったら対象外）
     IF  SUM-F16  >  DEN-F15
         GO                  TO   MAIN-01
     END-IF.
*売上伝票データを更新
*在庫マスタを参照・更新
     INITIALIZE                   ZAI-REC.
     MOVE     SUM-F04        TO   ZAI-F01.
     MOVE     SUM-F12(1:8)   TO   ZAI-F021.
     MOVE     SUM-F12(9:8)   TO   ZAI-F022.
     IF  TBL-INV-FLG  =  "INV"
         MOVE SPACE          TO   ZAI-F03
     ELSE
         MOVE SHO-F08        TO   ZAI-F03
     END-IF.
     PERFORM  ZAI-FIND-SEC.
*
     MOVE     "   "    TO   DEN-UPD-FLG.
     IF  ZAI-INV-FLG   =  "HIT"
         PERFORM   ZAI-UPD-SEC
     ELSE
*        MOVE     "UPD"    TO   DEN-UPD-FLG
         PERFORM   ZAI-WT-SEC
     END-IF.
*
 MAIN-01.
*
     PERFORM  CZSUML3-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE              SHOTBL1  MEIMS1
                        CZSUML3  ZAMZAIL1 SHTDENL1.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ ZAI_OUTPUT =" ZAI-OUT-CNT " +++" UPON CONS.
     DISPLAY  "+++ ZAI_UPDATE =" ZAI-RWT-CNT " +++" UPON CONS.
     DISPLAY  "+++ DEN_UPDATE =" DEN-RWT-CNT " +++" UPON CONS.
*取込件数セット（パラメタＯＵＴ）
     MOVE     IN-CNT         TO   PARA-OUT-KENSU.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE1070B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　在庫マスタ存在チェック　　　　　　　　　　　　　　　　*
****************************************************************
 ZAI-FIND-SEC     SECTION.
*
     READ     ZAMZAIL1
        INVALID
              MOVE   "INV"   TO   ZAI-INV-FLG
        NOT INVALID
              MOVE   "HIT"   TO   ZAI-INV-FLG
     END-READ.
*
 ZAI-FIND-EXIT.
     EXIT.
****************************************************************
*　　　　在庫マスタ更新　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 ZAI-UPD-SEC      SECTION.
*////売上伝票_発注数量 = 出荷確定_出荷数量 の場合は
*    在庫マスタは更新しない。
     MOVE    ZERO                TO   WK-DEN-F15.
     IF  DEN-F15   =   SUM-F16
*        売上伝票　フラグ更新
         MOVE 1        TO   DEN-F279
         GO            TO   ZAI-UPD-DEN-REWRITE
     ELSE
*        売上伝票　フラグ更新＆出荷数を数量に代入
         MOVE 1        TO   DEN-F279
         MOVE DEN-F15  TO   WK-DEN-F15
         MOVE SUM-F16  TO   DEN-F15
         COMPUTE  DEN-F181  =  SUM-F16  *  DEN-F172
         COMPUTE  DEN-F182  =  SUM-F16  *  DEN-F173
     END-IF.
*////売上伝票の引当フラグが１の場合
*　　　　　　　　　　　　引当済数　－　発注数量
     IF  DEN-F27D  =    1
         COMPUTE  ZAI-F27 =   ZAI-F27 -  WK-DEN-F15
         COMPUTE  ZAI-F28 =   ZAI-F28 -  WK-DEN-F15
     ELSE
         COMPUTE  ZAI-F27 =   ZAI-F27 -  WK-DEN-F15
     END-IF.
*
*  ＜引当判断＞　０以上の場合は引当ＯＫ
     COMPUTE  WK-SURYO       =    ZAI-F04   -    ZAI-F28
                                            -    SUM-F16.
     IF  WK-SURYO  >=   0
*        売上伝票の在庫引当フラグを１　出荷数 ADD 引当済数
         MOVE 1              TO   DEN-F27D
         MOVE HEN-DATE       TO   ZAI-F99
         COMPUTE   ZAI-F27   =    ZAI-F27   +    SUM-F16
         COMPUTE   ZAI-F28   =    ZAI-F28   +    SUM-F16
     ELSE
*        売上伝票の在庫引当フラグを０
         MOVE 0              TO   DEN-F27D
         MOVE HEN-DATE       TO   ZAI-F99
         COMPUTE  ZAI-F27  =  ZAI-F27  +  SUM-F16
     END-IF.
*
*  ＜在庫マスタ・売上伝票Ｆ更新＞
*
     REWRITE  ZAI-REC.
     ADD      1       TO     ZAI-RWT-CNT.
*
 ZAI-UPD-DEN-REWRITE.
*
     REWRITE  DEN-REC.
     MOVE    "UPD"    TO     DEN-UPD-FLG.
     ADD      1       TO     DEN-RWT-CNT.
*     検品結果集計ファイル更新
      MOVE     "1"               TO    SUM-F98.
      REWRITE  SUM-REC.
*
 ZAI-UPD-EXIT.
     EXIT.
****************************************************************
*　　　　在庫マスタ作成　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 ZAI-WT-SEC      SECTION.
*
*////売上伝票_発注数量 = 出荷確定_出荷数量 の場合は
*    在庫マスタは作成しない。
     IF  DEN-F15   =   SUM-F16
         GO   TO   ZAI-WT-010
     END-IF.
*商品在庫マスタ初期化
      MOVE      SPACE         TO   ZAI-REC.
      INITIALIZE                   ZAI-REC.
*商品在庫マスタ項目セット
      MOVE      SUM-F04       TO   ZAI-F01.
      MOVE      SUM-F12(1:8)  TO   ZAI-F021.
      MOVE      SUM-F12(9:8)  TO   ZAI-F022.
      IF  TBL-INV-FLG  =  "INV"
          MOVE SPACE          TO   ZAI-F03
      ELSE
          MOVE SHO-F08        TO   ZAI-F03
      END-IF.
*未出庫数＝未出庫数＋数量
      COMPUTE   ZAI-F27       =    ZAI-F27  +  SUM-F16.
*引当済数＝引当済数＋数量
      COMPUTE   ZAI-F28       =    ZAI-F28  +  SUM-F16.
*商品名称マスタ存在チェック
      IF    MEI-INV-FLG  =  "HIT"
            MOVE  MEI-F031   TO   ZAI-F30
      END-IF.
      MOVE  HEN-DATE         TO   ZAI-F98.
      MOVE  HEN-DATE         TO   ZAI-F99.
      WRITE ZAI-REC.
      ADD   1                TO   ZAI-OUT-CNT.
*     売上伝票の在庫引当フラグを０
      MOVE  0                TO   DEN-F27D.
*
 ZAI-WT-010.
*     売上伝票　フラグ更新＆出荷数を数量に代入
      MOVE  1             TO   DEN-F279.
      MOVE  SUM-F16       TO   DEN-F15.
      COMPUTE  DEN-F181  =  DEN-F172  *  DEN-F15.
      COMPUTE  DEN-F182  =  DEN-F173  *  DEN-F15.
      REWRITE  DEN-REC.
      MOVE    "UPD"              TO    DEN-UPD-FLG.
      ADD      1                 TO    DEN-RWT-CNT.
*     検品結果集計ファイル更新
      MOVE     "1"               TO    SUM-F98.
      REWRITE  SUM-REC.
*
 ZAI-WT-EXIT.
     EXIT.
****************************************************************
*　　　　売上伝票ファイル読　　　　　　　　　　　　　　　　　　*
****************************************************************
 DEN-FIND-SEC           SECTION.
*
     READ     SHTDENL1
        INVALID
              MOVE   "INV"   TO   DEN-INV-FLG
        NOT INVALID
              MOVE   "HIT"   TO   DEN-INV-FLG
     END-READ.
*
 DEN-FIND-EXIT.
     EXIT.
****************************************************************
*　　　　商品変換テーブル読　　　　　　　　　　　　　　　　　　*
****************************************************************
 TBL-FIND-SEC           SECTION.
*
     READ     SHOTBL1
        INVALID
              MOVE   "INV"   TO   TBL-INV-FLG
        NOT INVALID
              MOVE   "HIT"   TO   TBL-INV-FLG
     END-READ.
*
 TBL-FIND-EXIT.
     EXIT.
****************************************************************
*                商品名称マスタ読　　                          *
****************************************************************
 MEI-FIND-SEC           SECTION.
*
     READ      MEIMS1
        INVALID
               MOVE      "INV"    TO    MEI-INV-FLG
        NOT  INVALID
               MOVE      "HIT"    TO    MEI-INV-FLG
     END-READ.
*
 MEI-FIND-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
