# SNI0030B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SNI0030B.COB`

## ソースコード

```cobol
****************************************************************
*新基幹システムへ移行します。 1999/10/16 T.TAKAHASHI           *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　売上データ抽出　　　　　　　　　　*
*    作成日／更新日　　　：　93/05/14   (SSKT160ﾖﾘ)            *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　伝票データより，売上データを出力　*
*                        ：　する。　　　　　　　　　　　　　　*
*　　更新日　　　　　　　：  94/08/02                          *
*　　　　　　　　　　　　：　売上計上の対象データを入力日付と  *
*　　　　　　　　　　　　：　出荷日が一致したデータとする　　  *
*　　更新日　　　　　　　：  99/05/28                          *
*　　　　　　　　　　　　：　６月以降の大阪地方の売上を特販部  *
*　　　　　　　　　　　　：　から大阪営業所扱いにする。　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SNI0030B.
 AUTHOR.                T.A.
 DATE-WRITTEN.          93/05/14.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 伝票データ >>--*
     SELECT   HDENJNL   ASSIGN         DA-01-VI-DENJNL4
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F01   DEN-F02
                                       DEN-F04   DEN-F03
                        STATUS         HDENJNL-ST.
*----<< 条件ファイル >>--*
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01   JYO-F02
                        STATUS         HJYOKEN-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         HTENMS-ST.
*----<< 商品名称マスタ >>--*
     SELECT   HMEIMS    ASSIGN         DA-01-VI-MEIMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
*93/02******************RECORD    KEY  MEI-F01
                        RECORD    KEY  MEI-F011
                                       MEI-F0121
                                       MEI-F0122
                                       MEI-F0123
                        STATUS         HMEIMS-ST.
*----<< 商品変換テーブル >>--*
     SELECT   HSHOTBL   ASSIGN         DA-01-VI-SHOTBL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SHO-F01   SHO-F02
                        STATUS    HSHOTBL-ST1.
*----<< 売上データ >>--*
     SELECT   TOKU      ASSIGN         DA-01-S-TOKU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOKU-ST1.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 伝票データ >>--*
 FD  HDENJNL            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 条件ファイル >>--*
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 商品名称マスタ >>--*
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*----<< 商品変換テーブル >>--*
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*----<< 売上データ >>--*
 FD  TOKU               LABEL RECORD   IS   STANDARD.
     COPY     TOKUREC   OF        XFDLIB
              JOINING   TOKU      PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01).
     03  MEI-INV-FLG    PIC  9(01).
     03  SHO-INV-FLG    PIC  9(01).
     03  TEN-INV-FLG    PIC  9(01).
     03  ONL-FLG        PIC  9(01).
     03  TAN-ERR-FLG    PIC  9(01).
     03  TEI-FLG        PIC  9(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(03).
     03  OUT-CNT        PIC  9(03).
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
     03  CNT            PIC  9(06).
 01  INDEXES.
     03  I              PIC  9(03).
 01  TANTO-WORK.
     03  TAN-OLD        PIC  9(02).
     03  TAN-NEW        PIC  9(02).
 01  WK-TANCD           PIC  9(02).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  HDENJNL-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  HJYOKEN-ST        PIC  X(02).
 01  HTENMS-ST         PIC  X(02).
 01  HMEIMS-ST         PIC  X(02).
 01  HSHOTBL-ST.
     03  HSHOTBL-ST1   PIC  X(02).
     03  HSHOTBL-ST2   PIC  X(04).
 01  TOKU-ST.
     03  TOKU-ST1      PIC  X(02).
     03  TOKU-ST2      PIC  X(04).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  WK-NOUHIN          PIC  9(08).
 01  FILLER             REDEFINES      WK-NOUHIN.
     03  WK-YYYY        PIC  9(02).
     03  WK-YMD         PIC  9(06).
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
 01  WK-TOKCD           PIC  9(08).
 01  CYU-DATE           PIC  9(08).
 01  FILLER             REDEFINES      CYU-DATE.
     03  CYU-YY         PIC  9(04).
     03  CYU-MM         PIC  9(02).
     03  CYU-DD         PIC  9(02).
 01  NOU-DATE           PIC  9(08).
 01  FILLER             REDEFINES      NOU-DATE.
     03  NOU-YY         PIC  9(04).
     03  NOU-MM         PIC  9(02).
     03  NOU-DD         PIC  9(02).
 01  SYU-DATE           PIC  9(08).
 01  FILLER             REDEFINES      SYU-DATE.
     03  SYU-YY         PIC  9(04).
     03  SYU-MM         PIC  9(02).
     03  SYU-DD         PIC  9(02).
 01  CNV-YY             PIC  9(04)     VALUE  0.
 01  ACOS-MM            PIC  9(02)     VALUE  0.
*計上部門
 01  WK-BUMON           PIC  9(04)     VALUE  ZERO.
*----<< ﾌﾞﾚｲｸ ﾜｰｸ >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-F01    PIC  9(08).
         05  NEW-F02    PIC  9(09).
     03  OLD.
         05  OLD-F01    PIC  9(08).
         05  OLD-F02    PIC  9(09).
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
*### 99/05/28 NAV START ###*
*日付比較用ワーク
 01  WK-DATE-NONYU-SYUKA.
     03  WK-SYU         PIC  9(08)  VALUE  ZERO.
*### 99/05/28 NAV END   ###*
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(40)     VALUE  SPACE.
         05  FILLER     PIC  N(18)     VALUE
              NC"【　売上データ　変換エラーリスト　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"処理日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-011     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-02      PIC  ZZ9.
 01  HEAD02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(10)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"伝票_".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"店舗".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"店　舗　名".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"取引先商品".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"商品・品単".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"商　品　名".
         05  FILLER     PIC  X(22)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"エラー内容".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(10).
         05  ME-03      PIC  X(09).
         05  FILLER     PIC  X(02).
         05  ME-04      PIC  9(08).
         05  FILLER     PIC  X(03).
         05  ME-05      PIC  9(05).
         05  FILLER     PIC  X(02).
         05  ME-06      PIC  N(10).
         05  FILLER     PIC  X(02).
         05  ME-07      PIC  X(13).
         05  FILLER     PIC  X(02).
         05  ME-07A     PIC  X(16).
         05  FILLER     PIC  X(02).
         05  ME-08      PIC  X(30).
         05  FILLER     PIC  X(02).
         05  ME-09      PIC  N(10).
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
*--<< ｻﾌﾞﾙｰﾁﾝ ﾊﾟﾗﾒﾀ >>-*
 01  SSKTDTCK-PARA.
     03  SSKTDTCK-YMD        PIC  9(06).
     03  FILLER              REDEFINES SSKTDTCK-YMD.
         05  SSKTDTCK-Y      PIC  9(02).
         05  SSKTDTCK-M      PIC  9(02).
         05  SSKTDTCK-D      PIC  9(02).
     03  SSKTDTCK-RET        PIC  9(01).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 伝票データ >>--*
 HDENJNL-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HDENJNL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SNI0030B HDENJNL ERROR " HDENJNL-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU.
     CLOSE    HDENJNL   HTOKMS    HTENMS.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 条件ファイル >>--*
 HJYOKEN-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SNI0030B HJYOKEN ERROR " HJYOKEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU.
     CLOSE    HDENJNL   HTOKMS    HTENMS.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SNI0030B HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU.
     CLOSE    HDENJNL   HTOKMS    HTENMS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SNI0030B HTENMS ERROR " HTENMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU.
     CLOSE    HDENJNL   HTOKMS    HTENMS.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 商品名称マスタ >>--*
 HMEIMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HMEIMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SNI0030B HMEIMS ERROR " HMEIMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU.
     CLOSE    HDENJNL   HTOKMS    HTENMS.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 商品変換テーブル >>--*
 HSHOTBL-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHOTBL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SNI0030B HSHOTBL ERROR " HSHOTBL-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU.
     CLOSE    HDENJNL   HTOKMS    HTENMS.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 売上データ >>--*
 TOKU-ERR               SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOKU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SNI0030B TOKU ERROR " TOKU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN   HMEIMS    HSHOTBL   TOKU.
     CLOSE    HDENJNL   HTOKMS    HTENMS.
     MOVE     "4010"    TO        PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO     =    99.
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
     DISPLAY  "*** SNI0030B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       HDENJNL.
     OPEN     INPUT     HJYOKEN.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     INPUT     HMEIMS.
     OPEN     INPUT     HSHOTBL.
     OPEN     OUTPUT    TOKU.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     0         TO   GR-NO.
*----<< ｼｽﾃﾑﾋﾂﾞｹﾍﾝｶﾝ >>-*
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
     MOVE      LINK-OUT-YMD       TO   SYS-YYMD.
     MOVE     58        TO   JYO-F01.
     MOVE     SPACE     TO   JYO-F02.
     PERFORM  900-JYO-READ.
*----<< ﾌﾞﾓﾝｺｰﾄﾞ ｼｭﾄｸ >>-*
     MOVE    99              TO   JYO-F01.
     MOVE    "BUMON"         TO   JYO-F02.
     READ    HJYOKEN
             INVALID
             DISPLAY "HJYOKEN BUMON INVALID" JYO-F01 " - " JYO-F02
                      UPON CONS
                      STOP RUN
             NOT  INVALID
             MOVE     JYO-F04     TO   WK-BUMON
     END-READ.
*----<< 日次更新条件取得 >>-*
     MOVE     99        TO   JYO-F01.
     MOVE     "DAY"     TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG   =    0
              MOVE      JYO-F04   TO   WK-TOKCD
              MOVE      JYO-F05   TO   CYU-DATE
              MOVE      JYO-F06   TO   NOU-DATE
              MOVE      JYO-F07   TO   SYU-DATE
     END-IF.
     IF      INV-FLG         =    1
     OR      ERR-FLG    NOT  =    0
             MOVE       99        TO   GR-NO
     END-IF.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     OPEN     OUTPUT    PRTF.
     MOVE     99             TO   LINE-CNT.
     MOVE     0              TO   PAGE-CNT.
     MOVE     0              TO   ERR-FLG.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
*
     MOVE     ZERO           TO   DEN-F01.
     MOVE     ZERO           TO   DEN-F02.
     MOVE     ZERO           TO   DEN-F04.
     MOVE     ZERO           TO   DEN-F03.
     MOVE     WK-TOKCD       TO   DEN-F01.
     PERFORM  900-DEN-START-READ.
     PERFORM  241-OUT-MAIN
                        UNTIL     OLD  =    HIGH-VALUE.
     CLOSE    PRTF.
*
     MOVE     99             TO   GR-NO.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HDENJNL.
     CLOSE    HJYOKEN.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
     CLOSE    HMEIMS.
     CLOSE    HSHOTBL.
     CLOSE    TOKU.
*
     DISPLAY  "+++ SNI0030B ｾﾝﾀｸ  ｹﾝｽｳ = " IN-CNT " +++"
                                       UPON CONS.
     DISPLAY  "+++ SNI0030B ｼｭﾂﾘｮｸｹﾝｽｳ = " OUT-CNT " +++"
                                       UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SNI0030B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｳﾘｱｹﾞﾃﾞｰﾀ ｼｭﾂﾘｮｸ                            *
*--------------------------------------------------------------*
 241-OUT-MAIN           SECTION.
*---<< ﾚｺｰﾄﾞ ｶｲﾎｳ >>---*
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   900-DEN-REWRITE
              ADD       1         TO   IN-CNT
     END-IF.
*---<< ﾃﾞｰﾀ ｼｭﾂﾘｮｸ ｵﾖﾋﾞ ﾌﾗｸﾞｾｯﾄ >>---*
     IF       NEW  NOT  =    OLD
     AND      OLD  NOT  =    LOW-VALUE
     AND      ERR-FLG   =    0
              MOVE OLD-F01   TO   DEN-F01   NEW-F01
              MOVE OLD-F02   TO   DEN-F02   NEW-F02
              MOVE ZERO      TO   DEN-F04
              MOVE ZERO      TO   DEN-F03
              PERFORM   900-DEN-START-READ
              PERFORM   2411-TOKU-OUTPUT
                             UNTIL     NEW  NOT  =    OLD
              MOVE ZERO      TO   ONL-FLG TAN-ERR-FLG TEI-FLG
     END-IF.
     IF       NEW  NOT  =    OLD
              MOVE 0         TO   ERR-FLG
     END-IF.
*---<< ｴﾗｰ ﾁｪｯｸ >>---*
     IF       NEW  NOT  =    HIGH-VALUE
     AND      DEN-F03   NOT  =    80   AND  90
              MOVE      DEN-F01        TO   TEN-F52
              MOVE      DEN-F07        TO   TEN-F011
*\\  93.07.10
              MOVE      ZERO           TO   TEN-INV-FLG
              IF        DEN-F07        NOT =    ZERO
                        PERFORM   900-TEN-READ
              END-IF
*\\  93.07.10  END
*
              MOVE      DEN-F141       TO   MEI-F01
              PERFORM   900-MEI-READ
              MOVE 0         TO   SHO-INV-FLG
              IF   DEN-F25   NOT  =    SPACE
                   MOVE      DEN-F01        TO   SHO-F01
                   MOVE      DEN-F25        TO   SHO-F02
                   PERFORM   900-SHO-READ
              END-IF
              IF        TEN-INV-FLG    =    1
              OR        MEI-INV-FLG    =    1
              OR        SHO-INV-FLG    =    1
                        MOVE      1         TO   ERR-FLG
                        PERFORM   2412-ERROR-PRINT
              END-IF
*             ｵﾝﾗｲﾝ区分=1の場合
              IF   DEN-F274  =  1
                   IF     ONL-FLG  =  ZERO
                          MOVE DEN-F06  TO  TAN-OLD
                   END-IF
                   MOVE   1       TO    ONL-FLG
*                  伝票内担当者比較
                   IF   TAN-OLD  NOT =  TAN-NEW
                        MOVE   1  TO    TAN-ERR-FLG
                   END-IF
*                  数量訂正ﾁｪｯｸ
                   IF   DEN-F15  NOT =  DEN-F50
                        MOVE   1  TO    TEI-FLG
                   END-IF
              END-IF
     END-IF.
*
     MOVE     NEW       TO   OLD.
     MOVE     TAN-NEW   TO   TAN-OLD.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   900-DEN-READ
     END-IF.
 241-OUT-MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      ｳﾘｱｹﾞﾃﾞｰﾀ ｼｭﾂﾘｮｸ                            *
*--------------------------------------------------------------*
 2411-TOKU-OUTPUT       SECTION.
*売上作成ＦＬＧセット
     MOVE     9         TO  DEN-F277.
*売上計上日セット
     MOVE     SYS-YYMD  TO  DEN-F45.
     IF       DEN-F113  =        ZERO
        IF    SYU-DATE  NOT  =    0
              MOVE      SYU-DATE  TO   DEN-F113
         ELSE
              MOVE      DEN-F112  TO   DEN-F113
         END-IF
     END-IF.
*担当者チェック
     IF       ONL-FLG     = 1
     AND      TAN-ERR-FLG = 1
              IF  TAN-OLD = 90 OR 99
                  IF  TEI-FLG = 1
                      MOVE  90  TO  WK-TANCD DEN-F06
                      MOVE  1   TO  DEN-F53
                  ELSE
                      MOVE  99  TO  WK-TANCD DEN-F06
                      MOVE  0   TO  DEN-F53
                  END-IF
              ELSE
                  MOVE  DEN-F06 TO  WK-TANCD
              END-IF
     ELSE
              MOVE    DEN-F06   TO  WK-TANCD
     END-IF.
*
     PERFORM  900-DEN-REWRITE.
*
     MOVE     SPACE          TO   TOKU-REC.
     INITIALIZE                   TOKU-REC.
     MOVE     2900           TO   TOKU-F01.
     MOVE     DEN-F051       TO   TOKU-F02.
     MOVE     DEN-F23        TO   TOKU-F03.
     MOVE     DEN-F03        TO   TOKU-F04.
     MOVE     DEN-F04        TO   TOKU-F05.
     MOVE     DEN-F24        TO   TOKU-F06.
*### 99/05/28 NAV START ###*
*    大阪売上チェック
     MOVE     DEN-F113       TO   WK-SYU.
     IF       WK-SYU  >=  19990601
              EVALUATE   DEN-F24
*                 サンサンランド_
                  WHEN   "61100209"
                          MOVE  "27536580"  TO  TOKU-F06
*                 トーク
                  WHEN   "61101060"
                          MOVE  "28514010"  TO  TOKU-F06
*                 コーナン
                  WHEN   "61101100"
                          MOVE  "27532070"  TO  TOKU-F06
*                 丸長商事_
                  WHEN   "61101130"
                          MOVE  "30583520"  TO  TOKU-F06
*                 ニック産業_
                  WHEN   "61101140"
                          MOVE  "26569010"  TO  TOKU-F06
*                 丸長商事_パワーセンターマルチョウ
                  WHEN   "61101240"
                          MOVE  "30583510"  TO  TOKU-F06
*                 _キッコリー
                  WHEN   "61101320"
                          MOVE  "27527060"  TO  TOKU-F06
              END-EVALUATE
     END-IF.
*### 99/05/28 NAV END   ###*
*
     MOVE     DEN-F07        TO   TOKU-F07.
     MOVE     DEN-F111       TO   TOKU-F08.
     MOVE     DEN-F112       TO   TOKU-F09.
     MOVE     DEN-F113       TO   TOKU-F10.
     IF       DEN-F113       =    ZERO
         MOVE DEN-F112       TO   TOKU-F10
     END-IF.
*
     MOVE     DEN-F1411      TO   TOKU-F11.
     MOVE     DEN-F1412      TO   TOKU-F12.
*## 1999/12/27 ｽﾄｯｸNO. NAV START ##*
     MOVE     DEN-F21        TO   TOKU-F13.
*****MOVE     DEN-F43        TO   TOKU-F13.
     MOVE     DEN-F16        TO   TOKU-F14.
     MOVE     DEN-F15        TO   TOKU-F15.
     MOVE     DEN-F172       TO   TOKU-F16.
     MOVE     DEN-F181       TO   TOKU-F17.
     MOVE     DEN-F08        TO   TOKU-F18.
     MOVE     "2"            TO   TOKU-F19.
     MOVE     SPACE          TO   TOKU-F20.
     MOVE     SPACE          TO   TOKU-F21.
     MOVE     DEN-F22        TO   TOKU-F22.
     MOVE     DEN-F10        TO   TOKU-F23.
     MOVE     DEN-F25        TO   TOKU-F24.
*****MOVE     DEN-F06        TO   TOKU-F25.
     MOVE     WK-TANCD       TO   TOKU-F25.
     MOVE     DEN-F133       TO   TOKU-F26.
     MOVE     SYS-YYMD       TO   TOKU-F27.
     IF       DEN-F03   NOT  =    80
              ADD       1         TO   OUT-CNT
              WRITE     TOKU-REC
     END-IF.
*
     PERFORM  900-DEN-READ.
 2411-TOKU-OUTPUT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ｴﾗｰ  ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2412-ERROR-PRINT       SECTION.
     IF       LINE-CNT  >    63
              PERFORM   2413-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
     MOVE     DEN-F02        TO   ME-03.
     MOVE     DEN-F01        TO   ME-04.
     MOVE     DEN-F07        TO   ME-05.
     MOVE     TEN-F03        TO   ME-06.
     MOVE     DEN-F25        TO   ME-07.
     MOVE     DEN-F141       TO   ME-07A.
     MOVE     DEN-F142       TO   ME-08.
*****MOVE     MEI-F03        TO   ME-08.
     IF       SHO-INV-FLG    =    2
              MOVE NC"商品変換ＴＢＬ不一致" TO   ME-09
     END-IF.
     IF       SHO-INV-FLG    =    1
              MOVE NC"商品変換ＴＢＬ未登録" TO   ME-09
     END-IF.
     IF       MEI-INV-FLG    =    1
              MOVE NC"商品Ｍ　未登録"  TO   ME-09
     END-IF.
     IF       TEN-INV-FLG    =    1
              MOVE NC"店舗Ｍ　未登録"  TO   ME-09
     END-IF.
*
     WRITE    PRT-REC        FROM MEIS01    AFTER     1.
     ADD      1              TO   LINE-CNT.
 2412-ERROR-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 2413-HEAD-PRINT        SECTION.
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*
     ADD      1                   TO   PAGE-CNT.
     MOVE     SYS-YY              TO   HD-011.
     MOVE     SYS-MM              TO   HD-012.
     MOVE     SYS-DD              TO   HD-013.
     MOVE     PAGE-CNT            TO   HD-02.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     3.
     WRITE    PRT-REC   FROM      P-SPACE   AFTER     1.
     MOVE     7              TO   LINE-CNT.
 2413-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    条件ファイル　 READ                          *
*--------------------------------------------------------------*
 900-JYO-READ           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HJYOKEN   INVALID
              MOVE      1              TO   INV-FLG
     DISPLAY  "### SNI0030B HJYOKEN INVALID KEY= "
                                     JYO-F01  JYO-F02 " ###"
                                       UPON CONS
     END-READ.
 900-JYO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HTOKMS    INVALID
              MOVE      1         TO   INV-FLG
              MOVE      SPACE     TO   TOK-F03
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     MOVE     0         TO   TEN-INV-FLG.
     READ     HTENMS    INVALID
              MOVE      1         TO   TEN-INV-FLG
              MOVE      SPACE     TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    商品マスタ　　READ                           *
*--------------------------------------------------------------*
 900-MEI-READ           SECTION.
     MOVE     0         TO   MEI-INV-FLG.
     READ     HMEIMS    INVALID
              MOVE      1         TO   MEI-INV-FLG
              MOVE      SPACE     TO   MEI-F03
     END-READ.
 900-MEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    商品変換テーブル　　READ                     *
*--------------------------------------------------------------*
 900-SHO-READ           SECTION.
     MOVE     0         TO   SHO-INV-FLG.
     READ     HSHOTBL    INVALID
              MOVE      1         TO   SHO-INV-FLG
     END-READ.
     IF       SHO-INV-FLG    =    0
     AND      SHO-F03   NOT  =    DEN-F141
              MOVE      2         TO   SHO-INV-FLG
     END-IF.
 900-SHO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-DEN-START-READ     SECTION.
     START    HDENJNL   KEY  >=   DEN-F01   DEN-F02
                                  DEN-F04   DEN-F03
              INVALID   KEY
                        MOVE HIGH-VALUE     TO   NEW
     END-START.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   900-DEN-READ
     END-IF.
 900-DEN-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 READ                          *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     READ     HDENJNL   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-DEN-READ-EXIT
     END-READ.
 DEN010.
     MOVE     DEN-F112  TO   WK-NOUHIN.
     ADD      1         TO   CNT.
     IF       CNT(4:3) =  "500" OR  "000"
              DISPLAY  "DEN-CNT= " CNT    UPON   CONS
     END-IF.
 DEN020.
     IF       WK-YMD    =    ZERO
              GO   TO   900-DEN-READ
     END-IF.
 DEN030.
     IF       WK-TOKCD  NOT  =    0
     AND      DEN-F01   >    WK-TOKCD
              PERFORM   900-DEN-REWRITE
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-DEN-READ-EXIT
     END-IF.
 DEN040.
*
     IF       DEN-F277  NOT  =    0
     OR       DEN-F276  NOT  =    9
**************PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
 DEN050.
     IF       CYU-DATE  NOT  =    ZERO
     AND      DEN-F111  NOT  =    CYU-DATE
**************PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*
**** AND      DEN-F112  NOT  =    NOU-DATE   92.12.14
**** 納品日ベースの売上計上へ変更する****************
*****AND      DEN-F112  >         NOU-DATE          *
*****************************************************
 DEN060.
     IF       NOU-DATE  NOT  =    ZERO
     AND      DEN-F113  >         NOU-DATE
**************PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*
 DEN070.
     IF      (DEN-F113  =         ZERO)         AND
             (DEN-F051  =  40  OR 41  )
**************PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*
 DEN080.
     IF       DEN-F051  NOT =  40 AND 41
        IF    DEN-F112  >         NOU-DATE
**************PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
        END-IF
     END-IF.
*
*
 DEN090.
     MOVE     DEN-F01        TO   NEW-F01.
     MOVE     DEN-F02        TO   NEW-F02.
*    ﾀﾝﾄｳｼｬｾｯﾄ
     MOVE     DEN-F06        TO   TAN-NEW.
 900-DEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 REWRITE                       *
*--------------------------------------------------------------*
 900-DEN-REWRITE        SECTION.
     REWRITE  DEN-REC.
 900-DEN-REWRITE-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
